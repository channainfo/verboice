-module(callback).
-export([run/2]).
-include("session.hrl").
-include("db.hrl").
-include("uri.hrl").

run(Args, Session = #session{session_id = SessionId, js_context = JS, call_log = CallLog, call_flow = CallFlow, callback_params = CallbackParams}) ->
  CallLog:info("Callback started", [{command, "callback"}, {action, "start"}]),
  Url = case proplists:get_value(url, Args) of
    undefined -> CallFlow#call_flow.callback_url;
    X -> list_to_binary(X)
  end,
  ResponseType = proplists:get_value(response_type, Args, flow),
  Params = proplists:get_value(params, Args, []),
  Variables = proplists:get_value(variables, Args, []),
  Method = proplists:get_value(method, Args, "post"),
  Async = proplists:get_value(async, Args),

  QueryString = prepare_params(Params ++ Variables, [{"CallSid", SessionId} | CallbackParams], JS),
  RequestUrl = interpolate(Url, Args, Session),
  Uri = uri:parse(RequestUrl),

  case Async of
    true ->
      delayed_job:enqueue(""),
      {next, Session};

    _ ->
      io:format("~p~n", [uri:format(Uri)]),
      {ok, {_StatusLine, _Headers, Body}} = case Method of
        "get" ->
          (Uri#uri{query_string = QueryString}):get([]);
        _ ->
          Uri:post_form(QueryString, [])
      end,

      CallLog:trace(["Callback returned: ", Body], [{command, "callback"}, {action, "return"}]),
      handle_response(ResponseType, Body, Session)
  end.

handle_response(flow, Body, Session) ->
  Commands = twiml:parse(Body),
  io:format("~p~n", [Commands]),
  {{exec, Commands}, Session};

handle_response(variables, Body, Session) ->
  {ok, Response} = json:decode(Body),
  case Response of
    {Vars} -> {next, handle_response_variables(Vars, Session)};
    _ -> {next, Session}
  end;

handle_response(_, _, Session) -> {next, Session}.

handle_response_variables([], Session) -> Session;
handle_response_variables([{Name, Value} | Rest], Session = #session{js_context = JsContext}) ->
  VarName = binary_to_atom(<<"response_", Name/binary>>, utf8),
  VarValue = case Value of
    Bin when is_binary(Bin) -> binary_to_list(Bin);
    X -> X
  end,
  NewSession = Session#session{js_context = erjs_context:set(VarName, VarValue, JsContext)},
  handle_response_variables(Rest, NewSession).

prepare_params([], QueryString, _JS) -> QueryString;
prepare_params([{Name, Expr} | Rest], QueryString, JS) ->
  {Value, _} = erjs:eval(Expr, JS),
  NewQueryString = assign_from_js(QueryString, Name, Value, JS),
  prepare_params(Rest, NewQueryString, JS).

assign_from_js(QueryString, _, undefined, _) -> QueryString;
assign_from_js(QueryString, Prefix, Value, JS) ->
  if
    is_reference(Value) ->
      Properties = erjs_context:get_object(Value, JS),
      lists:foldl(fun({FName, FValue}, QS) ->
        assign_from_js(QS, [Prefix, "[", FName, "]"], FValue, JS)
      end, QueryString, Properties);
    true -> [{iolist_to_binary(Prefix), Value} | QueryString]
  end.

interpolate(Url, Args, #session{project = #project{id = ProjectId}}) ->
  InterpolatedUrl = util:interpolate(Url, fun(VarNameBin) ->
    case proplists:get_value(external_service_guid, Args) of
      undefined -> <<>>;
      SvcGuid ->
        Svc = external_service:find([{project_id, ProjectId}, {guid, SvcGuid}]),
        case Svc:global_variable_value_for(binary_to_list(VarNameBin)) of
          undefined -> <<>>;
          Value -> list_to_binary(Value)
        end
    end
  end),
  binary_to_list(InterpolatedUrl).
