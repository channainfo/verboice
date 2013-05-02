-module(sox).
-export([convert/2]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(TIMEOUT, 1000).
-record(state, {port, caller}).

start_link() ->
  gen_server:start_link(?MODULE, {}, []).

convert(InPath, OutPath) ->
  {ok, Pid} = start_link(),
  gen_server:call(Pid, {convert, InPath, OutPath}).

%% @private
init({}) ->
  {ok, #state{}, ?TIMEOUT}.

%% @private
handle_call({convert, Input, OutPath}, From, State) ->
  Port = do_convert(Input, OutPath),
  {noreply, State#state{port = Port, caller = From}, ?TIMEOUT * 5}.

%% @private
handle_cast(_Msg, State) ->
  {noreply, State, ?TIMEOUT}.

%% @private
handle_info(timeout, State) ->
  {stop, timeout, State};

handle_info({Port, {exit_status, N}}, State = #state{port = Port, caller = Caller}) ->
  Result = case N of 0 -> ok; _ -> error end,
  gen_server:reply(Caller, Result),
  {stop, normal, State};

handle_info(_Info, State) ->
  {noreply, State, ?TIMEOUT}.

%% @private
terminate(_Reason, #state{port = Port}) ->
  case erlang:port_info(Port, connected) of
    {connected, _} -> port_close(Port);
    _ -> ok
  end.

%% @private
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


do_convert(Input, OutPath) when is_list(Input) ->
  open_port({spawn, "sox " ++ Input ++ " -r 8000 -c1 " ++ OutPath}, [exit_status, stderr_to_stdout]);

do_convert(Input, OutPath) when is_binary(Input) ->
  Port = open_port({spawn, "sox - -r 8000 -c1 " ++ OutPath}, [binary, exit_status, stderr_to_stdout]),
  port_command(Port, Input),
  Port.
