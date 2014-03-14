-module(while).
-export([run/2]).
-include("session.hrl").

run(Args, Session = #session{js_context = JS}) ->
  Condition = proplists:get_value(condition, Args),
  io:format("~n ----------------------while condition ~p", [Condition]),
  {Value, JS2} = erjs:eval(Condition, JS),

  io:format("~n ---------------------- while value: ~p", [Value]),

  Action = case Value of
    true ->
      Block = proplists:get_value(block, Args),
      {goto, Block};
    _ ->
      next
  end,
  {Action, Session#session{js_context = JS2}}.
