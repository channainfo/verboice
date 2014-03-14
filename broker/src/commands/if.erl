-module('if').
-export([run/2]).
-include("session.hrl").

run(Args, Session = #session{js_context = JS}) ->
  Condition = proplists:get_value(condition, Args),
  io:format( " ~n  if condition :  ~p ", [Condition]),

  Then = proplists:get_value(then, Args),
  Else = proplists:get_value(else, Args),

  {Value, _} = erjs:eval(Condition, JS),

  io:format(" ~n if value : ~p", [Value]),

  Result = case Value of
    true -> run_branch(Then);
    _    -> run_branch(Else)
  end,
  {Result, Session}.

run_branch(undefined) -> next;
run_branch(Branch) -> {goto, Branch}.