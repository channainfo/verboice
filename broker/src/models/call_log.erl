-module(call_log).
-export([error/3, info/3, trace/3]).
-export([duration/1]).
-define(TABLE_NAME, "call_logs").
-include_lib("erl_dbmodel/include/model.hrl").

error(Message, Details, #call_log{id = CallId}) ->
  call_log_entry:create("error", CallId, Message, Details).

info(Message, Details, #call_log{id = CallId}) ->
  call_log_entry:create("info", CallId, Message, Details).

trace(Message, Details, #call_log{id = CallId}) ->
  call_log_entry:create("trace", CallId, Message, Details).

duration(#call_log{started_at = StartedAt, finished_at = FinishedAt, duration = Duration}) ->
  case Duration of
    undefined ->
      if
        FinishedAt /= undefined, StartedAt /= undefined ->
          {_, NewStartedAt} = StartedAt,
          {_, NewFinishedAt} = FinishedAt,
          calendar:datetime_to_grogerian_seconds(NewFinishedAt) - calendar:datetime_to_gregorian_seconds(NewStartedAt);
        true -> 0
      end;
    NewDuration -> NewDuration
  end.
