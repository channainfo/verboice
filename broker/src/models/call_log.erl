-module(call_log).
-export([error/3, info/3, trace/3]).
-export([duration/1, append_step_interaction/2]).
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

append_step_interaction(StepName, CallLog = #call_log{created_at = {_, CreatedAt}, started_at = StartedAt, step_interaction = StepInteraction}) ->
  From = case StartedAt of
    undefined -> CreatedAt;
    _ -> StartedAt
  end,
  Time = util:time_difference_in_seconds(From, calendar:universal_time()),
  Interaction = [StepName, ":", util:to_string(Time)],
  NewStepInteraction = case StepInteraction of
    undefined -> Interaction;
    _ ->
      [util:to_string(StepInteraction), ";", Interaction]
  end,
  CallLog#call_log{step_interaction = binary_util:to_binary(NewStepInteraction)}.
