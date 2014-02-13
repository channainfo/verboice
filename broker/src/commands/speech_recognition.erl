-module(speech_recognition).
-export([run/2]).
-include("session.hrl").
-include("db.hrl").

run(Args, Session = #session{pbx = Pbx, call_log = CallLog, contact = Contact, project = Project}) ->
  Key = util:to_string(proplists:get_value(key, Args)),
  Description = proplists:get_value(description, Args),
  StopKeys = proplists:get_value(stop_keys, Args, "01234567890*#"),
  Timeout = proplists:get_value(timeout, Args, 10),

  OldResult1 = proplists:get_value(old_result1, Args),
  Result1 = proplists:get_value(result1, Args),

  CallLog:info("Record user voice", [{command, "record"}, {action, "start"}]),
  CallLogId = CallLog:id(),
  Filename = filename(CallLogId, Key),
  filelib:ensure_dir(Filename),


  Pbx:record(Filename, StopKeys, Timeout),

  io:format("~n -------------------------------File generated: ~p ~n", [Filename]),

  RecordedAudio = #recorded_audio{
    contact_id = Contact#contact.id,
    project_id = Project#project.id,
    call_log_id = CallLogId,
    key = Key,
    description = Description
  },
  RecordedAudio:save(),

  create_call_log_recorded_audio(OldResult1, Result1, Key, Description, Project#project.id, CallLogId),

  CallLog:info("Speech translation", [{command, "speech_recognition"}, {action, "start"}]),
  % TODO text translation
  CallLog:info("Speech translation", [{command, "speech_recognition"}, {action, "finish"}]),


  CallLog:info("Recording saved", [{command, "speech_recognition"}, {action, "finish"}]),
  {next, Session}.

filename(CallLogId, Key) ->
  filename:join(["../data/call_logs/", util:to_string(CallLogId), "results", Key ++ ".wav"]).

create_call_log_recorded_audio(OldResult1, Result1, Key, Description, ProjectId, CallLogId) ->
  OldResult1Bin = list_to_binary(OldResult1),
  Result1Bin = list_to_binary(Result1),
  if
    OldResult1Bin /= <<>>; Result1Bin /= <<>> ->
      ProjectVariableOld = project_variable:find([{project_id, ProjectId}, {name, OldResult1}]),
      ProjectVariableCur = project_variable:find([{project_id, ProjectId}, {name, Result1}]),

      ProjectVariable = case ProjectVariableOld  of
        undefined -> 
          if 
            ProjectVariableCur == undefined ->    
              NewProjectVariable = #project_variable{project_id = ProjectId, name = Result1},
              NewProjectVariable:save() ;
            true -> 
              ProjectVariableCur      
          end;  
        _ -> 
          if OldResult1  /= OldResult1Bin -> ProjectVariableOld:update([{name, Result1}]) end,
          ProjectVariableOld
      end,

      ProjectVariableId = ProjectVariable#project_variable.id ,

      CallLogRecordedAudio = call_log_recorded_audio:find([ {call_log_id, CallLogId}, 
                                                            {project_variable_id, ProjectVariableId }
                                                          ]),
      case CallLogRecordedAudio of
        undefined ->
          NewCallLogRecordedAudio = #call_log_recorded_audio{
            call_log_id = CallLogId,
            project_variable_id = ProjectVariableId,
            key = Key,
            description = Description
          },
          NewCallLogRecordedAudio:save();
        _ -> 
          CallLogRecordedAudio:update([ {call_log_id, CallLogId}, 
                                        {project_variable_id, ProjectVariableId}, 
                                        {key, Key}, 
                                        {description, Description}
                                      ])
      end;
    true -> io:format("Do nothing", [])
  end.