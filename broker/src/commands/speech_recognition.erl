-module(speech_recognition).
-export([run/2]).
-include("session.hrl").
-include("db.hrl").

run(Args, Session = #session{pbx = Pbx, call_log = CallLog, contact = Contact, project = Project}) ->
  Key = util:to_string(proplists:get_value(key, Args)),
  Description = proplists:get_value(description, Args),
  StopKeys = proplists:get_value(stop_keys, Args, "01234567890*#"),
  Timeout = proplists:get_value(timeout, Args, 10),


  Store     = proplists:get_value(store, Args),
  OldStore  = proplists:get_value(old_store, Args),

  Result1   = proplists:get_value(result1, Args),
  Result2   = proplists:get_value(result2, Args),
  Result3   = proplists:get_value(result3, Args),

  Accuracy1 = proplists:get_value(accuracy1, Args),
  Accuracy2 = proplists:get_value(accuracy2, Args),
  Accuracy3 = proplists:get_value(accuracy3, Args),
  
  VariableList = { {Result1, Accuracy1}, {Result2, Accuracy2}, {Result3, Accuracy3} },




  io:format("~n--------------------------------- Args list : ~p", [Args]),
  io:format("~n--------------------------------- Project: ~p ", [Project]),

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
  record:create_call_log_recorded_audio(OldStore, Store, Key, Description, Project#project.id, CallLogId),

  CallLog:info("Speech translation", [{command, "speech_recognition"}, {action, "start"}]),
  % TODO text translation
  SpeechDecode = '{"results":[{"result":"kompong cham","confidence":85.5},{"result":"kompong chnaing","confidence":70.23},{"result":"kompong thom","confidence":40.31}],"error":""}',
  store_result_from_speech(SpeechDecode, VariableList, Session),

  CallLog:info("Speech translation", [{command, "speech_recognition"}, {action, "finish"}]),


  CallLog:info("Recording saved", [{command, "speech_recognition"}, {action, "finish"}]),
  {next, Session}.

filename(CallLogId, Key) ->
  filename:join(["../data/call_logs/", util:to_string(CallLogId), "results", Key ++ ".wav"]).

store_result_from_speech(SpeechDecode, VariableList, Session) ->

  Speech = json:decode(atom_to_list(SpeechDecode)),
         % {ok,{
         %      [
         %        {<<"results">>, 
         %          [ 
         %            {
         %              [ 
         %                {<<"result">>,<<"kompong cham">>},
         %                {<<"confidence">>,85.5 }
         %              ]
         %            },

         %            { [ 
         %                {<<"result">>,<<"kompong chnaing">>},
         %                {<<"confidence">>,70.23}
         %              ]
         %            },

         %            { [ 
         %                {<<"result">>,<<"kompong thom">>},
         %                {<<"confidence">>,40.31}]}
         %              ]
         %            },
         %        {<<"error">>,<<>>}
         %      ]
         %     }
         %  }

  if element(1, Speech) == ok ->
    { _, { [ { _ , ResultList }, { _ , Error} ] } } = Speech ,
      if Error /= <<>> ->
        io:format("~n Speech recognation raise error with: ~p", [Error]);
      true ->  
        WorkingResultList = lists:sublist(ResultList, 1, 3), % we are only interested in 3 first elements
        store_list_elements( WorkingResultList , VariableList, 1, Session)
      end;  
  true ->
    io:format("Result: ~p is invalid json format ", [Speech])  
  end.

store_element(Element, VarList, Index, Session) ->
  {ResultVar, ConfidenceVar} = element(Index, VarList), 
  {[{_, Result},{_, Confidence}]} = Element,
  io:format("~n store element for variable :  ~p ", [element(Index, VarList)]),
  io:format("~n store element for value:  ~p, ~p ", [Result, Confidence]),

  store_result_data(ResultVar, Result, Session),
  store_result_data(ConfidenceVar,Confidence, Session).

store_list_elements([Element], VariableList, N, Session) ->
  store_element(Element, VariableList, N, Session);

store_list_elements([Element|T], VariableList, N, Session) ->
  store_element(Element, VariableList, N, Session),
  store_list_elements(T, VariableList, N+1, Session).


store_result_data("", _ , _ ) -> io:format("~n Variable is empty");
store_result_data(VarName, Value, Session) ->
  #session{project = MapProject} = Session,
  ProjectId = MapProject#project.id,
  ProjectVar  = project_variable:find_or_create([{project_id, ProjectId}, {name, VarName}]),

  if ProjectVar == undefined ->
     NewProjectVariableName = #project_variable{project_id=ProjectId, name=VarName},
     NewProjectVariableName:save();
  true ->
     io:format("~n variable exists")
  end,
  persist_variable:store_peristed_variable(VarName, Value, Session).