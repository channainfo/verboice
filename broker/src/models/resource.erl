-module(resource).
-export([find_by_guid/1, localized_resource/2, prepare/2, prepare/3, prepare_text_resource/2, prepare_text_resource/3, prepare_blob_resource/3, prepare_url_resource/2]).
-define(CACHE, true).
-define(TABLE_NAME, "resources").
-include_lib("erl_dbmodel/include/model.hrl").
-include("session.hrl").

find_by_guid(Guid) ->
  find({guid, Guid}).

localized_resource(Language, #resource{id = Id}) ->
  localized_resource:find([{resource_id, Id}, {language, Language}]).

prepare(Guid, Session) ->
  prepare(Guid, Session, Session:language()).

prepare(Guid, Session, Language) ->
  Resource = find_by_guid(Guid),
  case Resource:localized_resource(Language) of
    undefined -> exit(resource_undefined);
    LocalizedResource -> LocalizedResource:prepare(Session)
  end.

prepare_text_resource(Text, Session) ->
  prepare_text_resource(Text, Session:language(), Session).

prepare_text_resource(Text, Language, #session{pbx = Pbx, project = Project, js_context = JsContext}) ->
  ReplacedText = util:interpolate(Text, fun(VarNameBin) ->
    VarName = binary_to_atom(<<"var_", VarNameBin/binary>>, utf8),
    case erjs_context:get(VarName, JsContext) of
      undefined -> <<>>;
      X -> list_to_binary(X)
    end
  end),
  case Pbx:can_play({text, Language}) of
    false ->
      Name = util:md5hex(ReplacedText),
      TargetPath = Pbx:sound_path_for(Name),
      case filelib:is_file(TargetPath) of
        true -> ok;
        false -> ok = tts:synthesize(ReplacedText, Project, Language, TargetPath)
      end,
      {file, Name};
    true ->
      {text, Language, ReplacedText}
  end.

prepare_blob_resource(Name, Blob, #session{pbx = Pbx}) ->
  TargetPath = Pbx:sound_path_for(Name),
  case filelib:is_file(TargetPath) of
    true -> ok;
    false -> sox:convert(Blob, "wav", TargetPath)
  end,
  {file, Name}.

prepare_url_resource(Url, #session{pbx = Pbx}) ->
  Name = util:md5hex(Url),
  TargetPath = Pbx:sound_path_for(Name),
  case filelib:is_file(TargetPath) of
    true -> ok;
    false ->
      TempFile = TargetPath ++ ".tmp",
      try
        {ok, {_, Headers, Body}} = httpc:request(Url),
        file:write_file(TempFile, Body),
        Type = guess_type(Url, Headers),
        sox:convert(TempFile, Type, TargetPath)
      after
        file:delete(TempFile)
      end
  end,
  {file, Name}.

guess_type(Url, Headers) ->
  case proplists:get_value("content-type", Headers) of
    "audio/mpeg" -> "mp3";
    "audio/mpeg3" -> "mp3";
    "audio/x-mpeg-3" -> "mp3";
    "audio/wav" -> "wav";
    "audio/x-wav" -> "wav";
    _ -> case filename:extension(Url) of
      ".wav" -> "wav";
      ".mp3" -> "mp3";
      ".gsm" -> "gsm";
      _ -> throw("Unknown file type")
    end
  end.
