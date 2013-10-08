-module(persist_variable).
-export([run/2]).
-include("session.hrl").
-include("db.hrl").

run(Args, Session = #session{js_context = JS}) ->
  Name = proplists:get_value(name, Args),
  Expression = proplists:get_value(expression, Args),
  Type = proplists:get_value(type, Args),
  {Value, JS2} = erjs:eval(Expression, JS),

  TypeBin = list_to_binary(Type),
  NewValue = case TypeBin of
    <<>> -> Value;
    UnitBin ->
      Today = erlang:date(),
      NewDate = time_ago(Value, Today, UnitBin),
      edate:date_to_string(NewDate)
  end,

  PersistedVar = (find_or_create_persisted_variable(Name, Session))#persisted_variable{value = NewValue},
  PersistedVar:save(),

  VarName = list_to_atom("var_" ++ Name),
  JS3 = erjs_context:set(VarName, NewValue, JS2),
  {next, Session#session{js_context = JS3}}.

find_or_create_persisted_variable(Name, #session{contact = Contact, project = Project}) ->
  case Name of
    "language" ->
      persisted_variable:find_or_new([
        {contact_id, Contact#contact.id},
        {implicit_key, "language"}
      ]);
    _ ->
      #project_variable{id = ProjectVarId} = project_variable:find([{project_id, Project#project.id}, {name, Name}]),
      persisted_variable:find_or_new([
        {contact_id, Contact#contact.id},
        {project_variable_id, ProjectVarId}
      ])
  end.

time_ago(NumberAgo, FromDate = {_, _, _}, UnitBin) ->
  NewDate = case UnitBin of
    <<>> -> erlang:localtime();
    <<"Day">> -> edate:shift(FromDate, -list_to_integer(NumberAgo), day);
    <<"Week">> -> edate:shift(FromDate, -list_to_integer(NumberAgo), week);
    <<"Month">> -> edate:shift(FromDate, -list_to_integer(NumberAgo), month);
    <<"Year">> -> edate:shift(FromDate, -list_to_integer(NumberAgo), year)
  end,

  NewDate.