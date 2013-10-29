-module(reminder_group).
-export([has_address/2, register_address/2, deregister_address/2]).
-define(TABLE_NAME, "ext_reminder_groups").

-include_lib("erl_dbmodel/include/model.hrl").

has_address(AddressBin, #reminder_group{addresses = AddrYaml}) ->
  Addresses = util:safe_load_yaml(AddrYaml),
  Address = binary_to_list(AddressBin),
  lists:member(Address, Addresses).

register_address(AddressBin, ReminderGroup = #reminder_group{addresses = AddrYaml}) ->
  case ReminderGroup:has_address(AddressBin) of
    true -> ReminderGroup;
    false ->
      Addresses = util:safe_load_yaml(AddrYaml),
      Address = binary_to_list(AddressBin),
      NewAddresses = lists:append(Addresses, [Address]),
      ReminderGroup#reminder_group{addresses = active_record_yaml:serialize(NewAddresses)}
  end.

deregister_address(AddressBin, ReminderGroup = #reminder_group{addresses = AddrYaml}) ->
  case ReminderGroup:has_address(AddressBin) of
    true -> 
      Addresses = util:safe_load_yaml(AddrYaml),
      Address = binary_to_list(AddressBin),
      ReminderGroup#reminder_group{addresses = active_record_yaml:serialize(lists:delete(Address, Addresses))};
    false ->
      ReminderGroup
  end.