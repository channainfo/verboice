class MigratePersistedVariablesDateDataToFormatDdMmYy < ActiveRecord::Migration
  def up
    PersistedVariable.find_each do |persisted_variable|
      if persisted_variable.value.try(:old_date_format?)
        new_date_string = Ext::Parser::DateParser.parse(persisted_variable.value, Date::PREVIOUS_DEFAULT_FORMAT).to_string(Date::DEFAULT_FORMAT)
        persisted_variable.value = new_date_string
        persisted_variable.save
      end
    end
  end

  def down
    PersistedVariable.find_each do |persisted_variable|
      if persisted_variable.value.try(:date_format?)
        new_date_string = Ext::Parser::DateParser.parse(persisted_variable.value, Date::DEFAULT_FORMAT).to_string(Date::PREVIOUS_DEFAULT_FORMAT)
        persisted_variable.value = new_date_string
        persisted_variable.save
      end
    end
  end
end
