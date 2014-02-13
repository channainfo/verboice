class AddStoreCallLogEntriesToProject < ActiveRecord::Migration
  def up
    add_column :projects, :store_call_log_entries, :boolean, default: true
  end

  def down
    remove_column :projects, :store_call_log_entries
  end
end
