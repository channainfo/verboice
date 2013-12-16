class AddDurationToCallLogs < ActiveRecord::Migration
  def up
    add_column :call_logs, :duration, :integer, null: false, default: 0

    # reset call log columns information
    CallLog.reset_column_information

    CallLog.find_each do |call_log|
      if call_log.finished_at && call_log.started_at
        call_log.duration = (call_log.finished_at - call_log.started_at).to_i
        call_log.save
      end
    end
  end

  def down
    remove_column :call_logs, :duration
  end
end
