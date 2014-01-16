module Ext
  class ReminderChannel < ExtActiveRecord
  	attr_accessible :reminder_schedule_id, :channel_id
    belongs_to :reminder_schedule, class_name: 'Ext::ReminderSchedule'
    belongs_to :channel, class_name: 'Channel'
  end
end

