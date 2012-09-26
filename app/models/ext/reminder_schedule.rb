module Ext
	class ReminderSchedule < ExtActiveRecord
		has_one :call_flow

		belongs_to :project

		TYPE_ONE_TIME = 1
		TYPE_DAILY   = 2
		TYPE_WEEKLY  = 3
		
		assign_has_many_to "Project", :ext_reminder_schedules, :class_name => "Ext::ReminderSchedule"
	end

end