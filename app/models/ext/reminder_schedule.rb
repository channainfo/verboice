module Ext
	class ReminderSchedule < ExtActiveRecord
		has_one :call_flow

		belongs_to :project

		TYPE_ONE_TIME = 1
		TYPE_DAILY   = 2
		TYPE_WEEKLY  = 3
		
		assign_has_many_to "Project", :ext_reminder_schedules, :class_name => "Ext::ReminderSchedule"

		def start_date=(val)
			write_attribute(:start_date, Ext::Util.parse_date_time(val) )
		end

		def date_format_for_calendar
			if self.start_date
			    return Ext::Util.date_time_to_str(self.start_date) 
			end

		end
	end

end