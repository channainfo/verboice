module Ext
	class ReminderSchedule < ExtActiveRecord

		include ActiveModel::Validations

		validates :client_start_date, :date_time => {:date_time_format => Ext::Util::DEFAULT_DATE_FORMAT, :field => :start_date }
		validates :name, :presence => true

		has_one :call_flow
		belongs_to :project


		TYPE_ONE_TIME = 1
		TYPE_DAILY   = 2
		TYPE_WEEKLY  = 3

		attr_accessor :client_start_date

		before_save :filter_start_date
		
		assign_has_many_to "Project", :ext_reminder_schedules, :class_name => "Ext::ReminderSchedule"

		def filter_start_date
			#write_attribute(:start_date, Ext::Util.parse_date_time(val) )
			self.start_date = Ext::Util.parse_date_time(client_start_date) if !client_start_date.nil?
		end
		
		def client_start_date
			@client_start_date
		end

		def client_start_date=(val)
			@client_start_date = val
		end

		def date_format_for_calendar
			if self.start_date
			    return Ext::Util.date_time_to_str(self.start_date)    
			end

		end
	end

end