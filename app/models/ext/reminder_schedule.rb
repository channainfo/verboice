module Ext
	class ReminderSchedule < ExtActiveRecord

		include ActiveModel::Validations
		validates :client_start_date, :date_time => {:date_time_format => Ext::Util::DEFAULT_DATE_FORMAT, :field => :start_date }
		validates :name, :presence => true

		has_one :call_flow
		belongs_to :project
		assign_has_many_to "Project", :ext_reminder_schedules, :class_name => "Ext::ReminderSchedule"

		TYPE_ONE_TIME = 1
		TYPE_DAILY   = 2
		TYPE_WEEKLY  = 3

		attr_accessor :client_start_date

		before_save :filter_start_date

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

		def schedule_description
			time_string  = ReminderSchedule.filter_time(self.start_date)
			day_string   = ReminderSchedule.filter_day(self.days)
			start_string = ReminderSchedule.filter_start self.start_date

			if self.schedule_type == ReminderSchedule::TYPE_ONE_TIME
			   "Call at #{time_string} on #{start_string} "
			elsif self.schedule_type == ReminderSchedule::TYPE_DAILY
			   "Start from #{start_string} Every week on #{day_string}, Call at #{time_string}"
			elsif self.schedule_type == ReminderSchedule::TYPE_WEEKLY
				"Start from #{start_string} Every #{self.recursion} weeks on #{day_string}. Call at #{time_string}"
			end	
		end

		def self.filter_start ar_time
			ar_time.to_datetime.strftime("%m/%d/%Y") 
		end

		def self.filter_time ar_time
			ar_time.to_datetime.strftime("%I:%M %P") 
		end

		def self.filter_day days_format
		   d = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"]	
		   days_format.split(",").map{|num| d[num.to_i]}.join(", ")	
		end
	end

end