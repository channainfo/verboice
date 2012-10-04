module Ext
	class ReminderSchedule < ExtActiveRecord

		include ActiveModel::Validations
		#TODO : alias attribute for :date_time_format
		validates :client_start_date, :date_time => {:date_time_format => Ext::Util::DEFAULT_DATE_FORMAT, :field => :start_date } 
		validates :name, :presence => true
		validates :call_flow_id, :presence => true
		validates :channel_id, :presence => true
		validates :days, :presence => true , :if => Proc.new {|record| record.schedule_type != ReminderSchedule::TYPE_ONE_TIME }
		validates :recursion, :presence => true , :if => Proc.new {|record| record.schedule_type == ReminderSchedule::TYPE_WEEKLY }



		has_one :call_flow
		has_one :schedule
		has_one :channel

		belongs_to :project
		assign_has_many_to "Project", :ext_reminder_schedules, :class_name => "Ext::ReminderSchedule"

		TYPE_ONE_TIME = 1
		TYPE_DAILY   = 2
		TYPE_WEEKLY  = 3

		attr_accessor :client_start_date

		before_save :filter_start_date

		def self.schedule_today project_id
			project = Project.find(project_id)
			phone_books = project.ext_reminder_phone_books
			reminder_schedules = project.ext_reminder_schedules
			now = DateTime.now
			reminder_schedules.each do |reminder_schedule|
				self.process_reminder reminder,phone_books, now
			end
		end
		# run at Y-m-d , 00:00
		def self.process_reminder reminder, phone_books, now
			if reminder.schedule_type == ReminderSchedule::TYPE_ONE_TIME
				if self.is_same_day? reminder.start_date, now
					self.call
				end
			elsif reminder.schedule_type == ReminderSchedule::TYPE_DAILY
				if reminder.start_date <= now
					if self.in_schedule_day? reminder.days, now.wday 
						self.call
					end
				end
			elsif reminder.schedule_type == ReminderSchedule::TYPE_WEEKLY
				if reminder.start_date <= now
					if reminder.next_run == false
						if self.in_schedule_day? reminder.days, now.wday 

							self.call
							if self.last_day_of_week_running? reminder.days, now.wday
								reminder.update_next_run true
							end
						end		
					else
					   number_days  = ((now.to_i - reminder.start_date.to_i)/(3600*24)).to_i
					   period   = ( reminder.recursion - 1) 
					   left_day = (number_days) % ( period * 7 )
					   p "number of days #{number_days} "
					   p "left_day #{left_day}"
					   if  left_day < 7 && number_days > ( 7 * period )
					   	  self.call
					   	  if self.last_day_of_week_running? reminder.days, now.wday
							reminder.update_next_run true
						  else
						  	reminder.update_next_run false		
						  end
					   end
					end
				end
			end
		end

		def self.is_same_day? day1, day2
			day1.strftime("%Y-%m-%d") == day2.strftime("%Y-%m-%d")
		end

		def self.call

		end

		def update_next_run type
			self.next_run = type
			self.save
		end

		def self.in_schedule_day? day_list, day
			day_list.include? day.to_s
		end

		def self.last_day_of_week_running? day_list, day
			day_ar = day_list.split(",") 
			day_ar[day_ar.size-1] == day.to_s 
		end

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
			start_string = ReminderSchedule.filter_start self.start_date
			

			if self.schedule_type == ReminderSchedule::TYPE_ONE_TIME
			   "Call at #{time_string} on #{start_string} "
			else
				day_string   = ReminderSchedule.filter_day(self.days)    
				if self.schedule_type == ReminderSchedule::TYPE_DAILY
			   		"Start from #{start_string} Every week on #{day_string}, Call at #{time_string}"
				elsif self.schedule_type == ReminderSchedule::TYPE_WEEKLY
					"Start from #{start_string} Every #{self.recursion} weeks on #{day_string}. Call at #{time_string}"
				end
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