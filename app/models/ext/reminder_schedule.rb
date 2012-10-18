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
		belongs_to :channel

		belongs_to :project
		assign_has_many_to "Project", :ext_reminder_schedules, :class_name => "Ext::ReminderSchedule"

		TYPE_ONE_TIME = 1
		TYPE_DAILY   = 2
		TYPE_WEEKLY  = 3

		attr_accessor :client_start_date

		before_save :filter_start_date

		def self.schedule project_id, at_time
			project = Project.find(project_id)

			phone_books 		= project.ext_reminder_phone_books
			reminder_schedules  = project.ext_reminder_schedules

			reminder_schedules.each do |reminder|
				self.process_reminder reminder, phone_books, at_time
			end
		end
		# run at Y-m-d , 00:00
		def self.process_reminder reminder, phone_books, now

			if reminder.schedule_type == ReminderSchedule::TYPE_ONE_TIME
				if self.is_same_day? reminder.start_date, now
					self.call reminder, phone_books 
				end
			elsif reminder.schedule_type == ReminderSchedule::TYPE_DAILY
				if reminder.start_date <= now
					if self.in_schedule_day? reminder.days, now.wday 
						self.call reminder, phone_books
					end
				end
			elsif reminder.schedule_type == ReminderSchedule::TYPE_WEEKLY
				if reminder.start_date <= now
					if self.in_schedule_day? reminder.days, now.wday 
						number_of_day_period = reminder.recursion * 7 
					  	ref_day = self.ref_offset_date reminder.days, reminder.start_date, now
					  	self.call(reminder, phone_books) if (ref_day && self.in_period?(now,ref_day, number_of_day_period))
					end
				end
			end
		end

		def self.in_period? now, ref_date, number_day_period
			current = DateTime.new(now.year, now.month, now.day)
			offset = DateTime.new(ref_date.year, ref_date.month, ref_date.day)

			number_days = (current.to_i - offset.to_i)/(24*3600)
			number_days % number_day_period == 0  
		end

		def self.ref_offset_date days, start_date, current
			#TODO add test
			days_list = self.days_list days, start_date
			days_list[current.wday.to_s]
		end

		def self.days_list days, start_date
			days_list = {}
			days.split(",").each  do |wday|
				diff = (start_date.wday - wday.to_i).abs 

				if start_date.wday > wday.to_i
					days_list[wday] = start_date - diff.days
				else
					days_list[wday] = start_date + diff.days	
				end
			end
			days_list
		end

		def self.is_same_day? day1, day2
			day1.strftime("%Y-%m-%d") == day2.strftime("%Y-%m-%d")
		end

		def self.call_options reminder, now
			start = reminder.start_date #.in_time_zone(reminder.timezone)
			
			not_before = DateTime.new(now.year, now.month, now.day, start.hour, start.min)

			options = { :call_flow_id  => reminder.call_flow_id ,
						:project_id    => reminder.project_id   ,
						:time_zone     => reminder.timezone     ,
						:not_before    => not_before
			}

			options[:schedule_id] = reminder.schedule_id  if reminder.schedule_id
			options
		end

		def self.call reminder, phone_books
			options = self.call_options reminder, DateTime.now
			queues = []
			phone_books.each do |phone_book| 
				address = phone_book.phone_number
				reminder.channel.call(address, options)
			end
			#queues
		end

		def self.in_schedule_day? day_list, day
			day_list.include? day.to_s
		end


		def filter_start_date
			#write_attribute(:start_date, Ext::Util.parse_date_time(val) )
			self.start_date = Ext::Util.parse_date_time(self.client_start_date, self.timezone) if !client_start_date.nil?	
		end
		
		def client_start_date
			@client_start_date
		end

		def client_start_date=(val)
			@client_start_date = val
		end

		def date_format_for_calendar
			if self.start_date
			    return Ext::Util.date_time_to_str(self.start_date, self.timezone)    
			end
		end

		def schedule_description
			time = self.start_date.in_time_zone self.timezone
			time_string  = ReminderSchedule.filter_time time
			start_string = ReminderSchedule.filter_start time

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