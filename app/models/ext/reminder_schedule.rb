module Ext
	class ReminderSchedule < ExtActiveRecord
		include ActiveModel::Validations

		serialize :queue_call_id

		#TODO : alias attribute for :date_time
		validates :client_start_date, :"ext/date" => {:date_format => Ext::Util::DEFAULT_DATE_FORMAT, :field => :start_date }

		validates :time_from, :presence => true
		validates :time_to, :presence => true

		validates :call_flow_id, :presence => true
		validates :channel_id, :presence => true
		validates :reminder_phone_book_type_id, :presence => true
		validates :days, :presence => true , :if => Proc.new {|record| record.schedule_type == ReminderSchedule::TYPE_DAILY }
		validates :recursion, :presence => true , :if => Proc.new {|record| record.schedule_type == ReminderSchedule::TYPE_DAILY }

		belongs_to :call_flow
		belongs_to :schedule
		belongs_to :channel
		belongs_to :reminder_phone_book_type

		belongs_to :project
		assign_has_many_to "Project", :ext_reminder_schedules, :class_name => "Ext::ReminderSchedule"

		TYPE_ONE_TIME = 0
		TYPE_DAILY   = 1
		# TYPE_WEEKLY  = 2

		attr_accessor :client_start_date, :client_time_from, :client_time_to

		before_save   :filter_date_time
		# after_create  :create_queue_call
		# after_destroy :remove_queues

		def create_queue_call
			now = DateTime.now.utc

			if( (start_date > now ) && ReminderSchedule.is_same_day?(start_date, now) )
				if schedule_type == ReminderSchedule::TYPE_ONE_TIME
					call(project.ext_reminder_phone_books, now) 
				elsif(in_schedule_day? now.wday)
					call(project.ext_reminder_phone_books, now) 
				end
			elsif start_date <= now 
				#hour and minute in the future
				if (start_date.hour * 60 + start_date.min) >= (now.hour * 60 + now.minute) 
					process_reminder project.ext_reminder_phone_books, now
				end
			end
		end

		def update_queues_call
			remove_queue_call
			create_queue_call
		end

		def remove_queues
			begin
				queues = QueuedCall.find queue_call_id
				queues.each do |queue|
					queue.destroy
				end
			rescue Exception => e	
				p e.message
			end
		end

		def remove_queue_call
			if queue_call_id
				remove_queues
				self.queue_call_id = []
				self.save
			end
		end

		def self.schedule project_id, at_time
			project = Project.find(project_id)

			phone_books 		= project.ext_reminder_phone_books
			reminder_schedules  = project.ext_reminder_schedules

			reminder_schedules.each do |reminder|
				reminder.process_reminder phone_books, at_time
			end
		end
		# run at Y-m-d , 00:00
		def process_reminder phone_books, now
			reminder = self
			if reminder.schedule_type == ReminderSchedule::TYPE_ONE_TIME
				if ReminderSchedule.is_same_day? reminder.start_date, now
					reminder.call phone_books, now
				end
			# elsif reminder.schedule_type == ReminderSchedule::TYPE_DAILY
			# 	if reminder.start_date <= now
			# 		if in_schedule_day? now.wday 
			# 			reminder.call phone_books, now
			# 		end
			# 	end
			# elsif reminder.schedule_type == ReminderSchedule::TYPE_WEEKLY
			else
				if reminder.start_date <= now
					if in_schedule_day? now.wday 
						number_of_day_period = reminder.recursion * 7 
					  	ref_day = ReminderSchedule.ref_offset_date reminder.days, reminder.start_date, now
					  	reminder.call(phone_books, now) if (ref_day && ReminderSchedule.in_period?(now,ref_day, number_of_day_period))
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
			end if days
			days_list
		end

		def self.is_same_day? day1, day2
			day1.strftime("%Y-%m-%d") == day2.strftime("%Y-%m-%d")
		end

		def call_options now
			start = self.start_date #.in_time_zone(reminder.timezone)
			
			not_before = DateTime.new(now.year, now.month, now.day, start.hour, start.min)

			options = { :call_flow_id  => self.call_flow_id ,
						:project_id    => self.project_id   ,
						:time_zone     => self.timezone     ,
						:not_before    => not_before
			}

			options[:schedule_id] = self.schedule_id  if self.schedule_id
			options
		end

		def call phone_books, now
			options = call_options now
			queues = []
			phone_books.each do |phone_book| 
				address = phone_book.phone_number
				call_log = self.channel.call(address, options)
				raise call_log.fail_reason if call_log.fail_reason
				queue =  QueuedCall.find_by_call_log_id(call_log.id) 
				queues << queue.id if queue
			end
			self.queue_call_id = queues
			self.save
			queues
		end

		def in_schedule_day? day
			days.nil? ? false : days.include?(day.to_s) 
		end

		def filter_date_time
			# self.time_from = Ext::Util.parse_date_time(self.client_time_from, self.timezone) unless client_time_from.nil?
			# self.time_to = Ext::Util.parse_date_time(self.client_time_to, self.timezone) unless client_time_to.nil?
			#write_attribute(:start_date, Ext::Util.parse_date_time(val) )
			self.start_date = Ext::Util.parse_date(self.client_start_date) unless client_start_date.nil?	
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
				# if self.schedule_type == ReminderSchedule::TYPE_DAILY
			 #   		"Start from #{start_string} Every week on #{day_string}, Call at #{time_string}"
				# elsif self.schedule_type == ReminderSchedule::TYPE_WEEKLY
					"Start from #{start_string} Every #{self.recursion} weeks on #{day_string}. Call at #{time_string}"
				# end
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