module Ext
	class ReminderSchedule < ExtActiveRecord
		include ActiveModel::Validations

		DEFAULT_DATE_FORMAT  = '%Y-%m-%d'
 		DEFAULT_DATE_TIME_FORMAT  = '%Y-%m-%d %H:%M'

		serialize :queue_call_id
		serialize :conditions, Array
		serialize :schedule, IceCube::Schedule

		#TODO : alias attribute for :date_time
		validates :client_start_date, :"ext/validator/date" => {:date_format => Ext::ReminderSchedule::DEFAULT_DATE_FORMAT, :field => :start_date }

		validates :time_from, :presence => true
		validates :time_to, :presence => true

		validates :call_flow_id, :presence => true
		validates :channel_id, :presence => true
		validates :reminder_group_id, :presence => true
		validates :days, :presence => true , :if => Proc.new {|record| record.schedule_type == ReminderSchedule::TYPE_DAILY }
		validates :recursion, :presence => true , :if => Proc.new {|record| record.schedule_type == ReminderSchedule::TYPE_DAILY }

		belongs_to :call_flow
		belongs_to :channel
		belongs_to :reminder_group
		belongs_to :reminder_phone_book_type

		belongs_to :project
		assign_has_many_to "Project", :ext_reminder_schedules, :class_name => "Ext::ReminderSchedule"

		TYPE_ONE_TIME = 0
		TYPE_DAILY   = 1
		# TYPE_WEEKLY  = 2

		attr_accessor :client_start_date

		before_save   :initialize_schedule
		after_create  :create_queues_call
		after_destroy :remove_queues

		def initialize_schedule
			validate_and_create_start_date
			from_date_time = Ext::Parser::DateTimeParser.parse("#{start_date.to_s} #{time_from}", ReminderSchedule::DEFAULT_DATE_TIME_FORMAT, project.time_zone)
			to_date_time = Ext::Parser::DateTimeParser.parse("#{start_date.to_s} #{time_to}", ReminderSchedule::DEFAULT_DATE_TIME_FORMAT, project.time_zone)
			
			if schedule_type == ReminderSchedule::TYPE_DAILY
				rule = IceCube::Rule.weekly(recursion)
				days.split(",").each do |wday|
					rule.day Ext::Weekday.new(wday).symbol
				end
			end

			self.schedule = IceCube::Schedule.new(start = from_date_time, :duration => to_date_time.to_i - from_date_time.to_i)
			self.schedule.add_recurrence_rule rule if rule
		end

		def create_queues_call
			process reminder_group.addresses, DateTime.now.utc.in_time_zone(project.time_zone) if reminder_group and reminder_group.has_addresses?
		end

		def update_queues_call
			remove_queue_call
			create_queues_call
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
			project.ext_reminder_schedules.each do |reminder_schedule|
				addresses = reminder_schedule.reminder_group.addresses if reminder_schedule.reminder_group && reminder_schedule.reminder_group.has_addresses?
				addresses = [] if addresses.nil?
				reminder_schedule.process addresses, at_time.in_time_zone(project.time_zone), true unless addresses.empty?
			end
		end

		def process addresses, running_time, is_schedule = false
			if in_schedule_date? running_time.to_date
				if running_time.to_date.equal? start_date
					from_date_time = Ext::Parser::DateTimeParser.parse("#{start_date.to_s} #{time_from}", ReminderSchedule::DEFAULT_DATE_TIME_FORMAT, project.time_zone)
					to_date_time = Ext::Parser::DateTimeParser.parse("#{start_date.to_s} #{time_to}", ReminderSchedule::DEFAULT_DATE_TIME_FORMAT, project.time_zone)
					should_enqueue = true if from_date_time.greater_or_equal?(running_time) || running_time.between?(from_date_time, to_date_time) || is_schedule
				elsif running_time.to_date.greater_than?(start_date)
					should_enqueue = true if schedule_type == ReminderSchedule::TYPE_DAILY
				end

				if should_enqueue
					phone_numbers = callers_matches_conditions addresses
					enqueued_call(phone_numbers, running_time) unless phone_numbers.empty?
				end
			end
		end

		def call_options at_time
			call_time_string = "#{at_time.to_string(Date::DEFAULT_FORMAT)} #{time_from}"

			not_before = Ext::Parser::DateTimeParser.parse(call_time_string, ReminderSchedule::DEFAULT_DATE_TIME_FORMAT, self.project.time_zone)

			options = { 
				:call_flow_id => self.call_flow_id,
				:project_id => self.project_id,
				:time_zone => self.project.time_zone,
				:not_before => not_before
			}
		end

		def enqueued_call addresses, at_time
			options = call_options at_time
			queues = []
			addresses.each do |address|
				call_log = self.channel.call(address, options)
				raise call_log.fail_reason if call_log.fail_reason
				queue =  QueuedCall.find_by_call_log_id(call_log.id) 
				queues << queue.id if queue
			end
			self.queue_call_id = queues
			self.save
			queues
		end

		def in_schedule_date? date
			schedule.occurs_on? date if schedule
		end

		def validate_and_create_start_date
			self.start_date = Ext::Parser::DateParser.parse(self.client_start_date) unless client_start_date.nil?
		end

		def client_start_date
			@client_start_date
		end

		def client_start_date=(val)
			@client_start_date = val
		end

		def has_conditions?
			exists = false
			exists = true if !conditions.nil? && !conditions.empty?
			exists
		end

		def callers_matches_conditions addresses
			phone_numbers = []
			if has_conditions?
				addresses.each do |address|
					contact = Contact.where(:address => address, :project_id => project.id).last
					phone_numbers.push address if contact and contact.evaluate? conditions
				end
			else
				phone_numbers = addresses
			end
			phone_numbers
		end

	end

end