module Ext
  class PregnancyReminder < ExtActiveRecord
    belongs_to :call_flow
    belongs_to :project
    belongs_to :schedule
    belongs_to :channel

    validates :name, :presence => true
    validates :call_flow, :presence => true
    validates :project, :presence => true
    validates :channel, :presence => true
    validates :week, :presence => true
    validates :timezone, :presence => true
    validates :started_at, :presence => true

    serialize :queued_call_ids

    assign_has_many_to "Project", :ext_pregnancy_reminders, :class_name => "Ext::PregnancyReminder"
    # attr_accessible :title, :body

    after_create  :create_queues_call
    after_destroy :remove_queues_call

    def update_queues_call
      remove_queues_call
      create_queues_call
    end

    def create_queues_call
      now = DateTime.now.utc
      process project.ext_reminder_phone_books, now
    end

    def remove_queues_call
      if queued_call_ids
        remove_queued_call
        self.queued_call_ids = []
        self.save
      end
    end

    def remove_queued_call
      begin
        queued_calls = QueuedCall.find queued_call_ids
        queued_calls.each do |queued_call|
          queued_call.destroy
        end
      rescue Exception => e 
        p e.message
      end
    end

    def self.schedule project_id, date_time
      project = Project.find(project_id)

      phone_books = project.ext_reminder_phone_books
      pregnancy_reminders = project.ext_pregnancy_reminders

      pregnancy_reminders.each do |reminder|
        reminder.process phone_books, date_time
      end
    end

    def process phone_books, date_time
      options = call_options date_time, self.started_at
      queues = []
      phone_books.each do |phone_book|
        pregnancy_date = phone_book.patient.pregnancy_date
        diff_day = (Time.now.to_date - pregnancy_date).ceil
        if (diff_day % 7 == 0) && (diff_day / 7 == self.week)
          queued_call = call phone_book.phone_number, options
          queues << queued_call.id if queued_call
        end
      end
      self.queued_call_ids = queues
      self.save
      queues
    end

    def call address, options
      call_log = self.channel.call(address, options)
      raise call_log.fail_reason if call_log.fail_reason
      QueuedCall.find_by_call_log_id(call_log.id)
    end

    def call_options date, time
      time_in_zone = time.in_time_zone(self.timezone)
      not_before = DateTime.new(date.year, date.month, date.day, time_in_zone.hour, time_in_zone.min)

      options = { :call_flow_id  => self.call_flow_id,
            :project_id    => self.project_id,
            :time_zone     => self.timezone,
            :not_before    => not_before
      }

      options[:schedule_id] = self.schedule_id  if self.schedule_id
      options
    end

  end
end