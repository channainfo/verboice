module Ext
  class TimeParser
    def self.parse date_time_string, format=Ext::ReminderSchedule::DEFAULT_DATE_TIME_FORMAT, timezone="UTC"
      time = Time.strptime date_time_string, format
      zone = ActiveSupport::TimeZone.new(timezone)
      Time.new(time.year, time.month, time.day, time.hour, time.min, time.sec, zone.utc_offset)
    end
  end
end