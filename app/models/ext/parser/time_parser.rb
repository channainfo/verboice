module Ext
  module Parser
    class TimeParser
      def self.parse time_string, format=Ext::ReminderSchedule::DEFAULT_DATE_TIME_FORMAT, timezone="UTC"
        time = Time.strptime time_string, format
        zone = ActiveSupport::TimeZone.new(timezone)
        ActiveSupport::TimeWithZone.new(nil, zone, time).to_time
      end
    end
  end
end