module Ext
  class DateParser
    def self.parse date_string, format=Ext::ReminderSchedule::DEFAULT_DATE_FORMAT
      Date.strptime date_string, format
    end
  end
end