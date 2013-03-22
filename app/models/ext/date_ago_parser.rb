module Ext
  class DateAgoParser
    def self.parse number, date_unit
      "Ext::Date::#{date_unit.camelize}Ago".constantize.new number
    end
  end
end