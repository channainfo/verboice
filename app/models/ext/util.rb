module Ext

 class Util

  DEFAULT_DATE_FORMAT  = '%Y-%m-%d'
 	DEFAULT_DATE_TIME_FORMAT  = '%m/%d/%Y %H:%M'

  def self.parse_date date_str, format = Util::DEFAULT_DATE_FORMAT
    Date.strptime date_str, format
  end

 	def self.parse_date_time str, time_zone="UTC", format = Util::DEFAULT_DATE_TIME_FORMAT 
 		time_zone = time_zone ? time_zone : "UTC"
		start = DateTime.strptime str, format
		zone = ActiveSupport::TimeZone.new(time_zone)
		Time.new(start.year, start.month, start.day, start.hour, start.min, start.second, zone.utc_offset)
	end

	def self.date_time_to_str date_time, time_zone = "UTC", format = Util::DEFAULT_DATE_TIME_FORMAT 
		zone = date_time.in_time_zone(time_zone)
		zone.strftime(format) 
	end
 end

end