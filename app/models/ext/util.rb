module Ext

 class Util

 	DEFAULT_DATE_FORMAT  = '%m/%d/%Y %H:%M'

 	def self.parse_date_time(str, format = Util::DEFAULT_DATE_FORMAT )
		DateTime.strptime(str, format)
	end

	def self.date_time_to_str(date_time, format = Util::DEFAULT_DATE_FORMAT )	
		date_time.strftime(format) 
	end
 end

end