module Ext
 class Util
 	def self.parse_date_time(str)
		DateTime.strptime(str, '%m/%d/%Y %H:%M')
	end

	def self.date_time_to_str(date_time)	
		date_time.strftime('%m/%d/%Y %H:%M') 
	end
 end

end