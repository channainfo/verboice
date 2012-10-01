module Ext 
	class DateTimeValidator < ActiveModel::EachValidator
	  # implement the method called during validation
	  def validate_each(record, attribute, value)
	  	raise "#{self.class} require date_time_format options for date time " if options[:date_time_format].nil?

	  	begin 
	  		Ext::Util.parse_date_time(record.send(attribute), options[:date_time_format])
	  	rescue
	  		field = options[:field] || attribute
	  		record.send("#{field}=",nil);
	  		record.errors[field] <<  " in correct format "
	  	end	

	  end
	end
		
end