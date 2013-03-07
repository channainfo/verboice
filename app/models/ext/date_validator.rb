module Ext 
  class DateValidator < ActiveModel::EachValidator
    # implement the method called during validation
    def validate_each(record, attribute, value)
      raise "#{self.class} #{I18n.t('activerecord.errors.models.ext/date_time_validator.attributes.date.require')} " if options[:date_format].nil?

      begin 
        Ext::Util.parse_date(record.send(attribute), options[:date_format] )
      rescue
        field = options[:field] || attribute
        record.send("#{field}=",nil);
        record.errors[field] <<  " #{I18n.t('activerecord.errors.models.ext/date_time_validator.attributes.date.incorrect_format')} "
      end 

    end
  end
    
end