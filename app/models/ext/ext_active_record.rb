module Ext
	class ExtActiveRecord < ActiveRecord::Base
	  self.abstract_class = true
	  self.table_name_prefix = 'ext_'


	  def self.assign_has_many_to class_name, associats, options = {}
	  	 class_name.constantize.instance_eval do
	  	 	has_many associats, options
	  	 end
	  end


	end
end