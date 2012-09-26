module Ext
	class ExtActiveRecord < ActiveRecord::Base
	  self.abstract_class = true
	  self.table_name_prefix = 'ext_'
	end
end