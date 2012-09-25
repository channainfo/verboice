module Ext
	class ReminderPhoneBook < ActiveRecord::Base
		belongs_to :project, :class_name => "::Project"

		validates :name, :phone_number, :presence => true
		validates :phone_number, :uniqueness => true

	end
end