module Ext
	class ReminderPhoneBook < ExtActiveRecord
		belongs_to :project, :class_name => "::Project"

		assign_has_many_to "Project" ,:ext_reminder_phone_books, :class_name => "Ext::ReminderPhoneBook"

		validates :name, :phone_number, :presence => true
		validates :phone_number, :uniqueness => true

	end
end