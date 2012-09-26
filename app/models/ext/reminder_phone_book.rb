module Ext
	class ReminderPhoneBook < ExtActiveRecord
		belongs_to :project, :class_name => "::Project"

		has_many_reverse "Project" ,:ext_reminder_phone_books, :class_name => "Ext::ReminderPhoneBook"

		validates :name, :phone_number, :presence => true
		validates :phone_number, :uniqueness => true

	end
end