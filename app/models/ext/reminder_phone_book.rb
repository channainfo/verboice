module Ext
	class ReminderPhoneBook < ExtActiveRecord
		belongs_to :project, :class_name => "::Project"
    has_one :patient

		assign_has_many_to "Project" ,:ext_reminder_phone_books, :class_name => "Ext::ReminderPhoneBook"

		validates :name, :phone_number, :presence => true
		validates :phone_number, :uniqueness => true

    before_destroy :destroy_patient

    private
    def destroy_patient
      self.patient.destroy if self.patient
    end
	end
end