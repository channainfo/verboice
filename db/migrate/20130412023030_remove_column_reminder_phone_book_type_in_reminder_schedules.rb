class RemoveColumnReminderPhoneBookTypeInReminderSchedules < ActiveRecord::Migration
  def up
    remove_column :ext_reminder_schedules, :reminder_phone_book_type_id
  end

  def down
    add_column :ext_reminder_schedules, :reminder_phone_book_type_id, :integer
  end
end
