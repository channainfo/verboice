class CreateExtReminderScheduleTable < ActiveRecord::Migration
  def change
  	create_table :ext_reminder_schedules do |t|
      t.string :name
      t.text :description
  		t.datetime :start_date
  		t.integer :type
  		t.integer :recursion
  		t.string  :days

  		t.references :call_flow
      t.references :project
  		t.timestamp 
  	end
  end

end
