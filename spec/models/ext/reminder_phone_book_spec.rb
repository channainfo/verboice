require "spec_helper"

describe Ext::ReminderPhoneBook  do
	before(:each) do
		type = Ext::ReminderPhoneBookType.make
		@valid = {
			:name => "Reminder",
			:phone_number => "123456",
			:type_id => type.id
		}
	end

	it "should create a new reminder phone book with valid attributes" do
		reminder = Ext::ReminderPhoneBook.new @valid
		reminder.save.should eq true
	end

	it "should require name" do
		attrs = @valid.merge(:name => "")
		reminder = Ext::ReminderPhoneBook.new attrs
		reminder.save.should eq false
	end

	it "should require phone number" do
		attrs = @valid.merge(:phone_number => "")
		reminder = Ext::ReminderPhoneBook.new attrs
		reminder.save.should eq false
	end

	it "should require type" do
		attrs = @valid.merge(:type_id => nil)
		reminder = Ext::ReminderPhoneBook.new attrs
		reminder.save.should eq false
	end

	it "should require phone number to be unique" do
		reminder = Ext::ReminderPhoneBook.new @valid
		reminder.save.should eq true
		reminder = Ext::ReminderPhoneBook.new @valid
		reminder.save.should eq false
	end

end