require 'spec_helper'

describe Ext::ReminderSchedule  do
	before(:each) do
		@project = Project.make
      @call_flow= CallFlow.make :project_id => @project.id
      @channel = Channels::Custom.make :call_flow => @call_flow
      @reminder_phone_book_type = Ext::ReminderPhoneBookType.make
	end

	describe "Create new reminder schedule" do
	  before(:each) do
	  	@valid = {
	  		:name => "reminder 1",
	  		:schedule_type => Ext::ReminderSchedule::TYPE_ONE_TIME,
	  		:project_id => @project.id,
	  		:call_flow_id => @call_flow.id,
	  		:reminder_phone_book_type_id => @reminder_phone_book_type.id,
	  		:client_start_date => "2012-10-25",
	  		:channel_id => @channel.id,
	  		:schedule => nil,
	  		:time_from => "10:00",
	  		:time_to => "12:00"
	  	}
	  end	

	  it "should create a reminder schedule with valid attribute" do 
		reminder_schedule = Ext::ReminderSchedule.new @valid
		result = reminder_schedule.save
		result.should eq true

		reminder_schedule.schedule_type.should eq Ext::ReminderSchedule::TYPE_ONE_TIME
	  end

	  it "should require start_date with valid format" do
	     invalid = @valid.merge(:client_start_date => "")	
	     reminder_schedule  =  Ext::ReminderSchedule.new invalid
	     reminder_schedule.save().should eq false
	  end	

	  it "should require days if type is repeat" do
	  	invalid = @valid.merge(:days => "", :schedule_type => Ext::ReminderSchedule::TYPE_DAILY)	
	    reminder_schedule  =  Ext::ReminderSchedule.new invalid
	    reminder_schedule.save().should eq false
	  end

	end

end