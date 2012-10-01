require 'spec_helper'

describe Ext::ReminderSchedule  do
	describe "Create new reminder schedule" do
	  before(:each) do

	  	@project = Project.make
        @call_flow= CallFlow.make :project_id => @project.id



	  	@valid = {
	  		:name => "reminder 1",
	  		:schedule_type => Ext::ReminderSchedule::TYPE_ONE_TIME,
	  		:project_id => @project.id,
	  		:call_flow_id => @call_flow.id,
	  		:client_start_date => "10/25/2012 09:20"
	  	}
	  end	
	  it "should create a reminder schedule with valid attribute" do 
		reminder_schedule = Ext::ReminderSchedule.new @valid
		result = reminder_schedule.save
		result.should eq true

		reminder_schedule.date_format_for_calendar.should eq "10/25/2012 09:20"
		reminder_schedule.schedule_type.should eq Ext::ReminderSchedule::TYPE_ONE_TIME
	  end

	  it "should require name" do
	  	invalid = @valid.merge(:name => "")	
	    reminder_schedule  =  Ext::ReminderSchedule.new invalid
	    reminder_schedule.save().should eq false
	  end

	  it "should require start when save new record" do
	     invalid = @valid.merge(:client_start_date => "")	
	     reminder_schedule  =  Ext::ReminderSchedule.new invalid
	     reminder_schedule.save().should eq false
	  end	
	end


  
end