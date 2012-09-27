require 'spec_helper'

describe Ext::ReminderSchedule  do
	describe "Create new reminder schedule" do
	  before(:each) do

	  	@project = Project.make
        @call_flow= CallFlow.make :project_id => @project.id



	  	@valid = {
	  		:name => "reminder 1",
	  		:type => Ext::ReminderSchedule::TYPE_ONE_TIME,
	  		:project_id => @project.id,
	  		:call_flow_id => @call_flow.id,
	  		:start_date => "10/25/2012 09:20"
	  	}
	  end	
	  it "should create a reminder schedule with valid attribute" do 
		reminder_schedule = Ext::ReminderSchedule.new @valid
		reminder_schedule.save.should eq true
		reminder_schedule.start_date.should eq "10/25/2012 09:20"
	  end	
	end
  
end