require 'spec_helper'

describe Ext::ReminderSchedule  do
	describe "Create new reminder schedule" do
	  before(:each) do

	  	@project = Project.make
        @call_flow= CallFlow.make :project_id => @project.id

        @channel = Channels::Custom.make :call_flow => @call_flow

	  	@valid = {
	  		:name => "reminder 1",
	  		:schedule_type => Ext::ReminderSchedule::TYPE_ONE_TIME,
	  		:project_id => @project.id,
	  		:call_flow_id => @call_flow.id,
	  		:client_start_date => "10/25/2012 09:20",
	  		:channel_id => @channel.id
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

	  it "should require start_date with valid format" do
	     invalid = @valid.merge(:client_start_date => "")	
	     reminder_schedule  =  Ext::ReminderSchedule.new invalid
	     reminder_schedule.save().should eq false
	  end	
	end

	describe "ReminderSchedule.filter_day"  do
	   it "should return a day string of given day" do

	   		days = [
	   			{ :format => "0,1,2", :result => "Sun, Mon, Tue" },
	   			{ :format => "2,3", :result => "Tue, Wed" },
	   			{ :format => "6", :result => "Sat" }
	   		].each do |elm|
	   			result = Ext::ReminderSchedule.filter_day elm[:format]
	   			result.should eq elm[:result]
	   		end
	   end
	end





  
end