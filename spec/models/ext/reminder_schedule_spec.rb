require 'spec_helper'

describe Ext::ReminderSchedule  do
	before(:each) do
		@project = Project.make
        @call_flow= CallFlow.make :project_id => @project.id
        @channel = Channels::Custom.make :call_flow => @call_flow
	end

	describe "Create new reminder schedule" do
	  before(:each) do

	  	@valid = {
	  		:name => "reminder 1",
	  		:schedule_type => Ext::ReminderSchedule::TYPE_ONE_TIME,
	  		:project_id => @project.id,
	  		:call_flow_id => @call_flow.id,
	  		:client_start_date => "10/25/2012 09:20",
	  		:channel_id => @channel.id,
	  		:schedule => nil
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

	  it "should require days if type is diff from onetime " do
	  	invalid = @valid.merge(:days => "", :schedule_type => Ext::ReminderSchedule::TYPE_WEEKLY )	
	    reminder_schedule  =  Ext::ReminderSchedule.new invalid
	    reminder_schedule.save().should eq false
	  end
	end

	describe "Reminder.schedule" do
		it "should process all reminder with phone books" do
		  	attr = {
		  		:name => "reminder",
		  		:schedule_type => Ext::ReminderSchedule::TYPE_ONE_TIME,
		  		:project_id => @project.id,
		  		:call_flow_id => @call_flow.id,
		  		:client_start_date => "10/25/2012 09:20",
		  		:channel_id => @channel.id,
		  		:schedule => nil
		  	}

		  	reminder_schedules = []
		  	phone_books = []

		  	3.times.each do |i|
	          reminder_schedules << Ext::ReminderSchedule.make(attr.merge(:name => "#{attr[:name]}-#{i}"))
	          phone_books << Ext::ReminderPhoneBook.make(:project_id => @project.id)
	        end

	        at_time = DateTime.new(2012,10,25, 9,21)
	        
	        Ext::ReminderSchedule.should_receive(:process_reminder).exactly(3).times
	        Ext::ReminderSchedule.schedule(@project.id, at_time)    
	    end	
	end

	describe "ReminderSchedule.call" do
		it "should enqueue call to verboice call_queue " do


			reminder = Ext::ReminderSchedule.make(  :name => "reminder",
											  		:schedule_type => Ext::ReminderSchedule::TYPE_ONE_TIME,
											  		:project_id => @project.id,
											  		:call_flow_id => @call_flow.id,
											  		:client_start_date => "10/25/2012 09:20",
											  		:channel_id => @channel.id,
											  		:schedule => nil )
			phone_books = []
		  	6.times.each do |i|
	          phone_books << Ext::ReminderPhoneBook.make(:project_id => @project.id)
	        end
		   	queues = Ext::ReminderSchedule.call(reminder, phone_books)
		   	queues.size.should eq 6

		end
	end

	describe "ReminderSchedule.create_call_options" do
		it "should create options for schedule call " do
			reminder = Ext::ReminderSchedule.make(  :name => "reminder",
											  		:schedule_type => Ext::ReminderSchedule::TYPE_ONE_TIME,
											  		:project_id => @project.id,
											  		:call_flow_id => @call_flow.id,
											  		:client_start_date => "10/18/2012 17:20",
											  		:channel_id => @channel.id,
											  		:schedule => nil,
											  		:timezone => "Bangkok" 
											  		)
			options = Ext::ReminderSchedule.call_options reminder, DateTime.new(2012,10,22)

			options[:call_flow_id].should eq reminder.call_flow_id 
			options[:project_id].should eq reminder.project_id   
			options[:time_zone].should  eq reminder.timezone 
			options[:not_before].should eq DateTime.new(2012,10,22,10,20)
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

	describe "ReminderSchedule.in_schedule_day? " do
		it "should tell if a datetime in days of reminder schedule days" do
			days = "0,2,3"
			Ext::ReminderSchedule.in_schedule_day?(days, DateTime.new(2012,10,7).wday).should eq true
			Ext::ReminderSchedule.in_schedule_day?(days, DateTime.new(2012,10,8).wday).should eq false
			Ext::ReminderSchedule.in_schedule_day?(days, DateTime.new(2012,10,9).wday).should eq true
			Ext::ReminderSchedule.in_schedule_day?(days, DateTime.new(2012,10,10).wday).should eq true
			Ext::ReminderSchedule.in_schedule_day?(days, DateTime.new(2012,10,11).wday).should eq false
		end
	end 

	describe "ReminderSchedule.last_day_of_week" do
	  it "should return list of week days with its related datetime" do
			result = {
				"0" => DateTime.new(2012,10,14), 
				"1" => DateTime.new(2012,10,15), 
				"5" => DateTime.new(2012,10,19) 
			}
			Ext::ReminderSchedule.days_list("0,1,5", DateTime.new(2012,10,16)).should eq result

	  end	
	end

	describe "ReminderSchedule.in_period?" do
		it "should return if in the same period" do
			[{:current =>DateTime.new(2012,10,25,10,10), :offset =>DateTime.new(2012,10,11),:period=> 14,:result =>true},
				{:current =>DateTime.new(2012,10,25,10,10), :offset =>DateTime.new(2012,10,18),:period=> 7, :result =>true},
				{:current =>DateTime.new(2012,10,25,10,10), :offset =>DateTime.new(2012,10,4),:period=> 21, :result =>true},
				{:current =>DateTime.new(2012,11,8,10,10), :offset =>DateTime.new(2012,10,11),:period=> 14,:result =>true},
				{:current =>DateTime.new(2012,11,8,0,0), :offset =>DateTime.new(2012,10,18),:period=> 14,:result => false},
				{:current =>DateTime.new(2012,11,8,0,0), :offset =>DateTime.new(2012,10,25),:period=> 21,:result => false},

			].each do |item|
				Ext::ReminderSchedule.in_period?(item[:current], item[:offset], item[:period]).should eq item[:result]
			end
		end
	end


	describe "ReminderSchedule.ref_day" do
		it "should return ref offset day if current is in the same wday" do
			ref_day =	Ext::ReminderSchedule.ref_offset_date "0,1,5", DateTime.new(2012,10,16) , DateTime.new(2012,10,29)
			ref_day.should eq DateTime.new(2012, 10,15)
		end

		it "should return nil if the current wday and start_date wday are different" do
			ref_day =	Ext::ReminderSchedule.ref_offset_date "0,1,5", DateTime.new(2012,10,16) , DateTime.new(2012,10,31)
			ref_day.should be_nil
		end 
	end

	describe "ReminderSchedule.alert_call to user in phonebook" do
		before(:each) do
			@phone_books = []
			
			10.times.each do
				@phone_books << Ext::ReminderPhoneBook.make
			end
		end
		describe "ReminderSchedule.process_reminder one time " do

			it "should call to all phone book of the project" do
				reminder_one_time = Ext::ReminderSchedule.make(:schedule_type => Ext::ReminderSchedule::TYPE_ONE_TIME,
													  		:project_id => @project.id,
													  		:call_flow_id => @call_flow.id,
													  		:channel_id => @channel.id,
													  		:schedule => nil,
													  		:client_start_date => Ext::Util.date_time_to_str(DateTime.now)
													  		)

				Ext::ReminderSchedule.should_receive(:call).with(reminder_one_time, @phone_books)
				Ext::ReminderSchedule.process_reminder(reminder_one_time, @phone_books, DateTime.now)

			end

			it "should not call to any phone book " do

				[ Ext::ReminderSchedule.make(:schedule_type => Ext::ReminderSchedule::TYPE_ONE_TIME,
													  		:project_id => @project.id,
													  		:call_flow_id => @call_flow.id,
													  		:channel_id => @channel.id,
													  		:schedule => nil,
													  		:client_start_date => Ext::Util.date_time_to_str(DateTime.now-1.day)
													  		),
				Ext::ReminderSchedule.make(:schedule_type => Ext::ReminderSchedule::TYPE_ONE_TIME,
													  		:project_id => @project.id,
													  		:call_flow_id => @call_flow.id,
													  		:channel_id => @channel.id,
													  		:schedule => nil,
													  		:client_start_date => Ext::Util.date_time_to_str(DateTime.now + 1.day)
													  		) 
				].each do |reminder|
					Ext::ReminderSchedule.should_receive(:call).never
					Ext::ReminderSchedule.process_reminder(reminder, @phone_books, DateTime.now)
				end
				
			end
		end

		describe "ReminderSchedule.process_reminder daily " do
		  before(:each) do
		  	@reminder_daily = Ext::ReminderSchedule.make(   :schedule_type => Ext::ReminderSchedule::TYPE_DAILY,
													  		:project_id => @project.id,
													  		:call_flow_id => @call_flow.id,
													  		:channel_id => @channel.id,
													  		:schedule => nil,
													  		:days => "0,3,4,6" ,
													  		:client_start_date => "10/15/2012 00:00"
													  		)
		  end

		  it "should not call at all " do

		  	[ DateTime.new(2012, 10, 7),
		  	  DateTime.new(2012, 10, 14), 
		  	  DateTime.new(2012, 10, 15) , 
		  	  DateTime.new(2012, 10, 16) , 
		  	  DateTime.new(2012, 10, 19) 
		  	].each do |current_date|
			    Ext::ReminderSchedule.should_receive(:call).never
				Ext::ReminderSchedule.process_reminder(@reminder_daily, @phone_books, current_date)
			end
		  end

		  it "should call to all phone books" do
		  	[ DateTime.new(2012, 10, 17), 
		  	  DateTime.new(2012, 10, 18) , 
		  	  DateTime.new(2012, 10, 20) 
		  	].each do |current_date|
			    Ext::ReminderSchedule.should_receive(:call).with(@reminder_daily, @phone_books)
				Ext::ReminderSchedule.process_reminder(@reminder_daily, @phone_books, current_date)
			end
		  end
		end

		describe "ReminderSchedule.process_reminder weekly " do

			describe "ReminderSchedule.process_reminder weekly first run " do
				before(:each ) do
					@reminder_weekly = Ext::ReminderSchedule.make(	:schedule_type => Ext::ReminderSchedule::TYPE_WEEKLY,
															  		:project_id => @project.id,
															  		:call_flow_id => @call_flow.id,
															  		:channel_id => @channel.id,
															  		:schedule => nil,
															  		:days => "0,1,2,6",
															  		:recursion => 3,
															  		:client_start_date => "10/15/2012 00:00",
														  		)
				end

				it "should not call at all " do 
				  [ 
				  	DateTime.new(2012, 10, 14),
				  	DateTime.new(2012, 10, 17), 
				  	DateTime.new(2012, 10, 18) , 
				  	DateTime.new(2012, 10, 19),
				    DateTime.new(2012, 10, 21),
				  	DateTime.new(2012, 10, 22),
				    DateTime.new(2012, 11, 11),
				  	DateTime.new(2012, 11, 12),
				  	DateTime.new(2012, 11, 18),
				  	DateTime.new(2012, 11, 19),
				  ].each do |current_date|
				  		Ext::ReminderSchedule.should_receive(:call).never
						Ext::ReminderSchedule.process_reminder(@reminder_weekly, @phone_books, current_date)
					end
				end

				it "should call to all phone books" do 
				  [ DateTime.new(2012, 10, 15),
				  	DateTime.new(2012, 10, 16), 
				  	DateTime.new(2012, 10, 20), 
				  	 
				  	DateTime.new(2012, 11, 5), 
				  	DateTime.new(2012, 11, 6), 
				  	DateTime.new(2012, 11, 10), 

				  	DateTime.new(2012, 11, 25), 
				  	DateTime.new(2012, 11, 26), 
				  	DateTime.new(2012, 11, 27), 
				  	DateTime.new(2012, 12, 1), 

				  	DateTime.new(2012, 12, 16), 
				  	DateTime.new(2012, 12, 17), 
				  	DateTime.new(2012, 12, 18), 
				  	DateTime.new(2012, 12, 22),

				  ].each do |current_date|
				  		Ext::ReminderSchedule.should_receive(:call).with(@reminder_weekly, @phone_books)
						Ext::ReminderSchedule.process_reminder(@reminder_weekly, @phone_books, current_date)
					end
				end
			end
		end
	end
end