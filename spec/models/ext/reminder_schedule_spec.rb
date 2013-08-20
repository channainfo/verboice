require 'spec_helper'

describe Ext::ReminderSchedule  do
	before(:each) do
		@project = Project.make(:time_zone => 'Bangkok')
    @call_flow= CallFlow.make :project_id => @project.id
    @channel = Channels::Custom.make :call_flow => @call_flow
    @reminder_group = Ext::ReminderGroup.make
	end

	it "should project time zone is 'Bangkok' +0070" do
		@project.time_zone.should eq 'Bangkok'
		(ActiveSupport::TimeZone.new(@project.time_zone).utc_offset / (60 * 60)).should eq 7
	end

	describe "Create new reminder schedule" do
	  before(:each) do
	  	@valid = {
	  		:schedule_type => Ext::ReminderSchedule::TYPE_ONE_TIME,
	  		:project_id => @project.id,
	  		:call_flow_id => @call_flow.id,
	  		:reminder_group_id => @reminder_group.id,
	  		:client_start_date => "2012-10-25",
	  		:channel_id => @channel.id,
	  		:schedule => nil,
	  		:time_from => "10:00",
	  		:time_to => "12:00",
	  		:recursion => 1,
	  		:retries => true,
	  		:retries_in_hours => "1,1"
	  	}
	  end	

	  it "should create a reminder schedule with valid attribute" do 
			reminder_schedule = Ext::ReminderSchedule.new @valid
			reminder_schedule.save.should eq true
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

	  it "should ignore save when it is retries and retries_in_hours is invalid" do
	  	invalid = @valid.merge(:retries => true, :retries_in_hours => "aa")
	    reminder_schedule  =  Ext::ReminderSchedule.new invalid
	    reminder_schedule.save().should eq false
	  end

	  it "should invoke create_queues_call after creating new reminder_schedule" do
	  	reminder_schedule = Ext::ReminderSchedule.new @valid
	  	reminder_schedule.should_receive(:create_queues_call)
			reminder_schedule.save
	  end
	end

	describe "#create_queues_call" do
		before(:each) do
			@now = DateTime.new(2012,10,25, 9,0,0, "+7") # use the same timezone as reminder schedule
			DateTime.stub!(:now).and_return(@now)
			@addresses = ["1000", "1001"]
			reminder_group = Ext::ReminderGroup.make addresses: @addresses

			@attr = {
	  		:schedule_type => Ext::ReminderSchedule::TYPE_ONE_TIME,
	  		:reminder_group_id => reminder_group.id,
	  		:project_id => @project.id,
	  		:call_flow_id => @call_flow.id,
	  		:channel_id => @channel.id,
	  		:schedule => nil,
	  		:client_start_date => "2012-10-25", #Thursday
	  		:time_from => "08:00",
	  		:time_to => "17:00"
	  	}
		end

		it "should process to enqueue call all contact in addresses" do
			reminder = Ext::ReminderSchedule.make(@attr.merge(:client_start_date => "2012-10-24", :time_from => "08:00", :time_to => "17:00"))
	  	reminder.should_receive(:process).with(@addresses, @now)
	  	reminder.create_queues_call
		end
	end

	describe "#process" do
		before(:each) do
			@now = DateTime.new(2012,10,25, 9,0,0, "+7") # use the same timezone as reminder schedule

			@attr = {
	  		:schedule_type => Ext::ReminderSchedule::TYPE_ONE_TIME,
	  		:reminder_group_id => @reminder_group.id,
	  		:project_id => @project.id,
	  		:call_flow_id => @call_flow.id,
	  		:channel_id => @channel.id,
	  		:schedule => nil,
	  		:client_start_date => "2012-10-25", #Thursday
	  		:time_from => "08:00",
	  		:time_to => "17:00"
	  	}

	  	@addresses = ["1000", "1001", "1002"]
		end

		it "should current date and time is 2012-10-25 09:00" do
			@now.year.should eq 2012
			@now.month.should eq 10
			@now.day.should eq 25
			@now.hour.should eq 9
			@now.minute.should eq 0
			@now.wday.should eq 4
		end

		describe "start_date in the past" do
			it "should not enqueue call to any contactas" do
				reminder = Ext::ReminderSchedule.make(@attr.merge(:client_start_date => "2012-10-24", :time_from => "08:00", :time_to => "17:00"))
		  	reminder.should_receive(:enqueued_call).never
		  	reminder.process @addresses, @now
			end
		end

		describe "start_date is the same as current date" do
			describe "from time is in the past of now" do
				describe "to time is in the past of now" do
					it "should not enqueue call to any contacts" do
						reminder = Ext::ReminderSchedule.make(@attr.merge(:client_start_date => "2012-10-25", :time_from => "08:00", :time_to => "08:30"))
						reminder.should_receive(:enqueued_call).with(@addresses, @now).never
						reminder.process @addresses, @now
					end

					it "should enqueue call to any contacts when it's scheduling" do
						reminder = Ext::ReminderSchedule.make(@attr.merge(:client_start_date => "2012-10-25", :time_from => "08:00", :time_to => "08:30"))
						reminder.should_receive(:callers_matches_conditions).with(@addresses).and_return(["1000", "1001", "1002"])
						reminder.should_receive(:enqueued_call).with(["1000", "1001", "1002"], @now)
						reminder.process @addresses, @now, true
					end
				end

				describe "to time is in the future of now" do
					it "should enqueue call to any contacts" do
						reminder = Ext::ReminderSchedule.make(@attr.merge(:client_start_date => "2012-10-25", :time_from => "08:00", :time_to => "17:00"))
						reminder.should_receive(:callers_matches_conditions).with(@addresses).and_return(["1000", "1001", "1002"])
						reminder.should_receive(:enqueued_call).with(["1000", "1001", "1002"], @now)
						reminder.process @addresses, @now
					end
				end
			end

			describe "from time is in the future of now" do
				describe "without repeat" do
					describe "without conditions" do
						it "should enqueue call to any contacts" do
							reminder = Ext::ReminderSchedule.make(@attr.merge(:client_start_date => "2012-10-25", :time_from => "10:00", :time_to => "17:00"))
							reminder.should_receive(:callers_matches_conditions).with(@addresses).and_return(["1000", "1001", "1002"])

							reminder.should_receive(:enqueued_call).with(["1000", "1001", "1002"], @now)
							reminder.process @addresses, @now
						end
					end

					describe "with conditions" do
						it "should not enqueue call to any contacts when there're no any contacts are matches all" do
							conditions = [Ext::Condition.new("var1", "=", "5", "number")]

							reminder = Ext::ReminderSchedule.make(@attr.merge(:client_start_date => "2012-10-25", :time_from => "10:00", :time_to => "17:00", :conditions => conditions))
							reminder.should_receive(:callers_matches_conditions).with(@addresses).and_return([])
							reminder.should_receive(:enqueued_call).with(@addresses, @now).never
							reminder.process @addresses, @now
						end

						it "should enqueue call to only contacts that matches all" do
							conditions = [Ext::Condition.new("var1", "=", "10", "number"), Ext::Condition.new("var2", "=", "20", "number")]

							reminder = Ext::ReminderSchedule.make(@attr.merge(:client_start_date => "2012-10-25", :time_from => "10:00", :time_to => "17:00", :conditions => conditions))
							reminder.should_receive(:callers_matches_conditions).with(@addresses).and_return(["1000"])
							reminder.should_receive(:enqueued_call).with(["1000"], @now)
							reminder.process @addresses, @now
						end
					end
				end

				describe "with repeat" do
					describe "without conditions" do
						it "should enqueue call to any contacts when wday of start_date and now are the same" do
							reminder = Ext::ReminderSchedule.make(@attr.merge(:schedule_type => Ext::ReminderSchedule::TYPE_DAILY, :days => "4", :recursion => 1, :client_start_date => "2012-10-25", :time_from => "10:00", :time_to => "17:00"))
							reminder.should_receive(:callers_matches_conditions).with(@addresses).and_return(["1000", "1001", "1002"])
					  	reminder.should_receive(:enqueued_call).with(["1000", "1001", "1002"], @now)
					  	reminder.process @addresses, @now
						end

						it "should not enqueue call to any contacts when wday of start_date and now are different" do
							@now = DateTime.new(2012,10,26, 9,0,0, "+7") # is Friday
							reminder = Ext::ReminderSchedule.make(@attr.merge(:schedule_type => Ext::ReminderSchedule::TYPE_DAILY, :days => "0,1,2,3,4,6", :recursion => 1, :client_start_date => "2012-10-25", :time_from => "10:00", :time_to => "17:00"))
							reminder.should_receive(:callers_matches_conditions).with(@addresses).never
					  	reminder.should_receive(:enqueued_call).with(["1000", "1001", "1002"], @now).never
					  	reminder.process @addresses, @now
						end
					end

					describe "with conditions" do
						it "should not enqueue call any contacts when it's not match at least one but start_date and now are the same" do
							conditions = [Ext::Condition.new("var1", "=", "5", "number"), Ext::Condition.new("var2", "=", "10", "number")]

							reminder = Ext::ReminderSchedule.make(@attr.merge(:schedule_type => Ext::ReminderSchedule::TYPE_DAILY, :days => "4", :recursion => 1, :client_start_date => "2012-10-25", :time_from => "10:00", :time_to => "17:00", :conditions => conditions))
							reminder.should_receive(:callers_matches_conditions).with(@addresses).and_return([])
					  	reminder.should_receive(:enqueued_call).with([], @now).never
					  	reminder.process @addresses, @now
						end

						it "should not enqueue call any contacts when it's match all but wday of start_date and now are different" do
							@now = DateTime.new(2012,10,26, 9,0,0, "+7") # is Friday
							conditions = [Ext::Condition.new("var1", "=", "5", "number"), Ext::Condition.new("var2", "=", "10", "number")]

							reminder = Ext::ReminderSchedule.make(@attr.merge(:schedule_type => Ext::ReminderSchedule::TYPE_DAILY, :days => "0,1,2,3,4,6", :recursion => 1, :client_start_date => "2012-10-25", :time_from => "10:00", :time_to => "17:00", :conditions => conditions))
							reminder.should_receive(:callers_matches_conditions).with(@addresses).never
					  	reminder.should_receive(:enqueued_call).with(["1000", "1001", "1002"], @now).never
					  	reminder.process @addresses, @now
						end

						it "should enqueue call any contacts when it's match all and wday of start_date and now are the same" do
							conditions = [Ext::Condition.new("var1", "=", "5", "number"), Ext::Condition.new("var2", "=", "10", "number")]

							reminder = Ext::ReminderSchedule.make(@attr.merge(:schedule_type => Ext::ReminderSchedule::TYPE_DAILY, :days => "0,1,2,3,4,6", :recursion => 1, :client_start_date => "2012-10-25", :time_from => "10:00", :time_to => "17:00", :conditions => conditions))
							reminder.should_receive(:callers_matches_conditions).with(@addresses).and_return(["1000"])
					  	reminder.should_receive(:enqueued_call).with(["1000"], @now)
					  	reminder.process @addresses, @now
						end
					end
				end
			end
		end

		describe "start_date in the future" do
			describe "is no repeat" do
				it "should not enqueued call to any contacts" do
					reminder = Ext::ReminderSchedule.make @attr.merge(:client_start_date => "2012-10-26", :time_from => "08:00", :time_to => "17:00")
					reminder.should_receive(:callers_matches_conditions).with(@addresses).never
			  	reminder.should_receive(:enqueued_call).with([], @now).never
			  	reminder.process @addresses, @now
				end
			end

			describe "is repeat" do
				it "should not enqueued call to any contacts when wday in schedule days has include wday of today" do
					reminder = Ext::ReminderSchedule.make @attr.merge(:client_start_date => "2012-10-26", :time_from => "08:00", :time_to => "17:00", :schedule_type => Ext::ReminderSchedule::TYPE_DAILY, :days => "4,5", :recursion => 1)
					reminder.should_receive(:callers_matches_conditions).with(@addresses).never
			  	reminder.should_receive(:enqueued_call).with([], @now).never
			  	reminder.process @addresses, @now
				end
			end
		end
	end

	describe ".schedule" do
		before(:each) do
			@attr = {
	  		:schedule_type => Ext::ReminderSchedule::TYPE_ONE_TIME,
	  		:project_id => @project.id,
	  		:call_flow_id => @call_flow.id,
	  		:channel_id => @channel.id,
	  		:reminder_group_id => @reminder_group.id,
	  		:schedule => nil,
	  		:client_start_date => "2012-10-25",
	  		:time_from => "08:00",
	  		:time_to => "17:00"
	  	}

	  	@reminder_schedules = []
	  	@addresses = []
	  	3.times.each do |i|
        @reminder_schedules << Ext::ReminderSchedule.make(@attr.merge(:name => "#{@attr[:name]}-#{i}"))
        @addresses << (1000 + i).to_s
      end

      @at_time = DateTime.new(2012,10,25, 8,1)
		end

		it "should process all reminders with addresses" do
			# @reminder_schedules.should_receive(:process)
      Ext::ReminderSchedule.schedule(@project.id, @at_time)
    end
	end

	describe "#enqueued_call" do
		before(:each) do
			@reminder = Ext::ReminderSchedule.make(
	  		:schedule_type => Ext::ReminderSchedule::TYPE_ONE_TIME,
	  		:project_id => @project.id,
	  		:call_flow_id => @call_flow.id,
	  		:channel_id => @channel.id,
	  		:reminder_group_id => @reminder_group.id,
	  		:schedule => nil,
	  		:client_start_date => "2012-10-25",
	  		:time_from => "08:00",
	  		:time_to => "17:00",
  		)

			@addresses = ["1001", "1002", "1003", "1004", "1005", "1006"]
		end

		it "should enqueued call to verboice queued call " do
	   	queues = @reminder.enqueued_call @addresses, DateTime.new(2012,11,26)
	   	queues.size.should eq 6
		end
	end

	describe "#call_options" do
		before(:each) do
			@reminder = Ext::ReminderSchedule.make(
	  		:schedule_type => Ext::ReminderSchedule::TYPE_ONE_TIME,
	  		:project_id => @project.id,
	  		:call_flow_id => @call_flow.id,
	  		:channel_id => @channel.id,
	  		:reminder_group_id => @reminder_group.id,
	  		:schedule => nil,
	  		:client_start_date => "2012-10-25",
	  		:time_from => "08:00",
	  		:time_to => "17:00",
	  	)
		end

		it "should create options for schedule enqueued call " do
			options = @reminder.call_options DateTime.new(2012,10,22)
			options[:call_flow_id].should eq @reminder.call_flow_id
			options[:project_id].should eq @reminder.project_id
			options[:time_zone].should  eq @reminder.project.time_zone
			options[:not_before].should eq DateTime.new(2012, 10, 22, 1, 0)
		end
	end

	describe "#has_conditions?" do
		before(:each) do
			@attr = {
	  		:schedule_type => Ext::ReminderSchedule::TYPE_ONE_TIME,
	  		:project_id => @project.id,
	  		:call_flow_id => @call_flow.id,
	  		:channel_id => @channel.id,
	  		:reminder_group_id => @reminder_group.id,
	  		:schedule => nil,
	  		:client_start_date => "2012-10-25",
	  		:time_from => "08:00",
	  		:time_to => "17:00"
	  	}
	  end

		it "should return false when reminder schedule's conditions are not setting up" do
			reminder_schedule = Ext::ReminderSchedule.make @attr

			reminder_schedule.has_conditions?.should be false
		end

		it "should return true when reminder schedule's conditions are setting up" do
			conditions = [Ext::Condition.new("var1", "=", "5", "number")]
			reminder_schedule = Ext::ReminderSchedule.make @attr.merge(:conditions => conditions)

			reminder_schedule.has_conditions?.should be true
		end
	end

	describe "#callers_matches_conditions" do
		before(:each) do
			@attr = {
	  		:schedule_type => Ext::ReminderSchedule::TYPE_ONE_TIME,
	  		:project_id => @project.id,
	  		:call_flow_id => @call_flow.id,
	  		:channel_id => @channel.id,
	  		:reminder_group_id => @reminder_group.id,
	  		:schedule => nil,
	  		:client_start_date => "2012-10-25",
	  		:time_from => "08:00",
	  		:time_to => "17:00"
	  	}

			@contact_one = @project.contacts.build
			@contact_one.addresses.build(address: "1000")
			@contact_one.save

			@contact_two = @project.contacts.build
			@contact_two.addresses.build(address: "1001")
			@contact_two.save

			@project_var1 = ProjectVariable.make :name => "var1"
			@project_var2 = ProjectVariable.make :name => "var2"
			PersistedVariable.make contact_id: @contact_one.id, project_variable_id: @project_var1.id, value: "5"
			PersistedVariable.make contact_id: @contact_one.id, project_variable_id: @project_var2.id, value: "10"
			PersistedVariable.make contact_id: @contact_two.id, project_variable_id: @project_var1.id, value: "15"
			PersistedVariable.make contact_id: @contact_two.id, project_variable_id: @project_var2.id, value: "20"

			@addresses = ["1000", "1001"]
		end

		it "should return an empty array when any contacts are not matches conditions" do
			conditions = [Ext::Condition.new("var1", "=", "99", "number")]
			reminder_schedule = Ext::ReminderSchedule.make @attr.merge(:conditions => conditions)

			phone_numbers = reminder_schedule.callers_matches_conditions @addresses
			phone_numbers.empty?.should be true
		end

		it "should return array of first contact phone number when only the first contact is matches conditions" do
			conditions = [Ext::Condition.new("var1", "=", "5", "number")]
			reminder_schedule = Ext::ReminderSchedule.make @attr.merge(:conditions => conditions)

			phone_numbers = reminder_schedule.callers_matches_conditions @addresses
			phone_numbers.size.should be 1
		end

		it "should return array of all contacts phone number when conditions are not setting up" do
			reminder_schedule = Ext::ReminderSchedule.make @attr

			phone_numbers = reminder_schedule.callers_matches_conditions @addresses
			phone_numbers.size.should be @addresses.size
		end
	end

end