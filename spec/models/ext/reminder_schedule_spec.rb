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
	  		:schedule_type => Ext::ReminderSchedule::TYPE_ONE_TIME,
	  		:project_id => @project.id,
	  		:call_flow_id => @call_flow.id,
	  		:reminder_phone_book_type_id => @reminder_phone_book_type.id,
	  		:client_start_date => "2012-10-25",
	  		:channel_id => @channel.id,
	  		:schedule => nil,
	  		:time_from => "10:00",
	  		:time_to => "12:00",
	  		:recursion => 1
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

	  it "should invoke create_queues_call after creating new reminder_schedule" do
	  	reminder_schedule = Ext::ReminderSchedule.new @valid
	  	reminder_schedule.should_receive(:create_queues_call)
			reminder_schedule.save
	  end
	end

	describe "Reminder#create_queues_call" do
		before(:each) do
			@now = DateTime.new(2012,10,25, 9,0,0, "+7") # use the same timezone as reminder schedule
			DateTime.stub!(:now).and_return(@now)

			@attr = {
	  		:schedule_type => Ext::ReminderSchedule::TYPE_ONE_TIME,
	  		:reminder_phone_book_type_id => @reminder_phone_book_type.id,
	  		:project_id => @project.id,
	  		:call_flow_id => @call_flow.id,
	  		:channel_id => @channel.id,
	  		:schedule => nil,
	  		:timezone => "Bangkok", # use the same timezone as DateTime object
	  		# :client_start_date => "10/25/2012 09:20", # thurday
	  		:client_start_date => "2012-10-25", #Thursday
	  		:time_from => "08:00",
	  		:time_to => "17:00"
	  	}
		  	
	  	@phone_books = []
	  	3.times.each do |i|
        @phone_books << Ext::ReminderPhoneBook.make(:project_id => @project.id, :type_id => @reminder_phone_book_type.id)
      end
		end

		it "should process to enqueue call all contact in phone books" do
			reminder = Ext::ReminderSchedule.make(@attr.merge(:client_start_date => "2012-10-24", :time_from => "08:00", :time_to => "17:00"))
	  	reminder.should_receive(:process).with(@phone_books, @now)
	  	reminder.create_queues_call
		end
	end

	describe "Reminder#process" do
		before(:each) do
			@now = DateTime.new(2012,10,25, 9,0,0, "+7") # use the same timezone as reminder schedule

			@attr = {
	  		:schedule_type => Ext::ReminderSchedule::TYPE_ONE_TIME,
	  		:reminder_phone_book_type_id => @reminder_phone_book_type.id,
	  		:project_id => @project.id,
	  		:call_flow_id => @call_flow.id,
	  		:channel_id => @channel.id,
	  		:schedule => nil,
	  		:timezone => "Bangkok", # use the same timezone as DateTime object
	  		# :client_start_date => "10/25/2012 09:20", # thurday
	  		:client_start_date => "2012-10-25", #Thursday
	  		:time_from => "08:00",
	  		:time_to => "17:00"
	  	}
		  	
	  	@phone_books = []
	  	3.times.each do |i|
        @phone_books << Ext::ReminderPhoneBook.make(:project_id => @project.id, :type_id => @reminder_phone_book_type.id)
      end
		end

		it "should current date and time is 2012-10-25 09:00" do
			@now.year.should eq 2012
			@now.month.should eq 10
			@now.day.should eq 25
			@now.hour.should eq 9
			@now.minute.should eq 0
		end

		describe "start_date in the past" do
			it "should not enqueue call" do
				reminder = Ext::ReminderSchedule.make(@attr.merge(:client_start_date => "2012-10-24", :time_from => "08:00", :time_to => "17:00"))
		  	reminder.should_receive(:enqueued_call).never
		  	reminder.process @phone_books, @now
			end
		end

		describe "start_date is the same as current date" do
			it "should not enqueue call when time from is in the past of now" do
				reminder = Ext::ReminderSchedule.make(@attr.merge(:client_start_date => "2012-10-25", :time_from => "08:00", :time_to => "17:00"))
				reminder.should_receive(:enqueued_call).with(@phone_books, @now).never
				reminder.process @phone_books, @now
			end

			describe "from time is in the future of now" do
				describe "without repeat" do
					describe "without conditions" do
						it "should enqueue call" do
							reminder = Ext::ReminderSchedule.make(@attr.merge(:client_start_date => "2012-10-25", :time_from => "10:00", :time_to => "17:00"))
							reminder.should_receive(:enqueued_call).with(@phone_books, @now)
							reminder.process @phone_books, @now
						end
					end

					describe "with conditions" do
						before(:each) do
							@call_log = CallLog.make
							@project_var_1 = ProjectVariable.make :name => "var1"
							@project_var_2 = ProjectVariable.make :name => "var2"
						end

						it "should not enqueue call when it's not match" do
							CallLogAnswer.make :project_variable_id => @project_var_1.id, :value => "10", :call_log_id => @call_log.id
							conditions = [Ext::Condition.new("var1", "=", "5")]

							reminder = Ext::ReminderSchedule.make(@attr.merge(:client_start_date => "2012-10-25", :time_from => "10:00", :time_to => "17:00", :conditions => conditions))
							reminder.should_receive(:enqueued_call).with(@phone_books, @now).never
							reminder.process @phone_books, @now
						end

						it "should not enqueue call when it's not match at least one" do
							CallLogAnswer.make :project_variable_id => @project_var_1.id, :value => "10", :call_log_id => @call_log.id
							conditions = [Ext::Condition.new("var1", "=", "5"), Ext::Condition.new("var2", "=", "5")]

							reminder = Ext::ReminderSchedule.make(@attr.merge(:client_start_date => "2012-10-25", :time_from => "10:00", :time_to => "17:00", :conditions => conditions))
							reminder.should_receive(:enqueued_call).with(@phone_books, @now).never
							reminder.process @phone_books, @now
						end

						it "should enqueue call when it's match all" do
							CallLogAnswer.make :project_variable_id => @project_var_1.id, :value => "10", :call_log_id => @call_log.id
							CallLogAnswer.make :project_variable_id => @project_var_2.id, :value => "20", :call_log_id => @call_log.id
							conditions = [Ext::Condition.new("var1", "=", "10"), Ext::Condition.new("var2", "=", "20")]

							reminder = Ext::ReminderSchedule.make(@attr.merge(:client_start_date => "2012-10-25", :time_from => "10:00", :time_to => "17:00", :conditions => conditions))
							reminder.should_receive(:enqueued_call).with(@phone_books, @now)
							reminder.process @phone_books, @now
						end
					end
				end

				describe "with repeat" do
					describe "without conditions" do
						it "should enqueue call when wday of start_date and now are the same" do
							reminder = Ext::ReminderSchedule.make(@attr.merge(:schedule_type => Ext::ReminderSchedule::TYPE_DAILY, :days => "4", :recursion => 1, :client_start_date => "2012-10-25", :time_from => "10:00", :time_to => "17:00"))
					  	reminder.should_receive(:enqueued_call).with(@phone_books, @now)
					  	reminder.process @phone_books, @now
						end

						it "should not enqueue call when wday of start_date and now are different" do
							@now = DateTime.new(2012,10,26, 9,0,0, "+7") # is Friday
							reminder = Ext::ReminderSchedule.make(@attr.merge(:schedule_type => Ext::ReminderSchedule::TYPE_DAILY, :days => "0,1,2,3,4,6", :recursion => 1, :client_start_date => "2012-10-25", :time_from => "10:00", :time_to => "17:00"))
					  	reminder.should_receive(:enqueued_call).never
					  	reminder.process @phone_books, @now
						end
					end

					describe "with conditions" do
						before(:each) do
							@call_log = CallLog.make
							@project_var_1 = ProjectVariable.make :name => "var1"
							@project_var_2 = ProjectVariable.make :name => "var2"
						end

						it "should not enqueue call when it's not match at least one but start_date and now are the same" do
							CallLogAnswer.make call_log_id: @call_log.id, project_variable_id: @project_var_1.id, value: "5"
							conditions = [Ext::Condition.new("var1", "=", "5"), Ext::Condition.new("var2", "=", "10")]

							reminder = Ext::ReminderSchedule.make(@attr.merge(:schedule_type => Ext::ReminderSchedule::TYPE_DAILY, :days => "4", :recursion => 1, :client_start_date => "2012-10-25", :time_from => "10:00", :time_to => "17:00", :conditions => conditions))
					  	reminder.should_receive(:enqueued_call).with(@phone_books, @now).never
					  	reminder.process @phone_books, @now
						end

						it "should not enqueue call when it's match all but wday of start_date and now are different" do
							@now = DateTime.new(2012,10,26, 9,0,0, "+7") # is Friday
							CallLogAnswer.make call_log_id: @call_log.id, project_variable_id: @project_var_1.id, value: "5"
							CallLogAnswer.make call_log_id: @call_log.id, project_variable_id: @project_var_2.id, value: "10"
							conditions = [Ext::Condition.new("var1", "=", "5"), Ext::Condition.new("var2", "=", "10")]

							reminder = Ext::ReminderSchedule.make(@attr.merge(:schedule_type => Ext::ReminderSchedule::TYPE_DAILY, :days => "0,1,2,3,4,6", :recursion => 1, :client_start_date => "2012-10-25", :time_from => "10:00", :time_to => "17:00", :conditions => conditions))
					  	reminder.should_receive(:enqueued_call).with(@phone_books, @now).never
					  	reminder.process @phone_books, @now
						end

						it "should enqueue call when it's match all and wday of start_date and now are the same" do
							CallLogAnswer.make call_log_id: @call_log.id, project_variable_id: @project_var_1.id, value: "5"
							CallLogAnswer.make call_log_id: @call_log.id, project_variable_id: @project_var_2.id, value: "10"
							conditions = [Ext::Condition.new("var1", "=", "5"), Ext::Condition.new("var2", "=", "10")]

							reminder = Ext::ReminderSchedule.make(@attr.merge(:schedule_type => Ext::ReminderSchedule::TYPE_DAILY, :days => "0,1,2,3,4,6", :recursion => 1, :client_start_date => "2012-10-25", :time_from => "10:00", :time_to => "17:00", :conditions => conditions))
					  	reminder.should_receive(:enqueued_call).with(@phone_books, @now)
					  	reminder.process @phone_books, @now
						end
					end
				end
			end
		end

		describe "start_date in the future" do
			describe "is no repeat" do
				it "should not enqueued call" do
					reminder = Ext::ReminderSchedule.make @attr.merge(:client_start_date => "2012-10-26", :time_from => "08:00", :time_to => "17:00")
			  	reminder.should_receive(:enqueued_call).with(@phone_books, @now).never
			  	reminder.process @phone_books, @now
				end
			end

			describe "is repeat" do
				it "should not enqueued call when wday in schedule days has include wday of today" do
					reminder = Ext::ReminderSchedule.make @attr.merge(:client_start_date => "2012-10-26", :time_from => "08:00", :time_to => "17:00", :schedule_type => Ext::ReminderSchedule::TYPE_DAILY, :days => "4,5", :recursion => 1)
			  	reminder.should_receive(:enqueued_call).with(@phone_books, @now).never
			  	reminder.process @phone_books, @now
				end
			end
		end
	end

	describe "Reminder.schedule" do
		before(:each) do
			@attr = {
	  		:schedule_type => Ext::ReminderSchedule::TYPE_ONE_TIME,
	  		:project_id => @project.id,
	  		:call_flow_id => @call_flow.id,
	  		:channel_id => @channel.id,
	  		:reminder_phone_book_type_id => @reminder_phone_book_type.id,
	  		:schedule => nil,
	  		:client_start_date => "2012-10-25",
	  		:time_from => "08:00",
	  		:time_to => "17:00",
	  		:timezone => "Bangkok"
	  	}

	  	@reminder_schedules = []
	  	@phone_books = []
	  	3.times.each do |i|
        @reminder_schedules << Ext::ReminderSchedule.make(@attr.merge(:name => "#{@attr[:name]}-#{i}"))
        @phone_books << Ext::ReminderPhoneBook.make(:project_id => @project.id, :type_id => @reminder_phone_book_type.id)
      end

      @at_time = DateTime.new(2012,10,25, 8,1)
		end

		it "should process all reminders with phone books" do
			# @reminder_schedules.should_receive(:process)
      Ext::ReminderSchedule.schedule(@project.id, @at_time)
    end
	end

	describe "ReminderSchedule#enqueued_call" do
		before(:each) do
			@reminder = Ext::ReminderSchedule.make(
	  		:schedule_type => Ext::ReminderSchedule::TYPE_ONE_TIME,
	  		:project_id => @project.id,
	  		:call_flow_id => @call_flow.id,
	  		:channel_id => @channel.id,
	  		:reminder_phone_book_type_id => @reminder_phone_book_type.id,
	  		:schedule => nil,
	  		:client_start_date => "2012-10-25",
	  		:time_from => "08:00",
	  		:time_to => "17:00",
	  		:timezone => "Bangkok"
  		)

  		@phone_books = []
	  	6.times.each do |i|
        @phone_books << Ext::ReminderPhoneBook.make(:project_id => @project.id, :type_id => @reminder_phone_book_type.id)
      end
		end

		it "should enqueued call to verboice queued call " do
	   	queues = @reminder.enqueued_call @phone_books, DateTime.new(2012,11,26)
	   	queues.size.should eq 6
		end
	end

	describe "ReminderSchedule#call_options" do
		before(:each) do
			@reminder = Ext::ReminderSchedule.make(
	  		:schedule_type => Ext::ReminderSchedule::TYPE_ONE_TIME,
	  		:project_id => @project.id,
	  		:call_flow_id => @call_flow.id,
	  		:channel_id => @channel.id,
	  		:reminder_phone_book_type_id => @reminder_phone_book_type.id,
	  		:schedule => nil,
	  		:client_start_date => "2012-10-25",
	  		:time_from => "08:00",
	  		:time_to => "17:00",
	  		:timezone => "Bangkok"
	  	)
		end

		it "should create options for schedule enqueued call " do
			options = @reminder.call_options DateTime.new(2012,10,22)
			options[:call_flow_id].should eq @reminder.call_flow_id
			options[:project_id].should eq @reminder.project_id
			options[:time_zone].should  eq @reminder.timezone
			options[:not_before].should eq DateTime.new(2012, 10, 22, 1, 0)
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

	describe "ReminderSchedule#in_schedule_day? " do
		it "should tell if a datetime in days of reminder schedule days" do
			reminder = Ext::ReminderSchedule.make(
				:schedule_type => Ext::ReminderSchedule::TYPE_ONE_TIME,
	  		:project_id => @project.id,
	  		:call_flow_id => @call_flow.id,
	  		:channel_id => @channel.id,
	  		:reminder_phone_book_type_id => @reminder_phone_book_type.id,
	  		:schedule => nil,
	  		:client_start_date => "2012-10-25",
	  		:time_from => "08:00",
	  		:time_to => "17:00",
	  		:timezone => "Bangkok",
	  		:days => "0,2,3"
  		)
			reminder.in_schedule_day?(DateTime.new(2012,10,7).wday).should eq true
			reminder.in_schedule_day?(DateTime.new(2012,10,8).wday).should eq false
			reminder.in_schedule_day?(DateTime.new(2012,10,9).wday).should eq true
			reminder.in_schedule_day?(DateTime.new(2012,10,10).wday).should eq true
			reminder.in_schedule_day?(DateTime.new(2012,10,11).wday).should eq false
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

end