require 'spec_helper'

describe Ext::PregnancyReminder do
  before(:each) do
    @project = Project.make
    @call_flow= CallFlow.make :project_id => @project.id
    @channel = Channels::Custom.make :call_flow => @call_flow
  end

  describe "Create new reminder schedule" do
    before(:each) do
      @valid = {
        :name => "reminder 1",
        :project_id => @project.id,
        :call_flow_id => @call_flow.id,
        :channel_id => @channel.id,
        :schedule => nil,
        :week => 3, 
        :timezone => "Bangkok"
      }
    end 

    it "should create a reminder schedule with valid attribute" do 
      pregnancy_reminder = Ext::PregnancyReminder.new @valid
      result = pregnancy_reminder.save
      result.should eq true
    end

    it "should require name" do
      invalid = @valid.merge(:name => "")
      pregnancy_reminder  =  Ext::PregnancyReminder.new invalid
      pregnancy_reminder.save().should eq false
    end

    it "should require week" do
      invalid = @valid.merge(:week => nil)
      pregnancy_reminder  =  Ext::PregnancyReminder.new invalid
      pregnancy_reminder.save().should eq false
    end

    it "should require channel" do
      invalid = @valid.merge(:channel_id => nil)
      pregnancy_reminder  =  Ext::PregnancyReminder.new invalid
      pregnancy_reminder.save().should eq false
    end

    it "should require project" do
      invalid = @valid.merge(:project_id => nil)
      pregnancy_reminder  =  Ext::PregnancyReminder.new invalid
      pregnancy_reminder.save().should eq false
    end

    it "should require call flow" do
      invalid = @valid.merge(:call_flow_id => nil)
      pregnancy_reminder  =  Ext::PregnancyReminder.new invalid
      pregnancy_reminder.save().should eq false
    end

    it "should invoke create_queue_call after creating new pregnancy reminder" do
      pregnancy_reminder = Ext::PregnancyReminder.new @valid
      pregnancy_reminder.should_receive(:create_queues_call)
    pregnancy_reminder.save
    end
  end

  describe "Reminder#create_queues_call" do
    before(:each) do
      @now = DateTime.new(2012,10,25, 9,0,0, "+7")
      DateTime.stub!(:now).and_return(@now)

      @valid = {
        :name => "reminder 1",
        :project_id => @project.id,
        :call_flow_id => @call_flow.id,
        :channel_id => @channel.id,
        :schedule => nil,
        :week => 3,
        :timezone => "Bangkok"
      }

      @phone_books = []
      3.times.each do |i|
        phone_book = Ext::ReminderPhoneBook.make(:project_id => @project.id)
        Ext::Patient.make :pregnancy_date => Date.new(2012, 11, 1), :reminder_phone_book_id => phone_book.id
        @phone_books << phone_book
      end
    end

    it "should run process to remind patients that have pregnant reach the setting" do
      reminder = Ext::PregnancyReminder.make(@valid)
      reminder.should_receive(:process).with(@phone_books, @now)
      reminder.create_queues_call
    end
  end

  describe "Reminder#destroy" do
    before(:each) do
      @valid = {
        :name => "reminder 1",
        :project_id => @project.id,
        :call_flow_id => @call_flow.id,
        :channel_id => @channel.id,
        :schedule => nil,
        :week => 3,
        :timezone => "Bangkok"
      }
    end

    it "should call remove_queues_call" do
      reminder = Ext::PregnancyReminder.make @valid
      reminder.should_receive(:remove_queues_call)
      reminder.destroy
    end
  end

  describe "Reminder#remove_queue_call" do
    before(:each) do
      @valid = {
        :name => "reminder 1",
        :project_id => @project.id,
        :call_flow_id => @call_flow.id,
        :channel_id => @channel.id,
        :schedule => nil,
        :week => 3,
        :timezone => "Bangkok",
        :queued_call_ids => [3,10,2,12]
      }
    end

    it "should remove all queue and reset queued_call_ids" do
      reminder = Ext::PregnancyReminder.make @valid
      reminder.should_receive(:remove_queued_call)
      reminder.remove_queues_call
      reminder.queued_call_ids.should eq []
    end
  end

  describe "Reminder.schedule" do
    before(:each) do
      @valid = {
        :name => "remind patients that have pregnant 3 weeks",
        :project_id => @project.id,
        :call_flow_id => @call_flow.id,
        :channel_id => @channel.id,
        :schedule => nil,
        :week => 3,
        :timezone => "Bangkok",
        :queued_call_ids => [3,10,2,12]
      }

      @phone_books = []
      @reminder_schedule = Ext::PregnancyReminder.make @valid
      3.times.each do |i|
        phone_book = Ext::ReminderPhoneBook.make(:project_id => @project.id)
        Ext::Patient.make :pregnancy_date => Date.new(2012, 11, 1), :reminder_phone_book_id => phone_book.id
        @phone_books << phone_book
      end

      @at_time = DateTime.new(2012,10,25, 9,21)
    end

    it "should process all reminder with phone books" do
        # @reminder_schedule.should_receive(:process).with(@phone_books, @at_time).exactly(1).times
        Ext::PregnancyReminder.schedule(@project.id, @at_time)    
      end 
  end

  describe "PregnancyReminder#process" do
    before(:each) do
      @valid = {
        :name => "remind patients that have pregnant 3 weeks",
        :project_id => @project.id,
        :call_flow_id => @call_flow.id,
        :channel_id => @channel.id,
        :schedule => nil,
        :week => 3,
        :timezone => "Bangkok",
      }

      @reminder = Ext::PregnancyReminder.make @valid
      @queued_call = QueuedCall.make
    end

    it "should enqueue call all patients that have pregnant 3 week until now to verboice queued call " do
      phone_books = []
      phone_book_one = Ext::ReminderPhoneBook.make(:project_id => @project.id, :phone_number => "85577710900")
      phone_book_two = Ext::ReminderPhoneBook.make(:project_id => @project.id, :phone_number => "85569860012")
      Ext::Patient.make :pregnancy_date => 21.days.ago.to_date, :reminder_phone_book_id => phone_book_one.id
      Ext::Patient.make :pregnancy_date => 20.days.ago.to_date, :reminder_phone_book_id => phone_book_two.id
      phone_books << phone_book_one

      @reminder.should_receive(:call_options).with(DateTime.new(2012, 11, 13)).and_return("options")
      @reminder.should_receive(:call).with("85577710900", "options").once.and_return(@queued_call)
      queued_calls = @reminder.process phone_books, DateTime.new(2012,11,13)
      queued_calls.size.should eq 1
    end
  end

  describe "PregnancyReminder#call_options" do
    before(:each) do
      @valid = {
        :name => "remind patients that have pregnant 3 weeks",
        :project_id => @project.id,
        :call_flow_id => @call_flow.id,
        :channel_id => @channel.id,
        :schedule => nil,
        :week => 3,
        :timezone => "Bangkok",
      }
      @reminder = Ext::PregnancyReminder.make @valid
    end

    it "should return hash of call options" do
      options = @reminder.call_options DateTime.new(2012, 11, 13)

      options[:call_flow_id].should eq(@call_flow.id)
      options[:project_id].should eq(@project.id)
      options[:time_zone].should eq("Bangkok")
      options[:not_before].should eq(DateTime.new(2012, 11, 13))
    end
  end

end
