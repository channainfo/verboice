require 'spec_helper'

describe Ext::ReminderGroup do
  before(:each) do
    @project = Project.make
    @valid = {
      :name => "Reminder Group",
      :project_id => @project.id
    }
  end

  it "should create a new reminder phone book with valid attributes" do
    type = Ext::ReminderGroup.new @valid
    type.save.should eq true
  end

  it "should require name" do
    attrs = @valid.merge(:name => "")
    type = Ext::ReminderGroup.new attrs
    type.save.should eq false
  end

  it "should require name to be unique in scope of project" do
    type = Ext::ReminderGroup.new @valid
    type.save.should eq true
    type = Ext::ReminderGroup.new @valid
    type.save.should eq false
  end

  describe "#register_address" do
    it "should register address when it doesn't exists" do
      reminder_group = Ext::ReminderGroup.create @valid
      reminder_group.register_address "1000"

      @project.contacts.size.should == 1
      @project.contacts.first.addresses.first.address.should == "1000"
      reminder_group.addresses.size.should == 1
      reminder_group.addresses.first.should == "1000"
    end

    it "should ignore when it's already exists" do
      reminder_group = Ext::ReminderGroup.create @valid.merge(addresses: ["1000"])
      reminder_group.register_address "1000"

      reminder_group.addresses.size.should == 1
      reminder_group.addresses.last.should == "1000"
    end
  end

  describe "#deregister_address" do
    it "should deregister address when it's exists" do
      reminder_group = Ext::ReminderGroup.create @valid.merge(addresses: ["1000"])

      # assert before process
      reminder_group.addresses.size.should == 1

      reminder_group.deregister_address "1000"

      # assert after process
      reminder_group.addresses.size.should == 0
    end

    it "should ignore when it doesn't exists" do
      reminder_group = Ext::ReminderGroup.create @valid.merge(addresses: ["1000"])

      # assert before process
      reminder_group.addresses.size.should == 1
      reminder_group.deregister_address "1001"

      # assert after process
      reminder_group.addresses.size.should == 1
      reminder_group.addresses.first.should == "1000"
    end
  end

end
