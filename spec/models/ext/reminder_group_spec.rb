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
    reminder_group = Ext::ReminderGroup.new @valid
    reminder_group.save.should eq true
  end

  it "should require name" do
    attrs = @valid.merge(:name => "")
    reminder_group = Ext::ReminderGroup.new attrs
    reminder_group.save.should eq false
  end

  it "should require name to be unique in scope of project" do
    reminder_group = Ext::ReminderGroup.new @valid
    reminder_group.save.should eq true
    reminder_group = Ext::ReminderGroup.new @valid
    reminder_group.save.should eq false
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

  describe "#deserialized_to_array" do
    it "should return an empty array when string is null" do
      string = nil
      array = Ext::ReminderGroup.deserialized_to_array string
      array.should be_empty
    end

    it "should return an empty array when string is empty" do
      string = ""
      array = Ext::ReminderGroup.deserialized_to_array string
      array.should be_empty
    end

    it "should return an empty array when string is '---'" do
      string = "---"
      array = Ext::ReminderGroup.deserialized_to_array string
      array.should be_empty
    end

    it "should return as Array of 2 elements and remove all special characters of those elements" do
      string = "---\n- '855884042998'\n- '85592327379'"
      array = Ext::ReminderGroup.deserialized_to_array string
      array.count.should eq(2)
      array.first.should eq("855884042998")
      array.last.should eq("85592327379")
    end
  end

end
