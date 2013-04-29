# Copyright (C) 2010-2012, InSTEDD
#
# This file is part of Verboice.
#
# Verboice is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Verboice is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Verboice.  If not, see <http://www.gnu.org/licenses/>.

require 'spec_helper'

module Commands
  describe RegisterCommand do

    let(:pbx) { double('pbx') }

    before :each do
      pbx.stub :record
    end

    it "should register caller to reminder group when caller is new" do
      contact = Contact.make :address => "1000"
      project = contact.project
      call_flow = CallFlow.make project: project
      call_log = CallLog.make call_flow: call_flow
      reminder_group = project.ext_reminder_groups.create! name: "Pregnancy"

      session = Session.new :pbx => pbx, :call_log => call_log
      session.stub :address => contact.address

      cmd = RegisterCommand.new "Pregnancy"
      cmd.next = :next
      cmd.run(session).should == :next

      Ext::ReminderGroup.first.addresses.size.should == 1
      Ext::ReminderGroup.first.project.name.size == project.name
      Ext::ReminderGroup.first.addresses.first.should == session.address
    end

    it "should ignore caller registration to reminder phone book when caller has been registered to reminder phone book type" do
      contact = Contact.make :address => "1000"
      project = contact.project
      call_flow = CallFlow.make project: project
      call_log = CallLog.make call_flow: call_flow
      reminder_group = project.ext_reminder_groups.create! name: "Pregnancy", addresses: ["1000"]

      session = Session.new :pbx => pbx, :call_log => call_log
      session.stub :address => contact.address

      # before process the step
      Ext::ReminderGroup.first.addresses.size.should == 1
      # Ext::ReminderPhoneBook.all.size.should eq(1)

      cmd = RegisterCommand.new "Pregnancy"
      cmd.next = :next
      cmd.run(session).should == :next

      # after process the step
      Ext::ReminderGroup.first.addresses.size.should == 1
      Ext::ReminderGroup.first.addresses.last.should == "1000"
    end

  end
end