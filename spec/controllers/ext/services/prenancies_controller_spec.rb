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

describe Ext::Services::PrenanciesController do
  include Devise::TestHelpers

  before(:each) do
    sign_in account
  end
  let!(:account) { Account.make }
  let!(:project) { Project.make account: account }
  let!(:call_log) { CallLog.make project: project }

  it "should ignore when caller has been in reminder phone book listed" do
    Ext::ReminderPhoneBook.create name: "reminder schedule", project: project, phone_number: "85569860012"
    post :register, :status => "1", :CallSid => call_log.id, :From => "85569860012"
    Ext::ReminderPhoneBook.count.should eq(1)
  end

  it 'should subscribe caller into reminder phone book to receive reminder schedule' do
    Ext::ReminderPhoneBook.count.should eq(0)
    post :register, :status => "1", :CallSid => call_log.id, :From => "85569860012"
    Ext::ReminderPhoneBook.count.should eq(1)
  end

  it 'should unsubscribe caller from phone book to stop receive reminder schedule' do
    Ext::ReminderPhoneBook.create name: "reminder schedule", project: project, phone_number: "85569860012"
    post :register, :status => "2", :CallSid => call_log.id, :From => "85569860012"
    Ext::ReminderPhoneBook.count.should eq(0)
  end
end
