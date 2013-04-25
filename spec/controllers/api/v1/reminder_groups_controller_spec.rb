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

describe Api::V1::ReminderGroupsController do
  include Devise::TestHelpers

  before(:each) do
    sign_in account
  end
  let!(:account) { Account.make }
  let!(:project) { Project.make account: account }
  let(:reminder_group) { Ext::ReminderGroup.make project: project }

  it "create reminder group" do
    post :create, project_id: project.id, name: "foo", addresses: [1000, 1001], format: :json

    assert_response :ok
    reminder_groups = project.ext_reminder_groups.all
    reminder_groups.size.should == 1
    reminder_groups[0].name.should == "foo"
  end

  it "should response with the creation errors when invalid name" do
    post :create, project_id: project.id, addresses: [1000, 1001], format: :json
    assert_response :ok

    project.ext_reminder_groups.count.should == 0

    response = JSON.parse(@response.body).with_indifferent_access
    response[:summary].should == "There were problems creating the Ext::ReminderGroup"
    response[:properties].should == [{"name" => "can not be blank"}]
  end

  it "should register contacts" do
    put :register_addresses, id: reminder_group.id, addresses: ["1000"]

    assert_response :ok
    reminder_groups = project.ext_reminder_groups.all
    reminder_groups.size.should == 1
    reminder_groups[0].addresses.last.should == "1000"
  end

end
