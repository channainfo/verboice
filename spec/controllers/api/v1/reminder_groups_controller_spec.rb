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
  let(:reminder_group) { Ext::ReminderGroup.make project: project, addresses: [] }

  it "should get index" do
    get :index, project_id: project.id
    response.should be_success
  end

  it "should create reminder group" do
    post :create, project_id: project.id, name: "foo"

    assert_response :ok
    reminder_groups = project.ext_reminder_groups.all
    reminder_groups.size.should == 1
    reminder_groups[0].name.should == "foo"
  end

  it "should response with the creation errors when invalid name" do
    post :create, project_id: project.id
    assert_response :ok

    project.ext_reminder_groups.count.should == 0

    response = JSON.parse(@response.body).with_indifferent_access
    response[:summary].should == "There were problems creating the Ext::ReminderGroup"
    response[:properties].should == [{"name" => "can not be blank"}]
  end

  it "should update reminder group" do
    put :update, project_id: project.id, id: reminder_group.id, addresses: [1000, 1001, "1000", "1001"]

    response.should be_success
    reminder_group.reload.addresses.size.should == 2
  end

  it "should destroy reminder group" do
    delete :destroy, project_id: project.id, id: reminder_group.id

    reminder_groups = project.ext_reminder_groups.all
    response.should be_success
    reminder_groups.size.should == 0
  end

end
