require 'spec_helper'

describe Ext::ReminderGroupsController do
  include Devise::TestHelpers

  let!(:account) { Account.make }
  let!(:project) { Project.make :account => account }

  before(:each) do
    sign_in account
  end

  it "should create reminder group when attributes are valid" do
    post :create, project_id: project.id, ext_reminder_group: {name: 'rspec'}
    response.should be_success
  end

  it "should not create reminder group when attributes are invalid" do
    post :create, project_id: project.id, ext_reminder_group: {name: ''}
    response.should be_bad_request
  end

  it "should update reminder group when attributes are valid" do
    reminder_group = Ext::ReminderGroup.create name: 'rspec', project_id: project.id
    put :update, project_id: project.id, id: reminder_group.id, ext_reminder_group: {name: 'rspec'}
    response.should be_success
  end

  it "should not update reminder group when attributes are invalid" do
    reminder_group = Ext::ReminderGroup.create name: 'rspec', project_id: project.id
    put :update, project_id: project.id, id: reminder_group.id, ext_reminder_group: {name: ''}
    response.should be_bad_request
  end

  it "should destroy reminder group" do
    reminder_group = Ext::ReminderGroup.create name: 'rspec', project_id: project.id
    delete :destroy, project_id: project.id, id: reminder_group.id
    response.should be_success
  end

end