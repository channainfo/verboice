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
    flash[:notice].should eq(I18n.t("controllers.reminder_phone_books.successfully_created"))
    response.should be_success
  end

  it "should not create reminder group when attributes are invalid" do
    post :create, project_id: project.id, ext_reminder_group: {name: ''}
    flash[:notice].should eq(I18n.t("controllers.reminder_phone_books.create_error"))
    response.should be_success
  end

  it "should update reminder group when attributes are valid" do
    reminder_group = Ext::ReminderGroup.create name: 'rspec', project_id: project.id
    put :update, project_id: project.id, id: reminder_group.id, ext_reminder_group: {name: 'rspec'}
    flash[:notice].should eq(I18n.t("controllers.reminder_phone_books.successfully_updated"))
    response.should be_success
  end

  it "should not update reminder group when attributes are invalid" do
    reminder_group = Ext::ReminderGroup.create name: 'rspec', project_id: project.id
    put :update, project_id: project.id, id: reminder_group.id, ext_reminder_group: {name: ''}
    flash[:notice].should eq(I18n.t("controllers.reminder_phone_books.update_error"))
    response.should be_success
  end

  it "should destroy reminder group" do
    reminder_group = Ext::ReminderGroup.create name: 'rspec', project_id: project.id
    delete :destroy, project_id: project.id, id: reminder_group.id
    flash[:notice].should eq(I18n.t("controllers.reminder_phone_books.successfully_deleted", contact: reminder_group.name))
    response.should be_success
  end

end