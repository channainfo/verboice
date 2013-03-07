require 'spec_helper'

describe Ext::ReminderPhoneBooksController do
  include Devise::TestHelpers

  let!(:account) { Account.make }
  let!(:project) { Project.make :account => account }

  before(:each) do
    sign_in account
  end

  context "update reminder phone book types" do
    it "should create new ext reminder phone book types" do
      params = {
        :ext_reminder_phone_book_types_attributes => {
          '0' => {
            'name'=> "bar",
          }
        }
      }

      put :update_reminder_phone_book_types, :project_id => project.id, :project => params
      response.should be_redirect
      flash[:notice].should eq(I18n.t("controllers.projects_controller.reminder_phone_book_types_successfully_updated"))
      Ext::ReminderPhoneBookType.count.should be(1)
    end

    it "should update an existing ext reminder phone book types" do
      reminder_phone_book_type = Ext::ReminderPhoneBookType.create :name => "bar", :project_id => project.id
      params = {
        :ext_reminder_phone_book_types_attributes => {
          '0' => {
            'name' => "foo",
            'id' => reminder_phone_book_type.id
          }
        }
      }

      Ext::ReminderPhoneBookType.count.should be(1)
      put :update_reminder_phone_book_types, :project_id => project.id, :project => params
      response.should be_redirect
      flash[:notice].should eq(I18n.t("controllers.projects_controller.reminder_phone_book_types_successfully_updated"))
      Ext::ReminderPhoneBookType.first.name.should eq("foo")
      Ext::ReminderPhoneBookType.count.should be(1)
    end
  end
end