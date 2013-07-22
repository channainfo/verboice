require 'spec_helper'

describe Ext::ReminderGroupsController do
  include Devise::TestHelpers

  let!(:account) { Account.make }
  let!(:project) { Project.make :account => account }

  before(:each) do
    sign_in account
  end

  it "should get index" do
    get :index, project_id: project.id
    response.should be_success
  end

end