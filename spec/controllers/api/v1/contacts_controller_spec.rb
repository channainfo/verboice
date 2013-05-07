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

describe Api::V1::ContactsController do
  include Devise::TestHelpers

  before(:each) do
    @account_one = Account.make
    @account_two = Account.make
    @project = @account_one.projects.make
    @other_project = @account_two.projects.make
    sign_in @account_one
  end

  let!(:contact) { Contact.make :project => @project, address: "1000" }
  let!(:other_contact) { Contact.make :project => @other_project }

  describe "GET index" do
    it "should response 404 when project doesn't exists" do
      get :index, project_id: 9999

      response.should be_not_found
    end

    it "should response 401 when project is under another account" do
      get :index, project_id: @other_project.id

      response.code.should eq("401")
    end
  end

  describe "POST create" do
    it "should response 404 when project doesn't exists" do
      post :create, project_id: 9999

      response.should be_not_found
    end

    it "should response 401 when project is under another account" do
      post :create, project_id: @other_project.id

      response.code.should eq("401")
    end

    it "should response 400 when addresses is missing" do
      post :create, project_id: @project.id

      response.should be_bad_request
    end

    describe "with valid params" do
      it "should assigns the current project to the contact" do
        expect {
          post :create, {:project_id => @project.id, :addresses => ["0123456789"]}
        }.to change(@project.contacts, :count).by(1)
      end
    end

    describe "with multiple address" do 
      it "should ignore when addresses is empty" do
        expect {
          post :create, :project_id => @project.id, :addresses => []
        }.to change(@project.contacts, :count).by(0)
      end

      it "should create 3 contacts" do
        expect {
          post :create, :project_id => @project.id, :addresses => ["01236475","0243332343","0186354633"]
        }.to change(@project.contacts, :count).by(3)
      end

      it "should create 2 contacts because one is existed" do
        expect {
          post :create, :project_id => @project.id, :addresses => ["2000", 2000, "3000"]
        }.to change(@project.contacts, :count).by(2)
      end
    end
  end

  describe "DELETE unregistration" do
    it "should response 404 when project doesn't exists" do
      delete :unregistration, project_id: 9999

      response.should be_not_found
    end

    it "should response 401 when project is under another account" do
      delete :unregistration, project_id: @other_project.id

      response.code.should eq("401")
    end

    it "should destroy existing addresses" do
      expect {
        delete :unregistration, {:project_id => @project.id, :addresses => ["1000"]}
      }.to change(@project.contacts, :count).from(1).to(0)
    end

    it "should ignore non-existing addresses" do
      expect {
        delete :unregistration, {:project_id => @project.id, :addresses => ["2000"]}
      }.to_not change(@project.contacts, :count).by(1)
    end

    it "should destroy existing and ignore non-existing addresses" do
      expect {
        delete :unregistration, {:project_id => @project.id, :addresses => ["1000", "2000"]}
      }.to change(@project.contacts, :count).from(1).to(0)
    end
  end

end
