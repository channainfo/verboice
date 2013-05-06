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
    @account = Account.make
    @project = @account.projects.make
    @other_project = Project.make
    sign_in @account
  end

  let!(:contact) { Contact.make :project => @project, address: "1000" }
  let!(:other_contact) { Contact.make :project => @other_project }

  describe "GET index" do
    it "response 404 when project doesn't exists" do
      get :index, project_id: 9999

      response.should be_not_found
    end

    it "assigns all project contacts as @contacts" do
      get :index, {:project_id => @project.id}
      assigns(:contacts).should eq([contact])
    end
  end

  describe "POST create" do
    it "response 404 when project doesn't exists" do
      post :create, project_id: 9999

      response.should be_not_found
    end

    describe "with valid params" do
      it "assigns the current project to the contact" do
        size = Contact.all.size
        post :create, {:project_id => @project.id, :addresses => ["0123456789"]}
        Contact.all.size.should eq(size + 1)
      end
    end

    describe "with multiple address" do 
      it "should create 3 contacts" do
        size = Contact.all.size
        post :create, :project_id => @project.id, :addresses => ["01236475","0243332343","0186354633"]
        Contact.all.size.should eq(size + 3)
      end

      it "should create 2 contacts because one is existed" do
        size = Contact.all.size
        post :create, :project_id => @project.id, :addresses => ["0123456789","0123456789","0186354633"]
        Contact.all.size.should eq(size + 2)
      end
    end
  end

  describe "DELETE unregistration" do
    it "response 404 when project doesn't exists" do
      delete :unregistration, project_id: 9999

      response.should be_not_found
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
