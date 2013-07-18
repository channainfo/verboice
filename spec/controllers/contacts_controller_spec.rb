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

describe ContactsController do
  include Devise::TestHelpers

  before(:each) do
    @account = Account.make
    @project = @account.projects.make
    @other_project = Project.make
    sign_in @account
  end

  let!(:contact) { Contact.make :project => @project }
  let!(:other_contact) { Contact.make :project => @other_project }

  describe "GET index" do
    it "assigns all project contacts as @contacts" do
      get :index, {:project_id => @project.id}
      assigns(:contacts).should eq([contact])
    end
  end

  describe "GET new" do
    it "assigns a new contact as @contact" do
      get :new, {:project_id => @project.id}
      assigns(:contact).should be_a_new(Contact)
    end
  end

  describe "GET edit" do
    it "assigns the requested contact as @contact" do
      get :edit, {:project_id => @project.id, :id => contact.to_param}
      assigns(:contact).should eq(contact)
    end

    it "fails if the requested contact is not in current project" do
      expect {
        get :edit, {:project_id => @project.id, :id => other_contact.to_param}
      }.should raise_error
      assigns(:contact).should be_nil
    end
  end

  describe "POST create" do
    describe "with valid params" do
      it "creates a new Contact" do
        expect {
          post :create, {:project_id => @project.id, :contact => {:addresses_attributes => [{:address => '123'}]}}
        }.to change(Contact, :count).by(1)
      end

      it "assigns a newly created contact as @contact" do
        post :create, {:project_id => @project.id, :contact => {:addresses_attributes => [{:address => '123'}]}}
        assigns(:contact).should be_a(Contact)
        assigns(:contact).should be_persisted
      end

      it "redirects to index" do
        post :create, {:project_id => @project.id, :contact => {:addresses_attributes => [{:address => '123'}]}}
        response.should redirect_to(project_contacts_url(@project))
      end

      it "assigns the current project to the contact" do
        post :create, {:project_id => @project.id, :contact => {:addresses_attributes => [{:address => '123'}]}}
        assigns(:contact).project.should eq(@project)
      end
    end

    describe "with invalid params" do
      it "assigns a newly created but unsaved contact as @contact" do
        Contact.any_instance.stub(:save).and_return(false)
        post :create, {:project_id => @project.id, :contact => {}}
        assigns(:contact).should be_a_new(Contact)
      end

      it "re-renders the 'new' template" do
        Contact.any_instance.stub(:save).and_return(false)
        post :create, {:project_id => @project.id, :contact => {}}
        response.should render_template("new")
      end
    end
  end

  describe "PUT update" do
    describe "with valid params" do
      it "updates the requested contact" do
        Contact.any_instance.should_receive(:update_attributes).with({'these' => 'params'})
        put :update, {:project_id => @project.id, :id => contact.to_param, :contact => {'these' => 'params'}}
      end

      it "assigns the requested contact as @contact" do
        put :update, {:project_id => @project.id, :id => contact.to_param, :contact => Contact.plan}
        assigns(:contact).should eq(contact)
      end

      it "redirects to index" do
        put :update, {:project_id => @project.id, :id => contact.to_param, :contact => Contact.plan}
        response.should redirect_to(project_contacts_url(@project))
      end
    end

    describe "with invalid params" do
      it "assigns the contact as @contact" do
        Contact.any_instance.stub(:save).and_return(false)
        put :update, {:project_id => @project.id, :id => contact.to_param, :contact => {}}
        assigns(:contact).should eq(contact)
      end

      it "re-renders the 'edit' template" do
        Contact.any_instance.stub(:save).and_return(false)
        put :update, {:project_id => @project.id, :id => contact.to_param, :contact => {}}
        response.should render_template("edit")
      end
    end

    it "fails if the requested contact is not in current project" do
      expect {
        put :update, {:project_id => @project.id, :id => other_contact.to_param}
      }.should raise_error
      assigns(:contact).should be_nil
    end
  end

  describe "DELETE destroy" do
    it "destroys the requested contact" do
      expect {
        delete :destroy, {:project_id => @project.id, :id => contact.to_param}
      }.to change(Contact, :count).by(-1)
    end

    it "redirects to the contacts list" do
      delete :destroy, {:project_id => @project.id, :id => contact.to_param}
      response.should redirect_to(project_contacts_url(@project))
    end

    it "fails if the requested contact is not in current project" do
      expect {
        delete :destroy, {:project_id => @project.id, :id => other_contact.to_param}
      }.should raise_error
      assigns(:contact).should be_nil
      Contact.find(other_contact.id).should eq(other_contact)
    end
  end

  describe "GET invitable" do
    before(:each) do
      contact.addresses.destroy_all # clear the default faker one for addresses in contact
      contact.addresses.build(address: "1000").save
    end

    it "should contact has first address is '1000'" do
      contact.first_address.should == "1000"
    end

    it "should response 200 and assign all the contacts in project that matches to term to @contacts" do
      get :invitable, :project_id => @project.id, term: "100"

      response.should be_success
      assigns(:contacts).should eq([contact])
    end

    it "should response 200 and assigns array of empty when there's no any contacts match the term to @contacts" do
      get :invitable, :project_id => @project.id, term: "999"

      response.should be_success
      assigns(:contacts).should be_empty
    end
  end

end
