require 'spec_helper'

describe Ext::Condition do
  it "should return an empty array when hash is undefined" do
    hash = nil
    array = Ext::Condition.build hash
    array.size.should eq 0
  end

  it "should build array from hash values" do
    hash = {"0" => {variable: 'testing', operator: '=', value: '5', data_type: 'number'} }
    array = Ext::Condition.build hash
    array.size.should eq 1
    array.first.class.should eq Ext::Condition
    array.first.variable.should eq 'testing'
    array.first.operator.should eq '='
    array.first.data_type.should eq 'number'
    array.first.value.should eq '5'
  end

  describe "#evaluate?" do
    before(:each) do
      @contact = Contact.make
      @project_variable = ProjectVariable.make :name => "var1"
    end

    it "should return true when project variable doesn't exists" do
      PersistedVariable.make(contact_id: @contact.id, project_variable_id: @project_variable.id, value: "5")
      persisted_variables = @contact.persisted_variables
      condition = Ext::Condition.new "var2", "=", "5", 'number'

      condition.evaluate?(persisted_variables).should be true
    end

    it "should return true when it's match" do
      PersistedVariable.make(contact_id: @contact.id, project_variable_id: @project_variable.id, value: "5")
      persisted_variables = @contact.persisted_variables
      condition = Ext::Condition.new "var1", "=", "5", 'number'

      condition.evaluate?(persisted_variables).should be true
    end

    it "should return false when it's not match" do
      PersistedVariable.make(contact_id: @contact.id, project_variable_id: @project_variable.id, value: "5")
      persisted_variables = @contact.persisted_variables
      condition = Ext::Condition.new "var1", ">", "5", 'number'

      condition.evaluate?(persisted_variables).should be false
    end
  end

end
