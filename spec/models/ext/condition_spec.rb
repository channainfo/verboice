require 'spec_helper'

describe Ext::Condition do
  it "should return an empty array when hash is undefined" do
    hash = nil
    array = Ext::Condition.build hash
    array.size.should eq 0
  end

  it "should build array from hash values" do
    hash = {"0" => {variable: 'testing', operator: '=', value: '5'} }
    array = Ext::Condition.build hash
    array.size.should eq 1
    array.first.class.should eq Ext::Condition
    array.first.variable.should eq 'testing'
    array.first.operator.should eq '='
    array.first.value.should eq '5'
  end

  describe "#evaluate?" do
    before(:each) do
      @call_log = CallLog.make
      @project_variable = ProjectVariable.make :name => "var1"
    end

    it "should return true when project variable doesn't exists" do
      condition = Ext::Condition.new "var2", "=", "5"
      condition.evaluate?.should be true
    end

    it "should return true when it's match" do
      CallLogAnswer.make call_log_id: @call_log.id, project_variable_id: @project_variable.id, value: 5
      condition = Ext::Condition.new "var1", "=", "5"

      condition.evaluate?.should be true
    end

    it "should return false when it's not match" do
      CallLogAnswer.make call_log_id: @call_log.id, project_variable_id: @project_variable.id, value: 5
      condition = Ext::Condition.new "var1", ">", "5"

      condition.evaluate?.should be false
    end
  end

end
