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
    array.first[:variable].should eq 'testing'
    array.first[:operator].should eq '='
    array.first[:value].should eq '5'
  end
end
