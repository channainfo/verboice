require 'spec_helper'

describe CallLogSearch do
  def assert_search(text)
    CallLog.search(text).to_sql
  end

  it "should search by call_flow_id" do
    assert_search("call_flow_id:123").should match /call_flow_id = '123'/
  end

  it "should search by phone_number" do
    assert_search("phone_number:123").should match /address like '%123%'/
  end
end