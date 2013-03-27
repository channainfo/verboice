require 'spec_helper'

describe Ext::TimeParser  do
  describe "TimeParser.parse" do
    it "should parse date time correctly with the format %Y-%m-%d %H:%M" do
      [ 
        {date_time_string: "2012-12-23 10:20", format: Ext::ReminderSchedule::DEFAULT_DATE_TIME_FORMAT, timezone: "Bangkok", result: "2012-12-23 10:20:00 +0700"},
        {date_time_string: "2012-10-21 9:20", format: Ext::ReminderSchedule::DEFAULT_DATE_TIME_FORMAT, timezone: "Cairo", result: "2012-10-21 09:20:00 +0200"}
      ].each do |elm|
        time = Ext::TimeParser.parse(elm[:date_time_string], elm[:format], elm[:timezone])
        time.class.should eq Time
        time.to_s.should eq elm[:result]
      end
    end

    it "should raise exception with other format  " do 
      [{date_time_string: "18/02/2012 13:20", format: Ext::ReminderSchedule::DEFAULT_DATE_TIME_FORMAT, timezone: "Bangkok", result: "18/02/2012 13:20"}].each do |elm|
        lambda {
          Ext::TimeParser.parse(elm[:date_time_string, elm[:format], elm[:timezone]]).class.should
        }.should raise_error ArgumentError
      end
    end
  end
end