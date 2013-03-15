require 'spec_helper'

describe Ext::Util  do
  describe "parse date" do
    it "should parse date correctly with the format %Y-%m-%d" do
      date_string = '2013-01-10'
      date = Ext::Util.parse_date(date_string)

      date.class.should eq Date
      date.year.should eq 2013
      date.month.should eq 1
      date.day.should eq 10
    end
  end

  describe "parse_date_time" do
    it "should parse date time correctly with the format %m/%d/%Y %H:%M" do
    	[ ["12/23/2012 10:20", "Bangkok", "2012-12-23 10:20:00 +0700" ] ,
        ["10/21/2010 9:20",  "Cairo",   "2010-10-21 09:20:00 +0200" ]  
      ].each do |elm|
    		time = Ext::Util.parse_date_time(elm[0], elm[1])
        time.class.should eq Time
        time.to_s.should eq elm[2]
    	end
    end

    it "should raise exception with other format  " do 
    	[ "18/02/2012 13:20",
        "2012/02/10 9:10", "23/14/2013",  "23/14/2013 23:60"].each do |elm|
  	  	lambda {
  	  		Ext::Util.parse_date_time(elm).class.should
  	  	}.should raise_error ArgumentError
    	end 	
    end
  end
  
  describe "Date time to string" do
    it "should parse date to string with the format %m/%d/%Y %H:%M" do
      [ {:value => DateTime.new(2012,11,20,10,12,5 , "00:00" )  , :result => "11/20/2012 17:12", :zone => "Bangkok" },
        {:value => DateTime.new(2011,4,10 ,12,21,9,  "00:00" )  , :result => "04/10/2011 14:21", :zone => "Cairo" } 
      ].each do |elm|
         Ext::Util.date_time_to_str(elm[:value], elm[:zone]).should eq elm[:result]
      end
    end
  end
end