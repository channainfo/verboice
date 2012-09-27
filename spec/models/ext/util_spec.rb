require 'spec_helper'

describe Ext::Util  do
  describe "parse_date_time" do
    it "should parse date time correctly with the format %m/%d/%Y %H:%M" do
    	["12/23/2012 13:20", "10/21/2010 9:10"].each do |elm|
    		Ext::Util.parse_date_time(elm).class.should eq DateTime
    	end
    end

    it "should raise exception with other format  " do 

    	["18/02/2012 13:20", "2012/02/10 9:10", "23/14/2013",  "23/14/2013 23:60"].each do |elm|
  	  	lambda {
  	  		Ext::Util.parse_date_time(elm).class.should
  	  	}.should raise_error ArgumentError
    	end 	
    end
  end
  
  describe "date time to string" do
    it "should parse date to string with the format %m/%d/%Y %H:%M" do
      [ {:value => DateTime.new(2012,11,20,10,12,5,'+7') , :result => "11/20/2012 10:12" },
        {:value => DateTime.new(2011,4,10,20,21,9,'+7')  , :result => "04/10/2011 20:21" } ].each do |elm|
          Ext::Util.date_time_to_str(elm[:value]).should eq elm[:result]
      end
    end
  end
end