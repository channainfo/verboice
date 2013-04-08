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

    describe "data type" do
      describe "exact value" do
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

        it "should return false when persisted variable value is date time" do
          PersistedVariable.make(contact_id: @contact.id, project_variable_id: @project_variable.id, value: "2013-03-20|date")
          persisted_variables = @contact.persisted_variables
          condition = Ext::Condition.new "var1", "=", "5", 'number'

          condition.evaluate?(persisted_variables).should be false
        end
      end

      describe "date time" do
        before(:each) do
          @today = Date.new(2013, 03, 22)
          Date.stub!(:today).and_return(@today)
        end

        it "should today is '2013-03-22'" do
          Date.today.to_string(Date::DEFAULT_FORMAT).should eq '2013-03-22'
        end

        describe "persisted variable value is number" do
          it "should return false" do
            PersistedVariable.make(contact_id: @contact.id, project_variable_id: @project_variable.id, value: "5")
            persisted_variables = @contact.persisted_variables
            condition = Ext::Condition.new "var1", "=", "5", 'day'

            condition.evaluate?(persisted_variables).should be false
          end
        end

        describe "day ago" do
          before(:each) do
            @persisted_variable = PersistedVariable.make(contact_id: @contact.id, project_variable_id: @project_variable.id, value: "2013-03-20|date")
            @persisted_variables = @contact.persisted_variables
          end

          it "should persisted variable has 1 element" do
            @persisted_variables.length.should eq 1
          end

          it "should first element of persisted value is '2013-03-20'" do
            @persisted_variables.first.value.persisted_variable_value.should eq "2013-03-20"
          end

          it "should return true when persisted variable has value is equal to 2 day ago" do
            condition = Ext::Condition.new "var1", "=", "2", 'day'

            condition.evaluate?(@persisted_variables).should be true
          end

          it "should return false when persisted variable has no value is greater than 2 day ago" do
            condition = Ext::Condition.new "var1", ">", "2", 'day'

            condition.evaluate?(@persisted_variables).should be false
          end

          it "should return true when persisted variable value is greater or equal to 2 day ago" do
            condition = Ext::Condition.new "var1", ">=", "2", 'day'

            condition.evaluate?(@persisted_variables).should be true
          end

          it "should return false when persisted variable has no value is less than 2 day ago" do
            condition = Ext::Condition.new "var1", "<", "2", 'day'

            condition.evaluate?(@persisted_variables).should be false
          end

          it "should return true when persisted variable has value is less or equal to 2 day ago" do
            condition = Ext::Condition.new "var1", "<=", "2", 'day'

            condition.evaluate?(@persisted_variables).should be true
          end
        end

        describe "week ago" do
          before(:each) do
            @persisted_variable = PersistedVariable.make(contact_id: @contact.id, project_variable_id: @project_variable.id, value: "2013-03-15|date")
            @persisted_variables = @contact.persisted_variables
          end

          it "should persisted variable has 1 element" do
            @persisted_variables.length.should eq 1
          end

          it "should first element of persisted value is '2013-03-15'" do
            @persisted_variables.first.value.persisted_variable_value.should eq "2013-03-15"
          end

          it "should return true when persisted variable has value is equal to 1 week ago" do
            condition = Ext::Condition.new "var1", "=", "1", 'week'

            condition.evaluate?(@persisted_variables).should be true
          end

          it "should return false when persisted variable has no value is greater than 1 week ago" do
            condition = Ext::Condition.new "var1", ">", "1", 'week'

            condition.evaluate?(@persisted_variables).should be false
          end

          it "should return true when persisted variable value is greater or equal to 1 week ago" do
            condition = Ext::Condition.new "var1", ">=", "1", 'week'

            condition.evaluate?(@persisted_variables).should be true
          end

          it "should return false when persisted variable has no value is less than 1 week ago" do
            condition = Ext::Condition.new "var1", "<", "1", 'week'

            condition.evaluate?(@persisted_variables).should be false
          end

          it "should return true when persisted variable has value is less or equal to 1 week ago" do
            condition = Ext::Condition.new "var1", "<=", "1", 'week'

            condition.evaluate?(@persisted_variables).should be true
          end
        end

        describe "month ago" do
          before(:each) do
            @persisted_variable = PersistedVariable.make(contact_id: @contact.id, project_variable_id: @project_variable.id, value: "2013-02-22|date")
            @persisted_variables = @contact.persisted_variables
          end

          it "should persisted variable has 1 element" do
            @persisted_variables.length.should eq 1
          end

          it "should first element of persisted value is '2013-02-22'" do
            @persisted_variables.first.value.persisted_variable_value.should eq "2013-02-22"
          end

          it "should return true when persisted variable has value is equal to 1 month ago" do
            condition = Ext::Condition.new "var1", "=", "1", 'month'

            condition.evaluate?(@persisted_variables).should be true
          end

          it "should return false when persisted variable has no value is greater than 1 month ago" do
            condition = Ext::Condition.new "var1", ">", "1", 'month'

            condition.evaluate?(@persisted_variables).should be false
          end

          it "should return true when persisted variable value is greater or equal to 1 month ago" do
            condition = Ext::Condition.new "var1", ">=", "1", 'month'

            condition.evaluate?(@persisted_variables).should be true
          end

          it "should return false when persisted variable has no value is less than 1 month ago" do
            condition = Ext::Condition.new "var1", "<", "1", 'month'

            condition.evaluate?(@persisted_variables).should be false
          end

          it "should return true when persisted variable has value is less or equal to 1 month ago" do
            condition = Ext::Condition.new "var1", "<=", "1", 'month'

            condition.evaluate?(@persisted_variables).should be true
          end
        end

        describe "year ago" do
          before(:each) do
            @persisted_variable = PersistedVariable.make(contact_id: @contact.id, project_variable_id: @project_variable.id, value: "2012-03-22|date")
            @persisted_variables = @contact.persisted_variables
          end

          it "should persisted variable has 1 element" do
            @persisted_variables.length.should eq 1
          end

          it "should first element of persisted value is '2013-02-22'" do
            @persisted_variables.first.value.persisted_variable_value.should eq "2012-03-22"
          end

          it "should return true when persisted variable has value is equal to 1 year ago" do
            condition = Ext::Condition.new "var1", "=", "1", 'year'

            condition.evaluate?(@persisted_variables).should be true
          end

          it "should return false when persisted variable has no value is greater than 1 year ago" do
            condition = Ext::Condition.new "var1", ">", "1", 'year'

            condition.evaluate?(@persisted_variables).should be false
          end

          it "should return true when persisted variable value is greater or equal to 1 year ago" do
            condition = Ext::Condition.new "var1", ">=", "1", 'year'

            condition.evaluate?(@persisted_variables).should be true
          end

          it "should return false when persisted variable has no value is less than 1 year ago" do
            condition = Ext::Condition.new "var1", "<", "1", 'year'

            condition.evaluate?(@persisted_variables).should be false
          end

          it "should return true when persisted variable has value is less or equal to 1 year ago" do
            condition = Ext::Condition.new "var1", "<=", "1", 'year'

            condition.evaluate?(@persisted_variables).should be true
          end
        end
      end
    end
  end

end
