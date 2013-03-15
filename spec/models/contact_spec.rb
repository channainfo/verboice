require 'spec_helper'

describe Contact do
  describe "#evaluate?" do
    before(:each) do
      @contact = Contact.make
      @project_var1 = ProjectVariable.make :name => "var1"
      @project_var2 = ProjectVariable.make :name => "var2"
    end

    it "should return true when all the project variables do not exists" do
      conditions = [Ext::Condition.new("var_undefined", "=", "5")]

      @contact.evaluate?(conditions).should be true
    end

    it "should return true when it's match all conditions" do
      PersistedVariable.make(contact_id: @contact.id, project_variable_id: @project_var1.id, value: "5")
      PersistedVariable.make(contact_id: @contact.id, project_variable_id: @project_var2.id, value: "10")
      conditions = [Ext::Condition.new("var1", "=", "5"), Ext::Condition.new("var2", ">", "5")]

      @contact.evaluate?(conditions).should be true
    end

    it "should return false when it's not match at least one condition" do
      PersistedVariable.make(contact_id: @contact.id, project_variable_id: @project_var1.id, value: "5")
      PersistedVariable.make(contact_id: @contact.id, project_variable_id: @project_var2.id, value: "10")
      conditions = [Ext::Condition.new("var1", "=", "5"), Ext::Condition.new("var2", ">", "10")]

      @contact.evaluate?(conditions).should be false
    end

    it "should return true when some project variables do not exists and the rest of the project variables are matches condition" do 
      PersistedVariable.make(contact_id: @contact.id, project_variable_id: @project_var1.id, value: "5")
      conditions = [Ext::Condition.new("var1", "=", "5"), Ext::Condition.new("var_undefined", "=", "10")]

      @contact.evaluate?(conditions).should be true
    end
  end

end
