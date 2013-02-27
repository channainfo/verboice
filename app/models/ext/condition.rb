module Ext
  class Condition
    attr_accessor :variable, :operator, :value

    def initialize variable, operator, value
      @variable = variable
      @operator = operator
      @value = value
    end

    def self.build hash
      conditions = []
      hash.each do |k, v|
        conditions.push Ext::Condition.new v[:variable], v[:operator], v[:value]
      end if hash
      conditions
    end

    def evaluate? persisted_variables
      match = false
      project_variable = ProjectVariable.where(:name => self.variable).first
      if project_variable
        result = persisted_variables.where(:project_variable_id => project_variable.id).where("value #{self.operator} #{self.value}")
        match = true if result.size > 0
      else
        match = true # ignore is default if project variable doesn't exists
      end
      match
    end
  end
end