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

    def evaluate?
      match = false
      project_variable = ProjectVariable.where(:name => self.variable).first
      if project_variable
        call_log_answers = CallLogAnswer.where(:project_variable_id => project_variable.id).where("value #{self.operator} #{self.value}")
        match = true if call_log_answers.size > 0
      else
        match = true # ignore is default if project variable doesn't exists
      end
      match
    end
  end
end