module Ext
  class Condition
    attr_accessor :project_id, :variable, :operator, :value, :data_type

    def initialize project_id, variable, operator, value, data_type
      @project_id = project_id
      @variable = variable
      @operator = operator
      @value = value
      @data_type = data_type
    end

    def self.build hash
      conditions = []
      hash.each do |k, v|
        conditions.push Ext::Condition.new v[:project_id], v[:variable], v[:operator], v[:value], v[:data_type]
      end if hash
      conditions
    end

    def evaluate? persisted_variables
      match = false
      project_variable = ProjectVariable.where(:name => self.variable, :project_id => @project_id).first
      if project_variable
        persisted_variables.each do |persisted_variable|
          if persisted_variable.project_variable_id == project_variable.id
            if data_type == "number"
              left_value = persisted_variable.value.number? ? persisted_variable.value.to_i : nil
              right_value = value.to_i
            else
              left_value = Date.strptime(Date.today.to_string(Date::DEFAULT_FORMAT), Date::DEFAULT_FORMAT) - eval("#{value}.#{data_type}")
              right_value = persisted_variable.value.date? ? Date.strptime(persisted_variable.value, Date::DEFAULT_FORMAT) : nil
            end
            
            match = Ext::Comparison.compare(left_value, operator, right_value) if left_value and right_value
            break if match
          end
        end
      else
        match = true # ignore is default if project variable doesn't exists
      end
      match
    end
  end
end