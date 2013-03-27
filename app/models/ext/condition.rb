module Ext
  class Condition
    attr_accessor :variable, :operator, :value, :data_type

    def initialize variable, operator, value, data_type
      @variable = variable
      @operator = operator
      @value = value
      @data_type = data_type
    end

    def self.build hash
      conditions = []
      hash.each do |k, v|
        conditions.push Ext::Condition.new v[:variable], v[:operator], v[:value], v[:data_type]
      end if hash
      conditions
    end

    def evaluate? persisted_variables
      match = false
      project_variable = ProjectVariable.where(:name => self.variable).first
      if project_variable
        persisted_variables.each do |persisted_variable|
          if persisted_variable.project_variable_id == project_variable.id
            if data_type == "number"
              left_value = persisted_variable.value.persisted_variable_value
              right_value = value
            else
              left_value = Time.parse(persisted_variable.value.persisted_variable_value)
              right_value = Time.parse(Time.parse(Time.now.to_s).to_string(Time::DEFAULT_DATE_TIME_FORMAT)) - eval("#{value}.#{data_type}")
            end
            
            match = Ext::Comparison.compare(left_value.to_i, operator, right_value.to_i)
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