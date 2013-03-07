onReminderSchedules ->
  class @Condition
    constructor: (data) ->
      @variable = ko.observable if data?.variable then window.model.find_variable data?.variable else new Variable
      @operator = ko.observable if data?.operator then window.model.find_operator data?.operator else new Operator
      @value = ko.observable data?.value

      @variable_name = ko.computed => if @variable() then @variable().name else "undefined"
      @operator_name = ko.computed => if @operator() then @operator().name else "undefined"
      @value_text = ko.computed => if @value() then @value() else "undefined"

      @variable_error = ko.computed => if @variable() then false else true
      @operator_error = ko.computed => if @operator() then false else true
      @value_error = ko.computed => if @value() then false else true

      @error = ko.computed => 
        @variable_error() || @operator_error() || @value_error()

    toJSON: =>
      variable: @variable().name()
      operator: @operator().code()
      value: @value()

    valid: =>
      !@error()