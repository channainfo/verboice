onReminderSchedules ->
  class @Condition
    constructor: (data) ->
      @variable = ko.observable new Variable data?.variable
      @operator = ko.observable new Operator data?.operator
      @value = ko.observable data?.value

      @variable_error = ko.computed => if @variable() then false else true
      @operator_error = ko.computed => if @operator() then false else true
      @value_error = ko.computed => if @value() then false else true

      @error = ko.computed => 
        @variable_error() || @operator_error() || @value_error()

    toJSON: =>
      variable: @variable().name()
      operator: @operator().value()
      value: @value()