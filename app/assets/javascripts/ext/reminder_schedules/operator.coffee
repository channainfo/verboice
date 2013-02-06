onReminderSchedules ->
  class @Operator
    @LESS_THAN = new Operator({id: 1, name: 'is less than', value: '<'})
    @LESS_THAN_OR_EQUAL = new Operator({id: 1, name: 'is less or equal to', value: '<='})
    @EQUAL = new Operator({id: 1, name: 'is equal to', value: '='})
    @GREATER_THAN_OR_EQUAL = new Operator({id: 1, name: 'is greater or equal to', value: '>='})
    @GREATER_THAN = new Operator({id: 1, name: 'is greater than', value: '>'})

    constructor: (data) ->
      @id = ko.observable data?.id
      @name = ko.observable data?.name
      @value = ko.observable data?.value

    toJSON: ->
      id: @id()
      name: @name()
      value: @value()