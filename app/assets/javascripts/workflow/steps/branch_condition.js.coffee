#= require workflow/steps/input_setting

onWorkflow ->
  class window.BranchConditionSetting extends window.InputSetting
    description: () =>
      if @content_kind() == 'step'
        workflow.get_step(@step_id()).name()
      else if @content_kind() == 'variable'
        @variable()
      else if @content_kind() == 'value'
        @value()
      else if @content_kind() == 'response'
        (response.name for response in @available_responses() when response.value == @response())[0]

  class window.BranchRhsConditionSetting extends window.BranchConditionSetting
  class window.BranchLhsConditionSetting extends window.BranchConditionSetting
    content_kinds: () =>
      return [{text: 'Variable', value: 'variable'},
      {text: 'Step', value: 'step'},
      {text: 'Response', value: 'response'}]


  class window.BranchCondition
    constructor: (attrs) ->

      # Left hand side
      @lhs = new BranchLhsConditionSetting({
        variable: attrs.variable
        step: attrs.step
        response: attrs.response
      })

      # Operator
      @operator = ko.observable attrs.operator
      @operators = [
        {text: 'is equal to', value: '=='},
        {text: 'greater or equal to', value: '>='},
        {text: 'less or equal to', value: '<='},
        {text: 'greater than', value: '>'},
        {text: 'less than', value: '<'}
      ]

      # Right hand side
      @rhs = new BranchRhsConditionSetting({
        variable: attrs.rhs_variable
        step: attrs.rhs_step
        response: attrs.rhs_response
        value: attrs.rhs_value || attrs.value
      })

    to_hash: () =>
      lhs = @lhs.to_hash()
      rhs = @rhs.to_hash()

      return {
        step: lhs['step']
        variable: lhs['variable']
        response: lhs['response']
        operator: @operator()
        rhs_value: rhs['value']
        rhs_variable: rhs['variable']
        rhs_response: rhs['response']
        rhs_step: rhs['step']
      }

    operator_text_for: (operator_value) =>
      if operator_value then (operator.text for operator in @operators when operator.value == operator_value)[0] else ''

    after_initialize: () =>
      @description = ko.computed () =>
        "#{@lhs.description()} #{@operator_text_for(@operator())} #{@rhs.description()}"

    on_step_removed: (step) =>
      @lhs.on_step_removed(step)
      @rhs.on_step_removed(step)
