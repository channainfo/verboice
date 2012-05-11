#= require workflow/steps/step

onWorkflow ->
  class window.Goto extends Step
    @type = 'goto'

    constructor: (attrs) ->
      super(attrs)

      @jump_id = ko.observable attrs.jump

    available_steps: () =>
      (step for step in workflow.steps() when step.type() != this.type())

    button_class: () =>
      'lbookmark'

    @add_to_steps: () ->
      workflow.add_step(new Goto)

    @initialize: (hash) ->
      step = new Goto(hash)
      return step

    to_hash: () =>
      $.extend(super,
        jump: @jump_id()
      )

    default_name: () =>
      'Link'

    can_add_next: () =>
      false

    can_continue: () =>
      false

    on_step_removed: (step) =>
      @jump_id(null) if step.id == parseInt(@jump_id())
