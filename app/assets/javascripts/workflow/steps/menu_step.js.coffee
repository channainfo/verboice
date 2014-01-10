#= require workflow/steps/step_with_children
#= require workflow/steps/menu_option

onWorkflow ->
  class window.Menu extends StepWithChildren
    @type = 'menu'

    constructor: (attrs) ->
      super(attrs)

      @options = ko.observableArray([])
      @new_option_command = ko.observable null

      @current_editing_resource = ko.observable null
      @timeout = ko.observable(attrs.timeout ? menu_default_time_out_in_seconds)
      @number_of_attempts = ko.observable(attrs.number_of_attempts ? menu_default_number_of_attempts)

      @store = ko.observable attrs.store
      @defines_store = ko.observable !!attrs.store

      @resources =
        invalid:     new ResourceEditor(@, attrs.invalid_resource)
        explanation: new ResourceEditor(@, attrs.explanation_resource)
        options:     new ResourceEditor(@, attrs.options_resource)

      @is_editing_resource = ko.computed () =>
        @current_editing_resource() != null

      @available_numbers = ko.computed () =>
        used_numbers = (opt.number() for opt in @options())
        (number for number in ['1','2','3','4','5','6','7','8','9','0','#','*'] when number not in used_numbers)

      @default_command_selected = ko.observable 'skip'
      @default = ko.observable( new DefaultOption(attrs.default, @))

      # Validations
      @is_explanation_resource_invalid = ko.computed () =>
        if @resources.explanation.name() and not @resources.explanation.is_valid() then true else false
      @is_options_resource_invalid = ko.computed () =>
        not @resources.options.is_valid()

      @are_options_invalid = ko.computed () =>
        (@options().length < 1)

      @is_invalid = ko.computed () =>
        @is_name_invalid() or @are_options_invalid() or @is_options_resource_invalid() or @is_explanation_resource_invalid()


    @initialize: (hash) ->
      menu = new Menu(hash)
      menu.options(new MenuOption(opt.number, opt.next, menu) for opt in (hash.options || []))
      return menu

    remove_child_step: (child_step) =>
      super(child_step)
      if child_step.is_default
        @default_command_selected('skip')
      else
        @options.remove child_step

    child_steps: () =>
      child = @options().sort((opt1, opt2) => opt1.number().charCodeAt(0) - opt2.number().charCodeAt(0)).concat(@default())

    add_option: () =>
      new_step_id = @new_child_step_for @new_option_command()
      @options.push(new MenuOption(@available_numbers()[0], new_step_id, @))

    remove_option_with_confirm: (option) =>
      if confirm("Are you sure you want to remove option #{option.number()} and all its steps?")
        @remove_child_step(option)

    button_class: () =>
      'ldial'

    can_insert_after: () =>
      false

    @add_to_steps: () ->
      workflow.add_step(new Menu)

    to_hash: () =>
      $.extend(super,
        # store: (if @defines_store() then @store() else null)
        store: (if @store() then @store() else null)
        options: (option.to_hash() for option in @options())
        invalid_resource: @resources.invalid.to_hash()
        explanation_resource: @resources.explanation.to_hash()
        options_resource: @resources.options.to_hash()
        timeout: @timeout()
        number_of_attempts: @number_of_attempts()
        default: @default().next_id
      )

    resource: (res) =>
      @resources[res]

    show_resource: (res) =>
      resource = @resources[res]
      @current_editing_resource(resource)

    show_invalid_resource: () =>
      @show_resource('invalid')

    show_options_resource: () =>
      @show_resource('options')

    show_explanation_resource: () =>
      @show_resource('explanation')
