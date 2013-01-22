#= require workflow/steps/step

onWorkflow ->
  class window.Register extends Step
    @type = 'register'

    constructor: (attrs) ->
      super(attrs)

      @types = ko.observableArray reminder_phone_book_types.map (type) -> type.name
      @store = ko.observable attrs.store

      @current_editing_resource = ko.observable null
      @resources =
        confirmation: new ResourceEditor(@, attrs.confirmation_resource)
      @is_editing_resource = ko.computed () =>
        @current_editing_resource() != null

      @is_confirmation_resource_invalid = ko.computed () =>
        not @resources.confirmation.is_valid()

      @is_invalid = ko.computed () =>
        @is_name_invalid() or @is_confirmation_resource_invalid()

    button_class: () =>
      'luser'

    @add_to_steps: () ->
      workflow.add_step(new Register)

    @initialize: (hash) ->
      step = new Register(hash)
      return step

    to_hash: () =>
      $.extend(super,
        store: @store()
        confirmation_resource: @resources.confirmation.to_hash()
      )

    resource: (res) =>
      @resources[res]

    show_resource: (res) =>
      resource = @resources[res]
      @current_editing_resource(resource)

    show_confirmation_resource: () =>
      @show_resource('confirmation')