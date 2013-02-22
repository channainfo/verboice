#= require workflow/steps/step

onWorkflow ->
  class window.Datetime extends Step
    @type = 'datetime'

    constructor: (attrs) ->
      super(attrs)

      @types = ko.observableArray(['Day', 'Week', 'Month'])
      @selected = ko.observable attrs.unit
      @store = ko.observable attrs.store
      @defines_store = ko.observable !!attrs.store

      @current_editing_resource = ko.observable null
      @resources =
        explanation:  new ResourceEditor(@, attrs.explanation_resource)
      @is_editing_resource = ko.computed () =>
        @current_editing_resource() != null

      @is_explanation_resource_invalid = ko.computed () =>
        not @resources.explanation.is_valid()

      @is_invalid = ko.computed () =>
        @is_name_invalid() or @is_explanation_resource_invalid

    button_class: () =>
      'ldate'

    @add_to_steps: () ->
      workflow.add_step(new Datetime)

    @initialize: (hash) ->
      step = new Datetime(hash)
      return step

    to_hash: () =>
      $.extend(super,
        unit: @selected(),
        store: (if @defines_store() then @store() else null)
        explanation_resource: @resources.explanation.to_hash()
      )

    resource: (res) =>
      @resources[res]

    show_resource: (res) =>
      resource = @resources[res]
      @current_editing_resource(resource)

    show_explanation_resource: () =>
      @show_resource('explanation')