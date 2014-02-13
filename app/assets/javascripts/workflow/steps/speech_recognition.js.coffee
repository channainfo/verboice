#= require workflow/steps/step

onWorkflow ->
  class window.SpeechRecognition extends Step
    @type = 'speech_recognition' #template ref id

    constructor: (attrs) ->
      super(attrs)

      @old_result1   = ko.observable attrs.result1
      @old_accuracy1 = ko.observable attrs.accuracy1

      @old_result2   = ko.observable attrs.result2
      @old_accuracy2 = ko.observable attrs.accuracy2

      @old_result3   = ko.observable attrs.result3
      @old_accuracy3 = ko.observable attrs.accuracy3


      @result1   = ko.observable attrs.result1
      @accuracy1 = ko.observable attrs.accuracy1

      @result2   = ko.observable attrs.result2
      @accuracy2 = ko.observable attrs.accuracy2

      @result3   = ko.observable attrs.result3
      @accuracy3 = ko.observable attrs.accuracy3


      @defines_result1   = ko.observable !!attrs.result1
      @defines_accuracy1 = ko.observable !!attrs.accuracy1


      @timeout = ko.observable (attrs.timeout || '10')
      @stop_key = ko.observable (attrs.stop_key || '#')

      @current_editing_resource = ko.observable null

      @resources =
        explanation:  new ResourceEditor(@, attrs.explanation_resource)
        confirmation: new ResourceEditor(@, attrs.confirmation_resource)

      @is_editing_resource = ko.computed () =>
        @current_editing_resource() != null

      @is_explanation_resource_invalid = ko.computed () =>
        not @resources.explanation.is_valid()

      @is_confirmation_resource_invalid = ko.computed () =>
        not @resources.confirmation.is_valid()

      @is_result1_value_invalid = ko.computed () =>
        not @result1()

      @is_accuracy1_value_invalid = ko.computed () =>
        not @accuracy1()

      @is_invalid = ko.computed () =>
        @is_name_invalid() or @is_explanation_resource_invalid() or @is_confirmation_resource_invalid() or @is_result1_value_invalid() or @is_accuracy1_value_invalid()

    button_class: () =>
      'control_step lspeech_recognition'

    default_name: () =>
      'Speech Recognition'
  
    @add_to_steps: () ->
      workflow.add_step(new SpeechRecognition)

    @initialize: (hash) ->
      step = new SpeechRecognition(hash)
      return step


    to_hash: () =>
      $.extend(super,
        # old_store: (if @defines_store() then @old_store() else null)
        # store: (if @defines_store() then @store() else null)
        old_result1: (if @old_result1() then @old_result1() else null)
        old_accuracy1: (if @old_accuracy1() then @old_accuracy1() else null)
        old_result2: (if @old_result2() then @old_result2() else null)
        old_accuracy2: (if @old_accuracy2() then @old_accuracy2() else null)
        old_result3: (if @old_result3() then @old_result3() else null)
        old_accuracy3: (if @old_accuracy3() then @old_accuracy3() else null)



        result1: (if @result1() then @result1() else null)
        accuracy1: (if @accuracy1() then @accuracy1() else null)
        result2: (if @result2() then @result2() else null)
        accuracy2: (if @accuracy2() then @accuracy2() else null)
        result3: (if @result3() then @result3() else null)
        accuracy3: (if @accuracy3() then @accuracy3() else null)
        
        timeout: @timeout()
        stop_key: @stop_key()
        explanation_resource: @resources.explanation.to_hash()
        confirmation_resource: @resources.confirmation.to_hash()
      )

    resource: (res) =>
      @resources[res]

    show_resource: (res) =>
      resource = @resources[res]
      @current_editing_resource(resource)

    show_explanation_resource: () =>
      @show_resource('explanation')

    show_confirmation_resource: () =>
      @show_resource('confirmation')

    available_keys: () =>
      ['1','2','3','4','5','6','7','8','9','0','#','*']
