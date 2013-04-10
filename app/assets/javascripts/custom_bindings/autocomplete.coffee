ko.bindingHandlers.autocomplete =
  init: (element, valueAccessor, allBindingsAccessor) ->
    value = valueAccessor()
    bindings = allBindingsAccessor()

    $(element).val ko.utils.unwrapObservable value
    $(element).autocomplete
      minLength : 1
      source    : bindings.source

  update: (element, valueAccessor) ->
    $(element).val ko.utils.unwrapObservable valueAccessor()
