onReminderSchedules ->
  class @ReminderSchedule
    @NO_REPEAT = '0'
    @REPEAT = '1'
    @DEFAULT_RECUR = 1

    constructor: (data) ->
      @id = ko.observable data?.id
      @phone_book_group = ko.observable if data?.reminder_phone_book_type_id then window.model.find_phone_book_group data?.reminder_phone_book_type_id else new PhoneBookGroup
      @call_flow = ko.observable if data?.call_flow_id then window.model.find_call_flow data?.call_flow_id else new CallFlow
      @channel = ko.observable if data?.channel_id then window.model.find_channel data?.channel_id else new Channel
      @repeat = ko.observable data?.schedule_type ? ReminderSchedule.NO_REPEAT
      @is_repeat = ko.computed =>
        parseInt(@repeat()) is parseInt(ReminderSchedule.REPEAT)

      @enable_css = ko.observable 'cb-enable'
      @disable_css = ko.observable 'cb-disalbe'
      @repeat_init = ko.computed =>
        if @is_repeat()
          @enable_css 'cb-enable selected'
          @disable_css 'cb-disable'
        else
          @enable_css 'cb-enable'
          @disable_css 'cb-disable selected'

      @start_date = ko.observable data?.start_date
      @from_time = ko.observable data?.time_from
      @to_time = ko.observable data?.time_to
      @timezone = ko.observable data?.timezone
      # repeat
      @recur = ko.observable data?.recur ? ReminderSchedule.DEFAULT_RECUR

      #conditions
      @conditions = ko.observableArray if data?.conditions then $.map(data.conditions, (x) -> new Condition(x)) else []
      @conditions_description = ko.computed =>
        items = []
        condition_items = $.map(@conditions(), (x) -> x.toJSON() if x.valid())
        for condition in condition_items
          item = []
          item.push if items.length > 0 then and_text else when_text
          item.push condition.variable
          item.push window.model.find_operator(condition.operator).name()
          item.push condition.value
          items.push item.join(" ")
        items.join(" ")

      @current_condition = ko.observable null

      @weekdays = ko.observableArray Day.selected_weekdays(data?.days)
      
      @hasFocus = ko.observable(false)

      @phone_book_group_error = ko.computed => if @has_phone_book_group() then null else "the reminder schedule's call flow is missing"
      @call_flow_error = ko.computed => if @has_call_flow() then null else "the reminder schedule's call flow is missing"
      @channel_error = ko.computed => if @has_channel() then null else "the reminder schedule's channel is missing"
      @start_date_error = ko.computed => if @has_start_date() then null else "the reminder schedule's client start date is missing"
      @from_time_error = ko.computed => if @has_from_time() then null else "the reminder schedule's from time is missing"
      @to_time_error = ko.computed => if @has_to_time() then null else "the reminder schedule's to time is missing"
      @call_time_error = ko.computed => if @has_from_time() and @has_to_time() then null else "the reminder schedule's call time is missing"
      @timezone_error = ko.computed => if @has_timezone() then null else "the reminder schedule's timezone is missing"
      @days_error = ko.computed => 
        if @is_repeat() && !@has_days_selected() then true else false
      @recur_error = ko.computed => if @has_recur() then null else "the reminder schedule's recur is missing"

      @error = ko.computed =>
        @phone_book_group_error() || @call_flow_error() || @channel_error() || @days_error() || @timezone_error() || @start_date_error() || @from_time_error() || @to_time_error() || @recur_error()
      @valid = ko.computed => !@error()

    repeat_enable: =>
      @repeat(ReminderSchedule.REPEAT)

    repeat_disable: =>
      @repeat(ReminderSchedule.NO_REPEAT)


    day_selected: (day) =>
      day.selected !day.selected()

      # refresh observableArray
      data = @weekdays().slice(0);
      @weekdays([]);
      @weekdays(data);

    add_condition: =>
      condition = new Condition()
      @conditions.push(condition)
      @current_condition(condition)

    show_condition: (condition) =>
      @current_condition(condition)

    cancel_condition_edition: =>
      @conditions.remove(@current_condition())
      @current_condition(null)

    close_condition_edition: =>
      @current_condition(null)

    remove_condition: (condition) => 
      @current_condition(null)
      @conditions.remove(condition)

    has_phone_book_group: => $.trim(@phone_book_group()).length > 0
    has_call_flow: => $.trim(@call_flow()).length > 0
    has_channel: => $.trim(@channel()).length > 0
    has_start_date: => $.trim(@start_date()).length > 0
    has_from_time: => $.trim(@from_time()).length >= 3 and @from_time().split(":").length == 2 # "1:00" or "01:00"
    has_to_time: => $.trim(@to_time()).length >= 3 and @to_time().split(":").length == 2 # "1:00" or "01:00"
    has_timezone: => $.trim(@timezone()).length > 0
    has_recur: => $.trim(@recur()).length > 0
    has_days_selected: => if $.map(@weekdays(), (x) -> x if x.selected() == true).length > 0 then true else false
    has_conditions: => if $.map(@conditions(), (x) -> x).length > 0 then true else false

    toJSON: =>
      reminder_phone_book_type_id: @phone_book_group().id()
      call_flow_id: @call_flow().id()
      channel_id: @channel().id()
      timezone: @timezone()
      client_start_date: @start_date()
      time_from: @from_time()
      time_to: @to_time()
      schedule_type: @repeat()
      days: $.map(@weekdays(), (x) -> x.id() if x.selected() == true).join(",") if @is_repeat()
      recursion: @recur() if @is_repeat()
      conditions: $.map(@conditions(), (x) -> x.toJSON() if x.valid())