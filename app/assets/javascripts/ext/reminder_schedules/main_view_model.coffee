onReminderSchedules ->
  class @MainViewModel
    constructor: () ->
      @project = ko.observable new Project(project)
      @reminderSchedules = ko.observableArray $.map(reminderSchedules, (x) -> new ReminderSchedule(x))
      @phone_book_groups = ko.observableArray $.map(phone_book_groups, (x) -> new PhoneBookGroup(x))
      @channels = ko.observableArray $.map(channels, (x) -> new Channel(x))
      @call_flows = ko.observableArray $.map(call_flows, (x) -> new CallFlow(x))
      @variables = ko.observableArray $.map(variables, (x) -> new Variable(x))

      @currentReminderSchedule = ko.observable()

      @optionsScheduleTypes = ko.observableArray $.map([{name: "No", id: 0}, {name: "Yes", id: 1}], (x) -> [[x.name, x.id]])

      @savingReminderSchedule = ko.observable(false)

    selectingReminderSchedule: () ->

    newReminderSchedule: =>
      reminderSchedule = new ReminderSchedule
      @reminderSchedules.push(reminderSchedule)
      @currentReminderSchedule(reminderSchedule)
      reminderSchedule.hasFocus(true)

    editReminderSchedule: (reminderSchedule) =>
      # reload references data
      reminderSchedule.channel(@find_channel(reminderSchedule.channel().id()))
      reminderSchedule.call_flow(@find_call_flow(reminderSchedule.call_flow().id()))
      reminderSchedule.phone_book_group(@find_phone_book_group(reminderSchedule.phone_book_group().id()))

      @currentReminderSchedule(reminderSchedule)
      reminderSchedule.hasFocus(true)

    cancelReminderSchedule: =>
      @reminderSchedules.remove(@currentReminderSchedule) unless @currentReminderSchedule().id()
      @currentReminderSchedule(null)

    saveReminderSchedule: =>
      @savingReminderSchedule(true)
      json = {ext_reminder_schedule: @currentReminderSchedule().toJSON()}
      if @currentReminderSchedule().id()
        json._method = 'put'
        $.post "/ext/projects/#{@project().id()}/reminder_schedules/#{@currentReminderSchedule().id()}.json", json, @saveReminderScheduleCallback
      else
        $.post "/ext/projects/#{@project().id()}/reminder_schedules.json", json, @saveReminderScheduleCallback

    saveReminderScheduleCallback: (data) =>
      #if reminder schedule is new, we need to set id
      $.status.showNotice("Reminder schedule successfully #{if @currentReminderSchedule().id() then 'saved' else 'created'}", 2000)
      @currentReminderSchedule().id(data.id)

      @currentReminderSchedule(null)
      @savingReminderSchedule(false)

    deleteReminderSchedule: (reminderSchedule) =>
      if confirm("Are you sure you want to delete this reminder schedule?")
        $.post "/ext/projects/#{@project().id()}/reminder_schedules/#{reminderSchedule.id()}.json", {_method: 'delete'}, =>
          @reminderSchedules.remove(reminderSchedule)
          $.status.showNotice("Reminder schedule successfully deleted", 2000)

    find_channel: (id) =>
      return channel for channel in @channels() when channel.id() == id

    find_call_flow: (id) =>
      return call_flow for call_flow in @call_flows() when call_flow.id() == id

    find_phone_book_group: (id) =>
      return phone_book_group for phone_book_group in @phone_book_groups() when phone_book_group.id() == id