onReminderSchedules ->
  class @ReminderChannel
    constructor: (data) ->
      @id = ko.observable(data.id)
      @reminder_schedule_id = ko.observable(data.reminder_schedule_id)
      @channel = ko.observable(data.channel)

    toJSON: =>
      id: @id()
      reminder_schedule_id: @reminder_schedule_id()
      channel_id: @channel().id()
