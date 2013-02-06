#= require ext/reminder_schedules/on_reminder_schedules
#= require_tree ./ext/reminder_schedules/.

# We do the check again so tests don't trigger this initialization
onReminderSchedules -> if $('#reminder_schedules-main').length > 0
  window.model = new MainViewModel()
  ko.applyBindings(model)
