#= require ext/reminder_phone_books/on_reminder_phone_books
#= require_tree ./ext/reminder_phone_books/.

# We do the check again so tests don't trigger this initialization
onReminderPhoneBooks -> if $('#reminder_phone_books-main').length > 0
  match = window.location.toString().match(/\/projects\/(\d+)\/reminder_phone_books/)
  project_id = parseInt(match[1])

  window.model = new ReminderPhoneBookMainViewModel(project_id)
  ko.applyBindings(window.model)

  $.get "/ext/projects/#{project_id}/reminder_phone_books.json", (data) ->
    window.model.reminder_phone_books $.map(data, (x) -> new ReminderPhoneBook(x))
    window.model.is_ready(true)