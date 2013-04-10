onReminderPhoneBooks ->
	class @ReminderPhoneBookMainViewModel
		constructor: (project_id) ->
      @project_id = ko.observable project_id
      @reminder_phone_books = ko.observableArray []
      @contacts = ko.observableArray []

      @is_ready = ko.observable false
      @current_reminder_phone_book = ko.observable()
      @saving_reminder_phone_book = ko.observable(false)

    new_reminder_phone_book: =>
      reminder_phone_book = new ReminderPhoneBook
      @reminder_phone_books.push(reminder_phone_book)
      @current_reminder_phone_book(reminder_phone_book)
      reminder_phone_book.hasFocus(true)

    edit_reminder_phone_book: (reminder_phone_book) =>
      if @current_reminder_phone_book() then @reminder_phone_books.remove(@current_reminder_phone_book()) unless @current_reminder_phone_book().id()
      @current_reminder_phone_book(reminder_phone_book)
      reminder_phone_book.hasFocus(true)

    cancel_reminder_phone_book: =>
      @reminder_phone_books.remove(@current_reminder_phone_book()) unless @current_reminder_phone_book().id()
      @current_reminder_phone_book(null)

    save_reminder_phone_book: =>
      @saving_reminder_phone_book(true)
      json = {ext_reminder_phone_book: @current_reminder_phone_book().toJSON()}
      if @current_reminder_phone_book().id()
        json._method = 'put'
        $.post "/ext/projects/#{@project_id()}/reminder_phone_books/#{@current_reminder_phone_book().id()}.json", json, @save_reminder_phone_bookCallback
      else
        $.post "/ext/projects/#{@project_id()}/reminder_phone_books.json", json, @save_reminder_phone_bookCallback

    save_reminder_phone_bookCallback: (data) =>
      #if reminder phone book is new, we need to set id
      $.status.showNotice("Reminder phone book successfully #{if @current_reminder_phone_book().id() then 'saved' else 'created'}", 2000)
      @current_reminder_phone_book().id(data.id)

      @current_reminder_phone_book(null)
      @saving_reminder_phone_book(false)

    delete_reminder_phone_book: (reminder_phone_book) =>
      if confirm("Are you sure you want to delete this reminder phone book?")
        $.post "/ext/projects/#{@project_id()}/reminder_phone_books/#{reminder_phone_book.id()}.json", {_method: 'delete'}, =>
          @reminder_phone_books.remove(reminder_phone_book)
          $.status.showNotice("Reminder phone book successfully deleted", 2000)

    find_contact: (address) =>
      return contact for contact in @contacts() when contact.address() == address