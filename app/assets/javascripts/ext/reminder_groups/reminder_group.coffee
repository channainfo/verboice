onReminderGroups ->
	class @ReminderGroup
		constructor: (data) ->
			@id = ko.observable data?.id
			@name = ko.observable data?.name
			@contacts = ko.observableArray if data?.addresses then $.map(data.addresses, (x) -> window.model.find_contact x) else []
			@has_contacts = ko.computed =>
				if @contacts().length > 0 then true else false
			@new_address = ko.observable null

			@name_error = ko.computed => if @has_name() then null else "Name is required"
			@new_address_error = ko.computed => if @has_new_address() then null else "new address is required"
			@current_contact = ko.observable null
			@saving_contact = ko.observable false

			@hasFocus = ko.observable(false)

			@error = ko.computed => 
				@name_error()
			@valid = ko.computed => 
				!@error()

		has_name: => $.trim(@name()).length > 0
		has_new_address: => $.trim(@new_address()).length > 0

		add_new_contact: =>
			if @new_address
				@current_contact(window.model.find_contact @new_address())
				@current_contact(new Contact({address: @new_address()})) unless @current_contact()
				if @current_contact().id()
					@contacts.push @current_contact()
					@current_contact(null)
				else
					@save_contact() unless @current_contact().id()

				# remove existing address from autocomplete filter list items
				autocomplete_list_items = $("#autocomplete-address").autocomplete("option", "source")
				selected_index = autocomplete_list_items.indexOf(@new_address())
				autocomplete_list_items.splice(selected_index, 1)
				@new_address(null)

		remove_contact: (contact) =>
			@contacts.remove(contact)
			# add contact address into autocomplete filter list items
			autocomplete_list_items = $("#autocomplete-address").autocomplete("option", "source")
			autocomplete_list_items.push(contact.address())

		save_contact: =>
			@saving_contact(true)
			json = {contact: @current_contact().toJSON()}
			$.post "/projects/#{window.model.project_id()}/contacts.json", json, @save_contact_callback

		save_contact_callback: (data) =>
			$.status.showNotice("Contact successfully created", 2000)
			@current_contact().id(data.id)
			@contacts.push @current_contact()
			@current_contact(null)
			@saving_contact(false)

		toJSON: =>
			name: @name()
			addresses: $.map(@contacts(), (x) -> x.address() if x.valid())
