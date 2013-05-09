onReminderGroups ->
	class @ReminderGroupMainViewModel
		constructor: (project_id) ->
      @project_id = ko.observable project_id
      @reminder_groups = ko.observableArray []

      @is_ready = ko.observable false
      @current_reminder_group = ko.observable()
      @saving_reminder_group = ko.observable(false)

    new_reminder_group: =>
      reminder_group = new ReminderGroup
      @reminder_groups.push(reminder_group)
      @current_reminder_group(reminder_group)
      reminder_group.hasFocus(true)

    edit_reminder_group: (reminder_group) =>
      if @current_reminder_group() then @reminder_groups.remove(@current_reminder_group()) unless @current_reminder_group().id()
      @current_reminder_group(reminder_group)
      reminder_group.hasFocus(true)

    cancel_reminder_group: =>
      @reminder_groups.remove(@current_reminder_group()) unless @current_reminder_group().id()
      @current_reminder_group(null)

    save_reminder_group: =>
      @saving_reminder_group(true)
      json = {ext_reminder_group: @current_reminder_group().toJSON()}
      if @current_reminder_group().id()
        json._method = 'put'
        $.post "/ext/projects/#{@project_id()}/reminder_groups/#{@current_reminder_group().id()}.json", json, @save_reminder_groupCallback
      else
        $.post "/ext/projects/#{@project_id()}/reminder_groups.json", json, @save_reminder_groupCallback

    save_reminder_groupCallback: (data) =>
      # if reminder group is new, we need to set id
      $.status.showNotice((if @current_reminder_group().id() then update_success else create_success), 2000)
      @current_reminder_group().id(data.id)

      @current_reminder_group(null)
      @saving_reminder_group(false)

    delete_reminder_group: (reminder_group) =>
      if confirm(confirm_delete)
        $.post "/ext/projects/#{@project_id()}/reminder_groups/#{reminder_group.id()}.json", {_method: 'delete'}, =>
          @reminder_groups.remove(reminder_group)
          $.status.showNotice(delete_success, 2000)
