module Ext
 class ReminderGroupsController  < ExtApplicationController

  def index
    load_project params[:project_id]
    respond_to do |format|
      format.html
      format.json { render json: @project.ext_reminder_groups }
    end
  end

 end
end