module Ext 
	class ReminderSchedulesController < ExtApplicationController
		def index
			load_project params[:project_id]
		end

		def new
			load_project params[:project_id]
			@reminder = ReminderSchedule.new
		end

		def create
			load_project params[:project_id]
			@reminder = @project.ext_reminder_schedules.build(params[:ext_reminder_schedule])
			if(@reminder.save)
				flash[:notice] = "Reminder has been save successfully"
				redirect_to :action => :index
			else
				flash[:error] = "Reminder failed to save"
				render :new
			end
		end

		def edit
			load_project params[:project_id]
			@reminder = @project.ext_reminder_schedules.find params[:id]
		end

	end
end