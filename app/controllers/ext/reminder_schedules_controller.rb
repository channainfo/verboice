module Ext 
	class ReminderSchedulesController < ExtApplicationController
		def index
			load_project params[:project_id]
			@reminder_schedules = @project.ext_reminder_schedules
			@channels = current_account.channels
			@call_flows = @project.call_flows
			@phone_book_groups = @project.ext_reminder_phone_book_types
			@variables = @project.project_variables
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

		def update
			begin
				load_project params[:project_id]
				@reminder = @project.ext_reminder_schedules.find(params[:id])
				if(@reminder.update_attributes(params[:ext_reminder_schedule]))
					@reminder.update_queues_call

					flash[:notice] = "Successfuly update reminder"
					redirect_to :action => :index
				else
					flash[:error] = "Update reminder failed"
					render :action => :edit
				end
			rescue Exception => e
				flash[:error] = e.message
				render :action => :index	
			end		
		end

		def destroy
			load_project params[:project_id]
	 		begin
	 			@reminder = @project.ext_reminder_schedules.find(params[:id])

		 		if @reminder.destroy
		 			flash[:notice] = " Record : #{@reminder.name} has been deleted"
		 		else	
		 			flash[:error] = "Failed to delete"
		 		end
	 		rescue Exception => e
	 			flash[:error] = e.message
	 		ensure	
	 			redirect_to :action => :index 
	 		end
		end
	end
end