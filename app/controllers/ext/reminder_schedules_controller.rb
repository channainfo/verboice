module Ext 
	class ReminderSchedulesController < ExtApplicationController
		def index
			load_project params[:project_id]
			@reminder_schedules = @project.ext_reminder_schedules
			respond_to do |format|
				format.html
	      format.json { render json: @reminder_schedules }
	    end
		end

		def create
			load_project params[:project_id]
			conditions = Ext::Condition.build params[:ext_reminder_schedule][:conditions]
			@reminder = @project.ext_reminder_schedules.build(params[:ext_reminder_schedule].merge(:conditions => conditions))
			if(@reminder.save)
				flash[:notice] = "Reminder has been save successfully"
				render json: @reminder
			end
		end

		def update
			begin
				load_project params[:project_id]
				conditions = Ext::Condition.build params[:ext_reminder_schedule][:conditions]
				@reminder = @project.ext_reminder_schedules.find(params[:id])
				if(@reminder.update_attributes(params[:ext_reminder_schedule].merge(:conditions => conditions)))
					@reminder.update_queues_call
					flash[:notice] = "Successfuly update reminder"
					render json: @reminder
				end
			rescue Exception => e
				flash[:error] = e.message
			end		
		end

		def destroy
			load_project params[:project_id]
	 		begin
	 			@reminder = @project.ext_reminder_schedules.find(params[:id])

		 		if @reminder.destroy
		 			flash[:notice] = " Record : #{@reminder.name} has been deleted"
		 			render json: @reminder
		 		end
	 		rescue Exception => e
	 			flash[:error] = e.message
	 		end
		end

		def references_data
			load_project params[:project_id]
			@channels = current_account.channels
			@call_flows = @project.call_flows
			@phone_book_groups = @project.ext_reminder_phone_book_types
			@variables = @project.project_variables
			render json: { project: @project, channels: @channels, call_flows: @call_flows, phone_book_groups: @phone_book_groups, variables: @variables }
		end
	end
end