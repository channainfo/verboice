module Ext 
	class PregnancyRemindersController < ExtApplicationController
		def index
			load_project params[:project_id]
		end

		def new
			load_project params[:project_id]
			@reminder = PregnancyReminder.new
		end

		def create
			load_project params[:project_id]
			params[:ext_pregnancy_reminder][:started_at] = Ext::Util.parse_date_time(params[:ext_pregnancy_reminder][:started_at], params[:ext_pregnancy_reminder][:timezone]) if params[:ext_pregnancy_reminder][:started_at].present?
			@reminder = @project.ext_pregnancy_reminders.build(params[:ext_pregnancy_reminder])

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
			@reminder = @project.ext_pregnancy_reminders.find params[:id]
		end

		def update
			begin
				load_project params[:project_id]
				params[:ext_pregnancy_reminder][:started_at] = Ext::Util.parse_date_time(params[:ext_pregnancy_reminder][:started_at], params[:ext_pregnancy_reminder][:timezone]) if params[:ext_pregnancy_reminder][:started_at].present?
				@reminder = @project.ext_pregnancy_reminders.find(params[:id])
				if(@reminder.update_attributes(params[:ext_pregnancy_reminder]))
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
	 			@reminder = @project.ext_pregnancy_reminders.find(params[:id])
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