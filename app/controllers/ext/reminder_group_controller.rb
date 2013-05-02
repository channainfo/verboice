module Ext
 class ReminderGroupsController  < ExtApplicationController

  def index
    load_project params[:project_id]
    respond_to do |format|
      format.html
      format.json { render json: @project.ext_reminder_groups }
    end
  end

  # def create
  #   load_project params[:project_id]
  #   @reminder_group = @project.ext_reminder_groups.build(params[:ext_reminder_group])
  #   if @reminder_group.save
  #     render json: @reminder_group
  #   else
  #     head :bad_request
  #   end
  # end

  # def update
  #   load_project params[:project_id]
  #   begin
  #     @reminder_group = @project.ext_reminder_groups.find(params[:id])
  #     if @reminder_group.update_attributes(params[:ext_reminder_group])
  #       render json: @reminder_group
  #     else
  #       head :bad_request
  #     end
  #   rescue Exception => e
  #     flash[:error] = e.message
  #   end
  # end

  # def destroy
  #   load_project params[:project_id]
  #   begin
  #     @reminder_group = @project.ext_reminder_groups.find(params[:id])
  #     if @reminder_group.destroy
  #       render json: @reminder
  #     else
  #       head :bad_request
  #     end
  #   rescue Exception => e
  #     flash[:error] = e.message
  #   end
  # end

 end
end