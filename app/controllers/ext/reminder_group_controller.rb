module Ext
 class ReminderGroupsController  < ExtApplicationController

  def index
    load_project params[:project_id]
    respond_to do |format|
      format.html
      format.json { render json: @project.ext_reminder_groups }
    end
  end

  def create
    load_project params[:project_id]
    @reminder_group = @project.ext_reminder_groups.build(params[:ext_reminder_group])
    if @reminder_group.save
      flash[:notice] = I18n.t("controllers.reminder_phone_books.successfully_created")
      render json: @reminder_group
    else
      flash[:notice] = I18n.t("controllers.reminder_phone_books.create_error")
      render text: I18n.t("controllers.reminder_phone_books.create_error")
    end
  end

  def update
    load_project params[:project_id]
    begin
      @reminder_group = @project.ext_reminder_groups.find(params[:id])
      if @reminder_group.update_attributes(params[:ext_reminder_group])
        flash[:notice] = I18n.t("controllers.reminder_phone_books.successfully_updated")
        render json: @reminder_group
      else
        flash[:notice] = I18n.t("controllers.reminder_phone_books.update_error")
        render text: I18n.t("controllers.reminder_phone_books.update_error")
      end
    rescue Exception => e
      flash[:error] = e.message
    end
  end

  def destroy
    load_project params[:project_id]
    begin
      @reminder_group = @project.ext_reminder_groups.find(params[:id])
      if @reminder_group.destroy
        flash[:notice] = I18n.t("controllers.reminder_phone_books.successfully_deleted", :contact => @reminder_group.name)
        render json: @reminder
      else
        flash[:error] = I18n.t("controllers.reminder_phone_books.delete_error", :contact => @reminder_group.name)
        render text: I18n.t("controllers.reminder_phone_books.delete_error", :contact => @reminder_group.name)
      end
    rescue Exception => e
      flash[:error] = e.message
    end
  end

 end
end