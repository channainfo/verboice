module Ext
 class ReminderPhoneBooksController  < ExtApplicationController

 	def index
 		load_project params[:project_id]
 	end


 	def new
 		load_project params[:project_id]
 		@reminder = Ext::ReminderPhoneBook.new

 	end

 	def create
 		load_project params[:project_id]
 		@reminder = @project.ext_reminder_phone_books.build(params[:ext_reminder_phone_book])
 		if 	@reminder.save
 			flash[:notice] = "Successfully created"
 			redirect_to :action => "index"
 		else		
 			render :new
 		end
 	end

 	def edit
 		load_project params[:project_id]
 		begin
 			@reminder = @project.ext_reminder_phone_books.find(params[:id])
 		rescue
 			flash[:error] = "Invalide record"
 			redirect_to :action => :index
 		end	

 	end

 	def update
 		load_project params[:project_id]
 		begin
 			@reminder = @project.ext_reminder_phone_books.find(params[:id])
	 		if @reminder.update_attributes(params[:ext_reminder_phone_book])
	 			flash[:notice] = "Successfully updated"
	 			redirect_to :action => :index 
	 		else		
	 			render :edit
 			end
 		rescue Exception => e
 			flash[:error] = e.message
 			redirect_to :action => :index
 		end
 	end

 	def update_reminder_phone_book_types
 		load_project params[:project_id]
    if @project.update_attributes(params[:project])
      redirect_to ext_project_reminder_phone_books_path(@project), notice: I18n.t("controllers.projects_controller.reminder_phone_book_types_successfully_updated")
    else
      redirect_to ext_project_reminder_phone_books_path(@project), flash: { error: I18n.t("controllers.projects_controller.error_updating_reminder_phone_book_types")}
    end
  end

 	def destroy
 		load_project params[:project_id]
 		begin
 			@reminder = @project.ext_reminder_phone_books.find(params[:id])
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