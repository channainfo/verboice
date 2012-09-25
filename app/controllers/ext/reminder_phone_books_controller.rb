module Ext
 class ReminderPhoneBooksController  < ApplicationController


 	def index
 		load_project		
 	end


 	def new
 		load_project
 		@reminder = Ext::ReminderPhoneBook.new

 	end

 	def load_project
 		@project = Project.find(params[:project_id])
 	end

 	def create
 		load_project
 		@reminder = @project.reminder_phone_books.build(params[:ext_reminder_phone_book])
 		if 	@reminder.save
 			flash[:notice] = "Successfully created"
 			redirect_to :action => "index"
 		else		
 			render :new
 		end
 	end

 	def edit
 		load_project
 		begin
 			@reminder = @project.reminder_phone_books.find(params[:id])
 		rescue
 			flash[:error] = "Invalide record"
 			redirect_to :action => :index
 		end	

 	end

 	def update
 		load_project
 		begin
 			@reminder = @project.reminder_phone_books.find(params[:id])
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

 	def destroy
 		load_project
 		begin
 			@reminder = @project.reminder_phone_books.find(params[:id])
	 		if @reminder.delete
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