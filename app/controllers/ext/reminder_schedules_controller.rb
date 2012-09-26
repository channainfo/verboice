module Ext 
	class ReminderSchedulesController < ApplicationController
		def index
			load_project
		end

		def new
			load_project
			@reminder = ReminderSchedule.new
		end

		def load_project
			@project = Project.find params[:project_id]

		end

	end
end