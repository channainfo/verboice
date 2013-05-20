module Ext
	class ExtApplicationController < ApplicationController
		def load_project project_id
      @project_id = project_id
			@project = Project.find project_id
		end
	end
end