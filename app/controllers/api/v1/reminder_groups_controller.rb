# Copyright (C) 2010-2012, InSTEDD
#
# This file is part of Verboice.
#
# Verboice is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Verboice is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Verboice.  If not, see <http://www.gnu.org/licenses/>.
module Api
  module V1
    class ReminderGroupsController < ApiController
      before_filter :validate, only: [:update, :destroy]
      expose(:project) { current_account.projects.find(params[:project_id]) }
      expose(:reminder_groups) { project.ext_reminder_groups }

      # GET /api/projects/:project_id/reminder_groups
      def index
        render json: reminder_groups
      end

      # POST /api/projects/:project_id/reminder_groups
      def create
        begin
          params[:reminder_group][:addresses] = params[:reminder_group][:addresses].map(&:to_s).uniq if params[:reminder_group] && params[:reminder_group][:addresses].present? && params[:reminder_group][:addresses].kind_of?(Array)
          new_reminder_group = reminder_groups.build params[:reminder_group]
          if new_reminder_group.save
            render json: new_reminder_group, status: :created
          else
            render json: errors_to_json(new_reminder_group, 'creating'), status: :bad_request
          end
        rescue Exception => ex
          render json: {error: true, error_message: ex.message}, status: :bad_request
        end
      end

      # PUT /api/projects/:project_id/reminder_groups/:id
      def update
        params[:reminder_group][:addresses] = params[:reminder_group][:addresses].map(&:to_s).uniq if params[:reminder_group] && params[:reminder_group][:addresses].present? && params[:reminder_group][:addresses].kind_of?(Array)
        if @reminder_group.update_attributes(params[:reminder_group])
          render json: @reminder_group
        else
          render json: errors_to_json(@reminder_group, 'updating'), status: :bad_request
        end
      end

      # DELETE /api/reminder_groups/:id
      def destroy
        if @reminder_group.destroy
          render json: @reminder_group
        else
          render json: errors_to_json(@reminder_group, 'deleting'), status: :bad_request
        end
      end

      private

      def validate
        begin
          @reminder_group = reminder_groups.find(params[:id])
        rescue
          render json: "The reminder group is not found", status: :not_found
          return
        end
      end

    end
  end
end