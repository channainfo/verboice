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
      expose(:project) { current_account.projects.find(params[:project_id]) }
      expose(:reminder_groups) { project.ext_reminder_groups }
      expose(:reminder_group) { project.ext_reminder_groups.find(params[:id])}

      # GET /api/projects/:project_id/reminder_groups
      def index
        render json: reminder_groups
      end

      # POST /api/projects/:project_id/reminder_groups
      def create
        new_reminder_group = reminder_groups.build()
        new_reminder_group.name = params[:name] if params[:name].present?
        new_reminder_group.addresses = params[:addresses] if params[:addresses].present?
        if new_reminder_group.save
          render json: new_reminder_group
        else
          render json: errors_to_json(new_reminder_group, 'creating')
        end
      end

      # POST /api/reminder_groups/:id/register_addresses
      def register_addresses
        reminder_group.addresses = reminder_group.addresses | params[:addresses] if params[:addresses].present?
        if reminder_group.save
          render json: reminder_group
        else
          render json: errors_to_json(reminder_group, 'register contacts')
        end
      end

      # DELETE /api/reminder_groups/:id
      def destroy
        if reminder_group.destroy
          head :ok
        else
          render json: errors_to_json(reminder_group, 'deleting')
        end
      end

    end
  end
end