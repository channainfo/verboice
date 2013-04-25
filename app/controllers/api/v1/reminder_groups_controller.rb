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
      # GET /projects/:project_id/reminder_groups
      def index
        project = Project.find params[:project_id]
        render json: project.ext_reminder_groups
      end

      # POST /projects/:project_id/reminder_groups
      def create
        project = Project.find params[:project_id]
        reminder_group = project.ext_reminder_groups.build()
        reminder_group.name = params[:name] if params[:name].present?
        reminder_group.addresses = params[:addresses] if params[:addresses].present?
        if reminder_group.save
          render json: reminder_group
        else
          render json: errors_to_json(reminder_group, 'creating')
        end
      end

      # POST /api/reminder_groups/:id/register_addresses
      def register_addresses
        reminder_group = Ext::ReminderGroup.find params[:id]

        reminder_group.addresses = reminder_group.addresses | params[:addresses] if params[:addresses].present?
        if reminder_group.save
          render json: reminder_group
        else
          render json: errors_to_json(reminder_group, 'register contacts')
        end
      end
    end
  end
end