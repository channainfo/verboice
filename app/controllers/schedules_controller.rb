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

class SchedulesController < ApplicationController

  respond_to :html
  expose(:project)
  expose(:schedules) { project.schedules.enabled }
  expose(:schedule)

  def create
    schedule.save
    render :partial => "box_content", :locals => { :schedule => schedule, :expanded => schedule.errors.any?}
  end

  def update
    schedule.save
    render :partial => "box_content", :locals => { :schedule => schedule, :expanded => schedule.errors.any?}
  end

  def destroy
    schedule.destroy
    redirect_to project_schedules_path(project), :notice => I18n.t("controllers.schedules_controller.schedule_succesfully_deleted", :schedule_name => schedule.name)
  end
end
