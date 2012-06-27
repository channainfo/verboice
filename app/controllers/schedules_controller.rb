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
  expose(:schedules) { project.schedules }
  expose(:schedule)

  def create
    if schedule.save
      redirect_to project_schedules_path(project), :notice => "Schedule #{schedule.name} successfully created."
    else
      render :action => "new"
    end
  end

  def update
    if schedule.save
      redirect_to project_schedules_path(project), :notice => "Schedule #{schedule.name} successfully updated."
    else
      render :action => "edit"
    end
  end

  def destroy
    schedule.destroy
    redirect_to project_schedules_path(project), :notice => "Schedule #{schedule.name} successfully deleted."
  end
end