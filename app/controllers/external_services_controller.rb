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

class ExternalServicesController < ApplicationController

  before_filter :authenticate_account!

  respond_to :html, :json

  expose(:project) { current_account.projects.find(params[:project_id]) }
  expose(:external_services) { project.external_services }
  expose(:external_service)

  def create
    external_service.save
    if request.xhr?
      render :partial => "box_content", :locals => { :external_service => external_service, :expanded => external_service.errors.any?}
    else
      render :action => "index"
    end
  end

  def update
    external_service.save
    if request.xhr?
      render :partial => "box_content", :locals => { :external_service => external_service, :expanded => external_service.errors.any?}
    else
      render :action => "index"
    end
  end

  def destroy
    external_service.clean_call_flows
    external_service.destroy
    render :action => "index"
  end

  def update_manifest
    begin
      external_service.update_manifest!
      flash[:notice] = I18n.t("controllers.external_services_controller.manifest_successfully_update")
    rescue Exception => ex
      flash[:error] = I18n.t("controllers.external_services_controller.error_updating_manifest")
      logger.warn ex
    end
    if request.xhr?
      render :partial => "box_content", :locals => { :external_service => external_service, :expanded => true}
    else
      render :action => "index"
    end
  end
end
