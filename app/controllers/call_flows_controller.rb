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

require 'csv'

class CallFlowsController < ApplicationController

  before_filter :authenticate_account!
  before_filter :load_call_flow_and_project, :only => [
    :download_results, :edit, :edit_workflow, :update_workflow, :update, :destroy, :play_result, :import, :export, :oauth
  ]
  before_filter :load_all_call_flows, :only => [:index, :update, :create]
  before_filter :load_recording_data, :only => [:play_result]

  def download_results
    @filename = "Call_results_-_#{@call_flow.name}_(#{Time.now.to_s.gsub(' ', '_')}).csv"
    @output_encoding = 'UTF-8'
    @streaming = true
    @csv_options = { :col_sep => ',' }
  end

  def index
    @project = current_account.projects.includes(:call_flows).find(params[:project_id])
  end

  def new
    @call_flow = CallFlow.new
    @call_flow.project = current_account.projects.find(params[:project_id])
  end

  def create
    @project = current_account.projects.includes(:call_flows).find(params[:project_id])
    @call_flow = @project.call_flows.create(params[:call_flow])

    @call_flow.save
    if request.xhr?
      render :partial => "box_content", :locals => { :call_flow => @call_flow, :expanded => (@call_flow.mode_flow? || @call_flow.errors.any?)}
    else
      render :action => "index"
    end
  end

  def destroy
    Channel.update_all({:call_flow_id => nil}, {:call_flow_id => @call_flow.id.to_i})
    @call_flow.destroy
    redirect_to project_call_flows_path(@project)
  end

  def edit
    @variables = @project.defined_variables
  end

  def update
    @call_flow.update_attributes(params[:call_flow])
    if request.xhr?
      render :partial => "box_content", :locals => { :call_flow => @call_flow, :expanded => @call_flow.errors.any? }
    else
      render :action => "index"
    end
  end

  def update_workflow
    @call_flow.user_flow = JSON.parse params[:flow]
    @call_flow.mode= :flow
    if @call_flow.save
        redirect_to edit_workflow_call_flow_path(@call_flow), :notice => I18n.t("controllers.call_flows_controller.call_flow_successfully_updated", :call_flow_name => @call_flow.name)
    else
      render :action => "edit_workflow"
    end
  end

  def edit_workflow
    @variables = @project.defined_variables
    @external_steps = @call_flow.project.external_service_steps.includes(:external_service)
  end

  def import
    if params[:vrb].blank?
      redirect_to({:action => :edit_workflow}, :flash => {:alert => I18n.t("controllers.call_flows_controller.no_file_found")})
    else
      begin
        extension = File.extname params[:vrb].original_filename
        case extension
        when '.vrb'
          yaml = Yaml.regenerate_new_resource_guid!(YAML::load(File.read(params[:vrb].tempfile.path)), @call_flow.project)
          @call_flow.user_flow = yaml
          @call_flow.save!
        when '.vrz', '.zip'
          VrzContainer.for(@call_flow).import params[:vrb].tempfile.path
        else
          raise I18n.t("controllers.call_flows_controller.invalide_extension")
        end
        redirect_to({ :action => :edit_workflow }, {:notice => I18n.t("controllers.call_flows_controller.call_flow_successfully_updated", :call_flow_name => @call_flow.name)})
      rescue Exception => ex
        redirect_to({:action => :edit_workflow}, :flash => {:error => I18n.t("controllers.call_flows_controller.invalide_file", :ex => ex)})
      end
    end
  end

  def export
    if params[:export_audios] || @call_flow.call_flow_external_services.count > 0
      file = Tempfile.new(@call_flow.id.to_s)
      file.chmod 0644 # NOTE: allow other read access for x_sendfile
      begin
        VrzContainer.for(@call_flow, params[:export_audios]).export file.path
      ensure
        file.close
      end
      send_file file.path, :filename => "Call flow #{@call_flow.id}.zip"
    else
      send_data @call_flow.user_flow.to_yaml, :filename => "Call flow #{@call_flow.id}.vrb"
    end
  end

  def oauth
    options = {:call_flow_id => @call_flow.id}
    options[:fusion_table_name] = params[:fusion_table_name] if params[:fusion_table_name].present?
    redirect_to google_oauth_path(:redirect_back_to => project_call_flows_path(@project, options))
  end

  private

  def load_recording_data
    @step_id = params[:step_id]
    @message = params[:message]
  end

  def load_call_flow_and_project
    @call_flow = current_account.call_flows.find(params[:id])
    @project = @call_flow.project
    @reminder_groups = @project.ext_reminder_groups
  end

  def load_all_call_flows
    @project = current_account.projects.includes(:call_flows).find(params[:project_id])
    @call_flows = if @call_flow
      @project.call_flows.reject { |call_flow| call_flow.id == @call_flow.id }.unshift(@call_flow)
    else
      @project.call_flows
    end
  end
end
