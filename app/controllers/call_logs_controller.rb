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

class CallLogsController < ApplicationController
  before_filter :authenticate_account!
  before_filter :paginate, only: [:index, :queued]

  helper_method :paginate

  def index
    @search = params[:search]
    @logs = current_account.call_logs.includes(:project).includes(:channel).includes(:call_log_answers).order('id DESC')
    @project = current_account.projects.find(params[:project_id]) if params[:project_id].present?
    @logs = @logs.where(:project_id => @project.id) if @project
    @logs = @logs.where call_flow_id: params[:call_flow_id] if params[:call_flow_id].present?
    @logs = @logs.search @search, :account => current_account if @search.present?
    @logs = @logs.paginate :page => @page, :per_page => @per_page
    render :template => "projects/call_logs/index" if @project
  end

  def show
    @log = current_account.call_logs.find params[:id]
  end

  def progress
    @log = current_account.call_logs.find params[:id]
    render :layout => false
  end

  def queued
    @calls = current_account.queued_calls.includes(:channel).includes(:call_log).includes(:schedule).order('id DESC')
    @calls = @calls.paginate :page => @page, :per_page => @per_page
  end

  def play_result
    @log = current_account.call_logs.find params[:id]
    send_file RecordingManager.for(@log).result_path_for(params[:key]), :x_sendfile => true, :content_type => "audio/x-wav"
  end

  def download
    @filename = "Call_logs_(#{Time.now.to_s.gsub(' ', '_')}).csv"
    @streaming = true
    @csv_options = { :col_sep => ',' }
  end

  def download_project_call_log
    @filename = "Project_Call_logs_(#{Time.now.to_s.gsub(' ', '_')}).csv"
    @streaming = true
    @csv_options = { :col_sep => ',' }
    @project = current_account.projects.find(params[:project_id]) if params[:project_id].present?
    if params[:call_flow_id].present?
      @call_logs = @project.call_logs.where(:call_flow_id => params[:call_flow_id]).order('id DESC')
    else
      @call_logs = @project.call_logs.order('id DESC')
    end
    render :template => "projects/call_logs/download" if @project
  end

  def download_details
    @log = current_account.call_logs.includes(:entries).find params[:id]
    @filename = "Call details #{@log.id} (#{Time.now}).csv"
    @streaming = true
    @csv_options = { :col_sep => ',' }
  end

  private
    def paginate
      @page = params[:page] || 1
      @per_page = 10
    end
end
