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
  before_filter :search, only: [:index, :download_project_call_logs, :generate_zip]
  before_filter :check_max_row, only: [:download_project_call_logs]
  before_filter :csv_settings, only: [:download, :download_details, :download_project_call_logs]

  helper_method :paginate

  def index
    @logs = @logs.paginate :page => @page, :per_page => @per_page
    render "projects/call_logs/index" if @project
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
    send_file RecordingManager.for(@log).result_path_for(params[:key]), :type => "audio/x-wav"
  end

  def download_project_call_logs
    render layout: false
  end

  def download_details
    @log = current_account.call_logs.includes(:entries).find params[:id]
  end

  def generate_zip
    Delayed::Job.enqueue Jobs::DownloadCallLogsJob.new current_account.id, @project.id, @search
    render layout: false
  end

  def download_zip
    path = File.join RecordingManager.for(current_account).path_for('downloads'), params[:filename]
    send_file path if File.exists? path
  end

  private
    def search
      @search = params[:search] || ""
      @logs = current_account.call_logs.includes(:project).includes(:channel).includes(:call_flow).order('call_logs.id DESC')
      if params[:project_id].present?
        %w(phone_number after before call_flow_id).each do |key|
          @search << search_by_key(key)
        end
        @project = current_account.projects.find(params[:project_id]) 
        @logs = @logs.includes(project: :project_variables).includes(:call_log_answers).includes(:call_log_recorded_audios)
        @logs = @logs.where(:project_id => @project.id)
      end
      @logs = @logs.search @search, :account => current_account if @search.present?
    end
    
    def search_by_key(key)
      params[key].present? ? " #{key}:\"#{params[key]}\"" : ""
    end

    def check_max_row
      if @logs.count > CallLog::CSV_MAX_ROWS
        flash[:error] = I18n.t("controllers.call_logs_controller.csv_is_too_big",
          max: CallLog::CSV_MAX_ROWS, count: @logs.count)
        redirect_to :back
      end
    end

    def csv_settings
      @filename = "Call_logs_(#{Time.now.to_s.gsub(' ', '_')}).csv"
      @output_encoding = 'UTF-8'
      @streaming = true
      @csv_options = { :col_sep => ',' }
    end

    def paginate
      @page = params[:page] || 1
      @per_page = 10
    end
end
