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
  class CallsController < ApiController

    def call
      params[:flow] = Parsers::Xml.parse request.body if request.post?
      call_log = current_account.call params
      render :json => {:call_id => call_log.id, :state => call_log.state}
    end

    def redirect
      options = {}
      if request.post?
        options[:flow] = Parsers::Xml.parse request.body
      elsif params[:call_flow_id]
        if not current_account.call_flows.exists? params[:call_flow_id]
          return render :status => 404
        end
        options[:call_flow_id] = params[:call_flow_id]
      elsif params[:project_id]
        if not current_account.projects.exists? params[:project_id]
          return render :status => 404
        end
        options[:project_id] = params[:project_id]
      elsif params[:callback_url]
        options[:callback_url] = params[:callback_url]
      else
        return render :status => 400
      end

      channel = CallLog.find(params[:id]).channel
      BrokerClient.redirect options

      render :text => 'OK'
    end

    def state
      call_log = current_account.call_logs.where(:id => params[:id]).first
      render :json => {:call_id => call_log.id, :state => call_log.state}
    end
  end
end