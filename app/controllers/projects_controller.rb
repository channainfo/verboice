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

class ProjectsController < ApplicationController
  before_filter :authenticate_account!
  before_filter :load_project, only: [:edit, :update, :destroy, :update_variables]
  before_filter :load_enqueue_call_fields, only: [:show, :enqueue_call]

  

  def index
    @projects = current_account.projects.all
  end

  def show
  end

  def new
    @project = Project.new
  end

  def edit
  end

  def create
    @project = Project.new(params[:project])
    @project.account = current_account

    if @project.save
      redirect_to(project_call_flows_path(@project), :notice => I18n.t("controllers.projects_controller.project_successfully_created", :project_name => @project.name))
    else
      render :action => "new"
    end
  end

  def update
    if @project.update_attributes(params[:project])
      redirect_to(project_path(@project), :notice => I18n.t("controllers.projects_controller.project_successfully_updated", :project_name => @project.name))
    else
      render :action => "edit"
    end
  end

  def enqueue_call
    redirect_to project_path(params[:id]), flash: {error: I18n.t("controllers.projects_controller.you_need_to_select_call_flow")} and return unless params[:call_flow_id].present?

    @channel = current_account.channels.find_by_id(params[:channel_id])
    redirect_to project_path(params[:id]), flash: {error: I18n.t("controllers.projects_controller.you_need_to_select_channel")} and return unless @channel

    addresses = params[:addresses].split(/\n/).map(&:strip).select(&:presence)

    options = {}
    options[:schedule_id] = params[:schedule_id] if params[:schedule_id].present?
    options[:not_before] = "#{params[:not_before_date]} #{params[:not_before_time]}" if params[:not_before_date].present? && params[:not_before].present?
    options[:time_zone] = params[:time_zone] if params[:time_zone].present?
    options[:call_flow_id] = params[:call_flow_id] if params[:call_flow_id].present?
    options[:project_id] = params[:id]
    options[:vars] = params[:vars]

    DateTime.parse(options[:not_before]) rescue redirect_to project_path(params[:id]), flash: {error: I18n.t("controllers.projects_controller.enter_valid_date")} and return if options[:not_before]

    addresses = curated_addresses(addresses)
    addresses.each do |address|
      @channel.call(address.strip, options)
    end

    redirect_to project_path(params[:id]), {:notice => I18n.t("controllers.projects_controller.enqueued_call_to_on_channel", :pluralize => pluralize(addresses.count, 'address'), :channel_name => @channel.name)}
  end

  def destroy
    @project.destroy
    redirect_to(projects_url, :notice => I18n.t("controllers.projects_controller.project_successfully_deleted", :project_name => @project.name))
  end

  def update_variables
    if @project.update_attributes(params[:project])
      redirect_to project_contacts_path(@project), notice: I18n.t("controllers.projects_controller.columns_successfully_updated")
    else
      redirect_to project_contacts_path(@project), flash: { error: I18n.t("controllers.projects_controller.error_updating_columns")}
    end
  end

  private

  def load_project
    @project = current_account.projects.find(params[:id])
  end

  def load_enqueue_call_fields
    @channels = current_account.channels
    @project = current_account.projects.includes(:call_flows).find(params[:id])
    @schedules = @project.schedules.enabled
    @call_flows = @project.call_flows.includes(:channels).includes(:queued_calls)
    @project_channels = @call_flows.collect(&:channels).flatten.to_set
    @queued_calls = @call_flows.collect(&:queued_calls).flatten
    @call_logs = @project.call_logs
  end

  def curated_addresses(addresses)
    # build a hash from contact_id to all his addresses 
    # eg. { 1 => ['123','456'], 2 => ['789'] }
    all_contacts = Hash.new { |hash,key| hash[key] = [] }
    all_contacts = @project.contact_addresses.order(:id).inject(all_contacts) do |contacts, contact_address|
      contacts[contact_address.contact_id] << contact_address.address
      contacts
    end
    # now build a hash from every contact's addresses to all his addresses
    # eg. { '123' => ['123', '456'], '456' => ['123', '456'], '789' => ['789'] }
    all_addresses = all_contacts.values.inject({}) do |all_addresses, contact_addresses|
      contact_addresses.each do |contact_address|
        all_addresses[contact_address] = contact_addresses
      end
      all_addresses
    end

    # curate the addresses so that there is only one entry for each contact in
    # the original list, and that entry is the contact's first registered address
    known_addresses = Set.new
    addresses.inject([]) do |curated, address|
      unless known_addresses.include?(address)
        contact_addresses = all_addresses[address]
        if contact_addresses.nil?
          known_addresses << address
          curated << address
        else
          known_addresses.merge(contact_addresses)
          curated << contact_addresses.first
        end
      end
      curated
    end
  end
end
