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
    class ContactsController < ApiController
      def index
        @project = Project.find(params[:project_id])
        if @project
          @contacts = @project.contacts
          render json: @contacts.to_json
        else
          render :json => errors_to_json(@project, 'listing')
        end
      end

      # POST /projects/:project_id/contact
      def create
        @project = Project.find params[:project_id]
        import = {}
        import["success"] = []
        import["existing"] = []
        list_contact = params[:addresses]
        list_contact.map do |address|
          contact = Contact.new(:address => address)
          contact.project = @project
          if contact.save
            import["success"].push(address)
          else
            import["existing"].push(address)
          end
        end
        import[:project_id] = @project.id
        render json: import

      end

      # POST /projects/:project_id/contacts/unregistration
      def unregistration
        @project = Project.find params[:project_id]
        result = {deleted: [], failed: []}
        params[:addresses].each do |address|
          contact = @project.contacts.where(address: address).first
          if contact.destroy
            result[:deleted].push address.to_s
          else
            result[:failed].push address.to_s
          end if contact
          result[:failed].push address.to_s if contact.nil?
        end if params[:addresses].present?
        render json: result
      end
    end
  end
end