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
      before_filter :validate_project

      def index
        if @project
          @contacts = @project.contacts
          render json: @contacts
        else
          render :json => errors_to_json(@project, 'listing')
        end
      end

      # POST /projects/:project_id/contact
      def create
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

      # DELETE /projects/:project_id/contacts/unregistration
      def unregistration
        result = { "success" => [], "non-existing" => [], "project_id" => @project.id }
        params[:addresses].each do |address|
          contact = @project.contacts.where(address: address).first
          if contact.destroy
            result['success'].push address.to_s
          else
            result['non-existing'].push address.to_s
          end if contact
          result['non-existing'].push address.to_s if contact.nil?
        end if params[:addresses].present?
        render json: result
      end

      private

      def validate_project
        begin
          @project = current_account.projects.find(params[:project_id])
        rescue
          render json: "The project is not found", status: :not_found
          return
        end
      end
    end
  end
end