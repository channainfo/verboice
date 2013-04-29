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
        @contact = Contact.new(params[:contact])
        @contact.project = @project
        if @contact.save
          render json: @contact
        else
          render json: errors_to_json(@contact, 'creating')
        end
      end


      # POST /projects/:project_id/contact
      def register_addresses
        @project = Project.find params[:project_id]
        import = {}
        import["created"] = []
        import["failed"] = []
        list_contact = params[:list_contact]
        list_contact.map do |address|
          contact = Contact.new(:address => address)
          contact.project = @project
          if contact.save
            import["created"].push(contact)
          else
            import["failed"].push(contact)
          end
        end
        render json: import
      end
    end
  end
end