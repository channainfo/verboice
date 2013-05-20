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
        render json: @project.contacts
      end

      # POST /projects/:project_id/contact
      def create
        if params[:addresses].nil?
          render json: "Addresses is missing".to_json, status: :bad_request
          return
        else
          unless params[:addresses].kind_of?(Array)
            render json: "Addresses was supposed to be a Array, but was a String".to_json, status: :bad_request
            return
          end
        end

        import = { "success" => [], "existing" => [], "project_id" => @project.id }
        params[:addresses].map do |address|
          contact = @project.contacts.build(:address => address)
          if contact.save
            import["success"].push(address.to_s)
          else
            import["existing"].push(address.to_s)
          end
        end
        render json: import
      end

      # DELETE /projects/:project_id/contacts/unregistration
      def unregistration
        if params[:addresses].nil?
          render json: "Addresses is missing".to_json, status: :bad_request
          return
        else
          unless params[:addresses].kind_of?(Array)
            render json: "Addresses was supposed to be a Array, but was a String".to_json, status: :bad_request
            return
          end
        end
        
        result = { "success" => [], "non-existing" => [], "project_id" => @project.id }
        params[:addresses].each do |address|
          contact = @project.contacts.where(address: address).first
          if contact.destroy
            result['success'].push address.to_s
          else
            result['non-existing'].push address.to_s
          end if contact
          result['non-existing'].push address.to_s if contact.nil?
        end
        render json: result
      end

      private

      def validate_project
        begin
          @project = current_account.projects.find(params[:project_id])
        rescue
          begin
            project_another_account = Project.find params[:project_id]
            render json: "The project is not under your account".to_json, status: :unauthorized
            return
          rescue
            render json: "The project is not found".to_json, status: :not_found
            return
          end
          render json: "The project is not found".to_json, status: :not_found
          return
        end
      end
    end
  end
end