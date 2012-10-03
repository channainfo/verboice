module Ext
  module Services
    class PrenanciesController < ApplicationController
      REMINDER_OPTIONS = {:enable => "1", :disable => "2"}

      expose(:call_log) { CallLog.find params[:CallSid] }
      expose(:project) { call_log.project }

      def manifest
        render :file => File.join(Rails.root, "samples", "external_services", "prenancy", "manifest.xml"), :content_type => 'application/xml'
      end

      def register
        result = "You can register our reminder system later"
        caller_phone_number = params[:From] if params[:From].present?
        if params[:status].present? && params[:status] == REMINDER_OPTIONS[:enable]
          reminder_phone_book = Ext::ReminderPhoneBook.new :project => project, :name => "Reminder Schedule", :phone_number => caller_phone_number
          reminder_phone_book.save unless reminder_phone_book.nil?
          result = "Welcome to reminder system"
        elsif params[:status] == REMINDER_OPTIONS[:disable]
          reminder_phone_book = Ext::ReminderPhoneBook.where(:phone_number => caller_phone_number).first
          reminder_phone_book.destroy unless reminder_phone_book.nil?
        end
        render :xml => "<Response><Say>#{result}</Say></Response>", :content_type => "application/xml"
      end
    end
  end
end