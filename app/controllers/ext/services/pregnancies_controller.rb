module Ext
  module Services
    class PregnanciesController < ApplicationController
      REMINDER_OPTIONS = {:enable => 1, :disable => 2}

      expose(:call_log) { CallLog.find params[:CallSid] }
      expose(:project) { call_log.project }

      def manifest
        render :file => File.join(Rails.root, "samples", "external_services", "pregnancy", "manifest.xml"), :content_type => 'application/xml'
      end

      def progress
        pregnancy_date = (DateType.new(params[:type].to_i).days * params[:duration].to_i).days.ago
        render :text => "Pregnancy's date registered: #{pregnancy_date}"
      end

      def register
        result = "<Response><Say>You can register our reminder system later</Say><Hangup/></Response>"
        caller_phone_number = params[:From] if params[:From].present?
        if params[:status].present? && params[:status].to_i == REMINDER_OPTIONS[:enable]
          reminder_phone_book = Ext::ReminderPhoneBook.new :project => project, :name => "Reminder Schedule", :phone_number => caller_phone_number
          reminder_phone_book.save unless reminder_phone_book.nil?
          result = "<Response><Say>Welcome to reminder system</Say></Response>"
        elsif params[:status].to_i == REMINDER_OPTIONS[:disable]
          reminder_phone_book = Ext::ReminderPhoneBook.where(:phone_number => caller_phone_number).first
          reminder_phone_book.destroy unless reminder_phone_book.nil?
        end
        render :xml => result, :content_type => "application/xml"
      end
    end
  end
end