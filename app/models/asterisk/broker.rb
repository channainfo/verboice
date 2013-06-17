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

module Asterisk
  class Broker < BaseBroker
    def self.instance
      $asterisk_broker ||= new
    end

    def initialize
      @asterisk_channels = {}
      handle_events
    end

    def start
      EM::connect 'localhost', Asterisk::Client::Port, Asterisk::Client
      EM::start_server 'localhost', Asterisk::CallManager::Port, Asterisk::CallManager

      EM.add_periodic_timer(30) do
        Fiber.new { check_channels_status }.resume
      end

      EM.add_periodic_timer(30) do
        Fiber.new { check_sessions_status }.resume
      end
    end

    def call(session)
      check_asterisk_available!

      address = get_dial_address session.channel, session.address

      result = $asterisk_client.originate({
        :channel => address,
        :application => 'AGI',
        :data => "agi://localhost:#{Asterisk::CallManager::Port},#{session.id}",
        :timeout => 60000,
        :async => true,
        :actionid => session.id,
        :variable => "verboice_session_id=#{session.id}"
      })

      result[:response] == 'Error' ? raise(result[:message]) : nil
    end

    def restart(session)
      $asterisk_client.redirect({
        :channel => session.pbx['channel'],
        :context => 'verboice-restart',
        :exten => session.id,
        :priority => 1
      })
    end

    def create_channel(channel_id)
      update_channel(channel_id)
    end

    def update_channel(channel_id)
      check_asterisk_available!
      trigger_regenerate_config
      @channel_status ||= {}
      @channel_status[channel_id] = {ok: false, messages: ["Connecting..."]}
    end

    def destroy_channel(channel_id)
      check_asterisk_available!
      trigger_regenerate_config
    end

    def get_dial_address(channel, address)
      channel.asterisk_address_string_for self, address
    end

    def sip_address_string_for channel, address
      raise "There is no available outbound server" unless channel.outbound?
      "SIP/verboice_#{channel.id}-outbound/#{address}"
    end

    def custom_address_string_for channel, address
      channel.dial_string.gsub '{number}', address
    end

    def channels
      Channel.where("type != '#{Channels::Voxeo.name}' && type != '#{Channels::Twilio.name}'")
    end

    def channel_status(*channel_ids)
      response_status = {}
      if @channel_status
        channel_ids.each do |channel_id|
          channel_status = @channel_status[channel_id]
          response_status[channel_id] = channel_status if channel_status
        end
      end
      response_status
    end

    def find_channel(pbx)
      channel_id = @channel_registry[[pbx.peer_ip, pbx.number]]
      Channel.find(channel_id ? channel_id : pbx.channel_id)
    end

    private

    def reload!
      $asterisk_client.command :command => 'sip reload'
    end

    def trigger_regenerate_config
      @must_regenerate_config = true

      unless @regenerating_config
        Thread.new do
          while @must_regenerate_config
            @must_regenerate_config = false
            @regenerating_config = true
            begin
              regenerate_config
            rescue Exception => ex
              puts ex.message
            ensure
              @regenerating_config = false
            end
          end
        end
      end
    end

    def regenerate_config
      new_channel_registry = {}
      domains_cache = {}
      File.open("#{Asterisk::ConfigDir}/sip_verboice_registrations.conf", 'w') do |f_reg|
        File.open("#{Asterisk::ConfigDir}/sip_verboice_channels.conf", 'w') do |f_channels|
          Channels::Sip.all.each do |channel|
            section = "verboice_#{channel.id}"

            f_channels.puts "[#{section}](!)"
            f_channels.puts "type=peer"
            f_channels.puts "canreinvite=no"
            f_channels.puts "nat=yes"
            f_channels.puts "qualify=yes"
            if channel.username.present? && channel.password.present?
              f_channels.puts "fromuser=#{channel.username}"
              f_channels.puts "defaultuser=#{channel.username}"
              f_channels.puts "secret=#{channel.password}"
            end
            f_channels.puts "insecure=invite,port"
            f_channels.puts "context=verboice"
            f_channels.puts

            if channel.outbound?
              f_channels.puts "[#{section}-outbound](#{section})"
              f_channels.puts "host=#{channel.domain}"
              f_channels.puts "domain=#{channel.domain}"
              f_channels.puts "fromdomain=#{channel.domain}"
              f_channels.puts "type=peer"
              f_channels.puts
            end

            expand_domain(channel.domain, domains_cache).each_with_index do |server, i|
              server[:ips].each do |ip|
                new_channel_registry[[ip, channel.number]] = channel.id
              end

              f_channels.puts "[#{section}-inbound-#{i}](#{section})"
              f_channels.puts "host=#{server[:host]}"
              f_channels.puts "port=#{server[:port]}" if server[:port]
              f_channels.puts "domain=#{server[:host]}"
              f_channels.puts "fromdomain=#{server[:host]}"
              f_channels.puts "type=user"
              f_channels.puts
            end

            if channel.register?
              f_reg.puts "register => #{channel.username}:#{channel.password}@#{channel.domain}/#{channel.number}"
            end
          end
        end
      end
      EM.schedule do
        Fiber.new { reload! }.resume
      end
      @channel_registry = new_channel_registry
    end

    def expand_domain(domain, cache = {})
      return [] unless domain

      if servers = cache[domain]
        return servers
      end

      dns = Resolv::DNS.new

      servers = []
      resources = dns.getresources "_sip._udp.#{domain}", Resolv::DNS::Resource::IN::SRV
      if resources.empty?
        ips = dns.getaddresses(domain).map(&:to_s)
        ips = [domain] if ips.empty?
        servers << {host: domain, ips: ips}
      else
        resources.each do |resource|
          ips = dns.getaddresses(resource.target).map(&:to_s)
          servers << {host: resource.target.to_s, ips: ips, port: resource.port}
        end
      end
      cache[domain] = servers

      servers
    end

    def pbx_available?
      $asterisk_client && !$asterisk_client.error?
    end

    def check_asterisk_available!
      raise PbxUnavailableException.new("Asterisk is not available") unless pbx_available?
    end

    def check_channels_status
      return unless pbx_available?
      return if @checking_channel_status

      @checking_channel_status = true

      @channel_status_cache = {}
      @new_channel_status = {}
      Channels::Sip.all.each do |channel|
        if channel.register?
          @channel_status_cache[[channel.username, channel.domain]] = channel.id
        end
      end

      $asterisk_client.sipshowregistry
    end

    def on_registry_entry(event)
      channel_id = @channel_status_cache[[event[:username], event[:host]]]
      if channel_id
        @new_channel_status[channel_id] ||= { ok: true, messages: [] }
        if event[:state] != 'Registered'
          @new_channel_status[channel_id][:ok] = false
          @new_channel_status[channel_id][:messages] << "Host #{event[:host]}, status: #{event[:state]}"
        end
      end
    end

    def on_registrations_complete
      $asterisk_client.sippeers
    end

    def on_peer_entry(event)
      if event[:channeltype] == 'SIP' && event[:objectname] =~ /verboice_(\d+)-.*/
        channel_id = $1.to_i
        @new_channel_status[channel_id] ||= { ok: true, messages: [] }
        unless event[:status].start_with?('OK')
          @new_channel_status[channel_id][:ok] = false
          @new_channel_status[channel_id][:messages] << "Host #{event[:ipaddress]}, status: #{event[:status]}"
        end
      end
    end

    def on_peer_list_complete
      @channel_status = @new_channel_status
      @checking_channel_status = false
    end

    def check_sessions_status
      return unless pbx_available?
      return if @checking_sessions_status

      @checking_sessions_status = true
      @active_session_channels = []
      $asterisk_client.status
    end

    def on_status(event)
      @active_session_channels << event[:channel]
    end

    def on_status_complete
      sessions.each do |id, session|
        if session.pbx
          finish_session(session) unless @active_session_channels.include?(session.pbx['channel'])
        else
          finish_session(session) if (Time.now - session.created_at) > 120
        end
      end
      @checking_sessions_status = false
    end

    def on_new_channel(event)
      channel = @asterisk_channels[event[:channel]] ||= {id: Guid.new.to_s, created_at: Time.now}
      PbxLog.create! guid: channel[:id], details: "New SIP channel: #{event[:channel]}"
    end

    def on_var_set(event)
      channel = @asterisk_channels[event[:channel]]
      if channel
        if event[:variable] == "verboice_session_id"
          associate_pbx_log event[:value], channel[:id]
        elsif event[:variable].start_with?("~HASH~SIP_CAUSE~SIP")
          PbxLog.create! guid: channel[:id], details: event[:value]
        else
          PbxLog.create! guid: channel[:id], details: "#{event[:variable]} = #{event[:value]}"
        end
      end
    end

    def on_hangup(event)
      channel = @asterisk_channels[event[:channel]]
      if channel
        PbxLog.create! guid: channel[:id], details: "Channel hangup: #{event[:channel]}, Reson: #{event[:'cause-txt']}"
        @asterisk_channels.delete event[:channel]
      end
    end

    def handle_events
      Asterisk::Client.on_connect do
        @checking_sessions_status = false
        trigger_regenerate_config
        wake_up_queued_calls
      end

      Asterisk::Client.on_event do |event|
        begin
          case event[:event]
          when 'OriginateResponse'
            if event[:response] == 'Failure'
              reason = case event[:reason]
                       when '3' then :no_answer
                       when '5' then :busy
                       else :failed
                       end
              call_rejected event[:actionid], reason
            end
          when 'RegistryEntry'
            on_registry_entry event
          when 'RegistrationsComplete'
            on_registrations_complete
          when 'Registry'
            check_channels_status
          when 'PeerEntry'
            on_peer_entry event
          when 'PeerlistComplete'
            on_peer_list_complete
          when 'Status'
            on_status event
          when 'StatusComplete'
            on_status_complete
          when 'Newchannel'
            on_new_channel event
          when 'VarSet'
            on_var_set event
          when 'Hangup'
            on_hangup event
          end
        rescue Exception => ex
          puts ex.message
          puts ex.backtrace
        end
      end
    end
  end
end
