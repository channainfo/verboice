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

class Channels::Sip < Channel

  validate :validate_domain
  validate :server_username_uniqueness
  validate :server_number_uniqueness

  config_accessor :username
  config_accessor :password
  config_accessor :domain
  config_accessor :port
  config_accessor :protocol
  config_accessor :direction
  config_accessor :register
  config_accessor :number

  attr_accessor :ip_address

  def register?
    register == true || register == "1"
  end

  def outbound?
    direction == 'outbound' || direction == 'both'
  end

  def inbound?
    direction == 'inbound' || direction == 'both'
  end

  def asterisk_address_string_for broker, address
    broker.sip_address_string_for self, address
  end

  def validate_domain
    ip_address = self.domain
    if domain_valid? self.domain
      ip_address = Resolv.getaddress self.domain
    else
      errors.add(:base, I18n.t("activerecord.errors.models.channel.domain_not_found"))
    end unless Rails.env.test?
  end

  def server_username_uniqueness
    return unless self.username.present?
    
    conflicting_channels = Channels::CustomSip
    conflicting_channels = conflicting_channels.where('id != ?', id) if id
    conflicting_channels = conflicting_channels.all.any? { |c| c.username == self.username && domain_valid?(c.domain) && Resolv.getaddress(c.domain) == ip_address }
    errors.add(:base, 'Username and domain have already been taken') if conflicting_channels
  end

  def server_number_uniqueness
    return unless self.number.present?
    
    conflicting_channels = Channels::CustomSip
    conflicting_channels = conflicting_channels.where('id != ?', id) if id
    conflicting_channels = conflicting_channels.all.any? { |c| c.number == self.number && domain_valid?(c.domain) && Resolv.getaddress(c.domain) == ip_address }
    errors.add(:base, 'Number and domain have already been taken') if conflicting_channels
  end

  def domain_valid? domain
    is_domain_valid = true
    begin
      Resolv.getaddress domain
    rescue
      is_domain_valid = false
    end
    is_domain_valid
  end

  def errors_count
    status = BrokerClient.channel_status(id)[id]
    status && !status[:ok] ? status[:messages].length : 0
  end
end