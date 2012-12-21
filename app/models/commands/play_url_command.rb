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

class Commands::PlayUrlCommand < Command
  include Commands::PlayCommand
  attr_accessor :url

  def initialize(url)
    @url = url
    super Digest::MD5.hexdigest @url
  end

  def run(session)
    session.info "Play #{@url}", command: command_name, action: 'start'
    next_command = super
    session.info "Play #{@url} finished", command: command_name, action: 'finish'
    next_command
  end

  def setup_file(session)
    target_path = get_target_path(session)
    session.trace "Download #{@url}", command: command_name, action: 'download_file'
    download_url_to @url, target_path
    target_path
  end

  def command_name
    'play_url'
  end
end
