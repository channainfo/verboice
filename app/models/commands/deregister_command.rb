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

class Commands::DeregisterCommand < Command

  def initialize reminder_group, options = {}
    @reminder_group = reminder_group
  end

  def run(session)
    session.info "Deregister caller from reminder group", command: 'deregister', action: 'start'
    p @reminder_group
    deregister_caller_from_reminder_group session
    session.info "Deregistration complete", command: 'deregister', action: 'finish'
    super
  end

  private

  def deregister_caller_from_reminder_group session
    reminder_group = session.project.ext_reminder_groups.where(:name => @reminder_group).first
    p ("Reminder group " + @reminder_group)
    reminder_group.deregister_address(session.address)
  end

end
