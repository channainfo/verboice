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

class Commands::JsCommand < Command
  def initialize(source)
    @source = source
  end

  def serialize_parameters
    {source: @source}
  end

  def run(session)
    session.trace "Starting JS code excecution.", command: 'js', action: 'start'
    session.eval @source
    session.trace "JS code excecution ended.", command: 'js', action: 'finish'
    super
  end
end
