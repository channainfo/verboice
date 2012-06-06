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

class CallLogEntry < ActiveRecord::Base
  belongs_to :call, :class_name => 'CallLog'
  attr_accessible :details, :severity, :call, :step_name, :step_id, :command, :action, :description
  store :details, accessors: [ :step_name, :step_id, :command, :action, :description ]
  Levels = [:error, :warn, :info, :trace]
  enum_attr :severity, Levels
end