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

class Commands::IfCommand < Command
  attr_accessor :condition
  attr_accessor :then
  attr_accessor :else

  def initialize(condition, if_true, if_false = nil)
    @condition = condition
    @then = if_true if if_true
    @else = if_false if if_false
  end

  def next=(cmd)
    @then.last.next = cmd if @then
    @else.last.next = cmd if @else
    super
  end

  def run(session)
    session.trace "Testing statement: #{@condition}", command: 'if', action: 'testing'
    if session.eval(@condition)
      session.trace "The statement is true", command: 'if', action: 'true'
      @then || super
    else
      session.trace "The statement is false", command: 'if', action: 'false'
      @else || super
    end
  end

  def serialize_parameters
    {:condition => @condition}.tap do |params|
      params[:then] = @then if @then
      params[:else] = @else if @else
    end
  end
end