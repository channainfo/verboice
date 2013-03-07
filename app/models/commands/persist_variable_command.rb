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

class Commands::PersistVariableCommand < Command

  attr_accessor :variable_name, :expression

  def initialize variable_name, expression
    @variable_name = variable_name
    @expression    = expression
  end

  def run session
    value = session["var_#{@variable_name}"] = evaluate_expression(session)
    session.trace "Saving '#{@variable_name}'", command: 'persist_variable', action: 'start'
    contact = contact_from session
    if implicit_variable = ImplicitVariable.find(@variable_name)
      persisted_variable = contact.persisted_variables.find_by_implicit_key(implicit_variable.key)
      if persisted_variable
        persisted_variable.value = value
        persisted_variable.save!
      else
        contact.persisted_variables.create!\
          implicit_key: implicit_variable.key,
          value: value
      end

    else
      project_variable = contact.project_variables.find_by_name @variable_name
      if project_variable
        persisted_variable = contact.persisted_variables.find_by_project_variable_id project_variable.id
        if persisted_variable
          persisted_variable.value = value
          persisted_variable.save!
        else
          persisted_variable = contact.persisted_variables.create!\
            project_variable: project_variable,
            value: value
        end
      else
        persisted_variable = contact.persisted_variables.create!\
          project_variable: contact.project.project_variables.create!(name: @variable_name),
          value: value
      end

    end

    call_log_answer = CallLogAnswer.find_by_call_log_id_and_project_variable_id(session.call_log.id, persisted_variable.project_variable.id)
    unless call_log_answer
      # add call_log_answer
      CallLogAnswer.create! :call_log_id => session.call_log.id, :project_variable_id => persisted_variable.project_variable.id, :value => evaluate_expression(session) if evaluate_expression(session) && persisted_variable
    else      
      call_log_answer.update_attributes({:value => evaluate_expression(session)})
    end

    session.trace "'#{@variable_name}' saved for contact '#{contact.address}'.", command: 'persist_variable', action: 'finish'
    super
  end

  def evaluate_expression(session)
    if @expression
      session.eval(@expression)
    else
      nil
    end
  end

  def contact_from session
    contact = session.contact
    session.trace "Caller address is unknown. Variable '#{@variable_name}' will be saved for contact '#{contact.address}'.", command: 'persist_variable', action: 'contact_unknown' unless session.address.presence
    contact
  end
end