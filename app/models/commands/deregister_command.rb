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

  def initialize reminder_phone_book_type_name, options = {}
    @reminder_phone_book_type_name = reminder_phone_book_type_name
  end

  def run(session)
    session.info "Deregister caller from reminder phone book", command: 'register', action: 'start'
    deregister_caller_to_reminder_phone_book session
    session.info "Deregistration complete", command: 'register', action: 'finish'
    super
  end

  private

  def deregister_caller_to_reminder_phone_book session
    reminder_phone_book_type = session.project.ext_reminder_phone_book_types.where(:name => @reminder_phone_book_type_name).first
    reminder_phone_book = reminder_phone_book_type.reminder_phone_books.where(phone_number: session.address).first unless reminder_phone_book_type.nil?
    reminder_phone_book.destroy unless reminder_phone_book.nil?
  end

end
