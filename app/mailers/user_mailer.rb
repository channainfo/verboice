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

class UserMailer < Devise::Mailer
  add_template_helper InsteddRails::MailerHelper
  
  default from: 'verboice@instedd.org'
  layout 'mail'

  def generating_zip project_id, filename
    @project = Project.find project_id
    @filename = filename

    mail to: @project.account.email, subject: 'Your call logs is ready to be download'
  end
end