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

class Contact < ActiveRecord::Base
  belongs_to :project
  has_many :persisted_variables, :dependent => :destroy, :inverse_of => :contact
  has_many :recorded_audios, :dependent => :destroy
  has_many :project_variables, :through => :project

  accepts_nested_attributes_for :persisted_variables,
    :reject_if => lambda { |attributes| attributes[:value].blank? || (attributes[:project_variable_id].blank? && attributes[:implicit_key].blank?) },
    :allow_destroy => true

  # TODO: anonymous is unused and will be remove in the future
  attr_accessible :address, :anonymous, :persisted_variables_attributes
  validates_presence_of :project, :address
  validates_uniqueness_of :address, :scope => :project_id

  def evaluate? conditions
    match = false
    conditions.each do |condition|
      if !condition.evaluate? persisted_variables
        match = false
        break
      else
        match = true
      end
    end if conditions
    match
  end

  def self.register addresses, project
    addresses.each { |address| project.contacts.where(address: address).first_or_create! }
  end

  def as_json(options={})
    super(options.merge({except: [:anonymous]}))
  end
end
