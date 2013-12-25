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

namespace :calllog do
  desc "Migrate call log trace to step interaction"
  task :migrate_traces => :environment do
    puts "Migrating call log traces..."

    started_at = Time.now
    CallLog.includes(:traces).where(step_interaction: nil).find_each do |call_log|
      call_log.update_attributes step_interaction: call_log.interaction_details.join(';')
    end
 
    puts "Task is done in #{Time.now - started_at} seconds."
  end
end
