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

module Restores
  class Incremental < Restore
    def initialize
      super
    end

    def mysql bin_log_file
      Log.info(:s3_log_dir, "restore: restoring mysql binary log: #{bin_log_file}")
      cmd = "mysqlbinlog --database=#{db_config['database']} #{bin_log_file}"
      cmd << " | mysql -u#{db_config['username']}"
      cmd << " -p'#{db_config['password']}'" if db_config['password'].present?
      system cmd
    end

    def type
      Backup::INCREMENTAL
    end
  end
end