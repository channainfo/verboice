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

class Backup

  BASEDIR = 'tmp/backups'
  BIN_LOGS = '/var/log/mysql/mysql-bin.[0-9]*'

  attr_reader :name, :directory

  def initialize(name)
    @name = name
  end

  def db_config
    @db_config ||= Rails.configuration.database_configuration[Rails.env]
  end

  def asterisk_config
    @asterisk_config ||= YAML::load(File.read 'config/asterisk.yml')
  end

  def current_dir
    @current_dir ||= [BASEDIR, '/', @name].join
  end

  class << self
    def full!
      backup = setup :full
      backup.copy_files
      backup.mysqldump
      backup.compress
    end

    def incremental!
      backup = setup :incremental
      backup.copy_files
      backup.incremental
      backup.compress
    end

    def setup type
      instance = Backup.new "#{Time.now.strftime '%Y%m%d%H%M%S'}_#{type}"
      instance.prepare!
      instance
    end
  end

  def prepare!
    @directory = {
      current: current_dir,
      config: [current_dir, '/', 'config'].join,
      asterisk: [current_dir, '/', 'asterisk'].join,
      asterisk_etc: [current_dir, '/', 'asterisk', '/', 'etc'].join,
      asterisk_sounds: [current_dir, '/', 'asterisk', '/', 'sounds'].join
    }
    FileUtils.mkdir @directory.values
  end

  def copy_files
    system "cp -rH data #{@directory[:current]}"
    system "cp config/*.yml #{@directory[:config]}"
    # asterisk files
    system "cp #{asterisk_config['config_dir']}/* #{@directory[:asterisk_etc]}"
    system "cp -rH #{asterisk_config['sounds_dir']}/verboice #{@directory[:asterisk_sounds]}"
  end

  def mysqldump
    cmd = "mysqldump --single-transaction -u#{db_config['username']} --flush-logs"
    cmd << " -p'#{db_config['password']}'" if db_config['password'].present?
    cmd << " #{db_config['database']} > #{@directory[:current]}/verboice.sql"
    system(cmd)
  end

  def incremental
    execute_sql 'flush logs'
    # backup binary logs
    logs = Dir.glob(BIN_LOGS).sort
    execute_sql "purge master logs to '#{File.basename(logs.pop)}'" do
      logs.each do |log|
        system "cp #{log} #{@directory[:current]}"
      end
    end
  end

  def compress
    system "tar -zcf #{@directory[:current]}.tar.gz #{@directory[:current]}"
    # clean up
    FileUtils.rm_rf @directory[:current]
  end

  private
  def execute_sql sql
    yield if block_given?
    cmd = %{mysql -u#{db_config['username']} -e "#{sql}"}
    cmd << " -p'#{db_config['password']}'" if db_config['password'].present?
    system(cmd)
  end
end