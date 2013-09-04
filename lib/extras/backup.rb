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

  def file_compression
    @file_compression = "#{@directory[:current]}.tar.gz"
  end

  class << self
    def full!
      if File.exists? Amazon::S3::CONFIG_FILE_PATH
        backup = setup :full
        backup.copy_files
        backup.mysqldump
        backup.compress
        # uploading to aws
        Amazon::S3.upload backup.file_compression
      end
    end

    def incremental!
      if File.exists? Amazon::S3::CONFIG_FILE_PATH
        backup = setup :incremental
        backup.copy_files
        backup.incremental
        backup.compress
        # uploading to aws
        Amazon::S3.upload backup.file_compression
      end
    end

    def setup type
      instance = Backup.new "#{Time.now.strftime '%Y%m%d%H%M%S'}_#{type}"
      instance.prepare!
      instance
    end
  end

  def prepare!
    p "=============== preparing ==============="
    @directory = {
      current: current_dir,
      config: [current_dir, '/', 'config'].join,
      asterisk: [current_dir, '/', 'asterisk'].join,
      asterisk_etc: [current_dir, '/', 'asterisk', '/', 'etc'].join,
      asterisk_sounds: [current_dir, '/', 'asterisk', '/', 'sounds'].join
    }
    FileUtils.mkdir BASEDIR unless File.exists? BASEDIR
    FileUtils.mkdir @directory.values
  end

  def copy_files
    p "=============== copying ==============="
    system "cp -rH data #{@directory[:current]}"
    system "cp config/*.yml #{@directory[:config]}"
    # asterisk files
    system "cp #{asterisk_config['config_dir']}/* #{@directory[:asterisk_etc]}"
    system "cp -rH #{asterisk_config['sounds_dir']}/verboice #{@directory[:asterisk_sounds]}"
  end

  def mysqldump
    p "=============== mysqldump database ==============="
    cmd = "mysqldump --single-transaction -u#{db_config['username']} --flush-logs"
    cmd << " -p'#{db_config['password']}'" if db_config['password'].present?
    cmd << " #{db_config['database']} > #{@directory[:current]}/verboice.sql"
    system(cmd)
  end

  def incremental
    p "=============== copying mysql log_bin ==============="
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
    p "=============== compressing ==============="
    system "tar -zcf #{file_compression} #{@directory[:current]}"
    # clean up
    FileUtils.rm_rf @directory[:current]
  end

  private
  def execute_sql sql
    p "=============== executing mysql command ==============="
    yield if block_given?
    cmd = %{mysql -u#{db_config['username']} -e "#{sql}"}
    cmd << " -p'#{db_config['password']}'" if db_config['password'].present?
    system(cmd)
  end
end