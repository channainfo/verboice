require 'fileutils'
require 'yaml'

namespace :backup do
  desc "Full backup"
  task :full => ['prepare:full', :files, :asterisk, :mysqldump, :compress] do
  end

  desc "Incremential backup"
  task :incremental => ['prepare:incremental', :files, :asterisk, :mysql_incremental, :compress] do
  end

  # "Backup files (data/, config/*.yml)"
  task :files do
    system "rsync -az data #{@directory}"
    system "rsync -az config/*.yml #{@directory}/config"
  end

  # "Backup asterisk sounds_dir and config_dir"
  task :asterisk do
    asterisk = YAML::load(File.read 'config/asterisk.yml')
    system "rsync -az #{asterisk['sounds_dir']}/verboice #{@directory}/asterisk/sounds"
    system "rsync -az #{asterisk['config_dir']}/* #{@directory}/asterisk/etc"
  end

  # "Backup mysql database with mysqldump"
  task :mysqldump do
    cmd = "mysqldump --single-transaction -u#{@db_config['username']} --flush-logs"
    cmd << " -p'#{@db_config['password']}'" if @db_config['password'].present?
    cmd << " #{@db_config['database']} > #{@directory}/verboice.sql"
    system(cmd)
  end

  # "Backup mysql database binary log"
  task :mysql_incremental do
    execute_sql 'flush logs'
    # backup binary logs
    logs = Dir.glob("/var/log/mysql/mysql-bin.[0-9]*").sort
    execute_sql "purge master logs to '#{File.basename(logs.pop)}'" do
      logs.each do |log|
        system "cp #{log} #{@directory}"
      end
    end
  end

  # "Compress backup directory"
  task :compress do
    system "tar -zcf #{@directory}.tar.gz #{@directory}"
    # clean up
    FileUtils.rm_rf @directory
  end

  # prepare backup directory and config
  namespace :prepare do
    task :full => :environment do
      prepare :full
    end

    task :incremental => :environment do
      prepare :incremental
    end
  end
end

# helper methods

private

def prepare(type)
  # database config
  @db_config ||= Rails.configuration.database_configuration[Rails.env]

  # backup directory
  @directory ||= "tmp/backups/#{Time.now.strftime '%Y%m%d%H%M%S'}_#{type.to_s}"
  FileUtils.mkdir_p "#{@directory}/asterisk"
end

def execute_sql(sql)
  yield if block_given?
  cmd = %{mysql -u#{@db_config['username']} -e "#{sql}"}
  cmd << " -p'#{@db_config['password']}'" if @db_config['password'].present?
  system(cmd)
end