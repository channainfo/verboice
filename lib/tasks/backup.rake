require 'fileutils'
require 'yaml'

namespace :backup do
  desc "Full backup"
  task :full => [:files, :asterisk, :mysql, :compress] do
  end

  desc "Incremential backup"
  task :incremental => [:files, :asterisk, :mysql_incremental, :compress] do
  end

  desc "Backup files (data/, config/*.yml)"
  task :files => :prepare do
    system "rsync -az data #{@directory}"
    system "rsync -az config/*.yml #{@directory}/config"
  end

  desc "Backup asterisk sounds_dir and config_dir"
  task :asterisk => :prepare do
    asterisk = YAML::load(File.read 'config/asterisk.yml')
    system "rsync -az #{asterisk['sounds_dir']}/verboice #{@directory}/asterisk/sounds"
    system "rsync -az #{asterisk['config_dir']}/* #{@directory}/asterisk/etc"
  end

  desc "Backup mysql database with mysqldump"
  task :mysql => [:prepare, :environment] do
    config = Rails.configuration.database_configuration[Rails.env]

    # mysqldump
    cmd = "mysqldump --single-transaction -u#{config['username']} --flush-logs"
    cmd << " -p'#{config['password']}'" if config['password'].present?
    cmd << " #{config['database']} > #{@directory}/verboice.sql"
    system(cmd)
  end

  desc "Backup mysql database binary log"
  task :mysql_incremental => [:prepare, :environment] do
    execute_sql 'flush logs'
    # backup binary logs
    logs = Dir.glob("/var/log/mysql/mysql-bin.[0-9]*").sort
    execute_sql "purge master logs to '#{File.basename(logs.pop)}'" do
      logs.each do |log|
        system "cp #{log} #{@directory}"
      end
    end
  end

  desc "Compress backup directory"
  task :compress => :prepare do
    system "tar -zcf #{@directory}.tar.gz #{@directory}"
    # clean up
    FileUtils.rm_rf @directory
  end

  desc "Prepare backup directory"
  task :prepare do
    # create backup directory structure
    #  @directory: tmp/backups/#{timestamp}
    #  - timestamp: #{Year}#{Month}#{Day}#{Hour}#{Minute}#{Second}
    #
    #  eg. tmp/backups/20130709155545
    @directory ||= "tmp/backups/#{Time.now.strftime '%Y%m%d%H%M%S'}"
    FileUtils.mkdir_p "#{@directory}/asterisk"
  end
end

# helpers

def execute_sql(sql)
  yield if block_given?
  config = Rails.configuration.database_configuration[Rails.env]

  cmd = %{mysql -u#{config['username']} -e "#{sql}"}
  cmd << " -p'#{config['password']}'" if config['password'].present?
  system(cmd)
end