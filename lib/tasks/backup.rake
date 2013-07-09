require 'fileutils'
require 'yaml'

namespace :backup do
  desc "Backup the whole database"
  task :full => [:files, :asterisk, :environment] do
    config = Rails.configuration.database_configuration[Rails.env]

    # mysqldump
    cmd = "mysqldump --single-transaction -u#{config['username']}"
    cmd << " -p'#{config['password']}'" if config['password'].present?
    cmd << " #{config['database']} > #{@directory}/verboice.sql"
    system(cmd)

    # tar and compress
    system "tar -zcf #{@directory}.tar.gz #{@directory}"

    # clean up
    FileUtils.rm_rf @directory
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