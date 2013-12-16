
namespace :reminder do
	desc "schedule calls to all reminder schedule in projects"
	task :schedule => :environment do
		now = DateTime.now.utc

    dir_name = "/tmp/log/"
    Dir.mkdir dir_name unless Dir.exists? dir_name
    file_name = "#{dir_name}#{now.to_s}.log"
    File.open file_name, "w" do |f|
      f.puts "started at #{now.to_s}"

  		Project.all.each do |project|
        f.puts "started project:#{project.id}"
  			Ext::ReminderSchedule.schedule project.id, now
        # Ext::PregnancyReminder.schedule project.id, now
        f.puts "finished project:#{project.id}"
  		end

      f.puts "finished at #{DateTime.now.utc.to_s}"
    end
	end

  desc "migrate reminder schedule to support channel-suggestion"
  task :migrate_channel => :environment do
    Ext::ReminderSchedule.channel_migrate_reminder_schedule
  end

end