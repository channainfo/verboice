
namespace :reminder do
	desc "schedule calls to all reminder schedule in projects"
	task :schedule => :environment do
		now = DateTime.now.utc
		current = DateTime.new(now.year, now.month, now.day, 23, 59, 59, "0")

    dir_name = "/tmp/log/"
    Dir.mkdir dir_name unless Dir.exists? dir_name
    file_name = "#{dir_name}#{current.to_s}.log"
    File.open file_name, "w" do |f|
      f.puts "started at #{current.to_s}"

  		Project.all.each do |project|
        f.puts "started project:#{project.id}"
  			Ext::ReminderSchedule.schedule project.id, current
        # Ext::PregnancyReminder.schedule project.id, current
        f.puts "finished project:#{project.id}"
  		end

      f.puts "finished at #{DateTime.now.utc.to_s}"
    end
	end
end