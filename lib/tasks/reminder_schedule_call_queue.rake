
namespace :reminder do
	desc "schedule calls to all reminder schedule in projects  " 
	task :schedule => :environment do
		now = DateTime.now.utc
		current = DateTime.new(now.year, now.month, now.day, 23, 59, 59, "0")

		p current

		Project.all.each do |project|
			Ext::ReminderSchedule.schedule project.id, current
		end
	end

end