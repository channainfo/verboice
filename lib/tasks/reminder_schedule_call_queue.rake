
namespace :reminder do
	desc "schedule calls to all reminder schedule in projects  " 
	task :schedule => :environment do
		now = DateTime.now
		current = DateTime.new(now.year, now.month, now.day, 23, 59, 59)

		Project.all.each do |project|
			Ext::ReminderSchedule.schedule project.id, current
		end
	end

end