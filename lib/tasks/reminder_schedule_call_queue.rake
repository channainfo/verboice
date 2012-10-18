
namespace :reminder do
	desc "schedule calls to all reminder schedule in projects  " 
	task :schedule => :environment do
		Project.all.each do |project|
			now = DateTime.now
			Ext::ReminderSchedule.schedule project.id, DateTime.new(now.year, now.month, now.day)
		end
	end

end