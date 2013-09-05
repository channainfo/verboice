every 1.day, :at => '0:00 am' do
	rake "reminder:schedule"
end

every "0 23 1 * *" do
  rake "backup:full"
end

every :day, :at => '1:00 am' do
  rake "backup:incremental"
end