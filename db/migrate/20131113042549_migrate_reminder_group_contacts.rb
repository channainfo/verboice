class MigrateReminderGroupContacts < ActiveRecord::Migration
  def up
    database_config_file = File.join(Rails.root, "config", "database.yml")
    database_yml = YAML::load(File.open(database_config_file))[Rails.env]
    begin
      db_name = "#{database_yml['database']}_bak"
      db_bak = Mysql2::Client.new(host: "localhost", username: database_yml["username"], password: database_yml["password"], database: db_name)
      resultset = db_bak.query("select * from ext_reminder_groups")
      resultset.each do |r|
        reminder_group = Ext::ReminderGroup.find(r['id']) rescue next
        addresses = r['addresses'].gsub!("---\n-", "").delete("-").delete("'").split("\n")
        addresses.each do |address|
          reminder_group.register_address address.strip.to_s.encode("UTF-8", "binary", :invalid => :replace, :undef => :replace)
        end
      end
    rescue
      say "There is no database #{db_name}"
    end
  end

  def down
  end
end
