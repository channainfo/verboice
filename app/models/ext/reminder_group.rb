module Ext
  class ReminderGroup < ExtActiveRecord
    belongs_to :project
    has_many :reminder_schedules, :dependent => :nullify

    assign_has_many_to "Project" ,:ext_reminder_groups, :class_name => "Ext::ReminderGroup", :dependent => :destroy
    assign_accepts_nested_attributes_for_to "Project", :ext_reminder_groups, 
      :reject_if => lambda { |attributes| attributes[:name].blank?},
      :allow_destroy => true

    assign_attr_accessible_to "Project", :ext_reminder_group_attributes

    serialize :addresses, Array
    validates :name, :project, :presence => true
    validates :name, :uniqueness => { :scope => :project_id }
    
    attr_accessible :name, :addresses, :project_id

    after_save :register_contacts

    def register_contacts
      Contact.register addresses, project if has_addresses?
    end

    def has_addresses?
      not addresses.empty?
    end

    def register_address(address)
      unless addresses.include? (address)
          addresses.push(address)
          save
      end      
    end

    def deregister_address(address)
      if addresses.include? (address)
          addresses.delete(address)
          save
      end  
    end
  end
end
