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

    def has_addresses?
      not addresses.empty?
    end

    def register_caller_to_group(address)
      unless self.addresses.include? (address)
          self.addresses.push(address)
          self.save!
          contact = self.project.contacts.find_by_address(address)
          unless contact
            self.project.contacts.create!(:address => address)
          end
      end      
    end

    def deregister_caller_from_group(address)
      if self.addresses.include? (address)
          self.addresses.delete(address)
          self.save!
      end  
    end
  end
end
