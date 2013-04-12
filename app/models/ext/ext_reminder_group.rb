module Ext
  class ReminderGroup < ExtActiveRecord
    belongs_to :project

    assign_has_many_to "Project" ,:ext_reminder_groups, :class_name => "Ext::ReminderGroup", :dependent => :destroy
    assign_accepts_nested_attributes_for_to "Project", :ext_reminder_groups, 
      :reject_if => lambda { |attributes| attributes[:name].blank?},
      :allow_destroy => true

    assign_attr_accessible_to "Project", :ext_reminder_group_attributes

    serialize :addresses, Array
    validates :name, :project, :presence => true
    validates :name, :uniqueness => { :scope => :project_id }
    
    attr_accessible :name, :description, :addresses, :project_id

    def has_addresses?
      not addresses.empty?
    end
  end
end
