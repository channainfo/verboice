module Ext
  class ReminderPhoneBookType < ExtActiveRecord
    belongs_to :project
    attr_accessible :description, :name, :project

    validates :name, :project, :presence => true
  end
end
