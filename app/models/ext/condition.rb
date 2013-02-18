module Ext
  class Condition
    def self.build hash
      conditions = []
      hash.each do |k, v|
        conditions.push v
      end
      conditions
    end
  end
end