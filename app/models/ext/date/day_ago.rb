module Ext
  module Date
    class DayAgo < Date
      def initialize number
        @number = number
      end

      def date
        @number.days.ago
      end
    end
  end
end