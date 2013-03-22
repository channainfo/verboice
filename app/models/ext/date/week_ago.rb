module Ext
  module Date
    class WeekAgo < Date
      def initialize number
        @number = number
      end

      def date
        @number.weeks.ago
      end
    end
  end
end