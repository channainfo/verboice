module Ext
  module Date
    class MonthAgo < Date
      def initialize number
        @number = number
      end

      def date
        @number.months.ago
      end
    end
  end
end