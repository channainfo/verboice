class Date
  DEFAULT_FORMAT = "%Y-%m-%d"

  def equal? date
    self == date
  end

  def greater_than? date
    self > date
  end

  def less_than? date
    self < date
  end

  def greater_or_equal? date
    greater_than? date or equal? date
  end

  def less_or_equal? date
    less_than? date or equal? date
  end

  def to_string format = Date::DEFAULT_FORMAT
    self.strftime(format)
  end
end