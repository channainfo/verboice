class Date
  include DateComparison

  DEFAULT_FORMAT = "%Y-%m-%d"

  def to_string format = Date::DEFAULT_FORMAT
    self.strftime(format)
  end
end