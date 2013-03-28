class DateTime
  DEFAULT_FORMAT  = '%Y-%m-%d %H:%M %Z'

  def to_string format = DateTime::DEFAULT_FORMAT, time_zone = "UTC"
    zone = self.in_time_zone(time_zone)
    zone.strftime(format)
  end
end