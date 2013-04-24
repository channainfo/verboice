class String
  def number?
    self == self.to_i.to_s
  end

  def date?
    begin
      Date.strptime(self, Date::DEFAULT_FORMAT)
    rescue
      nil
    end
  end

  def date_time?
    begin
      DateTime.strptime(self, DateTime::DEFAULT_FORMAT)
    rescue
      nil
    end
  end

  def audio_mime_type?
    ["audio/mp3", "audio/mpeg", "audio/x-wav"].include? self
  end

  def mpeg_mime_type?
    ["audio/mp3", "audio/mpeg"].include? self
  end

end