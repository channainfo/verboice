class String
  def number?
    self == self.to_i.to_s
  end
end