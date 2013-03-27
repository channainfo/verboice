class String
  def number?
    self == self.to_i.to_s
  end

  def persisted_variable_value
    self.split("|").first
  end
end