require 'spec_helper'


describe String do 
  describe "#is_contact?" do
    it 'return false with character beside number and string' do
      '123ds32'.is_contact?.should be_false
      '1234,009'.is_contact?.should be_false
      ''.is_contact?.should be_false
    end

    it 'return true with character are number and space' do
      '123456'.is_contact?.should be_true
      '1234 123'.is_contact?.should be_true
      ' 1234 123 '.is_contact?.should be_true
    end
  end
end