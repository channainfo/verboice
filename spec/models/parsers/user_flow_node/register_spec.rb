# Copyright (C) 2010-2012, InSTEDD
#
# This file is part of Verboice.
#
# Verboice is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Verboice is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Verboice.  If not, see <http://www.gnu.org/licenses/>.

require 'spec_helper'

module Parsers
  module UserFlowNode
    describe Register do

      let(:call_flow) { double('call_flow', :id => 5) }

      before(:each) do
        @options = {
          'id' => 1,
          'reminder_group' => 'pregnancy',
          'name' => 'Register Step',
          'confirmation_resource' => {
            "guid" => 2
          }
        }
      end

      context "Current caller" do
        it "should compile to an equivalent flow" do
          @options.merge! 'option' => {'current_caller' => 'current_caller'}
          register = Register.new call_flow, @options

          register.equivalent_flow.first.should eq(
            Compiler.parse do
              Label 1
              Assign "current_step", 1
              AssignValue "current_step_name", "Register Step"
              Register nil, "pregnancy"
              PlayResource 2
            end.first
          )
        end
      end

      context "Other phone number from input step" do
        it "should compile to an equivalent flow" do
          @options.merge! 'option' => {'step' => '9999'}
          register = Register.new call_flow, @options

          register.equivalent_flow.first.should eq(
              Compiler.parse do
                Label 1
                Assign "current_step", 1
                AssignValue "current_step_name", "Register Step"
                Register "value_9999", "pregnancy"
                PlayResource 2
              end.first
            )
        end
      end

      context "Other phone number from variable" do
        it "should compile to an equivalent flow" do
          @options.merge! 'option' => {'variable' => 'foo'}
          register = Register.new call_flow, @options

          register.equivalent_flow.first.should eq(
              Compiler.parse do
                Label 1
                Assign "current_step", 1
                AssignValue "current_step_name", "Register Step"
                Register "var_foo", "pregnancy"
                PlayResource 2
              end.first
            )
        end
      end
    end
  end
end
