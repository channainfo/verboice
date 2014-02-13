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

module Parsers
  module UserFlowNode
    class SpeechRecognition < UserCommand
      attr_reader :id, :name, :call_flow
      attr_accessor :next

      def initialize call_flow, params
        @id = params['id']
        @name = params['name'] || ''
        @explanation_resource = Resource.new params['explanation_resource']
        @confirmation_resource = Resource.new params['confirmation_resource']
        @timeout     = params['timeout']
        @stop_key    = params['stop_key']
        @call_flow   = call_flow
        @next        = params['next']
        @root_index  = params['root']

        @old_result1   = params['old_result1']
        @old_accuracy1 = params['old_accuracy1']
        
        @old_result2   = params['old_result2']
        @old_accuracy2 = params['old_accuracy2']

        @old_result3   = params['old_result3']
        @old_accuracy3 = params['old_accuracy3']


        @result1   = params['result1']
        @accuracy1 = params['accuracy1']
        
        @result2   = params['result2']
        @accuracy2 = params['accuracy2']
        
        @result3   = params['result3']
        @accuracy3 = params['accuracy3']
      end

      def is_root?
        @root_index.present?
      end

      def root_index
        @root_index
      end

      def equivalent_flow
        Compiler.parse do |compiler|
          compiler.Label @id
          compiler.AssignValue "current_step", @id
          compiler.AssignValue "current_step_name", "#{@name}"
          compiler.Trace context_for %("Record message. Download link: " + record_url(#{@id}))
          compiler.append @explanation_resource.equivalent_flow
          compiler.SpeechRecognition @id, @name,{:stop_keys     => @stop_key,
                                                 :timeout       => @timeout, 

                                                 :old_result1   => @old_result1, 
                                                 :old_accuracy1 => @old_accuracy1,
                                                 :old_result2   => @old_result2, 
                                                 :old_accuracy2 => @old_accuracy2,
                                                 :old_result3   => @old_result3, 
                                                 :old_accuracy3 => @old_accuracy3,


                                                 :result1       => @result1,
                                                 :accuracy1     => @accuracy1,
                                                 :result2       => @result2,
                                                 :accuracy2     => @accuracy2,
                                                 :result3       => @result3,
                                                 :accuracy3     => @accuracy3

                                               }

          compiler.append @confirmation_resource.equivalent_flow
          compiler.append @next.equivalent_flow if @next
        end
      end
    end
  end
end
