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

describe QueuedCall do
  it 'create new session with custom callback url' do
    qcall = QueuedCall.make :callback_url => 'http://foo.com'
    session = qcall.new_session
    session.callback_url.should == 'http://foo.com'
    session.commands.should == Compiler.make { Answer(); Callback('http://foo.com') }
  end

  it 'create new session with custom flow' do
    qcall = QueuedCall.make :flow => Compiler.make { Answer(); Hangup() }
    session = qcall.new_session
    session.commands.should == Compiler.make { Answer(); Hangup() }
  end

  it 'create new session with custom callback and custom status callback url' do
    qcall = QueuedCall.make :callback_url => 'http://callback', :status_callback_url => 'http://foo.com'
    session = qcall.new_session
    session.status_callback_url.should == 'http://foo.com'
  end

  it 'should cancel call log' do
    qcall = QueuedCall.make
    call_log = qcall.call_log
    call_log.state.should_not eq(:cancelled)
    qcall.cancel_call!
    call_log.reload.state.should eq(:cancelled)
  end
end
