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

describe Backup do
  let(:backup) { Backup.new 'test' }

  describe '.prepare!' do
    it 'should create directory strcuture' do
      FileUtils.expects(:mkdir).with [
        'tmp/backups/test',
        'tmp/backups/test/config',
        'tmp/backups/test/asterisk',
        'tmp/backups/test/asterisk/etc',
        'tmp/backups/test/asterisk/sounds',
      ]
      backup.prepare!
    end
  end

  describe 'copy_files' do
    it 'should copy data directory' do
      backup.expects(:system).with "cp -r data tmp/backups/test"
    end

    it 'should copy yml files in config' do
      backup.expects(:system).with "cp config/*.yml tmp/backups/test/config"
    end

    it 'should copy asterisk configuration' do
      backup.expects(:system).with "cp #{backup.asterisk_config['config_dir']}/* tmp/backups/test/asterisk/etc"
    end

    it 'should copy asterisk sound files' do
      backup.expects(:system).with "cp -r #{backup.asterisk_config['sounds_dir']}/verboice tmp/backups/test/asterisk/sounds"
    end
  end
end