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

describe VrzContainer do

  before(:each) do

    @resource = Resource.make name: 'resource name'
    @project = @resource.project

    @call_flow = CallFlow.make user_flow: [
      {
        'id' => 1,
        'root' => 1,
        'type' => 'play',
        'name' => 'Play number one',
        'resource' => {
          "guid" => @resource.guid
        }
      }
    ], project: @project, name: 'call flow name'

    @external_service = ExternalService.make project: @project, name: 'external service name'
    @external_step = ExternalServiceStep.make external_service: @external_service, name: 'external step name'
    @call_flow.external_service_guids = [@external_service.guid]

    @localized_resource = UploadLocalizedResource.make resource: @resource, language: 'en', audio: 'some audio'

    @vrz_container = VrzContainer.for @call_flow
  end

  it 'exports' do

    path = '/path/to/zip'

    # Mock zip open
    zip_block = nil
    Zip::ZipOutputStream.should_receive(:open) do |the_path, &block|
      zip_block = block
      the_path.should eq(path)
    end

    # Mock zip stream
    stream = double('stream')
    stream.should_receive(:put_next_entry).with('workflow.yml')
    stream.should_receive(:print).with(@call_flow.user_flow.to_yaml)

    stream.should_receive(:put_next_entry).with("Service #{@external_service.guid}.yml")
    stream.should_receive(:print).with(@external_service.attributes.tap do |a|
      a.delete 'id'
      a.delete 'project_id'
    end.to_yaml)

    stream.should_receive(:put_next_entry).with("Step #{@external_step.guid}.yml")
    stream.should_receive(:print).with(@external_step.attributes.tap{ |a| a.delete 'id' }.to_yaml)

    stream.should_receive(:put_next_entry).with("localized_resource #{@resource.guid} - #{@localized_resource.language} - #{@localized_resource.guid}.yml")
    stream.should_receive(:print).with(@localized_resource.attributes.tap do |attributes|
      attributes.delete 'audio'
      attributes.delete 'id'
    end.to_yaml)

    stream.should_receive(:put_next_entry).with("resource_audio #{@resource.guid} - #{@localized_resource.language} - #{@localized_resource.guid}.wav")
    stream.should_receive(:print).with(@localized_resource.audio)

    stream.should_receive(:put_next_entry).with("resource #{@resource.guid}.yml")
    stream.should_receive(:print).with(@resource.attributes.tap do |attributes|
      attributes.delete 'id'
      attributes.delete 'project_id'
    end.to_yaml)

    @vrz_container.export path
    zip_block.call stream
  end

  it 'imports when resources and external services doesnt exist' do

    in_temp_dir do |path|
      zip_path = File.join(path, 'zip_file.zip')

      @vrz_container.export(zip_path)

      CallFlow.first.tap { |f| f.user_flow = nil }.save!
      ExternalService.delete_all
      ExternalServiceStep.delete_all
      Resource.delete_all
      UploadLocalizedResource.delete_all

      @vrz_container.import zip_path

    end

    CallFlow.count.should == 1
    CallFlow.first.user_flow.should == @call_flow.user_flow

    Resource.count.should == 1
    Resource.first.project.should == @project
    Resource.first.guid.should == @resource.guid

    LocalizedResource.count.should == 1
    LocalizedResource.first.guid.should == @localized_resource.guid
    LocalizedResource.first.resource.attributes.except('id').should == @resource.attributes.except('id')
    LocalizedResource.first.audio.should == @localized_resource.audio
    LocalizedResource.first.type.should == "UploadLocalizedResource"
    LocalizedResource.first.attributes.except('id').should == @localized_resource.attributes.except('id')

    ExternalService.count.should == 1
    ExternalService.first.guid.should == @external_service.guid
    ExternalService.first.project.should == @project

    ExternalServiceStep.count.should == 1
    ExternalServiceStep.first.guid.should == @external_step.guid
    ExternalServiceStep.first.external_service.attributes.except('id').should == @external_service.attributes.except('id')
  end


  it 'imports when resources and external services do exist' do

    in_temp_dir do |path|
      zip_path = File.join(path, 'zip_file.zip')

      @vrz_container.export(zip_path)

      CallFlow.first.tap do|f|
        f.user_flow = nil
        f.name = 'other name'
      end.save!
      ExternalService.first.tap {|f| f.name = 'new external service name' }.save!
      ExternalServiceStep.first.tap {|f| f.name = 'new external service step name' }.save!
      Resource.first.tap {|f| f.name = 'asdfgr321' }.save!
      UploadLocalizedResource.first.tap do|f|
        f.audio = 'some other audio'
        f.language = 'es'
      end.save!

      @vrz_container.import zip_path

    end

    CallFlow.count.should == 1
    CallFlow.first.user_flow.should == @call_flow.user_flow

    Resource.count.should == 1
    Resource.first.project.should == @project
    Resource.first.guid.should == @resource.guid

    LocalizedResource.count.should == 1
    LocalizedResource.first.guid.should == @localized_resource.guid
    LocalizedResource.first.resource.attributes.except('updated_at').should == @resource.attributes.except('updated_at')
    LocalizedResource.first.audio.should == @localized_resource.audio
    LocalizedResource.first.type.should == "UploadLocalizedResource"
    LocalizedResource.first.attributes.except('updated_at').should == @localized_resource.attributes.except('updated_at')

    ExternalService.count.should == 1
    ExternalService.first.guid.should == @external_service.guid
    ExternalService.first.project.should == @project
    ExternalService.first.name.should == 'new external service name'

    ExternalServiceStep.count.should == 1
    ExternalServiceStep.first.guid.should == @external_step.guid
    ExternalServiceStep.first.name.should == 'new external service step name'
  end

  def in_temp_dir

    path = File.expand_path "#{Rails.root}/tmp/data/#{Time.now.to_i}#{rand(1000)}/"
    FileUtils.mkdir_p(path)

    yield(path)

  ensure
    FileUtils.rm_rf(path) if File.exist?(path)
  end

end
