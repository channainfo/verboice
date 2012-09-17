class LocalizedResource < ActiveRecord::Base

  belongs_to :resource

  has_one :project, through: :resource

  store :extras, accessors: [:duration, :description, :filename]

  attr_accessible :recorded_audio, :uploaded_audio, :language, :text, :type, :url, :description, :duration, :filename, :extras

  validates_presence_of :language #, :resource

  validates_uniqueness_of :language, :scope => :resource_id

  validates :guid, :presence => true, :uniqueness => { :scope => :resource_id }

  AUDIO_UPLOAD = {:NO_SELECTED => -1, :INVALID => 0, :VALID => 1}
  INVALID_AUDIO_UPLOAD_MSG = "Invalid audio file"

  after_initialize do
    self.guid ||= Guid.new.to_s
  end

  def has_recorded_audio
    self.recorded_audio.present?
  end

  def has_uploaded_audio
    self.uploaded_audio.present?
  end

  def uploaded_status
    return AUDIO_UPLOAD[:NO_SELECTED] if self.filename.nil?
    return AUDIO_UPLOAD[:INVALID] if self.filename == INVALID_AUDIO_UPLOAD_MSG
    AUDIO_UPLOAD[:VALID]
  end

  def as_json options = {}
    super options.merge(:methods => [:type, :has_recorded_audio, :has_uploaded_audio, :uploaded_status, :duration, :description, :filename],
      :except => [:recorded_audio, :uploaded_audio, :extras])
  end

  def play_command_for play_resource_command
    subclass_responsibility
  end

  def capture_resource_for play_resource_command, session
    subclass_responsibility
  end
end
