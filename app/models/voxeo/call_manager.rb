module Voxeo
  class CallManager
    
    attr_reader :session_id, :channel_id, :caller_id

    def initialize channel_id, session_id = nil, caller_id = nil
      @channel_id = channel_id
      @session_id = session_id
      @caller_id = caller_id
      @builder = Builders::Vxml.new
      @hangup = false
    end

    def answer
    end

    def play(filename, escape_digits = nil)
      return if @hangup
      @builder.play filename
    end
    
    def say(text)
      return if @hangup
      @builder.say text
    end
    
    def capture(options)
      return if @hangup
      
      @builder.capture(options)
      @builder.callback("http://staging.instedd.org:7000/")
      
      flush
      @params[:digits]
    end

    def hangup
      return if @hangup
      
      @builder.hangup

      # Set hangup to true, defer the operation to resume 
      # the fiber so the session can end
      @hangup = true
      current_fiber = Fiber.current
      EM.next_tick { current_fiber.resume }

      flush
    end
    
    def bridge_with(other_session)
      # TODO
    end
    
    def dial(address, options = {})
      # TODO
    end

    def is_answering_machine?
      false
    end

    def sound_path_for(basename)
      Rails.root.join "public", "sounds", "#{basename}.gsm"
    end
    
    private

    def flush
      @params = Fiber.yield @builder.build
    end

  end
end