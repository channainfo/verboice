class CallbackCommand
  def initialize(url = nil)
    @url = url
  end

  def run(session)
    @url = session.application.callback_url unless @url

    http = EventMachine::HttpRequest.new(@url).post :body => "CallSid=#{session.id}&Digits=#{session[:last_capture]}"

    f = Fiber.current
    http.callback do
      body = http.response
      commands = XmlParser.parse body
      session.push_commands commands

      f.resume
    end
    http.errback do
      puts "Error getting #{@url}"
      f.resume
    end
    Fiber.yield
  end
end
