require 'spec_helper'

describe Parsers::UserFlow do
  let(:application) do
    app = mock('application')
    app.stubs(:id).returns 1
    app
  end

  let (:application_flow) do
    [
      {
        'id' => 3,
        'type' => 'menu',
        'name' => 'Menu number one',
        'explanation_message' => { "name" => 'First Menu', 'type' => 'text' },
        'options_message' => {},
        'end_call_message' => {},
        'invalid_message' => {},
        'timeout' => 20,
        'options' =>[
          {
            'description' => 'foobar',
            'number' => 2,
            'next' => 4
          },
          {
            'description' => 'Some other option',
            'number' => 1,
            'next' => 6
          }
        ],
        'next' => 5
      },
      {
        'id' => 4,
        'type' => 'play',
        'name' => 'Say number 4',
        'message' => {
          "name" => "Say 4",
          "type" => "text"
        }
      },
      {
        'id' => 1,
        'root' => true,
        'type' => 'play',
        'name' => 'Play number one',
        'root' => 'true',
        'message' => {
          "name" => "Some explanation message",
          "type" => "recording",
          "file" => "file.wav",
          "duration" => 5
        },
        'next' => 2
      },
      {
        'id' => 2,
        'type' => 'capture',
        'name' => 'Capture number one',
        'instructions_message' => { "name" => 'First Capture', 'type' => 'text' },
        'invalid_message' => {
          "name" => "An invalid key was pressed",
          "type" => "recording",
          "file" => "file.wav",
          "duration" => 5
        },
        'end_call_message' => {
          "name" => "This call will end now",
          "type" => "recording",
          "file" => "file.wav",
          "duration" => 5
        },
        'valid_values' => '1-10',
        'finish_on_key' => '#',
        'min_input_length' => 1,
        'max_input_length' => 10,
        'timeout' => 10,
        'next' => 3
      },
      {
        'id' => 6,
        'type' => 'play',
        'name' => 'Say number 6',
        'message' => {
          "name" => "Say 6",
          "type" => "text"
        }
      },
      {
        'id' => 5,
        'type' => 'play',
        'name' => 'Say number 5',
        'message' => {
          "name" => "Say 5",
          "type" => "text"
        }
      },
      {
        'id' => 27,
        'root' => true,
        'type' => 'play',
        'name' => 'Play number 27',
        'root' => 'true',
        'message' => {
          "name" => "Some explanation message",
          "type" => "recording",
          "file" => "file.wav",
          "duration" => 5
        }
      }
    ]
  end

  it "should retrieve an equivalent flow in verboice internal representation" do
    (Parsers::UserFlow.new application, application_flow).equivalent_flow.should eq([
      Compiler.make do
        Trace application_id: 1, step_id: 1, step_name: 'Play number one', store: '"Message played."'
        PlayFile File.join(Rails.root, "data","applications","1","recordings", "1-message.wav")
        Assign 'attempt_number2', '1'
        While 'attempt_number2 <= 3' do
          Capture say: "First Capture", min: 1, max: 10, finish_on_key: '#', timeout: 10
          If "(digits >= 1 && digits <= 10)" do
            Trace application_id: 1, step_id: 2, step_name: 'Capture number one', store: '"User pressed: " + digits'
            Goto "end2"
          end
          If "digits != null" do
            PlayFile File.join(Rails.root, "data","applications","1","recordings", "2-invalid.wav")
            Trace application_id: 1, step_id: 2, step_name: 'Capture number one', store: '"Invalid key pressed"'
          end
          Else do
            Trace application_id: 1, step_id: 2, step_name: 'Capture number one', store: '"No key was pressed. Timeout."'
          end
          Assign 'attempt_number2', 'attempt_number2 + 1'
        end
        Trace application_id: 1, step_id: 2, step_name: 'Capture number one', store: '"Missed input for 3 times."'
        PlayFile File.join(Rails.root, "data","applications","1","recordings", "2-end_call.wav")
        End()
        Label "end2"
        Say 'First Menu'
        Assign 'attempt_number3', '1'
        While 'attempt_number3 <= 3' do
          Capture min: 1, max: 1, finish_on_key: '#', timeout: 20
          If "digits == 2" do
            Trace application_id: 1, step_id: 3, step_name: 'Menu number one', store: '"User pressed: " + digits'
            Trace application_id: 1, step_id: 4, step_name: 'Say number 4', store: '"Message played."'
            Say "Say 4"
            Goto "end3"
          end
          If "digits == 1" do
            Trace application_id: 1, step_id: 3, step_name: 'Menu number one', store: '"User pressed: " + digits'
            Trace application_id: 1, step_id: 6, step_name: 'Say number 6', store: '"Message played."'
            Say "Say 6"
            Goto "end3"
          end
          If "digits != null" do
            Trace application_id: 1, step_id: 3, step_name: 'Menu number one', store: '"Invalid key pressed"'
          end
          Else do
            Trace application_id: 1, step_id: 3, step_name: 'Menu number one', store: '"No key was pressed. Timeout."'
          end
          Assign 'attempt_number3', 'attempt_number3 + 1'
        end
        Trace application_id: 1, step_id: 3, step_name: 'Menu number one', store: '"Missed input for 3 times."'
        End()
        Label "end3"
        Trace application_id: 1, step_id: 5, step_name: 'Say number 5', store: '"Message played."'
        Say "Say 5"
      end,
      Compiler.make do
        Trace application_id: 1, step_id: 27, step_name: 'Play number 27', store: '"Message played."'
        PlayFile File.join(Rails.root, "data","applications","1","recordings", "27-message.wav")
      end
    ])
  end

  it "should provide a hash of step names and IDs" do
    (Parsers::UserFlow.new application, application_flow).step_names.should eq({
      3  => 'Menu number one',
      4  => 'Say number 4',
      1  => "Play number one",
      2  => "Capture number one",
      6  => "Say number 6",
      5  => "Say number 5",
      27 => "Play number 27"
    })
  end

end
