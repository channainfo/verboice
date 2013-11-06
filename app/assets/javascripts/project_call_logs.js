function audio_process(audio_id){
  var audio = $("#" + audio_id)[0];
  audio.paused ? play_audio(audio) : stop_audio(audio);
}

function play_audio(audio) {
  audio.load();
  audio.play();
  $("#link_audio_" + audio.id)[0].className = "fstop"
}

function stop_audio(audio) {
  audio.load();
  audio.pause();
  $("#link_audio_" + audio.id)[0].className = "fplay"
}

function render_pause_image(paused, log_id){
  if(paused){
    $("#link_audio_" + log_id)[0].className = "fstop"
  }
  else{
    $("#link_audio_" + log_id)[0].className = "fplay"
  } 
}

function editAudioAnnotation(audio_id){
  $("#audio_annotation_" + audio_id).show();
}

function saveAudioAnnotation(audio_id){
  var annotation = $("#audio_annotation_" + audio_id + " textarea").val();

  $.ajax({
    url     : "/call_log_recorded_audios/" + audio_id,
    type    : "PUT",
    data    : {
      call_log_recorded_audio : {
        annotation : annotation
      }
    },
    success : saveAudioAnnotationSuccess
  });
}

function saveAudioAnnotationSuccess(data){
  cancelEditAudioAnnotation(data.id);
}

function cancelEditAudioAnnotation(audio_id) {
  var audio = $("#" + audio_id)[0];

  stop_audio(audio);
  $("#audio_annotation_" + audio_id).hide();
}