$(function() {
  // This is to expand shortened text in tables
  $("td").live('click', function() {
    spans = $(this).children("span");
    if (spans.length > 0) {
      $span = $(spans[0]);
      if ($span && $span.attr('title')) {
        $span.text($span.attr('title'));
        $span.attr('title', '');
      }
    }
  });

  // Customize Help link to open in new windows
  $(function(){
    element = $("#NavMenu a").last();
    $(element).attr("target", "_blank");
  })

  // Handle automatic links
  $('.link').live('click', function() {
    window.location = $(this).data('url');
  });

  // Datetime components
  $(".ux-custom-datetimepicker:not([readonly])")
    .click(function(){ $(this).datepicker("show"); })
    .datetimepicker({showButtonPanel: false, dateFormat: 'dd/mm/yy'});

  $(".ux-custom-datepicker:not([readonly])")
    .click(function(){ if(!$(this).is('[readonly]')) {$(this).datepicker("show");} return false; })
    .datepicker({showButtonPanel: false, dateFormat: 'dd/mm/yy'});

});

function create_channel(select) {
  if (!select.value) return;
  window.location = '/channels/new?type=' + select.value.split('-')[0] + '&template=' + select.value.split('-')[1];
  select.value = '';
}

function create_nuntium_channel(select) {
  if (!select.value) return;
  window.location = '/nuntium_channels/new?kind=' + select.value;
  select.value = '';
}

function onWorkflow(callback) {
  $(function() {
    if($('#workflow').length > 0) {
      callback();
    }
  });
}

function onResources(callback) {
  $(function() {
    if($('#resources').length > 0) {
      callback();
    }
  });
}

function remove_fields(link) {
  if(confirm("Column '" +$(link).parent().children()[0].value +"' will be removed when confirmed") == true){
    $(link).prev("input[type=hidden]").val("1");
    $(link).closest(".fields").hide();
  }
}

function remove_contact_group(link){
  if(confirm("Contact group '" +$(link).parent().children()[0].value +"' will be removed when confirmed") == true){
    $(link).prev("input[type=hidden]").val("1");
    $(link).closest(".fields").hide();
  }
}

function add_fields(link, association, content) {
  var new_id = new Date().getTime();
  var regexp = new RegExp("new_" + association, "g");
  $(link).parent().before(content.replace(regexp, new_id));
}

function add_variable(link, association, content) {
  var label_regexp = new RegExp("Value", "g");
  var text_input = $(link).prev("input:text");
  if(text_input.attr('value') === "") {
    text_input.addClass('error');
    return false;
  } else {
    text_input.removeClass('error');
    add_fields(link, association, content.replace(label_regexp, text_input.attr('value')));
    $('.field').last().find('input[type=hidden]').attr('value', text_input.attr('value'));
  }
}

function allowKeyInput(elements, pattern){
  $(elements).controlKeyInput({
    allowChar: pattern,
    allow: function(input, char){
      if(char == "+" && ($.caretPosition(input) !=0 || input.value.indexOf(char) != -1 ))
        return false;
      return true;
    }
  });
}
