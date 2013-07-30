$ ->
  if $('ul.tabs.top .active').text().trim() == 'Calls'
    $('#call_flow_id').on 'change', -> $('#call-logs-form').submit()
