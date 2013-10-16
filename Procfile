web: bundle exec rails s
broker: sh -c "cd broker && erl -pa ebin/ deps/*/ebin -boot start_sasl -s main -config verboice -noinput +Bd"
delayed: bundle exec rake jobs:work
