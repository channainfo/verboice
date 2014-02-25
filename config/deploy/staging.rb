set :user, 'ilab'
set :server, "192.168.1.128"
set :branch, 'master'

server "192.168.1.128", :app, :web, :db, primary: true
