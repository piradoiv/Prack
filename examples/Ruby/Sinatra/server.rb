require '../Handler/rack/handler/prack.rb'
require 'sinatra'

set :server, :prack
get '/' do
    'Hello, Sinatra!'
end

