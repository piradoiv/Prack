require '../Handler/prack.rb'
require 'sinatra'

set :server, :prack
get '/' do
    'Hello, Sinatra!'
end

