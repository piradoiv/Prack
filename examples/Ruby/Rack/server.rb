require 'rack'
require '../Handler/rack/handler/prack.rb'

app = Proc.new do |env|
    ['200', {'Content-Type' => 'text/html'}, ['A barebones rack app.']]
end

Rack::Handler::Prack.run app
