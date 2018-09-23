# This file is used by Rack-based servers to start the application.
require_relative 'config/environment'
require './../Handler/prack.rb'

Rack::Handler::Prack.run Rails.application
