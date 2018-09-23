require 'rack/handler'
require 'net/http'
require 'uri'
require 'json'
require 'base64'

module Rack
    module Handler
        module Prack
            def self.run(app, options = nil)
                while true
                    serve app
                end
            end

            def self.serve(app)
                env = ENV.to_hash
                begin
                    request_contents = Net::HTTP.get(URI.parse(
                        'http://localhost:4242/api/v1/request'
                    ))
                rescue
                    return
                end

                request = JSON.parse(request_contents)
                if request['error']
                    return
                end

                request['environment'].each do |k,v|
                    env[k] = v
                end

                env['rack.input'] = StringIO.new('')
                status, headers, body = app.call(env)

                body_string = ''
                body.each do |line|
                    body_string = body_string + line
                end

                body_string = Base64.encode64(body_string);
                response_contents = JSON.generate({
                    :identifier => request['identifier'],
                    :code => status,
                    :headers => headers,
                    :body => body_string
                })

                uri = URI.parse('http://localhost:4242/api/v1/request')
                http = Net::HTTP.new(uri.host, uri.port)
                req = Net::HTTP::Post.new(uri.request_uri, {'Content-Type': 'application/json'})
                req.body = response_contents
                http.request(req)
            end
        end

        register :prack, Prack
    end
end
