# Prack
The scalable Web Server for humans.

## Project goals
- Small memory footprint, blazing fast multi-threaded Web Server
- [Rack-like interface](https://www.rubydoc.info/github/rack/rack/master/file/SPEC), so developers can focus on writing Web Frameworks, API, or Web pages without dealing with HTTP internals
- A design that eliminates or reduces Service Discovery or Orchestration systems needings, but still being capable to scale
- Zero-downtime deployments, without sorcery

## Background
Until now, Web Servers were used to receive HTTP requests and pass them directly to Web Apps, using Load Balancers. This requires a lot of effort to planify your infrastructure, as the Web Server needs to know what backends can receive these requests, or the host where they are listening.

There is an overwhelming amount of puzzle pieces to deal with these individual issues, like Consul, HAProxy, Nomad or Fabio. And thats just fine, they're amazing tools. You can even use Kubernetes on bigger applications if you don't want to handle these puzzle pieces one by one.

The problem is, on small to medium sized Websites, these tools feels like trying to catch flies using the whole Death Star.

Prack's design is completely different. Instead, these Servers must contact with Prack, fetch the pending Requests, and send back the HTTP Response, using the REST API. This design just eliminates the needing of Health Checks, Load Balancing or Service Discovery.

## How Prack works
With Prack, you can just start the server, and it will start listening for HTTP Requests, storing them into a Requests Queue. These Requests can be handled using a REST API, exposed on a different port.

Basically you have:    
GET /api/v1/request    
POST /api/v1/response    

Getting a Request will give you a JSON populated with the Environment variables you are used on any Rack-like Application, like the `REQUEST_METHOD` or the `PATH_INFO`.

Sending back Responses is being done by POSTing a JSON with Rack compatible contents:
- The HTTP Response Code
- A bunch of Response Headers
- The response body

![Prack Design](https://raw.githubusercontent.com/piradoiv/Prack/master/img/Prack.png)

## How to use Prack with your App Server

Prack is programming language agnostic, you can build Websites or APIs in your favorite one.

Usage examples:
- [Go](https://github.com/piradoiv/prack-go)
- [Node](https://github.com/piradoiv/prack-node-examples)
- [PHP](https://github.com/piradoiv/prack-php-examples)
- [Ruby](https://github.com/piradoiv/prack-ruby-examples)

## Is it fast and lightweight?
It is! but just in theory. I didn't have a chance to proper benchmarks yet.

Prack barely consumes a few megabytes while waiting for connections, and the threads are always sleeping until needed. While the server is multi-thread, it doesn't means it actually uses one thread per connection, so the memory per request consuption is really low (again, in theory).

## What's the status of the project?
Proof of concept, not production ready. It's still under heavy development.

## v1.0.0 Roadmap
- Proof of concept
  - Standalone HTTP Web Server (In progress)
  - HTTP Requests queue (In progress)
  - HTTP Responses queue (In progress)
  - Private API in a different port for external processes (In progress)
- Website and extensive documentation, with a good amount of examples
- Security

## How to build:
`lazbuild PrackServerApp.lpr`
