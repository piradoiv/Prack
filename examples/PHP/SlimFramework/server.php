<?php

require 'vendor/autoload.php';
define('API_ENDPOINT', 'http://localhost:4242/api/v1/request');

class PrackMiddleware
{
    private $_response;

    public function __invoke($request, $response, $next)
    {
        $this->_response = $next($request, $response);
        $this->sendResponse();
        return $this->_response;
    }

    private function sendResponse()
    {
        $ch = curl_init();
        curl_setopt($ch, CURLOPT_URL, API_ENDPOINT);
        curl_setopt($ch, CURLOPT_POST, true);
        curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
        curl_setopt($ch, CURLOPT_POSTFIELDS, $this->prepareResponse());
        curl_exec($ch);
    }

    private function prepareResponse()
    {
        $body = $this->getBody();
        return json_encode([
            'identifier' => $_SERVER['PRACK_IDENTIFIER'],
            'code' => $this->_response->getStatusCode(),
            'headers' => (object) [
                (object) ['Content-Type' => 'text/html'],
                (object) ['Content-length' => strlen($body)],
                (object) ['Connection' => 'close'],
            ],
            'body' => $body,
        ]);
    }

    private function getBody()
    {
        ob_start();
        echo $this->_response->getBody();
        return ob_get_clean();
    }
}

while (true) {
    // 1. Gets the next pending request from Prack
    $request = @file_get_contents(API_ENDPOINT);
    if (empty($request)) {
        continue;
    }

    $json = (array) json_decode($request);
    echo "{$json['environment']->REQUEST_METHOD}: {$json['identifier']}\n";

    // 2. We need to prepare the $_SERVER environment so the framework
    //    can understand the request
    $env = (array) $json['environment'];
    foreach ($env as $key => $value) {
        $_SERVER[$key] = $value;
    }
    $_SERVER['REQUEST_URI'] = $env['PATH_INFO'];
    $_SERVER['PRACK_IDENTIFIER'] = $json['identifier'];

    // 3. Run the framework using Prack's Middleware, which sends the
    //    response to Prack's API
    $app = new Slim\App();
    $app->add(new PrackMiddleware());
    $app->get('/', function ($request, $response, $args) {
        $response->write('Welcome to Slim!');
        return $response;
    });

    $app->get('/hello', function ($request, $response, $args) {
        $response->write('Hello, World!');
        return $response;
    });

    ob_start();
    $app->run();
    ob_end_clean();
}

