<?php
namespace Prack;

class Slim
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
            'headers' => [
                'Content-Type' => 'text/html',
                'Content-length' => strlen($body),
                'Connection' => 'close',
            ],
            'body' => base64_encode($body),
        ]);
    }

    private function getBody()
    {
        ob_start();
        echo $this->_response->getBody();
        return ob_get_clean();
    }
}


