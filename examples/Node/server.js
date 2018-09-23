var request = require('request');

var url = "http://localhost:4242/api/v1/request"
var request = require('request');

function nextRequest() {
    request(url, function (error, response, body) {
        if (typeof response === 'undefined') {
            setTimeout(nextRequest, 5000);
            return;
        }

        if (response.statusCode !== 200) {
            setTimeout(nextRequest, 0);
            return;
        }

        body = JSON.parse(body);

        request({
            uri: url,
            method: 'POST',
            json: true,
            body: {
                identifier: body.identifier,
                code: 200,
                headers: {
                    "Content-Type": "text/html",
                    "Connection": "close"
                },
                body: Buffer.from("Hello, Node!").toString('base64')
            }
        });

        setTimeout(nextRequest, 0);
    });
}

nextRequest();
