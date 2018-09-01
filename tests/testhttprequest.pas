unit TestHTTPRequest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, Requests;

const
  CRLF = #13#10;

  EXAMPLE_REQUEST =
    'POST /api/v1/request HTTP/1.1' + CRLF +
    'Host: 127.0.0.1:8080' + CRLF +
    'User-Agent: curl/7.54.0' + CRLF +
    'Accept: */*' + CRLF +
    'Content-Type: application/json' + CRLF +
    'Content-Length: 35' + CRLF +
    CRLF +
    '{"username":"xyz","password":"xyz"}';

type

  { TTestableRequest }

  TTestableRequest = class(TRequest)
    public
      function GetHeadersFromRawRequest(HTTPRequest: String): String;
      function GetBodyFromRawRequest(HTTPRequest: String): String;
  end;

  { TTestHTTPRequest }

  TTestHTTPRequest = class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCanBuildEnvironment;
    procedure TestCanGetHeadersFromRawRequest;
    procedure TestCanGetBodyFromRawRequest;
  end;

var
  Request: TTestableRequest;

implementation

{ TTestableRequest }

function TTestableRequest.GetHeadersFromRawRequest(HTTPRequest: String): String;
begin
  Result := inherited;
end;

function TTestableRequest.GetBodyFromRawRequest(HTTPRequest: String): String;
begin
  Result := inherited;
end;

{ TTestHTTPRequest }

procedure TTestHTTPRequest.SetUp;
begin
  Request := TTestableRequest.Create('Test', 42, EXAMPLE_REQUEST);
end;

procedure TTestHTTPRequest.TearDown;
begin
  FreeAndNil(Request);
end;

procedure TTestHTTPRequest.TestCanBuildEnvironment;
begin
  AssertEquals('Test', Request.ServerHost);
  AssertEquals(42, Request.ServerPort);
  AssertEquals(14, Request.Environment.Count);
  AssertEquals('POST', Request.Env('REQUEST_METHOD'));
  AssertEquals('/api/v1/request', Request.Env('REQUEST_URL'));
  AssertEquals('/api', Request.Env('SCRIPT_NAME'));
  AssertEquals('/v1/request', Request.Env('PATH_INFO'));
  AssertEquals('', Request.Env('QUERY_STRING'));
  AssertEquals('curl/7.54.0', Request.Env('HTTP_USER_AGENT'));
  AssertEquals('*/*', Request.Env('HTTP_ACCEPT'));
  AssertEquals('application/json', Request.Env('HTTP_CONTENT_TYPE'));
  AssertEquals('35', Request.Env('HTTP_CONTENT_LENGTH'));
  AssertEquals('1.1', Request.Env('HTTP_VERSION'));
  AssertEquals('Test', Request.Env('SERVER_NAME'));
  AssertEquals('42', Request.Env('SERVER_PORT'));
  AssertEquals('HTTP', Request.Env('rack.url_scheme'));
  AssertEquals('{"username":"xyz","password":"xyz"}', Request.Message);
end;

procedure TTestHTTPRequest.TestCanGetHeadersFromRawRequest;
var
  ExpectedHeaders, ActualHeaders: String;
begin
  ExpectedHeaders :=
    'Host: 127.0.0.1:8080' + CRLF +
    'User-Agent: curl/7.54.0' + CRLF +
    'Accept: */*' + CRLF +
    'Content-Type: application/json' + CRLF +
    'Content-Length: 35';

  ActualHeaders := Request.GetHeadersFromRawRequest(EXAMPLE_REQUEST);
  AssertEquals(ExpectedHeaders, ActualHeaders);
end;

procedure TTestHTTPRequest.TestCanGetBodyFromRawRequest;
var
  ExpectedBody, ActualBody: String;
begin
  ExpectedBody := '{"username":"xyz","password":"xyz"}';
  ActualBody := Request.GetBodyFromRawRequest(EXAMPLE_REQUEST);
  AssertEquals(ExpectedBody, ActualBody);
end;

initialization

  RegisterTest(TTestHTTPRequest);
end.

