unit TestPrackServer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, Server;

const
  CRLF = #13#10;

  EXAMPLE_CLIENT_REQUEST =
    'GET /helloWorld HTTP/1.1' + CRLF +
    'Host: 127.0.0.1:8080' + CRLF +
    'User-Agent: curl/7.54.0' + CRLF +
    'Accept: */*';

  EXAMPLE_SERVER_REQUEST =
    'POST /api/v1/request HTTP/1.1' + CRLF +
    'Host: 127.0.0.1:8080' + CRLF +
    'User-Agent: curl/7.54.0' + CRLF +
    'Accept: */*' + CRLF +
    'Content-Type: application/json' + CRLF +
    'Content-Length: 35' + CRLF +
    CRLF +
    '{"username":"xyz","password":"xyz"}';

type

  { TTestPrackServer }

  TTestPrackServer = class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCanCreate;
  end;

var
  PrackServer: TPrackServer;

implementation

procedure TTestPrackServer.SetUp;
begin
  PrackServer := TPrackServer.Create;
end;

procedure TTestPrackServer.TearDown;
begin
  FreeAndNil(PrackServer);
end;

procedure TTestPrackServer.TestCanCreate;
begin
  Assert(Assigned(PrackServer));
end;

initialization

  RegisterTest(TTestPrackServer);
end.

