unit Server;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BlckSock, DateUtils,
  Requests;

const
  SOCKET_READ_TIMEOUT  = 50;
  REQUEST_TIMEOUT_SECS = 30;
  DEFAULT_GATEWAY_HOST = '127.0.0.1';
  DEFAULT_GATEWAY_PORT = 80;
  DEFAULT_API_HOST     = '127.0.0.1';
  DEFAULT_API_PORT     = 4242;

type

  { TPrackServer }

  TPrackServer = class
    private
      FGatewayHost: String;
      FGatewayPort, FAPIPort: Integer;
      FRequestQueue, FResponseQueue: TRequestList;
      FRequestSocket, FAPISocket: TTCPBlockSocket;

      procedure ProcessGatewayRequest;
      procedure ProcessAPIRequest;
      procedure SendPendingResponses;
      procedure CleanRequestQueue;
    public
      constructor Create(
        GatewayHost: String  = DEFAULT_GATEWAY_HOST;
        GatewayPort: Integer = DEFAULT_GATEWAY_PORT;
        APIPort: Integer = DEFAULT_API_PORT
      );
      procedure Start;
      destructor Destroy; override;
  end;

implementation

{ TPrackServer }

procedure TPrackServer.ProcessGatewayRequest;
begin
  if FRequestSocket.CanRead(SOCKET_READ_TIMEOUT)
    then FRequestQueue.Add(TRequest.Create(
      FRequestSocket.Accept,
      FGatewayHost,
      FGatewayPort
    ));
end;

procedure TPrackServer.ProcessAPIRequest;
var
  ApiRequest, GatewayRequest: TRequest;
begin
  if FAPISocket.CanRead(SOCKET_READ_TIMEOUT) then
  begin
    ApiRequest := TRequest.Create(FAPISocket.Accept, FGatewayHost, FAPIPort);

    if FRequestQueue.Count > 0 then
    begin
      GatewayRequest := FRequestQueue.Extract(FRequestQueue.First);
      GatewayRequest.Response :=
        'HTTP/1.1 200 OK' + CRLF +
        'Content-Type: text/html' + CRLF +
        'Content-Length: 4' + CRLF +
        'Connection: close' + CRLF + CRLF +
        'OK' + CRLF;

      GatewayRequest.CanBeSent := True;
      FResponseQueue.Add(GatewayRequest);
    end;

    FreeAndNil(ApiRequest);
  end;
end;

procedure TPrackServer.SendPendingResponses;
var
  Index: Integer;
  RequestToResponse: TRequest;
begin
  if FResponseQueue.Count = 0 then Exit;
  for Index := 0 to FResponseQueue.Count -1 do
  begin
    RequestToResponse := FResponseQueue.Extract(FResponseQueue.First);
    RequestToResponse.Socket.SendString(RequestToResponse.Response);
    FreeAndNil(RequestToResponse);
  end
end;

procedure TPrackServer.CleanRequestQueue;
var
  Index: Integer;
  Age:   Integer;
begin
  if FRequestQueue.Count = 0 then Exit;
  for Index := FRequestQueue.Count - 1 downto 0 do
  begin
    Age := SecondsBetween(Now, FRequestQueue.Items[Index].UpdatedAt);
    if Age < REQUEST_TIMEOUT_SECS then Continue;
    FRequestQueue.Delete(Index);
  end
end;

constructor TPrackServer.Create(GatewayHost: String; GatewayPort: Integer;
  APIPort: Integer);
begin
  FGatewayHost   := GatewayHost;
  FGatewayPort   := GatewayPort;
  FAPIPort       := APIPort;
  FRequestQueue  := TRequestList.Create(True);
  FResponseQueue := TRequestList.Create(True);
  FRequestSocket := TTCPBlockSocket.Create;
  FAPISocket     := TTCPBlockSocket.Create;
end;

procedure TPrackServer.Start;
begin
  FRequestSocket.Bind(FGatewayHost, IntToStr(FGatewayPort));
  FRequestSocket.Listen;

  FAPISocket.Bind(FGatewayHost, IntToStr(FAPIPort));
  FAPISocket.Listen;

  while True do
  begin
    ProcessGatewayRequest;
    ProcessAPIRequest;
    SendPendingResponses;
    CleanRequestQueue;
  end;
end;

destructor TPrackServer.Destroy;
begin
  Writeln(CRLF + '(╯°□°）╯︵ ┻━┻');
  FreeAndNil(FRequestSocket);
  FreeAndNil(FRequestQueue);
  FreeAndNil(FResponseQueue);
  inherited;
end;

end.

