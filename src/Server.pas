unit Server;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BlckSock, DateUtils, fpJSON, jsonparser,
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
        GatewayHost: String = DEFAULT_GATEWAY_HOST;
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
  //if FRequestSocket.CanRead(SOCKET_READ_TIMEOUT)
  //  then FRequestQueue.Add(TRequest.Create(
  //    FRequestSocket.Accept,
  //    FGatewayHost,
  //    FGatewayPort
  //  ));
end;

procedure TPrackServer.ProcessAPIRequest;
var
  ApiRequest, GatewayRequest: TRequest;
  JSON: TJSONData;
  RawRequest: String;
begin
  if FAPISocket.CanRead(SOCKET_READ_TIMEOUT) then
  begin
    Writeln('< Receiving from API Port');
    if FRequestQueue.Count = 0 then Exit;

    try
      ApiRequest := TRequest.Create(FGatewayHost, FAPIPort, RawRequest);
      Writeln('Message: "', ApiRequest.Message, '"');
      Exit;

      if (ApiRequest.Env('REQUEST_METHOD') = 'GET') and
        (ApiRequest.Env('REQUEST_URL') = '/api/v1/request') then
      begin
        Writeln('< API: GET Request');

        GatewayRequest := FRequestQueue.GetNext;
        if not Assigned(GatewayRequest) then Exit;
        GatewayRequest.UpdatedAt := Now;
        GatewayRequest.Status := rsProcessing;

        Writeln('< Sending Request #', GatewayRequest.Identifier);
        Exit;
      end;

      if (ApiRequest.Env('REQUEST_METHOD') = 'POST') and
        (ApiRequest.Env('REQUEST_URL') = '/api/v1/request') then
      begin
        Writeln('< API: POST Request');
        try
          try
            JSON := GetJSON(ApiRequest.Message);
            Writeln('< Got a response for #', JSON.FindPath('REQUEST_ID').AsString);
          except
            Writeln('!!! Error parsing the request');
          end;
        finally
          FreeAndNil(JSON);
        end;
      end;
    finally
      //ApiRequest.Socket.CloseSocket;
      FreeAndNil(ApiRequest);
    end;
  end;
end;

procedure TPrackServer.SendPendingResponses;
var
  RequestToResponse: TRequest;
  ItemsToDestroy: TRequestList;
begin
  ItemsToDestroy := TRequestList.Create(True);

  for RequestToResponse in FResponseQueue do
  begin
    //RequestToResponse.Socket.SendString(RequestToResponse.Response);
    ItemsToDestroy.Add(FResponseQueue.Extract(RequestToResponse));
  end;

  FreeAndNil(ItemsToDestroy);
end;

procedure TPrackServer.CleanRequestQueue;
var
  Request: TRequest;
  ItemsToDestroy: TRequestList;
begin
  ItemsToDestroy := TRequestList.Create(True);
  for Request in FRequestQueue do
  begin
    if SecondsBetween(Now, Request.UpdatedAt) > REQUEST_TIMEOUT_SECS then
    begin
      //Request.Socket.CloseSocket;
      ItemsToDestroy.Add(FRequestQueue.Extract(Request));
    end;
  end;

  FreeAndNil(ItemsToDestroy);
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
  Writeln(CRLF, '(╯°□°）╯︵ ┻━┻');
  FRequestSocket.CloseSocket;
  FAPISocket.CloseSocket;
  inherited;
end;

end.

