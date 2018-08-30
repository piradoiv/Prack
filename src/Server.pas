unit Server;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BlckSock,
  Requests;

type

  { TPrackServer }

  TPrackServer = class
    private
      FHost: String;
      FGatewayPort: Integer;
      FAPIPort: Integer;
      FRequestList: TRequestList;
      FRequestSocket: TTCPBlockSocket;
      FAPISocket: TTCPBlockSocket;
    public
      constructor Create(Host: String = '127.0.0.1'; GatewayPort: Integer = 80;
        APIPort: Integer = 4242);
      procedure Start;
      destructor Destroy; override;
  end;

  { TSocketCallback }

  TSocketCallback = procedure(Request: TRequest) of object;

  { TSocketLoop }

  TSocketLoop = class(TThread)
    private
      FHost: String;
      FPort: Integer;
      FCallback: TSocketCallback;
    public
      constructor Create(Host: String; Port: Integer; Callback: TSocketCallback);
      procedure Execute; override;
      procedure Enable;
  end;

implementation

{ TSocketLoop }

constructor TSocketLoop.Create(Host: String; Port: Integer; Callback: TSocketCallback);
begin
  FHost := Host;
  FPort := Port;
  FCallback := Callback;
end;

procedure TSocketLoop.Execute;
begin
  Writeln('To do!');
end;

procedure TSocketLoop.Enable;
begin

end;

{ TPrackServer }

constructor TPrackServer.Create(Host: String; GatewayPort, APIPort: Integer);
begin
  FHost := Host;
  FGatewayPort := GatewayPort;
  FAPIPort := APIPort;
  FRequestList := TRequestList.Create(True);
  FRequestSocket := TTCPBlockSocket.Create;
  FAPISocket := TTCPBlockSocket.Create;
end;

procedure TPrackServer.Start;
var
  GatewayRequest, ApiRequest: TRequest;
begin
  FRequestSocket.Bind(FHost, IntToStr(FGatewayPort));
  FRequestSocket.Listen;

  FAPISocket.Bind(FHost, IntToStr(FAPIPort));
  FAPISocket.Listen;

  while True do
  begin
    if FRequestSocket.CanRead(100) then
    begin
      FRequestList.Add(TRequest.Create(FRequestSocket.Accept, FHost, FGatewayPort));
    end;

    if FAPISocket.CanRead(100) then
    begin
      ApiRequest := TRequest.Create(FAPISocket.Accept, FHost, FAPIPort);
      GatewayRequest := FRequestList.First;
      GatewayRequest.GetResponse.WriteString(
        'HTTP/1.1 200 OK' + CRLF +
        'Content-Type: text/html' + CRLF +
        'Content-Length: 3' + CRLF +
        'Connection: close' + CRLF + CRLF +
        'OK' + CRLF
      );

      GatewayRequest.ShipIt;
      FreeAndNil(ApiRequest);
    end;

    for GatewayRequest in FRequestList do
    begin
      if GatewayRequest.CanBeSent = False then Continue;
      Assert(GatewayRequest.GetResponse.Size > 1, 'Response must be something');
      Writeln(GatewayRequest.GetResponse.ReadString(GatewayRequest.GetResponse.Size));
      GatewayRequest.GetSocket.SendString(GatewayRequest.GetResponse.ReadString(GatewayRequest.GetResponse.Size));
      GatewayRequest.GetSocket.CloseSocket;
      FRequestList.Extract(GatewayRequest).Free;
    end;
  end;
end;

destructor TPrackServer.Destroy;
begin
  FreeAndNil(FRequestSocket);
  Writeln(CRLF + '(╯°□°）╯︵ ┻━┻');
  inherited;
end;

end.

