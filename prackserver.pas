unit PrackServer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BlckSock,
  PrackRequests, PrackResponses, PrackSocket;

type

  { TPrackServer }

  TPrackServer = class
    private
      FHost: String;
      FPort: Integer;
      FRequestList: TRequestList;
      FSocket: TTCPBlockSocket;

      FResponsesThread: TPrackResponsesThread;

      procedure StartResponsesThread;
      procedure StartGatewayListenLoop;
      procedure HandleGatewayConnection(Request: TRequest);
    public
      constructor Create(Host: String = '127.0.0.1'; Port: Integer = 80);
      procedure Start;
      destructor Destroy; override;
  end;

implementation

{ TPrackServer }

constructor TPrackServer.Create(Host: String; Port: Integer);
begin
  FHost := Host;
  FPort := Port;
  FRequestList := TRequestList.Create(True);
  FSocket := TTCPBlockSocket.Create;
end;

procedure TPrackServer.StartResponsesThread;
begin
  FResponsesThread := TPrackResponsesThread.Create(True);
  with FResponsesThread do
  begin
    SetRequestList(@FRequestList);
    FreeOnTerminate := False;
    Start;
  end;
end;

procedure TPrackServer.StartGatewayListenLoop;
var
  FSocketLoop: TSocketLoop;
begin
  FSocketLoop := TSocketLoop.Create(FHost, FPort, @HandleGatewayConnection);
  FSocketLoop.Enable;
end;

procedure TPrackServer.HandleGatewayConnection(Request: TRequest);
begin
  FRequestList.Add(Request);
  FResponsesThread.Start;
end;

procedure TPrackServer.Start;
begin
  StartResponsesThread;
  StartGatewayListenLoop;
end;

destructor TPrackServer.Destroy;
begin
  FreeAndNil(FSocket);
  Writeln(CRLF + '(╯°□°）╯︵ ┻━┻');
  inherited;
end;

end.

