unit GatewayServer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, HttpDefs, SSockets, Queue, Connections;

type

  { TGatewayServer }

  TGatewayServer = class(TInetServer)
  private
    FQueue: TPrackQueue;
    procedure OnConnectHandler(Sender: TObject; Data: TSocketStream);
  public
    constructor Create(AHost: string; APort: word; AQueue: TPrackQueue);
    procedure Start;
  end;

implementation

{ TGatewayServer }

procedure TGatewayServer.OnConnectHandler(Sender: TObject; Data: TSocketStream);
var
  Connection: TPrackConnection;
begin
  Connection := TPrackConnection.Create;
  Connection.Socket := Data;
  FQueue.Add(Connection);
  FQueue.PendingRequestsEvent.SetEvent;
end;

constructor TGatewayServer.Create(AHost: string; APort: word; AQueue: TPrackQueue);
begin
  FQueue := AQueue;
  OnConnect := @OnConnectHandler;
  inherited Create(AHost, APort);
end;

procedure TGatewayServer.Start;
begin
  StartAccepting;
end;

end.
