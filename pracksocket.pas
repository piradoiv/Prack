unit PrackSocket;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PrackRequests, BlckSock;

{ TSocketCallback }

type
  TSocketCallback = procedure(Request: TRequest) of object;

  { TSocketLoop }

  TSocketLoop = class
    private
      FEnabled: Boolean;
      FSocket: TTCPBlockSocket;
      FCallback: TSocketCallback;
      FHost: String;
      FPort: Integer;
    public
      constructor Create(Host: String; Port: Integer; Callback: TSocketCallback);
      procedure Enable;
      procedure Disable;
      function IsEnabled: Boolean;
  end;

implementation

{ TSocketLoop }

constructor TSocketLoop.Create(Host: String; Port: Integer;
  Callback: TSocketCallback);
begin
  FEnabled := False;
  FHost := Host;
  FPort := Port;
  FSocket := TTCPBlockSocket.Create;
  FCallback := Callback;
  with FSocket do
  begin
    CreateSocket;
    SetLinger(True, 10);
    Bind(Host, IntToStr(Port));
    Listen;
    Assert(SocksLastError >= 0, 'SockLastError must be zero');
  end;
end;

procedure TSocketLoop.Enable;
var
  Request: TRequest;
begin
  Assert(Assigned(FSocket), 'FSocket must be assigned');
  FEnabled := True;
  repeat
    if FSocket.CanRead(1000) then
    begin
      Request := TRequest.Create(FSocket.Accept, FHost, FPort);
      Assert(Request.FSocket.Socket > 0, 'Socket must be greater than zero');
      Assert(Assigned(FCallback), 'FCallback must be assigned');
      FCallback(Request);
    end;
  until FEnabled = False;
end;

procedure TSocketLoop.Disable;
begin
  FEnabled := False;
end;

function TSocketLoop.IsEnabled: Boolean;
begin
  Result := FEnabled;
end;


end.

