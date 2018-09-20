unit Server;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, fpJSON, jsonparser,
  GatewayServer, ApiServer, Queue, Orchestra;

const
  DEFAULT_GATEWAY_HOST = '0.0.0.0';
  DEFAULT_GATEWAY_PORT = 8080;
  DEFAULT_API_HOST = '0.0.0.0';
  DEFAULT_API_PORT = 4242;
  CRLF = #13#10;

type

  { TPrack }

  TPrack = class
  private
    FGatewayHost: string;
    FApiHost: string;
    FGatewayPort: integer;
    FAPIPort: integer;
    FGatewayServer: TGatewayServer;
    FApiServer: TApiServer;
    FQueue: TPrackQueue;
    FOrchestra: TOrchestra;
  public
    Active: boolean;
    constructor Create(GatewayHost: string; GatewayPort: integer;
      ApiHost: string; ApiPort: integer);
    destructor Destroy; override;
    procedure Start;
  end;

implementation

constructor TPrack.Create(GatewayHost: string; GatewayPort: integer;
  ApiHost: string; ApiPort: integer);
begin
  Active := False;
  FQueue := TPrackQueue.Create;

  FOrchestra := TOrchestra.Create(FQueue);

  FGatewayHost := GatewayHost;
  FGatewayPort := GatewayPort;
  FGatewayServer := TGatewayServer.Create(FGatewayHost, FGatewayPort, FQueue);

  FApiHost := ApiHost;
  FApiPort := ApiPort;
  FApiServer := TApiServer.Create(FApiHost, FApiPort, FQueue);
end;

procedure TPrack.Start;
var
  Counter: Integer;
  Face: String;
begin
  Active := True;
  Writeln(CRLF,
    '  ________                    ______', CRLF,
    '  ___  __ \____________ _________  /__', CRLF,
    '  __  /_/ /_  ___/  __ `/  ___/_  //_/', CRLF,
    '  _  ____/_  /   / /_/ // /__ _  ,<', CRLF,
    '  /_/     /_/    \__,_/ \___/ /_/|_| v1.0.0', CRLF);
  Writeln('Public Server listening on http://', FGatewayHost, ':', FGatewayPort);
  Writeln('   API Server listening on http://', FApiHost, ':', FApiPort, CRLF);

  TThread.ExecuteInThread(@FGatewayServer.Start);
  TThread.ExecuteInThread(@FApiServer.Start);

  while Active = True do
  begin
    Counter := FQueue.Count;
    Face := '😀';
    if Counter = 0 then Face := '🎉';
    if Counter >= 5 then Face := '😟';
    if Counter >= 50 then Face := '😰';
    Write(#13, '                                             ', #13,
      '    ', Face, ' ', Counter, ' pending connections');
    Sleep(1000);
  end;
end;

destructor TPrack.Destroy;
begin
  FreeAndNil(FGatewayServer);
  FreeAndNil(FApiServer);
  FreeAndNil(FQueue);
  Writeln('(╯°□°）╯︵ ┻━┻', CRLF);
  inherited;
end;

end.
