unit Server;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, fpJSON, jsonparser,
  GatewayServer, ApiServer;

const
  SOCKET_READ_TIMEOUT = 50;
  REQUEST_TIMEOUT_SECS = 30;
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
    FGatewayServer: TServer;
    FApiServer: TApiServer;

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
  FGatewayHost := GatewayHost;
  FGatewayPort := GatewayPort;
  FApiHost := ApiHost;
  FApiPort := ApiPort;
  FGatewayServer := TServer.Create(FGatewayHost, FGatewayPort);
  FApiServer := TApiServer.Create(FApiHost, FApiPort);
end;

procedure TPrack.Start;
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
    CheckSynchronize(1000);
  end;
end;

destructor TPrack.Destroy;
begin
  FreeAndNil(FGatewayServer);
  FreeAndNil(FApiServer);
  Writeln('(╯°□°）╯︵ ┻━┻', CRLF);
  inherited;
end;

end.

