program Prack;

{$mode objfpc}{$H+}

uses
  Cthreads, Cmem, BaseUnix, Classes, SysUtils, Server, Syncobjs;

var
  App: TPrack;
  GatewayHost, ApiHost: string;
  GatewayPort, ApiPort: integer;
  TerminateEvent: TEventObject;

  procedure SigKillHandler(Sig: longint); cdecl;
  begin
    Write(#8#8, '  ', CRLF, 'Shutting down the server...', CRLF, CRLF);
    App.Active := False;
    TerminateEvent.SetEvent;
  end;

begin
  TerminateEvent := TEventObject.Create(nil, True, False, '');
  FpSignal(SIGINT, @SigKillHandler);

  if ParamStr(1) <> '' then
    GatewayHost := ParamStr(1)
  else
    GatewayHost := Server.DEFAULT_GATEWAY_HOST;

  try
    GatewayPort := StrToInt(ParamStr(2));
  except
    GatewayPort := Server.DEFAULT_GATEWAY_PORT;
  end;

  if ParamStr(3) <> '' then
    ApiHost := ParamStr(3)
  else
    ApiHost := Server.DEFAULT_API_HOST;

  try
    APIPort := StrToInt(ParamStr(4));
  except
    APIPort := Server.DEFAULT_API_PORT;
  end;

  App := TPrack.Create(GatewayHost, GatewayPort, ApiHost, ApiPort);
  App.Start;

  TerminateEvent.WaitFor(INFINITE);
  FreeAndNil(App);
end.
