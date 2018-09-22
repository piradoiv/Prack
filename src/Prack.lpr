program Prack;

{$mode objfpc}{$H+}

uses
  {$IFDEF unix}
  Cthreads,
  //Cmem,
  BaseUnix,
  {$ENDIF}
  Classes, SysUtils, Server, Syncobjs;

var
  App: TPrack;
  GatewayHost, ApiHost: string;
  GatewayPort, ApiPort: integer;
  {$IFDEF UNIX}
  TerminateEvent: TEventObject;

  {$ENDIF}

  procedure SigKillHandler(Sig: longint); cdecl;
  begin
    Write(#8#8, '  ', CRLF, 'Shutting down the server...', CRLF, CRLF);
    App.Active := False;
    TerminateEvent.SetEvent;
  end;

begin
  {$IFDEF UNIX}
  TerminateEvent := TEventObject.Create(nil, True, False, '');
  FpSignal(SIGINT, @SigKillHandler);
  {$ENDIF}

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

  {$IFDEF UNIX}
  TerminateEvent.WaitFor(INFINITE);
  {$ENDIF}

  FreeAndNil(App);
end.

