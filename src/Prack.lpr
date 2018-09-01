program Prack;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  //Cthreads,
  //Cmem,
  {$ENDIF}
  Classes, SysUtils, BaseUnix,
  Server;

var
  App: TPrackServer;
  Host: String;
  GatewayPort, APIPort: Integer;

procedure SigKillHandler(Sig: Longint); cdecl;
begin
  Sig.ToString;
  FreeAndNil(App);
  Halt(0);
end;

begin
  //FpSignal(SIGINT, @SigKillHandler);

  if ParamStr(1) <> '' then Host := ParamStr(1) else Host := '127.0.0.1';
  try
    GatewayPort := StrToInt(ParamStr(2));
  except
    GatewayPort := 8080;
  end;

  try
    APIPort := StrToInt(ParamStr(3));
  except
    APIPort := 4242;
  end;

  Writeln('Starting public server on http://', Host, ':', IntToStr(GatewayPort),
    '; API Server on http://', Host, ':', APIPort);
  App := TPrackServer.Create(Host, GatewayPort, APIPort);
  App.Start;
end.

