program Prack;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  Cthreads, Cmem,
  {$ENDIF}
  Classes, SysUtils, BaseUnix,
  PrackServer;

var
  Server: TPrackServer;
  Host: String;
  Port: Integer;

procedure SigKillHandler(Sig : Longint); cdecl;
begin
  FreeAndNil(Server);
  Halt(0);
end;

begin
  FpSignal(SIGINT, @SigKillHandler);

  if ParamStr(1) <> '' then Host := ParamStr(1) else Host := '127.0.0.1';
  try
    Port := StrToInt(ParamStr(2));
  except
    Port := 8080;
  end;

  Writeln('Starting server on http://', Host, ':', IntToStr(Port));
  Server := TPrackServer.Create(Host, Port);
  Server.Start;
end.

