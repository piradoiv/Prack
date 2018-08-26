program PrackServerApp;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  Cthreads, Cmem,
  {$ENDIF}
  Classes, SysUtils, BaseUnix,
  PrackCommon, PrackServer, PrackRequests, PrackResponses;

var
  Server: TPrackServer;

procedure SigKillHandler(Sig : Longint); cdecl;
begin
  FreeAndNil(Server);
  Halt(0);
end;

begin
  FpSignal(SIGINT, @SigKillHandler);
  Server := TPrackServer.Create('127.0.0.1', 8080);
  Server.Start;
end.

