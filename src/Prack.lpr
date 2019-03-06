program Prack;

{$mode objfpc}{$H+}

uses
  Cthreads,
  Cmem,
  BaseUnix,
  Classes,
  SysUtils,
  Server,
  Syncobjs,
  CustApp;

type

  { TConsoleApp }

  TConsoleApp = class(TCustomApplication)
  private
    PrackApp: TPrack;
    GatewayHost, ApiHost: string;
    GatewayPort, ApiPort: integer;
    TerminateEvent: TEventObject;
    procedure PrintHelp;
  protected
    procedure DoRun; override;
  end;

  { TConsoleApp }

  procedure TConsoleApp.PrintHelp;
  begin
    Writeln(Title);
    Writeln('Usage: ', ParamStr(0), ' [arguments]');
    Writeln;
    Writeln('Arguments:');
    Writeln('  --help                                 Prints this help');
    Writeln('  -h <hostname> or --host=<hostname>     Specify gateway''s hostname');
    Writeln('  -p <port> or --port=<port>             Specify gateway''s port');
    Writeln('  --api-host=<hostname>                  Specify API hostname');
    Writeln('  --api-port=<port>                      Specify API port');
    Halt(0);
  end;

  procedure TConsoleApp.DoRun;
  begin
    if HasOption('help') then
      PrintHelp;

    GatewayHost := Server.DEFAULT_GATEWAY_HOST;
    if HasOption('h', 'host') then
      GatewayHost := GetOptionValue('h', 'host');

    GatewayPort := Server.DEFAULT_GATEWAY_PORT;
    if HasOption('p', 'port') then
    begin
      try
        GatewayPort := StrToInt(GetOptionValue('p', 'port'));
      except
        Writeln('Gateway Port must be numeric');
        Halt(1);
      end;
    end;

    ApiHost := Server.DEFAULT_API_HOST;
    if HasOption('api-host') then
      ApiHost := GetOptionValue('api-host');

    APIPort := Server.DEFAULT_API_PORT;
    if HasOption('api-port') then
    begin
      try
        APIPort := StrToInt(GetOptionValue('api-port'));
      except
        Writeln('API Port must be numeric');
        Halt(1);
      end;
    end;

    PrackApp := TPrack.Create(GatewayHost, GatewayPort, ApiHost, ApiPort);
    PrackApp.Start;

    TerminateEvent := TEventObject.Create(nil, True, False, '');
    TerminateEvent.WaitFor(INFINITE);
  end;

var
  ConsoleApp: TConsoleApp;
begin
  ConsoleApp := TConsoleApp.Create(nil);
  ConsoleApp.Title := 'Prack';
  ConsoleApp.Run;
end.
