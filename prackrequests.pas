unit PrackRequests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FGL, BlckSock, Sockets, StrUtils, Prack;

type

  { TRequest }

  TRequest = class
    private
      FServerName: String;
      FServerPort: Integer;
      function BuildEnvironment: TEnvItemList;
      function BuildFirstLine(Line: String): TEnvItemList;
      function BuildGlobalEnvItems: TEnvItemList;
      function ExtractEnvItem(Line: String): TEnvItem;
    public
      Identifier: String;
      FSocket: TTCPBlockSocket;
      Message: String;
      CreatedAt: TDateTime;
      Environment: TEnvItemList;

      constructor Create(Socket: TSocket; ServerName: String; ServerPort: Integer);
      procedure ExampleHandler;
      destructor Destroy; override;
  end;

{ TRequestList }

TRequestList = specialize TFPGObjectList<TRequest>;
PRequestList = ^TRequestList;

implementation

{ TRequest }

constructor TRequest.Create(Socket: TSocket; ServerName: String; ServerPort: Integer);
var
  Guid: TGuid;
  EnvItem: TEnvItem;
begin
  Assert(Socket > 0, 'Socket seems to be invalid');
  CreatedAt := Now;
  CreateGUID(Guid);
  Identifier := GuidToString(Guid);
  FSocket := TTCPBlockSocket.Create;
  FSocket.Socket := Socket;
  FServerName := ServerName;
  FServerPort := ServerPort;
  Assert(FSocket.Socket > 0, 'FSocket.Socket must be assigned');
  Environment := BuildEnvironment;
  Writeln('New Request: ' + Identifier);
  for EnvItem in Environment do
  begin
    Writeln(EnvItem.GetName, ' -> ', EnvItem.GetValue);
  end;
end;

function TRequest.BuildEnvironment: TEnvItemList;
var
  Line: String;
  LineNumber: Integer;
begin
  Result := TEnvItemList.Create;
  LineNumber := 0;
  repeat
    Inc(LineNumber);
    Line := FSocket.RecvString(100);
    if Line = '' then Continue;
    if LineNumber = 1 then
      Result.Append(BuildFirstLine(Line))
    else
      Result.Add(ExtractEnvItem(Line));
  until Line = '';

  Result.Append(BuildGlobalEnvItems);
end;

function TRequest.BuildFirstLine(Line: String): TEnvItemList;
var
  Method, URL, ScriptName, Path, Version: String;
begin
  Result := TEnvItemList.Create(False);
  Method := ExtractDelimited(1, Line, [' ']);
  Result.Add(TEnvItem.Create('REQUEST_METHOD', Method));

  URL := ExtractDelimited(2, Line, [' ']);
  Result.Add(TEnvItem.Create('REQUEST_URL', URL));

  ScriptName := ExtractDelimited(2, URL, ['/']);
  if ScriptName <> '' then ScriptName := Concat('/', ScriptName);
  Result.Add(TEnvItem.Create('SCRIPT_NAME', ScriptName));

  Path := StringReplace(URL, ScriptName, '', []);
  Result.Add(TEnvItem.Create('PATH_INFO', ExtractDelimited(1, Path, ['?'])));
  Result.Add(TEnvItem.Create('QUERY_STRING', ExtractDelimited(2, Path, ['?'])));

  Version := ExtractDelimited(2, ExtractDelimited(3, Line, [' ']), ['/']);
  Result.Add(TEnvItem.Create('HTTP_VERSION', Version));
end;

function TRequest.BuildGlobalEnvItems: TEnvItemList;
begin
  Result := TEnvItemList.Create(False);
  Result.Add(TEnvItem.Create('SERVER_NAME', FServerName));
  Result.Add(TEnvItem.Create('SERVER_PORT', IntToStr(FServerPort)));
  Result.Add(TEnvItem.Create('rack.url_scheme', 'HTTP'));
end;

function TRequest.ExtractEnvItem(Line: String): TEnvItem;
var
  S: String;
begin
  S := Concat('HTTP_', UpperCase(ExtractDelimited(1, Line, [':'])));
  S := StringReplace(S, ' ', '_', [rfReplaceAll]);
  S := StringReplace(S, '-', '_', [rfReplaceAll]);
  Result := TEnvItem.Create(S, Trim(ExtractDelimited(2, Line, [':'])));
end;

procedure TRequest.ExampleHandler;
begin
  Assert(Assigned(FSocket), 'FSocket must be assigned');
  Assert(FSocket.Socket > 0, 'FSocket.Socket must be a valid socket');
  with FSocket do
  begin
    SendString('HTTP/1.1 200 OK' + CRLF);
    SendString('Connection: close' + CRLF);
    SendString('Content-Type: text/html' + CRLF);
    SendString('Content-Length: 4' + CRLF);
    SendString(CRLF);
    SendString('OK' + CRLF);
  end;
end;

destructor TRequest.Destroy;
begin
  FSocket.CloseSocket;
  FreeAndNil(FSocket);
  FreeAndNil(Environment);
  inherited;
end;

end.

