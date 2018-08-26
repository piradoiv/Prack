unit PrackRequests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FGL, BlckSock, Sockets, StrUtils;

type
  { TEnvItem }

  TEnvItem = class
    private
      FName: String;
      FValue: String;
    public
      constructor Create(Name: String; Value: String);
      function GetName: String;
      function GetValue: String;
  end;

  { TEnvItemList }

  TEnvItemList = specialize TFPGObjectList<TEnvItem>;

  { TRequest }

  TRequest = class
    private
      FServerName: String;
      FServerPort: Integer;
      function BuildEnvironment: TEnvItemList;
      function BuildFirstLine(Line: String): TEnvItemList;
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

{ TEnvItem }

constructor TEnvItem.Create(Name: String; Value: String);
begin
  FName := Name;
  FValue := Value;
end;

function TEnvItem.GetName: String;
begin
  Result := FName;
end;

function TEnvItem.GetValue: String;
begin
  Result := FValue;
end;

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
  S: String;
  FirstLineItems: TEnvItemList;
  Item: TEnvItem;
begin
  Result := TEnvItemList.Create;
  LineNumber := 0;
  repeat
    Inc(LineNumber);
    Line := FSocket.RecvString(100);
    if Line = '' then Continue;

    if LineNumber = 1 then
    begin
      FirstLineItems := BuildFirstLine(Line);
      for Item in FirstLineItems do
      begin
        Result.Add(Item);
      end;
      FreeAndNil(FirstLineItems);
      Continue;
    end;

    S := Concat('HTTP_', UpperCase(ExtractDelimited(1, Line, [':'])));
    S := StringReplace(S, ' ', '_', [rfReplaceAll]);
    S := StringReplace(S, '-', '_', [rfReplaceAll]);
    Result.Add(TEnvItem.Create(S, Trim(ExtractDelimited(2, Line, [':']))));
  until Line = '';

  Result.Add(TEnvItem.Create('SERVER_NAME', FServerName));
  Result.Add(TEnvItem.Create('SERVER_PORT', IntToStr(FServerPort)));
end;

function TRequest.BuildFirstLine(Line: String): TEnvItemList;
var
  URL, S: String;
begin
  Result := TEnvItemList.Create(False);
  Result.Add(TEnvItem.Create('REQUEST_METHOD', ExtractDelimited(1, Line, [' '])));
  URL := ExtractDelimited(2, Line, [' ']);
  Result.Add(TEnvItem.Create('REQUEST_URL', URL));
  S := ExtractDelimited(2, URL, ['/']);
  Result.Add(TEnvItem.Create('SCRIPT_NAME', S));

  S := StringReplace(URL, '/' + S, '', []);
  Result.Add(TEnvItem.Create('PATH_INFO', ExtractDelimited(1, S, ['?'])));
  Result.Add(TEnvItem.Create('QUERY_STRING', ExtractDelimited(2, S, ['?'])));
  Result.Add(TEnvItem.Create(
    'HTTP_VERSION',
    ExtractDelimited(2, ExtractDelimited(3, Line, [' ']), ['/']))
  );
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

