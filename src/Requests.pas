unit Requests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FGL, StrUtils, fpJSON, Sockets;

const
  CR = #13;
  LF = #10;
  CRLF = CR + LF;

type

  { TEnvItem }

  TEnvItem = class
    private
      FName, FValue: String;
    public
      constructor Create(Name: String; Value: String);
      function    GetName: String;
      function    GetValue: String;
  end;

  { TEnvItemList }

  TCustomEnvItemList = specialize TFPGObjectList<TEnvItem>;
  TEnvItemList = class(TCustomEnvItemList)
    public
      procedure Append(ItemsToAdd: TEnvItemList);
      function  Env(Name: String): String;
  end;

  { TRequestStatus }

  TRequestStatus = (rsIncoming, rsProcessing, rsReady, rsDelivered, rsFailed);

  { TRequest }

  TRequest = class
    private
      function BuildFirstLine(Line: String): TEnvItemList;
      function BuildGlobalEnvItems: TEnvItemList;
      function ExtractEnvItem(Line: String): TEnvItem;
      function BuildHTTPVars(Headers: String): TEnvItemList;
      function BuildEnvironment(HTTPRequest: String): TEnvItemList;

    protected
      function GetHeadersFromRawRequest(HTTPRequest: String): String;
      function GetBodyFromRawRequest(HTTPRequest: String): String;
      function GetNextHeaderLine(LineNumber: Integer; Headers: String): String;
      function GetHeaderNameFromLine(Line: String): String;
      function GetHeaderValueFromLine(const Line: String): String;

    public
      Identifier:  String;
      Message:     String;
      Response:    String;
      ServerHost:  String;
      ServerPort:  Integer;
      CreatedAt:   TDateTime;
      UpdatedAt:   TDateTime;
      Environment: TEnvItemList;
      Status:      TRequestStatus;
      Socket:      TSocket;

      constructor  Create(Host: String; Port: Integer; HTTPRequest: String);
      function     Env(Name: String): String;
      function     ToJSONString: String;
      destructor   Destroy; override;
  end;

  { TRequestList }

  TCustomRequestList = specialize TFPGObjectList<TRequest>;
  TRequestList = class(TCustomRequestList)
    public
      function GetNext: TRequest;
  end;

implementation

{ TRequestList }

function TRequestList.GetNext: TRequest;
var
  Index: Integer;
  Item: TRequest;
begin
  Result := Nil;
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index];
    if Item.Status = rsIncoming then
    begin
      Result := Item;
      Exit;
    end;
  end;
end;

{ TEnvItemList }

procedure TEnvItemList.Append(ItemsToAdd: TEnvItemList);
var
  Index: Integer;
begin
  for Index := 0 to ItemsToAdd.Count - 1 do Self.Add(ItemsToAdd.Items[Index]);
  FreeAndNil(ItemsToAdd);
end;

function TEnvItemList.Env(Name: String): String;
var
  Index: Integer;
begin
  Result := '';
  for Index := 0 to Count - 1 do
  begin
    if Items[Index].FName <> Name then Continue;
    Result := Items[Index].FValue;
    Exit;
  end;
end;

{ TEnvItem }

constructor TEnvItem.Create(Name: String; Value: String);
begin
  Self.FName  := Name;
  Self.FValue := Value;
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

constructor TRequest.Create(Host: String; Port: Integer; HTTPRequest: String);
var
  Guid: TGuid;
begin
  CreateGUID(Guid);

  CreatedAt   := Now;
  Identifier  := GuidToString(Guid);
  Status      := rsIncoming;
  ServerHost  := Host;
  ServerPort  := Port;
  Environment := BuildEnvironment(HTTPRequest);
  UpdatedAt   := Now;
end;

function TRequest.Env(Name: String): String;
begin
  Result := Environment.Env(Name);
end;

function TRequest.ToJSONString: String;
var
  Index: Integer;
  JSON: TJSONObject;
begin
  JSON := TJSONObject.Create;
  JSON.Add('REQUEST_ID', Identifier);
  for Index := 0 to Environment.Count - 1 do
  begin
    JSON.Add(Environment.Items[Index].FName, Environment.Items[Index].FValue);
  end;
  Result := JSON.FormatJSON;
  FreeAndNil(JSON);
end;

function TRequest.BuildEnvironment(HTTPRequest: String): TEnvItemList;
var
  FirstLine, Headers: String;
begin
  Result := TEnvItemList.Create(True);

  FirstLine := ExtractDelimited(1, HTTPRequest, [LF]);
  Headers := GetHeadersFromRawRequest(HTTPRequest);
  Message := GetBodyFromRawRequest(HTTPRequest);;

  Result.Append(BuildFirstLine(FirstLine));
  Result.Append(BuildHTTPVars(Headers));
  Result.Append(BuildGlobalEnvItems);
end;

function TRequest.GetHeadersFromRawRequest(HTTPRequest: String): String;
var
  InitOfHeadersPos: Integer;
  EndOfHeadersPos: Integer;
  Size: Integer;
begin
  InitOfHeadersPos := HTTPRequest.IndexOf(CRLF) + Length(CRLF);
  EndOfHeadersPos := HTTPRequest.IndexOf(CRLF + CRLF);
  Size := EndOfHeadersPos - InitOfHeadersPos;
  Result := HTTPRequest.Substring(InitOfHeadersPos, Size);
end;

function TRequest.GetBodyFromRawRequest(HTTPRequest: String): String;
var
  EndOfHeadersPos: Integer;
begin
  EndOfHeadersPos := HTTPRequest.IndexOf(CRLF + CRLF) + Length(CRLF + CRLF);
  Result := HTTPRequest.Substring(EndOfHeadersPos);
end;

function TRequest.BuildHTTPVars(Headers: String): TEnvItemList;
var
  Line: String;
  LineNumber: Integer;
  Name, Value: String;
begin
  Result := TEnvItemList.Create(False);
  LineNumber := 1;
  Line := GetNextHeaderLine(LineNumber, Headers);
  while Line <> '' do
  begin
    Inc(LineNumber);
    if Line = '' then Continue;
    Name := GetHeaderNameFromLine(Line);
    Value := GetHeaderValueFromLine(Line);
    Result.Add(TEnvItem.Create(Name, Value));
    Line := GetNextHeaderLine(LineNumber, Headers);
  end;
end;

function TRequest.BuildFirstLine(Line: String): TEnvItemList;
var
  Method, URL, ScriptName, Path, Version: String;
begin
  Result := TEnvItemList.Create(False);

  Method := Trim(ExtractDelimited(1, Line, [' ']));
  Result.Add(TEnvItem.Create('REQUEST_METHOD', Method));

  URL := Trim(ExtractDelimited(2, Line, [' ']));
  Result.Add(TEnvItem.Create('REQUEST_URL', URL));

  ScriptName := Trim(ExtractDelimited(2, URL, ['/']));
  if ScriptName <> '' then ScriptName := Concat('/', ScriptName);
  Result.Add(TEnvItem.Create('SCRIPT_NAME', ScriptName));

  Path := Trim(StringReplace(URL, ScriptName, '', []));
  Result.Add(TEnvItem.Create('PATH_INFO', ExtractDelimited(1, Path, ['?'])));
  Result.Add(TEnvItem.Create('QUERY_STRING', ExtractDelimited(2, Path, ['?'])));

  Version := Trim(ExtractDelimited(2, ExtractDelimited(3, Line, [' ']), ['/']));
  Result.Add(TEnvItem.Create('HTTP_VERSION', Version));
end;

function TRequest.BuildGlobalEnvItems: TEnvItemList;
begin
  Result := TEnvItemList.Create(False);
  Result.Add(TEnvItem.Create('SERVER_NAME', ServerHost));
  Result.Add(TEnvItem.Create('SERVER_PORT', IntToStr(ServerPort)));
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

function TRequest.GetHeaderValueFromLine(const Line: String): String;
begin
  Result := Trim(ExtractDelimited(2, Line, [':']));
end;

function TRequest.GetHeaderNameFromLine(Line: String): String;
begin
  Result := Trim(ExtractDelimited(1, Line, [':']));
  Result := Concat('HTTP_', StringReplace(Result, '-', '_', [rfReplaceAll]));
  Result := Uppercase(Result);
end;

function TRequest.GetNextHeaderLine(LineNumber: Integer; Headers: String): String;
begin
  Result := Trim(ExtractDelimited(LineNumber, Headers, [LF]));
end;

destructor TRequest.Destroy;
begin
  FreeAndNil(Socket);
  FreeAndNil(Environment);
  inherited;
end;

end.

