unit Headers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Connections, HttpDefs, fpJson, JsonParser, StrUtils;

function BuildHeaders(Connection: TPrackConnection): string;
function GetHeadersFromApi(Request: TJSONData): string;

implementation

const
  PATH_HEADERS = 'headers';
  FORMAT_HEADER = '%s: %s';

procedure BuildRackHeaders(RequestHeaders: TRequest; var Headers: TJSONObject);
var
  ServerName, ServerPort: string;
begin
  ServerName := ExtractDelimited(1, RequestHeaders.Host, [':']);
  ServerPort := ExtractDelimited(2, RequestHeaders.Host, [':']);
  with Headers do
  begin
    Add('REQUEST_METHOD', Trim(RequestHeaders.Command));
    Add('SCRIPT_NAME', Trim(RequestHeaders.ScriptName));
    Add('PATH_INFO', Trim(RequestHeaders.URI));
    Add('QUERY_STRING', Trim(RequestHeaders.QueryString));
    Add('SERVER_NAME', Trim(ServerName));
    Add('SERVER_PORT', Trim(ServerPort));
  end;
end;

procedure BuildHTTPHeaders(RequestHeaders: TRequest; var Headers: TJSONObject);
var
  I: integer;
  FieldName, FieldValue: string;
begin
  // TODO: FieldCount, FieldNames and FieldValues has been deprecated
  for I := 0 to RequestHeaders.FieldCount - 1 do
  begin
    FieldName := RequestHeaders.FieldNames[I];
    FieldName := StringReplace(FieldName, '-', '_', [rfReplaceAll]);
    FieldValue := Trim(RequestHeaders.FieldValues[I]);
    Headers.Add(Concat('HTTP_', UpperCase(FieldName)), FieldValue);
  end;
end;

function BuildHeaders(Connection: TPrackConnection): string;
var
  Headers: TJSONObject;
begin
  Headers := TJSONObject.Create;
  try
    BuildRackHeaders(Connection.Request, Headers);
    BuildHTTPHeaders(Connection.Request, Headers);
    Result := Headers.FormatJSON;
    Assert(Result <> '');
  except
    on E: Exception do
      Writeln('TApiServer.BuildHeaders: ', E.Message);
  end;
  FreeAndNil(Headers);
end;

function GetHeadersFromApi(Request: TJSONData): string;
var
  I: integer;
  Key, Value: string;
begin
  Result := '';
  try
    for I := 0 to Request.FindPath(PATH_HEADERS).Count - 1 do
    begin
      Key := TJSONObject(Request.FindPath(PATH_HEADERS)).Names[I];
      Value := Request.FindPath(PATH_HEADERS).Items[I].AsString;
      Result := Concat(Result, Format(FORMAT_HEADER, [Key, Value]), CRLF);
    end;
  except
    on E: Exception do
      Writeln('GetHeadersFromApi: ', E.Message);
  end;
end;

end.

