unit ApiServer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FpHTTPServer, DateUtils, FpJson, JsonParser,
  HttpDefs, Queue, StrUtils, Connections;

const
  API_CONTENT_TYPE = 'application/json';
  API_DEFAULT_ERROR = '{"error": "There are no requests pending"}';
  API_THANK_YOU = '{"message": "thank you"}';
  API_GET_JSON = '{"identifier": "%s", "environment": %s}';
  CODE_NOT_FOUND = 404;
  METHOD_GET = 'GET';
  METHOD_POST = 'POST';
  HTTP_HEADER_PREFIX = 'HTTP_';
  PATH_IDENTIFIER = 'identifier';
  PATH_CODE = 'code';
  PATH_HEADERS = 'headers';
  PATH_BODY = 'body';
  FORMAT_HEADER = '%s: %s';

type

  { TApiServer }

  TApiServer = class(TFPCustomHttpServer)
  private
    procedure Get(var AResponse: TFPHTTPConnectionResponse);
    procedure Post(var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);
    procedure BuildRackHeaders(RequestHeaders: TRequest; var Headers: TJSONObject);
    procedure BuildHTTPHeaders(RequestHeaders: TRequest; var Headers: TJSONObject);
    procedure ProcessPost(var Connection: TPrackConnection; Content: string);
    function BuildHeaders(Connection: TPrackConnection): string;
    function GetHeadersFromApi(Request: TJSONData): string;
  protected
    FQueue: TPrackQueue;
    procedure RequestHandler(Sender: TObject;
      var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);
  public
    constructor Create(ServerAddress: string; ServerPort: word; AQueue: TPrackQueue);
      reintroduce;
    procedure Start;
  end;

implementation

{ TApiServer }

constructor TApiServer.Create(ServerAddress: string; ServerPort: word;
  AQueue: TPrackQueue);
begin
  inherited Create(nil);
  FQueue := AQueue;
  Address := ServerAddress;
  Port := ServerPort;
  Threaded := True;
  OnRequest := @RequestHandler;
end;

procedure TApiServer.Start;
begin
  Active := True;
end;

procedure TApiServer.RequestHandler(Sender: TObject;
  var ARequest: TFPHTTPConnectionRequest; var AResponse: TFPHTTPConnectionResponse);
begin
  AResponse.ContentType := API_CONTENT_TYPE;
  AResponse.Content := API_DEFAULT_ERROR;
  AResponse.Code := CODE_NOT_FOUND;

  Writeln(Format('API: %s %s', [ARequest.Method, ARequest.URI]));

  case ARequest.Method of
    METHOD_GET: Get(AResponse);
    METHOD_POST: Post(ARequest, AResponse);
  end;
end;

procedure TApiServer.Get(var AResponse: TFPHTTPConnectionResponse);
var
  Connection: TPrackConnection;
begin
  FQueue.PendingRequestsEvent.WaitFor(5000);
  Connection := FQueue.Pop(pcsIncoming);
  if not Assigned(Connection) then
    Exit;

  Connection.Status := pcsProcessing;
  Connection.Setup;
  AResponse.Code := 200;
  AResponse.Content := Format(API_GET_JSON, [Connection.Identifier,
    BuildHeaders(Connection)]);
  FQueue.Add(Connection);
end;

function TApiServer.BuildHeaders(Connection: TPrackConnection): string;
var
  Headers: TJSONObject;
begin
  Headers := TJSONObject.Create;
  try
    BuildRackHeaders(Connection.RequestHeaders, Headers);
    BuildHTTPHeaders(Connection.RequestHeaders, Headers);
    Result := Headers.FormatJSON;
    Assert(Result <> '');
  except
    on E: Exception do
      Writeln('TApiServer.BuildHeaders: ', E.Message);
  end;
  FreeAndNil(Headers);
end;

procedure TApiServer.BuildRackHeaders(RequestHeaders: TRequest;
  var Headers: TJSONObject);
var
  ServerName, ServerPort: string;
begin
  ServerName := ExtractDelimited(1, RequestHeaders.Host, [':']);
  ServerPort := ExtractDelimited(2, RequestHeaders.Host, [':']);
  Assert(ServerName <> '');
  Assert(ServerPort <> '');
  Assert(RequestHeaders.Command <> '');

  with Headers do
  begin
    Add('REQUEST_METHOD', Trim(RequestHeaders.Command));
    Add('SCRIPT_NAME', Trim(RequestHeaders.ScriptName));
    Add('PATH_INFO', Trim(RequestHeaders.Uri));
    Add('QUERY_STRING', Trim(RequestHeaders.QueryString));
    Add('SERVER_NAME', Trim(ServerName));
    Add('SERVER_PORT', Trim(ServerPort));
  end;
end;

procedure TApiServer.BuildHTTPHeaders(RequestHeaders: TRequest;
  var Headers: TJSONObject);
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
    Headers.Add(Concat(HTTP_HEADER_PREFIX, UpperCase(FieldName)), FieldValue);
  end;
end;

procedure TApiServer.Post(var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
var
  Connection: TPrackConnection;
  Identifier: string;
  JsonRequest: TJSONData;
begin
  try
    try
      JsonRequest := GetJSON(ARequest.Content);
      Identifier := JsonRequest.FindPath(PATH_IDENTIFIER).AsString;
    except
      Exit;
    end;
  finally
    FreeAndNil(JsonRequest);
  end;

  Connection := FQueue.Pop(pcsProcessing, Identifier);
  if not Assigned(Connection) then
    Exit;

  AResponse.Code := 200;
  AResponse.Content := API_THANK_YOU;

  ProcessPost(Connection, ARequest.Content);
  FQueue.Add(Connection);
  FQueue.ReadyRequestsEvent.SetEvent;
end;

procedure TApiServer.ProcessPost(var Connection: TPrackConnection; Content: string);
var
  Request: TJSONData;
begin
  Connection.Status := pcsReady;
  try
    Request := GetJson(Content);
    Connection.Response.Code := Request.FindPath(PATH_CODE).AsInteger;
    Connection.Response.Body := Request.FindPath(PATH_BODY).AsString;
    Connection.Response.Headers := GetHeadersFromApi(Request);
  except
    Connection.Status := pcsError;
  end;
  FreeAndNil(Request);
end;

function TApiServer.GetHeadersFromApi(Request: TJSONData): string;
var
  I: integer;
begin
  Result := '';
  for I := 0 to Request.FindPath(PATH_HEADERS).Count - 1 do
    with TJSONObject(Request.FindPath(PATH_HEADERS).Items[I]) do
      Result := Concat(Result, Format(FORMAT_HEADER,
        [Names[0], Items[0].AsString]), CRLF);
end;

end.