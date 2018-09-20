unit ApiServer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FpHTTPServer, DateUtils,
  FpJson, JsonParser, HttpDefs, Queue, StrUtils, Connections;

const
  API_GET_JSON = '{"identifier": "%s", "environment": %s}';

type

  { TApiServer }

  TApiServer = class(TFPCustomHttpServer)
  private
    procedure Get(var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);
    procedure Post(var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);
    procedure BuildRackHeaders(RequestHeaders: TRequest; var Headers: TJSONObject);
    procedure BuildHTTPHeaders(RequestHeaders: TRequest; var Headers: TJSONObject);
    function BuildHeaders(Connection: TPrackConnection): string;
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
  Threaded := False;
  OnRequest := @RequestHandler;
end;

procedure TApiServer.Start;
begin
  Active := True;
end;

procedure TApiServer.RequestHandler(Sender: TObject;
  var ARequest: TFPHTTPConnectionRequest; var AResponse: TFPHTTPConnectionResponse);
begin
  AResponse.ContentType := 'application/json';
  AResponse.Content := '{"error": "There are no requests pending"}';
  AResponse.Code := 404;

  case ARequest.Method of
    'GET': Get(ARequest, AResponse);
    'POST': Post(ARequest, AResponse);
  end;
end;

procedure TApiServer.Get(var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
var
  Connection: TPrackConnection;
begin
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
    Headers.Add(Concat('HTTP_', UpperCase(FieldName)), FieldValue);
  end;
end;

procedure TApiServer.Post(var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
var
  List: TList;
  I, J: integer;
  Connection: TPrackConnection;
  Identifier: string;
  Code: integer;
  Headers: string;
  Body: string;
  Header: TJSONObject;
  JsonRequest: TJSONData;
begin
  try
    JsonRequest := GetJSON(ARequest.Content);
    Identifier := JsonRequest.FindPath('identifier').AsString;
  except
    Exit;
  end;

  List := FQueue.LockList;
  try
    for I := 0 to List.Count - 1 do
    begin
      Connection := TPrackConnection(List.Items[I]);
      if (Connection.Status <> pcsProcessing) or
        (Connection.Identifier <> Identifier) then
        Continue;

      try
        Code := JsonRequest.FindPath('code').AsInteger;
        Headers := '';
        for J := 0 to JsonRequest.FindPath('headers').Count - 1 do
        begin
          Header := TJSONObject(JsonRequest.FindPath('headers').Items[J]);
          Headers := Concat(Headers, Header.Names[0], ': ',
            Header.Items[0].AsString, CRLF);
        end;
        Body := JsonRequest.FindPath('body').AsString;
      except
        Writeln('TApiServer.Post: Error parsing the JSON');
        Connection.Status := pcsError;
        Exit;
      end;

      with Connection do
      begin
        Status := pcsReady;
        Response.Code := Code;
        Response.Headers := Headers;
        Response.Body := Body;
      end;

      AResponse.Code := 200;
      AResponse.Content := '{"message": "thank you"}';
      Exit;
    end;
  finally
    FQueue.UnlockList;
    FQueue.Event.SetEvent;
    FreeAndNil(Header);
    FreeAndNil(JsonRequest);
  end;
end;

end.
