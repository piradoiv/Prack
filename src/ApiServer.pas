unit ApiServer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FpHTTPServer, FpJson, JsonParser, Queue,
  Connections, Base64, Headers;

type

  { TApiServer }

  TApiServer = class(TFPCustomHttpServer)
  private
    procedure Get(var AResponse: TFPHTTPConnectionResponse);
    procedure Post(var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);
    procedure ProcessPost(var Connection: TPrackConnection; Content: string);
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

const
  PENDING_REQUESTS_WAIT_LIMIT = 5000;
  API_CONTENT_TYPE = 'application/json';
  API_DEFAULT_ERROR = '{"error": "There are no requests pending"}';
  API_THANK_YOU = '{"message": "thank you"}';
  API_GET_JSON = '{"identifier": "%s", "environment": %s}';
  PATH_IDENTIFIER = 'identifier';
  PATH_CODE = 'code';
  PATH_BODY = 'body';

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
  AResponse.Code := 404;

  Writeln(Format('API: %s %s', [ARequest.Method, ARequest.URI]));

  case ARequest.Method of
    'GET': Get(AResponse);
    'POST': Post(ARequest, AResponse);
  end;
end;

procedure TApiServer.Get(var AResponse: TFPHTTPConnectionResponse);
var
  Connection: TPrackConnection;
begin
  FQueue.PendingRequestsEvent.WaitFor(PENDING_REQUESTS_WAIT_LIMIT);
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
    Connection.Response.Code := StrToInt(Request.FindPath(PATH_CODE).AsString);
    Connection.Response.Body := DecodeStringBase64(Request.FindPath(PATH_BODY).AsString);
    Connection.Response.Headers := GetHeadersFromApi(Request);
  except
    on E: Exception do
    begin
      Connection.Status := pcsError;
      Writeln('TApiServer.ProcessPost: ', E.Message);
    end;
  end;
  FreeAndNil(Request);
end;

end.
