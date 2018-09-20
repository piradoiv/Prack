unit ApiServer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FpHTTPServer, DateUtils,
  FpJson, JsonParser, HttpDefs, Queue, StrUtils, Connections;

type

  { TApiServer }

  TApiServer = class(TFPCustomHttpServer)
  private
    FQueue: TPrackQueue;
    procedure RequestHandler(Sender: TObject;
      var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);
    procedure Get(var ARequest: TFPHTTPConnectionRequest; var AResponse: TFPHTTPConnectionResponse);
    procedure Post(var ARequest: TFPHTTPConnectionRequest; var AResponse: TFPHTTPConnectionResponse);
  public
    constructor Create(ServerAddress: string; ServerPort: word; AQueue: TPrackQueue); reintroduce;
    procedure Start;
  end;

implementation

{ TApiServer }

procedure TApiServer.RequestHandler(Sender: TObject;
  var ARequest: TFPHTTPConnectionRequest; var AResponse: TFPHTTPConnectionResponse);
begin
  case ARequest.Method of
    'GET': Get(ARequest, AResponse);
    'POST': Post(ARequest, AResponse);
  end;
end;

procedure TApiServer.Get(var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
var
  List: TList;
  I, J: Integer;
  Connection: TPrackConnection;
  Headers: TJSONObject;
  HeadersStr: String;
  FieldName, FieldValue: String;
begin
  AResponse.ContentType := 'application/json';
  AResponse.Content := '{"error": "There are no requests pending"}';
  AResponse.Code := 404;

  List := FQueue.LockList;
  try
    for I := 0 to List.Count - 1 do
    begin
      Connection := TPrackConnection(List.Items[I]);
      if Connection.Status <> pcsIncoming then Continue;

      Connection.Setup;
      try
        Headers := TJSONObject.Create;
        Headers.Add('REQUEST_METHOD', Trim(Connection.RequestHeaders.Command));
        Headers.Add('SCRIPT_NAME', Trim(Connection.RequestHeaders.ScriptName));
        Headers.Add('PATH_INFO', Trim(Connection.RequestHeaders.Uri));
        Headers.Add('QUERY_STRING', Trim(Connection.RequestHeaders.QueryString));
        Headers.Add('SERVER_NAME', Trim(ExtractDelimited(1, Connection.RequestHeaders.Host, [':'])));
        Headers.Add('SERVER_PORT', Trim(ExtractDelimited(2, Connection.RequestHeaders.Host, [':'])));
        for J := 0 to Connection.RequestHeaders.FieldCount - 1 do
        begin
          with Connection.RequestHeaders do
          begin
            FieldName := Concat('HTTP_', UpperCase(StringReplace(FieldNames[J], '-', '_', [rfReplaceAll])));
            FieldValue := Trim(FieldValues[J]);
            Headers.Add(FieldName, FieldValue);
          end;
        end;

        HeadersStr := Headers.FormatJSON;

      except
        on E: Exception do Writeln(E.Message);
      end;

      AResponse.Code := 200;
      AResponse.Content := '{"identifier": "' + Connection.Identifier + '", "environment": ' + HeadersStr + '}';

      FreeAndNil(Headers);

      Connection.Status := pcsProcessing;
      Exit;
    end;
  finally
    FQueue.UnlockList;
    List := nil;
    Connection := nil;
  end;
end;

procedure TApiServer.Post(var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
var
  List: TList;
  I, J: Integer;
  Connection: TPrackConnection;
  Identifier: String;
  Code: Integer;
  Headers: String;
  Body: String;
  Header: TJSONObject;
  JsonRequest: TJSONData;
begin
  AResponse.ContentType := 'application/json';
  AResponse.Content := '{"error": "There are no requests pending"}';
  AResponse.Code := 404;

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
      if (Connection.Status <> pcsProcessing) or (Connection.Identifier <> Identifier) then Continue;

      try
        Code := JsonRequest.FindPath('code').AsInteger;
        Headers := '';
        for J := 0 to JsonRequest.FindPath('headers').Count - 1 do
        begin
          Header := TJSONObject(JsonRequest.FindPath('headers').Items[J]);
          Headers := Concat(
            Headers,
            Header.Names[0], ': ',
            Header.Items[0].AsString,
            CRLF
          );
        end;
        Body := JsonRequest.FindPath('body').AsString;
      except
        Writeln('TApiServer.Post: Error parsing the JSON');
        Connection.Status := pcsError;
        Exit;
      end;

      Connection.Status := pcsReady;
      Connection.Response.Code := Code;
      Connection.Response.Headers := Headers;
      Connection.Response.Body := Body;

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

constructor TApiServer.Create(ServerAddress: string; ServerPort: word; AQueue: TPrackQueue);
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

end.

