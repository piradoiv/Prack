unit ApiServer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FpHTTPServer, DateUtils,
  FpJson, JsonParser, HttpDefs, Queue;

const
  CRLF = #13#10;

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
  I: Integer;
  Connection: TPrackConnection;
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

      AResponse.Code := 200;
      AResponse.Content := '{"identifier": "' + Connection.Identifier + '"}';
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

