unit ApiServer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FpHTTPServer, DateUtils,
  FpJson, JsonParser, HttpDefs;

type

  { TApiServer }

  TApiServer = class(TFPCustomHttpServer)
  private
    procedure RequestHandler(Sender: TObject;
      var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);
  public
    constructor Create(ServerAddress: string; ServerPort: word);
    procedure Start;
  end;

implementation

{ TApiServer }

procedure TApiServer.RequestHandler(Sender: TObject;
  var ARequest: TFPHTTPConnectionRequest; var AResponse: TFPHTTPConnectionResponse);

var
  GatewayResponse: TFPHTTPConnectionResponse;
  JsonResponse: TJSONData;
  Message: string;

begin
  AResponse.ContentType := 'application/json';
  AResponse.Content := '{"error": "There are no requests pending"}';
  AResponse.Code := 404;

  if ARequest.Method = 'POST' then
  begin
    Message := '';

    try
      JsonResponse := GetJson(ARequest.Content);
      Message := JsonResponse.FindPath('response').AsString;
    except
      on E: Exception do
      begin
        Writeln('ApiServer.pas:RequestHandler: ', E.Message);
        //FQueue.WillDeliver(PrackRequest.Identifier);
        Exit;
      end;
    end;

    Writeln('Dispatching: ', Message);

    //FQueue.WillDeliver(PrackRequest.Identifier);
    AResponse.Code := 200;
    AResponse.Content := '{"status": "ok"}';
    Exit;
  end;

  //PrackRequest := FQueue.GetFirstPending;

  AResponse.Code := 200;
  //FQueue.WillProcess(PrackRequest.Identifier);
end;

constructor TApiServer.Create(ServerAddress: string; ServerPort: word);
begin
  inherited Create(nil);
  Address := ServerAddress;
  Port := ServerPort;
  Threaded := True;
  OnRequest := @RequestHandler;
end;

procedure TApiServer.Start;
begin
  Active := True;
end;

end.

