unit Connections;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SSockets, HTTPDefs, FPHTTPServer;

const
  CRLF = #13#10;

type
  { TPrackConnectionStatus }

  TPrackConnectionStatus = (pcsIncoming, pcsProcessing, pcsReady, pcsError);

  { TPrackResponse }

  TPrackResponse = record
    Code: integer;
    Headers: string;
    Body: string;
  end;

  { TPrackConnection }

  TPrackConnection = class
  public
    Identifier: string;
    CreatedAt: TDateTime;
    Status: TPrackConnectionStatus;
    Socket: TSocketStream;
    Request: TRequest;
    Response: TPrackResponse;
    constructor Create(Data: TSocketStream);
    destructor Destroy; override;
    procedure SetErrorResponse(Code: integer; Body: string);
    procedure SendResponse;
    procedure Setup;
  end;

implementation

{ TPrackConnection }

constructor TPrackConnection.Create(Data: TSocketStream);
var
  GUID: TGUID;
begin
  CreatedAt := Now;
  CreateGuid(GUID);
  Identifier := GuidToString(GUID);
  Status := pcsIncoming;
  Socket := Data;
end;

destructor TPrackConnection.Destroy;
begin
  FreeAndNil(Socket);
  FreeAndNil(Request);
  inherited Destroy;
end;

procedure TPrackConnection.SetErrorResponse(Code: integer; Body: string);
begin
  Response.Code := Code;
  Response.Body := Body;
  Status := pcsReady;
end;

procedure TPrackConnection.SendResponse;
var
  Http: string;
  ResponseStream: TStringStream;
begin
  try
    try
      Http := Format('HTTP/1.1 %s %s', [IntToStr(Response.Code),
        GetStatusCode(Response.Code)]);
      ResponseStream := TStringStream.Create(Concat(
        Http, CRLF, Response.Headers, CRLF, Response.Body));
      Socket.CopyFrom(ResponseStream, ResponseStream.Size);
    except
      on E: Exception do
        Writeln('TPrackConnection.SendResponse: ', E.Message);
    end;
  finally
    FreeAndNil(ResponseStream);
  end;
end;

procedure TPrackConnection.Setup;
begin
  if not Assigned(Request) then
  begin
    Request := TRequest.Create;
    Request.LoadFromStream(Socket, True);
    if Request.Query = '' then
      Writeln(Format('Gateway: %s %s', [Request.Command, Request.URI]))
    else
      Writeln(Format('Gateway: %s %s?%s', [Request.Command, Request.URI,
        Request.Query]));
  end;
end;

end.
