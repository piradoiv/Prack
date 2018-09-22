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

  TPrackResponse = class
  public
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
    RequestHeaders: TRequest;
    Response: TPrackResponse;
    constructor Create;
    destructor Destroy; override;
    procedure SendResponse;
    procedure Setup;
  end;

implementation

{ TPrackConnection }

constructor TPrackConnection.Create;
var
  GUID: TGUID;
begin
  CreatedAt := Now;
  CreateGuid(GUID);
  Identifier := GuidToString(GUID);
  Status := pcsIncoming;
end;

destructor TPrackConnection.Destroy;
begin
  FreeAndNil(Socket);
  FreeAndNil(Response);
  FreeAndNil(RequestHeaders);
  inherited Destroy;
end;

procedure TPrackConnection.SendResponse;
var
  Http: string;
  StringStream: TStringStream;
begin
  Http := Format('HTTP/1.1 %s %s', [IntToStr(Response.Code),
    GetStatusCode(Response.Code)]);
  StringStream := TStringStream.Create(Concat(Http, CRLF, Response.Headers,
    CRLF, Response.Body));
  Socket.CopyFrom(StringStream, StringStream.Size);
  FreeAndNil(StringStream);
end;

procedure TPrackConnection.Setup;
begin
  if not Assigned(Response) then
    Response := TPrackResponse.Create;

  if not Assigned(RequestHeaders) then
  begin
    RequestHeaders := TRequest.Create;
    RequestHeaders.LoadFromStream(Socket, True);
    if RequestHeaders.Query = '' then
      Writeln(Format('Gateway: %s %s', [RequestHeaders.Command, RequestHeaders.URI]))
    else
      Writeln(Format('Gateway: %s %s?%s', [RequestHeaders.Command, RequestHeaders.URI, RequestHeaders.Query]));
  end;
end;

end.
