unit Connections;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ssockets, httpdefs, fphttpserver;

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
  Response := TPrackResponse.Create;
  RequestHeaders := TRequest.Create;
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
  Assert(Response.Code >= 100);
  //Assert(Response.Headers <> '');
  Assert(Response.Body <> '');

  Http := Concat('HTTP/1.1 ', IntToStr(Response.Code), ' ',
    GetStatusCode(Response.Code), CRLF);
  StringStream := TStringStream.Create(Concat(Http, Response.Headers,
    CRLF, Response.Body));
  Socket.CopyFrom(StringStream, StringStream.Size);
  FreeAndNil(StringStream);

  Assert(not Assigned(StringStream));
end;

end.

