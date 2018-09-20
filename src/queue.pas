unit Queue;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ssockets, HttpDefs, fphttpserver, Syncobjs;

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
    function Build: TStringStream;
  end;

  { TPrackConnection }

  TPrackConnection = class
  public
    Identifier: string;
    CreatedAt: TDateTime;
    Status: TPrackConnectionStatus;
    Sender: TObject;
    Socket: TSocketStream;
    RequestHeaders: TRequest;
    Response: TPrackResponse;
    constructor Create;
    destructor Destroy; override;
    procedure SendResponse;
  end;

  { TPrackQueue }

  TPrackQueue = class(TThreadList)
  public
    Event: TEventObject;
    constructor Create;
    destructor Destroy; override;
    function Count: integer;
  end;

implementation

{ TPrackResponse }

function TPrackResponse.Build: TStringStream;
var
  Http: string;
begin
  Http := Concat('HTTP/1.1 ', IntToStr(Code), ' ', GetStatusCode(Code), CRLF);
  Result := TStringStream.Create(Concat(Http, Headers, CRLF, Body));
end;

{ TPrackQueue }

constructor TPrackQueue.Create;
begin
  Event := TEventObject.Create(nil, True, False, '');
  inherited Create;
end;

destructor TPrackQueue.Destroy;
var
  List: TList;
  I: Integer;
begin
  List := LockList;
  FreeAndNil(Event);
  for I := List.Count - 1 downto 0 do
  begin
    TPrackConnection(List.Items[I]).Free;
  end;
  inherited Destroy;
end;

function TPrackQueue.Count: integer;
var
  List: TList;
begin
  List := LockList;
  try
    Result := List.Count;
  finally
    UnlockList;
  end;
end;

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
  StringStream: TStringStream;
begin
  StringStream := Response.Build;
  Socket.CopyFrom(StringStream, StringStream.Size);
  FreeAndNil(StringStream);
end;

end.
