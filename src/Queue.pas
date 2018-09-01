unit Queue;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Requests, DateUtils;

const
  DEFAULT_TIMEOUT = 30;

type

  { TPrackQueue }

  TPrackQueue = class
    private
      FQueue: TRequestList;

      function    Count(Status: TRequestStatus):            Integer;
      function    GetFirstByStatus(Status: TRequestSTatus): TRequest;
      function    FindRequestById(Identifier: String):      TRequest;

    public
      RequestTimeoutInSeconds: Integer;

      constructor Create;
      destructor  Destroy; override;

      procedure   Add(Request: TRequest);
      procedure   AttachResponse(Identifier: String; Response: String);
      procedure   WillProcess(Identifier: String);
      procedure   WillDeliver(Identifier: String);
      procedure   CheckTimeoutRequests;
      procedure   CleanupRequests;
      function    GetFirstPending:  TRequest;
      function    GetFirstReady:    TRequest;
      function    QueueLength:      Integer;
      function    IncomingLength:   Integer;
      function    ProcessingLength: Integer;
      function    ReadyLength:      Integer;
      function    DeliveredLength:  Integer;
      function    FailedLength:     Integer;
  end;

implementation

{ TPrackQueue }

constructor TPrackQueue.Create;
begin
  FQueue := TRequestList.Create(True);
  RequestTimeoutInSeconds := DEFAULT_TIMEOUT;
end;

destructor TPrackQueue.Destroy;
begin
  FreeAndNil(FQueue);
  inherited;
end;

function TPrackQueue.Count(Status: TRequestStatus): Integer;
var
  Request: TRequest;
begin
  Result := 0;
  for Request in FQueue do
    if Request.Status = Status
    then Inc(Result);
end;

function TPrackQueue.GetFirstByStatus(Status: TRequestSTatus): TRequest;
var
  Request: TRequest;
begin
  for Request in FQueue do
  begin
    if Request.Status <> Status then Continue;
    Result := Request;
    Exit;
  end;
end;

function TPrackQueue.FindRequestById(Identifier: String): TRequest;
var
  Request: TRequest;
begin
  for Request in FQueue do
  begin
    if Request.Identifier <> Identifier then Continue;
    Result := Request;
    Exit;
  end;
end;

function TPrackQueue.QueueLength: Integer;
begin
  Result := FQueue.Count;
end;

function TPrackQueue.IncomingLength: Integer;
begin
  Result := Count(rsIncoming);
end;

function TPrackQueue.ProcessingLength: Integer;
begin
  Result := Count(rsProcessing);
end;

function TPrackQueue.ReadyLength: Integer;
begin
  Result := Count(rsReady);
end;

function TPrackQueue.FailedLength: Integer;
begin
  Result := Count(rsFailed);
end;

function TPrackQueue.DeliveredLength: Integer;
begin
  Result := Count(rsDelivered);
end;

function TPrackQueue.GetFirstPending: TRequest;
begin
  Result := GetFirstByStatus(rsIncoming);
end;

function TPrackQueue.GetFirstReady: TRequest;
begin
  Result := GetFirstByStatus(rsReady);
end;

procedure TPrackQueue.Add(Request: TRequest);
begin
  FQueue.Add(Request);
end;

procedure TPrackQueue.WillProcess(Identifier: String);
var
  Request: TRequest;
begin
  Request := Self.FindRequestById(Identifier);
  if not Assigned(Request) then Exit;
  if Request.Status = rsIncoming then Request.Status := rsProcessing;
end;

procedure TPrackQueue.WillDeliver(Identifier: String);
var
  Request: TRequest;
begin
  Request := Self.FindRequestById(Identifier);
  if not Assigned(Request) then Exit;
  if Request.Status = rsReady then Request.Status := rsDelivered;
end;

procedure TPrackQueue.AttachResponse(Identifier: String; Response: String);
var
  Request: TRequest;
begin
  Request := FindRequestById(Identifier);

  if (not Assigned(Request))
  or (Request.Status <> rsProcessing)
    then Exit;

  Request.Response := Response;
  Request.Status   := rsReady;
end;

procedure TPrackQueue.CheckTimeoutRequests;
var
  Request: TRequest;
begin
  for Request in FQueue do
    if SecondsBetween(Now, Request.UpdatedAt) >= RequestTimeoutInSeconds
    then Request.Status := rsFailed;
end;

procedure TPrackQueue.CleanupRequests;
var
  Index: Integer;
begin
  for Index := FQueue.Count - 1 downto 0 do
    if (FQueue.Items[Index].Status = rsDelivered)
    or (FQueue.Items[Index].Status = rsFailed)
      then FQueue.Delete(Index);
end;

end.

