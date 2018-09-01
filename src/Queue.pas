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

      function    Count(Status: TRequestStatus): Integer;
      function    GetFirstByStatus(Status: TRequestSTatus): TRequest;
      function    FindRequestById(Identifier: String): TRequest;

    public
      RequestTimeoutInSeconds: Integer;

      constructor Create;
      destructor  Destroy; override;

      procedure   AddNewRequest(Request: TRequest);
      procedure   CheckTimeoutRequests;
      procedure   CleanupRequests;
      procedure   ApiPostResponse(Request: TRequest);
      procedure   WillProcess(Request: TRequest);
      procedure   WillDeliver(Request: TRequest);
      function    QueueLength: Integer;
      function    PendingLength: Integer;
      function    ProcessingLength: Integer;
      function    ReadyLength: Integer;
      function    FailedLength: Integer;
      function    DeliveredLength: Integer;
      function    GetFirstPending: TRequest;
      function    GetFirstReady: TRequest;
  end;

implementation

{ TPrackQueue }

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

constructor TPrackQueue.Create;
begin
  FQueue := TRequestList.Create(True);
  RequestTimeoutInSeconds := DEFAULT_TIMEOUT;
end;

function TPrackQueue.QueueLength: Integer;
begin
  Result := FQueue.Count;
end;

function TPrackQueue.PendingLength: Integer;
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

procedure TPrackQueue.AddNewRequest(Request: TRequest);
begin
  FQueue.Add(Request);
end;

procedure TPrackQueue.WillProcess(Request: TRequest);
begin
  if Request.Status = rsIncoming then Request.Status := rsProcessing;
end;

procedure TPrackQueue.WillDeliver(Request: TRequest);
begin
  if Request.Status = rsReady then Request.Status := rsDelivered;
end;

procedure TPrackQueue.ApiPostResponse(Request: TRequest);
var
  GatewayRequest: TRequest;
begin
  GatewayRequest := FindRequestById(Request.Identifier);
  if not Assigned(GatewayRequest) then Exit;
  GatewayRequest.Response := Request.Response;
  GatewayRequest.Status := rsReady;
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
    if FQueue.Items[Index].Status = rsFailed
    then FQueue.Delete(Index);
end;

destructor TPrackQueue.Destroy;
begin
  FreeAndNil(FQueue);
  inherited;
end;

end.

