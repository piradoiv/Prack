unit TestQueue;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, Queue, Requests, DateUtils;

type

  { TTestQueue }

  TTestQueue = class(TTestCase)
    protected
      procedure Setup; override;
      procedure TearDown; override;
      function  GenerateRequest(Status: TRequestStatus = rsIncoming): TRequest;

    published
      procedure TestCanCreate;
      procedure TestCanAddNewGatewayRequests;
      procedure TestCanGetQueueStats;
      procedure TestCanSendRequestsThroughTheApi;
      procedure TestCanEnqueueResponsesFromTheApi;
      procedure TestCanSendBackReadyResponses;
      procedure TestRequestsCanTimeout;
      procedure TestCanCleanupFailedRequests;
      procedure TestCanNotSwitchToAnInvalidState;
  end;

var
  PrackQueue: TPrackQueue;

implementation

{ TTestQueue }

procedure TTestQueue.Setup;
begin
  PrackQueue := TPrackQueue.Create;
end;

procedure TTestQueue.TearDown;
begin
  FreeAndNil(PrackQueue);
end;

function TTestQueue.GenerateRequest(Status: TRequestStatus): TRequest;
begin
  Result := TRequest.Create('Test', 42, 'Test');
  Result.Status := Status;
end;

procedure TTestQueue.TestCanCreate;
begin
  AssertTrue(Assigned(PrackQueue));
end;

procedure TTestQueue.TestCanAddNewGatewayRequests;
begin
  AssertEquals(0, PrackQueue.QueueLength);
  PrackQueue.AddNewRequest(GenerateRequest);
  AssertEquals(1, PrackQueue.QueueLength);
end;

procedure TTestQueue.TestCanSendRequestsThroughTheApi;
var
  Request: TRequest;
begin
  PrackQueue.AddNewRequest(GenerateRequest);
  Request := PrackQueue.GetFirstPending;
  Assert(Request.Status = rsIncoming);
  AssertEquals(0, PrackQueue.ProcessingLength);
  PrackQueue.WillProcess(Request);
  Assert(Request.Status = rsProcessing);
  AssertEquals(1, PrackQueue.ProcessingLength);
end;

procedure TTestQueue.TestCanEnqueueResponsesFromTheApi;
var
  Request: TRequest;
begin
  PrackQueue.AddNewRequest(GenerateRequest(rsIncoming));
  PrackQueue.AddNewRequest(GenerateRequest(rsProcessing));
  AssertEquals(1, PrackQueue.PendingLength);
  AssertEquals(1, PrackQueue.ProcessingLength);
  AssertEquals(0, PrackQueue.ReadyLength);

  Request := PrackQueue.GetFirstPending;
  Request.Status := rsProcessing;
  PrackQueue.WillProcess(Request);
  AssertEquals(0, PrackQueue.PendingLength);
  AssertEquals(2, PrackQueue.ProcessingLength);
  AssertEquals(0, PrackQueue.ReadyLength);

  PrackQueue.ApiPostResponse(Request);
  AssertEquals(0, PrackQueue.PendingLength);
  AssertEquals(1, PrackQueue.ProcessingLength);
  AssertEquals(1, PrackQueue.ReadyLength);
end;

procedure TTestQueue.TestCanSendBackReadyResponses;
var
  Request: TRequest;
begin
  Request := GenerateRequest;
  Request.Status := rsReady;
  PrackQueue.AddNewRequest(Request);
  AssertEquals(1, PrackQueue.ReadyLength);

  Request := PrackQueue.GetFirstReady;
  PrackQueue.WillDeliver(Request);
end;

procedure TTestQueue.TestRequestsCanTimeout;
var
  Request: TRequest;
begin
  Request := GenerateRequest;
  Request.CreatedAt := IncHour(Now, -5);
  Request.UpdatedAt := Request.CreatedAt;
  PrackQueue.AddNewRequest(Request);

  Request := GenerateRequest;
  Request.CreatedAt := IncSecond(Now, -2);
  Request.UpdatedAt := Request.CreatedAt;
  PrackQueue.AddNewRequest(Request);

  PrackQueue.AddNewRequest(GenerateRequest);

  AssertEquals(0, PrackQueue.FailedLength);
  AssertEquals(3, PrackQueue.QueueLength);

  PrackQueue.CheckTimeoutRequests;

  AssertEquals(1, PrackQueue.FailedLength);
  AssertEquals(3, PrackQueue.QueueLength);
end;

procedure TTestQueue.TestCanCleanupFailedRequests;
begin
  PrackQueue.AddNewRequest(GenerateRequest(rsFailed));
  PrackQueue.AddNewRequest(GenerateRequest(rsIncoming));
  PrackQueue.AddNewRequest(GenerateRequest(rsProcessing));
  PrackQueue.AddNewRequest(GenerateRequest(rsFailed));

  AssertEquals(4, PrackQueue.QueueLength);
  AssertEquals(2, PrackQueue.FailedLength);

  PrackQueue.CleanupRequests;

  AssertEquals(2, PrackQueue.QueueLength);
  AssertEquals(0, PrackQueue.FailedLength);
end;

procedure TTestQueue.TestCanNotSwitchToAnInvalidState;
var
  Request: TRequest;
begin
  Request := GenerateRequest;
  Request.Status := rsReady;
  PrackQueue.AddNewRequest(Request);
  Assert(Request.Status = rsReady);
  PrackQueue.WillProcess(Request);
  Assert(Request.Status = rsReady);

  Request.Status := rsIncoming;
  PrackQueue.WillDeliver(Request);
  Assert(Request.Status = rsIncoming);
end;

procedure TTestQueue.TestCanGetQueueStats;
var
  Request: TRequest;
begin
  AssertEquals(0, PrackQueue.PendingLength);
  PrackQueue.AddNewRequest(GenerateRequest);
  AssertEquals(1, PrackQueue.PendingLength);

  AssertEquals(0, PrackQueue.ProcessingLength);
  Request := GenerateRequest;
  Request.Status := rsProcessing;
  PrackQueue.AddNewRequest(Request);
  AssertEquals(1, PrackQueue.ProcessingLength);

  AssertEquals(0, PrackQueue.ReadyLength);
  Request := GenerateRequest;
  Request.Status := rsReady;
  PrackQueue.AddNewRequest(Request);
  AssertEquals(1, PrackQueue.ReadyLength);

  AssertEquals(0, PrackQueue.DeliveredLength);
  Request := GenerateRequest;
  Request.Status := rsDelivered;
  PrackQueue.AddNewRequest(Request);
  AssertEquals(1, PrackQueue.DeliveredLength);

  AssertEquals(4, PrackQueue.QueueLength);
end;

initialization

  RegisterTest(TTestQueue);

end.

