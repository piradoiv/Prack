unit Queue;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, HttpDefs, Syncobjs, Connections;

type

  { TPrackQueue }

  TPrackQueue = class(TThreadList)
  private
    Mutex: TRTLCriticalSection;
    PendingRequests: integer;
  public
    ReadyRequestsEvent: TEventObject;
    PendingRequestsEvent: TEventObject;
    constructor Create;
    destructor Destroy; override;
    procedure IncPendingRequests;
    procedure DecPendingRequests;
    function Pop(Status: TPrackConnectionStatus): TPrackConnection;
    function Pop(Status: TPrackConnectionStatus; Identifier: string): TPrackConnection;
  end;

implementation

{ TPrackQueue }

constructor TPrackQueue.Create;
begin
  PendingRequests := 0;
  PendingRequestsEvent := TEventObject.Create(nil, True, True, '');
  ReadyRequestsEvent := TEventObject.Create(nil, False, False, '');
  InitCriticalSection(Mutex);
  inherited Create;
end;

destructor TPrackQueue.Destroy;
var
  List: TList;
  I: integer;
begin
  List := LockList;
  DoneCriticalSection(Mutex);
  for I := List.Count - 1 downto 0 do
  begin
    TPrackConnection(List.Items[I]).Free;
    List.Items[I] := nil;
  end;
  inherited Destroy;
  PendingRequestsEvent.SetEvent;
  FreeAndNil(PendingRequestsEvent);
  FreeAndNil(ReadyRequestsEvent);
end;

procedure TPrackQueue.IncPendingRequests;
begin
  EnterCriticalSection(Mutex);
  try
    Inc(PendingRequests);
    PendingRequestsEvent.SetEvent;
  finally
    LeaveCriticalSection(Mutex);
  end;
end;

procedure TPrackQueue.DecPendingRequests;
begin
  EnterCriticalSection(Mutex);
  try
    if PendingRequests = 0 then
      Exit;
    Dec(PendingRequests);
  finally
    if PendingRequests = 0 then
      PendingRequestsEvent.ResetEvent;
    LeaveCriticalSection(Mutex);
  end;
end;

function TPrackQueue.Pop(Status: TPrackConnectionStatus): TPrackConnection;
var
  I: integer;
  List: TList;
  Connection: TPrackConnection;
begin
  Result := nil;
  List := LockList;
  try
    for I := 0 to List.Count - 1 do
    begin
      Connection := TPrackConnection(List.Items[I]);
      if Connection.Status <> Status then
        Continue;

      Result := TPrackConnection(List.Items[I]);
      Remove(Result);
      Exit;
    end;
  finally
    if not Assigned(Result) and (Status = pcsIncoming) then
      PendingRequestsEvent.ResetEvent;
    UnlockList;
  end;
end;

function TPrackQueue.Pop(Status: TPrackConnectionStatus;
  Identifier: string): TPrackConnection;
var
  I: integer;
  List: TList;
  Connection: TPrackConnection;
begin
  Result := nil;
  List := LockList;
  try
    for I := 0 to List.Count - 1 do
    begin
      Connection := TPrackConnection(List.Items[I]);
      if (Connection.Status <> Status) or (Connection.Identifier <> Identifier) then
        Continue;

      Result := Connection;
      Remove(Result);
      Exit;
    end;
  finally
    UnlockList;
  end;
end;

end.
