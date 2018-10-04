unit Queue;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Syncobjs, Connections;

type

  { TPrackQueue }

  TPrackQueue = class(TThreadList)
  private
    Mutex: TRTLCriticalSection;
  public
    ReadyRequestsEvent: TEventObject;
    PendingRequestsEvent: TEventObject;
    constructor Create;
    destructor Destroy; override;
    function Pop(Status: TPrackConnectionStatus): TPrackConnection;
    function Pop(Status: TPrackConnectionStatus; Identifier: string): TPrackConnection;
  end;

implementation

{ TPrackQueue }

constructor TPrackQueue.Create;
begin
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
  for I := List.Count - 1 downto 0 do
  begin
    TPrackConnection(List.Items[I]).Free;
    List.Items[I] := nil;
  end;

  PendingRequestsEvent.SetEvent;
  DoneCriticalSection(Mutex);
  FreeAndNil(Mutex);
  FreeAndNil(PendingRequestsEvent);
  FreeAndNil(ReadyRequestsEvent);
  inherited Destroy;
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
