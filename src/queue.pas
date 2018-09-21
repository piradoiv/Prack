unit Queue;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, HttpDefs, Syncobjs, Connections;

type

  { TPrackQueue }

  TPrackQueue = class(TThreadList)
  public
    Event: TEventObject;
    constructor Create;
    destructor Destroy; override;
    function Count: integer;
    function Pop(Status: TPrackConnectionStatus): TPrackConnection;
    function Pop(Status: TPrackConnectionStatus; Identifier: string): TPrackConnection;
  end;

implementation

{ TPrackQueue }

constructor TPrackQueue.Create;
begin
  inherited Create;
  Event := TEventObject.Create(nil, True, False, '');
end;

destructor TPrackQueue.Destroy;
var
  List: TList;
  I: integer;
begin
  List := LockList;
  FreeAndNil(Event);
  for I := List.Count - 1 downto 0 do
  begin
    TPrackConnection(List.Items[I]).Free;
    List.Items[I] := nil;
  end;
  inherited Destroy;
end;

function TPrackQueue.Count: integer;
var
  List: TList;
begin
  List := LockList;
  Result := List.Count;
  UnlockList;
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
