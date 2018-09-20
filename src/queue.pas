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
begin
  Result := nil;
  List := LockList;
  try
    for I := 0 to List.Count - 1 do
    begin
      if TPrackConnection(List.Items[I]).Status <> pcsIncoming then
        Continue;

      Result := TPrackConnection(List.Items[I]);
      Remove(Result);
      Exit;
    end;
  finally
    UnlockList;
  end;
end;

end.
