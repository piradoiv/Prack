unit Orchestra;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Queue, DateUtils, Connections;

const
  TIMEOUT_SECONDS_LIMIT = 60;
  READY_REQUEST_LIMIT = 1000;

type

  { TOrchestra }

  TOrchestra = class(TThread)
  private
    FQueue: TPrackQueue;
  protected
    procedure Execute; override;
    procedure ProcessPendingRequests(List: TList);
  public
    constructor Create(aQueue: TPrackQueue);
  end;

implementation

{ TOrchestra }

constructor TOrchestra.Create(aQueue: TPrackQueue);
begin
  FQueue := aQueue;
  inherited Create(False);
end;

procedure TOrchestra.Execute;
var
  List: TList;
begin
  while not Terminated do
  begin
    FQueue.ReadyRequestsEvent.WaitFor(READY_REQUEST_LIMIT);
    List := FQueue.LockList;
    try
      ProcessPendingRequests(List);
    finally
      FQueue.UnlockList;
    end;
  end;
end;

procedure TOrchestra.ProcessPendingRequests(List: TList);
var
  Connection: TPrackConnection;
  I: integer;
begin
  for I := List.Count - 1 downto 0 do
  begin
    Connection := TPrackConnection(List.Items[I]);
    Connection.Setup;

    if (Connection.Status <> pcsReady) and
      (SecondsBetween(Now, Connection.CreatedAt) >= TIMEOUT_SECONDS_LIMIT) then
      Connection.SetErrorResponse(504, 'Timeout' + CRLF);

    if Connection.Status = pcsError then
      Connection.SetErrorResponse(502, 'Backend error' + CRLF);

    if Connection.Status <> pcsReady then
      Continue;

    // TODO: Responses can be sent in parallel
    // https://github.com/piradoiv/Prack/issues/2
    Connection.SendResponse;
    List.Remove(Connection);
    FreeAndNil(Connection);
  end;
end;

end.
