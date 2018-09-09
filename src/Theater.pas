unit Theater;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs;

type

  TActor = class;

  { TMessageProcessor }

  TMessageProcessor = procedure(From: TActor; Subject: string; Data: TObject) of object;

  { TEnvelope }

  TEnvelope = class
    Sender: TActor;
    Receiver: TActor;
    Subject: string;
    Data: TObject;
  end;

  { TMailbox }

  TMailbox = class(TObjectQueue)
  private
    FLock: TRTLCriticalSection;
  protected
    procedure Lock;
    procedure Unlock;
  public
    constructor Create;
    destructor Destroy; override;
    function Push(AObject: TObject): TObject;
    function Pop: TObject;
    function Peek: TObject;
  end;

  { TActor }
  TActor = class(TThread)
  private
    Identifier: string;
    Mailbox: TMailbox;
  protected
    OnReceiveMessage: TMessageProcessor;
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Send(Subject: string);
    procedure Send(Sender: TActor; Subject: string);
    procedure Send(Subject: string; Data: TObject);
    procedure Send(Sender: TActor; Subject: string; Data: TObject);
    function GetIdentifier: string;
  end;

implementation

{ TMailbox }

constructor TMailbox.Create;
begin
  InitCriticalSection(FLock);
  inherited Create;
end;

destructor TMailbox.Destroy;
begin
  DoneCriticalSection(FLock);
  inherited Destroy;
end;

procedure TMailbox.Lock;
begin
  System.EnterCriticalSection(FLock);
end;

procedure TMailbox.Unlock;
begin
  System.LeaveCriticalSection(FLock);
end;

function TMailbox.Push(AObject: TObject): TObject;
begin
  Lock;
  try
    Result := inherited Push(AObject);
  finally
    Unlock;
  end;
end;

function TMailbox.Pop: TObject;
begin
  Lock;
  try
    Result := inherited Pop;
  finally
    Unlock;
  end;
end;

function TMailbox.Peek: TObject;
begin
  Result := inherited Peek;
end;

{ TActor }

procedure TActor.Execute;
var
  Message: TEnvelope;
begin
  if not Assigned(OnReceiveMessage) then
    raise Exception.Create('OnReceiveMessage must be assigned');

  while not Terminated do
  begin
    if Mailbox.Count = 0 then
    begin
      Sleep(10);
      Continue;
    end;

    while Mailbox.Count > 0 do
    begin
      Message := TEnvelope(Mailbox.Pop);
      OnReceiveMessage(Message.Sender, Message.Subject, Message.Data);
      FreeAndNil(Message.Data);
      FreeAndNil(Message);
    end;
  end;
end;

constructor TActor.Create;
var
  Guid: TGuid;
begin
  FreeOnTerminate := False;
  Mailbox := TMailbox.Create;
  CreateGuid(Guid);
  Identifier := GuidToString(Guid);
  inherited Create(False);
end;

destructor TActor.Destroy;
begin
  Mailbox.Free;
  inherited Destroy;
end;

procedure TActor.Send(Subject: string);
begin
  Send(nil, Subject, nil);
end;

procedure TActor.Send(Sender: TActor; Subject: string);
begin
  Send(Sender, Subject, nil);
end;

procedure TActor.Send(Subject: string; Data: TObject);
begin
  Send(nil, Subject, Data);
end;

procedure TActor.Send(Sender: TActor; Subject: string; Data: TObject);
var
  Message: TEnvelope;
begin
  if Terminated then
    Exit;

  Message := TEnvelope.Create;
  Message.Sender := Sender;
  Message.Receiver := Self;
  Message.Subject := Subject;
  Message.Data := Data;
  Mailbox.Push(Message);
end;

function TActor.GetIdentifier: string;
begin
  Result := Identifier;
end;

end.
