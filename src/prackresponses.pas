unit PrackResponses;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FGL, PrackCommon, PrackRequests;

type

  { TResponse }

  TResponse = class
    Headers: THeaderList;
  end;

  { TPrackResponseList }

  TPrackResponseList = specialize TFPGObjectList<TResponse>;

  { TPrackResponsesThread }

  TPrackResponsesThread = class(TThread)
    private
      RequestList: TRequestList;
    protected
      procedure Execute; override;
      procedure Loop;
    public
      procedure SetRequestList(Data: PRequestList);
  end;

implementation

{ TPrackResponsesThread }

procedure TPrackResponsesThread.Execute;
begin
  Loop;
end;

procedure TPrackResponsesThread.Loop;
var
  Request: TRequest;
begin
  while True do
  begin
    if RequestList.Count = 0 then Suspended := True;
    Writeln('Pending requests: ' + IntToStr(RequestList.Count));
    Request := RequestList.Extract(RequestList.Items[0]);
    Request.ExampleHandler;
    FreeAndNil(Request);
  end;
end;

procedure TPrackResponsesThread.SetRequestList(Data: PRequestList);
begin
  Assert(Assigned(Data^), 'Data must be assigned');
  RequestList := Data^;
end;

end.

