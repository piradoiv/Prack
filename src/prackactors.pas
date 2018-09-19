unit PrackActors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Theatre;

type

  { TRequestSTatus }

  TRequestStatus = (rsIncoming, rsProcessing, rsReady, rsFailed);

  { TPrackRequest }

  TPrackRequest = class(TActor)
  private
    FRequest: string;
    FResponse: string;
    FResponseCode: integer;
    FStatus: TRequestStatus;

  public
    constructor Create(Request: string; Response: string; Status: TRequestStatus);
    destructor Destroy; override;
    function GetRequest: string;
    function GetResponse: string;
    function GetResponseCode: integer;
    function GetStatus: TRequestStatus;
  end;

implementation

{ TPrackRequest }

constructor TPrackRequest.Create(Request: string; Response: string;
  Status: TRequestStatus);
begin
  inherited Create;
  FRequest := Request;
  FResponse := Response;
  FStatus := Status;
end;

destructor TPrackRequest.Destroy;
begin
  inherited Destroy;
end;

function TPrackRequest.GetRequest: string;
begin
  Result := FRequest;
end;

function TPrackRequest.GetResponse: string;
begin
  Result := FResponse;
end;

function TPrackRequest.GetResponseCode: integer;
begin
  Result := FResponseCode;
end;

function TPrackRequest.GetStatus: TRequestStatus;
begin
  Result := FStatus;
end;

end.
