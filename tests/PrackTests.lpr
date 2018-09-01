program PrackTests;

{$mode objfpc}{$H+}

uses
  Classes, consoletestrunner, TestHTTPRequest, TestQueue;

type

  { TMyTestRunner }

  TMyTestRunner = class(TTestRunner)
  end;

var
  Application: TMyTestRunner;

begin
  Application := TMyTestRunner.Create(nil);
  Application.Initialize;
  Application.Run;
  Application.Free;
end.
