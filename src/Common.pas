unit Common;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FGL, BlckSock, Requests;

type

  { THeader }

  THeader = class
    public
      Name: String;
      Value: String;
  end;

  { THeaderList }

  THeaderList = specialize TFPGObjectList<THeader>;

implementation

end.

