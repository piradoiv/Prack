unit PrackCommon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FGL;

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

