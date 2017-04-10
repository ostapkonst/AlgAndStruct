unit ListIntegerUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GenericListUnit, Windows;

type
  TListInteger = class(specialize TGList<integer>)
    procedure ToTextFile(var aFile: TextFile; aValue: integer); override;
    function FromTextFile(var aFile: TextFile): integer; override;
  end;

implementation

procedure TListInteger.ToTextFile(var aFile: TextFile; aValue: integer);
begin
  Write(aFile, aValue, ' ');
end;

function TListInteger.FromTextFile(var aFile: TextFile): integer;
var
  space: char;
begin
  Read(aFile, Result, space);
end;

end.

