unit ListIntegerUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GenericListUnit;

type
  TListInteger = class(specialize TGList<integer>)
    function ToStrings: TStrings; override;
    procedure FromStrings(const List: TStrings); override;
  end;

implementation

procedure TListInteger.FromStrings(const List: TStrings);
var
  OutPutList: TStringList;
  i: integer;
begin
  Clear;
  OutPutList := TStringList.Create;
  try
    OutPutList.DelimitedText := List.Text;
    for i := 0 to OutPutList.Count - 1 do
      Add(StrToInt(Trim(OutPutList[i])));
  finally
    OutPutList.Free;
  end;
end;

function TListInteger.ToStrings: TStrings;
var
  i: integer;
  s: string = '';
begin
  Result := TStringList.Create;
  for i := 0 to Count - 1 do
    s += IntToStr(Items[i]) + ' ';
  Result.Text := LeftStr(s, s.Length - 1);
end;

end.

