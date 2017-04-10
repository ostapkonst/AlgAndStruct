unit ListProfitUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GenericListUnit;

type
  TProfit = record
    Id: integer;    // Код товара
    Name: string;   // Наименование товара
    Count: integer; // Количество продаж
  end;

operator > (a, b: TProfit): boolean;
operator < (a, b: TProfit): boolean;
operator <= (a, b: TProfit): boolean;
operator >= (a, b: TProfit): boolean;

type
  TListProfit = class(specialize TGList<TProfit>)
    procedure ToTextFile(var aFile: TextFile; aValue: TProfit); override;
    function FromTextFile(var aFile: TextFile): TProfit; override;
  end;


implementation

operator > (a, b: TProfit): boolean;
begin
  Result := a.Count > b.Count;
end;

operator < (a, b: TProfit): boolean;
begin
  Result := a.Count < b.Count;
end;

operator <= (a, b: TProfit): boolean;
begin
  Result := a.Count <= b.Count;
end;

operator >= (a, b: TProfit): boolean;
begin
  Result := a.Count <= b.Count;
end;

procedure Split(Delimiter: char; Str: string; ListOfStrings: TStrings);
begin
  ListOfStrings.Clear;
  ListOfStrings.Delimiter := Delimiter;
  ListOfStrings.StrictDelimiter := True;
  ListOfStrings.DelimitedText := Str;
end;

procedure TListProfit.ToTextFile(var aFile: TextFile; aValue: TProfit);
begin
  with aValue do
    WriteLn(aFile, Id, ', ', Name, ', ', Count);
end;

function TListProfit.FromTextFile(var aFile: TextFile): TProfit;
var
  OutPutList: TStringList;
  inpStr: string;
begin
  OutPutList := TStringList.Create;
  try
    ReadLn(aFile, inpStr);
    Split(',', inpStr, OutPutList);
    with Result do
    begin
      Id := StrToInt(OutPutList[0]);
      Name := Trim(OutPutList[1]);
      Count := StrToInt(OutPutList[2]);
    end;
  finally
    OutPutList.Free;
  end;
end;

end.


