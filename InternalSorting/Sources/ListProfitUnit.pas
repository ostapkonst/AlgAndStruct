unit ListProfitUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GenericListUnit, Dialogs;

type
  TProfit = record
    Id: integer;    // Код товара
    Name: string;   // Наименование товара
    Count: integer; // Количество продаж
  end;

operator > (a, b: TProfit): boolean;

type
  TListProfit = class(specialize TGList<TProfit>)
    function ToStrings: TStrings; override;
    procedure FromStrings(const List: TStrings); override;
  end;

implementation

operator > (a, b: TProfit): boolean;
begin
  Result := a.Count > b.Count;
end;

procedure Split(Delimiter: char; Str: string; ListOfStrings: TStrings);
begin
  ListOfStrings.Clear;
  ListOfStrings.Delimiter := Delimiter;
  ListOfStrings.StrictDelimiter := True;
  ListOfStrings.DelimitedText := Str;
end;

procedure TListProfit.FromStrings(const List: TStrings);
var
  OutPutList: TStringList;
  ProfitTmp: TProfit;
  i: integer;
begin
  Clear;
  OutPutList := TStringList.Create;
  try
    for i := 0 to List.Count - 1 do
    begin
      Split(',', List[i], OutPutList);
      with ProfitTmp do
      begin
        Id := StrToInt(OutPutList[0]);
        Name := Trim(OutPutList[1]);
        Count := StrToInt(OutPutList[2]);
      end;
      Add(ProfitTmp);
    end;
  finally
    OutPutList.Free;
  end;
end;

function TListProfit.ToStrings: TStrings;
var
  i: integer;
begin
  Result := TStringList.Create;
  for i := 0 to Count - 1 do
    with Items[i] do
      Result.Append(IntToStr(Id) + ', ' + Name + ', ' + IntToStr(Count));
end;

end.


