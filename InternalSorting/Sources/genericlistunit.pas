unit GenericListUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  IGList = Interface(IInterface)
    procedure Sort;
    function ToStrings: TStrings;
    procedure FromStrings(const List: TStrings);
  end;

  generic TGList<T> = class(TInterfacedObject,IGList)
  private
    Values: array of T;
    function GetItem(Index: integer): T;
    procedure SetItem(Index: integer; const Value: T);
    function GetCount(): integer;
    procedure Swap(var a, b: T); inline;
  public
    property Count: integer read GetCount;
    procedure Add(const Value: T);
    property Items[Index: integer]: T read GetItem write SetItem; default;
    function ToStrings: TStrings; virtual; abstract;
    procedure FromStrings(const List: TStrings); virtual; abstract;
    procedure Sort;
    procedure Clear;
  end;

implementation

uses
  ListProfitUnit;

procedure TGList.Swap(var a, b: T);
var
  c: T;
begin
  c := a;
  a := b;
  b := c;
end;

function TGList.GetCount(): integer;
begin
  Result := Length(Values);
end;

procedure TGList.Add(const Value: T);
begin
  SetLength(Values, Length(Values) + 1);
  Values[Length(Values) - 1] := Value;
end;

function TGList.GetItem(Index: integer): T;
begin
  Result := Values[Index];
end;

procedure TGList.SetItem(Index: integer; const Value: T);
begin
  Values[Index] := Value;
end;

procedure TGList.Clear;
begin
  Finalize(Values);
end;

procedure TGList.Sort;
var
  i, j: integer;
begin
  for i := 1 to Count - 1 do
    for j := 0 to Count - 1 - i do
      if Values[j] > Values[j + 1] then
        Swap(Values[j], Values[j + 1])
end;

end.

