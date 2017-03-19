unit GenericListUnit;

{$mode objfpc}{$H+}{$Q+}

interface

uses
  Classes, SysUtils, Windows, Grids;

type
  // Поддерживаемые методы сортировки
  TSortMethod = (sfSelection, sfBubble, sfQuick, sfInsert, sfShell);

  TSortStats = object
    sortName: string; // Вид сортировки
    typeName: string; // Тип данных
    elementCount: int64; // Количество элементов
    compareCount: int64; // Количество сравнений
    swapCount: int64; // Количество перестановок
    timeCount: extended; // Время выполнения алгоритма
    procedure ToStrGrid(StrGrid: TStringGrid);
  end;

  IGList = interface(IInterface)
    function Sort(Method: TSortMethod): TSortStats;
    function ToStrings: TStrings;
    procedure FromStrings(const List: TStrings);
  end;

  generic TGList<T> = class(TInterfacedObject, IGList)
  private
    Values: array of T;
    function GetItem(Index: integer): T;
    procedure SetItem(Index: integer; const Value: T);
    procedure Swap(var a, b: T); inline;
    // Алгоритмы сортировки
    procedure SelectionSort(var Stat: TSortStats);
    procedure BubbleSort(var Stat: TSortStats);
    procedure QuickSort(var Stat: TSortStats; First, last: integer);
    procedure InsertSort(var Stat: TSortStats);
    procedure ShellSort(var Stat: TSortStats);
  public
    function Count: integer;
    procedure Add(const Value: T);
    property Items[Index: integer]: T read GetItem write SetItem; default;
    function ToStrings: TStrings; virtual; abstract;
    procedure FromStrings(const List: TStrings); virtual; abstract;
    function Sort(Method: TSortMethod): TSortStats;
    procedure Clear;
  end;

implementation

uses
  ListProfitUnit;

procedure TSortStats.ToStrGrid(StrGrid: TStringGrid);
var
  LastRow: integer;
begin
  with StrGrid do
  begin
    LastRow := RowCount;
    RowCount := RowCount + 1;
    Cells[0, LastRow] := IntToStr(LastRow);
    Cells[1, LastRow] := sortName;
    Cells[2, LastRow] := typeName;
    Cells[3, LastRow] := IntToStr(elementCount);
    Cells[4, LastRow] := IntToStr(compareCount);
    Cells[5, LastRow] := IntToStr(swapCount);
    Cells[6, LastRow] := FloatToStrF(timeCount, ffFixed, 0, 2);
  end;
end;

procedure TGList.Swap(var a, b: T);
var
  c: T;
begin
  c := a;
  a := b;
  b := c;
end;

function TGList.Count: integer;
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

// Сортировка обменом
procedure TGList.SelectionSort(var Stat: TSortStats);
var
  i, j, min: integer;
begin
  for i := 0 to Count - 2 do
  begin
    Inc(Stat.swapCount);
    min := i;
    for j := i + 1 to Count - 1 do
    begin
      Inc(Stat.compareCount);
      if Values[j] < Values[min] then
        min := j;
    end;
    Swap(Values[i], Values[min]);
  end;
end;

// Пузырькова сортировка
procedure TGList.BubbleSort(var Stat: TSortStats);
var
  i, j: integer;
begin
  for i := 1 to Count - 1 do
    for j := 0 to Count - 1 - i do
    begin
      Inc(Stat.compareCount);
      if Values[j] > Values[j + 1] then
      begin
        Inc(Stat.swapCount);
        Swap(Values[j], Values[j + 1]);
      end;
    end;
end;

// Быстрая сортировка
procedure TGList.QuickSort(var Stat: TSortStats; first, last: integer);
var
  i, j: integer;
  x: T;
begin
  i := first;
  j := last;

  if (i >= j) then
    Exit;
  x := Values[(i + j) div 2]; // Можно использовать Values[i + random(j - i)];

  repeat
    while Values[i] < x do
      Inc(i);
    while Values[j] > x do
      Dec(j);

    if i <= j then
    begin
      Inc(Stat.compareCount);
      if Values[i] > Values[j] then
      begin
        Inc(Stat.swapCount);
        Swap(Values[i], Values[j]);
      end;
      Inc(i);
      Dec(j);
    end;
  until i > j;

  QuickSort(Stat, i, last);
  QuickSort(Stat, first, j);
end;

// Сортировка вставками
procedure TGList.InsertSort(var Stat: TSortStats);
var
  i, j: integer;
  x: T;
begin
  for i := 1 to Count - 1 do
  begin
    Inc(Stat.swapCount);
    x := Values[i];
    j := i - 1;
    while j >= 0 do
    begin
      Inc(Stat.compareCount);
      if Values[j] <= x then
        break;
      Inc(Stat.swapCount);
      Values[j + 1] := Values[j];
      Dec(j);
    end;
    Values[j + 1] := x;
  end;
end;

// Сортировка Шелла
procedure TGList.ShellSort(var Stat: TSortStats);
var
  i, j, step: integer;
begin
  step := Count div 2;
  while step > 0 do
  begin
    for i := 0 to Count - 1 - step do
    begin
      j := i;
      while j >= 0 do
      begin
        Inc(Stat.compareCount);
        if Values[j] <= Values[j + step] then
          break;
        Inc(Stat.swapCount);
        Swap(Values[j], Values[j + step]);
        j -= step;
      end;
    end;
    step := step div 2;
  end;
end;

// Способы замера времени описаны на сайте
// http://www.cyberforum.ru/delphi/thread697210.html
function TGList.Sort(Method: TSortMethod): TSortStats;
var
  TimeBefore, TimeAfter, Freq: int64;
begin
  with Result do
  begin
    elementCount := Count;
    compareCount := 0;
    swapCount := 0;

    QueryPerformanceFrequency(Freq);
    QueryPerformanceCounter(TimeBefore);
    case Method of
      sfSelection: SelectionSort(Result);
      sfBubble: BubbleSort(Result);
      sfQuick: QuickSort(Result, 0, Count - 1);
      sfInsert: InsertSort(Result);
      sfShell: ShellSort(Result);
    end;
    QueryPerformanceCounter(TimeAfter);

    timeCount := (TimeAfter - TimeBefore) * 1000 / Freq;
  end;
end;

end.
