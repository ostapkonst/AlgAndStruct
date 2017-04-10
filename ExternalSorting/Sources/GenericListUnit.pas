unit GenericListUnit;

{$mode objfpc}{$H+}{$Q+}

interface

uses
  Classes, SysUtils, Windows, Grids, Math;

type
  // Поддерживаемые методы сортировки
  TSortMethod = (sfNaturalFusion, sfSimpleFusion);

  TSortStats = object
    sortName: string; // Вид сортировки
    typeName: string; // Тип данных
    elementCount: int64; // Количество элементов
    compareCount: int64; // Количество сравнений
    swapCount: int64; // Количество серий
    mergeCount: int64; // Количество слияний
    timeCount: extended; // Время выполнения алгоритма
    procedure ToStrGrid(StrGrid: TStringGrid);
  end;

  IGList = interface(IInterface)
    function Sort(inpFile, outFile: string; Method: TSortMethod): TSortStats;
  end;

  generic TGList<T> = class(TInterfacedObject, IGList)
  private
    function GetTempFile: string; inline;
    procedure MergeFiles(var f1, f2, oFile: Text; var Stat: TSortStats); inline;
    // Алгоритмы сортировки
    procedure NaturalFusionSort(inpFile, outFile: string; var Stat: TSortStats);
    procedure SimpleFusionSort(inpFile, outFile: string; var Stat: TSortStats);
  public
    procedure ToTextFile(var aFile: TextFile; aValue: T); virtual; abstract;
    function FromTextFile(var aFile: TextFile): T; virtual; abstract;
    function Sort(inpFile, outFile: string; Method: TSortMethod): TSortStats;
  end;

implementation

uses
  ListProfitUnit;

function TGList.GetTempFile: string;
var
  Buffer: array[0..MAX_PATH] of char;
begin
  GetTempPath(MAX_PATH, Buffer);
  GetTempFileName(Buffer, '~', 0, Buffer);
  Result := Buffer;
end;

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
    Cells[6, LastRow] := IntToStr(mergeCount);
    Cells[7, LastRow] := FloatToStrF(timeCount, ffFixed, 0, 2);
  end;
end;

procedure TGList.MergeFiles(var f1, f2, oFile: Text; var Stat: TSortStats);
var
  v1, v2: T;
begin
  Inc(Stat.mergeCount);

  try
    ReWrite(oFile);
    Reset(f1);
    Reset(f2);

    while not (EOF(f1) or EOF(f2)) do
    begin
      v1 := FromTextFile(f1);
      v2 := FromTextFile(f2);

      while True do
      begin
        Inc(Stat.compareCount);
        if v1 <= v2 then
        begin
          ToTextFile(oFile, v1);
          if EOLn(f1) then
          begin
            ToTextFile(oFile, v2);
            break;
          end;
          v1 := FromTextFile(f1);
        end
        else
        begin
          ToTextFile(oFile, v2);
          if EOLn(f2) then
          begin
            ToTextFile(oFile, v1);
            break;
          end;
          v2 := FromTextFile(f2);
        end;
      end;

      while not EOLn(f1) do
      begin
        v1 := FromTextFile(f1);
        ToTextFile(oFile, v1);
      end;

      while not EOLn(f2) do
      begin
        v2 := FromTextFile(f2);
        ToTextFile(oFile, v2);
      end;

      ReadLn(f1);
      ReadLn(f2);
    end;

    while not EOF(f1) do
    begin
      v1 := FromTextFile(f1);
      ToTextFile(oFile, v1);
    end;

    while not EOF(f2) do
    begin
      v2 := FromTextFile(f2);
      ToTextFile(oFile, v2);
    end;
  finally
    Close(oFile);
    Close(f1);
    Close(f2);
  end;
end;

// Сортировка простым слиянием
// Подробное описание, реализация на СИ
// http://www.intuit.ru/studies/courses/648/504/lecture/11473?page=1
procedure TGList.SimpleFusionSort(inpFile, outFile: string; var Stat: TSortStats);
var
  i, j, k: integer;
  f1, f2, iFile, oFile: Text;
  v1: T;
  cur, inp: ^Text;
begin
  Assign(iFile, inpFile);
  Assign(oFile, outFile);
  Assign(f1, GetTempFile);
  Assign(f2, GetTempFile);

  inp := @iFile;
  k := 0;
  repeat
    try
      Reset(inp^);
      ReWrite(f1);
      ReWrite(f2);

      j := 0;
      i := 0;
      cur := @f2;
      while not EOF(inp^) do
      begin
        v1 := FromTextFile(inp^);
        if j mod round(power(2, k)) = 0 then
        begin
          if j > 0 then
            WriteLn(cur^);
          if cur = @f1 then
            cur := @f2
          else
            cur := @f1;
          Inc(i);
          Inc(Stat.swapCount);
        end;
        ToTextFile(cur^, v1);
        Inc(j);

        if k = 0 then
          Inc(Stat.elementCount);
      end;
      Inc(k);
    finally
      Close(inp^);
      Close(f1);
      Close(f2);
    end;

    MergeFiles(f1, f2, oFile, Stat);

    inp := @oFile;
  until i < 3;
end;


// Сортировка естественным слиянием
// Подробное описание, реализация на СИ
// http://www.intuit.ru/studies/courses/648/504/lecture/11473?page=2
procedure TGList.NaturalFusionSort(inpFile, outFile: string; var Stat: TSortStats);
var
  i: integer;
  f1, f2, iFile, oFile: Text;
  v1, v2: T;
  cur, inp: ^Text;
  secRun, firstLoop: boolean;
begin
  Assign(iFile, inpFile);
  Assign(oFile, outFile);
  Assign(f1, GetTempFile);
  Assign(f2, GetTempFile);

  inp := @iFile;
  firstLoop := True;
  repeat
    try
      Reset(inp^);
      ReWrite(f1);
      ReWrite(f2);

      i := 0;
      cur := @f1;
      secRun := False;
      Inc(Stat.swapCount);
      while not EOF(inp^) do
      begin
        v2 := FromTextFile(inp^);
        if secRun and (v1 > v2) then
        begin
          WriteLn(cur^);
          if cur = @f1 then
            cur := @f2
          else
            cur := @f1;
          Inc(i);
          Inc(Stat.swapCount);
        end;
        secRun := True;
        ToTextFile(cur^, v2);
        v1 := v2;
        if firstLoop then
          Inc(Stat.elementCount);
      end;
      firstLoop := False;
    finally
      Close(inp^);
      Close(f1);
      Close(f2);
    end;

    MergeFiles(f1, f2, oFile, Stat);

    inp := @oFile;
  until i < 2;
end;

// Способы замера времени описаны на сайте
// http://www.cyberforum.ru/delphi/thread697210.html
function TGList.Sort(inpFile, outFile: string; Method: TSortMethod): TSortStats;
var
  TimeBefore, TimeAfter, Freq: int64;
begin
  with Result do
  begin
    elementCount := 0;
    compareCount := 0;
    swapCount := 0;
    mergeCount := 0;

    QueryPerformanceFrequency(Freq);
    QueryPerformanceCounter(TimeBefore);
    case Method of
      sfNaturalFusion: NaturalFusionSort(inpFile, outFile, Result);
      sfSimpleFusion: SimpleFusionSort(inpFile, outFile, Result);
    end;
    QueryPerformanceCounter(TimeAfter);

    timeCount := (TimeAfter - TimeBefore) * 1000 / Freq;
  end;
end;

end.
