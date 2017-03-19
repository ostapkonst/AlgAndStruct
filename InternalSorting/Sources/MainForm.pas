unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Dialogs, Menus, ExtCtrls, ComCtrls, StdCtrls,
  PairSplitter, Grids, Math, AboutForm, GenericListUnit, ListIntegerUnit,
  ListProfitUnit;

type

  { TSortsMain }

  TSortsMain = class(TForm)
    SortButton: TButton;
    StatsGrid: TStringGrid;
    WarningImage: TImage;
    DatasType: TRadioGroup;
    InputLabel: TLabel;
    InputMemo: TMemo;
    ExitApp: TMenuItem;
    OutputMemo: TMemo;
    OutputLabel: TLabel;
    MainMenu: TMainMenu;
    FileItem: TMenuItem;
    AdoutItem: TMenuItem;
    OpenDialog: TOpenDialog;
    PageControl: TPageControl;
    MemoSplitter: TPairSplitter;
    InputSide: TPairSplitterSide;
    OutputSide: TPairSplitterSide;
    SortsType: TRadioGroup;
    SortingSplitter: TPairSplitter;
    SortsSide: TPairSplitterSide;
    DataSide: TPairSplitterSide;
    SaveDialog: TSaveDialog;
    SaveFile: TMenuItem;
    OpenFile: TMenuItem;
    SortingTab: TTabSheet;
    StatisticsTab: TTabSheet;
    procedure AdoutItemClick(Sender: TObject);
    procedure ExitAppClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure InputMemoChange(Sender: TObject);
    procedure OpenFileClick(Sender: TObject);
    procedure SaveFileClick(Sender: TObject);
    procedure SortButtonClick(Sender: TObject);
    procedure StatsGridDrawCell(Sender: TObject; aCol, aRow: integer;
      aRect: TRect; aState: TGridDrawState);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  SortsMain: TSortsMain;

implementation

{$R *.lfm}

{ TSortsMain }

procedure TSortsMain.OpenFileClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    InputMemo.Lines.LoadFromFile(OpenDialog.FileName);
end;

procedure TSortsMain.SaveFileClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    OutputMemo.Lines.SaveToFile(SaveDialog.FileName);
end;

procedure TSortsMain.SortButtonClick(Sender: TObject);
var
  GenList: IGList;
  SortMeth: TSortMethod;
  SortStat: TSortStats;
begin
  case DatasType.ItemIndex of
    0: GenList := TListInteger.Create;
    1: GenList := TListProfit.Create;
  end;

  case SortsType.ItemIndex of
    0: SortMeth := sfSelection;
    1: SortMeth := sfBubble;
    2: SortMeth := sfQuick;
    3: SortMeth := sfInsert;
    4: SortMeth := sfShell;
  end;

  try
    GenList.FromStrings(InputMemo.Lines);

    SortStat := GenList.Sort(SortMeth);
    with SortStat do
    begin
      sortName := SortsType.Items[SortsType.ItemIndex];
      typeName := DatasType.Items[DatasType.ItemIndex];
      ToStrGrid(StatsGrid);
    end;

    OutputMemo.Lines := GenList.ToStrings;
    StatisticsTab.TabVisible := True;
    WarningImage.Hide;
  except
    WarningImage.Show;
  end;
end;

procedure TSortsMain.StatsGridDrawCell(Sender: TObject; aCol, aRow: integer;
  aRect: TRect; aState: TGridDrawState);
var
  iWidth: integer;
begin
  with StatsGrid do
  begin
    iWidth := 10 + Canvas.TextWidth(Cells[aCol, aRow]);
    ColWidths[aCol] := Max(iWidth, ColWidths[aCol]);
  end;
end;

procedure TSortsMain.ExitAppClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TSortsMain.FormCreate(Sender: TObject);
var
  i: integer;
begin
  with StatsGrid do
  begin
    for i := 0 to ColCount - 1 do
      ColWidths[i] := 10 + Canvas.TextWidth(Cells[i, 0]);
  end;
end;

procedure TSortsMain.InputMemoChange(Sender: TObject);
begin
  SortButton.Enabled := Trim(InputMemo.Text) <> '';
  DatasType.ItemIndex := Min(Pos(',', InputMemo.Text), 1);
end;

procedure TSortsMain.AdoutItemClick(Sender: TObject);
begin
  AboutForm.About.ShowModal;
end;

end.
