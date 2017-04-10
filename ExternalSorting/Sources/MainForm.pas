unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Dialogs, Menus, ExtCtrls, ComCtrls, StdCtrls,
  PairSplitter, Grids, Math, AboutForm, GenericListUnit, ListIntegerUnit,
  ListProfitUnit, Controls;

type

  { TSortsMain }

  TSortsMain = class(TForm)
    InputEdit: TEdit;
    OutputEdit: TEdit;
    SortButton: TButton;
    StatsGrid: TStringGrid;
    WarningImage: TImage;
    DatasType: TRadioGroup;
    InputLabel: TLabel;
    ExitApp: TMenuItem;
    OutputLabel: TLabel;
    MainMenu: TMainMenu;
    FileItem: TMenuItem;
    AdoutItem: TMenuItem;
    OpenDialog: TOpenDialog;
    PageControl: TPageControl;
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
    procedure FileEditChange(Sender: TObject);
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
  InputFileName, OutputFileName: string;

implementation

{$R *.lfm}

{ TSortsMain }

procedure TSortsMain.OpenFileClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    InputEdit.Text := OpenDialog.FileName;
end;

procedure TSortsMain.SaveFileClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    OutputEdit.Text := SaveDialog.FileName;
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
    0: SortMeth := sfNaturalFusion;
    1: SortMeth := sfSimpleFusion;
  end;

  try
    SortStat := GenList.Sort(InputEdit.Text, OutputEdit.Text, SortMeth);

    with SortStat do
    begin
      sortName := SortsType.Items[SortsType.ItemIndex];
      typeName := DatasType.Items[DatasType.ItemIndex];
      ToStrGrid(StatsGrid);
    end;

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

procedure TSortsMain.FileEditChange(Sender: TObject);
begin
  SortButton.Enabled := (Trim(InputEdit.Text) <> '') and
    (Trim(OutputEdit.Text) <> '');
end;

procedure TSortsMain.AdoutItemClick(Sender: TObject);
begin
  AboutForm.About.ShowModal;
end;

end.
