unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, ComCtrls, StdCtrls, PairSplitter, Buttons, ActnList, Math,
  AboutForm, GenericListUnit, ListIntegerUnit, ListProfitUnit;

type

  { TSortsForm }

  TSortsForm = class(TForm)
    ClearButton: TButton;
    DatasType: TRadioGroup;
    SortButton: TButton;
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
    procedure ClearButtonClick(Sender: TObject);
    procedure ExitAppClick(Sender: TObject);
    procedure InputMemoChange(Sender: TObject);
    procedure OpenFileClick(Sender: TObject);
    procedure SaveFileClick(Sender: TObject);
    procedure SortButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  SortsForm: TSortsForm;

implementation

{$R *.lfm}

{ TSortsForm }

procedure TSortsForm.OpenFileClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    InputMemo.Lines.LoadFromFile(OpenDialog.FileName);
end;


procedure TSortsForm.SaveFileClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    OutputMemo.Lines.SaveToFile(SaveDialog.FileName);
end;

procedure TSortsForm.SortButtonClick(Sender: TObject);
var
  GenList: IGList;
begin
  case DatasType.ItemIndex of
    0: GenList := TListInteger.Create;
    1: GenList := TListProfit.Create;
  end;
  GenList.FromStrings(InputMemo.Lines);
  GenList.Sort;
  OutputMemo.Lines := GenList.ToStrings;
end;

procedure TSortsForm.ExitAppClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TSortsForm.InputMemoChange(Sender: TObject);
begin
  SortButton.Enabled := InputMemo.Lines.Count > 0;
  DatasType.ItemIndex := Min(Pos(',', InputMemo.Lines.Text), 1);
end;

procedure TSortsForm.AdoutItemClick(Sender: TObject);
begin
  AboutForm.About.ShowModal;
end;

procedure TSortsForm.ClearButtonClick(Sender: TObject);
begin
  InputMemo.Clear;
  OutputMemo.Clear;
end;

end.
