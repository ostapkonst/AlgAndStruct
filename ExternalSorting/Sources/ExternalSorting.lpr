program ExternalSorting;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MainForm, AboutForm, ListIntegerUnit, ListProfitUnit,
  GenericListUnit;

{$R *.res}

begin
  Application.Title:='ExternalSorting';
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TSortsMain, SortsMain);
  Application.CreateForm(TAbout, About);
  Application.Run;
end.

