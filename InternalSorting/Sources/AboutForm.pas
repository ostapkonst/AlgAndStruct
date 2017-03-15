unit AboutForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  { TAbout }

  TAbout = class(TForm)
    SortImage: TImage;
    CopyrightLabel: TLabel;
    DescriptionLabel: TLabel;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  About: TAbout;

implementation

{$R *.lfm}

end.

