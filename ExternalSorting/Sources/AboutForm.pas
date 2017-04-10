unit AboutForm;

{$mode objfpc}{$H+}

interface

uses
  Forms, ExtCtrls, StdCtrls;

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

