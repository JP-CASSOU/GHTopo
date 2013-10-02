unit AboutBox;

// Date: 14/05/2012
{$INCLUDE CompilationParameters.inc}
interface
uses
  {$IFDEF FRENCH_MESSAGES}
     UnitMessages_fr,
  {$ENDIF}
  {$IFDEF ENGLISH_MESSAGES}
     UnitMessages_en,
  {$ENDIF}
  {$IFDEF SPANISH_MESSAGES}
     UnitMessages_sp,
  {$ENDIF}
  StructuresDonnees,
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Buttons, Grids, StdCtrls, curredit, types;

type

  { TdlgAbout }

  TdlgAbout = class(TForm)
    BitBtn1: TBitBtn;
    lbPlateforme: TLabel;
    lbAuthor: TLabel;
    lbLicence: TLabel;
    lbVersion: TLabel;
    lbGHTopoName: TLabel;
    lsbResolutionEcrans: TListBox;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure FormShow(Sender: TObject);
  private


  public
  end;

var
  dlgAbout: TdlgAbout;

implementation

{$R *.lfm}
{ TdlgAbout }

procedure TdlgAbout.FormShow(Sender: TObject);
const
  FMT_ECRANS_RESOL = 'Monitor: %d: %dx%d (%s)';
var
  M: TMonitor;
  W,H: integer;
  i: Integer;
  DateCompilation: TDateTime;
begin
  DateCompilation := FileDateToDateTime(FileAge(ParamStr(0)));
  self.Caption := AnsiToUtf8(rsABOUT);
  lbGHTopoName.Caption := AnsiToUtf8(rsGHTOPOEXENAME);
  lbVersion.Caption    := Format(rsGHTOPOVERSION, [DateToStr(DateCompilation)]);  //AnsiToUtf8(rsGHTOPOVERSION);
  lbAuthor.Caption     := AnsiToUtf8(rsGHTOPOAUTHOR);
  lbLicence.Caption    := AnsiToUtf8(rsGHTOPOLICENSE);
  lsbResolutionEcrans.Clear;
  for i:= 0 to Screen.MonitorCount - 1 do
  begin
    M := Screen.Monitors[i];
    W := M.Width;
    H := M.Height;
    lsbResolutionEcrans.Items.Add(AnsiToUtf8(Format(FMT_ECRANS_RESOL, [M.MonitorNum,W, H, BoolToStr(M.Primary, 'Primary', 'Auxiliary')])));

  end;
  lsbResolutionEcrans.ItemIndex := 0;
end;


end.

