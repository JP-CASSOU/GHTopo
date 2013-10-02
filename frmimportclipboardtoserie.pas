unit frmImportClipboardToSerie;
// Importation 'intelligente' (lol) du presse-papiers vers la grille de saisie
// des points topo
//
// 18/09/2013 18:31:43

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
  Common,
  StructuresDonnees,
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  Clipbrd, LCLType,
  Grids, ComCtrls, StdCtrls;

type

  { TdlgImportClipboardToSerie }

  TdlgImportClipboardToSerie = class(TForm)
    chkPremLigneIsTitres: TCheckBox;
    grdDonnees: TStringGrid;
    memoClipboardBrut: TMemo;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    procedure PasteFromClipboard;
  public
    { public declarations }
  end;

var
  dlgImportClipboardToSerie: TdlgImportClipboardToSerie;

implementation

{$R *.lfm}

{ TdlgImportClipboardToSerie }

procedure TdlgImportClipboardToSerie.FormShow(Sender: TObject);
begin
  self.Caption := rsDLG_CLIPBRD_PTS_TITLE;

  // presse papiers
  PasteFromClipboard;

end;

procedure TdlgImportClipboardToSerie.PasteFromClipboard;
begin
  if (Not GRDCollerDepuisClipBoard(grdDonnees, chkPremLigneIsTitres.Checked)) then Exit;

end;

end.

