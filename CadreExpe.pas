unit CadreExpe;
// Date: 16/05/2013
// Statut: OK
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
  CallDialogsStdVersion,
  UnitClassPalette,
  // pour la déclinaison magnétique
  {$IFDEF MSWINDOWS}
    UnitWrapperDeclimag,
  {$ENDIF}

  {$IFDEF LINUX}
    UnitWrapperDeclimagLinux,
  {$ENDIF}


  math, Classes, SysUtils, FileUtil, Forms, Dialogs, Controls, StdCtrls,
  Buttons, curredit;

type

  { TCdrExpe }

  TCdrExpe = class(TFrame)
    btnCalcDeclimag: TButton;
    cmbModeDecl: TComboBox;
    editCouleur: TCurrencyEdit;
    edit100Declimag: TCurrencyEdit;
    editInclinaison: TCurrencyEdit;
    editSpeleometre: TEdit;
    editMonth: TCurrencyEdit;
    editSpeleographe: TEdit;
    editYear: TCurrencyEdit;
    editNoExpe: TCurrencyEdit;
    editDay: TCurrencyEdit;
    Label1: TLabel;
    lblColor: TLabel;
    lblObs: TLabel;
    lblInclin: TLabel;
    lblDeclimag: TLabel;
    lblSpeleometre: TLabel;
    lblSpeleographe: TLabel;
    lblDate: TLabel;
    lblSeance: TLabel;
    editComment: TMemo;
    lbColorSeance: TStaticText;
    procedure btnCalcDeclimagClick(Sender: TObject);
    procedure cmbModeDeclChange(Sender: TObject);
    procedure editCouleurKeyPress(Sender: TObject; var Key: char);
    procedure FrameEnter(Sender: TObject);
    procedure lbColorSeanceClick(Sender: TObject);
  private
    FModifie: boolean;
    FExpe : TExpe;
    procedure InitCaptions;

    procedure PutExpeInForm;

    procedure SetLbColor(const IdxColor: integer);

    { private declarations }
  public
    { public declarations }
    procedure SetExpe(const E: TExpe; const DoInitCaptions: boolean);
    function GetExpeFromForm: TExpe;
    function IsModified: boolean;
    procedure SetModified(const m: boolean);
  end; 

implementation

{$R *.lfm}
// appelée une seule fois dans leurs feuilles respectives
// --> initialisation du cadre effectuée en même temps
procedure TCdrExpe.SetExpe(const E: TExpe; const DoInitCaptions: boolean);
begin
  FExpe := E;
  if (DoInitCaptions) then InitCaptions;
  PutExpeInForm;
end;

procedure TCdrExpe.lbColorSeanceClick(Sender: TObject);
var
  EWE: Integer;
begin
  EWE := SelectionCouleurToporobot(editCouleur.AsInteger);
  SetLbColor(EWE);
end;

procedure TCdrExpe.editCouleurKeyPress(Sender: TObject; var Key: char);
begin
  if (Key = #13) then SetLbColor(editCouleur.AsInteger);
end;

procedure TCdrExpe.FrameEnter(Sender: TObject);
begin
  FModifie := True;
  AfficherMessage('Element modifie');
end;

procedure TCdrExpe.btnCalcDeclimagClick(Sender: TObject);
var
  PT: TPoint2Df;
  ResultatCalcul,
  Declinaison: double;
begin
  ResultatCalcul := 0.00;
  Declinaison    := 0.00;
  CallCalculette('', PT, ResultatCalcul, Declinaison);
  edit100Declimag.Value:= GetTOPOROBOTDecliMag(Declinaison);
end;

procedure TCdrExpe.cmbModeDeclChange(Sender: TObject);
begin
end;

procedure TCdrExpe.InitCaptions;
begin
 lblSeance.Caption       := AnsiToUtf8(rsCDR_EXPE_SEANCE);
 lblDate.Caption         := AnsiToUtf8(rsCDR_EXPE_DATE);
 lblSpeleometre.Caption  := AnsiToUtf8(rsCDR_EXPE_SPELEOMETRE);
 lblSpeleographe.Caption := AnsiToUtf8(rsCDR_EXPE_SPELEOGRAPHE);
 lblDeclimag.Caption     := AnsiToUtf8(rsCDR_EXPE_DECLIMAG);
 lblInclin.Caption       := AnsiToUtf8(rsCDR_EXPE_INCLIN);
 lblColor.Caption        := AnsiToUtf8(rsCOLOR);
 lblObs.Caption          := AnsiToUtf8(rsLBL_COMMENTS);
 btnCalcDeclimag.Caption := AnsiToUtf8(rsDLG_CALC_DOCALC);
 cmbModeDecl.Enabled     := False;
end;

procedure TCdrExpe.PutExpeInForm;
begin
  FModifie:= False;
  editNoExpe.AsInteger    := FExpe.IDExpe;
  editDay.AsInteger       := FExpe.JourExpe;
  editMonth.AsInteger     := FExpe.MoisExpe;
  editYear.AsInteger      := FExpe.AnneeExpe;
  editSpeleometre.Text    := AnsiToUtf8(FExpe.Speleometre);
  editSpeleographe.Text   := AnsiToUtf8(FExpe.Speleographe);
  edit100Declimag.Value   := FExpe.Declinaison;
  editInclinaison.Value   := FExpe.Inclinaison;

  //editCouleur.AsInteger:=FExpe.Couleur;
  SetLbColor(FExpe.Couleur);
  cmbModeDecl.ItemIndex   := 0; // FExpe.ModeDecl; // volontairement non supporté par GHTopo
  editComment.Text:=FExpe.Commentaire;
end;
function TCdrExpe.GetExpeFromForm: TExpe;
var
  E: TExpe;
begin
  with E do begin
    IDExpe      := editNoExpe.AsInteger;
    JourExpe    := editDay.AsInteger;
    MoisExpe    := editMonth.AsInteger;
    AnneeExpe   := editYear.AsInteger;
    Speleometre := editSpeleometre.Text;
    Speleographe:= editSpeleographe.Text;
    Inclinaison := editInclinaison.Value;
    Declinaison := edit100Declimag.Value;
    Couleur     := editCouleur.AsInteger;
    ModeDecl    := 0; // cmbModeDecl.ItemIndex;    // volontairement non supporté par GHTopo
    JourExpe    := editDay.AsInteger;
    MoisExpe    := editMonth.AsInteger;
    AnneeExpe   := editYear.AsInteger;
    Commentaire := editComment.Text;
  end;
  Result:=E;
end;
procedure TCdrExpe.SetLbColor(const IdxColor: integer);
begin
  editCouleur.AsInteger:=IdxColor;
  with TPalette256.Create do begin
    try
      GenerateTOPOROBOTPalette;
      lbColorSeance.Color   := GetColorByIndex(IdxColor);
      editCouleur.AsInteger := IdxColor;
    finally
      Free;
    end;
  end;
end;
function TCdrExpe.IsModified: boolean;
begin
  Result := FModifie;
end;

procedure TCdrExpe.SetModified(const m: boolean);
begin
  FModifie := m;
end;


end.

