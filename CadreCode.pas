unit CadreCode;
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
  Common, StructuresDonnees,
  CallDialogsStdVersion,
  Classes, SysUtils, FileUtil, Forms, Controls,
  StdCtrls, curredit;

type

  { TCdrCode }

  TCdrCode = class(TFrame)
    cmbUB: TComboBox;
    cmbUC: TComboBox;
    cmbAzimutDirecte: TComboBox;
    cmbPenteDirecte: TComboBox;
    cmbPosZero: TComboBox;
    editFactLong: TCurrencyEdit;
    editPsiL: TCurrencyEdit;
    editPsiAz: TCurrencyEdit;
    editPsiP: TCurrencyEdit;
    editAngleLimite: TCurrencyEdit;
    editNoCode: TCurrencyEdit;
    editCommentaires: TEdit;
    lbErrAz: TStaticText;
    lbErrInc: TStaticText;
    lblLimit: TLabel;
    lblPrecision: TLabel;
    lblObs: TLabel;
    lblZero: TLabel;
    lblLength: TLabel;
    lbViseeInvDir: TLabel;
    lblClino: TLabel;
    lblCompas: TLabel;
    lblNCode: TLabel;
    lbCodeComp: TStaticText;
    lbCodeIncl: TStaticText;
    procedure cmbUBChange(Sender: TObject);
    procedure cmbUCChange(Sender: TObject);
    procedure FrameEnter(Sender: TObject);
  private
    FCode: TCode;
    FModifie: boolean;

    procedure InitCaptions;
    procedure PutCodeInForm;

    { private declarations }
  public
    { public declarations }
    procedure SetCode(const C: TCode; const DoInitCaptions: boolean);
    function GetCodeFromForm: TCode;
    function  IsModified: boolean;
    procedure SetModified(const m: boolean);
  end; 

implementation

{$R *.lfm}
procedure TCdrCode.SetCode(const C: TCode; const DoInitCaptions: boolean);
begin
  FCode := C;
  if (DoInitCaptions) then InitCaptions;
  PutCodeInForm;
end;

procedure TCdrCode.cmbUBChange(Sender: TObject);
begin
  case cmbUB.ItemIndex of
    0: lbErrAz.Caption:='Az (gr)';
    1: lbErrAz.Caption:='Az (°)';
  end;
end;

procedure TCdrCode.cmbUCChange(Sender: TObject);
begin
  case cmbUC.ItemIndex of
    0: lbErrInc.Caption:='Inc (gr)';  // 400
    1: lbErrInc.Caption:='Inc (°)';   // 360
    2: lbErrInc.Caption:='Inc (%)';   // 370
    3: lbErrInc.Caption:='Z (m)';   // 380
  else lbErrInc.Caption:='Inc (gr)';
  end;
end;

procedure TCdrCode.FrameEnter(Sender: TObject);
begin
  FModifie := True;
  AfficherMessage('Element modifie');
end;

procedure TCdrCode.InitCaptions;
  procedure SetCmbDirInv(var CB: TComboBox);
  begin
    with CB do begin
      Clear;
      Items.Add(AnsiToUtf8(rsCDR_CODES_VDIRECT));
      Items.Add(AnsiToUtf8(rsCDR_CODES_VINVERSE));
      ItemIndex := 0;
    end;
  end;
begin

  lblNCode.Caption  := AnsiToUtf8(rsCDR_CODES_NUMERO);

  // unité boussole
  lblCompas.Caption := AnsiToUtf8(rsCDR_CODES_GRADCOMPAS);
  with cmbUB do begin
    Clear;
    Items.Add(AnsiToUtf8(rsCDR_CODES_CMBUNIT_0));
    Items.Add(AnsiToUtf8(rsCDR_CODES_CMBUNIT_1));
  end;
  lblClino.Caption  := AnsiToUtf8(rsCDR_CODES_GRADCLINO);
  // unité clino
  with cmbUC do begin
    Clear;
    Items.Add(AnsiToUtf8(rsCDR_CODES_CMBUNIT_0));
    Items.Add(AnsiToUtf8(rsCDR_CODES_CMBUNIT_1));
    Items.Add(AnsiToUtf8(rsCDR_CODES_CMBUNIT_2));
    Items.Add(AnsiToUtf8(rsCDR_CODES_CMBUNIT_3));
  end;
  // visées directes/inverse
  lbViseeInvDir.Caption  := AnsiToUtf8(rsCDR_CODES_VISEE);
  SetCmbDirInv(cmbAzimutDirecte);
  SetCmbDirInv(cmbPenteDirecte);
  lblLength.Caption:=AnsiToUtf8(rsCDR_CODES_FACT);
  lblZero.Caption  :=AnsiToUtf8(rsCDR_CODES_POSZERO);
  with cmbPosZero do begin
    Clear;
    Items.Add(AnsiToUtf8(rsCDR_CODES_CMBZERO_0));
    Items.Add(AnsiToUtf8(rsCDR_CODES_CMBZERO_1));
    Items.Add(AnsiToUtf8(rsCDR_CODES_CMBZERO_2));
  end;
  lblLimit.Caption := AnsiToUtf8(rsCDR_CODES_ANGLIMIT);
  lblPrecision.Caption:=AnsiToUtf8(rsCDR_CODES_PRECISION);
  lblObs.Caption:= AnsiToUtf8(rsLBL_COMMENTS);

end;

function TCdrCode.GetCodeFromForm: TCode;
var
  c: TCode;
begin
  C.IDCode:=editNoCode.AsInteger;
  //C.IDCode:=StrToInt(editNoCode.Text);
  case cmbUB.ItemIndex of
    0: C.GradAz:=400.00;
    1: C.GradAz:=360.00;
  end;
  case cmbUC.ItemIndex of
    0: C.GradInc:=400.00;
    1: C.GradInc:=360.00;
    2: C.GradInc:=370.00;
    3: C.GradInc:=380.00;
  end;
  // TODO: Revoir ce code, qui est provisoire
  // visée directe ou inverse: azimuts: retrancher 10 du code azimut
  if cmbAzimutDirecte.ItemIndex = 1 then C.GradAz  := C.GradAz - 10.0;
  // visée directe ou inverse: pentes
  if cmbPenteDirecte.ItemIndex  = 1 then C.GradInc := C.GradInc - 10.0;

  // position du zéro
  case cmbPosZero.ItemIndex of
    0: C.Gradinc:=C.Gradinc-1.0; // nadiral
    1: ;                         // horizontal
    2: C.Gradinc:=C.Gradinc+1.0; // zénithal
  end;

  C.PsiL  :=editPsiL.Value;
  C.PsiAz :=editPsiAZ.Value;
  C.PsiP  :=editPsiP.Value;

  C.FactLong:=editFactLong.Value;
  C.AngLimite:=editAngleLimite.Value;
  // type de galeries
  C.TypeGalerie :=0;//cmbTypeGalerie.ItemIndex;
  //-----------------
  C.Commentaire:=editCommentaires.Text;
  Result:=C;
end;

function TCdrCode.IsModified: boolean;
begin
  Result := FModifie;
end;

procedure TCdrCode.SetModified(const m: boolean);
begin
  FModifie := m;
end;

procedure TCdrCode.PutCodeInForm;
var
  ucc: integer;
begin;
  FModifie := False;

  // valeurs internes
  lbCodeComp.Caption := Format('%d',[Round(FCode.GradAz)]);
  lbCodeIncl.Caption := Format('%d',[Round(FCode.GradInc)]);

  // visées directes par défaut
  cmbAzimutDirecte.ItemIndex := 0;
  cmbPenteDirecte.ItemIndex  := 0;
  //---------------------------------
  editNoCode.AsInteger:=FCode.IDCode;
  // Graduation du compas
  ucc:=Round(FCode.GradAz);
  case ucc of
    389, 399, 400: begin cmbUB.ItemIndex:=0; lbErrAz.Caption:='Az (gr)'; end;
    349, 359, 360: begin cmbUB.ItemIndex:=1; lbErrAz.Caption:='Az (°)'; end;
    // visées inverses
    //350, 390:
    350: begin
           cmbUB.ItemIndex            := 1;
           lbErrAz.Caption            := 'Az (°)';
           cmbAzimutDirecte.ItemIndex := 1;
         end;
    390: begin
           cmbUB.ItemIndex             := 0;
           lbErrAz.Caption            := 'Az (gr)';
           cmbAzimutDirecte.ItemIndex := 1;
         end;
  else cmbUB.ItemIndex:=0;
  end;
  // Graduation du clinomètre
  ucc:=Round(FCode.GradInc);
  case ucc of
    400: begin cmbUC.ItemIndex:=0; lbErrInc.Caption:='Inc (gr)'; end;
    360: begin cmbUC.ItemIndex:=1; lbErrInc.Caption:='Inc (°)'; end;
    370: begin cmbUC.ItemIndex:=2; lbErrInc.Caption:='Inc (%)'; end;
    380: begin cmbUC.ItemIndex:=3; lbErrInc.Caption:='Inc (m)'; end;
    // visées inverses
    350: begin
           cmbUC.ItemIndex            := 1;
           lbErrInc.Caption           := 'Inc (°)';
           cmbPenteDirecte.ItemIndex := 1;
         end;
    390: begin
           cmbUC.ItemIndex            := 0;
           lbErrInc.Caption           := 'Inc (gr)';
           cmbPenteDirecte.ItemIndex  := 1;
         end;
  else   begin cmbUC.ItemIndex:=0; lbErrInc.Caption:='Inc (gr)'; end;
  end;
  // Position du zéro
  case ucc of
    359, 399: cmbPosZero.ItemIndex:=0;
    360, 400: cmbPosZero.ItemIndex:=1;
    361, 401: cmbPosZero.ItemIndex:=2;
  else
  end;
  // Facteur de correction des longueurs
  editFactLong.Value:=FCode.FactLong;
  // modifié 26.08.04
  // Angle Limite
  editAngleLimite.Value:=FCode.AngLimite;
  // Précision des instruments
  editPsiL.Value  :=FCode.PsiL;
  editPsiAZ.Value :=FCode.PsiAZ;
  editPsiP.Value  :=FCode.PsiP;
  // Commentaires
  editCommentaires.Text:=AnsiToUtf8(FCode.Commentaire);
end;

end.

