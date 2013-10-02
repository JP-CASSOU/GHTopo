unit CdrCodes;

{$mode DELPHI}{$H+}

interface

uses
  StructuresDonnees,
  ToporobotClasses,
  Common,
  Dialogs,
  Classes, SysUtils, FileUtil, LResources, Forms, StdCtrls;

type

  { TCadreCodes }

  TCadreCodes = class(TFrame)
    cmbUB: TComboBox;
    cmbAzimutDirecte: TComboBox;
    cmbPenteDirecte: TComboBox;
    cmbUC: TComboBox;
    cmbPosZero: TComboBox;
    editWL: TEdit;
    editWAz: TEdit;
    editWP: TEdit;
    editIDCode: TEdit;
    editObsCode: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    lbMaxIdx: TStaticText;


  private
    { Déclarations privées }
    //FCode   : TCode;
  public
    { Déclarations publiques }
    procedure PutCodeInForm(const CD: TCode);
    function  GetCodeFromForm: TCode;
  end;

implementation


procedure TCadreCodes.PutCodeInForm(const CD: TCode);
var ucc : integer;
begin
  editIDCode.Text    := Format('%d', [CD.IDCode]);
  editObsCode.Text   := PurgerAccents(CD.Commentaire);
  editWL.Text        := Format('%.2f',[CD.PsiL]);
  editWAz.Text       := Format('%.2f',[CD.PsiAz]);
  editWP.Text        := Format('%.2f',[CD.PsiP]);

  ucc:=Round(CD.GradAz);
  case ucc of
    389, 399, 400: begin cmbUB.ItemIndex:=0; end; //lbErrAz.Caption:='Az (gr)'; end;
    349, 359, 360: begin cmbUB.ItemIndex:=1; end; //lbErrAz.Caption:='Az (°)'; end;

    // TODO: Revoir ce code, qui est provisoire
    // visées inverses
    //350, 390:
    350: begin
           cmbUB.ItemIndex            := 1;
           //lbErrAz.Caption            := 'Az (°)';
           cmbAzimutDirecte.ItemIndex := 1;
         end;
    390: begin
           cmbUB.ItemIndex             := 0;
           //lbErrAz.Caption            := 'Az (gr)';
           cmbAzimutDirecte.ItemIndex := 1;
         end;


  else cmbUB.ItemIndex:=0;
  end;
  // Graduation du clinomètre
  ucc:=Round(CD.GradInc);
  case ucc of
    400: begin cmbUC.ItemIndex:=0; end; //lbErrInc.Caption:='Inc (gr)'; end;
    360: begin cmbUC.ItemIndex:=1; end; //lbErrInc.Caption:='Inc (°)'; end;
    370: begin cmbUC.ItemIndex:=2; end; //lbErrInc.Caption:='Inc (%)'; end;
    380: begin cmbUC.ItemIndex:=3; end; // lbErrInc.Caption:='Inc (m)'; end;
    // TODO: Revoir ce code, qui est provisoire
    // visées inverses
    350: begin
           cmbUC.ItemIndex            := 1;
           //lbErrInc.Caption           := 'Inc (°)';
           cmbPenteDirecte.ItemIndex := 1;

         end;
    390: begin
           cmbUC.ItemIndex            := 0;
           //lbErrInc.Caption           := 'Inc (gr)';
           cmbPenteDirecte.ItemIndex  := 1;
         end;
  else   begin cmbUC.ItemIndex:=0; end; //lbErrInc.Caption:='Inc (gr)'; end;
  end;
  // Position du zéro
  case ucc of
    359, 399: cmbPosZero.ItemIndex:=0;
    360, 400: cmbPosZero.ItemIndex:=1;
    361, 401: cmbPosZero.ItemIndex:=2;

  else
  end;

end;
function TCadreCodes.GetCodeFromForm: TCode;
begin
  Result.IDCode      := StrToIntDef(Trim(editIDCode.Text), 0);
  Result.Commentaire := PurgerAccents(trim(editObsCode.Text));
  Result.PsiL        := StrToFloatDef(Trim(editWL.Text), 1.00);
  Result.PsiAZ       := StrToFloatDef(Trim(editWAz.Text), 1.00);
  Result.PsiP        := StrToFloatDef(Trim(editWP.Text), 1.00);



end;

initialization
  {$I CdrCodes.lrs}

end.


