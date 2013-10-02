//Gestion des Expés
// Habilité à modifier la base : OUI

unit CdrExpes;

{$mode delphi}{$H+}


interface

uses
  StructuresDonnees,
  ToporobotClasses,
  Common,
  CallDialogs,
  Classes, SysUtils, FileUtil, LResources, Forms, Dialogs, StdCtrls, MaskEdit;

type

  { TCadreExpes }

  TCadreExpes = class(TFrame)
    cmbModeDecl: TComboBox;
    editIncl: TEdit;
    editDeclination: TEdit;
    editObserv: TEdit;
    editSpeleometre: TEdit;
    editSpeleographe: TEdit;
    editIDExpe: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    editJJ: TMaskEdit;
    editMM: TMaskEdit;
    editAA: TMaskEdit;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    lbMaxIdx: TStaticText;
    lbColorExpe: TStaticText;
    procedure lbColorExpeClick(Sender: TObject);



  private
    { Déclarations privées }
    FExpe   : TExpe;
    FNumExpe: integer;


  public
    { Déclarations publiques }
    procedure PutExpeInForm(const EX: TExpe);
    function  GetExpeFromForm: TExpe;
end;

implementation

procedure TCadreExpes.lbColorExpeClick(Sender: TObject);
begin
  lbColorExpe.Caption := Format('%d', [ChooseToporobotColor(StrToIntDef(lbColorExpe.Caption, 0))]);
end;

// afficher une expé
procedure TCadreExpes.PutExpeInForm(const EX: TExpe);
begin
  FExpe := EX;
  editIDExpe.Text := Format('%d',[EX.IDExpe]);
  editJJ.Text := format('%.2d',[EX.JourExpe]);
  editMM.Text := format('%.2d',[EX.MoisExpe]);
  editAA.Text := format('%.2d',[EX.AnneeExpe]);
  editSpeleometre.Text  := EX.Speleometre;
  editSpeleographe.Text := EX.Speleographe;
  editObserv.Text       := EX.Commentaire;
  lbColorExpe.Caption   := IntToStr(EX.Couleur);
  editDeclination.Text  := Format('%.2f',[EX.Declinaison]);
  cmbModeDecl.ItemIndex := EX.ModeDecl;
  editIncl.Text         := Format('%.2f',[EX.Inclinaison]);
end;
// extraire une expé
function TCadreExpes.GetExpeFromForm: TExpe;
begin
  Result.IDExpe    := StrToIntDef(Trim(editIDExpe.Text), 1);
  Result.AnneeExpe := StrToIntDef(Trim(editAA.Text), 0);
  Result.JourExpe  := StrToIntDef(Trim(editJJ.Text), 0);
  Result.MoisExpe  := StrToIntDef(Trim(editMM.Text), 0);
  Result.Couleur   := StrToIntDef(lbColorExpe.Caption, 1);
  Result.Speleographe := PurgerAccents(trim(editSpeleographe.Text));
  Result.Speleometre  := PurgerAccents(trim(editSpeleometre.Text));
  Result.Declinaison  := StrToFloatDef(trim(editDeclination.Text), 0.00);
  Result.Commentaire  := PurgerAccents(trim(editObserv.Text));
  Result.ModeDecl     := cmbModeDecl.ItemIndex;
  Result.Inclinaison  := StrToFloatDef(trim(editIncl.Text), 0.00);

end;

initialization
  {$I CdrExpes.lrs}

end.

