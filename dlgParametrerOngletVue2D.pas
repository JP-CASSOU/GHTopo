unit dlgParametrerOngletVue2D;
// Date: 28/08/2012
// Dialogue pour paramétrage des onglets de la vue 2D
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
  CallDialogsStdVersion,
  StructuresDonnees, Classes, SysUtils, FileUtil, curredit, Forms, Controls,
  Graphics, Dialogs, StdCtrls, Buttons;

type

  { TfrmParametrerOngletVue2D }

  TfrmParametrerOngletVue2D = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    btnBGColor: TStaticText;
    btnQdrColor: TStaticText;
    chkCotation: TCheckBox;
    chkEntrances: TCheckBox;
    chkIDStations: TCheckBox;
    chkParois: TCheckBox;
    chkPolygonales: TCheckBox;
    chkQuadrillage: TCheckBox;
    chkRemplissage: TCheckBox;
    chkSections: TCheckBox;
    chkStations: TCheckBox;
    cmbTypeQuadrillage: TComboBox;
    editQdrSpc: TComboBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lbX1Y1: TStaticText;
    lbX2Y2: TStaticText;
    procedure btnBGColorClick(Sender: TObject);
    procedure btnQdrColorClick(Sender: TObject);
  private
    FC1, FC2: TPoint2Df; // sauvegarde des coordonnées de zone

    { private declarations }
  public
    { public declarations }
    procedure SetValuesOnglet(const O: TVue2DParams);
    function  GetValuesOnglet: TVue2DParams;
  end;

var
  frmParametrerOngletVue2D: TfrmParametrerOngletVue2D;

implementation

{$R *.lfm}

procedure TfrmParametrerOngletVue2D.btnBGColorClick(Sender: TObject);
begin
  btnBGColor.Color :=ChooseColor(btnBGColor.Color);
end;

procedure TfrmParametrerOngletVue2D.btnQdrColorClick(Sender: TObject);
begin
  btnQdrColor.Color:= ChooseColor(btnQdrColor.Color);
end;

procedure TfrmParametrerOngletVue2D.SetValuesOnglet(const O: TVue2DParams);
begin
 // éléments à dessiner
 if edPolygonals     in O.ongElementsDrawn then chkPolygonales.Checked := True;
 if edStations       in O.ongElementsDrawn then chkStations.Checked := True;
 if edCotation       in O.ongElementsDrawn then chkCotation.Checked := True;
 if edIDStations     in O.ongElementsDrawn then chkIDStations.Checked := True;
 if edWalls          in O.ongElementsDrawn then chkParois.Checked := True;
 if edFillGalerie    in O.ongElementsDrawn then chkRemplissage.Checked := True;
 if edCrossSections  in O.ongElementsDrawn then chkSections.Checked := True;
 if edQuadrilles     in O.ongElementsDrawn then chkQuadrillage.Checked := True;
 if edENTRANCES      in O.ongElementsDrawn then chkEntrances.Checked := True;

 // autres valeurs
 editQdrSpc.Text   := Format('%.0f',[O.ongQdrSpc]);
 btnBGColor.Color  := O.ongBackGround;
 btnQdrColor.Color := O.ongQdrColor;
 cmbTypeQuadrillage.ItemIndex := Ord(O.ongQdrType);
 // sauvegarde temp des coordonnées de la zone d'affichage (non modifiables)
 FC1.X := O.ongX1;  FC1.Y := O.ongY1;
 FC2.X := O.ongX2;  FC2.Y := O.ongY2;
 lbX1Y1.Caption := Format('Du coin: %.0f - %.0f', [FC1.X, FC1.Y]);
 lbX2Y2.Caption := Format('Au coin: %.0f - %.0f', [FC2.X, FC2.Y]);

end;

function TfrmParametrerOngletVue2D.GetValuesOnglet: TVue2DParams;
begin
  with Result do
  begin
    // couleur de fond
    ongBackGround      := btnBGColor.Color;
    // éléments à dessiner
    if chkEntrances.Checked then ongElementsDrawn := ongElementsDrawn + [edENTRANCES]
                            else ongElementsDrawn := ongElementsDrawn - [edENTRANCES];
    if chkStations.Checked then ongElementsDrawn := ongElementsDrawn + [edStations]
                              else ongElementsDrawn := ongElementsDrawn - [edStations];
    if chkCotation.Checked then ongElementsDrawn := ongElementsDrawn + [edCotation]
                              else ongElementsDrawn := ongElementsDrawn - [edCotation];
    if chkParois.Checked then ongElementsDrawn := ongElementsDrawn + [edWalls]
                              else ongElementsDrawn := ongElementsDrawn - [edWalls];
    if chkIDStations.Checked then ongElementsDrawn := ongElementsDrawn + [edIDStations]
                              else ongElementsDrawn := ongElementsDrawn - [edIDStations];
    if chkSections.Checked then ongElementsDrawn := ongElementsDrawn + [edCrossSections]
                              else ongElementsDrawn := ongElementsDrawn - [edCrossSections];
    if chkRemplissage.Checked then ongElementsDrawn := ongElementsDrawn + [edFillGalerie]
                              else ongElementsDrawn := ongElementsDrawn - [edFillGalerie];
    if chkQuadrillage.Checked then ongElementsDrawn := ongElementsDrawn + [edQuadrilles]
                              else ongElementsDrawn := ongElementsDrawn - [edQuadrilles];
    if chkPolygonales.Checked then ongElementsDrawn := ongElementsDrawn + [edPolygonals]
                              else ongElementsDrawn := ongElementsDrawn - [edPolygonals];
    // quadrillage
    ongQdrColor        := btnQdrColor.Color;

    case cmbTypeQuadrillage.ItemIndex of
      0: ongQdrType   := qtGRID;
      1: ongQdrType   := qtCROSS;
    else
      ongQdrType:= qtGRID;
    end;
    ongQdrSpc          := StrToFloatDef(Trim(editQdrSpc.Text), 10.00);
    if (ongQdrSpc < 10.00) then ongQdrSpc := 10.00; // valeur minimale de l'espacement
    // récupération des coordonnées de zone non modifiables
    ongX1 := FC1.X;   ongY1 := FC1.Y;
    ongX2 := FC2.X;   ongY2 := FC2.Y;
  end;
end;

end.

