unit dlgSelectElementsDrawn;

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
  StructuresDonnees,
  Classes, SysUtils, FileUtil, LResources, Forms, Controls,
  Graphics, Dialogs, StdCtrls, Buttons;

type

  { TfrmSelectElementsDrawn }

  TfrmSelectElementsDrawn = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;

    chkPolygonales: TCheckBox;
    chkStations: TCheckBox;
    chkIDStations: TCheckBox;
    chkCotation: TCheckBox;
    chkQuadrilles: TCheckBox;
    chkRemplissage: TCheckBox;
    chkSections: TCheckBox;
    chkParois: TCheckBox;
    chkEntrances: TCheckBox;
    btnQdrColor: TStaticText;
    btnBGColor: TStaticText;
    editQdrSpc: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    procedure BitBtn1Click(Sender: TObject);
    procedure btnBGColorClick(Sender: TObject);
    procedure btnQdrColorClick(Sender: TObject);

    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    FMyElementsADessiner : TElementsDrawn;
     // quadrillage
    FQuadrillageSpc  : double;
    FQuadrillageColor: TColor;
    // couleur de fond
    FBackGround: TColor;

    procedure SetCbx;
  public
    { public declarations }
    procedure QSetElementsADessiner(const ED: TElementsDrawn);
    function  QGetElementsADessiner: TElementsDrawn;
    procedure QSetQuadrillageSpc(const Spacing: double);
    function  QGetQuadrillageSpc: Double;
    procedure QSetQuadrillageColor(const Col: TColor);
    function  QGetQuadrillageColor: TColor;
    procedure QSetBackGrdColor(const Col: TColor);
    function  QGetBackGrdColor: TColor;
    procedure GetValues;

    // getter et setter
    procedure SetValuesOnglet(const O: TVue2DParams);
    function  GetValuesOnglet: TVue2DParams;

  end;

var
  frmSelectElementsDrawn: TfrmSelectElementsDrawn;

implementation
procedure TfrmSelectElementsDrawn.QSetBackGrdColor(const Col: TColor);
begin
  FBackGround:=Col;
end;
function  TfrmSelectElementsDrawn.QGetBackGrdColor: TColor;
begin
  Result := FBackGround;
end;

procedure TfrmSelectElementsDrawn.QSetQuadrillageSpc(const Spacing: double);
begin
  FQuadrillageSpc := Spacing;
end;

function  TfrmSelectElementsDrawn.QGetQuadrillageSpc: Double;
begin
  Result := FQuadrillageSpc;
end;

procedure TfrmSelectElementsDrawn.QSetQuadrillageColor(const Col: TColor);
begin
  FQuadrillageColor:= Col;
end;

function  TfrmSelectElementsDrawn.QGetQuadrillageColor: TColor;
begin
  Result := FQuadrillageColor;
end;


function TfrmSelectElementsDrawn.QGetElementsADessiner: TElementsDrawn;
begin
  Result := FMyElementsADessiner;
end;
procedure TfrmSelectElementsDrawn.QSetElementsADessiner(const ED: TElementsDrawn);
begin
  FMyElementsADessiner := ED;
  SetCbx;
end;


procedure TfrmSelectElementsDrawn.BitBtn1Click(Sender: TObject);
begin
  GetValues;

end;

procedure TfrmSelectElementsDrawn.btnBGColorClick(Sender: TObject);
begin
  btnBGColor.Color :=ChooseColor(btnBGColor.Color);
end;

procedure TfrmSelectElementsDrawn.btnQdrColorClick(Sender: TObject);
begin
  btnQdrColor.Color:= ChooseColor(btnQdrColor.Color);
end;


procedure TfrmSelectElementsDrawn.FormShow(Sender: TObject);
begin
  SetCbx;
end;


procedure TfrmSelectElementsDrawn.SetCbx;
begin
 if edPolygonals     in FMyElementsADessiner then chkPolygonales.Checked := True;
 if edStations       in FMyElementsADessiner then chkStations.Checked := True;
 if edCotation       in FMyElementsADessiner then chkCotation.Checked := True;
 if edIDStations     in FMyElementsADessiner then chkIDStations.Checked := True;
 if edWalls          in FMyElementsADessiner then chkParois.Checked := True;
 if edFillGalerie    in FMyElementsADessiner then chkRemplissage.Checked := True;
 if edCrossSections  in FMyElementsADessiner then chkSections.Checked := True;
 if edQuadrilles     in FMyElementsADessiner then chkQuadrilles.Checked := True;
 if edENTRANCES      in FMyElementsADessiner then chkEntrances.Checked := True;

 // autres valeurs
 editQdrSpc.Text:=Format('%.0f',[FQuadrillageSpc]);
 btnBGColor.Color  := FBackGround;
 btnQdrColor.Color := FQuadrillageColor;

end;
procedure TfrmSelectElementsDrawn.GetValues;
begin
  if chkEntrances.Checked then FMyElementsADessiner := FMyElementsADessiner + [edENTRANCES]
                            else FMyElementsADessiner := FMyElementsADessiner - [edENTRANCES];
  if chkStations.Checked then FMyElementsADessiner := FMyElementsADessiner + [edStations]
                            else FMyElementsADessiner := FMyElementsADessiner - [edStations];
  if chkCotation.Checked then FMyElementsADessiner := FMyElementsADessiner + [edCotation]
                            else FMyElementsADessiner := FMyElementsADessiner - [edCotation];
  if chkParois.Checked then FMyElementsADessiner := FMyElementsADessiner + [edWalls]
                            else FMyElementsADessiner := FMyElementsADessiner - [edWalls];
  if chkIDStations.Checked then FMyElementsADessiner := FMyElementsADessiner + [edIDStations]
                            else FMyElementsADessiner := FMyElementsADessiner - [edIDStations];
  if chkSections.Checked then FMyElementsADessiner := FMyElementsADessiner + [edCrossSections]
                            else FMyElementsADessiner := FMyElementsADessiner - [edCrossSections];
  if chkRemplissage.Checked then FMyElementsADessiner := FMyElementsADessiner + [edFillGalerie]
                            else FMyElementsADessiner := FMyElementsADessiner - [edFillGalerie];
  if chkQuadrilles.Checked then FMyElementsADessiner := FMyElementsADessiner + [edQuadrilles]
                            else FMyElementsADessiner := FMyElementsADessiner - [edQuadrilles];
  if chkPolygonales.Checked then FMyElementsADessiner := FMyElementsADessiner + [edPolygonals]
                            else FMyElementsADessiner := FMyElementsADessiner - [edPolygonals];
  FQuadrillageColor := btnQdrColor.Color;
  FQuadrillageSpc   := StrToFloatDef(Trim(editQdrSpc.Text), 10.00);
  FBackGround       := btnBGColor.Color;
  //ShowMessage('GetValues');
end;

procedure TfrmSelectElementsDrawn.SetValuesOnglet(const O: TVue2DParams);
begin
  // éléments à dessiner

end;

function TfrmSelectElementsDrawn.GetValuesOnglet: TVue2DParams;
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
    if chkQuadrilles.Checked then ongElementsDrawn := ongElementsDrawn + [edQuadrilles]
                              else ongElementsDrawn := ongElementsDrawn - [edQuadrilles];
    if chkPolygonales.Checked then ongElementsDrawn := ongElementsDrawn + [edPolygonals]
                              else ongElementsDrawn := ongElementsDrawn - [edPolygonals];
    // quadrillage
    ongQdrColor        := btnQdrColor.Color;
    ongQdrSpc          := StrToFloatDef(Trim(editQdrSpc.Text), 10.00);
    if (ongQdrSpc < 10.00) then ongQdrSpc := 10.00; // valeur minimale de l'espacement

  end;
end;

initialization
{$I dlgSelectElementsDrawn.lrs}

end.

