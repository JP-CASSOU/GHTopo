unit CentreImpression;
// Centre d'impression
// Situation au 14/05/2012:
// - Aperçu du plan: OK
// - Paramétrage imprimante: OK
// - Calculs d'échelle: Semble OK
// - Affichage des pages avec mention de pages vides: OK
// - Activer/Désactiver page sur dbl_clic: OK
// - Fonction PrintAPage: OK - Des détails à revoir
// - Impression des atlas: OK - A enrichir
// - Filtres: OK
// 10/09/2012: Depuis Lazarus 1.0, l'unité OSPrinters est indispensable
// TODO: Implémenter le support Linux
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
  Math,
  Common, StructuresDonnees, UnitEntites, CadreFiltres,
  Printers, OSPrinters, // OSPrinters est INDISPENSABLE ici !!
  Classes, SysUtils, FileUtil,
  Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, curredit, types,
  LCLType, ComCtrls, Buttons; // pour odSelected;

type TPageProperties = record
  PrinterName: string;
  Portrait: boolean;
  PageWidthInMM: double;
  PageHeightInMM: double;
end;
type TQdrType = (qtGRID, qtCROSS, qtPOINTS);
type

  { TdlgPrintingCenter }

  TdlgPrintingCenter = class(TForm)
    BitBtn1: TBitBtn;
    btnPreview: TButton;
    btnStartImpression: TButton;
    CdrMetaFiltre1: TCdrMetaFiltre;
    chkRegle: TCheckBox;
    chkGrpElementsDessin: TCheckGroup;
    cmbPrinters: TComboBox;
    cmbTypeQuadrillage: TComboBox;
    cmbUsualQdrSpacing: TComboBox;
    cmbEchelle: TComboBox;
    editTailleRegle: TCurrencyEdit;
    GroupBox1: TGroupBox;
    grbPrinterNames: TGroupBox;
    grbxApercu: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    lbEchelle: TLabel;
    lbOrientation: TLabel;
    lbPageFormat: TLabel;
    lbPrintCurrent: TLabel;
    lbQdrSpacing: TLabel;
    lbNbPages: TLabel;
    lbPrinterName: TLabel;
    PaintBoxVue: TPaintBox;
    pnlCadre: TPanel;
    lbColorQuadrillage: TStaticText;
    lbMouseCoordinates: TStaticText;
    pnlProgressPrinting: TPanel;
    progbarPrinting: TProgressBar;
    procedure btnFiltresClick(Sender: TObject);
    procedure btnPreviewClick(Sender: TObject);
    procedure btnStartImpressionClick(Sender: TObject);
    procedure chkGrpElementsDessinClick(Sender: TObject);
    procedure chkGrpElementsDessinDblClick(Sender: TObject);
    procedure chkGrpElementsDessinItemClick(Sender: TObject; Index: integer);
    procedure chkRegleChange(Sender: TObject);
    procedure cmbEchelleChange(Sender: TObject);
    procedure cmbPrintersChange(Sender: TObject);
    procedure cmbTypeQuadrillageChange(Sender: TObject);
    procedure cmbUsualQdrSpacingChange(Sender: TObject);

    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbColorQuadrillageClick(Sender: TObject);
    procedure PaintBoxVueDblClick(Sender: TObject);
    procedure PaintBoxVueMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBoxVuePaint(Sender: TObject);
  private
    { private declarations }
     { Déclarations privées }
    // fichiers
    //FFichierTOP   : string;
    FPrinter      : TPrinter;
    // éléments à dessiner
    FTableEntites : TTableDesEntites;
    //FListeAnnotations: TAnnotationList;
    //FElementsDrawn: TElementsDrawn;
    // modes de représentation
    FModeRepresentationGaleries: TModeRepresentationGaleries;
    // variables internes
    // coordonnées internes de la souris dans la vue
    FPP: TPoint;
    // limites du dessin
    FRXMini      : double;
    FRXMaxi      : double;
    FRYMini      : double;
    FRYMaxi      : double;
    // variables diverses

    FRappScrReal : double;
    FInvRappScrReal: double;
    // nombre de pages sur X et Y
    // tableau des pages dessinées
    FNbPagesX         : integer;
    FNbPagesY         : integer;
    FTableauPagesDrawn: array of array of boolean;
    FDoDrawPages      : boolean;
    // échelle du dessin
    FEchelle: double;
    // quadrillage
    FQdrEspc       : double;
    FQdrCrossSize  : double;
    FQdrType       : TQdrType;
    FQdrColor      : TColor;
    // Règle
    FRegleDisplayed: boolean;
    FRegleSize     : double;
    // caractéristiques de la page
    FPageProperties: TPageProperties;
    // dessiner les annotations si elles sont chargées
    //FDoDrawAnnotations: boolean;
    // Fontes pour les stations
    FAttributsTexteStations: TTexteAttributs;
    FAttributsTexteCotes   : TTexteAttributs;

    procedure CalcIndexPageIncludingPt(const Pt: TPoint2Df; var IdxX,
      IdxY: integer);
    procedure CalcNbPagesAndEmpty;
    procedure ChoosePrinter(const Idx: integer);
    procedure DrawApercu;
    function GetCoordsMonde(const PP: TPoint): TPoint2Df;
    function GetCoordsPlan(const PM: TPoint2Df): TPoint;


    procedure GetDataFromDialog;
    function GetElementDrawn: TElementsDrawn;
    function PreSelectPDFPrinter: integer;
    procedure PrintAPage(const PageProp: TPageProperties;
      const Echelle: double; const L, C: integer;
      const TextCotationHeight: double; const TextIDStationHeight: double);
    procedure PrintTopo;
    procedure Recadrer;
    procedure RegenererVue;
    procedure SetElementDrawn(const FE: TElementsDrawn);
    procedure SetMyCurrentPrinter;


  public
    { public declarations }
    function Initialise(const F: string): Boolean;

  end; 

var
  dlgPrintingCenter: TdlgPrintingCenter;

implementation

{$R *.lfm}

function Millimetres2PixelsXf(const PRN: TPrinter; const Millims: double): integer;
begin
  Result:=trunc(100* PRN.XDPI * Millims / 25.40) div 100;
end;
function Millimetres2PixelsYf(const PRN: TPrinter; const Millims: double): integer;
begin
  Result:=trunc(100 * PRN.YDPI* Millims / 25.40) div 100;
end;
function Pixels2MillimetresX(const PRN: TPrinter; const Pixls: integer): Double;
begin
  Result:=25.40 * Pixls / PRN.XDPI;
end;
function Pixels2MillimetresY(const PRN: TPrinter; const Pixls: integer): Double;
begin
  Result:=25.40 * Pixls / PRN.YDPI;
end;

function GetPageProperties(const PRN: TPrinter): TPageProperties;
begin
  with Result do begin
    PrinterName := PRN.Printers[PRN.PrinterIndex];
    Portrait := (PRN.Orientation = poPortrait);
    PageWidthInMM  := Pixels2MillimetresX(PRN, PRN.PageWidth);
    PageHeightInMM := Pixels2MillimetresY(PRN, PRN.PageHeight);
  end;
end;

(* TdlgPrintingCenter *)
// options de dessin
procedure TdlgPrintingCenter.SetElementDrawn(const FE: TElementsDrawn);
begin
  chkGrpElementsDessin.Checked[0] := (edPolygonals in FE);
  chkGrpElementsDessin.Checked[1] := (edStations in FE);
  chkGrpElementsDessin.Checked[2] := (edIDStations in FE);
  chkGrpElementsDessin.Checked[3] := (edCotation in FE);
  chkGrpElementsDessin.Checked[4] := (edWalls in FE);
  chkGrpElementsDessin.Checked[5] := (edCrossSections in FE);
  chkGrpElementsDessin.Checked[6] := (edFillGalerie in FE);
  chkGrpElementsDessin.Checked[7] := (edQuadrilles in FE);
  chkGrpElementsDessin.Checked[8] := (edENTRANCES in FE);
  chkGrpElementsDessin.Checked[9] := (edANNOTATIONS in FE);
end;
function TdlgPrintingCenter.GetElementDrawn: TElementsDrawn;
begin
  Result := [];
  if (chkGrpElementsDessin.Checked[0]) then Result := Result + [edPolygonals]
                                       else Result := Result - [edPolygonals];
  if (chkGrpElementsDessin.Checked[1]) then Result := Result + [edStations]
                                       else Result := Result - [edStations];
  if (chkGrpElementsDessin.Checked[2]) then Result := Result + [edIDStations]
                                       else Result := Result - [edIDStations];
  if (chkGrpElementsDessin.Checked[3]) then Result := Result + [edCotation]
                                       else Result := Result - [edCotation];
  if (chkGrpElementsDessin.Checked[4]) then Result := Result + [edWalls]
                                       else Result := Result - [edWalls];
  if (chkGrpElementsDessin.Checked[5]) then Result := Result + [edCrossSections]
                                       else Result := Result - [edCrossSections];
  if (chkGrpElementsDessin.Checked[6]) then Result := Result + [edFillGalerie]
                                       else Result := Result - [edFillGalerie];
  if (chkGrpElementsDessin.Checked[7]) then Result := Result + [edQuadrilles]
                                       else Result := Result - [edQuadrilles];
  if (chkGrpElementsDessin.Checked[8]) then Result := Result + [edENTRANCES]
                                       else Result := Result - [edENTRANCES];
  if (chkGrpElementsDessin.Checked[9]) then Result := Result + [edANNOTATIONS]
                                       else Result := Result - [edANNOTATIONS];
end;

//----------------------------------
procedure TdlgPrintingCenter.GetDataFromDialog;
begin
  // récupérer les valeurs de quadrillage depuis les box
  FQdrEspc  := StrToFloatDef(cmbUsualQdrSpacing.Text, 100.00);//editQuadrillage.Value;
  FQdrColor := lbColorQuadrillage.Color;
  FQdrType  := qtGRID; // cmbTypeQuadrillage.ItemIndex;
  FQdrCrossSize := FQdrEspc / 5.00 ; // provisoire
  FEchelle   := 1 / StrToIntDef(cmbEchelle.Text, 1000);
  FRegleSize := editTailleRegle.Value;
end;

// conversion de coordonnées
function TdlgPrintingCenter.GetCoordsPlan (const PM: TPoint2Df): TPoint;
begin
  // rotation et mise à l'échelle
  (* Formule: (F = FRappScrReal)

  | Xp |   | +F    0    -F * FRXMini |   | PM.X |
  |    |   |                         |   |      |
  | Yp | = |  0   -F    +F * FRYMaxi | * | PM.Y |
  |    |   |                         |   |      |
  |  1 |   |  0    0          1      |   |   1  |

  //*)
  //T.X := FZoom * (PM.X * FCosX[FTheta] - PM.Y * FSinX[FTheta]); //+TX
  //T.Y := FZoom * (PM.X * FSinX[FTheta] + PM.Y * FCosX[FTheta]); //
  //T.Y := (PM.Y - FRYMini) * FRappScrReal;
  Result.X:=Round((PM.X - FRXMini) * FRappScrReal);
  Result.Y:=Round((FRYMaxi-PM.Y) * FRappScrReal);
end;
function TdlgPrintingCenter.GetCoordsMonde(const PP: TPoint): TPoint2Df;
begin
  (* Formule: (F = FRappScrReal)

  | Xm |   | +1/F    0     FRXMini   |   | PP.X |
  |    |   |                         |   |      |
  | Ym | = |  0   -1/F     FRYMaxi   | * | PP.Y |
  |    |   |                         |   |      |
  |  1 |   |  0    0          1      |   |   1  |

  //*)

  Result.X:=  FInvRappScrReal * PP.X + FRXMini;
  Result.Y:= -FInvRappScrReal * PP.Y + FRYMaxi;
end;
// cadrage du dessin
// pas besoin de fonction de zoom
// (sinon, on aurait utilisé un TCdrVisualisateur2D)
procedure TdlgPrintingCenter.Recadrer;
var
  Marge: double;
  qdx, qdy: Double;
  C1, C2: TPoint3Df;

begin
  AfficherMessage(Format('%s.Recadrer',[ClassName]));

  with FTableEntites do begin
    C1 := GetCoinBasGauche;
    C2 := GetCoinHautDroit;

    Marge:=0.02 * Hypot2D(C2.X - C1.X,
                          C2.Y - C1.Y);

    FRXMini:=C1.X - Marge;
    FRXMaxi:=C2.X + Marge;
    FRYMini:=C1.Y - Marge;
    FRYMaxi:=C2.Y + Marge;
  end;
  qdx := FRXMaxi - FRXMini;
  qdy := FRYMaxi - FRYMini;
  //pnlCadre.top:=0;
  //pnlCadre.left:=0;

  if (qdx>qdy) then
    FRappScrReal:= pnlCadre.Width / qdx
  else
    FRappScrReal:= pnlCadre.Height / qdy;
  FInvRappScrReal:=1/FRappScrReal;
  AfficherMessage(Format('%s.Recadrer OK',[ClassName]));

end;
// présélectionne une imprimante PDF. 0 = PDF Printer non trouvé ou en tête de liste
function TdlgPrintingCenter.PreSelectPDFPrinter: integer;
var
  i, p: integer;
  S: string;
begin
  Result := 0;
  for i := 0 to cmbPrinters.Items.Count -1 do
  begin
    S := cmbPrinters.Items[i];
    if (Pos('PDF', S) > 0) then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

function TdlgPrintingCenter.Initialise(const F: string): Boolean;
var
  i: Integer;
  WU : TElementsDrawn;
begin
  AfficherMessage(Format('%s.Initialise(%s)',[ClassName, F]));
  // Initialisation de l'imprimante: Nécessite le paquet Printers4Lazarus,
  // qui est à ajouter à la liste des paquets requis par GHTopo
  // Ouvrir l'inspecteur de projet
  // Cliquer sur [ + ] en tete de la fenetre
  // Une fenetre s'ouvre
  // Onglet __/ Nouvelle Condition \__
  // Dérouler la première combobox
  // Sélectionner 'Printer4Lazarus'
  // Les unités Printers et OSPrinters sont INDISPENSABLES !!
  Result := False;
  // Printer est initialisé par le paquet. Ne pas utiliser  TPrinter.Create;
  FPrinter := Printer;


  with FPrinter do begin
    AfficherMessage('Imprimante');
    AfficherMessage('------------');
  end;
  // si aucune imprimante installée: -->[]
  if (FPrinter.Printers.Count = 0) then Exit;
  AfficherMessage('001');
  // liste des imprimantes
  with cmbPrinters do begin
    Clear;
    for i:=0 to FPrinter.Printers.Count-1 do begin
      Items.Add(FPrinter.Printers[i]);
    end;
    cmbPrinters.ItemIndex := PreSelectPDFPrinter();
  end;




  FModeRepresentationGaleries := rgSEANCES;
  WU := [edQuadrilles, edPolygonals, edWalls, edFillGalerie];
  SetElementDrawn(WU);
  AfficherMessage('002');

  FTableEntites := TTableDesEntites.Create;
  try
    if (FTableEntites.LoadEntites(F, True) = 0) then
    begin
      ShowMessage(AnsiToUtf8(rsMSG_PRINT_CENTER_FAIL));
      Exit;
    end;
    // fixation de variables locales
    // quadrillage

    FQdrEspc       := 100.00;
    FQdrEspc       := ProposerEquidistanceDef(FTableEntites.GetCoinBasGauche,
                                              FTableEntites.GetCoinHautDroit, 100.00);
    FRegleSize     := 2 * FQdrEspc;
    FQdrCrossSize  := FQdrEspc / 5;
    FQdrType       := qtGRID;
    FQdrColor      := clSilver;
    FEchelle       := 1 / 1000.00;
    cmbTypeQuadrillage.ItemIndex := Ord(FQdrType);
    cmbUsualQdrSpacing.Text := Format('%.0f', [FQdrEspc]);
    editTailleRegle.Value   := FRegleSize;
    //editQuadrillage.Value := FQdrEspc;
    lbColorQuadrillage.Color := FQdrColor;

    // définition de l'imprimante
    SetMyCurrentPrinter;
    // redessin
    Recadrer;
    DrawApercu;

    Result := True;
  except
  end;
  AfficherMessage('100');

end;

procedure TdlgPrintingCenter.PaintBoxVuePaint(Sender: TObject);
begin
  DrawApercu;
end;

procedure TdlgPrintingCenter.FormShow(Sender: TObject);
begin
  CdrMetaFiltre1.SetFiltre('');
  CdrMetaFiltre1.SetProcApplyMetaFiltre(RegenererVue);
end;

procedure TdlgPrintingCenter.lbColorQuadrillageClick(Sender: TObject);
begin
  lbColorQuadrillage.Color:=ChooseColor(lbColorQuadrillage.Color);
end;

procedure TdlgPrintingCenter.PaintBoxVueDblClick(Sender: TObject);
var
  PM: TPoint2Df;
  PP: TPoint;
  IdxX, IdxY: integer;
begin
  PM:=GetCoordsMonde(FPP);
  CalcIndexPageIncludingPt(PM, IdxX, IdxY);
  if (IdxX>-1) and (IdxY>-1) then begin
    affichermessage(format('%d %d',[IdxX, IdxY]));
    FTableauPagesDrawn[IdxX, IdxY]:=Not(FTableauPagesDrawn[IdxX, IdxY]);
    DrawApercu;
  end;
end;

procedure TdlgPrintingCenter.PaintBoxVueMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  PM: TPoint2Df;
begin
  try
    FPP.X:=X;
    FPP.Y:=Y;
    PM:=GetCoordsMonde(FPP);
    lbMouseCoordinates.Caption:=Format('%.2f, %.2f',[PM.X, PM.Y]);
  except
  end;
end;

procedure TdlgPrintingCenter.FormCreate(Sender: TObject);
begin
  // éléments du dessin
  cmbTypeQuadrillage.ItemWidth := cmbTypeQuadrillage.Width;
  chkGrpElementsDessin.Caption := AnsiToUtf8(rsLAYERS);
  chkRegle.Caption             := AnsiToUtf8(rsREGLE);

  grbPrinterNames.Caption      := AnsiToUtf8(rsPRN_TBPRINTER);
  lbQdrSpacing.Caption         := AnsiToUtf8(rsQDRSPACING);
  lbEchelle.Caption            := AnsiToUtf8(rsECHELLE);
  btnStartImpression.Caption   := AnsiToUtf8(rsSTARTPRINTING);
  btnPreview.Caption           := AnsiToUtf8(rsPREVIEW);
  // règle
  FRegleDisplayed              := true;
  chkRegle.Checked             := FRegleDisplayed;
  FRegleSize                   := 100.00;
  editTailleRegle.Value        := FRegleSize;
  //-----------------------
  // captions des checkbox
  with chkGrpElementsDessin do
  begin
    Items.Clear;
    Items.Add(AnsiToUtf8(rsPRN_CHKPOLY));
    Items.Add(AnsiToUtf8(rsPRN_CHKSTATIONS));
    Items.Add(AnsiToUtf8(rsPRN_CHKSTATIONS_LBL));
    Items.Add(AnsiToUtf8(rsPRN_CHKCOTATION));
    Items.Add(AnsiToUtf8(rsPRN_CHKWALLS));
    Items.Add(AnsiToUtf8(rsPRN_CHKSECTS));
    Items.Add(AnsiToUtf8(rsPRN_CHKFILL));
    Items.Add(AnsiToUtf8(rsPRN_CHKQUADRILLAGE));
    Items.Add(AnsiToUtf8(rsPRN_CHKENTREES));
    Items.Add(AnsiToUtf8(rsPRN_CHKANNOTATIONS));
  end;
  // combo des quadrillages
  with cmbTypeQuadrillage do begin
    Clear;
    Style:=csDropDownList;
    Items.Add(AnsiToUtf8(rsPRN_QDCROSS));
    Items.Add(AnsiToUtf8(rsPRN_QDQUADRILLES));
  end;
 // espacement des croix/grille
 with cmbUsualQdrSpacing do
 begin
     Clear;
     Items.Add(Format('%.0f',[FQdrEspc])); // item0 = Espacement par défaut
     Items.Add('10');
     Items.Add('20');
     Items.Add('25');
     Items.Add('50');
     Items.Add('100');
     Items.Add('200');
     Items.Add('250');
     Items.Add('500');
     Items.Add('1000');
     ItemIndex := 0;
  end;
end;

procedure TdlgPrintingCenter.btnPreviewClick(Sender: TObject);
begin
  GetDataFromDialog;
  //CalcNbPagesAndEmpty;
  Recadrer;
  DrawApercu;
end;

procedure TdlgPrintingCenter.btnFiltresClick(Sender: TObject);
begin

end;
procedure TdlgPrintingCenter.RegenererVue;
begin
  FTableEntites.MetaFiltre(CdrMetaFiltre1.GetFiltre);
  Recadrer;
  DrawApercu;
end;

procedure TdlgPrintingCenter.btnStartImpressionClick(Sender: TObject);
begin
  if QuestionOuiNon('Lancer l''impression ?') then begin
    PrintTopo;
  end;
end;

procedure TdlgPrintingCenter.chkGrpElementsDessinClick(Sender: TObject);
begin

end;

procedure TdlgPrintingCenter.chkGrpElementsDessinDblClick(Sender: TObject);
begin

end;

procedure TdlgPrintingCenter.chkGrpElementsDessinItemClick(Sender: TObject;
  Index: integer);
begin
  GetDataFromDialog;
  Recadrer;
  DrawApercu;
end;

procedure TdlgPrintingCenter.chkRegleChange(Sender: TObject);
begin
  FRegleDisplayed          := chkRegle.Checked;
  editTailleRegle.Enabled  := FRegleDisplayed;
end;

procedure TdlgPrintingCenter.cmbEchelleChange(Sender: TObject);
begin
  FEchelle := 1 / StrToIntDef(cmbEchelle.Text, 1000);
  CalcNbPagesAndEmpty;
  Recadrer;
  DrawApercu;
end;

procedure TdlgPrintingCenter.cmbPrintersChange(Sender: TObject);
begin
  ChoosePrinter(cmbPrinters.ItemIndex);
  SetMyCurrentPrinter;
end;

procedure TdlgPrintingCenter.cmbTypeQuadrillageChange(Sender: TObject);
begin
  case cmbTypeQuadrillage.ItemIndex of
    0: FQdrType:=qtCROSS;
    1: FQdrType:=qtGRID;
    2: FQdrType:=qtPOINTS;
  else
       FQdrType:=qtGRID;
  end;

  Recadrer;
  DrawApercu;
end;

procedure TdlgPrintingCenter.cmbUsualQdrSpacingChange(Sender: TObject);
begin
  FQdrEspc   := StrToFloat(cmbUsualQdrSpacing.Text);

  //CalcNbPagesAndEmpty;
  Recadrer;
  DrawApercu;
end;

// retourne les index de pages en fonction des coordonnées réelles
procedure TdlgPrintingCenter.CalcIndexPageIncludingPt(const Pt: TPoint2Df;
                                                      var IdxX, IdxY: integer);
var
  i,j   : integer;
  L1, H1: Double;
  R666  : TRect2Df;
begin
  IdxX:=-1;
  IdxY:=-1;
  AfficherMessage(Format('%s.CalcIndexPageIncludingPt(%.2f, %.2f)',
                         [ClassName, Pt.X, Pt.Y]));

  L1 := FPageProperties.PageWidthInMM  / (1000*FEchelle);
  H1 := FPageProperties.PageHeightInMM / (1000*FEchelle) ;
  for i:=0 to FNbPagesX-1 do
    for j:=0 to FNbPagesY-1 do begin
      R666.X1 := FRXMini + i * L1;
      R666.Y1 := FRYMini + j * H1;

      R666.X2 := FRXMini + (1+i) * L1;
      R666.Y2 := FRYMini + (1+j) * H1;
      if IsInRange(Pt.X, R666.X1, R666.X2) and
         IsInRange(Pt.Y, R666.Y1, R666.Y2) then
      begin
        IdxX:=i; IdxY:=j;
        AfficherMessage(Format('Page %d, %d sélectionnée',[i,j]));
        Exit;
      end;
    end;  // for i,j
end;

// définir imprimante courante: paramètres, etc ...
procedure TdlgPrintingCenter.SetMyCurrentPrinter;
begin
  FPageProperties:=GetPageProperties(FPrinter);
  with FPageProperties do begin
    lbPrinterName.Caption:= FPageProperties.PrinterName;
    lbOrientation.Caption:=IIF(Portrait, 'PORTRAIT', 'LANDSCAPE');
    lbPageFormat.Caption:=Format('%f mm x %f mm', [PageWidthInMM,
                                                   PageHeightInMM]);
  end;

  // reconstruire le plan
  Recadrer;
  // calculer le nombre de pages
  CalcNbPagesAndEmpty;
  // redessiner
  DrawApercu;;
end;
// choix de l'imprimante
procedure TdlgPrintingCenter.ChoosePrinter(const Idx: integer);
begin
  with FPrinter do begin
    PrinterIndex:=Idx;
  end;
end;
// calculer le nombre de pages et celles qui sont vides
procedure TdlgPrintingCenter.CalcNbPagesAndEmpty;
var
  i,j, v: integer;
  E1    : TEntite;
  L1, H1: Double;
  R666  : TRect2Df;
  RVS   : TRect2Df;
  Grayed: boolean;
begin
  AfficherMessage('Calcul du nombre de pages');
  // calculer le nombre de pages
  FNbPagesX:=1+Trunc(1000*(FRXMaxi - FRXMini) * FEchelle / FPageProperties.PageWidthInMM);
  FNbPagesY:=1+Trunc(1000*(FRYMaxi - FRYMini) * FEchelle / FPageProperties.PageHeightInMM);
  lbNbPages.Caption :=Format('%d x %d pages',[FNbPagesX, FNbPagesY]);
  SetLength(FTableauPagesDrawn, 0, 0);
  SetLength(FTableauPagesDrawn, FNbPagesX, FNbPagesY);

  L1 := FPageProperties.PageWidthInMM  / (1000*FEchelle);
  H1 := FPageProperties.PageHeightInMM / (1000*FEchelle) ;
  for i:=0 to FNbPagesX-1 do
    for j:=0 to FNbPagesY-1 do begin
      R666.X1 := FRXMini + i * L1;
      R666.Y1 := FRYMini + j * H1;

      R666.X2 := FRXMini + (1+i) * L1;
      R666.Y2 := FRYMini + (1+j) * H1;
      // supprimer les pages vides
      Grayed:=false;
      for v:=1 to FTableEntites.GetNbEntites - 1 do begin
        E1:=FTableEntites.GetEntite(v);
        if E1.Type_Entite=tgEntrance then Continue;
        RVS.X1 := Min(E1.Une_Station_1_X, E1.Une_Station_2_X);
        RVS.Y1 := Min(E1.Une_Station_1_Y, E1.Une_Station_2_Y);

        RVS.X2 := Max(E1.Une_Station_1_X, E1.Une_Station_2_X);
        RVS.Y2 := Max(E1.Une_Station_1_Y, E1.Une_Station_2_Y);
        Grayed := Grayed or IntersectRectangles(R666, RVS);
      end;
      FTableauPagesDrawn[i,j]:=Grayed;
      AfficherMessage(Format('--> Page %d.%d: %s',[i, j, IIF((FTableauPagesDrawn[i,j]), 'Affichée', 'Masquée')]));
    end;
    FDoDrawPages:=True;
end;


//**********************************
//afficher tout le plan
procedure TdlgPrintingCenter.DrawApercu;
var
  TmpBuffer: TBitMap;
  R: TRect;
  FElementsDrawn : TElementsDrawn;
  procedure DrawStations;
  var
    i: integer;
    E: TEntite;
    P1, P2: TPoint;
    PM    : TPoint2Df;

  begin
   AfficherMessage(' --> DrawStations');
   with TmpBuffer.Canvas do begin
     Pen.Color:=clRed;
     Brush.Color:=Pen.Color;
   end;
    for i:=0 to FTableEntites.GetNbEntites - 1 do
    begin
     E := FTableEntites.GetEntite(i);
     if (E.Type_Entite=tgENTRANCE) then
     begin // dessin des entrées
       ;
     end else
     begin
       PM.X:=E.Une_Station_2_X;
       PM.Y:=E.Une_Station_2_Y;
       P1:=GetCoordsPlan(PM);
       with TmpBuffer.Canvas do begin
         Ellipse(P1.X-2, P1.Y-2, P1.X+2, P1.Y+2);
       end;
     end; // if E.Type_Entite=tgENTRANCE
    end;
  end;

  procedure DrawBoundingBox;
  var
    P1, P2: TPoint;
    PM    : TPoint2Df;
    C1, C2: TPoint3Df;
  begin
    with FTableEntites do
    begin
      MetaFiltre(CdrMetaFiltre1.GetFiltre);
      C1 := GetCoinBasGauche;
      C2 := GetCoinHautDroit;
      PM.X:=C1.X;
      PM.Y:=C1.Y;
      P1:=GetCoordsPlan(PM);
      PM.X:=C2.X;
      PM.Y:=C2.Y;
      P2:=GetCoordsPlan(PM);
      with TmpBuffer.Canvas do
      begin
        Pen.Width:=0;
        Pen.Color:=clSilver;
        Brush.Style:=bsClear;
        Rectangle(P1.X, P1.Y, P2.X, P2.Y);
      end;
    end;
  end;
  // affichage des pages (avec mention de pages vides)
  procedure DrawPages;
  var
    i,j,v : integer;
    P1, P2: TPoint;
    PM    : TPoint2Df;
    L1, H1: Double;
    R666,
    RVS   : TRect2Df;
    E1    : TEntite;
    //Grayed: boolean;
  begin
    if not FDoDrawPages then Exit;
    with TmpBuffer.Canvas do
    begin
      // todo
      Pen.Width:=0;
      Pen.Color:=clBlue;
      Brush.Style:=bsClear;
      Font.Height := 4;
      Font.Color:=clRed;
      L1 := FPageProperties.PageWidthInMM  / (1000*FEchelle);
      H1 := FPageProperties.PageHeightInMM / (1000*FEchelle) ;
      for i:=0 to FNbPagesX-1 do
        for j:=0 to FNbPagesY-1 do
        begin
          PM.X := FRXMini + i * L1;
          PM.Y := FRYMini + j * H1;
          R666.X1 := PM.X;
          R666.Y1 := PM.Y;

          P1   := GetCoordsPlan(PM);
          PM.X := FRXMini + (1+i) * L1;
          PM.Y := FRYMini + (1+j) * H1;
          R666.X2 := PM.X;
          R666.Y2 := PM.Y;

          P2   := GetCoordsPlan(PM);
          Brush.Style := bsSolid;      // toujours spécifier styles de brosses
          if (FTableauPagesDrawn[i,j]) then
            Brush.Color:=clWhite
          else
            Brush.Color:=clGray;

          Rectangle(P1.X, P1.Y, P2.X, P2.Y);
          //TextOut(p1.X + 2, p1.Y-(Font.Height shl 2), format('%d.%d',[i,j]));
          TextOut(p1.X + 2, p1.Y-(Font.Height shl 2),
                  format('%d',[1 + FNbPagesY*i+j]));
        end;
    end;
  end;
  procedure DrawEntrances;
  const R666 = 3;
  var
   i: integer;
    E: TEntite;
    P2: TPoint;
    PM    : TPoint2Df;
  begin
    AfficherMessage(' --> DrawEntrances');
    with TmpBuffer.Canvas do begin
      Pen.Color:=clFuchsia;
      Brush.Color:=Pen.Color;
    end;
    for i:=0 to FTableEntites.GetNbEntites - 1 do
    begin
      E := FTableEntites.GetEntite(i);
      if (E.Type_Entite=tgENTRANCE) then
      begin // dessin des entrées
        AfficherMessage('Entrée');
        with TmpBuffer.Canvas do
        begin
          PM.X:=E.Une_Station_2_X;
          PM.Y:=E.Une_Station_2_Y;
          P2:=GetCoordsPlan(PM);
          Ellipse(P2.X-R666, P2.Y-R666, P2.X+R666, P2.Y+R666);
        end;
      end;  //
    end; // for

  end;
  procedure DrawQuadrilles;
  var
    P1, P2: TPoint;
    C1, C2: TPoint2Df;
    Coin1, Coin2: TPoint3Df;
    t: integer;
    A,B, B0: double;
  begin
    with TmpBuffer.Canvas do
    begin
      Pen.Color:=FQdrColor;
      Pen.Width:=0;
    end;
    with FTableEntites do
    begin
      Coin1 := GetCoinBasGauche;
      Coin2 := GetCoinHautDroit;
      AfficherMessage(Format('Quadrillage: Spc: %.0f - X = (%.0f, %.0f) - Y = (%.0f, %.0f',
                             [FQdrEspc, Coin1.X, Coin2.X, Coin1.Y, Coin2.Y]));
      case FQdrType of
        qtGRID:
        begin
          t:=trunc(Coin1.X / FQdrEspc);
          A:=FQdrEspc * t;
          while (A<Coin2.X) do
          begin
            // AfficherMessage(Format('%.2f  %.2f',[A, QdrEspc]));
            C1.X:=A; C1.Y:=Coin1.Y;
            C2.X:=A; C2.Y:=Coin2.Y;
            P1:=GetCoordsPlan(C1);
            P2:=GetCoordsPlan(C2);

            TmpBuffer.Canvas.MoveTo(P1.X, P1.Y);
            TmpBuffer.Canvas.LineTo(P2.X, P2.Y);
            A:=A+FQdrEspc;
          end;
          t:=trunc(Coin1.Y / FQdrEspc);
          A:=FQdrEspc * t;
          while (A<Coin2.Y) do
          begin
            C1.X:=Coin1.X; C1.Y:=A;
            C2.X:=Coin2.X; C2.Y:=A;
            P1:=GetCoordsPlan(C1);
            P2:=GetCoordsPlan(C2);
            TmpBuffer.Canvas.MoveTo(P1.X, P1.Y);
            TmpBuffer.Canvas.LineTo(P2.X, P2.Y);
            A:=A+FQdrEspc;
          end;
        end;
        qtCROSS:
        begin
          t:=trunc(Coin1.X / FQdrEspc);
          A:=FQdrEspc * t;
          t:=trunc(Coin1.Y / FQdrEspc);
          B:=FQdrEspc * t;
          B0:=B;
          while (A<Coin2.X) do
          begin
            B:=B0;
            while (B<Coin2.Y) do
            begin
              C1.X:=A - FQdrCrossSize; C1.Y:=B;
              C2.X:=A + FQdrCrossSize; C2.Y:=B ;
              P1:=GetCoordsPlan(C1);
              P2:=GetCoordsPlan(C2);
              TmpBuffer.Canvas.MoveTo(P1.X, P1.Y);
              TmpBuffer.Canvas.LineTo(P2.X, P2.Y);
              C1.X:=A; C1.Y:=B - FQdrCrossSize;
              C2.X:=A; C2.Y:=B + FQdrCrossSize;
              P1:=GetCoordsPlan(C1);
              P2:=GetCoordsPlan(C2);
              TmpBuffer.Canvas.MoveTo(P1.X, P1.Y);
              TmpBuffer.Canvas.LineTo(P2.X, P2.Y);
              B:=B+FQdrEspc;
            end;
            A:=A+FQdrEspc;
          end;
        end;
      end; // case QdrType
    end;// with Ftable...
  end;


  //procedure DrawGaleries(const Filled: boolean);
  procedure DrawGaleries;
  var
    i: integer;
    E: TEntite;
    P: array[0..3] of TPoint;
    PM    : TPoint2Df;
    q1, q2: boolean;
  begin
    q1:=(edFillGalerie in FElementsDrawn);
    q2:=(edWalls in FElementsDrawn);

    AfficherMessage(Format(' --> DrawGaleries avec: %s%s',
                           [IIF(q1,'FILLS, ',''),
                            IIF(q2,'WALLS','')]));
    // propriétés de pinceau et de brosses par défaut
    with TmpBuffer.Canvas do
    begin
      Brush.Style := bsSolid;
      Pen.Style   := psSolid;
      Brush.Color := clSilver;
      Pen.Color   := clBlack;
    end;

    for i:=0 to FTableEntites.GetNbEntites - 1 do
    begin
      E := FTableEntites.GetEntite(i);
      if (E.Type_Entite=tgENTRANCE) then
      begin // dessin des entrées
      end
      else
      begin
        if Not(E.Drawn) then Continue;  // prise en compte du MétaFiltre
        PM.X:=E.X1PG;
        PM.Y:=E.Y1PG;
        P[0]:=GetCoordsPlan(PM);
        PM.X:=E.X1PD;
        PM.Y:=E.Y1PD;
        P[1]:=GetCoordsPlan(PM);
        PM.X:=E.X2PD;
        PM.Y:=E.Y2PD;
        P[2]:=GetCoordsPlan(PM);
        PM.X:=E.X2PG;
        PM.Y:=E.Y2PG;
        P[3]:=GetCoordsPlan(PM);
        with TmpBuffer.Canvas do
        begin
          if (q1) then
          begin // remplissages
            //Brush.Color:=E.ColorEntite;
            case FModeRepresentationGaleries of
              rgSEANCES : Brush.Color := E.ColorEntite;
              rgRESEAUX : Brush.Color := E.ColorReseau;
              rgGRAY    : Brush.Color := clSilver;
            else
              Brush.Color:=E.ColorEntite;
            end;
            Pen.Color  :=Brush.Color;
            Polygon(P);                   // DONE: Polygon ne remplit pas avec la couleur - Corrigé par Brush.Style := bsSolid;
          end;
          //Continue;
          if (q2) then
          begin // parois
            Pen.Color:=clBlack;
            MoveTo(P[0].X, P[0].Y);
            LineTo(P[3].X, P[3].Y);
            MoveTo(P[1].X, P[1].Y);
            LineTo(P[2].X, P[2].Y);
          end;

        end;
      end;
    end;
  end;
  procedure DrawPolygonals;
  var
    i: integer;
    E: TEntite;
    P1, P2: TPoint;
    PM    : TPoint2Df;
  begin
    AfficherMessage(' --> DrawPolygonals');
    for i:=0 to FTableEntites.GetNbEntites - 1 do begin
     E := FTableEntites.GetEntite(i);
     if (E.Type_Entite = tgENTRANCE) then
     begin // dessin des entrées
       ;
     end else
     begin

       PM.X:=E.Une_Station_1_X;
       PM.Y:=E.Une_Station_1_Y;
       P1:=GetCoordsPlan(PM);
       PM.X:=E.Une_Station_2_X;
       PM.Y:=E.Une_Station_2_Y;
       P2:=GetCoordsPlan(PM);
       with TmpBuffer.Canvas do
       begin
         case FModeRepresentationGaleries of
           rgSEANCES : Pen.Color := E.ColorEntite;
           rgRESEAUX : Pen.Color := E.ColorReseau;
           rgGRAY    : Pen.Color := clSilver;
         else
           Pen.Color:=E.ColorEntite;
         end;
         MoveTo(P1.X, P1.Y);
         LineTo(P2.X, P2.Y);
       end;
     end; // if E.Type_Entite=tgENTRANCE
    end;
  end;
  procedure DrawSections;
  var
    i: integer;
    E: TEntite;
    P1, P2: TPoint;
    PM    : TPoint2Df;
  begin
    AfficherMessage(' --> DrawSections');
    with TmpBuffer.Canvas do begin
      Pen.Color:=clGray;
    end;
    for i:=0 to FTableEntites.GetNbEntites - 1 do begin
     E := FTableEntites.GetEntite(i);
     if (E.Type_Entite = tgENTRANCE) then
     begin // dessin des entrées
       ;
     end
     else
     begin
       PM.X:=E.X2PD;
       PM.Y:=E.Y2PD;
       P1:=GetCoordsPlan(PM);
       PM.X:=E.X2PG;
       PM.Y:=E.Y2PG;
       P2:=GetCoordsPlan(PM);
       with TmpBuffer.Canvas do
       begin
         MoveTo(P1.X, P1.Y);
         LineTo(P2.X, P2.Y);
       end;
     end;
    end;
  end;
  procedure DrawIDStations;
  var
    i: integer;
    E: TEntite;
    P1, P2: TPoint;
    PM    : TPoint2Df;
  begin
    AfficherMessage(' --> DrawIDStations');
    with TmpBuffer.Canvas do begin
      Brush.Color := clWhite;
      Font.Color:=clBlue;
      Font.Name:='Arial';
      Font.Size:=5;
    end;
    for i:=0 to FTableEntites.GetNbEntites -1 do begin
     E := FTableEntites.GetEntite(i);
     if (E.Type_Entite = tgENTRANCE) then
     begin // dessin des entrées
       ;
     end else
     begin
       PM.X:=E.Une_Station_2_X;
       PM.Y:=E.Une_Station_2_Y;
       P1:=GetCoordsPlan(PM);
       with TmpBuffer.Canvas do
       begin
         TextOut(P1.X+2, P1.Y+2, Trim(E.ID_Litteral_Pt));
       end;
     end; // if E.Type_Entite=tgENTRANCE
    end;
  end;
  procedure DrawRegle(const TailleRegle: double);
  const
    NB_CARREAUX_X = 8;
  var
    i : Integer;
    Mg: double;
    QXo, QYo, QY1: double;
    LargeurCarreau: Extended;
    HauteurCarreau: Extended;
    procedure QDrawRect(const X, Y, L, H: double; const LC, FC: TColor);
    var
      QP1, QP2: TPoint;
      QPM: TPoint2Df;
    begin
      QPM.X := X;
      QPM.Y := Y;
      QP1 := GetCoordsPlan(QPM);
      QPM.X := X + L;
      QPM.Y := Y + H;
      QP2 := GetCoordsPlan(QPM);
      with TmpBuffer.Canvas do
      begin
        Pen.Width    := 0;
        Pen.Color    := LC;
        Brush.Color  := FC;
        Rectangle(QP1.x, QP1.y, QP2.x, QP2.y);
      end;
    end;
    procedure QDrawTextLegende(const X, Y, L, H: double);
    var
      LargeurTexte, HauteurTexte: integer;
      QP1, QP2: TPoint;
      QPM     : TPoint2Df;
      WU : string;
      EWE: TSize;
    begin
      with TmpBuffer.Canvas do
      begin
        Brush.Color:= clWhite;
        Font.Style := [fsBold];
        Font.Color := clBlue;
        WU := Format('%.0f m',[L]);
        // zéro de gauche
        QPM.X := X;
        QPM.Y := Y;
        QP1   := GetCoordsPlan(QPM);
        QPM.Y := Y + H;
        QP2   := GetCoordsPlan(QPM);
        HauteurTexte := QP2.Y - QP1.Y;
        Font.Height  := HauteurTexte;
        EWE := TextExtent('0');
        //AfficherMessage(Format('----> %d, %d',[EWE.cx, EWE.cy]));
        TextOut(QP1.X, QP1.Y - EWE.cy, '0');
        QPM.X := X + L;
        QPM.Y := Y;
        QP1   := GetCoordsPlan(QPM);
        EWE := TextExtent(WU);
        QP1.X := QP1.X - EWE.cx;
        TextOut(QP1.X, QP1.Y - EWE.Cy, WU);
      end;
    end;
  begin
    Mg := (FRXMaxi - FRXMini) / 50; // marge
    QXo := FRXMini + Mg;
    QYo := FRYMini + Mg;
    LargeurCarreau := TailleRegle / NB_CARREAUX_X;
    HauteurCarreau := TailleRegle / 25;
    AfficherMessage(Format(' --> DrawRegle: L = %.0f m at (%.0f, %.0f)', [TailleRegle, FRXMini + Mg, FRYMini + Mg]));
    // cadre périmétrique
    QDrawRect(QXo, QYo, TailleRegle, 2 * HauteurCarreau, clBlack, clWhite);
    // carreaux alternés
    for i := 0 to NB_CARREAUX_X - 1 do
    begin
      if (Odd(i)) then QY1 := QYo + HauteurCarreau else QY1 := QYo;
      QDrawRect(QXo + (i * LargeurCarreau), QY1, LargeurCarreau, HauteurCarreau, clBlack, clGray);
    end;
    // légende
    QDrawTextLegende(QXo, QYo + 2 * HauteurCarreau + Mg/4, TailleRegle, HauteurCarreau);
  end;
  (*
  procedure DrawAnnotations;
  var
    A: TAnnotation;
    i: integer;
    PM: TPoint2Df;
    PP: TPoint;
    // positionnement du texte
    PosTXT: TPoint;
    // encombrement du texte
    ExTxt : TSize;

    s: string;
    // pour positionnement relatif à un point topo
    E: TEntite;

  begin
    AfficherMessage(' --> DrawAnnotations');
    if not (FTableEntites.LesAnnotationsSontOK) then Exit;
    try
      with FTableEntites.MyTableAnnotations do begin
        if NbAnnotations=0 then Exit;
        with TmpBuffer.Canvas do begin
          for i:=0 to FTableEntites.MyTableAnnotations.NbAnnotations-1 do begin
            A:=FTableEntites.MyTableAnnotations.GetAnnotation(i);
            if Not(A.Displayed) then Continue;
            case A.ModePosition of
              0: begin // positionnement absolu
                   PM.X:=A.X;
                   PM.Y:=A.Y;
                 end;
              1: begin // positionnement relatif à une station
                   E := FTableEntites.GetEntiteFromSerSt(A.BaseRefSer, A.BaseRefSt);
                   PM.X := E.Une_Station_2_X + A.DX;
                   PM.Y := E.Une_Station_2_Y + A.DY;
                   A.Z  := E.Une_Station_2_Z + A.DZ;

                 end;
            end;
            PP:=GetCoordsPlan(PM);

            Font.Name:=IIF(A.FontName=NAME_FOR_DEFAULT_FONT, 'Arial', A.FontName);
            Font.Size:=A.FontSize;
            Font.Color:=A.FontColor;
            Brush.Color:=clWhite;
            //Brush.Color:=FBackGround;
            if A.FontBold then Font.Style:=Font.Style+[fsBold]
                          else Font.Style:=Font.Style-[fsBold];
            if A.FontItalic then Font.Style:=Font.Style+[fsItalic]
                            else Font.Style:=Font.Style-[fsItalic];
            if A.FontUnderline then Font.Style:=Font.Style+[fsUnderline]
                               else Font.Style:=Font.Style-[fsUnderline];

            S:=LLANFAIR_PG(A.Caption, A.MaxLength, A.Z,
                           A.BaseRefSer, A.BaseRefSt);
            // positionnement du texte

            //PP.X := 0;
            //PP.Y := 0;

            ExTxt :=  TextExtent(S);
            case A.PosPtDeBase of
              0,1: begin  // défaut = en bas à gauche
                     PosTXT.X := PP.X;
                     PosTXT.Y := PP.Y - ExTxt.cy;
                   end;
              2  : begin
                     PosTXT.X := PP.X - ExTxt.cx div 2;
                     PosTXT.Y := PP.Y - ExTxt.cy;
                   end;
              3  : begin
                     PosTXT.X := PP.X - ExTxt.cx;
                     PosTXT.Y := PP.Y - ExTxt.cy;
                   end;
              4  : begin
                     PosTXT.X := PP.X;
                     PosTXT.Y := PP.Y - ExTxt.cy div 2;
                   end;
              5  : begin
                     PosTXT.X := PP.X - ExTxt.cx div 2;
                     PosTXT.Y := PP.Y - ExTxt.cy div 2;
                   end;
              6  : begin
                     PosTXT.X := PP.X - ExTxt.cx;
                     PosTXT.Y := PP.Y - ExTxt.cy div 2;
                   end;
              7  : begin
                     PosTXT.X := PP.X;
                     PosTXT.Y := PP.Y;
                   end;
              8  : begin
                     PosTXT.X := PP.X - ExTxt.cx div 2;
                     PosTXT.Y := PP.Y;
                   end;
              9  : begin
                     PosTXT.X := PP.X - ExTxt.cx;
                     PosTXT.Y := PP.Y;
                   end;
            else
              begin
                PosTXT.X := PP.X;
                PosTXT.Y := PP.Y - ExTxt.cy;
              end;
            end;
            // calcul de BoundingBox
            A.BoundingBox.Left    := PosTXT.X;
            A.BoundingBox.Top     := PosTXT.Y;
            A.BoundingBox.Right   := PosTXT.x + Extxt.cx;
            A.BoundingBox.Bottom  := PosTXT.Y + Extxt.cy;
            // et sauvegarde Bounding Box
            FTableEntites.MyTableAnnotations.PutAnnotation(A, i);

            TextOut(PosTXT.X, PosTXT.Y, S);
            // débogage: Tracé du point de base
            Rectangle(PP.X, PP.Y,
                      PP.X + 2, PP.Y + 2);
          end; // for i
        end; // with TmpBuffer.Canvas do begin
      end; // with FTableEntites.MyTableAnnotations do begin
    except
    end;
  end;
  //*)
begin
  AfficherMessage(Format('%s.DrawApercu',[ClassName]));
  GetDataFromDialog;
  FElementsDrawn := GetElementDrawn;
  try
    TmpBuffer:=TBitmap.Create;
    with TmpBuffer do begin
      Height:= PaintBoxVue.Height;
      Width := PaintBoxVue.Width;
      //affichermessage(format('%d %d',[PaintBoxVue.Height,PaintBoxVue.Width]));
      R.Left:=PaintBoxVue.Left;
      R.Top :=PaintBoxVue.Top;
      R.Bottom:=PaintBoxVue.Top  + PaintBoxVue.Height;
      R.Right :=PaintBoxVue.Left + PaintBoxVue.Width;
      Canvas.Brush.Color:=clWhite;
      Canvas.FillRect(R);
      // dessin des limites
      DrawBoundingBox;
      // dessin des pages
      DrawPages;
      // dessin des éléments

      if edPolygonals    in FElementsDrawn then DrawPolygonals;
      //if edFillGalerie  in FElementsDrawn then
      DrawGaleries;
      if edQuadrilles    in FElementsDrawn then DrawQuadrilles;
      if edEntrances     in FElementsDrawn then DrawEntrances;
      if edAnnotations   in FElementsDrawn then ; //DrawAnnotations;
      if edCrossSections in FElementsDrawn then DrawSections;
      if edStations      in FElementsDrawn then DrawStations;
      if edIDStations    in FElementsDrawn then DrawIDStations;
      //*)
      // dessin de la règle
      if (FRegleDisplayed) then DrawRegle(FRegleSize);
    end;
    PaintBoxVue.Canvas.CopyRect(R, TmpBuffer.Canvas, R);
  finally
    TmpBuffer.Free;
  end;
end;


// imprimer une page
procedure TdlgPrintingCenter.PrintAPage(const PageProp: TPageProperties;
                                        const Echelle: double;
                                        const L,C: integer;
                                        const TextCotationHeight: double;
                                        const TextIDStationHeight: double);
const
  RD=8;
var
  FElementsDrawn : TElementsDrawn;
  function Coord2Draft(const Xf, Yf: double): TPoint;
  var
    QXf, QYf: Double;
  begin
    // transformation en millimètres et mise à l'échelle
    QXf:=((-FRXMini+Xf) * 1000.00) * Echelle;
    QYf:=((-FRYMini+Yf) * 1000.00) * Echelle;

    Result.x := -FPrinter.PageWidth * C +
                 Millimetres2PixelsXf(FPrinter, QXf);
    Result.y := FPrinter.PageHeight * L +
                FPrinter.PageHeight-Millimetres2PixelsYf(FPrinter, QYf);
  end;

  //procedure DrawGaleries(const Filled: boolean);
  procedure DrawGaleries;
  var
    i: integer;
    E: TEntite;
    P: array[0..3] of TPoint;
    q1, q2: boolean;
  begin
    q1:=(edFillGalerie in FElementsDrawn);
    q2:=(edWalls in FElementsDrawn);
    for i:=0 to FTableEntites.GetNbEntites - 1 do begin
     E := FTableEntites.GetEntite(i);
     if E.Type_Entite=tgENTRANCE then begin // dessin des entrées
     end else begin

       if Not(E.Drawn) then Continue;    // prise en compte du MétaFiltre
       P[0]:=Coord2Draft(E.X1PG, E.Y1PG);
       P[1]:=Coord2Draft(E.X1PD,E.Y1PD);
       P[2]:=Coord2Draft(E.X2PD,E.Y2PD);
       P[3]:=Coord2Draft(E.X2PG,E.Y2PG);
       with FPrinter.Canvas do begin
         if q1 then begin // remplissages
           case FModeRepresentationGaleries of
             rgSEANCES : Brush.Color := E.ColorEntite;
             rgRESEAUX : Brush.Color := E.ColorReseau;
             rgGRAY    : Brush.Color := clSilver;
           else
             Brush.Color:=E.ColorEntite;
           end;


           //Brush.Color:=E.ColorEntite;
           Pen.Color  :=Brush.Color;
           Polygon(P);
         end;
         if q2 then begin // parois
           Pen.Color:=clBlack;
           MoveTo(P[0].X, P[0].Y);
           LineTo(P[3].X, P[3].Y);
           MoveTo(P[1].X, P[1].Y);
           LineTo(P[2].X, P[2].Y);
         end;
       end;
     end;
    end;
  end;
  procedure DrawPolygonals(const BlackLines: boolean);
  var
    i: integer;
    E: TEntite;
    P1, P2: TPoint;
  begin
    with FPrinter.Canvas do begin
      for I:=0 to FTableEntites.GetNbEntites - 1 do begin
        E:=FTableEntites.GetEntite(I);
        if E.Type_Entite=tgENTRANCE then Continue;
        case FModeRepresentationGaleries of
           rgSEANCES : Pen.Color := E.ColorEntite;
           rgRESEAUX : Pen.Color := E.ColorReseau;
           rgGRAY    : Pen.Color := clSilver;
        else
          Pen.Color:=E.ColorEntite;
        end;
        if (BlackLines) then Pen.Color:=clBlack;

        //Pen.Color := E.ColorEntite;
        P1:=Coord2Draft(E.Une_Station_1_X, E.Une_Station_1_Y);
        MoveTo(P1.X, P1.Y);
        P2:=Coord2Draft(E.Une_Station_2_X, E.Une_Station_2_Y);
        LineTo(P2.X, P2.Y);
      end;
    end;
  end;
  procedure DrawSections;
  var
    i: integer;
    E: TEntite;
    P1, P2: TPoint;
  begin
    with FPrinter.Canvas do begin
      for I:=0 to FTableEntites.GetNbEntites - 1 do begin
        E:=FTableEntites.GetEntite(I);
        if E.Type_Entite=tgENTRANCE then Continue;
        Pen.Color := clGray;
        P1:=Coord2Draft(E.X2PD, E.Y2PD);
        MoveTo(P1.X, P1.Y);
        P2:=Coord2Draft(E.X2PG, E.Y2PG);
        LineTo(P2.X, P2.Y);
      end;
    end;
  end;
  procedure DrawQuadrilles;
  var
    P1, P2: TPoint;
    C1, C2: TPoint2Df;
    Coin1, Coin2: TPoint3Df;
    t: integer;
    A,B, B0: double;
    S: string;
  begin
    with FTableEntites do begin
      FPrinter.Canvas.Pen.Color := FQdrColor;
      FPrinter.Canvas.Pen.Width := Millimetres2PixelsXf(FPrinter, 0.1);
      Coin1 := GetCoinBasGauche;
      Coin2 := GetCoinHautDroit;
      case FQdrType of
        qtGRID: begin
          t:=trunc(Coin1.X / FQdrEspc);
          A:=FQdrEspc * t;
          with FPrinter.Canvas do begin
            Brush.Color:=clWhite;
            Font.Color:= clBlack;
            Font.Name := 'Arial';
            Font.Style:=[fsItalic];
            Font.Height := Millimetres2PixelsYf(FPrinter, 3.00);
          end;
          while (A<Coin2.X) do begin

            //AfficherMessage(Format('%.2f  %.2f',[A, QdrEspc]));

            C1.X:=A; C1.Y:=Coin1.Y;
            C2.X:=A; C2.Y:=Coin2.Y;
            P1:=Coord2Draft(C1.X, C1.Y);
            P2:=Coord2Draft(C2.X, C2.Y);
            FPrinter.Canvas.MoveTo(P1.X, P1.Y);
            FPrinter.Canvas.LineTo(P2.X, P2.Y);
            // cotation
            with FPrinter.Canvas do begin
              S:=Format('%.0f',[A]);
              TextOut(P1.X - TextWidth(S) div 2,
                      P1.Y - 2,
                      S);
            end;
            A:=A+FQdrEspc;
           end;
           t:=trunc(Coin1.Y / FQdrEspc);
           A:=FQdrEspc * t;
           while (A<Coin2.Y) do begin

             C1.X:=Coin1.X; C1.Y:=A;
             C2.X:=Coin2.X; C2.Y:=A;
             P1:=Coord2Draft(C1.X, C1.Y);
             P2:=Coord2Draft(C2.X, C2.Y);
             FPrinter.Canvas.MoveTo(P1.X, P1.Y);
             FPrinter.Canvas.LineTo(P2.X, P2.Y);
             with FPrinter.Canvas do begin
              S:=Format('%.0f ',[A]);
              TextOut(P1.X + 2,
                      P1.Y - TextHeight(S) div 2,
                      S);
            end;
             A:=A+FQdrEspc;
           end;
          end;
        qtCROSS: begin
          t:=trunc(Coin1.X / FQdrEspc);
          A:=FQdrEspc * t;
          t:=trunc(Coin1.Y / FQdrEspc);
          B:=FQdrEspc * t;
          B0:=B;
          while (A<Coin2.X) do begin
           B:=B0;
           while (B<Coin2.Y) do begin
             C1.X:=A - FQdrCrossSize; C1.Y:=B;
             C2.X:=A + FQdrCrossSize; C2.Y:=B ;
             P1:=Coord2Draft(C1.X, C1.Y);
             P2:=Coord2Draft(C2.X, C2.Y);
             FPrinter.Canvas.MoveTo(P1.X, P1.Y);
             FPrinter.Canvas.LineTo(P2.X, P2.Y);
             C1.X:=A; C1.Y:=B - FQdrCrossSize;
             C2.X:=A; C2.Y:=B + FQdrCrossSize;
             P1:=Coord2Draft(C1.X, C1.Y);
             P2:=Coord2Draft(C2.X, C2.Y);
             FPrinter.Canvas.MoveTo(P1.X, P1.Y);
             FPrinter.Canvas.LineTo(P2.X, P2.Y);
             B:=B+FQdrEspc;
           end;
           A:=A+FQdrEspc;
          end;
        end;
      end; // case QdrType
    end;// with Ftable...
  end;
  // dessin du cadre de découpage
  procedure DrawCorners;
  var
    CornerSize: integer;
  begin
    with FPrinter do begin
      CornerSize:= PageWidth div 20;
      with Canvas do begin
        Pen.Color:=clBlue;
        Pen.Width:=0;

        MoveTo(0,CornerSize);
        LineTo(0,0);
        LineTo(CornerSize,0);

        MoveTo(0,PageHeight-1-CornerSize);
        LineTo(0,PageHeight-1);
        Lineto(CornerSize,PageHeight-1);

        MoveTo(PageWidth-1-CornerSize,0);
        LineTo(PageWidth-1, 0);
        LineTo(PageWidth-1,CornerSize);

        MoveTo(PageWidth-1-CornerSize, PageHeight-1);
        LineTo(PageWidth-1, PageHeight-1);
        LineTo(PageWidth-1, PageHeight-1-CornerSize);

        // numéros de page
        Font.Size   := 8;
        Brush.Color := clWhite;
        TextOut(10,10, Format('Page %d %d - by GHTopo',[C+1,L+1]));
      end;
    end;
  end;


  procedure DrawEntrances;

  var
    R666 : integer;
    i: integer;
    E: TEntite;
    P2: TPoint;
    PM    : TPoint2Df;
  begin
    R666:=Millimetres2PixelsXf(FPrinter, 0.80);
    for i:=0 to FTableEntites.GetNbEntites - 1 do begin
      E := FTableEntites.GetEntite(i);
      if E.Type_Entite=tgENTRANCE then begin // dessin des entrées
        AfficherMessage('Entrée');
        with FPrinter.Canvas do begin
          PM.X:=E.Une_Station_2_X;
          PM.Y:=E.Une_Station_2_Y;
          P2:=Coord2Draft(PM.X, PM.Y);
          Brush.Color:=clFuchsia;

          Ellipse(P2.X-R666, P2.Y-R666,
                  P2.X+R666, P2.Y+R666);

        end;
      end;  //
    end; // for

  end;
  (*
  procedure DrawAnnotations;
  var
    A: TAnnotation;
    i: integer;
    PP: TPoint;
    PM: TPoint2Df;

    //TxtDecalage: TPoint;
    // positionnement du texte
    PosTXT: TPoint;
    // encombrement du texte
    ExTxt : TSize;

    s: string;
    // pour positionnement relatif à un point topo
    E: TEntite;
  begin
    if not (FTableEntites.LesAnnotationsSontOK) then Exit;
    try
      with FPrinter.Canvas do begin
        if FTableEntites.MyTableAnnotations.NbAnnotations=0 then Exit;
        for i:=0 to FTableEntites.MyTableAnnotations.NbAnnotations-1 do begin
          A:=FTableEntites.MyTableAnnotations.GetAnnotation(i);
          if Not(A.Displayed) then Continue;
          case A.ModePosition of
            0: begin // positionnement absolu
                 PM.X:=A.X;
                 PM.Y:=A.Y;
               end;
            1: begin // positionnement relatif à une station
                 E := FTableEntites.GetEntiteFromSerSt(A.BaseRefSer, A.BaseRefSt);
                 PM.X := E.Une_Station_2_X + A.DX;
                 PM.Y := E.Une_Station_2_Y + A.DY;
                 A.Z  := E.Une_Station_2_Z + A.DZ;

               end;
          end;
          PP:=Coord2Draft(PM.X, PM.Y);

          Font.Name:=IIF(A.FontName=NAME_FOR_DEFAULT_FONT, 'Arial', A.FontName);
          Font.Size:=A.FontSize;
          Font.Color:=A.FontColor;
          Brush.Color:=clWhite;
          //Brush.Color:=FBackGround;
          if A.FontBold then Font.Style:=Font.Style+[fsBold]
                        else Font.Style:=Font.Style-[fsBold];
          if A.FontItalic then Font.Style:=Font.Style+[fsItalic]
                          else Font.Style:=Font.Style-[fsItalic];
          if A.FontUnderline then Font.Style:=Font.Style+[fsUnderline]
                             else Font.Style:=Font.Style-[fsUnderline];

          S:=LLANFAIR_PG(A.Caption, A.MaxLength, A.Z,
                         A.BaseRefSer, A.BaseRefSt);
          // positionnement du texte

          //PP.X := 0;
          //PP.Y := 0;

          ExTxt :=  TextExtent(S);
          case A.PosPtDeBase of
            0,1: begin  // défaut = en bas à gauche
                   PosTXT.X := PP.X;
                   PosTXT.Y := PP.Y - ExTxt.cy;
                 end;
            2  : begin
                   PosTXT.X := PP.X - ExTxt.cx div 2;
                   PosTXT.Y := PP.Y - ExTxt.cy;
                 end;
            3  : begin
                   PosTXT.X := PP.X - ExTxt.cx;
                   PosTXT.Y := PP.Y - ExTxt.cy;
                 end;
            4  : begin
                   PosTXT.X := PP.X;
                   PosTXT.Y := PP.Y - ExTxt.cy div 2;
                 end;
            5  : begin
                   PosTXT.X := PP.X - ExTxt.cx div 2;
                   PosTXT.Y := PP.Y - ExTxt.cy div 2;
                 end;
            6  : begin
                   PosTXT.X := PP.X - ExTxt.cx;
                   PosTXT.Y := PP.Y - ExTxt.cy div 2;
                 end;
            7  : begin
                   PosTXT.X := PP.X;
                   PosTXT.Y := PP.Y;
                 end;
            8  : begin
                   PosTXT.X := PP.X - ExTxt.cx div 2;
                   PosTXT.Y := PP.Y;
                 end;
            9  : begin
                   PosTXT.X := PP.X - ExTxt.cx;
                   PosTXT.Y := PP.Y;
                 end;
          else
            begin
              PosTXT.X := PP.X;
              PosTXT.Y := PP.Y - ExTxt.cy;
            end;
          end;
          // calcul de BoundingBox
          A.BoundingBox.Left    := PosTXT.X;
          A.BoundingBox.Top     := PosTXT.Y;
          A.BoundingBox.Right   := PosTXT.x + Extxt.cx;
          A.BoundingBox.Bottom  := PosTXT.Y + Extxt.cy;
          // et sauvegarde Bounding Box
          FTableEntites.MyTableAnnotations.PutAnnotation(A, i);

          TextOut(PosTXT.X, PosTXT.Y, S);
          // débogage: Tracé du point de base
          Rectangle(PP.X, PP.Y,
                    PP.X + 2, PP.Y + 2);


        end; // for
      end; //with FPrinter.Canvas do begin
    except
    end;
  end;
  //*)
  procedure DrawIDStations;
  var
    i: integer;
    E: TEntite;
    P1, P2: TPoint;
  begin
    with FPrinter.Canvas do begin
      Brush.Color := clWhite;
      Font.Color  := clBlue;
      Font.Name   := 'Arial';
      //Font.Size   := 4;
      Font.Height := Millimetres2PixelsYf(FPrinter, TextIDStationHeight);
      Font.Style  :=[];
      for I:=0 to FTableEntites.GetNbEntites - 1 do begin
        E:=FTableEntites.GetEntite(I);
        if E.Type_Entite=tgENTRANCE then Continue;
        Pen.Color := clGray;
        P1:=Coord2Draft(E.Une_Station_2_X, E.Une_Station_2_Y);
        TextOut(P1.X+2+RD, P1.Y+2+RD, Trim(E.ID_Litteral_Pt));
      end;
    end;
  end;
  procedure DrawStations;
  var
    i: integer;
    E: TEntite;
    P1, P2: TPoint;
  begin
   AfficherMessage(' --> Printer.DrawStations');
    with FPrinter.Canvas do begin
      Pen.Color:=clRed;
      Brush.Color:=Pen.Color;
    end;
    for i:=0 to FTableEntites.GetNbEntites - 1 do begin
     E := FTableEntites.GetEntite(i);
     if Not (E.Type_Entite = tgENTRANCE) then Continue;

     P1:=Coord2Draft(E.Une_Station_2_X,E.Une_Station_2_Y);
     with FPrinter.Canvas do begin
       Ellipse(P1.X-RD, P1.Y-RD, P1.X+RD, P1.Y+RD);
     end;
    end;
  end;

  // dessin du cartouche et du cadre périmétrique
  procedure DrawCartouche;
  const
    MG = 2.0; // marge 20 m
  var
    P1, P2: TPoint; //, P3, P4: TPoint;
  begin
    AfficherMessage(' --> Printer.DrawCartouche');

    P1 := Coord2Draft(FRXMini + MG, FRYMini + MG);
    P2 := Coord2Draft(FRXMaxi - MG, FRYMaxi - MG);
    //P3 := Coord2Draft(FTableEntites.cnMaxi.X, FTableEntites.cnMaxi.Y);
    //P4 := Coord2Draft(FTableEntites.cnMini.X, FTableEntites.cnMaxi.Y);
    with FPrinter.Canvas do begin
      Pen.Color  :=clBlack;
      Pen.Width  := Millimetres2PixelsXf(FPrinter, 0.45);
      //Brush.Color:=Pen.Color;
      MoveTo(P1.X, P1.Y);
      LineTo(P2.X, P1.Y);
      LineTo(P2.X, P2.Y);
      LineTo(P1.X, P2.Y);
      LineTo(P1.X, P1.Y);
      Pen.Width  := 0;
    end;

  end;
  // cotation
  procedure DrawCotationStations;
  var
    i: integer;
    E: TEntite;
    P1, P2: TPoint;
  begin
    AfficherMessage(' --> Printer.DrawCotationStations');
    with FPrinter.Canvas do begin
      Brush.Color := clWhite;
      Font.Color  := clBlue;
      Font.Name   := 'Arial';
      //Font.Size   := 4;
      Font.Height := Millimetres2PixelsYf(FPrinter, TextCotationHeight);
      Font.Style  :=[];
      for I:=0 to FTableEntites.GetNbEntites - 1 do begin
        E:=FTableEntites.GetEntite(I);
        if E.Type_Entite=tgENTRANCE then Continue;
        Pen.Color := clGray;
        P1:=Coord2Draft(E.Une_Station_2_X, E.Une_Station_2_Y);
        TextOut(P1.X+2+RD, P1.Y+2-RD, Format('%f',[E.Une_Station_2_Z]));
      end;
    end;
  end;
  // dessin de la règle
  procedure DrawRegle(const TailleRegle: double);
  const
    NB_CARREAUX_X = 8;
  var
    i : Integer;
    Mg: double;
    QXo, QYo, QY1: double;
    LargeurCarreau: Extended;
    HauteurCarreau: Extended;
    procedure QDrawRect(const X, Y, L, H: double; const LC, FC: TColor);
    var
      QP1, QP2: TPoint;
    begin
      QP1 := Coord2Draft(X, Y);
      QP2 := Coord2Draft(X+L, Y+H);
      with FPrinter.Canvas do begin
        Pen.Width    := 0;
        Pen.Color    := LC;
        Brush.Color  := FC;
        Rectangle(QP1.x, QP1.y, QP2.x, QP2.y);
      end;
    end;
    procedure QDrawTextLegende(const X, Y, L, H: double);
    var
      LargeurTexte, HauteurTexte: integer;
      QP1, QP2: TPoint;
      WU : string;
      EWE: TSize;
    begin
      with FPrinter.Canvas do begin
        Brush.Color:= clWhite;
        Font.Style := [fsBold];
        Font.Color := clBlue;

        WU := Format('%.0f m',[L]);

        // zéro de gauche
        QP1   := Coord2Draft(X, Y);
        QP2   := Coord2Draft(X, Y + H);
        HauteurTexte := QP2.Y - QP1.Y;
        Font.Height  := HauteurTexte;
        EWE := TextExtent('0');
        //AfficherMessage(Format('----> %d, %d',[EWE.cx, EWE.cy]));
        TextOut(QP1.X, QP1.Y - EWE.cy, '0');
        QP1   := Coord2Draft(X + L, Y);
        EWE := TextExtent(WU);
        QP1.X := QP1.X - EWE.cx;
        TextOut(QP1.X, QP1.Y - EWE.Cy, WU);
      end;
    end;
  begin
    Mg := (FRXMaxi - FRXMini) / 50; // marge
    QXo := FRXMini + Mg;
    QYo := FRYMini + Mg;
    LargeurCarreau := TailleRegle / NB_CARREAUX_X;
    HauteurCarreau := TailleRegle / 25;
    AfficherMessage(Format(' --> DrawRegle: L = %.0f m at (%.0f, %.0f)', [TailleRegle, FRXMini + Mg, FRYMini + Mg]));
    // cadre périmétrique
    QDrawRect(QXo, QYo, TailleRegle, 2 * HauteurCarreau, clBlack, clWhite);
    // carreaux alternés
    for i := 0 to NB_CARREAUX_X - 1 do
    begin
      if (Odd(i)) then QY1 := QYo + HauteurCarreau else QY1 := QYo;
      QDrawRect(QXo + (i * LargeurCarreau), QY1, LargeurCarreau, HauteurCarreau, clBlack, clGray);
    end;
    // légende
    QDrawTextLegende(QXo, QYo + 2 * HauteurCarreau + Mg/4, TailleRegle, HauteurCarreau);
  end;

  //*)
begin
  try
    GetDataFromDialog;
    FElementsDrawn := GetElementDrawn;
    AfficherMessage(Format('Impression carré %d.%d (page %d)',[L,C, Printer.PageNumber]));
    FPrinter.NewPage;
    DrawGaleries;
    if edPolygonals    in FElementsDrawn then begin
      if (edFillGalerie in FElementsDrawn) then DrawPolygonals(True)
                                           else DrawPolygonals(False);
    end;

    if edQuadrilles    in FElementsDrawn then DrawQuadrilles;
    if edEntrances     in FElementsDrawn then DrawEntrances;
    if edAnnotations   in FElementsDrawn then ; // DrawAnnotations;
    if edcrossSections in FElementsDrawn then DrawSections;
    if edStations      in FElementsDrawn then DrawStations;
    if edIDStations    in FElementsDrawn then DrawIDStations;
    if edCotation      in FElementsDrawn then DrawCotationStations;
    // coins de découpage
    DrawCorners;
    // Cartouche
    DrawCartouche;
    // Règle
    DrawRegle(FRegleSize);

  except
  end;
end;

// imprimer la topo
procedure TdlgPrintingCenter.PrintTopo;
var
  L,C,N,Q: integer;
begin
  pnlProgressPrinting.Visible:=True;
  lbPrintCurrent.Caption:='Démarrage de l''impression';
  with FPrinter do begin
    try
      Refresh;
      AfficherMessage('>> 001');
      BeginDoc;
      AfficherMessage('>> 002');
      N:=0;
      progbarPrinting.Min := 0;
      Q := FNbPagesX*FNbPagesY;
      progbarPrinting.Max := Q;

      for C:=0 to FNbPagesX-1 do
        for L:=0 to FNbPagesY-1 do begin
          Inc(N);
          progbarPrinting.Position := N;
          if Not FTableauPagesDrawn[C,L] then Continue;
            try

              AfficherMessage(Format('>> %d-%d - 003',[c,l]));
              PrintAPage(FPageProperties,
                         FEchelle,
                         L,C,
                         8, //editCotFontHeight.Value,
                         8); //editIDStsFontHeight.Value);
              Application.ProcessMessages;
              lbPrintCurrent.Caption:=Format('Impression en cours de la page %d/%d',
                                                  [N,Q]);
            except
              AfficherMessage(Format('>> Erreur impression page %d %d',[C,L]));
            end;
        end;
      AfficherMessage('>> 999');
      EndDoc;
      AfficherMessage('>> Impresion terminée');
    except
      ShowMessage('666');
    end;
  end;
  pnlProgressPrinting.Visible:=False;
end;




end.

//******************************************************************************
procedure TdlgPrintingCenter.cmbTypeQuadrillageDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  Q : integer;
  Q1: Integer;
  Q2: Integer;
  procedure DessinLigneHZ(const Y, L: integer);
  begin
    cmbTypeQuadrillage.Canvas.MoveTo(ARect.Left  + 2, Y);
    cmbTypeQuadrillage.Canvas.LineTo(ARect.Left + L - 4, Y);
  end;
  procedure DessinLigneVT(const X: integer);
  begin
    cmbTypeQuadrillage.Canvas.MoveTo(X, ARect.Top + 2);
    cmbTypeQuadrillage.Canvas.LineTo(X, ARect.Bottom - 2);
  end;

begin
  //AfficherMessage('cmbTypeQuadrillageDrawItem');
  cmbTypeQuadrillage.Canvas.Pen.Color := clBlack;
  if (odSelected in State) then
    cmbTypeQuadrillage.Canvas.Brush.Color := clAqua
  else
    cmbTypeQuadrillage.Canvas.Brush.Color := clWhite;
  cmbTypeQuadrillage.Canvas.FillRect(ARect);
  with cmbTypeQuadrillage do begin
   case Index of
    0: begin// croix
         Q := cmbTypeQuadrillage.ItemHeight;
         DessinLigneVT(  Q div 4);
         DessinLigneVT(  Q div 2);
         DessinLigneVT(3*Q div 4);
         //Q := Rect.Bottom - Rect.Top;
         DessinLigneHZ(ARect.Top +  Q div 4, Q);
         DessinLigneHZ(ARect.Top +  Q div 2, Q);
         DessinLigneHZ(ARect.Top +3*Q div 4, Q);
       end;
    1: begin // grille
         Q := cmbTypeQuadrillage.ItemHeight;
         //Q := Rect.Right - Rect.Left;
         DessinLigneVT(  Q div 2);
         //Q := Rect.Bottom - Rect.Top;
         DessinLigneHZ(ARect.Top +  Q div 2, Q);

       end;
    2: begin // points
         Q1 := cmbTypeQuadrillage.ItemHeight div 2;
         Q2 := cmbTypeQuadrillage.Width      div 2;
         cmbTypeQuadrillage.Canvas.Rectangle(Q1 - 4, Q2 - 4, Q1 + 4, Q2 + 4);
       end ;
   else
     ;
   end;
  end;
end;

