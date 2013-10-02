unit CdrDessin2D;
// Cadre de dessin 2D
// Statut: Opérationnel.
// DONE: Histogramme des directions
// DONE: Histogramme des altitudes
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
  Graphics, StructuresDonnees, types,
  UnitEntites, Common, CadreFiltres, Classes, SysUtils, FileUtil, curredit,
  dateutils,
  ClasseMaillage,
  LResources, Forms, Dialogs, ExtCtrls, Controls, StdCtrls, ActnList, Menus;


type

  { TCadreDessin2D }
  TCadreDessin2D = class(TFrame)
    acMetaFiltreSerie: TAction;
    acMetaFiltreTopoDuJour: TAction;
    acMetaFiltreReseau: TAction;
    acMetaFiltreCode: TAction;
    acMetaFiltreExpe: TAction;
    acMetaFiltreTopoAnnee: TAction;
    acMetaFiltreCouleur: TAction;
    acMetaFiltreVueCourante: TAction;
    acMetaFiltreZone: TAction;
    acIsovaleurSurZStation: TAction;
    ActListCdrVue2D: TActionList;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    CdrMetaFiltre1: TCdrMetaFiltre;
    chkDrawMaillage: TCheckBox;
    chkQuadrillage: TCheckBox;
    chkCotation: TCheckBox;
    chkEntrances: TCheckBox;
    chkIDStations: TCheckBox;
    chkParois: TCheckBox;
    chkPolygonales: TCheckBox;
    chkRemplissage: TCheckBox;
    chkSections: TCheckBox;
    chkStations: TCheckBox;
    editIsoValeur: TCurrencyEdit;
    GroupBox1: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lbX1Y1: TLabel;
    lbColorVisee: TStaticText;
    lbMesures: TStaticText;
    lbCoordonnees: TStaticText;
    btnQdrColor: TStaticText;
    lbFiltre: TLabel;
    lbX2Y2: TLabel;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    mnuMetaFiltreStation: TMenuItem;
    pnlMaillage: TPanel;
    pnlDebugOnglet: TPanel;
    pbDG: TPaintBox;


    Panel1:  TPanel;
    lbInfos: TStaticText;
    pnlHistoDirections: TPanel;
    pnlFullInfos: TPanel;
    lbColorReseau: TStaticText;
    lbStation: TStaticText;
    lbGCSMouse: TStaticText;
    PopUpCdrVue2D: TPopupMenu;
    sclDeplX: TScrollBar;
    sclDeplY: TScrollBar;
    lbModeTravail: TStaticText;
    btnBGColor: TStaticText;
    lbOngQdrSpc: TStaticText;
    btnCouleurIsovaleur: TStaticText;
    Vue:     TPaintBox;
    Vue1:    TPaintBox;
    Vue2:    TPaintBox;

    procedure acIsovaleurSurZStationExecute(Sender: TObject);
    procedure acMetaFiltreCodeExecute(Sender: TObject);
    procedure acMetaFiltreCouleurExecute(Sender: TObject);
    procedure acMetaFiltreExpeExecute(Sender: TObject);
    procedure acMetaFiltreSerieExecute(Sender: TObject);
    procedure acMetaFiltreTopoAnneeExecute(Sender: TObject);
    procedure acMetaFiltreVueCouranteExecute(Sender: TObject);
    procedure acMetaFiltreZoneExecute(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure chkDrawMaillageChange(Sender: TObject);
    procedure editIsoValeurChange(Sender: TObject);
    procedure Finalise;
    procedure FrameResize(Sender: TObject);
    procedure lbMesuresClick(Sender: TObject);
    procedure pbDGPaint(Sender: TObject);
    procedure PopUpCdrVue2DPopup(Sender: TObject);
    procedure sclDeplXChange(Sender: TObject);
    procedure sclDeplYChange(Sender: TObject);
    procedure SetElementsDrawn(const ED: TElementsDrawn);
    function GetElementsDrawn: TElementsDrawn;



    procedure SetModeTravail(const MT: TModesTravail);
    procedure SetViewLimits(const X1, Y1, X2, Y2: double);

    procedure ApplyMetaFiltre;
    procedure btnCouleurIsovaleurClick(Sender: TObject);
    procedure VueClick(Sender: TObject);

    procedure VueMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure VueMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure VueMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure VuePaint(Sender: TObject);
  private
    { private declarations }
    // onglets
    FArrOngletsParams : TArrOngletsParams;
    FCurrIdxOnglet    : byte;
    //FCurrentOnglet    : TVue2DParams;

    //----------------------------
    //*)
    // station courante
    FBaseStation : TEntite;
    // station trouvée (peut être différente de la station courante)
    FLastStationFound: TEntite;
    FAStationIsFound: boolean;

    // souris
    FSCRMouse: TPoint;
    FGCSMouse: TPoint2Df;

    // Table des entités
    FTableDesEntites: TTableDesEntites;
    // peut-on dessiner ?
    FCanDraw:     boolean;
    // station courante
    FCurrentStation: TEntite;
    //------------
    // variables diverses
    FRappHLVue:   double;
    FRappScrReal: double;
    FInvRappScrReal: double;
    // modes de travail
    FModesTravail: TModesTravail;

    // limites du dessin, utilisées dans les calculs
    FRXMini:      double;
    FRXMaxi:      double;
    FRYMini:      double;
    FRYMaxi:      double;
    // contrôle du zoom
    FZC1, FZC2:   TPoint2Df;
    FZP1, FZP2:   TPoint;
    FZoomC1OK:    boolean;


    // histogramme visible
    FHistoDirectionsVisible: boolean;
    // maillage de surface
    FMyMaillage: TMaillage;
    FMaillageTrace: boolean;

    procedure CenterAtPositionXY(const X, Y: Double);
    procedure DeplacerVue(const DepX, DepY: integer);
    procedure DisplayFullInfosStation(const S: TEntite);
    function GetCoordsMonde(const PP: TPoint): TPoint2Df;
    function GetCoordsPlan(const PM: TPoint2Df): TPoint;
    function GetRYMaxi: double;

    procedure SetBasePoint(const E: TEntite);
    // afficher le débogage du système d'onglets
    procedure DisplayDebugOnglet(const O: TVue2DParams);

  public
    function Initialize(const Filename: string): boolean;

    { public declarations }
    function GetNbOngletsVues: integer;
    function GetCurrentOngletIdx: integer;
    function GetOngletByIndex(const Idx: integer): TVue2DParams;
    procedure SetCurrentIdxOnglet(const Idx: byte);
    procedure PutOngletByIndex(const Idx: integer; const O: TVue2DParams);

    procedure SetNomOnglet(const S: string);

    procedure SetQuadrillageSpc(const Spacing: double);
    function  GetQuadrillageSpc: double;
    procedure SetQuadrillageColor(const Col: TColor);
    function  GetQuadrillageColor: TColor;
    function  GetNomOnglet: string;
    procedure SetBackGrdColor(const Col: TColor);
    function  GetBackGrdColor: TColor;
    function  GetFiltreOnglet: string;
    procedure SetFiltreOnglet(const F: string);

    function  GetXYZMini: TPoint3Df;
    function  GetXYZMaxi: TPoint3Df;
    function  GetTOPFileName: string;
    function GetNbEntites: integer;
    procedure SetStationInfo(const S: TEntite);
    procedure ResetVue;
    procedure RedessinEcran;
    function GetCanDraw: boolean;
    procedure SetParamsVue;
    // recherche de station et centrage éventuel si trrouvée
    procedure FindAndCenterStation;
    // générer un fichier TOP filtré
    function Generate3DMetafiltered(const AFileTop: string): integer;
    // attraper le filtre courant
    function GetMetaFiltre: string;
    // recalculer et afficher diagramme des directions
    procedure DisplayOrHideHistoDirections();
    // dessiner un texte T à l'endroit (x, y) avec les attributs Attr
    procedure DrawTexte(const Cnv: TCanvas; const X, Y: double;  const Attr: TTexteAttributs; const Texte: string);
    // renvoie le pointeur sur la table des entités
    function GetPointerTableEntites: TTableDesEntites;
    // passage du pointeur sur le maillage
    procedure SetMaillagePointer(const MP: TMaillage);
  end;

implementation

uses
  dlgSelectElementsDrawn;

function TCadreDessin2D.GetCanDraw: boolean;
begin
  Result := FCanDraw;
end;

procedure TCadreDessin2D.SetStationInfo(const S: TEntite);
begin
  lbInfos.Caption := Format('[%d.%d] %s (Z=%.0f)', [S.Entite_Serie, S.Entite_Station,
                                                    S.ID_Litteral_Pt,
                                                    S.Une_Station_2_Z]);
end;

procedure TCadreDessin2D.ApplyMetaFiltre;
var
  Filtre: string;
begin
  Filtre := CdrMetaFiltre1.GetFiltre;
  // on met le filtre dans l'onglet courant
  self.SetFiltreOnglet(Filtre);
  // traitements
  FTableDesEntites.MetaFiltre(Filtre);
  FTableDesEntites.ParseDiagram(18, 0.5);
  FTableDesEntites.ParseDepthHistogramme(20, 0.5);
  Vue.Invalidate;
  pbDG.Repaint;
end;

procedure TCadreDessin2D.btnCouleurIsovaleurClick(Sender: TObject);
begin
  btnCouleurIsovaleur.Color := ChooseColor(btnCouleurIsovaleur.Color);
  vue.Invalidate; //RedessinEcran;
end;


procedure TCadreDessin2D.DisplayFullInfosStation(const S: TEntite);
var
  L, A, P, G,D,H,B, dx, dy, dz, dp: Double;
begin
  dx := S.Une_Station_2_X - S.Une_Station_1_X;
  dy := S.Une_Station_2_Y - S.Une_Station_1_Y;
  dz := S.Une_Station_2_Z - S.Une_Station_1_Z;
  GetBearingInc(dx, dy, dz,
                L,A,P,
                360.0,360.0);
  lbInfos.Caption:=Format(rsVUE2D_FMT_INFOS_STATION_RAPIDE,
                            [S.ID_Litteral_Pt,
                             S.Entite_Serie,
                             S.Entite_Station,
                             S.Une_Station_2_X,
                             S.Une_Station_2_Y,
                             S.Une_Station_2_Z]);
  G:=Hypot2D((S.X2PG-S.Une_Station_2_X),
             (S.Y2PG-S.Une_Station_2_Y));
  D:=Hypot2D((S.X2PD-S.Une_Station_2_X),
             (S.Y2PD-S.Une_Station_2_Y));
  H:=S.Z2PH-S.Une_Station_2_Z;
  B:=S.Une_Station_2_Z - S.Z2PB;


  lbColorReseau.Color := S.ColorReseau;
  lbColorVisee.Color  := S.ColorEntite;
  lbColorReseau.Caption := Format('%d',[S.IdxReseau]);
  lbStation.Caption:= Format(rsVUE2D_FMT_INFOS_ID_STATION, [S.Entite_Serie, S.Entite_Station, S.ID_Litteral_Pt]);
  lbMesures.Caption:= Format(rsVUE2D_FMT_INFOS_MESURES, [L, A, P, G,D,H,B]);
  lbCoordonnees.Caption := Format(rsVUE2D_FMT_INFOS_COORDONNEES, [
                                 FormatterNombreAvecSepMilliers(S.Une_Station_2_X),
                                 FormatterNombreAvecSepMilliers(S.Une_Station_2_Y),
                                 FormatterNombreAvecSepMilliers(S.Une_Station_2_Z)]);
end;



function TCadreDessin2D.GetNbEntites: integer;
begin
  Result := FTableDesEntites.GetNbEntites;
end;

function TCadreDessin2D.GetXYZMini: TPoint3Df;
begin
  Result := FTableDesEntites.GetCoinBasGauche;
end;

function TCadreDessin2D.GetXYZMaxi: TPoint3Df;
begin
  Result := FTableDesEntites.GetCoinHautDroit;
end;

function TCadreDessin2D.GetTOPFileName: string;
begin
  Result := FTableDesEntites.GetFileName;
end;

procedure TCadreDessin2D.SetBackGrdColor(const Col: TColor);
begin
  //FBackGround := Col;
  FArrOngletsParams[FCurrIdxOnglet].ongBackGround := Col;
end;

function TCadreDessin2D.GetBackGrdColor: TColor;
begin
  //Result := FBackGround;
  Result := FArrOngletsParams[FCurrIdxOnglet].ongBackGround;
end;

function TCadreDessin2D.GetFiltreOnglet: string;
begin
  Result :=  FArrOngletsParams[FCurrIdxOnglet].ongVueFiltres;
end;

procedure TCadreDessin2D.SetFiltreOnglet(const F: string);
begin
   FArrOngletsParams[FCurrIdxOnglet].ongVueFiltres := F;
end;

procedure TCadreDessin2D.SetQuadrillageSpc(const Spacing: double);
begin
  //FQuadrillageSpc := Spacing;
  FArrOngletsParams[FCurrIdxOnglet].ongQdrSpc := Spacing;
end;

function TCadreDessin2D.GetQuadrillageSpc: double;
begin
  //Result := FQuadrillageSpc;
  Result := FArrOngletsParams[FCurrIdxOnglet].ongQdrSpc;
end;

procedure TCadreDessin2D.SetQuadrillageColor(const Col: TColor);
begin
  //FQuadrillageColor := Col;
  FArrOngletsParams[FCurrIdxOnglet].ongQdrColor := Col;
end;

function TCadreDessin2D.GetQuadrillageColor: TColor;
begin
  //Result := FQuadrillageColor;
  Result := FArrOngletsParams[FCurrIdxOnglet].ongQdrColor;
end;

procedure TCadreDessin2D.SetElementsDrawn(const ED: TElementsDrawn);
begin
  //FElementsDrawn := ED;
  FArrOngletsParams[FCurrIdxOnglet].ongElementsDrawn := ED;
end;

function TCadreDessin2D.GetElementsDrawn: TElementsDrawn;
begin
  //Result := FElementsDrawn;
  Result := FArrOngletsParams[FCurrIdxOnglet].ongElementsDrawn;
end;



function TCadreDessin2D.Initialize(const Filename: string): boolean;
var
  C1, C2: TPoint3Df;
  O: TVue2DParams;
  i: integer;
begin
  Result           := False;
  FAStationIsFound := False;
  FCurrIdxOnglet   := 0;
  FCanDraw         := False;
  SetModeTravail(mtREADY);
  FHistoDirectionsVisible := False;

  pnlHistoDirections.Visible := FHistoDirectionsVisible;
  FMaillageTrace   := False;
  FTableDesEntites := TTableDesEntites.Create;
  try
    if FTableDesEntites.LoadEntites(Filename, True) > 0 then
    begin
      FBaseStation := FTableDesEntites.GetEntiteFromSerSt(1,1);
      C1 := FTableDesEntites.GetCoinBasGauche;
      C2 := FTableDesEntites.GetCoinHautDroit;
      SetNomOnglet('Vue initiale');
      SetQuadrillageColor(clSilver);
      SetQuadrillageSpc(ProposerEquidistanceDef(C1, C2, 100.00));

      SetBackGrdColor(clWhite);
      SetElementsDrawn([edPolygonals, edQuadrilles, edWalls, edFillGalerie]);
      SetViewLimits(C1.X, C1.Y, C2.X, C2.Y); // renseigne aussi l'onglet courant
      FCanDraw := True;
      CdrMetaFiltre1.SetFiltre('');
      ApplyMetaFiltre;                   // renseigne aussi l'onglet courant
      ResetVue;
      Result := True;
      // initialiser le tableau d'onglets avec les paramètres de l'onglet courant
      PutOngletByIndex(0, FArrOngletsParams[0]);
      for i := 1 to High(FArrOngletsParams) do
      begin
        O := FArrOngletsParams[0];
        O.ongName := Format('Vue %0.2d',[i]);
        PutOngletByIndex(i, O);
      end;
      // initialiser la fonction de rappel du cadre MetaFiltre
      CdrMetaFiltre1.SetProcApplyMetaFiltre(ApplyMetaFiltre);
    end;
   except
   end;
end;
procedure TCadreDessin2D.SetCurrentIdxOnglet(const Idx: byte);
var
  FCurrentOnglet: TVue2DParams;
begin
  AfficherMessage(Format('%s.SetCurrentOngletByIdx(%d)',[ClassName, Idx]));
  FCurrIdxOnglet := Idx;
  FCurrentOnglet := FArrOngletsParams[Idx];
  // mise à jour du champ Filtres
  CdrMetaFiltre1.SetFiltre(FArrOngletsParams[Idx].ongVueFiltres);
  ApplyMetaFiltre();
  // recadrage
  SetViewLimits(FCurrentOnglet.ongX1, FCurrentOnglet.ongY1, FCurrentOnglet.ongX2, FCurrentOnglet.ongY2);
  Vue.Invalidate; //RedessinEcran;
end;

procedure TCadreDessin2D.Finalise;
begin
  try
    FTableDesEntites.Finalise;
  finally
    FTableDesEntites.Free;
  end;
end;

procedure TCadreDessin2D.FrameResize(Sender: TObject);
begin
  Vue.Invalidate;
end;

procedure TCadreDessin2D.lbMesuresClick(Sender: TObject);
begin

end;

procedure TCadreDessin2D.pbDGPaint(Sender: TObject);
begin
  if (FHistoDirectionsVisible) then FTableDesEntites.DrawDiagram(pbDG.Canvas, pbDG.Width, clBlue,clBlue);
end;



procedure TCadreDessin2D.PopUpCdrVue2DPopup(Sender: TObject);
begin
  mnuMetaFiltreStation.Caption := Format('Filtres d''après la station: %d.%d [%s]',
                                  [FCurrentStation.Entite_Serie,
                                   FCurrentStation.Entite_Station,
                                   FCurrentStation.ID_Litteral_Pt]);
  // titres du sous-menu
  acMetaFiltreSerie.Caption       := Format('Série %d', [FCurrentStation.Entite_Serie]);
  acMetaFiltreTopoDuJour.Caption  := Format('Relevés du %s', [DateToStr(FCurrentStation.DateLeve)]);
  acMetaFiltreReseau.Caption      := Format('Réseau %d', [FCurrentStation.IdxReseau]);
  acMetaFiltreCode.Caption        := Format('Code %d', [FCurrentStation.eCode]);
  acMetaFiltreExpe.Caption        := Format('Séance %d', [FCurrentStation.eExpe]);
  acMetaFiltreTopoAnnee.Caption   := Format('Année %d', [YearOf(FCurrentStation.DateLeve)]);
  acMetaFiltreCouleur.Caption     := Format('Couleur #%X', [FCurrentStation.ColorEntite]);
  acMetaFiltreVueCourante.Caption := 'Filtrer sur la vue courante';

  acIsovaleurSurZStation.Caption  := Format('Courbe de niveau pour la cote %.0f m', [FCurrentStation.Une_Station_2_Z]);

end;

procedure TCadreDessin2D.sclDeplXChange(Sender: TObject);
begin
  DeplacerVue(sclDeplX.Position, 0);
  sclDeplX.Position:=0;
end;

procedure TCadreDessin2D.sclDeplYChange(Sender: TObject);
begin
  DeplacerVue(0, sclDeplY.Position);
  sclDeplY.Position:=0;
end;

procedure TCadreDessin2D.Button1Click(Sender: TObject);
begin
  pnlFullInfos.Visible := not pnlFullInfos.Visible;
  if (pnlFullInfos.Visible) then DisplayFullInfosStation(FCurrentStation);
end;

procedure TCadreDessin2D.acMetaFiltreSerieExecute(Sender: TObject);
begin
  CdrMetaFiltre1.SetFiltre(Format('%s=%d',[rsMETAFILTRE_SERIE, FCurrentStation.Entite_Serie]));
  ApplyMetaFiltre();
  vue.Invalidate; //RedessinEcran;
end;

procedure TCadreDessin2D.acMetaFiltreCodeExecute(Sender: TObject);
begin
//  ApplyMetaFiltre(CdrMetaFiltre1.GetFiltre);
//  RedessinEcran;
end;

procedure TCadreDessin2D.acIsovaleurSurZStationExecute(Sender: TObject);
begin
  try
    FMaillageTrace := True;
    editIsoValeur.Value := FCurrentStation.Une_Station_2_Z;
    vue.Invalidate; // RedessinEcran;
  except
    FMaillageTrace := False;
  end;
end;

procedure TCadreDessin2D.acMetaFiltreCouleurExecute(Sender: TObject);
begin
//  ApplyMetaFiltre(CdrMetaFiltre1.GetFiltre);
//  RedessinEcran;
end;

procedure TCadreDessin2D.acMetaFiltreExpeExecute(Sender: TObject);
begin
//  ApplyMetaFiltre(CdrMetaFiltre1.GetFiltre);
//  RedessinEcran;
end;

procedure TCadreDessin2D.acMetaFiltreTopoAnneeExecute(Sender: TObject);
var
  YY,MM,DD: word;
  S1, S2: string;
begin
  SafeDecodeDate(FCurrentStation.DateLeve, YY, MM, DD);
  S1:=DateToStr(SafeEncodeDate(YY, 01, 01));
  S2:=DateToStr(SafeEncodeDate(YY, 12, 31));
  CdrMetaFiltre1.SetFiltre(Format('%s=(%s, %s)',[rsMETAFILTRE_DATE, S1, S2]));
  ApplyMetaFiltre();
end;

procedure TCadreDessin2D.acMetaFiltreVueCouranteExecute(Sender: TObject);
var
  PP: TPoint;
  FC1, FC2: TPoint2Df;
begin
  PP.X := 0;
  PP.Y := Vue.Height;
  FC1 := GetCoordsMonde(PP);
  PP.X := Vue.Width;
  PP.Y := 0;
  FC2 := GetCoordsMonde(PP);
  CdrMetaFiltre1.SetFiltre(Format('COORD_X=(%.2f, %.2f) & COORD_Y=(%.2f, %.2f)',
                          [FC1.X, FC2.X, FC1.Y, FC2.Y]));
  ApplyMetaFiltre();
  Vue.Invalidate; //RedessinEcran;

end;

procedure TCadreDessin2D.acMetaFiltreZoneExecute(Sender: TObject);
begin
  SetModeTravail(mtMFZONE);
end;

procedure TCadreDessin2D.Button2Click(Sender: TObject);
begin
  //ApplyMetaFiltre();
  Vue.Invalidate; // RedessinEcran;
end;

procedure TCadreDessin2D.Button3Click(Sender: TObject);
begin
  pnlDebugOnglet.Visible := not pnlDebugOnglet.Visible;
end;

procedure TCadreDessin2D.chkDrawMaillageChange(Sender: TObject);
begin
  FMaillageTrace := chkDrawMaillage.Checked;
  Vue.Invalidate; //RedessinEcran;
end;

procedure TCadreDessin2D.editIsoValeurChange(Sender: TObject);
begin

end;



procedure TCadreDessin2D.DeplacerVue(const DepX, DepY: integer);
var
  P : TPoint;
  dq, dx, dy: Double;

begin
  P.X := 0;
  P.Y := 0;
  FZC1:= GetCoordsMonde(P);
  P.X := Vue.Width   ; P.Y:=Vue.Height;
  FZC2:=GetCoordsMonde(P);

  dq  := 0.02 * (FZC2.X - FZC1.X);
  dx  :=  dq * DepX;
  dy  := -dq * DepY;
  SetViewLimits(FRXMini+dx, FRYMini+dy, FRXMaxi+dx,FRYMaxi+dy);
  Vue.Invalidate; //RedessinEcran;
end;
procedure TCadreDessin2D.ResetVue;
var
  cnMaxi, cnMini: TPoint3Df;
  DX, DY: Double;
  M, L, W: double;
  PM0, PM1: TPoint2Df;
  PP: TPoint;
begin
  if not FCanDraw then Exit;

  cnMini := FTableDesEntites.GetCoinBasGauche;
  cnMaxi := FTableDesEntites.GetCoinHautDroit;

  PP.X := Vue.Width;
  PP.Y := Vue.Height;
  PM1 := GetCoordsMonde(PP);
  PP.X := 0;
  PP.Y := 0;
  PM0 := GetCoordsMonde(PP);
  DX:=cnMaxi.X - cnMini.X;
  DY:=cnMaxi.Y - cnMini.Y;
  M := 0.01 * Hypot2D(DX, DY);
  // marge périmétrique
  cnMini.X := cnMini.X - M;
  cnMini.Y := cnMini.Y - M;
  cnMaxi.X := cnMaxi.X + M;
  cnMaxi.Y := cnMaxi.Y + M;

  DX:=cnMaxi.X - cnMini.X;
  DY:=cnMaxi.Y - cnMini.Y;
  //M:=0.00;
  L := IIF(DX>DY, DX, DY);
  L := L * (Vue.Width / Vue.Height);
  SetViewLimits(cnMini.X, cnMini.Y, cnMini.X + L, cnMini.Y+L);
  Vue.Invalidate;
end;
// affiche le dialogue de paramétrage d'une vue
procedure TCadreDessin2D.SetParamsVue;
begin
  with TfrmSelectElementsDrawn.Create(self) do
  begin
    try
      QSetElementsADessiner(GetElementsDrawn);
      QSetBackGrdColor(GetBackGrdColor);
      QSetQuadrillageColor(GetQuadrillageColor);
      QSetQuadrillageSpc(GetQuadrillageSpc);
      ShowModal;
      if ModalResult = mrOk then
      begin
        SetElementsDrawn(QGetElementsADessiner);
        SetBackGrdColor(QGetBackGrdColor);
        SetQuadrillageColor(QGetQuadrillageColor);
        SetQuadrillageSpc(QGetQuadrillageSpc);
        Vue.Invalidate;
      end;
    finally
      Release;
    end;
  end;
end;

procedure TCadreDessin2D.FindAndCenterStation; // validé
var
  WU: String;
  EWE: TEntite;
  zdar: Boolean;
begin
  WU := '';
  if (InputQuery(AnsiToUtf8(rsMSG_FIND_STATION_TITRE),
                 AnsiToUtf8(rsMSG_FIND_STATION_PROMPT), WU)) then
  begin
    zdar := FTableDesEntites.FindStationByCle(WU, EWE);
    if (zdar) then
    begin
      CenterAtPositionXY(EWE.Une_Station_2_X, EWE.Une_Station_2_Y);
      FAStationIsFound := zdar;
      FLastStationFound:= EWE;
      Vue.Invalidate; //  RedessinEcran();
    end;
  end;
end;

function TCadreDessin2D.GetRYMaxi: double;
  // Cette fonction calcule également d'autres paramètres
begin
  // calcul du rapport Hauteur/largeur de vue
  FRappHLVue := Vue.Height / Vue.Width;
  // calcul du rapport Ecran/Réel
  FRappScrReal := Vue.Width / (FRXMaxi - FRXMini);
  FInvRappScrReal := 1 / FRappScrReal;
  // calcul de la hauteur de visualisation
  Result := FRYMini + (FRXMaxi - FRXMini) * FRappHLVue;
end;

function TCadreDessin2D.GetCoordsPlan(const PM: TPoint2Df): TPoint;
begin
  Result.X := Round((PM.X - FRXMini) * FRappScrReal);
  Result.Y := Round((FRYMaxi - PM.Y) * FRappScrReal);
end;

function TCadreDessin2D.GetCoordsMonde(const PP: TPoint): TPoint2Df;
begin
  Result.X :=  FInvRappScrReal * PP.X + FRXMini;
  Result.Y := -FInvRappScrReal * PP.Y + FRYMaxi;
end;

procedure TCadreDessin2D.SetViewLimits(const X1, Y1, X2, Y2: double);
const
  Epsilon = 1e-2;
var
  qX1, qX2, qY1, qY2: double;
  d1, d2: double;
begin
  AfficherMessage(Format('%s.SetViewLimits(%.2f, %.2f, %.2f, %.2f',
    [ClassName, X1, Y1, X2, Y2]));
  qX1 := X1;
  qY1 := Y1;
  qX2 := X2;
  qY2 := Y2;

  d1 := qX2 - qX1;
  d2 := qY2 - qY1;
  // si zone trop étroite, abandonner
  if (Abs(d1) < Epsilon) or (Abs(d2) < Epsilon) then
    Exit;
  // échanger de manière à rendre indifférent le sens du rectangle de sélection
  if (qX2 < qX1) then Swap(qX1, qX2);
  if (qY2 < qY1) then Swap(qY1, qY2);

  FRXMini := qX1;
  FRXMaxi := qX2;
  FRYMini := qY1;
  FRYMaxi := qY2;
  //*)
  // Redéfinition de la hauteur maxi
  FRYMaxi := GetRYMaxi;
  // on met tout çà dans l'onglet
  FArrOngletsParams[FCurrIdxOnglet].ongX1 := FRXMini;
  FArrOngletsParams[FCurrIdxOnglet].ongY1 := FRYMini;
  FArrOngletsParams[FCurrIdxOnglet].ongX2 := FRXMaxi;
  FArrOngletsParams[FCurrIdxOnglet].ongY2 := FRYMaxi;
end;


procedure TCadreDessin2D.VuePaint(Sender: TObject);
begin
  RedessinEcran;
end;

procedure TCadreDessin2D.SetModeTravail(const MT: TModesTravail);
begin
  FModesTravail := MT;
  FZoomC1OK     := False;
  lbModeTravail.Caption := ChooseString(Ord(MT), ['READY', 'ZOOM', 'PAN', 'DISTANCE', 'FILTRE']);
end;

procedure TCadreDessin2D.RedessinEcran;
const
  R666 = 4;
var
  R: TRect;
  TmpBuffer: TBitmap;

  procedure DrawPipistrelle;
  const
    UnitLongueur: integer = 2;
  var
    i, j: integer;
    TraX, TraY: integer;
    Pipistrelle: array[0..1, 0..13] of TPoint;
  begin
    //AfficherMessage(' --> DrawPipistrelle');
    TraX := TmpBuffer.Width - UnitLongueur * 16;
    TraY := TmpBuffer.Height - UnitLongueur * 16;
    for i := 0 to 13 do
    begin
      Pipistrelle[0, i].X := 0;
      Pipistrelle[0, i].Y := 0;
      Pipistrelle[1, i].X := 0;
      Pipistrelle[1, i].Y := 0;
    end;
    Pipistrelle[0, 0].X  := 0;
    Pipistrelle[0, 0].Y  := 6 * UnitLongueur;
    Pipistrelle[0, 1].X  := 0;
    Pipistrelle[0, 1].Y  := -9 * UnitLongueur;
    Pipistrelle[0, 2].X  := 16 * UnitLongueur;
    Pipistrelle[0, 2].Y  := -9 * UnitLongueur;
    Pipistrelle[0, 3].X  := 0;
    Pipistrelle[0, 3].Y  := -5 * UnitLongueur;
    Pipistrelle[0, 4].X  := 3 * UnitLongueur;
    Pipistrelle[0, 4].Y  := -6 * UnitLongueur;
    Pipistrelle[0, 5].X  := 8 * UnitLongueur;
    Pipistrelle[0, 5].Y  := -1 * UnitLongueur;
    Pipistrelle[0, 6].X  := 13 * UnitLongueur;
    Pipistrelle[0, 6].Y  := -1 * UnitLongueur;
    Pipistrelle[0, 7].X  := 8 * UnitLongueur;
    Pipistrelle[0, 7].Y  := 4 * UnitLongueur;
    Pipistrelle[0, 9].X  := 2 * UnitLongueur;
    Pipistrelle[0, 9].Y  := 2 * UnitLongueur;
    Pipistrelle[0, 8].X  := 0;
    Pipistrelle[0, 8].Y  := 1 * UnitLongueur;
    Pipistrelle[0, 10].X := 4 * UnitLongueur;
    Pipistrelle[0, 10].Y := 8 * UnitLongueur;
    Pipistrelle[0, 11].X := 0;
    Pipistrelle[0, 11].Y := 6 * UnitLongueur;
    Pipistrelle[0, 12].X := 4 * UnitLongueur;
    Pipistrelle[0, 12].Y := -8 * UnitLongueur;
    Pipistrelle[0, 13].X := 5 * UnitLongueur;
    Pipistrelle[0, 13].Y := -8 * UnitLongueur;
    for i := 0 to 13 do
    begin
      Pipistrelle[1, i].X := -Pipistrelle[0, i].X;
      Pipistrelle[1, i].Y := Pipistrelle[0, i].Y;
      Pipistrelle[1, i].X := Pipistrelle[1, i].X + TraX;
      Pipistrelle[1, i].Y := TmpBuffer.Height - (Pipistrelle[1, i].Y + TraY);
      Pipistrelle[0, i].X := Pipistrelle[0, i].X + TraX;
      Pipistrelle[0, i].Y := TmpBuffer.Height - (Pipistrelle[0, i].Y + TraY);
    end;
    with TmpBuffer.Canvas do
    begin
      Pen.Color := clBlue;
      for j := 0 to 1 do
      begin
        MoveTo(Pipistrelle[j, 0].X, Pipistrelle[j, 0].Y);
        LineTo(Pipistrelle[j, 1].X, Pipistrelle[j, 1].Y);
        LineTo(Pipistrelle[j, 2].X, Pipistrelle[j, 2].Y);
        MoveTo(Pipistrelle[j, 3].X, Pipistrelle[j, 3].Y);
        for i := 4 to 8 do
          LineTo(Pipistrelle[j, i].X, Pipistrelle[j, i].Y);
        MoveTo(Pipistrelle[j, 9].X, Pipistrelle[j, 9].Y);
        for i := 10 to 11 do
          LineTo(Pipistrelle[j, i].X, Pipistrelle[j, i].Y);
        MoveTo(Pipistrelle[j, 4].X, Pipistrelle[j, 4].Y);
        for i := 12 to 13 do
          LineTo(Pipistrelle[j, i].X, Pipistrelle[j, i].Y);
      end;
    end;
  end;
  // polygonales
  procedure DrawPolygonals;
  var
    i:      integer;
    E:      TEntite;
    P1, P2: TPoint;
    PM:     TPoint2Df;
  begin
    //AfficherMessage(' --> DrawPolygonals');
    for i := 0 to FTableDesEntites.GetNbEntites - 1 do
    begin
      E := FTableDesEntites.GetEntite(i);
      if E.Type_Entite = tgENTRANCE then
      begin // dessin des entrées
        ;
      end
      else
      begin
        PM.X := E.Une_Station_1_X;
        PM.Y := E.Une_Station_1_Y;
        P1   := GetCoordsPlan(PM);
        PM.X := E.Une_Station_2_X;
        PM.Y := E.Une_Station_2_Y;
        P2   := GetCoordsPlan(PM);
        with TmpBuffer.Canvas do
        begin
          Pen.Color := E.ColorEntite;
          MoveTo(P1.X, P1.Y);
          LineTo(P2.X, P2.Y);
        end;
      end; // if E.Type_Entite=tgENTRANCE
    end;
  end;
  procedure DrawSections;
  var
    i:      integer;
    E:      TEntite;
    P1, P2: TPoint;
    PM:     TPoint2Df;
  begin
    for i := 0 to FTableDesEntites.GetNbEntites - 1 do
    begin
      E := FTableDesEntites.GetEntite(i);
      if (not E.Drawn) then Continue;

      PM.X := E.X2PD;
      PM.Y := E.Y2PD;
      P1   := GetCoordsPlan(PM);
      PM.X := E.X2PG;
      PM.Y := E.Y2PG;
      P2   := GetCoordsPlan(PM);
      with TmpBuffer.Canvas do
      begin
        Pen.Color := clSilver;
        MoveTo(P1.X, P1.Y);
        LineTo(P2.X, P2.Y);
      end;
    end;
  end;

  procedure DrawGaleries;
  var
    i:      integer;
    E:      TEntite;
    P:      array[0..3] of TPoint;
    PM:     TPoint2Df;
    q1, q2: boolean;
  begin
    q1 := (edFillGalerie in FArrOngletsParams[FCurrIdxOnglet].ongElementsDrawn);
    q2 := (edWalls in FArrOngletsParams[FCurrIdxOnglet].ongElementsDrawn);
    for i := 0 to FTableDesEntites.GetNbEntites - 1 do
    begin
      E := FTableDesEntites.GetEntite(i);
      if E.Type_Entite = tgENTRANCE then
      begin // dessin des entrées
      end
      else
      begin

        if not (E.Drawn) then
          Continue;
        PM.X := E.X1PG;
        PM.Y := E.Y1PG;
        P[0] := GetCoordsPlan(PM);
        PM.X := E.X1PD;
        PM.Y := E.Y1PD;
        P[1] := GetCoordsPlan(PM);
        PM.X := E.X2PD;
        PM.Y := E.Y2PD;
        P[2] := GetCoordsPlan(PM);
        PM.X := E.X2PG;
        PM.Y := E.Y2PG;
        P[3] := GetCoordsPlan(PM);
        with TmpBuffer.Canvas do
        begin
          if q1 then
          begin // remplissages
            Brush.Color := E.ColorEntite;
            Pen.Color   := Brush.Color;
            Polygon(P);
          end;
          if q2 then
          begin // parois
            Pen.Color := clBlack;
            MoveTo(P[0].X, P[0].Y);
            LineTo(P[3].X, P[3].Y);
            MoveTo(P[1].X, P[1].Y);
            LineTo(P[2].X, P[2].Y);

          end;
        end;
      end;
    end;
  end;

  procedure DrawStations;
  const
    R = 2;
  var
    i:  integer;
    E:  TEntite;
    P1: TPoint;
    PM: TPoint2Df;
  begin
    //AfficherMessage(' --> DrawStations');
    for i := 0 to FTableDesEntites.GetNbEntites - 1 do
    begin
      E := FTableDesEntites.GetEntite(i);
      if (not E.Drawn) then
        Continue;
      PM.X := E.Une_Station_2_X;
      PM.Y := E.Une_Station_2_Y;
      P1   := GetCoordsPlan(PM);
      with TmpBuffer.Canvas do
      begin
        Pen.Color   := clRed;
        Brush.Color := Pen.Color;
        Ellipse(P1.X - R, P1.Y - R, P1.X + R, P1.Y + R);
      end;
    end;
  end;

  procedure DrawCotation;
  var
    i:  integer;
    E:  TEntite;
    P:  TPoint;
    PM: TPoint2Df;
  begin
    //AfficherMessage(' --> DrawCotation');
    with TmpBuffer.Canvas do
    begin
      Brush.Color := clWhite;
      Brush.Style := bsSolid;
      Font.Color  := clBlue;
      Font.Name   := 'Arial';
      Font.Style  := [fsItalic];
      Font.Size   := 7;
    end;
    for i := 0 to FTableDesEntites.GetNbEntites - 1 do
    begin
      E := FTableDesEntites.GetEntite(i);
      if (not E.Drawn) then
        Continue;
      PM.X := E.Une_Station_2_X;
      PM.Y := E.Une_Station_2_Y;
      P    := GetCoordsPlan(PM);
      with TmpBuffer.Canvas do TextOut(P.X + 4, P.Y + 2, Format('%.1f', [E.Une_Station_2_Z]));
    end;
  end;

  procedure DrawIDStations;
  var
    i:  integer;
    E:  TEntite;
    PM: TPoint2Df;
    P:  TPoint;
  begin
    with TmpBuffer.Canvas do
    begin
      Brush.Color := clWhite;
      Brush.Style := bsSolid;
      Font.Color  := clMaroon;
      Font.Name   := 'Arial';
      Font.Style  := [];
      Font.Size   := 7;
    end;
    for i := 0 to FTableDesEntites.GetNbEntites - 1 do
    begin
      E := FTableDesEntites.GetEntite(i);
      if (not E.Drawn) then Continue;
      PM.X := E.Une_Station_2_X;
      PM.Y := E.Une_Station_2_Y;
      P    := GetCoordsPlan(PM);
      with TmpBuffer.Canvas do TextOut(P.X + 4, P.Y - Font.Size - 4, Trim(E.ID_Litteral_Pt));
    end;
  end;

  procedure DrawEntrancesOrPointEntities;
  var
    i:      integer;
    E:      TEntite;
    P1, P2: TPoint;
    PM:     TPoint2Df;
  begin
    //AfficherMessage(' --> DrawEntrances');
    for i := 0 to FTableDesEntites.GetNbEntites - 1 do
    begin
      E := FTableDesEntites.GetEntite(i);
      if E.Type_Entite = tgENTRANCE then
      begin // dessin des entrées
        with TmpBuffer.Canvas do
        begin
          PM.X := E.Une_Station_2_X;
          PM.Y := E.Une_Station_2_Y;
          P2   := GetCoordsPlan(PM);
          Brush.Color := clFuchsia;
          Ellipse(P2.X - R666, P2.Y - R666, P2.X + R666, P2.Y + R666);
        end;
      end;
    end; // for
  end;
  procedure DrawQuadrilles;
  var
    P1, P2:   TPoint;
    C1, C2:   TPoint2Df;
    // carré de la vue courante
    C1_VueCourante, C2_VueCourante: TPoint2Df;
    q, t:     integer;
    A, B, B0: double;
    S: string;
    qQuadrillageSpc: double;
  begin
    qQuadrillageSpc:= FArrOngletsParams[FCurrIdxOnglet].ongQdrSpc;
    with FTableDesEntites do
    begin
      with TmpBuffer.Canvas do
      begin
        Pen.Color := FArrOngletsParams[FCurrIdxOnglet].ongQdrColor;
        Font.Name := DEFAULT_FONT_NAME;
        Font.Size := 8;
      end;
      TmpBuffer.Canvas.Brush.Color := FArrOngletsParams[FCurrIdxOnglet].ongBackGround;
      // vue courante
      P1.X := 0;
      P1.Y := Vue.Height;
      C1_VueCourante := GetCoordsMonde(P1);
      P1.X := Vue.Width;
      P1.Y := 0;
      C2_VueCourante := GetCoordsMonde(P1);
      t := trunc(C1_VueCourante.X / qQuadrillageSpc);
      A := qQuadrillageSpc * t;
      while (A < C2_VueCourante.X) do
      begin
        C1.X := A;
        C1.Y := C1_VueCourante.Y; //C1.Y:=cnMini.Y;
        C2.X := A;
        C2.Y := C2_VueCourante.Y; //C2.Y:=cnMaxi.Y;
        P1   := GetCoordsPlan(C1);
        P2   := GetCoordsPlan(C2);
        TmpBuffer.Canvas.MoveTo(P1.X, P1.Y);
        TmpBuffer.Canvas.LineTo(P2.X, P2.Y);
        //-------- Coordonnées en rive
        S:=Format('%.0f',[A]);
        q:=P1.X - TmpBuffer.Canvas.TextWidth(S) div 2;
        TmpBuffer.Canvas.TextOut(q, 1, S);
        //----------------------------
        A := A + qQuadrillageSpc;
      end;
      //t:=trunc(cnMini.Y / QdrEspc);
      t := trunc(C1_VueCourante.Y / qQuadrillageSpc);
      A := qQuadrillageSpc * t;
      while (A < C2_VueCourante.Y) do
      begin
        C1.X := C1_VueCourante.X;
        C1.Y := A;
        C2.X := C2_VueCourante.X;
        C2.Y := A;

        P1 := GetCoordsPlan(C1);
        P2 := GetCoordsPlan(C2);
        TmpBuffer.Canvas.MoveTo(P1.X, P1.Y);
        TmpBuffer.Canvas.LineTo(P2.X, P2.Y);
        //-------- Coordonnées en rive
        S:=Format('%.0f',[A]);
        q:=P1.Y - TmpBuffer.Canvas.TextHeight(S) div 2;
        TmpBuffer.Canvas.TextOut(2, q, S);
        //----------------------------
        A := A + qQuadrillageSpc;
      end;

    end;// with Ftable...
  end;
  procedure DrawCadre;
  var
    C1, C2: TPoint3Df;
    F1, F2: TPoint2Df;
    P1, P2: TPoint;
  begin
    C1 := FTableDesEntites.GetCoinBasGauche;
    C2 := FTableDesEntites.GetCoinHautDroit;
    //showmessagefmt('R: %f %f %f %f',[ C1.X,  C1.Y,  C2.X,  C2.Y]);
    F1.X := C1.X;
    F1.Y := C1.Y;
    F2.X := C2.X;
    F2.Y := C2.Y;
    P1 := GetCoordsPlan(F1);
    P2 := GetCoordsPlan(F2);
    with TmpBuffer.Canvas do begin
      Pen.Color:=clRed;
      MoveTo(P1.X, P1.Y);
      LineTo(P2.X, P1.Y);
      LineTo(P2.X, P2.Y);
      LineTo(P1.X, P2.Y);
      LineTo(P1.X, P1.Y);
    end;
  end;
  procedure DrawLastStationFound(const E: TEntite);
  const DEMI_COTE = 8;
  var
    PP: TPoint;
    PM: TPoint2Df;
  begin
    if (FAStationIsFound) then
    begin
      PM.X := E.Une_Station_2_X; PM.Y := E.Une_Station_2_Y;
      PP := GetCoordsPlan(PM);
      AfficherMessage(Format('-- DrawLastStationFound(%f, %f - %d, %d)', [PM.X, PM.Y, PP.X, PP.Y]));
      with TmpBuffer.Canvas do
      begin
        Pen.Color   := clBlue;
        Brush.Style := bsSolid;
        brush.Color := clAqua;
        EllipseC(PP.X, PP.Y, DEMI_COTE, DEMI_COTE);
      end;
    end;
  end;
  procedure DrawMaillage;
  begin
    try
      if (FMaillageTrace) then FMyMaillage.TracerMaillage(TmpBuffer.Canvas, GetCoordsPlan, editIsoValeur.Value, btnCouleurIsovaleur.Color);
    except
    end;
  end;
begin
  if not (FCanDraw) then
    Exit;
  TmpBuffer := TBitmap.Create;
  try
    with TmpBuffer do
    begin
      Height   := vue.Height;
      Width    := Vue.Width;
      R.Left   := Vue.Left;
      R.Top    := Vue.Top;
      R.Bottom := Vue.Top  + Vue.Height;
      R.Right  := Vue.Left + Vue.Width;
      Canvas.Brush.Color := FArrOngletsParams[FCurrIdxOnglet].ongBackGround;
      Canvas.FillRect(R);

      DrawPipistrelle;
      DrawGaleries;
      if edQuadrilles    in FArrOngletsParams[FCurrIdxOnglet].ongElementsDrawn then DrawQuadrilles;
      if edPolygonals    in FArrOngletsParams[FCurrIdxOnglet].ongElementsDrawn then DrawPolygonals;
      if edCrossSections in FArrOngletsParams[FCurrIdxOnglet].ongElementsDrawn then DrawSections;
      if edStations      in FArrOngletsParams[FCurrIdxOnglet].ongElementsDrawn then DrawStations;
      if edCotation      in FArrOngletsParams[FCurrIdxOnglet].ongElementsDrawn then DrawCotation;
      if edIDStations    in FArrOngletsParams[FCurrIdxOnglet].ongElementsDrawn then DrawIDStations;
      if edENTRANCES     in FArrOngletsParams[FCurrIdxOnglet].ongElementsDrawn then DrawEntrancesOrPointEntities;
      // dernière station trouvée
      DrawLastStationFound(FLastStationFound);
      // maillage
      DrawMaillage;

    end;
    vue.Canvas.CopyRect(R, TmpBuffer.Canvas, R);
  finally
    TmpBuffer.Free;
  end;
  // affichage de débug pour les onglets
  if (pnlDebugOnglet.Visible) then DisplayDebugOnglet(FArrOngletsParams[FCurrIdxOnglet]);
end;
// /!\ En Lazarus, l'événement OnClick est déclenché aussi au relevage de la souris
// Y mettre les événements du OnMouseUp
procedure TCadreDessin2D.VueClick(Sender: TObject);
const
  FMT_LIGNE_STATION = '%s: %s - (%d.%d) - X = %.2f, Y = %.2f, Z = %.2f';

var
  PM: TPoint2Df;
  dx, dy, dz, dist: double;
  qL, qAZ, qP: double;
  ST1, ST2: TEntite;
  WU: String;
  zdx: Extended;
  zdy: Extended;
  function EWE(const Q: TEntite; const fromTo: string): string;
  begin
    Result := Format(FMT_LIGNE_STATION, [FromTo,
                                         Q.ID_Litteral_Pt,
                                         Q.Entite_Serie, Q.Entite_Station,
                                         Q.Une_Station_2_X, Q.Une_Station_2_Y, Q.Une_Station_2_Z
                                         ]);
  end;
begin
  //Exit;
  if not FCanDraw then Exit;
  PM := GetCoordsMonde(FSCRMouse);
  FCurrentStation := FTableDesEntites.GetEntiteFromXY(PM.X, PM.Y);
  SetStationInfo(FCurrentStation);
  if (pnlFullInfos.Visible) then DisplayFullInfosStation(FCurrentStation);

  case FModesTravail of
    mtPANVUE,
    mtDISTANCE:
    begin
      if FZoomC1OK then begin
        FZC2      := GetCoordsMonde(FSCRMouse);
        FZP2      := FSCRMouse;
        FZP2      := FSCRMouse;

        zdx := -(FZC2.X - FZC1.X);
        zdy := -(FZC2.Y - FZC1.Y);

        FZoomC1OK := False;

        ST1 := FTableDesEntites.GetEntiteFromXY(FZC1.X, FZC1.Y);
        ST2 := FTableDesEntites.GetEntiteFromXY(FZC2.X, FZC2.Y);

        dx := (ST2.Une_Station_2_X - ST1.Une_Station_2_X);
        dy := (ST2.Une_Station_2_Y - ST1.Une_Station_2_Y);
        dz := (ST2.Une_Station_2_Z - ST1.Une_Station_2_Z);

        case FModesTravail of
          mtPANVUE  :
            begin
              SetViewLimits(FRXMini + zdx, FRYMini + zdy, FRXMaxi + zdx, FRYMaxi + zdy);
            end;
          mtDISTANCE:
            begin
              dist := Hypot3D(dx, dy, dz);

              GetBearingInc(dx, dy, dz, qL, qAZ, qP, 360.00, 360.00);
              WU := Format('Distance = %.2f m - Az = %.2f° - P = %.2f°',[dist, qAz, qP]);
              WU := WU + #13#10 + EWE(ST1, 'Du point');
              WU := WU + #13#10 + EWE(ST2, 'Au point');
              WU := WU + #13#10 + Format('dx = %.2f m, dy = %.2f m, dz = %.2fm', [dx, dy, dz]);

              ShowMessage(WU);
            end;
        end;

        SetModeTravail(mtREADY);
        Vue.Invalidate;
      end else begin
        FZC1      := GetCoordsMonde(FSCRMouse);
        FZP1      := FSCRMouse;
        FZP2      := FSCRMouse;
        FZoomC1OK := True;
     end;
    end;
    mtZOOM,
    mtMFZONE:
    begin // zoom et filtre sur sélection
      if FZoomC1OK then begin
        FZC2      := GetCoordsMonde(FSCRMouse);
        FZP1      := FSCRMouse;
        FZP2      := FSCRMouse;
        FZoomC1OK := False;
        case FModesTravail of
          mtZOOM:
            begin
              SetViewLimits(FZC1.X, FZC1.Y, FZC2.X, FZC2.Y);
            end;
          mtMFZONE:
            begin
              if (FZC1.X > FZC2.X) then Swap(double(FZC1.X), double(FZC2.X));
              if (FZC1.Y > FZC2.Y) then Swap(double(FZC1.Y), double(FZC2.Y));
              CdrMetaFiltre1.SetFiltre(Format('COORD_X=(%.2f, %.2f) & COORD_Y=(%.2f, %.2f)',
                                        [FZC1.X, FZC2.X, FZC1.Y, FZC2.Y]));
              ApplyMetaFiltre();
            end;
        else
          ;
        end;
        SetModeTravail(mtREADY);
        Vue.Invalidate;
      end else begin
        FZC1      := GetCoordsMonde(FSCRMouse);
        FZP1      := FSCRMouse;
        FZP2      := FSCRMouse;
        FZoomC1OK := True;
      end

    end;
  end;

end;

/// Les procédures
procedure TCadreDessin2D.VueMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  PP: TPoint;
begin
  //(*
  if not FCanDraw then Exit;
  FZoomC1OK := False; // désactiver toutes sélections

  //{$IFDEF GHTOPO_STANDARD}

  if Shift = [ssShift, ssLeft] then SetModeTravail(mtZOOM);
   // FModesTravail := mtZOOM;
  if Shift = [ssCtrl, ssLeft] then SetModeTravail(mtPANVUE);
   // FModesTravail := mtPANVUE;

  case FModesTravail of
    mtPANVUE,
    mtDISTANCE:
    begin

      PP.X      := X;
      PP.Y      := Y;
      FZC1      := GetCoordsMonde(PP);
      FZP1      := PP;
      FZP2      := PP;
      FZoomC1OK := True;
      with Vue.Canvas do
      begin
        Pen.Mode  := pmNotXOR;
        Pen.Color := clSilver;
        PP.X      := X;
        PP.Y      := Y;
      end;
    end;
    mtZOOM,
    mtMFZONE:
    begin // zoom et filtre sur sélection

      PP.X      := X;
      PP.Y      := Y;
      FZC1      := GetCoordsMonde(PP);
      FZP1      := PP;
      FZP2      := PP;
      FZoomC1OK := True;
      with Vue.Canvas do
      begin
        Pen.Mode  := pmNotXOR;
        Pen.Color := clSilver;
      end;
    end;
  end;
  //*)  {$ENDIF}
end;

procedure TCadreDessin2D.VueMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
var

  PM: TPoint2Df;
  //P: TPoint;
  procedure DrawLigne(const P1, P2: TPoint);
  begin
    with Vue.Canvas do
    begin
      MoveTo(P1.X, P1.Y);
      LineTo(P2.X, P2.Y);
    end;
  end;

  procedure DrawRectangle(const P1, P2: TPoint);
  begin
    with Vue.Canvas do
    begin
      MoveTo(P1.X, P1.Y);
      LineTo(P2.X, P1.Y);
      LineTo(P2.X, P2.Y);
      LineTo(P1.X, P2.Y);
      LineTo(P1.X, P1.Y);
    end;

  end;

begin
  if not FCanDraw then Exit;

  FSCRMouse.X := X;
  FSCRMouse.Y := Y;
  FGCSMouse   := GetCoordsMonde(FSCRMouse);
  //(* {$IFDEF GHTOPO_STANDARD}
  lbGCSMouse.Caption:=Format('%.2f, %.2f',[FGCSMouse.X, FGCSMouse.Y]);
  case FModesTravail of
    mtPANVUE,
    mtDISTANCE:
    begin
      if not (FZoomC1OK) then
        Exit;
      with Vue.Canvas do
      begin
        DrawLigne(FZP1, FZP2);
        FZP2.X := X;
        FZP2.Y := Y;
        DrawLigne(FZP1, FZP2);

      end;
    end;
    mtZOOM,
    mtMFZONE:
    begin
      if not (FZoomC1OK) then
        Exit;
      with Vue.Canvas do
      begin
        DrawRectangle(FZP1, FZP2);
        FZP2.X := X;
        FZP2.Y := Y;
        DrawRectangle(FZP1, FZP2);
      end;
    end;
    else;
  end;
  //*) {$ENDIF}
end;

// /!\ Cet événement n'est pas traité.
procedure TCadreDessin2D.VueMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  dx, dy, dz: double;
  P:      TPoint;
  E1, E2: TEntite;
  Lg, Az, Pe: double;
  S1, S2, S3, S4, S5: string;
begin
  //ShowMessage(ChooseString(Ord(FModesTravail), ['READY', 'ZOOM', 'PANVUE', 'DISTANCE', 'MFZONE']));
  exit;
  (*
  if not FCanDraw then Exit;
  case FModesTravail of
    mtPANVUE:
      begin
        if (FZoomC1OK) then
        begin
          P.X  := X;
          P.Y  := Y;
          FZC2 := GetCoordsMonde(P);
          with Vue.Canvas do
          begin
            Pen.Mode := pmCopy;
          end;
          dx := -(FZC2.X - FZC1.X);
          dy := -(FZC2.Y - FZC1.Y);
          SetViewLimits(FRXMini + dx, FRYMini + dy, FRXMaxi + dx, FRYMaxi + dy);
          Vue.Invalidate;
        end;
        SetModeTravail(mtREADY);
      end;
    mtMFZONE:
      begin

        if (FZoomC1OK) then
        begin
          P.X  := X;
          P.Y  := Y;
          FZC2 := GetCoordsMonde(P);
          with Vue.Canvas do
          begin
            Pen.Mode := pmCopy;
          end;

          if (FZC1.X > FZC2.X) then
            Swap(double(FZC1.X), double(FZC2.X));
          if (FZC1.Y > FZC2.Y) then
            Swap(double(FZC1.Y), double(FZC2.Y));
          editFiltres.Text := Format('COORD_X=(%.2f, %.2f) & COORD_Y=(%.2f, %.2f)',
                            [FZC1.X, FZC2.X, FZC1.Y, FZC2.Y]);
          ApplyMetaFiltre(editFiltres.Text);
          RedessinEcran;
        end;
        SetModeTravail(mtREADY);
      end;
    mtZOOM:
      begin
        if (FZoomC1OK) then
        begin
          P.X  := X;
          P.Y  := Y;
          FZC2 := GetCoordsMonde(P);
          with Vue.Canvas do
          begin
            Pen.Mode := pmCopy;
          end;
          SetViewLimits(FZC1.X, FZC1.Y, FZC2.X, FZC2.Y);
        end;
        SetModeTravail(mtREADY);
        RedessinEcran;
      end;
    else
      begin
        ;
      end;
  end;
  //*)
end;

// centrer le dessin sur le point trouvé
procedure TCadreDessin2D.CenterAtPositionXY(const X,Y: Double);
var
  dx, dy: Double;
begin
  dx := (FRXMaxi - FRXMini)/2;
  dy := (FRYMaxi - FRYMini)/2;
  SetViewLimits(X - dx, Y - dy, X + dx, Y + dy);
  Vue.Invalidate; // RedessinEcran;
end;
procedure TCadreDessin2D.SetBasePoint(const E: TEntite);
begin
  FBaseStation := E;
  Vue.Invalidate; // RedessinEcran;
end;

procedure TCadreDessin2D.DisplayDebugOnglet(const O: TVue2DParams);
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
 lbOngQdrSpc.Caption   := Format('%.0f',[O.ongQdrSpc]);
 btnBGColor.Color  := O.ongBackGround;
 btnQdrColor.Color := O.ongQdrColor;
 lbFiltre.Caption  := O.ongVueFiltres;
 //cmbTypeQuadrillage.ItemIndex := Ord(O.ongQdrType);
 lbX1Y1.Caption := Format('%.0f > %.0f', [O.ongX1, O.ongX2]);
 lbX2Y2.Caption := Format('%.0f > %.0f', [O.ongY1, O.ongY2]);
end;

function TCadreDessin2D.GetNbOngletsVues: integer;
begin
  Result := 1 + High(FArrOngletsParams);
end;

function TCadreDessin2D.GetCurrentOngletIdx: integer;
begin
  Result := FCurrIdxOnglet;
end;

function TCadreDessin2D.GetOngletByIndex(const Idx: integer): TVue2DParams;
begin
  Result := FArrOngletsParams[Idx];
end;


procedure TCadreDessin2D.PutOngletByIndex(const Idx: integer; const O: TVue2DParams);
begin
  FArrOngletsParams[Idx] := O;
end;

procedure TCadreDessin2D.SetNomOnglet(const S: string);
begin
  FArrOngletsParams[FCurrIdxOnglet].ongName := S;
end;

function TCadreDessin2D.GetNomOnglet: string;
begin
  Result := FArrOngletsParams[FCurrIdxOnglet].ongName;
end;





function TCadreDessin2D.GetMetaFiltre: string;
begin
  Result := CdrMetaFiltre1.GetFiltre;
end;

procedure TCadreDessin2D.DisplayOrHideHistoDirections();
begin

  FHistoDirectionsVisible:= Not FHistoDirectionsVisible;
  pnlHistoDirections.Visible := FHistoDirectionsVisible;
  AfficherMessage(Format('%s.DisplayOrHideHistoDirections (%s)',[ClassName, BoolToStr(FHistoDirectionsVisible, 'Visible', 'Non visible')]));
  if (FHistoDirectionsVisible) then
  begin
    FTableDesEntites.ParseDiagram(18, 0.50);
    pbDG.Repaint;
  end;
end;


procedure TCadreDessin2D.DrawTexte(const Cnv: TCanvas; const X, Y: double;  const Attr: TTexteAttributs; const Texte: string);
var
  PM, PM1: TPoint2Df;
  PP, PP1: TPoint;
  BB: TSize;
  dx, dy: integer;
  QTextBitmap: TBitmap;
begin
  PM.X := X; PM.Y := Y;
  PM1.X := PM.X;
  PM1.Y := PM.Y + Attr.HauteurTexte;
  PP := GetCoordsPlan(PM);
  PP1:= GetCoordsPlan(PM1);
  Cnv.Font.Color := Attr.FontColor;
  Cnv.Font.Name  := Attr.FontName;
  Cnv.Font.Style := Attr.FontStyle;
  Cnv.Font.Size  := PP1.Y - PP.Y;
  BB := Cnv.TextExtent(Texte);
  // alignement vertical
  case Attr.Position of
    1, 2, 3: dy := -BB.cy;
    4, 5, 6: dy := -BB.cy div 2;
    7, 8, 9: dy := 0;
  else
    dy := 0;
  end;
  // alignement horizontal
  case Attr.Position of
    1, 4, 7: dx := 0;
    2, 5, 8: dx := -BB.cx div 2;
    3, 6, 9: dx := -BB.cx;
  else
    dx := 0;
  end;
  if (Attr.AngleRot = 0) then // process simplifié = dessin direct
  begin
    Cnv.TextOut(PP.X + dx, PP.Y + dy, Texte);
  end
  else
  begin // on va utiliser une image provisoire
    QTextBitmap := TBitmap.Create;
    try
      QTextBitmap.SetSize(BB.cx, BB.cy); // définit image de la taille du texte
      QTextBitmap.Canvas.Font := Cnv.Font; // copie de la fonte du canvas courant dans l'image provisoire
      // rotation
      //QTextBitmap.;
      // transfert de l'image
      Cnv.Draw(PP.X, PP.Y, QTextBitmap);
    finally
      QTextBitmap.Free;
    end;
  end;
end;

function TCadreDessin2D.GetPointerTableEntites: TTableDesEntites;
begin
  Result := FTableDesEntites;
end;

procedure TCadreDessin2D.SetMaillagePointer(const MP: TMaillage);
begin
  FMyMaillage := MP;
end;

// génération d'un fichier .TOP filtré
function TCadreDessin2D.Generate3DMetafiltered(const AFileTop: string): integer;
begin
  Result:=FTableDesEntites.GenerateMetaFilteredTOP(AFileTop, GetMetaFiltre());
end;

// tracé du maillage

initialization
{$I CdrDessin2D.lrs}

end.

