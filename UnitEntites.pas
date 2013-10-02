unit UnitEntites;
// Une version simplifiée de UnitTableEntites de GHTopo
// Date: 19/04/2012
// Statut: Fonctionnel pour 2D et 3D
// Des détails à revoir
{$INCLUDE CompilationParameters.inc}
interface

uses
  StructuresDonnees,
  UnitMessages_fr,
  {$IFDEF MSWINDOWS}
    {$IFDEF USE_CONVERTISSEUR_EPSG}
      ConversionsCoordonneesEPSG, // API Conversapi
    {$ENDIF}
    {$IFDEF USE_CONVERTISSEUR_ESIBERT}
      ConversionsCoordonneesESibert, // API E Sibert
    {$ENDIF}
    {$IFDEF USE_CONVERTISSEUR_JPC}
      ConvertisseurJPC, // API E Sibert
    {$ENDIF}
  {$ENDIF}
  {$IFDEF LINUX}
    ConversionsCoordonneesESibert,
  {$ENDIF}
  Common,
  Graphics,
  Classes, SysUtils, Math;

const
  LOW_INDEX = 1;
// filtres
type
  TCompOperateur = integer;

type
  TConnectorFiltres = (ftERROR, ftAND, ftOR);

type
  TTypeFiltre = (ftINVALID, ftSCALAR, ftINTERVAL, ftFLAGS);

type
  TFiltre = record
    Caption: string;
    TypeFiltre: TTypeFiltre;
    Filter: integer;
    Operateur: TCompOperateur;
    Basculed: boolean;
    Valeur: string;
    BorneInf: string;
    BorneSup: string;
    Flags: string;
    ConnectedWithPrevious: TConnectorFiltres;
  end;
// points 2D
type
  PointsTopo2D = record
    UID_Entite: integer;                 // ID unique de l'entité
    Type_Entite: byte;                    // Type d'entité
    DateLeve: TDateTime;
    Sub_System: integer;                 // Sous-réseau
    Une_Station_1_X: integer;                  // Extrémités des visées
    Une_Station_1_Y: integer;
    Une_Station_2_X: integer;                // Extrémités des visées
    Une_Station_2_Y: integer;
    Depth: double;                // Profondeur (distance à l'observateur);
  end;

type
  TQuad = record
    Visible: boolean;
    VertexA: TPoint3Df;
    VertexB: TPoint3Df;
    VertexC: TPoint3Df;
    VertexD: TPoint3Df;
    Normale: TPoint3Df;
    Depth: double;

    Vertex2D_A: TPoint;
    Vertex2D_B: TPoint;
    Vertex2D_C: TPoint;
    Vertex2D_D: TPoint;
    Couleur: TColor;
    Type_Entite: byte;
  end;


// silhouette d'une série
type

{ TSilhouetteGalerie }

TSilhouetteGalerie = class(TList)
  private
    FFillColor: TColor;
    FPenColor: TColor;
    FArrayPolygone: TArrayPoints2Df;
    constructor Create;
    destructor Destroy;
    procedure AddPtsParois(const E: TPtsSectionTransversale);
    function BuildPolygoneParois: Boolean;
    function GetNbPtsParois: integer;
    function GetPointPolygoneParois(const Idx: integer): TPoint2Df;
    function GetNbPointsPolygoneParois: integer;
    function GetPtsParois(const Idx: integer): TPtsSectionTransversale;
  public
    function  GetFillColor: TColorRGBA;
    procedure ClearSilhouette(const PenColor, FillColor: TColor);

end;

type

  { TTableDesEntites }

  TTableDesEntites = class
    function LoadEntites(const MyFichierTOP: string;
      const SortedByDepth: boolean): integer;
    procedure Finalise;
    function RecenserDates: integer;
    function RecenserReseaux: integer;
    function RecenserCouleurs: integer;
    procedure SetMinMax;


    function GetEntite(const Idx: integer): TEntite;
    function GetFileName: string;

    function MetaFiltre(const Filtre: string): integer;

    //procedure ReinitParamVue;
  private
    // fonction de rappel
    FProcDispProgression: TProcDisplayProgression;
    //-----------------------
    FFichierTOP: string;
    FNbEntites: integer;
    TableEntites: array of TEntite;

    TableDates: array of TDatesTopo;
    TableReseaux: array of TReseauxTopo;
    TableCouleurs: array of TColorGaleries;

    FTable_Pts2D: array of PointsTopo2D;
    FTableQuads: array of TQuad;
    FColors_Legs: array[0..8] of TColor;
    FCube2d: array[1..8] of TPoint;

    // histogramme des profondeurs
    FClassRepartDepthDiagram: array of double;
    // histogramme des directions
    FClassRepart: array of double;
    // statistiques
    FSpeleometrieParDates,
    FSpeleometrieParCouleurs,
    FSpeleometrieParReseaux  : TTableauVentilationSpeleometrie;
    FRecapitulation : TVentilationSpeleometrie;

    FNbDates: integer;
    FNbCouleurs: integer;
    FNbReseaux: integer;


    FRayonCirconscrit: double;
    FCentreCirconscrit: TPoint2Df;
    FEtendue: double;
    // variables de la zone de plan
    FDoDrawWalls, FDoDrawChems, FDoDrawSects, FDoDrawNotes,
    FDoDrawPhotos, FDoDrawPtTopo: boolean;

    FTypePlan: byte;

    FPlanHeight, FPlanWidth: integer;
    FTheta: double;
    FPhi: double;
    FRapp: double;
    FZoom: double;
    FTranslateX, FTranslateY: integer;
    FTX, FTY: double;
    FOrigine: TPoint2Df;

    FCoinBasGauche, FCoinHautDroit: TPoint3Df;
    FIDStationXMini, FIDStationYMini, FIDStationZMini,
    FIDStationXMaxi  , FIDStationYMaxi, FIDStationZMaxi: string;


    FICM: extended;
    FIRS: extended;
    FIC: extended;
  public
    procedure SetProcDisplayProgression(const ProcProgression: TProcDisplayProgression);
    procedure CalculSpeleometrie;
    function GetSpeleometrieParCouleurByIdx(const Idx: integer): TVentilationSpeleometrie;
    function GetSpeleometrieParDateByIdx(const Idx: integer): TVentilationSpeleometrie;
    function GetSpeleometrieParReseauxByIdx(const Idx: integer): TVentilationSpeleometrie;
    function GetCouleurByIdx(const Idx: integer): TColorGaleries;
    function GetDateSeanceByIdx(const Idx: integer): TDatesTopo;
    function GetReseauByIdx(const Idx: integer): TReseauxTopo;

    function GetIDStationXMaxi: string;
    function GetIDStationXMini: string;
    function GetIDStationYMaxi: string;
    function GetIDStationYMini: string;
    function GetIDStationZMaxi: string;
    function GetIDStationZMini: string;
    function GetNbCouleurs: integer;
    function GetNbDates: integer;
    function GetNbReseaux: integer;
    function GetCoinBasGauche: TPoint3Df;
    function GetCoinHautDroit: TPoint3Df;
    function GetNbEntites: integer;
    function ParseDepthHistogramme(const NbPetales: integer; const ViseeMini: double): integer;
    procedure DrawDepthHistogramme(const Cnv: TCanvas; const L, H: integer;
      const ColorPetales, ColorLines: TColor);
    procedure DrawDiagram(const Cnv: TCanvas; const L: integer;
      const ColorPetales: TColor; const ColorLines: TColor);
    function GetAltitudeDiagram(const Cnv: TCanvas;
      const ImgHeight, Y: integer): double;
    function ParseDiagram(const NbPetales: integer;
      const ViseeMini: double): integer;
    // tri selon les numéros de série/station
    procedure SortBySerSts;
    // fonctions de recherche
    function GetEntiteFromXY(const X, Y: double): TEntite;
    function GetEntiteFromCle(const Cle: string): TEntite;
    function GetEntiteFromSerSt(const Ser, St: Integer): TEntite;
    function FindStationByCle(const Cle: string; out E: TEntite): boolean;
    // calcul du volume du plus petit cube englobant la cavité
    function GetVolumeMinimalCirconscrit(const MyFiltre: string): Double;
    // export pour GHCaveDraw
    procedure ExportForGHCaveDraw(const MyFile: string; const QMyFiltre: string);
    // export pour KML, OSM, ...
    {$IFDEF USE_CONVERTISSEUR_EPSG}
    procedure ExportForCarto(const FileName: string;
                             const Convertisseur: TConversionSysteme;
                             const QSystemeGeog: string;
                             const Mode: TOutputFormatGIS;
                             const Silhouette: boolean;
                             const Xo, Yo: double;
                             const Filtres: string;
                             const PrefixStations: string;
                             const CouleurDefaut: TColor;
                             const UseColorGroupes: boolean);
    {$ENDIF}
    {$IFDEF USE_CONVERTISSEUR_ESIBERT}
    procedure ExportForCarto(const FileName: string;
                             const Convertisseur: TConversionSysteme;
                             const QSystemeGeog: string;
                             const Mode: TOutputFormatGIS;
                             const Silhouette: boolean;
                             const Xo, Yo: double;
                             const Filtres: string;
                             const PrefixStations: string;
                             const CouleurDefaut: TColor;
                             const UseColorGroupes: boolean);
    {$ENDIF}
    {$IFDEF USE_CONVERTISSEUR_JPC}
    procedure ExportForCarto(const FileName: string;
                             const Convertisseur: TConversionSysteme;
                             const QSystemeGeog: string;
                             const Mode: TOutputFormatGIS;
                             const Silhouette: boolean;
                             const Xo, Yo: double;
                             const Filtres: string;
                             const PrefixStations: string;
                             const CouleurDefaut: TColor;
                             const UseColorGroupes: boolean);
    {$ENDIF}

    // génération de fichiers TOP métafiltrés
    function GenerateMetaFilteredTOP(const MyFichierTOP, MyFilter: string): integer;


  end;

implementation

const
  INVDEGREES = 180.0 / PI;
// constantes des filtres
const

  kFLT_NIL = 0;            //  NIL               NIL
  kFLT_ALL = 1;            //  ALL               ALL
  kFLT_ID = 2;            //  ID                ID
  kFLT_LONGUEUR = 3;            //  LONGUEUR          LONGUEUR
  kFLT_AZIMUT = 4;            //  AZIMUT            AZIMUT
  kFLT_PENTE = 5;            //  PENTE             PENTE
  kFLT_DATE = 6;            //  DATE              DATE
  kFLT_COULEUR = 7;            //  COULEUR           COULEUR
  kFLT_X = 8;            //  X                 X
  kFLT_Y = 9;            //  Y                 Y
  kFLT_Z = 10;           //  Z                 Z
  kFLT_LARGEUR = 11;           //  LARGEUR           LARGEUR
  kFLT_HAUTEUR = 12;           //  HAUTEUR           HAUTEUR
  kFLT_DATES = 13;           //  DATES             DATES
  kFLT_COULEURS = 14;           //  COULEURS          COULEURS
  kFLT_SERIE = 15;           //  SERIE;
  kFLT_RESEAU = 16;           //  Réseau
  kFLT_CODE = 17;           //  code
  kFLT_EXPE = 18;           //  Séance (expé)
  kFLT_TYPEVISEE = 19;           //  type de visée

{ TSilhouetteGalerie }

procedure TSilhouetteGalerie.ClearSilhouette(const PenColor, FillColor: TColor);
var
  i: integer;
begin
  //AfficherMessage(Format('%s.ClearSilhouette(%X, %X',[ClassName,PenColor, FillColor]));
  FPenColor := PenColor;
  FFillColor:= FillColor;
  SetLength(FArrayPolygone, 0);
  try
    if (self.Count > 0) then
    begin
      for i:= 0 to self.Count - 1 do Dispose(Items[i]);
    end;
  finally
    self.Clear;
  end;
end;


constructor TSilhouetteGalerie.Create;
begin
  inherited;
  //AfficherMessage('TSilhouetteGalerie créé');

end;

destructor TSilhouetteGalerie.Destroy;
begin
  //AfficherMessage('TSilhouetteGalerie détruit');
  inherited;
end;

procedure TSilhouetteGalerie.AddPtsParois(const E: TPtsSectionTransversale);
var
  pE: ^TPtsSectionTransversale;
begin
  New(pE);
  pE^ := E;
  self.Add(pE);
end;

function TSilhouetteGalerie.GetPtsParois(const Idx: integer): TPtsSectionTransversale;
var
  pE: ^TPtsSectionTransversale;
begin
  pE := self.Items[Idx];
  Result := pE^;
end;

function TSilhouetteGalerie.GetFillColor: TColorRGBA;
begin
  Result.R := Red(FFillColor);
  Result.G := Green(FFillColor);
  Result.B := Blue(FFillColor);
  Result.A := $FF;
end;

function TSilhouetteGalerie.GetNbPtsParois: integer;
begin
  Result := self.Count;
end;

function TSilhouetteGalerie.GetPointPolygoneParois(const Idx: integer
  ): TPoint2Df;
begin
  Result := FArrayPolygone[Idx];
end;

function TSilhouetteGalerie.GetNbPointsPolygoneParois: integer;
begin

  //AfficherMessage(Format('--------------- GetNbPointsPolygoneParois: %d',[High(FArrayPolygone)]));
  Result := High(FArrayPolygone) + 1;
end;




function TSilhouetteGalerie.BuildPolygoneParois: boolean;
var
  i, Nb: integer;
  WU: TPtsSectionTransversale;
  EWE : TStringArray;
  TempStrings: TStringList;
begin
  Result := False;
  SetLength(FArrayPolygone, 0);
  // la classique astuce du TStringList ...
  TempStrings := TStringList.Create;
  try
    TempStrings.Clear;
    // paroi droite
    for i:= 0 to self.GetNbPtsParois - 1 do
    begin
     WU := self.GetPtsParois(i);
     TempStrings.Add(Format('%.2f|%.2f', [WU.ParoiDroiteX, WU.ParoiDroiteY]));
    end;
    // paroi gauche = parcours en arrière
    for i:= self.GetNbPtsParois - 1 downto 0 do
    begin
      WU := self.GetPtsParois(i);
      TempStrings.Add(Format('%.2f|%.2f', [WU.ParoiGaucheX, WU.ParoiGaucheY]));
    end;
    // construire le tableau ici
    if (TempStrings.Count > 0) then
    begin
      SetLength(FArrayPolygone, TempStrings.Count);
      for i:= 0 to TempStrings.Count - 1 do
      begin
        EWE := split(TempStrings.Strings[i], '|');
        FArrayPolygone[i].X := StrToFloatDef(EWE[0], 0.00);
        FArrayPolygone[i].Y := StrToFloatDef(EWE[1], 0.00);
        //AfficherMessage(Format('--------------- %.2f - %.2f',[FArrayPolygone[i].X, FArrayPolygone[i].Y]));
      end;
      // résultat OK si le polygone contient au moins quatre points
      Result := (High(FArrayPolygone) > 3);
    end;
    //AfficherMessage(Format('--------- Liste OK (%d sommets)', [High(FArrayPolygone) + 1]));
  finally
    TempStrings.Free;
  end;
end;

//-------------------------------------------------------

// attraper les dates, couleurs et réseaux
function TTableDesEntites.GetDateSeanceByIdx(const Idx: integer): TDatesTopo;
begin
  Result := TableDates[Idx];
end;

function TTableDesEntites.GetReseauByIdx(const Idx: integer): TReseauxTopo;
begin
  Result := TableReseaux[Idx];
end;

function TTableDesEntites.GetCouleurByIdx(const Idx: integer): TColorGaleries;
begin
  Result := TableCouleurs[Idx];
end;


// attraper les spéléométries par index
function TTableDesEntites.GetSpeleometrieParDateByIdx(const Idx: integer): TVentilationSpeleometrie;
begin
  Result := FSpeleometrieParDates[Idx];
end;
function TTableDesEntites.GetSpeleometrieParReseauxByIdx(const Idx: integer): TVentilationSpeleometrie;
begin
  Result := FSpeleometrieParReseaux[Idx];
end;
function TTableDesEntites.GetSpeleometrieParCouleurByIdx(const Idx: integer): TVentilationSpeleometrie;
begin
  Result := FSpeleometrieParCouleurs[Idx];
end;



// ID des stations extremes
function TTableDesEntites.GetIDStationXMini: string;
begin
  Result := FIDStationXMini;
end;

function TTableDesEntites.GetIDStationYMini: string;
begin
  Result := FIDStationYMini;
end;
function TTableDesEntites.GetIDStationZMini: string;
begin
  Result := FIDStationZMini;
end;

function TTableDesEntites.GetIDStationXMaxi: string;
begin
  Result := FIDStationXMaxi;
end;
function TTableDesEntites.GetIDStationYMaxi: string;
begin
  Result := FIDStationYMaxi;
end;
function TTableDesEntites.GetIDStationZMaxi: string;
begin
  Result := FIDStationZMaxi;
end;

// obtention du nombre de réseaux
function TTableDesEntites.GetNbReseaux: integer;
begin
  Result := FNbReseaux;
end;
// obtention du nombre de dates
function TTableDesEntites.GetNbDates: integer;
begin
  Result := FNbDates;
end;
// obtention du nombre de couleurs
function TTableDesEntites.GetNbCouleurs: integer;
begin
  Result := FNbCouleurs;
end;



// obtention points extrêmes
function TTableDesEntites.GetCoinBasGauche: TPoint3Df;
begin
  Result := FCoinBasGauche;
end;

function TTableDesEntites.GetCoinHautDroit: TPoint3Df;
begin
  Result := FCoinHautDroit;
end;

// Obtention d'informations
function TTableDesEntites.GetEntiteFromXY(const X, Y: double): TEntite;
var
  i: integer;
  E: TEntite;
  d, dMax: double;
begin
  dMax := 1.00e10;
  for i := 1 to High(TableEntites) do
  begin
    E := TableEntites[i];
    d := Hypot2D(E.Une_Station_2_X - X, E.Une_Station_2_Y - Y);
    if (d < dMax) then
    begin
      dMax := d;
      Result := E;
    end;
  end;

end;

//*****************************************************************************
// Recensement couleurs, dates, réseaux
//*********************************
// recenser les réseaux
function TTableDesEntites.RecenserReseaux: integer;
var
  i: integer;
begin
  Result := 0;
  AfficherMessage(format('%s.RecenserReseaux', [ClassName]));

  Result := -1;
  FNbReseaux := 0;

  with TStringList.Create do
  begin
    try
      try
        Clear;
        Sorted := True;
        Duplicates := dupIgnore;
        for i := LOW_INDEX to FNbEntites - 1 do
        begin
          //TableEntites[i].UID_Entite;
          Add(Format('$%X', [TableEntites[i].ColorReseau]));
        end;
        // assigner réseaux
        SetLength(TableReseaux, 0);
        SetLength(TableReseaux, Count);
        for i := 0 to Count - 1 do
        begin
          TableReseaux[i].Couleur := StrToIntDef(Strings[i], 0);
          //AfficherMessage(Format('   Reseau %d = %X', [i, TableReseaux[i].Couleur]));
        end;
        FNbReseaux := Count;
        Result := Count;
      except
      end;
    finally
      Free;
    end;
  end;
end;

function TTableDesEntites.RecenserDates: integer;
var
  i: integer;
  Year, Month, Day: word;
begin
  AfficherMessage(Format('%s.RecenserDates ()', [ClassName]));
  FNbDates := 0;
  Result := -1;
  with TStringList.Create do
  begin
    try
      try
        Sorted := True;
        Duplicates := dupIgnore;
        Clear;
        for i := LOW_INDEX to FNbEntites - 1 do
        begin
          SafeDecodeDate(TableEntites[i].DateLeve, Year, Month, Day);
          Add(Format('%.4d/%.2d/%.2d', [Year, Month, Day]));
        end;
        // remplir le tableau des dates
        SetLength(TableDates, 0);
        SetLength(TableDates, Count);
        for i := 0 to Count - 1 do
        begin
          Year  := StrToIntDef(Copy(Strings[i], 1, 4), 2000);
          Month := StrToIntDef(Copy(Strings[i], 6, 2), 1);
          Day   := StrToIntDef(Copy(Strings[i], 9, 2), 1);
          TableDates[i].DateTopo := SafeEncodeDate(Year, Month, Day);
          //AfficherMessage(Format('   Date %d = %s', [i, DateToStr(TableDates[i].DateTopo)]));
        end;
        FNbDates := Count;
        Result := Count;
      except
      end;
    finally
      Free;
    end;
  end;
  AfficherMessage(Format('%s.RecenserDates () OK', [ClassName]));
end;

function TTableDesEntites.RecenserCouleurs: integer;
var
  i: integer;
  C: TColor;
begin
  AfficherMessage(Format('%s.RecenserCouleurs', [ClassName]));
  Result := -1;
  FNbCouleurs := 0;
  with TStringList.Create do
  begin
    try
      try
        Clear;
        Sorted := True;
        Duplicates := dupIgnore;
        for i := LOW_INDEX to FNbEntites - 1 do
          Add(Format('$%X', [TableEntites[i].ColorEntite]));
        // assigner couleurs
        SetLength(TableCouleurs, 0);
        SetLength(TableCouleurs, Count);
        for i := 0 to Count - 1 do
        begin
          TableCouleurs[i].Color := StrToIntDef(Strings[i], 0);
          // AfficherMessage(Format('   Couleur %d = %X (%s)', [i, TableCouleurs[i].Color, Strings[i]]));
        end;
        FNbCouleurs := Count;
        Result := Count;
      except
      end;
    finally
      Free;
    end;
  end;
end;
//******************************************************************************
function TTableDesEntites.GetFileName: string;
begin
  Result := FFichierTOP;
end;

function TTableDesEntites.GetEntite(const Idx: integer): TEntite;
begin
  if (Idx < 0) or (Idx > FNbEntites) then
    Exit;
  Result := self.TableEntites[Idx];
end;

function TTableDesEntites.LoadEntites(const MyFichierTOP: string;
  const SortedByDepth: boolean): integer;
var
  i, j: integer;
  pTOP: file of TEntite;
  Max: TDateTime;
  s: string;

  procedure QSortCotes(var Table_Entites: array of TEntite; lidx: integer; ridx: integer);
  var
    k, e, mid: integer;
    Buffer: TEntite;
    bidon1, bidon2: double;
  begin
    if lidx >= ridx then
      Exit;
    mid := (lidx + ridx) div 2;
    Buffer := Table_Entites[lidx];
    Table_Entites[lidx] := Table_Entites[mid];
    Table_Entites[mid] := Buffer;
    e := lidx;
    for k := lidx + 1 to ridx do
    begin
      bidon1 := 0.50 * (Table_Entites[k].Une_Station_1_Z +
        Table_Entites[k].Une_Station_2_Z);
      bidon2 := 0.50 * (Table_Entites[lidx].Une_Station_1_Z +
        Table_Entites[lidx].Une_Station_2_Z);
      if bidon1 < bidon2 then
      begin
        Inc(e);
        Buffer := Table_Entites[e];
        Table_Entites[e] := Table_Entites[k];
        Table_Entites[k] := Buffer;
      end;
    end;
    Buffer := Table_Entites[lidx];
    Table_Entites[lidx] := Table_Entites[e];
    Table_Entites[e] := Buffer;
    QSortCotes(Table_Entites, lidx, e - 1);
    QSortCotes(Table_Entites, e + 1, ridx);
  end; // QSortCotes

begin
  FFichierTOP := MyFichierTOP;
  Result := -1;
  with self do
  begin
    if not (FileExists(FFichierTOP)) then
    begin
      AfficherMessage(Format(ClassName + '.LoadEntites: %s not found', [FFichierTOP]));
      Result := -2;
      Exit;
    end;
    AssignFile(pTOP, FFichierTOP);
    try
      try
        Reset(pTOP);
        FNbEntites := FileSize(pTOP);

        // table des entités
        SetLength(TableEntites, 0);
        SetLength(TableEntites, FNbEntites);
        // coordonnées transformées
        SetLength(FTable_Pts2D, 0);
        SetLength(FTable_Pts2D, FNbEntites);
        // facettes
        SetLength(FTableQuads, 0);
        SetLength(FTableQuads, FNbEntites);
        for i := LOW_INDEX to FNbEntites - 1 do
        begin
          Seek(pTOP, i - 1);
          Read(pTOP, TableEntites[i]);
        end;
        TableEntites[0].Type_Entite := tgFOSSILE;
        AfficherMessage(Format('%d entities in %s', [FNbEntites, FFichierTOP]));
        // tri de profondeur
        if SortedByDepth then QSortCotes(TableEntites, 1, FNbEntites - 1);
        SetMinMax;            // définision des mini et maxi
        RecenserReseaux;      // réseaux
        RecenserCouleurs;     // couleurs
        RecenserDates;        // dates
        MetaFiltre('');       // métafiltre
        // passage par ici=succès
        Result := FNbEntites;
      except
        Result := -1;
        Exit;
      end;
    finally
      CloseFile(pTOP);
    end;
  end;
end;

procedure TTableDesEntites.Finalise;
begin
  SetLength(FTableQuads, 0);
  SetLength(FTable_Pts2D, 0);
  SetLength(TableEntites, 0);
  SetLength(TableDates, 0);
  SetLength(TableCouleurs, 0);
  SetLength(TableReseaux, 0);
end;

function TTableDesEntites.GetNbEntites: integer;
begin
  Result := FNbEntites;
end;

// définir mini et maxi
// DONE: Membres XMini, XMaxi, YMini, YMaxi,ZMini, ZMaxi : double; localisés ici:
// font double emploi avec les membres privés FCoinBasGauche et FCoinHautDroit
procedure TTableDesEntites.SetMinMax;
const
  GM = 1e20;
  FMTMINMAX = '  %s: %12.2f %12.2f %9.2f';
var
  i: integer;
  XMini, XMaxi, YMini, YMaxi, ZMini, ZMaxi: double;

begin
  AfficherMessage(ClassName + '.SetMinMax');

  with self do
  begin
    XMini :=  GM;
    YMini :=  GM;
    ZMini :=  GM;
    XMaxi := -GM;
    YMaxi := -GM;
    ZMaxi := -GM;
    for i := LOW_INDEX to FNbEntites - 1 do
    begin

      if (TableEntites[i].Une_Station_1_X > XMaxi) then
      begin
        XMaxi := TableEntites[i].Une_Station_1_X;
        FIDStationXMaxi := TableEntites[i].ID_Litteral_Pt;
      end;
      if (TableEntites[i].Une_Station_1_X < XMini) then
      begin
        XMini := TableEntites[i].Une_Station_1_X;
        FIDStationXMini := TableEntites[i].ID_Litteral_Pt;
      end;

      if (TableEntites[i].Une_Station_1_Y > YMaxi) then
      begin
        YMaxi := TableEntites[i].Une_Station_1_Y;
        FIDStationYMaxi := TableEntites[i].ID_Litteral_Pt;
      end;
      if (TableEntites[i].Une_Station_1_Y < YMini) then
      begin
        YMini := TableEntites[i].Une_Station_1_Y;
        FIDStationYMini := TableEntites[i].ID_Litteral_Pt;
      end;

      if (TableEntites[i].Une_Station_1_Z > ZMaxi) then
      begin
        ZMaxi := TableEntites[i].Une_Station_1_Z;
        FIDStationZMaxi := TableEntites[i].ID_Litteral_Pt;
      end;
      if (TableEntites[i].Une_Station_1_Z < ZMini) then
      begin
        ZMini := TableEntites[i].Une_Station_1_Z;
        FIDStationZMini := TableEntites[i].ID_Litteral_Pt;
      end;

      if (TableEntites[i].Une_Station_2_X > XMaxi) then
      begin
        XMaxi := TableEntites[i].Une_Station_2_X;
        FIDStationXMaxi := TableEntites[i].ID_Litteral_Pt;
      end;
      if (TableEntites[i].Une_Station_2_X < XMini) then
      begin
        XMini := TableEntites[i].Une_Station_2_X;
        FIDStationXMini := TableEntites[i].ID_Litteral_Pt;
      end;
      if (TableEntites[i].Une_Station_2_Y > YMaxi) then
      begin
        YMaxi := TableEntites[i].Une_Station_2_Y;
        FIDStationYMaxi := TableEntites[i].ID_Litteral_Pt;
      end;
      if (TableEntites[i].Une_Station_2_Y < YMini) then
      begin
        YMini := TableEntites[i].Une_Station_2_Y;
        FIDStationYMini := TableEntites[i].ID_Litteral_Pt;
      end;
      if (TableEntites[i].Une_Station_2_Z > ZMaxi) then
      begin
        ZMaxi := TableEntites[i].Une_Station_2_Z;
        FIDStationZMaxi := TableEntites[i].ID_Litteral_Pt;
      end;
      if (TableEntites[i].Une_Station_2_Z < ZMini) then
      begin
        ZMini := TableEntites[i].Une_Station_2_Z;
        FIDStationZMini := TableEntites[i].ID_Litteral_Pt;
      end;
    end;
    // affectation des membres privés suivants
    FCoinBasGauche.X := XMini;
    FCoinBasGauche.Y := YMini;
    FCoinBasGauche.Z := ZMini;

    FCoinHautDroit.X := XMaxi;
    FCoinHautDroit.Y := YMaxi;
    FCoinHautDroit.Z := ZMaxi;
    AfficherMessage(Format(' Mini: %.0f, %.0f, %.0f',
      [FCoinBasGauche.X, FCoinBasGauche.Y, FCoinBasGauche.Z]));
    AfficherMessage(Format(' Maxi: %.0f, %.0f, %.0f',
      [FCoinHautDroit.X, FCoinHautDroit.Y, FCoinHautDroit.Z]));

  end;
end;

//******************************************************************************
// MétaFiltre
// NE PAS MODIFIER
function TTableDesEntites.MetaFiltre(const Filtre: string): integer;
var
  NoFiltre: integer;
  i, m, n: integer;
  NbViseesRetenues: integer;
  LV: double;
  DevelVisees: double;
  f1, st1, st2: string;
  ListeFiltres: array of TFiltre;
  // Détermine le type de filtre:
  //    -1 = Filtre erronné
  //     0 = Valeur simple
  //     1 = Intervalle
  function ChooseFilter(const s90: string): integer;
  begin
    Result := IndexOfString(s90, False, [rsMETAFILTRE_NIL,         // 0
      rsMETAFILTRE_ALL,         // 1
      rsMETAFILTRE_ID,          // 2
      rsMETAFILTRE_LENGTH,      // 3
      rsMETAFILTRE_AZIMUTH,     // 4
      rsMETAFILTRE_PENTE,       // 5
      rsMETAFILTRE_DATE,        // 6
      rsMETAFILTRE_COLOR,       // 7
      rsMETAFILTRE_X,           // 8
      rsMETAFILTRE_Y,           // 9
      rsMETAFILTRE_Z,           // 10
      rsMETAFILTRE_LARGEUR,     // 11
      rsMETAFILTRE_HAUTEUR,     // 12
      rsMETAFILTRE_DATES,       // 13
      rsMETAFILTRE_COLORS,      // 14
      rsMETAFILTRE_SERIE,        // 15
      rsMETAFILTRE_RESEAU,       // 16
      rsMETAFILTRE_CODE,         // 17
      rsMETAFILTRE_EXPE,         // 18
      rsMETAFILTRE_TYPEVISEE]);
  end;

  function DoDrawVisee(const F: TFiltre; const E: TEntite): boolean;
  var
    Long: double;
    s: string;
    d: TDateTime;
    p, q, j: integer;
    h, v: double;
    Ai, Bi: integer; // bornes entières
    Af, Bf: double;  // bornes réelles
    Ad, Bd: TDateTime; // bornes date
    Q666: TTypeDeVisee;

  begin
    Result := False;
    // -------
    case F.TypeFiltre of
      ftINVALID:
      begin
        Result := False;
      end; // ftINVALID
      ftSCALAR:
      begin
        case F.Filter of
          -1: Result := False;
          kFLT_NIL:
            Result := False;
          kFLT_ALL:
            Result := True;
          kFLT_ID:
            Result := (Trim(F.Valeur) = Trim(E.ID_Litteral_Pt));
          kFLT_LONGUEUR:
          begin // longueur des visées
            Long := Hypot3D(E.Une_Station_2_X - E.Une_Station_1_X,
              E.Une_Station_2_Y - E.Une_Station_1_Y,
              E.Une_Station_2_Z - E.Une_Station_1_Z);
            case F.Operateur of
              1: Result := (Long <= StrToFloat(F.Valeur));
              3: Result := (Long >= StrToFloat(F.Valeur));
              else
                Result := False;
            end;
          end;
          kFLT_DATE:
          begin // filtres sur dates
            d := StrToDate(F.Valeur);
            case F.Operateur of
              1: Result := (E.DateLeve <= d);
              2: Result := (E.DateLeve = d);
              3: Result := (E.DateLeve >= d);
              else
                Result := False;
            end;
          end;
          kFLT_COULEUR:
          begin
            p := StrToIntDef(F.Valeur, 0);
            case F.Operateur of
              1: Result := (E.ColorEntite <= p);
              2: Result := (E.ColorEntite = p);
              3: Result := (E.ColorEntite >= p);
              else
                Result := False;
            end;
          end;
          kFLT_X:
          begin
            d := StrToFloatDef(F.Valeur, 0.00);
            case F.Operateur of
              1: Result :=
                  ((E.Une_Station_1_X <= d) or (E.Une_Station_2_X <= d));
              2: Result :=
                  ((E.Une_Station_1_X = d) or (E.Une_Station_2_X = d));
              3: Result :=
                  ((E.Une_Station_1_X >= d) or (E.Une_Station_2_X >= d));
              else
                Result := False;
            end;
          end;
          kFLT_Y:
          begin
            d := StrToFloatDef(F.Valeur, 0.00);
            case F.Operateur of
              1: Result :=
                  ((E.Une_Station_1_Y <= d) or (E.Une_Station_2_Y <= d));
              2: Result :=
                  ((E.Une_Station_1_Y = d) or (E.Une_Station_2_Y = d));
              3: Result :=
                  ((E.Une_Station_1_Y >= d) or (E.Une_Station_2_Y >= d));
              else
                Result := False;
            end;
          end;
          kFLT_Z:
          begin // filtres sur profondeur
            d := StrToFloatDef(F.Valeur, 0.00);
            case F.Operateur of
              1: Result :=
                  ((E.Une_Station_1_Z <= d) or (E.Une_Station_2_Z <= d));
              2: Result :=
                  ((E.Une_Station_1_Z = d) or (E.Une_Station_2_Z = d));
              3: Result :=
                  ((E.Une_Station_1_Z >= d) or (E.Une_Station_2_Z >= d));
              else
                Result := False;
            end;
          end;
          kFLT_LARGEUR: // filtres sur largeur
          begin
            d := StrToFloatDef(F.Valeur, 0.00);
            H := Hypot2D(E.X1PG - E.X1PD, E.Y1PG - E.Y1PD);
            V := Hypot2D(E.X2PG - E.X2PD, E.Y2PG - E.Y2PD);
            case F.Operateur of
              1: Result := ((V <= d) or (H <= d));
              2: Result := ((H = d) or (V = d));
              3: Result := ((H >= d) or (V >= d));
              else
                Result := False;
            end;
          end;
          kFLT_HAUTEUR: // filtres sur hauteurs
            // TODO: Implanter ce filtre HAUTEURS
          begin
            d := StrToFloatDef(F.Valeur, 0.00);
            H := E.Z1PH - E.Z1PB;
            V := E.Z2PH - E.Z2PB;
            case F.Operateur of
              1: Result := ((V <= d) or (H <= d));
              2: Result := ((H = d) or (V = d));
              3: Result := ((H >= d) or (V >= d));
              else
                Result := False;
            end;
          end;
          kFLT_SERIE: // filtre sur séries
          begin
            p := StrToIntDef(F.Valeur, 0);
            case F.Operateur of
              1: Result := (E.Entite_Serie <= p);
              2: Result := (E.Entite_Serie = p);
              3: Result := (E.Entite_Serie >= p);
              else
                Result := False;
            end;
          end;
          kFLT_RESEAU: // filtre sur réseaux
          begin
            p := StrToIntDef(F.Valeur, 0);
            case F.Operateur of
              1: Result := (E.IdxReseau <= p);
              2: Result := (E.IdxReseau = p);
              3: Result := (E.IdxReseau >= p);
              else
                Result := False;
            end;
          end;
          kFLT_CODE: // filtre sur codes
          begin
            p := StrToIntDef(F.Valeur, 0);
            case F.Operateur of
              1: Result := (E.eCode <= p);
              2: Result := (E.eCode = p);
              3: Result := (E.eCode >= p);
              else
                Result := False;
            end;
          end;
          kFLT_EXPE: // filtre sur expés
          begin
            p := StrToIntDef(F.Valeur, 0);
            case F.Operateur of
              1: Result := (E.eExpe <= p);
              2: Result := (E.eExpe = p);
              3: Result := (E.eExpe >= p);
              else
                Result := False;
            end;
          end;
          kFLT_TYPEVISEE: // filtre sur type de visée
          begin
            p := StrToIntDef(F.Valeur, 0);
            Q666 := GetTypeDeVisee(p);
            Result := (E.Type_Entite = Q666);
          end;
          else
            Result := False;
        end; // case Filter ..
        //-----------------
      end; // ftScalar
      ftINTERVAL:
      begin
        case F.Filter of
          -1: Result := False;
          kFLT_NIL: ;
          kFLT_AZIMUT:
          begin // intervalle sur les azimuts
            Af := StrToFloatDef(F.BorneInf, 0.00);
            Bf := StrToFloatDef(F.BorneSup, 0.00);
            V := GetAzimut(E.Une_Station_2_X - E.Une_Station_1_X,
              E.Une_Station_2_Y - E.Une_Station_1_Y,
              360.00);
            Result := ((V > Af) and (V < Bf)) or
              (((V + 180) > Af) and ((V + 180) < Bf));
          end;
          kFLT_PENTE:
          begin // intervalle sur les inclinaisons
            Af := StrToFloatDef(F.BorneInf, 0.00);
            Bf := StrToFloatDef(F.BorneSup, 0.00);
            Long := 1e-12 + Hypot3D(
              E.Une_Station_2_X - E.Une_Station_1_X,
              E.Une_Station_2_Y - E.Une_Station_1_Y,
              E.Une_Station_2_Z - E.Une_Station_1_Z);
            H := E.Une_Station_2_Z - E.Une_Station_1_Z;
            V := ArcSin(H / Long) * INVDEGREES;
            Result := ((V > Af) and (V < Bf));
          end;
          kFLT_DATE:
          begin // intervalle sur dates
            try
              Ad := StrToDate(F.BorneInf);
              Bd := StrToDate(F.BorneSup);
              Result := ((E.DateLeve > Ad) and (E.DateLeve < Bd));
            except
              Result := False;
            end;
          end;
          kFLT_COULEUR: ; // couleurs
          kFLT_X: // X
          begin // intervalle sur Z
            Af := StrToFloatDef(F.BorneInf, 0.00);
            Bf := StrToFloatDef(F.BorneSup, 0.00);
            V := E.Une_Station_1_X;
            Result := ((V > Af) and (V < Bf));
            V := E.Une_Station_2_X;
            Result := Result or ((V > Af) and (V < Bf));
          end;

          kFLT_Y:   //Y
          begin // intervalle sur Z
            Af := StrToFloatDef(F.BorneInf, 0.00);
            Bf := StrToFloatDef(F.BorneSup, 0.00);
            V := E.Une_Station_1_Y;
            Result := ((V > Af) and (V < Bf));
            V := E.Une_Station_2_Y;
            Result := Result or ((V > Af) and (V < Bf));
          end;
          kFLT_Z:
          begin // intervalle sur Z
            Af := StrToFloatDef(F.BorneInf, 0.00);
            Bf := StrToFloatDef(F.BorneSup, 0.00);
            V := 0.50 * (E.Une_Station_1_Z + E.Une_Station_2_Z);
            Result := ((V > Af) and (V < Bf));
          end;
          kFLT_SERIE: // filtre sur séries
          begin
            Ai := StrToIntDef(F.BorneInf, 0);
            Bi := StrToIntDef(F.BorneSup, 0);
            Result := ((E.Entite_Serie >= Ai) and (E.Entite_Serie <= Bi));
          end;
          kFLT_RESEAU: // filtres sur réseaux
          begin
            Ai := StrToIntDef(F.BorneInf, 0);
            Bi := StrToIntDef(F.BorneSup, 0);

            Result := ((E.IdxReseau >= Ai) and (E.IdxReseau <= Bi));
          end;
          kFLT_CODE: // filtres sur codes
          begin
            Ai := StrToIntDef(F.BorneInf, 0);
            Bi := StrToIntDef(F.BorneSup, 0);

            Result := ((E.eCode >= Ai) and (E.eCode <= Bi));
          end;
          kFLT_EXPE: // filtres sur expés
          begin
            Ai := StrToIntDef(F.BorneInf, 0);
            Bi := StrToIntDef(F.BorneSup, 0);

            Result := ((E.eExpe >= Ai) and (E.eExpe <= Bi));
          end;
          else
            Result := False;
        end; // case F.Filter
      end; // ftINTERVAL
      ftFLAGS:
      begin
        case F.Filter of
          -1: Result := False;
          kFLT_NIL:
            ;
          kFLT_DATES:
          begin // dates
            Result := False;
            for j := 0 to FNbDates - 1 do
            begin
              //if (F.Flags[j+1]='1') and (E.DateLeve=TableDates[j].DateTopo) then
              Result := Result or ((F.Flags[j + 1] = '1') and
                (E.DateLeve = TableDates[j].DateTopo));
            end;
          end;
          kFLT_COULEURS:
          begin // couleurs
            Result := False;
            for j := 0 to FNbCouleurs - 1 do
            begin
              Result := Result or ((F.Flags[j + 1] = '1') and
                (E.ColorEntite = TableCouleurs[j].Color));
            end;
          end;
          else
            Result := False;
        end;
      end; // ftFLAGS
    end;   // case TypeFiltre
  end;

  function SetFiltre(const F44: string): TFiltre;
  var
    P: integer;
    Q: integer;
    F4: string;
    s1, s2: string;

    function GetConnector(var s: string): TConnectorFiltres;
      // Retourne le connecteur avec le filtre précédent
      // et supprime le premier caractère du filtre
    begin

      if Length(s) = 0 then
      begin
        Result := ftERROR;
        Exit;
      end;
      case S[1] of
        'U':
        begin  // (U)nion
          Result := ftOR;
          Delete(S, 1, 1);
        end;
        'N':
        begin
          Result := ftAND;
          Delete(S, 1, 1);
        end;   // I(N)tersection
        else
          Result := ftERROR;
      end;
    end;

  begin
    F4 := F44;
    Result.Caption := F4;
    try
      (* rechercher le connecteur avec le filtre précédent *)
      (* si aucun connecteur trouvé, c'est le début de la liste des filtres *)
      case GetConnector(F4) of
        ftOR: Result.ConnectedWithPrevious := ftOR;
        ftAND: Result.ConnectedWithPrevious := ftAND;
        ftERROR: Result.ConnectedWithPrevious := ftOR;
      end;
      (* rechercher un point d'exclamation              *)
      (* et mettre le flag d'inversion à True si trouvé *)
      (* Le résultat du MétaFiltre sera basculé si True *)
      Q := Pos('!', F4);
      Result.Basculed := (q > 0);
      if Result.Basculed then
      begin
        Delete(F4, q, 1);
      end;
      if Pos('ALL', F4) > 0 then
      begin
        Result.Filter := 1;
        Result.Operateur := 0;
        Result.Valeur := '';
        Exit;
      end;
      if Pos('NIL', F4) > 0 then
      begin
        Result.Filter := 0;
        Result.Operateur := 0;
        Result.Valeur := '';
        Exit;
      end;
      (* rechercher un ensemble de drapeaux
        Format: <Filtre>=[xxxxxxxxxxxxx] avec x = 0 ou 1
      ******************)
      Q := Pos('[', F4);
      if (Q > 0) then
      begin
        // définir le type de filtre: Drapeaux
        // rechercher le type de filtre
        P := Pos('=', F4);
        s1 := Trim(UpperCase(Copy(F4, 1, P - 1)));
        Result.Filter := ChooseFilter(s1);
        //AfficherMessage(Format('Type de filtre sur drapeaux: "%s" %d',[s1, Result.Filter]));
        // attraper la valeur du champ Drapeaux
        P := Pos(']', F4);
        Result.Flags := Copy(F4, Q + 1, P - Q - 1);
        // protection: Compléter par des zéros
        Result.Flags := Result.Flags + StringOfChar('0', 300);
        //AfficherMessage(Format('Flags: %s',[Result.Flags]));

        //AfficherMessage(Format('Types filtres: %s',[IIF(Result.TypeFiltre=ftFLAGS, 'Drapeaux', 'Autres')]));
        Result.TypeFiltre := ftFLAGS;
        Exit;
      end;
      (* rechercher un spécificateur d'intervalle
      Formats acceptés:
      <Filtre>=(A, B);
      ******************)
      //Q:=  Pos('[', F4);
      Q := Pos('(', F4);
      //Q:=Q+Pos('..', F4);
      if (Q > 0) then
      begin
        //rechercher la donnée à filtrer
        P := Pos('=', F4);
        s1 := Trim(UpperCase(Copy(F4, 1, P - 1)));
        Result.Filter := ChooseFilter(S1);
        // Extraction de la première valeur
        P := Pos(',', F4);
        if P = 0 then
        begin
          Result.Filter := -1;
          Result.Operateur := 0;
          Result.Valeur := '';
          Exit;
        end;
        Result.BorneInf := Copy(F4, Q + 1, P - Q - 1);
        //AfficherMessage('Inférieur: '+Result.BorneInf);
        // Extraction de la deuxième valeur
        Q := P; // sauvegarder position de la virgule
        P := Pos(')', F4); // recherche de la parenthèse fermante
        if P = 0 then
        begin
          Result.Filter := -1;
          Result.Operateur := 0;
          Result.Valeur := '';
          Exit;
        end;
        Result.BorneSup := Copy(F4, Q + 1, P - Q - 1);
        // définir le type de filtre comme intervalle et quitter
        Result.TypeFiltre := ftINTERVAL;
        Exit;
      end;
      // rechercher un signe égal
      P := Pos('=', F4);
      P := P + Pos('<', F4);
      P := P + Pos('>', F4);
      // pas de signe égal -> filtre erroné
      if P = 0 then
      begin
        Result.Filter := -1;
        Result.Operateur := 0;
        Result.Valeur := '';
        Exit;
      end;
      s1 := Trim(Copy(F4, 1, P - 1));
      Result.Filter := ChooseFilter(s1);
      //AfficherMessage(Format('Filter=%d',[Result.Filter]));
      Result.Operateur := IndexOfString(F4[P], False, [' ', '<', '=', '>']);
      s2 := Trim(copy(F4, P + 1, Length(F4)));
      Result.Valeur := s2;
      Result.TypeFiltre := ftSCALAR;
    except
      Result.TypeFiltre := ftINVALID;
      Result.Filter := -1;
      Result.Operateur := 0;
      Result.Valeur := '';
      Exit;
    end;
  end;

  procedure RecenserFiltres;
  var
    l: integer;
    S1, S2, s3: string;
    P: integer;

    function FoundSeparator(const S: string): integer;
    var
      q: integer;
    begin
      Result := 0;
      if Length(S) = 0 then
        Exit;
      for q := 1 to Length(S) do
      begin
        if (S[q] in [';', '&', '|', '+', '*']) then
        begin
          Result := q;
          Exit;
        end;
      end;
    end;

  begin
    //AfficherMessage(Format('  RecenserFiltres %s',[f1]));
    with TStringList.Create do
    begin
      try
        s2 := 'N' + f1; // connecteur de début
        Duplicates := dupIgnore;
        Clear;
        repeat
          P := FoundSeparator(s2);
          s1 := trim(Copy(s2, 1, P - 1));
          //s2:=Trim(Copy(s2,P+1,Length(s2)));
          s2 := Trim(Copy(s2, P, Length(s2)));
          if Length(s2) > 0 then
          begin
            case s2[1] of
              '&', '*'      : s2[1] := 'N'; // intersection (AND) (produit)
              '|', ';', '+' : s2[1] := 'U'; // union (OR)         (somme)
            else
            end;
          end;
          if Length(S1) > 0 then
            Add(s1);
        until P = 0;
        if Length(S2) > 0 then Add(s2);

        //if (Length(Copy(S2, 2,Length(S2)))>0) then Add(s2);
        // controle et définition des filtres
        SetLength(ListeFiltres, 0);
        SetLength(ListeFiltres, Count);
        for l := 0 to Count - 1 do ListeFiltres[l] := SetFiltre(Strings[l]);
      finally
        Free;
      end;
    end;
  end;

  procedure SelectAll(const WU: boolean);
  var
    v: integer;
  begin
    for v := Low(TableEntites) to High(TableEntites) do
      TableEntites[v].Drawn := WU;
  end;
begin
  f1 := Trim(UpperCase(Filtre));
  AfficherMessage(Format('%s.MetaFiltre("%s")', [ClassName, Filtre]));
  Result := -1;
  if Trim(Filtre) = '' then
  begin
    SelectAll(True);
    Result := 1;
    Exit;
  end;
  if f1 = 'NIL' then
  begin
    SelectAll(False);
    Result := 1;
    Exit;
  end;
  RecenserFiltres;
  SelectAll(False);
  // application des filtres
  NbViseesRetenues := 0;
  DevelVisees := 0.00;
  for i := Low(TableEntites) to High(TableEntites) do
  begin
    TableEntites[i].Drawn := True;
    //for
    for NoFiltre := 0 to High(ListeFiltres) do
    begin
      case ListeFiltres[NoFiltre].ConnectedWithPrevious of
        ftERROR, ftOR: TableEntites[i].Drawn :=
            TableEntites[i].Drawn or
            DoDrawVisee(ListeFiltres[NoFiltre],
            TableEntites[i]);
        ftAND: TableEntites[i].Drawn :=
            TableEntites[i].Drawn and
            DoDrawVisee(ListeFiltres[NoFiltre],
            TableEntites[i]);
      end;
      if ListeFiltres[NoFiltre].Basculed then
        TableEntites[i].Drawn := not (TableEntites[i].Drawn);
    end;
    with TableEntites[i] do
      if Drawn then
      begin
        Inc(NbViseesRetenues);
        LV := Hypot3D(Une_Station_2_X - Une_Station_1_X,
          Une_Station_2_Y - Une_Station_1_Y,
          Une_Station_2_Z - Une_Station_1_Z);

        DevelVisees := DevelVisees + LV;
      end;
  end;
  AfficherMessage(Format('%s.MetaFiltre("%s"): %d items, %.2f m',
    [ClassName, Filtre, NbViseesRetenues, DevelVisees]));
  SetLength(ListeFiltres, 0);
end;

procedure TTableDesEntites.SetProcDisplayProgression(
  const ProcProgression: TProcDisplayProgression);
begin
  FProcDispProgression := ProcProgression;
end;

function TTableDesEntites.GetAltitudeDiagram(const Cnv: TCanvas;
  const ImgHeight, Y: integer): double;
const
  MG = 60;
  MD = 10;
  MH = 5;
  MB = 5;
var
  dy: double;
  ratio: double;
begin
  dy := FCoinHautDroit.Z - FCoinBasGauche.Z;
  ratio := -dy / (ImgHeight - MH - MB);
  Result := FCoinHautDroit.Z + (Y - MH) * Ratio;
end;

procedure TTableDesEntites.DrawDepthHistogramme(const Cnv: TCanvas;
  const L, H: integer;
  const ColorPetales, ColorLines: TColor);
const
  MG = 60;
  MD = 10;
  MH = 5;
  MB = 5;
var
  i: integer;
  HBarre: integer;
  LMax: double;
  DoubleBuffer: TBitmap;
  SourceRect, DestRect: TRect;
  LargMaxBarres: integer; // longueur max des barres
  FTabloBarres: array of integer;
begin
  AfficherMessage(Format('%s.DrawDepthHistogramme: L=%d, H=%d', [ClassName, L, H]));
  // longueurs des barres
  SetLength(FTabloBarres, 0);
  SetLength(FTabloBarres, 1 + High(FClassRepartDepthDiagram));
  LMax := -1.00;
  for i := 0 to High(FTabloBarres) do
    if (FClassRepartDepthDiagram[i] > LMax) then
      LMax := FClassRepartDepthDiagram[i];
  LargMaxBarres := L - MG - MD;
  for i := 0 to High(FTabloBarres) do
    FTabloBarres[i] := round(LargMaxBarres * FClassRepartDepthDiagram[i] / LMax);
  HBarre := round((H - MH - MB) / (1 + High(FClassRepartDepthDiagram)));
  try
    DoubleBuffer := TBitmap.Create;
    DoubleBuffer.Height := H;
    DoubleBuffer.Width := L;
    SourceRect.Left := 0;
    SourceRect.Top := 0;
    SourceRect.Right := L;
    SourceRect.Bottom := H;
    // dessin du diagramme
    with DoubleBuffer.Canvas do
    begin
      Brush.Color := clWhite;
      FillRect(SourceRect);
      // labels
      TextOut(10, MH + HBarre, Format('%.0f m', [FCoinHautDroit.Z]));
      TextOut(10, MH + (High(FTabloBarres) - 1) * HBarre,
        Format('%.0f m', [FCoinBasGauche.Z]));
      Pen.Color := ColorLines;
      Brush.Color := ColorPetales;
      //FillRect(SourceRect);
      for i := 0 to High(FTabloBarres) do
      begin
        Rectangle(MG,
          MD + i * HBarre,
          MG + 1 + FTabloBarres[High(FTabloBarres) - i],
          MD + (1 + i) * HBarre);
      end;
    end;
    Cnv.CopyRect(SourceRect, DoubleBuffer.Canvas, SourceRect);
  finally
    DoubleBuffer.Free;

  end;
end;

// histogramme des directions
procedure TTableDesEntites.DrawDiagram(const Cnv: TCanvas;
  const L: integer;
  const ColorPetales: TColor;
  const ColorLines: TColor);
const
  Marge = 20;
var
  DoubleBuffer: TBitmap;
  SourceRect: TRect;
  Rayon: double;
  Rapport: double;
  Rd: integer;
  Cx, cy: integer;
  i, j: integer;
  Ang1, Ang2: double;
  pg: array[0..3] of TPoint;
  P1, P2: TPoint;
  Interval: double;
  NbPetales: integer;
begin
  AfficherMessage(Format('%s.DrawDiagram', [ClassName]));
  try
    DoubleBuffer := TBitmap.Create;
    DoubleBuffer.Height := L;
    DoubleBuffer.Width := L;
    SourceRect.Left := 0;
    SourceRect.Top := 0;
    SourceRect.Right := L;
    SourceRect.Bottom := L;

    // dessin du diagramme

    Rayon := 10.00;
    Rapport := 0.5 * (DoubleBuffer.Width - Marge) / Rayon;
    Rd := round(Rayon * Rapport);
    cx := DoubleBuffer.Width div 2;
    cy := DoubleBuffer.Height div 2;
    NbPetales := 1 + High(FClassRepart);
    AfficherMessage('  Paramètres du diagramme:');
    AfficherMessage(Format(' Rayon = %.2f', [Rayon]));

    Interval := PI / NbPetales;
    with DoubleBuffer.Canvas do
    begin
      Brush.Color := clWhite;
      FillRect(SourceRect);

      Pen.Color := clBlue;
      // mire
      Ellipse(cx - Rd, cy - Rd, cx + Rd, cy + Rd);
      //AngleRot:=StrToFloat(editAngleRot.Text)*pi/180;
      P1.X := Trunc(cx - Rd);
      P1.Y := Trunc(DoubleBuffer.Height - cy);
      P2.X := Trunc(cx + Rd);
      P2.Y := Trunc(DoubleBuffer.Height - cy);
      MoveTo(P1.X, P1.Y);
      LineTo(P2.X, P2.Y);
      P1.X := Trunc(cx);
      P1.Y := Trunc(DoubleBuffer.Height - (cy - rd));
      P2.X := Trunc(cx);
      P2.Y := Trunc(DoubleBuffer.Height - (cy + Rd));
      MoveTo(P1.X, P1.Y);
      LineTo(P2.X, P2.Y);

      //--------------------------------
      Brush.Color := ColorPetales;
      Pen.Color := ColorLines;
      // rosace
      for j := 0 to 1 do
        for i := 0 to NbPetales - 1 do
        begin
          Rd := round(self.FClassRepart[i] * Rayon * Rapport);
          Ang1 := PI * j + PI + ((0.5 * pi) - i * Interval);
          Ang2 := PI * j + PI + ((0.5 * pi) - (i + 1) * Interval);
          pg[0].X := cx;
          pg[0].Y := cy;
          pg[1].X := Trunc(cx + Rd * Cos(Ang1));
          pg[1].Y := Trunc(DoubleBuffer.Height - (cy + Rd * Sin(Ang1)));
          pg[2].X := Trunc(cx + Rd * Cos(Ang2));
          pg[2].Y := Trunc(DoubleBuffer.Height - (cy + Rd * Sin(Ang2)));
          pg[3].X := cx;
          pg[3].Y := cy;
          Polygon(pg);
        end;
    end;
    Cnv.CopyRect(SourceRect, DoubleBuffer.Canvas, SourceRect);
  finally
    DoubleBuffer.Free;
  end;
end;

function TTableDesEntites.ParseDiagram(const NbPetales: integer;
  const ViseeMini: double): integer;
var
  i, j: integer;
  LP: double;
  dx: double;
  dy: double;
  dz: double;
  Ang: double;
  LPT: double;
  s: string;
  ClasseMax: double;
  b: boolean;
  Interval: double;
begin
  //AfficherMessage(Format('%s.ParseDiagram: Nb = %d; Mini=%.2f', [ClassName, NbPetales, ViseeMini]));
  Result := -1;
  try
    Interval := PI / NbPetales;
    //AfficherMessage(Format('   %d pétales - Visée mini: %.2f', [NbPetales, ViseeMini]));
    // dimensionner table des classes
    SetLength(FClassRepart, 0);
    SetLength(FClassRepart, NbPetales);
    // initialiser cette table
    for i := 0 to NbPetales - 1 do
      FClassRepart[i] := 0.00;
    // boucle de calcul
    LPT := 0.00;
    for i := 1 to FNbEntites - 1 do
    begin
      // calcul longueur projetee
      with TableEntites[i] do
      begin
        if not (Drawn) then
          Continue;
        if Type_Entite = tgENTRANCE then
          Continue;

        dx := Une_Station_2_X - Une_Station_1_X;
        dy := Une_Station_2_Y - Une_Station_1_Y;
        dz := Une_Station_2_Z - Une_Station_1_Z;
        LP := Sqrt(Sqr(dx) + Sqr(dy));
        // test: passer à entite suivante si ce n'est pas une galerie
        if not (Type_Entite in [tgDEFAULT, tgFOSSILE .. tgSIPHON]) then
          Continue;
      end;
      // test: passer à entitée suivante si LP<ViseeMini
      if LP < ViseeMini then
        CONTINUE;
      // cumul des longueurs projetées
      LPT := LPT + LP;
      // calcul de l'angle
      // pour le calcul d'une rosette symétrique
      // l'angle retourné est dans la plage [0; PI]
      //Ang:=(0.50*PI)-ArcSin(dy/LP);
      Ang := ArcTan2(dx, dy + 1e-05);
      if Ang < 0 then
        Ang := Ang + 2 * PI;
      // répartition
      for j := 0 to NbPetales - 1 do
      begin
        if (Ang >= j * Interval) and (Ang < (j + 1) * Interval) then
        begin
          FClassRepart[j] := FClassRepart[j] + LP;
        end;
        if (Ang >= (PI + j * Interval)) and (Ang <
          (PI + (j + 1) * Interval)) then
        begin
          FClassRepart[j] := FClassRepart[j] + LP;
        end;
      end;
    end; // for i ..
    // Longueur totale nulle -> sortie de la fonction
    if (LPT = 0) then Exit;

    // afficher résultats sur la console
    dx := 360.0 / NbPetales;  // réutilisation de dx : calcul de l'angle d'un pétale
    dy := 100.0 / LPT;

    // mise en pourcentage
    ClasseMax := 0;
    for j := 0 to NbPetales - 1 do
      if FClassRepart[j] > ClasseMax then
        ClasseMax := FClassRepart[j];
    for j := 0 to NbPetales - 1 do
    begin
      FClassRepart[j] := FClassRepart[j] / ClasseMax;
    end;
    Result := 2;
  except
  end;// with self ...
end;


// histogramme des profondeurs
// DONE: Fonctionnement OK
function TTableDesEntites.ParseDepthHistogramme(const NbPetales: integer;
  const ViseeMini: double): integer;
var
  i, j: integer;
  Interval: double;
  dx, dy, dz: double;
  LPT, LP: double;
  Z1, Z2, ZMoy: double;
  ClasseMax: double;
  SinP: double; // sinus de la pente
begin
  //AfficherMessage(Format('%s.ParseDepthHistogramme: Nb = %d; Mini=%.2f', [ClassName, NbPetales, ViseeMini]));
  // pente maxi
  SinP := 0.50; // angle de 30°
  // intervalle
  Interval := (FCoinHautDroit.Z - FCoinBasGauche.Z) / NbPetales;
  // AfficherMessage(Format('Etendue en Z: %.2f (%.2f à %.2f) - Intervalle: %.2f', [FCoinHautDroit.Z - FCoinBasGauche.Z, FCoinHautDroit.Z, FCoinBasGauche.Z, Interval]));
  // table des classes
  SetLength(FClassRepartDepthDiagram, 0);
  SetLength(FClassRepartDepthDiagram, NbPetales);
  // initialiser cette table
  for i := 0 to NbPetales - 1 do
    FClassRepartDepthDiagram[i] := 0.00;
  // boucle de calcul
  LPT := 0.00;
  for i := 1 to FNbEntites - 1 do
  begin
    // calcul longueur projetee
    with TableEntites[i] do
    begin
      if not (Drawn) then
        Continue; // visées non dessinées
      if (Type_Entite = tgENTRANCE) then
        Continue;  // entrées
      if not (Type_Entite in [tgDEFAULT, tgFOSSILE .. tgSIPHON]) then
        Continue; // galeries non naturelles
      dx := Une_Station_2_X - Une_Station_1_X;
      dy := Une_Station_2_Y - Une_Station_1_Y;
      dz := Une_Station_2_Z - Une_Station_1_Z;
      ZMoy := (Une_Station_2_Z + Une_Station_1_Z) / 2.0;
      LP := Hypot3D(dx, dy, dz);
    end;
    // test: passer à entitée suivante si LP<ViseeMini
    if LP < ViseeMini then
      CONTINUE;

    // calcul de l'angle
    // pour le calcul d'une rosette symétrique
    // l'angle retourné est dans la plage [0; PI]
    //Ang:=(0.50*PI)-ArcSin(dy/LP);
    if not IsInRange(dz / LP, -SinP, SinP) then
      Continue;
    // cumul des longueurs projetées
    LPT := LPT + LP;
    // répartition
    for j := 0 to NbPetales - 1 do
    begin
      Z1 := FCoinBasGauche.Z + Interval * j;
      Z2 := FCoinBasGauche.Z + Interval * (j + 1);
      if IsInRange(ZMoy, Z1, Z2) then
        FClassRepartDepthDiagram[j] := FClassRepartDepthDiagram[j] + LP;
    end;
  end; // for i ..
  // Longueur totale nulle -> sortie de la fonction
  if LPT = 0 then
  begin
    Exit;
  end;
  // afficher résultats sur la console
  (*for j := 0 to NbPetales - 1 do
  begin
    AfficherMessage(Format('[%.2f; %.2f[: %.2f m (%.3f%%)',
      [FCoinBasGauche.Z + Interval * j,
      FCoinBasGauche.Z + Interval * (1 + j),
      FClassRepartDepthDiagram[j],
      100 * FClassRepartDepthDiagram[j] / LPT
      ]));
  end;
  //*)
  // mise en pourcentage
  ClasseMax := 0.0;
  for j := 0 to NbPetales - 1 do
    if FClassRepartDepthDiagram[j] > ClasseMax then
      ClasseMax := FClassRepartDepthDiagram[j];
  for j := 0 to NbPetales - 1 do
  begin
    FClassRepartDepthDiagram[j] := FClassRepartDepthDiagram[j] / ClasseMax;
  end;
  Result := 2;
end;

// calculs statistiques
procedure TTableDesEntites.CalculSpeleometrie;
var
  i,j : integer;
  EWE: TEntite;
  procedure  AjouterLongueurVisee(var QTableauRecap: TTableauVentilationSpeleometrie; const QE: TEntite; const QIdx: integer);
  var
    dv: Double;
  begin
    dv := Hypot3D(QE.Une_Station_2_X - QE.Une_Station_1_X,
                  QE.Une_Station_2_Y - QE.Une_Station_1_Y,
                  QE.Une_Station_2_Z - QE.Une_Station_1_Z);
    case QE.Type_Entite of
      tgENTRANCE   : ; // entrées
      tgDEFAULT,
      tgFOSSILE    : QTableauRecap[QIdx].Fossiles := QTableauRecap[QIdx].Fossiles + dv;
      tgVADOSE     : QTableauRecap[QIdx].Vadoses:=QTableauRecap[QIdx].Vadoses + dv;
      tgENNOYABLE  : QTableauRecap[QIdx].Ennoyables:=QTableauRecap[QIdx].Ennoyables + dv;
      tgSIPHON     : QTableauRecap[QIdx].Siphons:=QTableauRecap[QIdx].Siphons + dv;
      tgFIXPOINT   : ; // points fixes
      tgSURFACE    : QTableauRecap[QIdx].Speciaux:=QTableauRecap[QIdx].Speciaux + dv;
      tgTUNNEL     : QTableauRecap[QIdx].Filons:=QTableauRecap[QIdx].Filons + dv;
      tgMINE       : QTableauRecap[QIdx].Tunnels:=QTableauRecap[QIdx].Tunnels + dv;
    else
      QTableauRecap[QIdx].Fossiles:=QTableauRecap[QIdx].Fossiles + dv;
    end;
  end;
  function  GetEmptyVentilation: TVentilationSpeleometrie;
  begin
    with Result do
    begin
      Fossiles  :=0.00;
      Vadoses   :=0.00;
      Ennoyables:=0.00;
      Siphons   :=0.00;
      Tunnels   :=0.00;
      Filons    :=0.00;
      Speciaux  :=0.00;
    end;
  end;
  procedure PurgerTableau(var QTableauRecap: TTableauVentilationSpeleometrie; const QNb: integer);
  var
    n: Integer;
  begin
    SetLength(QTableauRecap, QNb);
    for n := 0 to QNb do QTableauRecap[n] := GetEmptyVentilation;
  end;
begin
  AfficherMessage('TdlgSpeleometrie.CalculSpeleometrie()');
  AfficherMessage('');
  // init tableaux
  PurgerTableau(FSpeleometrieParDates    , GetNbDates);
  PurgerTableau(FSpeleometrieParCouleurs , GetNbCouleurs);
  PurgerTableau(FSpeleometrieParReseaux  , GetNbReseaux);
  FRecapitulation := GetEmptyVentilation;
  AfficherMessage(format('  Speleometrie par dates (%d dates)',[GetNbDates]));
  //-----------------------
  for i:=1 to FNbEntites - 1 do
  begin
    EWE := TableEntites[i];
    for j:=0 to GetNbDates - 1 do
    begin
      if (trunc(EWE.DateLeve) = trunc(TableDates[j].DateTopo)) then
        AjouterLongueurVisee(FSpeleometrieParDates, EWE, j);
    end;
  end;
  // calcul de la spéléométrie par couleurs
  AfficherMessage(format('  Speleometrie par couleurs (%d couleurs)',[GetNbCouleurs]));
  //SetLength(FSpeleometrieParDates,FNbSeances);
  for i:=1 to FNbEntites - 1 do
  begin
    for j:=0 to GetNbCouleurs -1 do
    begin
      EWE := TableEntites[i];
      if (EWE.ColorEntite = TableCouleurs[j].Color) then
        AjouterLongueurVisee(FSpeleometrieParCouleurs, EWE, j);
    end;
  end;
  // calcul de la spéléométrie par réseaux
  AfficherMessage(format('  Speleometrie par reseaux (%d reseaux)',[GetNbReseaux]));
  //SetLength(FSpeleometrieParDates,FNbSeances);
  for i:=1 to FNbEntites - 1 do
  begin
    EWE := TableEntites[i];
    for j:=0 to GetNbReseaux -1 do
    begin
      if (EWE.IdxReseau = j) then AjouterLongueurVisee(FSpeleometrieParReseaux, EWE, j);
    end;
  end;

end;

// tri selon les numéros de série/station
procedure TTableDesEntites.SortBySerSts;
var
  ll666: integer;
  procedure QSortSerSts(var Table_Entites: array of TEntite; lidx: integer; ridx: integer);
  var
    k, e, mid: integer;
    Buffer: TEntite;
    bidon1, bidon2: Int64;
  begin
    if lidx >= ridx then Exit;
    mid := (lidx + ridx) div 2;
    Buffer := Table_Entites[lidx];
    Table_Entites[lidx] := Table_Entites[mid];
    Table_Entites[mid] := Buffer;
    e := lidx;
    for k := lidx+1 to ridx do
    begin
      bidon1 := Table_Entites[k].Entite_Serie * 10000 + Table_Entites[k].Entite_Station;
      bidon2 := Table_Entites[lidx].Entite_Serie * 10000 + Table_Entites[lidx].Entite_Station;
      if (bidon1 < bidon2) then
      begin
        Inc(e);
        Buffer := Table_Entites[e];
        Table_Entites[e] := Table_Entites[k];
        Table_Entites[k] := Buffer;
      end;
    end;
    Buffer := Table_Entites[lidx];
    Table_Entites[lidx] := Table_Entites[e];
    Table_Entites[e] := Buffer;
    QSortSerSts(Table_Entites, lidx, e-1);
    QSortSerSts(Table_Entites, e+1, ridx);
  end; // QSortCotes
begin
  AfficherMessage('Tri par index série/stations');
  QSortSerSts(TableEntites, 1, FNbEntites-1);
end;

// Classe TTableDesEntites
// Recherche de station
function TTableDesEntites.GetEntiteFromCle(const Cle: string): TEntite;
var
  S: string;
  WU: string;
  i: integer;
begin
  WU := UpperCase(Cle);
  //AfficherMessage('Dans GetEntiteFromCle: '+ Cle);
  Result:=GetEntite(LOW_INDEX);
  Result.Entite_Serie   := -1;
  Result.Entite_Station := -1;

  for i:=LOW_INDEX to FNbEntites-1 do
  begin
    S:=UpperCase(Trim(String(TableEntites[i].ID_Litteral_Pt)));
    if (Pos(WU, S) > 0) then
    begin
      Result:=GetEntite(i);
      Exit;
    end;
  end;
end;


function  TTableDesEntites.GetEntiteFromSerSt(const Ser, St: Integer): TEntite;
var
  S: string;
  i: integer;
begin
 // AfficherMessage('>> Dans GetEntiteFromSerSt');
  Result:=GetEntite(LOW_INDEX);
  Result.Entite_Serie   := -1;
  Result.Entite_Station := -1;
  for i:=LOW_INDEX to FNbEntites-1 do begin
    //AfficherMessage(Format('  F4: -> %d - %d %d',[i, Ser, St]));
    if (TableEntites[i].Entite_Serie   = Ser) and
       (TableEntites[i].Entite_Station = St) then
    begin
      Result:=GetEntite(i);
      Exit;
    end;
  end;
end;

// recherche d'une station
function TTableDesEntites.FindStationByCle(const Cle: string; out E: TEntite): boolean;
var
  S, S1, S2  : String;
  WU         : SizeInt;
  N          : Integer;
  qSer, qSt  : Integer;
begin
  Result := False;
  S := Trim(Cle);
  AfficherMessage(Format('%s.FindStationByCle(%s)', [self.ClassName, Cle]));
  // analyse de la clé
  // la clé contient un point --> recherche par série/station
  WU := Pos('.', S);
  if (WU > 0) then
  begin
    N := Length(S);
    S1 := Trim(Copy(S, 0, WU - 1));
    S2 := Trim(Copy(S, WU + 1, N));
    qSer := StrToIntDef(S1, 0);
    qSt  := StrToIntDef(S2, 0);
    E := GetEntiteFromSerSt(qSer, qSt);
    if (E.Entite_Serie >= 0) then
    begin
      Result := True;
      Exit;
    end;
  end;
  // station non trouvee ? Recherche par clé
  AfficherMessage('-- La clé de recherche est textuelle');
  E := GetEntiteFromCle(S);
  Result := (E.Entite_Serie >= 0);
end;

// calcul du volume du plus petit cube englobant la cavité
function TTableDesEntites.GetVolumeMinimalCirconscrit(const MyFiltre: string): Double;
const PI_180 = PI/180.0;
type
  TPoint2DT = record
    Drawn : boolean;
    X     : Double;
    Y     : Double;
  end;
var
  q: integer;
  Theta    : Double;
  Vo       : double;
  ArrP2D   : array of TPoint2DT;
  function CalculateVolumeCube(const Angle: double): double;
  var
    i: integer;
    E: TEntite;
    CosTheta,
    SinTheta : Double;

    QXMini,
    QYMini,
    QZMini,
    QXMaxi,
    QYMaxi,
    QZMaxi: Double;
    function Rot2D(const XX,YY: Double; const DR: boolean): TPoint2DT;
    begin
      Result.X := XX * CosTheta - YY * SinTheta;
      Result.Y := XX * SinTheta + YY * CosTheta;
      Result.Drawn := DR;
    end;

  begin
    Result:=0.00;
    QXMini  :=  1E15;
    QYMini  :=  1E15;
    QXMaxi  := -1E15;
    QYMaxi  := -1E15;

    QZMini  :=  1E15;
    QZMaxi  := -1E15;
    CosTheta:=Cos(Angle * PI/180);
    SinTheta:=Sin(Angle * PI/180);
    for i:=LOW_INDEX to FNbEntites-1 do begin
      E:=GetEntite(i);
      if E.Drawn then begin
        ArrP2D[i] := Rot2D(E.Une_Station_2_X, E.Une_Station_2_Y, E.Drawn);
        if (ArrP2D[i].X > QXMaxi) then QXMaxi := ArrP2D[i].X;
        if (ArrP2D[i].Y > QYMaxi) then QYMaxi := ArrP2D[i].Y;
        if (ArrP2D[i].X < QXMini) then QXMini := ArrP2D[i].X;
        if (ArrP2D[i].Y < QYMini) then QYMini := ArrP2D[i].Y;
        if (E.Une_Station_2_Z > QZMaxi) then QZMaxi := E.Une_Station_2_Z;
        if (E.Une_Station_2_Z < QZMini) then QZMini := E.Une_Station_2_Z;
      end; //if E.Drawn then begin
    end;
    Result := (QXMaxi - QXMini) *
              (QYMaxi - QYMini) *
              (QZMaxi - QZMini);
  end;
begin
  Result:=1e27;
  MetaFiltre(MyFiltre);
  AfficherMessage(format('%s.GetVolumeMinimalCirconscrit(%s)',[MyFiltre, ClassName]));
  try
    SetLength(ArrP2D, 0);
    SetLength(ArrP2D, FNbEntites);
    // purge du tableau
    for q:=Low(ArrP2D) to High(ArrP2D) do begin
      ArrP2D[q].X:=0.00;
      ArrP2D[q].Y:=0.00;
      ArrP2D[q].Drawn:=False;
    end;
    // calcul du volume du cube
    Theta:=0;
    for q:=0 to 179 do begin
      Vo := CalculateVolumeCube(q);
      if (Vo < Result) then begin
        Theta  := Q * 1.00;
        Result := Vo;
      end;
    end;
    AfficherMessage(Format('Volume: %f m3, angle: %f',[Result, Theta]));
  finally
    SetLength(ArrP2D, 0);
  end;
end;
// exportation dans un fichier de texte pour logiciel futur
// de dessin de type Therion
// fonction à renommer
//
//
//
//       (4).-------------------------------. (3)          Largeurs à gauche
//
//
//   n-1 (0) ================================ (1) n
//
//        (5) .----------------------------. (2)            Largeurs à droite
//
// export pour GHCaveDraw
procedure TTableDesEntites.ExportForGHCaveDraw(const MyFile: string; const QMyFiltre: string);
const
  FMT = '    %d'+TAB+ '%s'+ // ID de la visée
        TAB+'%d'+TAB+'%d' + // attributs
        TAB+'%.2f'+TAB+'%.2f'+TAB+'%.2f' +  // point de départ
        TAB+'%.2f'+TAB+'%.2f'+TAB+'%.2f' +  // point d'arrivée
        TAB+'%.2f'+TAB+'%.2f'+TAB+'%.2f' +  // point droit  (avec Z du sol)
        TAB+'%.2f'+TAB+'%.2f'+TAB+'%.2f';   // point gauche (avec Z du plafond)
var
  F: TextFile;
  i: integer;
  E : TEntite;
  ID: Int64;
  OldFilter: string;
  procedure WrtLn(const S: string);
  begin
    WriteLn(F, S);
  end;
begin
  AfficherMessage(Format('%s.ExportForGHCaveDraw(%s) - Filtres: %s',[ClassName, MyFile, QMyFiltre]));
  AssignFile(F, MyFile);
  try
    // sauvegarder le filtre
    OldFilter := QMyFiltre;
    // export vers GHCaveDraw = on envoie tout
    MetaFiltre('');
    // **********************************
    ReWrite(F);
    // en-tête
    //WrtLn(Format('##### %s generated by GHTopo - %s', [MyFile, DateTimeToStr(Now)]));
    //WrtLn('# -----------------');


    // points de base des éléments du dessin
    WrtLn('# Base points');
    WrtLn('');
    WrtLn('basepoints');
    for i := 1 to FNbEntites -1 do
    begin
      E  := GetEntite(i);
      if Not(E.Drawn) then Continue;

      ID := 100000 * E.Entite_Serie +
                10 * E.Entite_Station;
      WrtLn(Format(FMT,
                  [ID  , E.ID_Litteral_Pt,
                   E.Type_Entite, E.ColorEntite,
                   E.Une_Station_1_X, E.Une_Station_1_Y, E.Une_Station_1_Z,
                   E.Une_Station_2_X, E.Une_Station_2_Y, E.Une_Station_2_Z,
                   E.X2PG, E.Y2PG,  E.Z1PB,
                   E.X2PD, E.Y2PD,  E.Z1PH
                   ]));
    end;
    WrtLn('endbasepoints');

  finally
    // restauration du filtre précédent
    MetaFiltre(OldFilter);
    CloseFile(F);
  end;
end;
// export pour KML, OSM, ...
// Mode 0: 'Polygonale pour GHCaveDraw'
// Mode 1: 'Google Earth KML'
// Mode 2: 'Standard GPX'
// Mode 3: 'OpenStreetMap'
// Mode 4: 'Carto Exploreur (France uniquement)'
// Mode 5: 'Memory Map (France uniquement)'
// Silhouette: True = empreinte des galeries; False: polygonale seule
// Xo, Yo: coordonnées de l'entrée ppale si utilisation d'un repère local
// Filtres: valeur pour MetaFiltre
{$IFDEF USE_CONVERTISSEUR_EPSG}
procedure TTableDesEntites.ExportForCarto(const FileName: string;
                             const Convertisseur: TConversionSysteme;
                             const QSystemeGeog: string;
                             const Mode: TOutputFormatGIS;
                             const Silhouette: boolean;
                             const Xo, Yo: double;
                             const Filtres: string;
                             const PrefixStations: string;
                             const CouleurDefaut: TColor;
                             const UseColorGroupes: boolean);
{$ENDIF}
{$IFDEF USE_CONVERTISSEUR_ESIBERT}
procedure TTableDesEntites.ExportForCarto(const FileName: string;
                             const Convertisseur: TConversionSysteme;
                             const QSystemeGeog: string;
                             const Mode: TOutputFormatGIS;
                             const Silhouette: boolean;
                             const Xo, Yo: double;
                             const Filtres: string;
                             const PrefixStations: string;
                             const CouleurDefaut: TColor;
                             const UseColorGroupes: boolean);
{$ENDIF}
{$IFDEF USE_CONVERTISSEUR_JPC}
procedure TTableDesEntites.ExportForCarto(const FileName: string;
                             const Convertisseur: TConversionSysteme;
                             const QSystemeGeog: string;
                             const Mode: TOutputFormatGIS;
                             const Silhouette: boolean;
                             const Xo, Yo: double;
                             const Filtres: string;
                             const PrefixStations: string;
                             const CouleurDefaut: TColor;
                             const UseColorGroupes: boolean);
{$ENDIF}



const
  STYLE_GALERIE_PAR_DEFAUT = 'StyleGalerieParDefaut';
var
  fp: TextFile;
  i: integer;
  MyEntite: TEntite;
  E1: TEntite;
  OldSerie: LongInt;
  E2: TEntite;
  MySilhouette: TSilhouetteGalerie;
  PtsParois, PtsParois2: TPtsSectionTransversale;
  PolygoneSilhouette: Boolean;
  foo: String;
  NbreSilhouettes: Integer;
  FConvertisseurEPSG: TConversionSysteme;
  _WU: TColorGaleries;
  function IsSameSerie(const E1,E2: TEntite): boolean;
  begin
    Result:= ((E1.Entite_Serie   = E2.Entite_Serie) and
              (E1.Entite_Station = (E2.Entite_Station + 1)));

  end;

  // encodage du style
  procedure KML_WritePolygonStyle(const IDStyle: string;
                                  const LineColor, FillColor: TColorRGBA;
                                  const LineWidth: double);
  begin
    //ewe

    Writeln(fp, Format('   <Style id="%s">', [IDStyle]));
    WriteLn(fp,        '     <LineStyle>');
    WriteLn(fp, Format('       %s', [KMLColor(LineColor.R, LineColor.G, LineColor.B, LineColor.A)]));
    WriteLn(fp, Format('       <width>%.1f</width>', [LineWidth]));
    WriteLn(fp,        '     </LineStyle>');
    WriteLn(fp,        '     <PolyStyle>');
    WriteLn(fp, Format('       %s', [KMLColor(FillColor.R, FillColor.G, FillColor.B, FillColor.A)]));
    WriteLn(fp,        '     </PolyStyle>');
    WriteLn(fp,        '   </Style>');
  end;
  // données OSM
  procedure WriteOSMNodeRef(const Idx: Int64);
  begin
    //<nd ref='-4666' />
    WriteLn(fp, Format('      <nd ref=''%d'' />', [-Idx]));
  end;
  procedure WriteOSMNode(const Idx: Int64; const Action: string; const Visible: boolean; const QOSMLon, QOSMLat: double);
  begin
    // <node id='-4768' action='modify' visible='true' lat='0.013789139478116975' lon='-0.024434175728050987' />
    WriteLn(fp, Format('  <node id=''%d'' action=''%s'' visible=''%s'' lat=''%.15f'' lon=''%.15f'' />',
                       [-Idx,       // ajout ds OSM ==> IDX négatifs
                        Action,
                        BoolToStr(Visible, 'true', 'false'),
                        QOSMLat, QOSMLon
                       ]));
  end;
  procedure WriteOSMTag(const K, V: string);
  begin
    WriteLn(fp, Format('   <tag k=''%s'' v=''%s'' />',[K, V]));
  end;

  // en-tête de fichiers et déf des styles
  procedure WriteHeader(const M: TOutputFormatGIS);
  var
    ii    : Integer;
    QColor: TColorRGBA;
    QColorShadow: TColorRGBA;
    EWE: TColorGaleries;
  begin
    QColor.R := Red(CouleurDefaut);
    QColor.G := Green(CouleurDefaut);
    QColor.B := Blue(CouleurDefaut);
    QColor.A := $FF;
    QColorShadow.R := Red(clWhite);
    QColorShadow.G := Green(clWhite);
    QColorShadow.B := Blue(clWhite);
    QColorShadow.A := $FF;
    case M of
      gisGHCAVEDRAW:  // GHCaveDraw; sans objet
        begin
          ;
        end;
      gisKML: // KML: OK pour les silhouettes
        begin // KML // On définit aussi les styles d'objets polygones =
          WriteLn(fp, FormatterVersionEncodageXML('1.0', 'UTF-8'));
          WriteLn(fp, Format('<kml xmlns="%s" xmlns:gx="%s" xmlns:kml="%s" xmlns:atom="%s">',
                      [KML_OPENGIS_WEBSITE,
                       KML_GOOGLE_KML_WEBSITE,
                       KML_OPENGIS_WEBSITE,
                       W3C_W3_WEBSITE
                      ]));
          WriteLn(fp, '  <Document>');
          // styles de polygones
          KML_WritePolygonStyle(STYLE_GALERIE_PAR_DEFAUT, QColor, QColor, 1.0);
          if (UseColorGroupes) then
          begin
            for ii := 0 to self.GetNbCouleurs - 1 do
            begin
              EWE := self.GetCouleurByIdx(ii);
              QColor.R := Red(EWE.Color);
              QColor.G := Green(EWE.Color);
              QColor.B := Blue(EWE.Color);
              QColor.A := $FF;
              QColorShadow := QColor;
             // QColorShadow.A:= $FF;;
              KML_WritePolygonStyle(STYLE_GALERIE_PAR_DEFAUT +
                                    format('%.2X%.2X%.2X%.2X', [QColor.R, QColor.G, QColor.B, QColor.A]),
                                    QColorShadow,
                                    QColor,
                                    1.0);
            end;
          end;
          // dossier principal = la cavité
          WriteLn(fp, '    <Folder>');
          WriteLn(fp, Format('    <name>%s</name>', ['MyCavite001']));
          WriteLn(fp, Format('    <open>%d</open>', [1]));

        end;
      gisGPX:
        begin // GPX
          WriteLn(fp, FormatterVersionEncodageXML('1.0', 'ISO-8859-1')); //<?xml version="1.0" encoding="ISO-8859-1"?>');
          WriteLn(fp, Format('<gpx version="%.1f" creator="%s" xmlns:xsi="%s" xmlns="%s">',
                             [1.1, // version xml
                              rsGHTOPOEXENAME, // logiciel auteur
                              W3C_XML_SCHEMA_WEBSITE,
                              GPX_TOPOGRAPHIX_WEBSITE
                             ]));
        end;
      gisOSM:
        begin // OSM
          //<?xml version='1.0' encoding='UTF-8'?>
          WriteLn(fp, FormatterVersionEncodageXML('1.0', 'UTF-8'));
          WriteLn(fp, Format('<osm version=''%s'' upload=''%s'' generator=''%s''>',
                      ['0.6', 'true', rsGHTOPOEXENAME]));
        end;
      gisCARTO_EXPLOREUR:
        begin // Carto Exploreur
          WriteLn(fp, 'F;T;Lib');
        end;
      gisMEMORY_MAP:
        begin // Memory Map
        end;
    end;
  end;
  procedure WriteFooter(const M: TOutputFormatGIS);
  begin
     case M of
      gisGHCAVEDRAW: ; // GHCaveDraw; sans objet
      gisKML:
         begin // KML
           // dossier principal = la cavité
           WriteLn(fp, '    </Folder>');
           WriteLn(fp, '  </Document>');
           WriteLn(fp, '</kml>');
         end;
      gisGPX:
        begin // GPX
          WriteLn(fp, ' </gpx>');
        end;
      gisOSM:
        begin // OSM
          WriteLn(fp, ' </osm>');                       ;
        end;
      gisCARTO_EXPLOREUR:   ; // Carto Exploreur , pas de footer
      gisMEMORY_MAP:        ; // Memory Map, pas de footer
   end;
  end;
  procedure WriteConvertedPoint(const M: TOutputFormatGIS;
                                const PolyMode: TPolyMode;
                                const ET: TEntite;
                                const DefaultColor: TColor);
  const
    FMT_CE = 'P;%.12f;%.12f;%.0f;-9999;;0.00;00:00:00;0;0.0;-1';

  var
    CPColor: TColor;
    R   : Double;
    P1, P2: TProjUV;
  begin
    CPColor := DefaultColor; //IIF(DoUseColors, ET.ColorEntite, );
    // éliminer visées trop courtes et trop longues
    R:=Hypot3D(ET.Une_Station_2_X - ET.Une_Station_1_X,
               ET.Une_Station_2_Y - ET.Une_Station_1_Y,
               ET.Une_Station_2_Z - ET.Une_Station_1_Z
              );
    if (R > 160.00) then Exit;
    // éliminer les entrées
    if (ET.Type_Entite = tgENTRANCE) then Exit;
    // export point
    case M of
      gisGHCAVEDRAW: ; // GHCaveDraw; sans objet
      gisKML:
        begin // KML; sans objet
          ;
        end;
      gisGPX:
        begin // GPX
          case PolyMode of
            tpmENTETE_POLY: //0
              begin  // en-tete de track/série
                WriteLn(fp, '    <trk> <trkseg>');
              end;
            tpmSTART_POLY: //1
              begin // début de polyligne (série)
                P1.U := ET.Une_Station_1_X;
                P1.V := ET.Une_Station_1_Y;
                {$IFDEF USE_CONVERTISSEUR_EPSG}
                   P2 := FConvertisseurEPSG.ConversionSyst1ToSyst2(QSystemeGeog, '4326', P1);
                   WriteLn(fp, Format('        <trkpt lat="%.8f" lon="%.8f"></trkpt> <!-- %d.%d -->', [P2.V, P2.U, ET.Entite_Serie, ET.Entite_Station]));
                {$ENDIF}
                {$IFDEF USE_CONVERTISSEUR_ESIBERT}
                   P2 := FConvertisseurEPSG.ConversionSyst1ToSyst2(QSystemeGeog, 'WGS84', P1);
                   WriteLn(fp, Format('        <trkpt lat="%.8f" lon="%.8f"></trkpt> <!-- %d.%d -->', [P2.U, P2.V, ET.Entite_Serie, ET.Entite_Station]));
                {$ENDIF}
                {$IFDEF USE_CONVERTISSEUR_JPC}
                   P2 := FConvertisseurEPSG.ConversionSyst1ToSyst2(QSystemeGeog, 'WGS84', P1);
                   WriteLn(fp, Format('        <trkpt lat="%.8f" lon="%.8f"></trkpt> <!-- %d.%d -->', [P2.U, P2.V, ET.Entite_Serie, ET.Entite_Station]));
                {$ENDIF}
               end;
            tpmPOINT_POLY: //2
              begin // point de polyligne (série)
                P1.U := ET.Une_Station_2_X;
                P1.V := ET.Une_Station_2_Y;
                {$IFDEF USE_CONVERTISSEUR_EPSG}
                   P2 := FConvertisseurEPSG.ConversionSyst1ToSyst2(QSystemeGeog, '4326', P1);
                   WriteLn(fp, Format('        <trkpt lat="%.8f" lon="%.8f"></trkpt> <!-- %d.%d -->', [P2.V, P2.U, ET.Entite_Serie, ET.Entite_Station]));
                {$ENDIF}
                {$IFDEF USE_CONVERTISSEUR_ESIBERT}
                   P2 := FConvertisseurEPSG.ConversionSyst1ToSyst2(QSystemeGeog, 'WGS84', P1);
                   WriteLn(fp, Format('        <trkpt lat="%.8f" lon="%.8f"></trkpt> <!-- %d.%d -->', [P2.U, P2.V, ET.Entite_Serie, ET.Entite_Station]));
                {$ENDIF}
                {$IFDEF USE_CONVERTISSEUR_JPC}
                   P2 := FConvertisseurEPSG.ConversionSyst1ToSyst2(QSystemeGeog, 'WGS84', P1);
                   WriteLn(fp, Format('        <trkpt lat="%.8f" lon="%.8f"></trkpt> <!-- %d.%d -->', [P2.U, P2.V, ET.Entite_Serie, ET.Entite_Station]));
                {$ENDIF}
                //P2 := FConvertisseurEPSG.Conversion_EPSG_To_EPSG(QCodeEPSG, 4326, P1);
             end;
            tpmEND_POLY:   //3
              begin // GPX: balise de cloture
               WriteLn(fp, '    </trkseg> </trk>');
              end;
          end;
        end;
      gisOSM:
        begin // OSM
          ;
        end;
      gisCARTO_EXPLOREUR:
        begin // Carto Exploreur
          case PolyMode of
            tpmENTETE_POLY:
              WriteLn(fp, Format('T; Serie%d;1;0;%d;139;2;1',[ET.Entite_Serie, CPColor]));
            tpmSTART_POLY:
              begin  // début de polyligne (série)
                P1.U := ET.Une_Station_1_X;
                P1.V := ET.Une_Station_1_Y;
                //P2 := FConvertisseurEPSG.Conversion_EPSG_To_EPSG(QCodeEPSG, 4326, P1);
                {$IFDEF USE_CONVERTISSEUR_EPSG}
                   P2 := FConvertisseurEPSG.ConversionSyst1ToSyst2(QSystemeGeog, '4326', P1);
                   WriteLn(fp, Format(FMT_CE, [P2.U, P2.V,
                                             ET.Une_Station_1_Z,
                                             ET.Entite_Serie, ET.Entite_Station
                                 ]));
                {$ENDIF}
                {$IFDEF USE_CONVERTISSEUR_ESIBERT}
                   P2 := FConvertisseurEPSG.ConversionSyst1ToSyst2(QSystemeGeog, 'WGS84', P1);
                   WriteLn(fp, Format(FMT_CE, [P2.V, P2.U,
                                            ET.Une_Station_1_Z,
                                            ET.Entite_Serie, ET.Entite_Station
                                 ]));
                {$ENDIF}
                {$IFDEF USE_CONVERTISSEUR_JPC}
                   P2 := FConvertisseurEPSG.ConversionSyst1ToSyst2(QSystemeGeog, 'WGS84', P1);
                   WriteLn(fp, Format(FMT_CE, [P2.V, P2.U,
                                            ET.Une_Station_1_Z,
                                            ET.Entite_Serie, ET.Entite_Station
                                 ]));
                {$ENDIF}
              end;
            tpmPOINT_POLY:
              begin // point de polyligne (série)
                P1.U := ET.Une_Station_2_X;
                P1.V := ET.Une_Station_2_Y;
                // P2 := FConvertisseurEPSG.Conversion_EPSG_To_EPSG(QCodeEPSG, 4326, P1);
                {$IFDEF USE_CONVERTISSEUR_EPSG}
                   P2 := FConvertisseurEPSG.ConversionSyst1ToSyst2(QSystemeGeog, '4326', P1);
                 WriteLn(fp, Format(FMT_CE,
                                   [P2.U, P2.V,
                                    ET.Une_Station_2_Z,
                                    ET.Entite_Serie, ET.Entite_Station
                                   ]));
                {$ENDIF}
                {$IFDEF USE_CONVERTISSEUR_ESIBERT}
                   P2 := FConvertisseurEPSG.ConversionSyst1ToSyst2(QSystemeGeog, 'WGS84', P1);
                   WriteLn(fp, Format(FMT_CE,
                                   [P2.V, P2.U,
                                    ET.Une_Station_2_Z,
                                    ET.Entite_Serie, ET.Entite_Station
                                   ]));
                {$ENDIF}
                {$IFDEF USE_CONVERTISSEUR_JPC}
                   P2 := FConvertisseurEPSG.ConversionSyst1ToSyst2(QSystemeGeog, 'WGS84', P1);
                   WriteLn(fp, Format(FMT_CE,
                                   [P2.V, P2.U,
                                    ET.Une_Station_2_Z,
                                    ET.Entite_Serie, ET.Entite_Station
                                   ]));
                {$ENDIF}
              end;
           end;
         end;
       gisMEMORY_MAP:
         begin // Memory Map
          case PolyMode of
            tpmENTETE_POLY:
              begin
                Writeln(fp, Format('TK01, "Serie%d", "Track:%s", 1, 0, %d, 0, %d, 0, 0, 0, 0',
                   [ET.Entite_Serie, PrefixStations,
                    0, // nombre de points
                    CPColor
                   ]));
              end;
            tpmSTART_POLY:
              begin  // début de polyligne (série)
                P1.U := ET.Une_Station_1_X;
                P1.V := ET.Une_Station_1_Y;
                //P2 := FConvertisseurEPSG.Conversion_EPSG_To_EPSG(QCodeEPSG, 4326, P1);
                {$IFDEF USE_CONVERTISSEUR_EPSG}
                  P2 := FConvertisseurEPSG.ConversionSyst1ToSyst2(QSystemeGeog, '4326', P1);
                  WriteLn(fp, Format('TP01,%.6f,%.6f, 0, 0, 0', [P2.U, P2.V]));
                {$ENDIF}
                {$IFDEF USE_CONVERTISSEUR_ESIBERT}
                   P2 := FConvertisseurEPSG.ConversionSyst1ToSyst2(QSystemeGeog, 'WGS84', P1);
                   WriteLn(fp, Format('TP01,%.6f,%.6f, 0, 0, 0', [P2.V, P2.U]));
                {$ENDIF}
                {$IFDEF USE_CONVERTISSEUR_JPC}
                   P2 := FConvertisseurEPSG.ConversionSyst1ToSyst2(QSystemeGeog, 'WGS84', P1);
                   WriteLn(fp, Format('TP01,%.6f,%.6f, 0, 0, 0', [P2.V, P2.U]));
                {$ENDIF}
              end;
            tpmPOINT_POLY:
              begin // point de polyligne (série)
                P1.U := ET.Une_Station_2_X;
                P1.V := ET.Une_Station_2_Y;
                //P2 := FConvertisseurEPSG.Conversion_EPSG_To_EPSG(QCodeEPSG, 4326, P1);
                {$IFDEF USE_CONVERTISSEUR_EPSG}
                  P2 := FConvertisseurEPSG.ConversionSyst1ToSyst2(QSystemeGeog, '4326', P1);
                  WriteLn(fp, Format('TP01,%.6f,%.6f, 0, 0, 0', [P2.U, P2.V]));
                {$ENDIF}
                {$IFDEF USE_CONVERTISSEUR_ESIBERT}
                  P2 := FConvertisseurEPSG.ConversionSyst1ToSyst2(QSystemeGeog, 'WGS84', P1);
                  WriteLn(fp, Format('TP01,%.6f,%.6f, 0, 0, 0', [P2.V, P2.U]));
                {$ENDIF}
                {$IFDEF USE_CONVERTISSEUR_JPC}
                  P2 := FConvertisseurEPSG.ConversionSyst1ToSyst2(QSystemeGeog, 'WGS84', P1);
                  WriteLn(fp, Format('TP01,%.6f,%.6f, 0, 0, 0', [P2.V, P2.U]));
                {$ENDIF}
              end; // 2
           end;
         end;
    end; // case
  end;
  // dessiner le polygone de silhouette
  procedure DessinerPolygoneSilhouette(const M: TOutputFormatGIS; const QSilhouette: TSilhouetteGalerie; const TagString: string; const TagNum: Int64);
  const
    DELTA_Z_SURFACE = 10.00;
  var
    QC: TColorRGBA;
    ii : integer;
    PP: TPoint2Df;
    P1, P2: TProjUV;
    Nb: LongInt;
    EE: TPtsSectionTransversale;
    QStart: Integer;
    QStartWay: Integer;
    fred_mignon: String;  // le Fred Mignon est en string :lol:
  begin
     // construction du polygone
     if (not QSilhouette.BuildPolygoneParois) then Exit;
     case M of
      gisGHCAVEDRAW: ; // GHCaveDraw; sans objet
      gisKML:
        begin // KML
          Nb := QSilhouette.GetNbPointsPolygoneParois;
          WriteLn(fp, '      <Placemark>');
          WriteLn(fp, Format('       <!-- Polygone: %s (%d sommets) -->',['MyPolygone', Nb]));
          WriteLn(fp, Format('         <name>%s</name>', [TagString]));
          if (UseColorGroupes) then    // utilise les couleurs de groupe de la topographie
          begin
            QC := QSilhouette.GetFillColor;
            fred_mignon := STYLE_GALERIE_PAR_DEFAUT +
                           format('%.2X%.2X%.2X%.2X', [QC.R, QC.G, QC.B, QC.A]);
            WriteLn(fp, Format('         <styleUrl>#%s</styleUrl>', [fred_mignon]));
          end
          else                        // sinon, utilise la couleur spécifiée
          begin
            WriteLn(fp, Format('         <styleUrl>#%s</styleUrl>', [STYLE_GALERIE_PAR_DEFAUT]));
          end;
          WriteLn(fp,        '         <Polygon>');
          WriteLn(fp, Format('           <tesselate>%d</tesselate>', [1]));
          WriteLn(fp, '           <outerBoundaryIs>');
          WriteLn(fp, '             <LinearRing>');
          WriteLn(fp, '               <coordinates>');
          for ii := 0 to Nb - 1 do
          begin
            PP := QSilhouette.GetPointPolygoneParois(ii);
            P1.U := PP.X;
            P1.V := PP.Y;
            //P2 := FConvertisseurEPSG.Conversion_EPSG_To_EPSG(QCodeEPSG, 4326, P1);
            {$IFDEF USE_CONVERTISSEUR_EPSG}
               P2 := FConvertisseurEPSG.ConversionSyst1ToSyst2(QSystemeGeog, '4326', P1);
               WriteLn(fp, Format('             %.16f,%.16f,%.0f', [P2.U, P2.V, DELTA_Z_SURFACE]));   //Lon, Lat
            {$ENDIF}
            {$IFDEF USE_CONVERTISSEUR_ESIBERT}
               P2 := FConvertisseurEPSG.ConversionSyst1ToSyst2(QSystemeGeog, 'WGS84', P1);
               WriteLn(fp, Format('             %.16f,%.16f,%.0f', [P2.V, P2.U, DELTA_Z_SURFACE]));   //Lon, Lat
            {$ENDIF}
            {$IFDEF USE_CONVERTISSEUR_JPC}
               P2 := FConvertisseurEPSG.ConversionSyst1ToSyst2(QSystemeGeog, 'WGS84', P1);
               WriteLn(fp, Format('             %.16f,%.16f,%.0f', [P2.V, P2.U, DELTA_Z_SURFACE]));   //Lon, Lat
            {$ENDIF}
          end;
          WriteLn(fp, '               </coordinates>');
          WriteLn(fp, '             </LinearRing>');
          WriteLn(fp, '           </outerBoundaryIs>');
          WriteLn(fp, '         </Polygon>');
	  WriteLn(fp, '      </Placemark>');
        end;
      gisGPX:
        begin // GPX
          Nb := QSilhouette.GetNbPointsPolygoneParois;
          WriteLn(fp, Format('<!-- Polygone: %s (%d sommets) -->',['MyPolygone', Nb]));
          WriteLn(fp, '    <trk> <trkseg>');
          for ii := 0 to Nb - 1 do
          begin
            PP := QSilhouette.GetPointPolygoneParois(ii);
            P1.U := PP.X;
            P1.V := PP.Y;
            //P2 := FConvertisseurEPSG.Conversion_EPSG_To_EPSG(QCodeEPSG, 4326, P1);
            {$IFDEF USE_CONVERTISSEUR_EPSG}
               P2 := FConvertisseurEPSG.ConversionSyst1ToSyst2(QSystemeGeog, '4326', P1);
            {$ENDIF}
            {$IFDEF USE_CONVERTISSEUR_ESIBERT}
               P2 := FConvertisseurEPSG.ConversionSyst1ToSyst2(QSystemeGeog, 'WGS84', P1);
            {$ENDIF}
            {$IFDEF USE_CONVERTISSEUR_JPC}
               P2 := FConvertisseurEPSG.ConversionSyst1ToSyst2(QSystemeGeog, 'WGS84', P1);
            {$ENDIF}
            WriteLn(fp, Format('        <trkpt lat="%.8f" lon="%.8f"></trkpt>', [P2.U, P2.V]))    //QLat, QLon
          end;
          WriteLn(fp, '    </trkseg> </trk>');
        end;
      gisOSM:
        begin // OSM
          // première passe: Liste des sommets
          QStart := 10000 * (1 + TagNum);
          Nb := QSilhouette.GetNbPointsPolygoneParois;
          for ii := 0 to Nb - 1 do
          begin
            PP := QSilhouette.GetPointPolygoneParois(ii);
            P1.U := PP.X;
            P1.V := PP.Y;
            //P2 := FConvertisseurEPSG.Conversion_EPSG_To_EPSG(QCodeEPSG, 4326, P1);
            {$IFDEF USE_CONVERTISSEUR_EPSG}
              P2 := FConvertisseurEPSG.ConversionSyst1ToSyst2(QSystemeGeog, '4326', P1);
              WriteOSMNode(QStart + (ii + 1), 'modify', True, P2.U, P2.V);        //QLon, QLat
            {$ENDIF}
            {$IFDEF USE_CONVERTISSEUR_ESIBERT}
              P2 := FConvertisseurEPSG.ConversionSyst1ToSyst2(QSystemeGeog, 'WGS84', P1);
              WriteOSMNode(QStart + (ii + 1), 'modify', True, P2.V, P2.U);        //QLon, QLat
            {$ENDIF}
            {$IFDEF USE_CONVERTISSEUR_JPC}
              P2 := FConvertisseurEPSG.ConversionSyst1ToSyst2(QSystemeGeog, 'WGS84', P1);
              WriteOSMNode(QStart + (ii + 1), 'modify', True, P2.V, P2.U);        //QLon, QLat
            {$ENDIF}
          end;
          // seconde passe: Vertex
          QStartWay := 100000 * (1 + TagNum);
          WriteLn(fp, Format('   <way id=''%d'' action=''%s'' visible=''%s''>', [-QStartWay, 'modify', 'true']));
          for ii := 0 to Nb - 1 do
          begin
            PP := QSilhouette.GetPointPolygoneParois(ii);
            P1.U := PP.X;
            P1.V := PP.Y;
            //P2 := FConvertisseurEPSG.Conversion_EPSG_To_EPSG(QCodeEPSG, 4326, P1);
            {$IFDEF USE_CONVERTISSEUR_EPSG}
               P2 := FConvertisseurEPSG.ConversionSyst1ToSyst2(QSystemeGeog, '4326', P1);
            {$ENDIF}
            {$IFDEF USE_CONVERTISSEUR_ESIBERT}
               P2 := FConvertisseurEPSG.ConversionSyst1ToSyst2(QSystemeGeog, 'WGS84', P1);
            {$ENDIF}
            {$IFDEF USE_CONVERTISSEUR_JPC}
               P2 := FConvertisseurEPSG.ConversionSyst1ToSyst2(QSystemeGeog, 'WGS84', P1);
            {$ENDIF}
            WriteOSMNodeRef(QStart + (ii + 1));
          end;
          // cloture du polygone
          WriteOSMNodeRef(QStart + 1);
          WriteOSMTag('leisure', 'common');
          WriteLn(fp, '   </way>');
        end;
      gisCARTO_EXPLOREUR:  ; // Carto Exploreur , pas de footer
      gisMEMORY_MAP:       ; // Memory Map, pas de footer
    end;
  end;
begin
  // lister les différentes couleurs
  AfficherMessage(Format('Liste des couleurs (%d couleurs)',[self.GetNbCouleurs - 1]));
  for i := 0 to self.GetNbCouleurs - 1 do
  begin
    _WU := self.GetCouleurByIdx(i);
    AfficherMessage(Format('%d: %X', [i, _wu.Color]));
  end;
  AfficherMessage('');
  // affectation du convertisseur
  FConvertisseurEPSG := Convertisseur;
  NbreSilhouettes := 0;
  foo := ChooseString(Ord(Mode), ['GHCaveDraw', 'Google Earth', 'GPX standard', 'OpenStreetMap', 'Carto Exploreur', 'Memory Map']);
  //CouleurDefaut := clRed;
  // controle paramètres
  AfficherMessage(Format('%s.ExportForCarto(%s)',[self.ClassName,FileName]));
  AfficherMessage(Format('--> Filtres: %s',[Filtres]));
  AfficherMessage(Format('--> Mode: %d; %s',[Mode, IIF(Silhouette, 'Silhouette des parois', 'Polygonale seule')]));
  {$IFDEF USE_CONVERTISSEUR_EPSG}
    AfficherMessage(Format('--> Systeme coordonnees: EPSG:%d',[QSystemeGeog]));
  {$ENDIF}
  {$IFDEF USE_CONVERTISSEUR_ESIBERT}
    AfficherMessage(Format('--> Systeme coordonnees: %s',[QSystemeGeog]));
  {$ENDIF}
  {$IFDEF USE_CONVERTISSEUR_JPC}
    AfficherMessage(Format('--> Systeme coordonnees: %s',[QSystemeGeog]));
  {$ENDIF}
  AfficherMessage(Format('--> Xo = %.2f, Yo = %.2f',[Xo, Yo]));
  AfficherMessage(Format('--> Export vers: %s', [foo]));
  // si Mode = 0, export vers GHCaveDraw
  if (Mode = gisGHCAVEDRAW) then
  begin
    ExportForGHCaveDraw(FileName, ''); // on envoie tout le réseau
    Exit;
  end;
  try
    // Un coup de MétaFiltre pour commencer
    self.MetaFiltre(Filtres);
    // Tri par séries et stations
    SortBySerSts;
    // ouverture fichier
    AssignFile(fp, FileName);
    try
      ReWrite(fp);
      // initialisation variable de début et écriture première ligne
      WriteHeader(Mode);
      if (Silhouette) then
      begin
        AfficherMessage('Mode silhouettes');
        MySilhouette := TSilhouetteGalerie.Create;
        // premier polygone
        E1 := GetEntite(1);
        AfficherMessage('00');
        MySilhouette.ClearSilhouette(e1.ColorEntite, e1.ColorEntite);
        AfficherMessage('01');
        PtsParois.ParoiDroiteX := E1.X1PD;
        PtsParois.ParoiDroiteY := E1.Y1PD;
        PtsParois.ParoiGaucheX := E1.X1PG;
        PtsParois.ParoiGaucheY := E1.Y1PG;
        MySilhouette.AddPtsParois(PtsParois);
        AfficherMessage(format('%d entités',[self.GetNbEntites]));
        for i:= 2 to self.GetNbEntites - 1 do
        begin
          if (Assigned(FProcDispProgression)) then FProcDispProgression(i, 2, self.GetNbEntites, Format('Entite %d / %d', [i, self.GetNbEntites]));
          E1 := GetEntite(i);
          E2 := GetEntite(i-1);
          // entrée de cavité --> Au suivant !
          if (E1.Type_Entite = tgENTRANCE) then Continue;
          PtsParois.ParoiDroiteX := E1.X2PD;
          PtsParois.ParoiDroiteY := E1.Y2PD;
          PtsParois.ParoiGaucheX := E1.X2PG;
          PtsParois.ParoiGaucheY := E1.Y2PG;
          if IsSameSerie(E1, E2) then
          begin
            //WriteConvertedPoint(Mode, tpmPOINT_POLY, E1, CouleurDefaut);
            MySilhouette.AddPtsParois(PtsParois);
          end else
          begin
            // on construit et trace le polygone
            DessinerPolygoneSilhouette(Mode, MySilhouette, Format('Galerie%.5d',[NbreSilhouettes]), NbreSilhouettes);
            NbreSilhouettes := 1 + NbreSilhouettes;
            // on purge la silhouette courante pour en accueillir la suivante
            MySilhouette.ClearSilhouette(E1.ColorEntite, E1.ColorEntite);
            PtsParois2.ParoiDroiteX := E1.X1PD;
            PtsParois2.ParoiDroiteY := E1.Y1PD;
            PtsParois2.ParoiGaucheX := E1.X1PG;
            PtsParois2.ParoiGaucheY := E1.Y1PG;
            MySilhouette.AddPtsParois(PtsParois2);
          end;
        end;  // for
        AfficherMessage('Mode silhouettes terminé');
        // dernier polygone
        DessinerPolygoneSilhouette(Mode, MySilhouette, Format('Galerie%.5d',[NbreSilhouettes]), NbreSilhouettes);
        NbreSilhouettes := 1 + NbreSilhouettes;
        MySilhouette.ClearSilhouette(clWhite, clWhite);
        MySilhouette.Free;
      end
      else
      begin
        E1 := GetEntite(1);
        WriteConvertedPoint(Mode, tpmENTETE_POLY, E1, CouleurDefaut);
        WriteConvertedPoint(Mode, tpmSTART_POLY, E1, CouleurDefaut);
        for i:= 2 to self.GetNbEntites - 1 do
        begin
          if (Assigned(FProcDispProgression)) then FProcDispProgression(i, 2, self.GetNbEntites, Format('Entite %d / %d', [i, self.GetNbEntites]));
          E1 := GetEntite(i);
          E2 := GetEntite(i-1);
          if IsSameSerie(E1, E2) then
          begin
            WriteConvertedPoint(Mode, tpmPOINT_POLY, E1, CouleurDefaut);
          end else
          begin
            WriteConvertedPoint(Mode, tpmEND_POLY, E1, CouleurDefaut); // pour GPX uniquement: balise de cloture
            WriteConvertedPoint(Mode, tpmENTETE_POLY, E1, CouleurDefaut);
            WriteConvertedPoint(Mode, tpmSTART_POLY, E1, CouleurDefaut);
          end;
        end;
        // dernière entité
        WriteConvertedPoint(Mode, tpmEND_POLY, E1, CouleurDefaut); // pour GPX uniquement: balise de cloture
      end;
      WriteFooter(Mode);
    finally
      CloseFile(fp);
    end;
  finally
  end;
end;
// génération de fichiers TOP métafiltrés
function TTableDesEntites.GenerateMetaFilteredTOP(const MyFichierTOP, MyFilter: string): integer;
var
  E: TEntite;
  i,q: integer;
  FTOP: file of TEntite;
begin
  Result:=-1;
  MetaFiltre(MyFilter);
  if FNbEntites=0 then Exit;
  AssignFile(FTOP, MyFichierTOP);
  try
    ReWrite(FTop);
    q:=0;
    for i := 1 to FNbEntites - 1 do
    begin
      E:= GetEntite(i);//TableEntites[i];
      if (E.Type_Entite=tgENTRANCE) then Continue;
      if (E.Drawn) then
      begin
        Seek(FTOP, q);
        Write(FTOP, E);
        Inc(q);
      end;
    end;
    Result:=Q;
  finally
    CloseFile(fTOP);
  end;
end;
end.

