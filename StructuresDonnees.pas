unit StructuresDonnees;
// Date: 19/04/2012
// Statut: Fonctionnel
// Les fichiers propriétaires de travail (e.g.: *.top) sont
// alignés sur 4 octets pour compatibilité WinCE

//{$mode delphi}{$H+}
//{$PACKRECORDS 4}
{$INCLUDE CompilationParameters.inc}
interface

uses
  {$IFDEF MSWINDOWS}
    Classes,
    SysUtils,
    Graphics,
    ProjApi447;
  {$ENDIF}
  {$IFDEF LINUX}
    Classes,
    SysUtils,
    Graphics;
  {$ENDIF}

const
  MAX_LENGTH_VISEE_4_TOPOROBOT = 160.00; // longueur maxi de visée pour TOPOROBOT

  TAB                      = #9;
  MAX_LINES_LOG            = 1000; // nb max de lignes de l^'historique des opérations
  MAX_SIZE_PARAM_ARRAY     = 63;
  DEFAULT_FONT_NAME        = 'Arial';
  NAME_FOR_DEFAULT_FONT    = 'font_default';

// couleur du réseau par défaut:
const
  COULEUR_RESEAU_0             = $000080FF;   // orange
  COULEUR_VISEE_ANTENNE        = clGray;      // couleur des visées en antenne

// constantes pour types de galeries
(*
const
  tgDEFAULT   = 0;         // X défaut = galerie fossile
  tgENTRANCE  = 1;
  tgFOSSILE   = 2;         // X
  tgVADOSE    = 3;         // X
  tgENNOYABLE = 4;         // X
  tgSIPHON    = 5;         // X
  tgFIXPOINT  = 6;
  tgSURFACE   = 7;
  tgTUNNEL    = 8;
  tgMINE      = 9;
  tgTYPE_ENTITES_VISEES_ANTENNE = 7; // types visées en antenne
//*)
//----------------------------------
// format standard TOPOROBOT Série + Point
const FMTSERST = '%d.%d';


type
  { Point record and analoga }
   TProjUV = Record U,V:Double End;
   { Point record and analoga }
   TProjXY = TProjUV;
type TTypeDeVisee = (tgDEFAULT,         // X défaut = galerie fossile
                     tgENTRANCE,
                     tgFOSSILE,
                     tgVADOSE,
                     tgENNOYABLE,
                     tgSIPHON,
                     tgFIXPOINT,
                     tgSURFACE,
                     tgTUNNEL,
                     tgMINE,
                     tgTYPE_ENTITES_VISEES_ANTENNE);

// tableaux de strings

type TStringArray   = array[0..MAX_SIZE_PARAM_ARRAY] of string;
type TStringArray2D = array[0..MAX_SIZE_PARAM_ARRAY] of TStringArray;
// pour la sélection du mode vue 3D: GDI ou OpenGL
type TTypeVue3D = (tv3dGDI, tv3dOPENGL);
// pour la sélection dans les dialogues listes
type TModeSelectionListe = (mslEXPE, mslCODE, mslSERIE, mslENTRANCES, mslRESEAUX);

// graphiques 2D: éléments dessinés
type TElementsDrawn = set of (edPolygonals, edStations, edIDStations, edCotation,
                                edWalls, edCrossSections, edFillGalerie,
                                edQuadrilles, edENTRANCES, edANNOTATIONS);
// mode de représentation: par séances, réseaux, gris
type TModeRepresentationGaleries = (rgSEANCES, rgRESEAUX, rgGRAY, rgDEPTH);


// attributs de texte
type TTexteAttributs = record
  FontName     : string;
  FontColor    : TColor;
  BackColor    : TColor;
  FontStyle    : TFontStyles;
  HauteurTexte : double;
  Position     : byte;  // position du texte; cf organisation d'un clavier numérique
                        // pour le point d'accrochage
  AngleRot     : integer;
end;


  type TVector = array of double;
  type TMatrix = array of array of double;
  type TPoint3Df = record
    X:  double;
    Y:  double;
    Z:  double;
  end;
  type TPoint2Df = record
    X:  double;
    Y:  double;
  end;
  type TArrayPoints2Df = array of TPoint2Df;


// couleur TOPOROBOT: couple Index - Couleur, évitant un appel de TPalette256
type TToporobotCouleur = record
  Index: integer;
  Couleur: TColor;
end;

// couple de points de sections transversale
// contenant les coordonnées X, Y des points extremes d'une section transversale
type TPtsSectionTransversale = record
  ParoiGaucheX: double;
  ParoiGaucheY: double;

  ParoiDroiteX: double;
  ParoiDroiteY: double;
end;

  type TIDLitteralPoint = array[0..15] of char;
// format de sauvegarde:
// mtabEXTENDEDTAB =  format étendu XTB, natif de GHTopo
// mtabTOPOROBOT   =  pour compat(err)ibilité TOPOROBOT
type TModeSaveTAB =(mtabEXTENDEDTAB, mtabTOPOROBOT);

// gestion des fins de lignes
// des standards PC, UNIX, Macintache
type TTextFileFormat=(tfWINDOWS, tfUNIX, tfMAC);
//***************************************************************************
//****************************
// Types de données TOPOROBOT
//****************************
// entrées
type TEntrance = record
  eNumEntree: integer;
  eNomEntree: string;
  eXEntree  : double;
  eYEntree  : double;
  eZEntree  : double;
  //eDeltaX   : double;
  //eDeltaY   : double;
  //eDeltaZ   : double;

  eRefSer   : integer;
  eRefSt    : integer;
  eObserv   : string;
end;

// réseau ou secteur spéléologique
// Section -8 du fichier xtb
// Une série ne peut faire partie de plusieurs réseaux.
type TReseau = record
  IdxReseau    : integer;
  ColorReseau  : TColor;
  TypeReseau   : integer; // type de réseau:
  NomReseau    : string;
  ObsReseau    : string;
end;

// expés
type TExpe = record
    IDExpe      : integer;
    JourExpe    : integer;
    MoisExpe    : integer;
    AnneeExpe   : integer;
    Speleometre : String;
    Speleographe: string;
    ModeDecl    : integer;
    Declinaison : double;
    Inclinaison : double;
    Couleur     : Integer;
    Commentaire : string;
end;
// codes
type TCode = record
    IDCode      : integer;
    GradAz      : double;
    GradInc     : double;
    PsiL        : double;
    PsiAz       : double;
    PsiP        : double;
    FactLong    : double; // pour compatibilité ascendante
    AngLimite   : double;
    TypeGalerie : integer;   // type de galerie
    Commentaire : string;
end;
// visées
type TUneVisee = record
    NoVisee   : integer;
    //NoViseeSer: integer;
    Code      : integer;
    Expe      : integer;
    Longueur  : double;
    Azimut    : double;
    Pente     : double;
    LD        : double;
    LG        : double;
    HZ        : double;
    HN        : double;
    Commentaires    : string;
    IDTerrainStation: string;
    TypeVisee : TTypeDeVisee;
    X         : double;
    Y         : double;
    Z         : double;
end;
// stations topo (données de base)
Type TStation = record
   Date                : TDateTime;
   Couleur             : TColor;
   TypeGalerie         : TTypeDeVisee;
   NumPoint            : smallint;    //'Numéro du point
   PtDepart            : smallint;    //        'Départ visée
   PtArrivee           : smallint;    //        'Arrivée visée
   Longueur            : double;      //         'Longueur
   Azimut              : double;      //         'Azimut
   Pente               : double;      //         'Pente
   LD                  : double;      //         'Distance à droite
   LG                  : double;      //         'Distance à gauche
   HZ                  : double;      //         'Hauteur au-dessus
   HN                  : double;      //         'Hauteur du point

   IDTerrainStation    : string;
   Commentaire         : string;
   X                   : double;      //         'X
   Y                   : double;      //         'Y cheminement
   Z                   : double;      //         'Z
   // code et expé
   stCode              : integer;
   stExpe              : integer;
End;
// visées en antenne
type
  pViseeAntenne = ^TViseeAntenne;
  TViseeAntenne = record
   IDViseeAntenne      : integer;
   Reseau              : integer;
   SerieDepart         : integer;
   PtDepart            : integer;
   Code                : integer;
   Expe                : integer;
   IDTerrainStation    : string;

   Longueur            : double;      //         'Longueur
   Azimut              : double;      //         'Azimut
   Pente               : double;      //         'Pente
   Commentaires        : string;
end;

type TDatesTopo = record
  //Displayed: Boolean;
  DateTopo : TDateTime;
end;
type TReseauxTopo = record
  Couleur: TColor;
end;
type TColorGaleries = record
  //Displayed : boolean;
  Color     : TColor;
end;

//***************************************************************************
// structure du fichier propriétaire de travail *.top
// généré lors du calcul
type TEntite = record

   //UID_Entite        : integer;                 // Couleur ID unique de l'entité
   ColorEntite       : TColor;
   Drawn             : boolean;                 // dessinée ?
   Type_Entite       : TTypeDeVisee;            // Type d'entité
   DateLeve          : TDateTime;
   // serie et point
   Entite_Serie      : integer;                 // Série
   Entite_Station    : integer;                 // Station
   Une_Station_1_X   : double;                  // Extrémités des visées
   Une_Station_1_Y   : double;
   Une_Station_1_Z   : double;

   Une_Station_2_X   : double;                // Extrémités des visées
   Une_Station_2_Y   : double;
   Une_Station_2_Z   : double;
   //LongVisee         : double;
   X1PD                 : double;      //         'X point droit contour
   Y1PD                 : double;      //         'Y point gauche contour
   X1PG                 : double;      //         'X point droit contour
   Y1PG                 : double;      //         'Y point gauche contour

   X2PD                 : double;      //         'X point droit contour
   Y2PD                 : double;      //         'Y point gauche contour
   X2PG                 : double;      //         'X point droit contour
   Y2PG                 : double;      //         'Y point gauche contour

   Z1PH                 : double;      //         'Z point haut contour
   Z1PB                 : double;      //         'Z point bas contour
   Z2PH                 : double;      //         'Z point haut contour
   Z2PB                 : double;      //         'Z point bas contour

   ID_Litteral_Pt       : TIDLitteralPoint;      // ID alphanum. de l'entité
   // réseaux
   IdxReseau            : integer;
   ColorReseau          : TColor;

   // serie et point
   //eSerie  : integer;
   //eStation: integer;
   // code et expé
   eCode   : integer;
   eExpe   : integer;
end;
// coordonnées WGS84
type TPointWGS84 = record
  Longitude : double;
  Latitude  : double;
end;
// angle
type TUniteAngles = (uaRADIANS, uaDEGRES, uaGRADES);
// systèmes de coordonnées français
type TSystemeCoordonnees = (scLAMBERT1, scLAMBERT2, scLAMBERT2E, scLAMBERT3,
                            scLAMBERT4,
                            scLAMBERT93,
                            scUTM);
// modes d'exportation pour logiciels de carto
type TOutputFormatGIS = (gisGHCAVEDRAW, gisKML, gisGPX, gisOSM, gisCARTO_EXPLOREUR, gisMEMORY_MAP);
type TPolyMode = (tpmENTETE_POLY, tpmSTART_POLY, tpmPOINT_POLY, tpmEND_POLY);

// fonctions spécifiques
function EmptyVisee(const C: string): TUneVisee;

// annotations
type TAnnotation = record
  IDAnnotation  : integer;
  // Mode de positionnement:
  // 0 = Texte positionné aux coordonnées X, Y et Z
  // 1 = Texte positionné par rapport à une station topo
  // (X, Y, Z devient le décalage par rapport au point topo)
  ModePosition  : byte;
  // Station de référence
  BaseRefSer    : integer;
  BaseRefSt     : integer;
  // Position du texte par rapport au point de base
  // 0 = défaut
  // 7        8        9
  // 4  Lorem 5 Ipsum  6
  // 1        2        3

  PosPtDeBase   : byte;

  // coordonnées du point de référence du texte (ou décalage)
  X             : Double;
  Y             : Double;
  Z             : Double;
  // décalage
  dx            : double;
  dy            : double;
  dz            : double;

  //Color         : TColor;
  FontColor     : TColor;  // Couleur du texte
  MaxLength     : integer; // Longueur maxi de la chaîne affichée

  FontName      : String;  // fonte
  FontSize      : Byte;
  //Accrochage    : Byte;    // alignement
  FontBold      : boolean;
  FontItalic    : boolean;
  FontUnderline : boolean;

  Displayed     : boolean; // texte affiché ?
  Reserved      : integer; // champ réservé
  //------------------------------
  Caption       : String;  // texte
  //------------------------------
  BoundingBox   : TRect;
end;


// couleurs 24 bits
type TColor3b = record
  R: byte;
  G: byte;
  B: byte;
end;
// couleurs 32 bits
type TColorRGBA = record
  R: byte;
  G: byte;
  B: byte;
  A: byte;
end;

type TPoint3DVRML = record
  X: Int64;
  Y: Int64;
  Z: Int64;
end;

// Couleurs Macintosh
type TMacintoshColor = record
  R : word;
  G : word;
  B : word;
end;


// pour stats spéléometriques
type TVentilationSpeleometrie = record
  Fossiles  : double;
  Vadoses   : double;
  Ennoyables: double;
  Siphons   : double;
  Tunnels   : double;
  Filons    : double;
  Speciaux  : double;
end;
type TTableauVentilationSpeleometrie = array of TVentilationSpeleometrie;
// type rectangle
type TRect2Df = record
  X1: double;
  Y1: double;
  X2: double;
  Y2: double;
end;


// procédure d'affichage de la progression d'une opération
type TProcDisplayProgression = procedure (const Done, Starting, Ending: Int64;  const Etape: string) of object;

// types pour UnitClasseMaillages (MNT terrain)
type TGLColor = record
  R : double;
  G : double;
  B : double;
  A : double;
end;

type TGridMNTArray = array of array of Double;
type TTypeMaillage = (tmUNKNOWN, tmREGULAR_GRID, tmTRIANGLES);
type TMaillageModeDessin = (M3D_NONE, M3D_WIRE_FRAME, M3D_MESH, M3D_BLEND, M3D_MIXTE);
//type TOpenGLColor = array[0..3] of GLFloat;
type TIsoHypse = record
  Cote: double;
  Color: TColor;
end;
type TMaillageVertex = record
  ID         : integer;
  X          : double;
  Y          : double;
  Z          : double;
  NormX      : double;
  NormY      : double;
  NormZ      : double;
  Norme      : double;
end;
type TTriangleABC = record
   Numero  :  integer;
   PointA  :  integer;
   PointB  :  integer;
   PointC  :  integer;
   //TriangleVoisinAB: integer;
   //TriangleVoisinBC: integer;
   //TriangleVoisinCA: integer;
end;
// Paramètres EPSG
type TParametresEPSG = record
  CodeEPSG   : integer;
  Parameters : string;
  Comments   : string;
  {$IFDEF MSWINDOWS}
     ProjPointer: PProjPJ;
  {$ENDIF}
  {$IFDEF LINUX}
  {$ENDIF}


end;

// onglets pour les vues 2D
const NB_MAX_ONGLETS = 10;
type  TModesTravail  = (mtREADY, mtZOOM, mtPANVUE, mtDISTANCE, mtMFZONE);
type  TQdrType       = (qtGRID, qtCROSS, qtPOINTS);

type TVue2DParams  = record
  ongName: string;
  ongX1  : double;
  ongY1  : double;
  ongX2  : double;
  ongY2  : double;
  ongVueFiltres: string;
  ongBackGround: TColor;
  ongQdrSpc    : double;
  ongQdrType   : TQdrType;
  ongQdrColor   : TColor;
  ongElementsDrawn: TElementsDrawn;
end;
type TArrOngletsParams = array[0..NB_MAX_ONGLETS] of TVue2DParams;

// mode de fonctionnement des navigateurs de la BDD
type TModeBDD = (mbddDISABLED, mbddENTRANCES, mbddRESEAUX, mbddCODES, mbddEXPES, mbddSERIES, mbddANTENNES);

// procédures de conversions GCS->Ecran
type TProcGCSToSRC = function(const PM: TPoint2Df): TPoint of Object;

// export KML: Références
const
  KML_OPENGIS_WEBSITE     = 'http://www.opengis.net/kml/2.2';
  KML_GOOGLE_KML_WEBSITE  = 'http://www.google.com/kml/ext/2.2';
  W3C_W3_WEBSITE          = 'http://www.w3.org/2005/Atom';

  W3C_XML_SCHEMA_WEBSITE  = 'http://www.w3.org/2001/XMLSchema-instance';
  GPX_TOPOGRAPHIX_WEBSITE = 'http://www.topografix.com/GPX/1/0';

implementation
// visée vide
function EmptyVisee(const C: string): TUneVisee;
begin
  with Result do begin
    TypeVisee := tgDEFAULT;
    IDTerrainStation :='';
    NoVisee:=0;
    Code   :=1;
    Expe   :=1;
    Longueur:= 0.00;
    Azimut  := 0.00;
    Pente   := 0.00;
    LD      := 0.00;
    LG      := 0.00;
    HZ      := 0.00;
    HN      := 0.00;
    Commentaires  :=  C;
  end;
end;





end.

