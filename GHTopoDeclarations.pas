unit GHTopoDeclarations;
{$INCLUDE SelectionLangue.inc}
{$IFDEF FREEPASCAL}
  {$mode DELPHI}{$H+}
  {$PACKRECORDS 1}
{$ELSE}
  {$A-}
{$ENDIF}

interface
uses
  SysUtils, Classes,
  {$IFDEF MSWINDOWS}
    Windows,
  {$ENDIF}
  Graphics;

const
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
const
  tgDEFAULT   = 0;         // défaut = galerie fossile
  tgENTRANCE  = 1;
  tgFOSSILE   = 2;
  tgVADOSE    = 3;
  tgENNOYABLE = 4;
  tgSIPHON    = 5;
  tgFIXPOINT  = 6;
  tgSURFACE   = 7;
  tgTUNNEL    = 8;
  tgMINE      = 9;
  tgTYPE_ENTITES_VISEES_ANTENNE = 7; //1 + tgMINE;          // types visées en antenne
//----------------------------------
const FMTSERST = '%d.%d';


const DFT_SPELEOMETRE  = 'CASSOU JP';
const DFT_SPELEOGRAPHE = 'CASSOU JP';
const DFT_REPORTER     = 'SiliconCavings';
//************************************************************************************************
type TModeSaveTAB =(mtabEXTENDEDTAB,mtabTOPOROBOT);
type TTextFileFormat=(tfWINDOWS, tfUNIX, tfMAC);
// mode de représentation: par séances, réseaux, gris
type TModeRepresentationGaleries = (rgSEANCES, rgRESEAUX, rgGRAY, rgDEPTH);
//
type TIDLitteralPoint = array[0..15] of char;



//****************************
// Types de données TOPOROBOT
//****************************
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
// stations topo (données de base)
Type TStation = record
   Date                : TDateTime;
   Couleur             : TColor;
   TypeGalerie         : byte;
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

// visées
type
  pUneVisee = ^TUneVisee;
  TUneVisee = record
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
    TypeGalerie     : byte;
    X         : double;
    Y         : double;
    Z         : double;
end;
// visées en antenne
type
  pViseeAntenne = ^TViseeAntenne;
  TViseeAntenne = record
   IDViseeAntenne      : integer;
   Reseau              : integer;
   SerieDepart         : smallint;
   PtDepart            : smallint;
   Code                : integer;
   Expe                : integer;
   IDTerrainStation    : string;

   Longueur            : double;      //         'Longueur
   Azimut              : double;      //         'Azimut
   Pente               : double;      //         'Pente
   Commentaires        : string;
end;
// entrées
type TEntrance = record
  eNumEntree: integer;
  eNomEntree: string;
  eXEntree  : double;
  eYEntree  : double;
  eZEntree  : double;
  eDeltaX   : double;
  eDeltaY   : double;
  eDeltaZ   : double;

  eRefSer   : integer;
  eRefSt    : integer;
  eObserv   : string;
end;

// expés
type
  pExpe = ^TExpe;
  TExpe = record
    IDExpe      : integer;
    JourExpe    : byte;
    MoisExpe    : byte;
    AnneeExpe   : word;
    Speleometre : String;
    Speleographe: string;
    ModeDecl    : byte;
    Declinaison : double;
    Inclinaison : double;
    Couleur     : Integer;
    Commentaire : string;
end;
// codes
type
  pCode = ^TCode;
  TCode = record
    IDCode      : integer;
    GradAz      : double;
    GradInc     : double;
    PsiL        : double;
    PsiAz       : double;
    PsiP        : double;
    FactLong    : double; // pour compatibilité ascendante
    AngLimite   : double;
    TypeGalerie : byte;   // type de galerie
    Commentaire : string;
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
// descro des couches
type TLayer = record
  Name    :  string;
  Color   : TColor;
end;
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
// modes de travail des sélecteurs de listes
type TModeSelectionListes = (mslENTREES, mslRESEAUX,
                             mslCODES, mslEXPES, mslSERIES);



//******************************************************************************
// Types de données pour les visualisateurs graphiques
//******************************************************************************


// Entités
type TEntite = record
   //UID_Entite        : integer;                 // Couleur ID unique de l'entité
   ColorEntite       : TColor;
   Drawn             : boolean;                 // dessinée ?
   Type_Entite       : byte;                    // Type d'entité
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

//----------------------------------------------
// Classe pour la table des expés
type TTableExpes = class(TList);
// Classe pour la table des séries
type TTableSeries = class(TList);
//Classe pour la table des codes
type TTableCodes = class(TList);

// Fonctions spécifiques GHTopo
//***************************************************************************
// visée vide
function EmptyVisee(const C: string): TUneVisee;
// Interprétation d'une annotation
//LLANFAIRPWLLGWYNGYLLGOGERYCHWYRNDROBWLLLLANTISILIOGOGOGOCH
    // Tronque le texte à NbC caractères
    // DONE: Factoriser ce code SVP !
//function LLANFAIR_PG(const STR: string; const NbC: integer; const Alt: double): string;
function LLANFAIR_PG(const STR: string;
                     const NbC: integer;
                     const Alt: double;
                     const Ser, St: integer): string;

implementation
// easter-egg = llanfair_pg
const
  rsLLANFAIR_PG = 'Llanfairpwllgwyngyllgogerychwyrndrobwllllantysiliogogogoch';


//------------------------------------------------------------------------------
// visée vide
// Paramètre: Commentaire
function EmptyVisee(const C: string): TUneVisee;
begin
  with Result do begin
    TypeGalerie := tgDEFAULT;
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

// -----------------------------------------------------------------------------
// Interprétation d'une annotation
//LLANFAIRPWLLGWYNGYLLGOGERYCHWYRNDROBWLLLLANTISILIOGOGOGOCH
// DONE: Factoriser ce code
function LLANFAIR_PG(const STR: string;
                     const NbC: integer;
                     const Alt: double;
                     const Ser, St: integer): string;
var
  ppp : integer;
  S666: string;
begin
  S666:=STR;
  // Interprétation de certains codes
  // %Z => remplacement de ce code par l'altitude
  // %S => Affichage de la station d'accrochage
  // %L => affichage du libellé de la station d'accrochage

  PPP := Pos('%z', LowerCase(S666));
  if (PPP > 0) then begin
    // On efface le code
    System.Delete(S666, PPP, 2);
    // puis on insère l'altitude
    System.Insert(Format('Alt %.0n m',[Alt]), S666, PPP);
  end;
  PPP := Pos('%s', LowerCase(S666));
  if (PPP > 0) then begin
    // On efface le code
    System.Delete(S666, PPP, 2);
    // puis on insère l'altitude
    System.Insert(Format('[%d.%d]',[Ser, St]), S666, PPP);
  end;
  // easter egg
  PPP := Pos('%l', LowerCase(S666));
  if (PPP > 0) then begin
    // On efface le code
    System.Delete(S666, PPP, 2);
    // puis on insère le texte
    System.Insert(Format('Bienvenue à %s',[UpperCase(rsLLANFAIR_PG)]), S666, PPP);
  end;

  //---------------
  try
    if (Length(S666)=0) or (NbC=-1) then
      begin Result:=S666; exit; end;
    if Length(STR)>NbC then
      Result:=Copy(S666, 1, NbC)
    else
      Result:=S666;
  except
    Result:=S666;
  end;
end;



end.
