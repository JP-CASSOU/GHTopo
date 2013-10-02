unit ToporobotClasses2012;
//****************************************************
// Projet     : GHTopo
// Modules    : Classe pour gestion des données TOPOROBOT
// Licence    : General Public License - WITHOUT WARRANTY
// Auteur     : JP CASSOU
// OS         : Windows 9x - Linux
// Langage    : Free-Pascal 2.6.x sous Lazarus
//-----------------------------------------------------
// Eléments du cahier des charges:
// - Les données sont stockées sous format texte
// - Gestion de la base entièrement en mémoire sous forme d'une arborescence
// - Les listes simples sont des TList dont les membres sont des 'record'.
// - Les séries sont des objets autonomes stockés dans une liste de type TList
//   L'accès aux membres de l'objet se fait uniquement par getters et setters
// - La création de nouveaux items se fait par des fonctions
//   de la forme AddObjet(const MonObjet)
// - Le processus d'édition d'un objet est le suivant:
//   1. Extraction d'un objet (expé, code, entrée, série, antenne, réseau) depuis sa liste
//      via une fonction de la forme GetObjet(const Idx: integer): TMonObjet
//   2. Utilisation de l'objet et modification de ses propriétés
//   3. Mise à jour de l'objet dans sa liste
//      via une fonction de la forme PutObjet(const Idx: integer; const Obj: TMonObjet);
// - Support du format Toporobot TAB en lecture-écriture
// - Support du format XTB ( = format TAB avec sections additionnelles)
// - Lecture des fichiers Text (format considéré comme obsolète et peu robuste.
//                              L'écriture dans ce format ne figure pas dans le CdC)
// - Evolutivité vers un nouveau format de type XML
// - Exportation vers d'autres logiciels
//   * Visual Topo (indispensable) - La méthode TOPOROBOT est incompatible avec la notation alphanumérique
//   * Therion
//-----------------------------------------------------
// Module     : ToporobotClasses:
//            : Structure des données d'un dossier Toporobot
// Compatibilité intégrale avec le standard Toporobot 1994.
// 22/08/2012 : Refonte du noyau de GHTopo (ToporobotClasses)
//              en séparant le SGBD et le code de calcul
// 24/08/2012 : Opérationnel
// 14/02/2013 : Réacculturation au code de GHTopo. Petites corrections
//              Préparation de fonctions de recherche plein texte dans les listes
// 03/05/2013 : Réorganisation du fichier de paramètres de compilation
// 13/06/2013 : Quelques modifications dont ExtractFileNameOnly
// 14/06/2013 : Implantation du nouveau format GHTopo XML
//-----------------------------------------------------
//                     Lecture
//                     |  Ajout
//                     |  |  Modif
//                     |  |  |  Suppr
//                     |  |  |  |  Count
//                     |  |  |  |  |  Tri
// Table entrées       X  X  X  X  X  0
// Table réseaux       X  X  X  X  X  0
// Table expés         X  X  X  X  X  X
// Table codes         X  X  X  X  X  X
// Table antennes      X  X  X  X  X  0
// Table séries        X  X  X  X  X  X
//        |-Stations   X  X  X  O  X  0
//
// LoadFromTab()       OK
// LoadFichierText     OK   /!\ Les commentaires des séries et stations sont ignorés
// SaveToFile()        OK
// Export THERION      ...   /!\ A valider
// Export Visual Topo: X     /!\ Visual Topo est très peu tolérant.


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
  StructuresDonnees,
  ObjetSerie,
  Common,
  FileUtil,
  dateutils,
  UnitClassPalette,
  math,
  Classes, SysUtils, Graphics,
  DOM, XMLWrite, XMLRead;

// LISTES SIMPLES
// Classe pour la table des entrées
type TTableEntrees = class(TList);
// classe pour la table des réseaux
type TTableReseaux = class(TList);
// Classe pour la table des expés
type TTableExpes = class(TList);
//Classe pour la table des codes
type TTableCodes = class(TList);
//Table des Noeuds
//type TTableNoeuds = array of TNoeud;
// Classe des visées en antenne
type TTableViseesAntenne = class(TList);

// REFONTE DU CONTENEUR TTOPOROBOTSTRUCTURE
//------------------------------------------------------------------------------
type

{ TToporobotStructure2012 }

 TToporobotStructure2012 = class
    constructor Create;
    //destructor Free;
  private
    // général
    FNomEtude     : string;
    FCommentaireEtude: string;
    FDatabaseName : string;
    FSystemeDeCoordonneesEPSG : string;
    // coordonnées et station de référence
    FDefaultCoordX : double;
    FDefaultCoordY : double;
    FDefaultCoordZ : double;
    FRefSer        : integer;
    FRefPt         : integer;
    // listes simples
    FTableEntrees : TTableEntrees;
    FTableReseaux : TTableReseaux;
    FTableExpes   : TTableExpes;
    FTableCodes   : TTableCodes;
    FTableViseesAntenne: TTableViseesAntenne;

    // liste des séries
    FListeDesSeries : TList;

  public
    procedure ReInitialiser(const DoCreateItemsZero: boolean);
    procedure Finaliser;
    procedure ViderTablesSimples;
    // section General
    procedure SetNomEtude(const S: string);
    procedure SetCommentairesEtude(const S: string);
    procedure SetDatabaseName(const S: string);

    function  GetNomEtude: string;
    function  GetCommentairesEtude: string;
    procedure SetRefSeriePoint(const Ser, Pt: integer);
    // gestion des entrées
    procedure AddEntree(const AEntree: TEntrance);
    function  GetEntree(const NumEntree: integer): TEntrance;
    procedure PutEntree(const NoEntree: integer; const AEntree: TEntrance);
    function  RemoveEntree(const Idx: integer): boolean;
    function GetNbEntrees: integer;
    function  FindIdxEntranceByText(const S: string): integer;
    // gestion des réseaux
    procedure AddReseau(const AReseau: TReseau);
    function  GetReseau(const NumReseau: integer): TReseau;
    procedure PutReseau(const NoReseau: integer;
                        const AReseau: TReseau);
    function  RemoveReseau(const Idx: integer): boolean;
    function GetNbReseaux: integer;
    function  FindIdxReseauByText(const S: string): boolean;
    // gestion des expés
    procedure AddExpe(const LaExpe: TExpe);
    function  GetExpe(const NumExpe: integer): TExpe;
    procedure PutExpe(const NoExpe: integer; const LaExpe: TExpe);
    function  RemoveExpe(const NoExpe: integer): boolean;
    function  GetExpeByIndex(const Idx: integer): TExpe;
    function  GetNbExpes: integer;
    function  FindIdxExpeByText(const S: string): integer;

    // gestion des codes
    procedure AddCode(const LeCode: TCode);
    function  RemoveCode(const NoCode: integer): boolean;
    procedure PutCode(const NoCode: integer; const LeCode: TCode);
    function  GetCode(const NoCode: integer): TCode;
    function  GetCodeByIndex(const Idx: integer): TCode;
    function GetNbCodes: integer;
    function  FindIdxCodeByText(const S: string): integer;
    // gestion des antennes
    procedure AddViseeAntenne(const VA: TViseeAntenne);
    function  GetViseeAntenne(const No: integer): TViseeAntenne;
    procedure PutViseeAntenne(const No: integer; const VA: TViseeAntenne);
    function  RemoveViseeAntenne(const No: integer): boolean;
    function  GetNbAntennes: integer;

    // gestion des séries
    procedure ClearListeSeries;
    procedure CreateNewSerie(const NomSerie: string;
                             const SerieDep, PtDep, SerieArr, PtArr: integer;
                             const Reseau, Chances, Obstacles: integer;
                             const Couleur: TColor;
                             const Raideur: double;
                             const Obs: string);
    function  GetNbSeries: integer;
    procedure AddSerie(const S: TObjSerie);
    function  GetSerie(const Idx: integer): TObjSerie;
    procedure PutSerie(const Idx: integer; const S: TObjSerie);
    procedure PutLastSerie(const S: TObjSerie);
    function  RemoveSerie(const Idx: integer): boolean;
    function  GetLastSerie: TObjSerie;
    function  FindIdxSerieByText(const S: string): integer;
    // procédures de tri
    procedure SortExpes;
    procedure SortCodes;
    procedure SortSeries;
    // chargement document topo
    function  LoadFichierTab(const FichierTAB: string): integer;
     // charger un fichier Text
    function  LoadFichierText(const FichierText: string): integer;
    function  LoadFromXML(const FichierXML: string): integer;
    // sauvegarde document
    procedure SaveToFile(const FichierTAB: string;
                         const ModeSaveTAB: TModeSaveTAB;
                         const TextFileFormat: TTextFileFormat);
    procedure SaveToXML(const FichierXML: string);
    // systèmes de coordonnées
    function  GetSystemeCoordonnees: string;
    procedure SetSystemeCoordonnees(const CodeEPSG: string);
    // coordonnées par défaut
    function GetDatabaseName: string;
    procedure SetDefaultCoords(const X, Y, Z: double);

    function GetDefaultCoordX: double;
    function GetDefaultCoordY: double;
    function GetDefaultCoordZ: double;

    // fonctions de recherche
    function GetIdxSerie(const NumeroSerie: integer): integer;
    function FindPtTopo(var Cle: string; var Sr, Pt: integer): boolean;

    function GetRefPoint: integer;
    function GetRefSerie: integer;
    // infos générales sur les données de la cavité
    function GetCaveInfoSeries(const FichierTXT: string): integer;
    // export vers VisualTopo et Therion
    procedure ExportVersTherion(const FichierTH: string);
    procedure ExportVisualTopo(const FichierTRO: string);
    // utilitaires
    procedure CalculerDeclinaisonsMagnetiques(const DoReplaceManualValues: boolean);
    // check des valeurs
    function  CheckAzimutByIndex(const Idx: integer; const QAz: double; out MsgErr: string): boolean;
    function  CheckPenteByIndex(const Idx: integer; const QP: double; out MsgErr: string): boolean;
    // test d'existence des éléments
    function  ExistsEntrance(const Idx: integer): boolean;
    function  ExistsReseau(const Idx: integer): boolean;
    function  ExistsCode(const Idx: integer): boolean;
    function  ExistsExpe(const Idx: integer): boolean;
    function  ExistsSerie(const Idx: integer): boolean;

end;
//------------------------------------------------------------------

implementation
uses
  // pour la déclinaison magnétique
  {$IFDEF MSWINDOWS}
    UnitWrapperDeclimag;
  {$ENDIF}

  {$IFDEF LINUX}
    UnitWrapperDeclimagLinux;
  {$ENDIF}
  //********************
  // Inclusion des constantes GtxKey
  {$INCLUDE GTX_Keys.inc}

// Routines internes non incorporées dans les objets
function SortCodesCriteria(Item1, Item2: Pointer): Integer;
var
  E1, E2: ^TCode;
begin
  E1:=Item1;
  E2:=Item2;
  if (E1.IDCode<E2.IDCode) then
    Result:=-1
  else if (E1.IDCode=E2.IDCode) then
    Result:=0
  else
    Result:=1;
end;
// Trier les expés par ordre croissant
function SortExpesCriteria(Item1, Item2: Pointer): Integer;
var
  E1, E2: ^TExpe;
begin
  E1:=Item1;
  E2:=Item2;
  if (E1.IDExpe<E2.IDExpe) then
    Result:=-1
  else if (E1.IDExpe=E2.IDExpe) then
    Result:=0
  else
    Result:=1;
end;
// Trier les séries par ordre croissant
function SortSerieCriteria(Item1, Item2: Pointer): Integer;
var
  E1, E2: ^TObjSerie;
begin
  E1:=Item1;
  E2:=Item2;
  if (E1.GetIndexSerie < E2.GetIndexSerie) then
    Result:=-1
  else if (E1.GetIndexSerie = E2.GetIndexSerie) then
    Result:=0
  else
    Result:=1;
end;

// TToporobotStructure2012
constructor TToporobotStructure2012.Create;
begin
  inherited;
  AfficherMessage(Format('%s.Create', [self.ClassName]));
  FListeDesSeries := TList.Create;
  FListeDesSeries.Clear;
  // tables simples
  FTableEntrees := TTableEntrees.Create;
  FTableReseaux := TTableReseaux.Create;
  FTableExpes   := TTableExpes.Create;
  FTableCodes   := TTableCodes.Create;
  FTableViseesAntenne := TTableViseesAntenne.Create;
  AfficherMessage('-- Tables created');
end;

procedure TToporobotStructure2012.ReInitialiser(const DoCreateItemsZero: boolean);
var
  UneEntree: TEntrance;
  UneExpe: TExpe;
  UnCode: TCode;
  UnReseau: TReseau;
  UneViseeAntenne: TViseeAntenne;
  UneSerie: TObjSerie;
begin
   ClearListeSeries;
   ViderTablesSimples;
   if (Not DoCreateItemsZero) then Exit;

   // Réseau 0
   with UnReseau do begin
     IdxReseau   := 0;
     ColorReseau := COULEUR_RESEAU_0;
     TypeReseau  := 0;
     NomReseau   := rsMAIN_NETWORK;
     ObsReseau   := '';
   end;
   AddReseau(UnReseau);
   // Expé 0
   with UneExpe do begin
      IDExpe:=0;
      JourExpe      := 01;
      MoisExpe      := 01;
      AnneeExpe     := 00;
      Speleometre   := '';             // spéléomètre
      Speleographe  := '';             // spéléographe
      ModeDecl      := 0;   // déclinaison auto ?
      Declinaison   := 0.00; // déclinaison
      Inclinaison   := 0.00; // correction clino x10
      Couleur       := 0;  // couleur
      Commentaire   := rsFORFIXEDPOINTS;            // commentaire
   end;
   AddExpe(UneExpe);
   // Code 0
   with UnCode do begin
     IDCode:=0;
     GradAz:=400.00;
     GradInc:=400.00;
     PsiL:=0.00;
     PsiAz:=0.00;
     PsiP:=0.00;
     FactLong:=1.00;
     AngLimite:=0.00;
     TypeGalerie:=0;
     Commentaire:=rsFORFIXEDPOINTS;
   end;
   AddCode(UnCode);
   // Antennes
   with UneViseeAntenne do begin
      IDViseeAntenne   := 0;
      Reseau           := 0;
      SerieDepart      := 1;
      PtDepart         := 0;
      Code             := 1;
      Expe             := 1;
      IDTerrainStation := '';
      Longueur         := 0.001;
      Azimut           := 0.00;
      Pente            := 0.00;
      Commentaires     := '';
    end;
    AddViseeAntenne(UneViseeAntenne);
    // série
    //Serie 0
    UneSerie := TObjSerie.Create;
    with UneSerie do begin
      ClearSerie;
      SetNoReseau(0);
      SetNomObsSerie(rsSERIE_INITIALISATION, '');
      SetIndexSerie(0);
      SetSeriePtExtremites(0,0,1,0);
      SetChanceObstacle(0,0);
      SetRaideur(1.00);
      AddVisee(EmptyVisee('Point 1/0'));
    end;
    AddSerie(UneSerie);
end;

procedure TToporobotStructure2012.ViderTablesSimples;
var i: integer;
begin
  AfficherMessage('');
  //AfficherMessage('Vidage des tables simples');
  try
    if (FTableExpes.Count > 0)           then for i:=0 to FTableExpes.Count - 1        do Dispose(FTableExpes.Items[i]); // 2
    if (FTableCodes.Count > 0)           then for i:=0 to FTableCodes.Count - 1        do Dispose(FTableCodes.Items[i]); //3
    if (FTableEntrees.Count > 0)         then for i:=0 to FTableEntrees.Count -1       do Dispose(FTableEntrees.Items[i]); // 5
    if (FTableReseaux.Count > 0)         then for i:=0 to FTableReseaux.Count -1       do Dispose(FTableReseaux.Items[i]); // 6
    if (FTableViseesAntenne.Count > 0)   then for i:=0 to FTableViseesAntenne.Count -1 do Dispose(FTableViseesAntenne.Items[i]); // 7

    FTableCodes.Clear;
    FTableEntrees.Clear;
    FTableExpes.Clear;
    FTableReseaux.Clear;
    FTableViseesAntenne.Clear;
    AfficherMessage('Vidage des tables simples OK');
  except
    AfficherMessage('Echec purge des tables simples');
  end;

end;

// A invoquer avant libération
// SIGSEGV inexpliqué si implanté dans le destructeur
procedure TToporobotStructure2012.Finaliser;
begin
  ViderTablesSimples;
  ClearListeSeries;
  AfficherMessage(' --- Series table cleared');
  FListeDesSeries.Free;
  // libération des tables simples


  FTableEntrees.Free;
  AfficherMessage(' --- Entrances table cleared');
  FTableReseaux.Free;
  AfficherMessage(' --- Networks table cleared');
  FTableExpes.Free;
  AfficherMessage(' --- Trips table cleared');
  FTableCodes.Free;
  AfficherMessage(' --- Codes table cleared');
  FTableViseesAntenne.Free;
  AfficherMessage(' --- Antennas table cleared');
end;


function TToporobotStructure2012.GetDefaultCoordX: double;
begin
  Result := FDefaultCoordX;
end;

function TToporobotStructure2012.GetDefaultCoordY: double;
begin
  Result := FDefaultCoordY;
end;

function TToporobotStructure2012.GetDefaultCoordZ: double;
begin
  Result := FDefaultCoordZ;
end;

procedure TToporobotStructure2012.SetDefaultCoords(const X, Y, Z: double);
begin
  FDefaultCoordX := X;
  FDefaultCoordY := Y;
  FDefaultCoordZ := Z;
end;

function TToporobotStructure2012.GetRefSerie: integer;
begin
  Result := FRefSer;
end;

function TToporobotStructure2012.GetRefPoint: integer;
begin
  Result:= FRefPt;
end;

procedure TToporobotStructure2012.SetRefSeriePoint(const Ser, Pt: integer);
begin
  FRefSer := Ser;
  FRefPt  := Pt;
end;

procedure TToporobotStructure2012.SetNomEtude(const S: string);
begin
  FNomEtude:=S;
end;

procedure TToporobotStructure2012.SetCommentairesEtude(const S: string);
begin
  FCommentaireEtude:=S;
end;

procedure TToporobotStructure2012.SetDatabaseName(const S: string);
begin
  FDatabaseName:=S;
end;

function TToporobotStructure2012.GetNomEtude: string;
begin
  Result := FNomEtude;
end;

function TToporobotStructure2012.GetCommentairesEtude: string;
begin
  Result := FCommentaireEtude;
end;

function TToporobotStructure2012.GetDatabaseName: string;
begin
  Result := FDatabaseName;
end;

(* TToporobotStructure2012 *)

//**************************
// système de coordonnées EPSG
function TToporobotStructure2012.GetSystemeCoordonnees: string;
begin
  Result := FSystemeDeCoordonneesEPSG;
end;

procedure TToporobotStructure2012.SetSystemeCoordonnees(const CodeEPSG: string);
begin
  FSystemeDeCoordonneesEPSG := CodeEPSG;
end;

//**************************

procedure TToporobotStructure2012.ClearListeSeries;
var
  ii: Integer;
begin
  AfficherMessage(Format('-- %s.ClearListeSeries()',[self.ClassName]));
  if (FListeDesSeries.Count > 0) then
  begin
    try
      for ii := 0 to FListeDesSeries.Count - 1 do
      begin
        //AfficherMessage(Format('%d/%d',[1+ii, FListeDesSeries.Count]));
        //TObjSerie(FListeDesSeries.Items[ii]).Free;
        Dispose(FListeDesSeries.Items[ii]);
      end;
    except

    end;
  end;
  FListeDesSeries.Clear;
  AfficherMessage(Format('-- %s.ClearListeSeries() OK',[self.ClassName]));
  AfficherMessage('');


end;

procedure TToporobotStructure2012.CreateNewSerie(const NomSerie: string;
  const SerieDep, PtDep, SerieArr, PtArr: integer; const Reseau, Chances,
  Obstacles: integer; const Couleur: TColor; const Raideur: double;
  const Obs: string);
var
  S: TObjSerie;
begin
  S := TObjSerie.Create;
  S.Clear;
  S.SetIndexSerie(101); // TODO: Pquoi 101 ?
  S.SetSeriePtExtremites(SerieDep, PtDep, SerieArr, PtArr);
  S.SetNomSerie(NomSerie);
  S.SetNoReseau(Reseau);
  S.SetCouleur(Couleur);
  S.SetChanceObstacle(Chances, Obstacles);
  S.SetRaideur(Raideur);
  S.SetObsSerie(Obs);
  self.AddSerie(S);
end;

function TToporobotStructure2012.GetNbSeries: integer;
begin
  Result := FListeDesSeries.Count;
end;

procedure TToporobotStructure2012.AddSerie(const S: TObjSerie);
var
  pS: ^TObjSerie;
begin
  New(pS);
  pS^ := S;
  FListeDesSeries.Add(pS);
end;

function TToporobotStructure2012.GetSerie(const Idx: integer): TObjSerie;
var
  pS: ^TObjSerie;
begin
  pS := FListeDesSeries.Items[Idx];
  Result := pS^;
end;

procedure TToporobotStructure2012.PutSerie(const Idx: integer; const S: TObjSerie);
var
  pS: ^TObjSerie;
begin
  pS := FListeDesSeries.Items[Idx]; // Récupérer le pointeur ^^
  pS^ := S;
  FListeDesSeries.Items[Idx] := pS;
end;

procedure TToporobotStructure2012.PutLastSerie(const S: TObjSerie);
var
  WU: Integer;
begin
  WU := FListeDesSeries.Count - 1;
  PutSerie(WU, S);
end;

function TToporobotStructure2012.RemoveSerie(const Idx: integer): boolean;
var
  OS: TObjSerie;
begin
  Result := False;
  try
    OS := GetSerie(Idx);
    OS.ClearSerie;
    OS.Free;
    Dispose(FListeDesSeries.Items[Idx]);
    FListeDesSeries.Delete(Idx);
    Result := True;
  except

  end;

end;

function TToporobotStructure2012.GetLastSerie: TObjSerie;
begin
  Result := GetSerie(FListeDesSeries.Count - 1);
end;

function TToporobotStructure2012.FindIdxSerieByText(const S: string): integer;
begin
  Result := -1;
end;

procedure TToporobotStructure2012.SortExpes;
begin
  AfficherMessage(Format('%s.SortExpes: Tri des seances',[ClassName]));
  FTableExpes.Sort(SortExpesCriteria);
end;

procedure TToporobotStructure2012.SortCodes;
begin
  AfficherMessage(Format('%s.SortCodes: Tri des codes',[ClassName]));
  FTableCodes.Sort(SortCodesCriteria);
end;

procedure TToporobotStructure2012.SortSeries;
begin
  AfficherMessage(Format('%s.SortSeries: Tri des series',[ClassName]));
  FListeDesSeries.Sort(SortSerieCriteria);
end;

// charger depuis fichier Tab ou XTB
function TToporobotStructure2012.LoadFichierTab(const FichierTAB: string): integer;
var
  pTAB                : TextFile;
  // compteurs
  NoLigneTAB, i       : integer;
  Prefix1, Prefix2    : Integer;
  WU : string;
  // date
  yyy: Word;
  mmm: Word;
  ddd: Word;
  // variables de lignes
  LigneTab: String;
  PrmsLn: TStringArray;
  ErrMsg: String;
  // liste provisoire pour lecture de la section -6
  ProvListeEntrees: TStringList;
  // série
  UneSerie: TObjSerie;
  qVisee: TUneVisee;
  qNbVisees: Integer;

  // items de tables simples
  UneEntree: TEntrance;
  UneExpe: TExpe;
  UnCode: TCode;
  UnReseau: TReseau;
  UneViseeAntenne: TViseeAntenne;
  NbErrorsInLoading: Integer;
  Ex: Integer;
  qNoSerie: Integer;
  qNoStation: Integer;
  FDoSetDefaulCoordSyst: Boolean;
  blaireau: Integer;
  function LireLigne: string;
  var Lign: string;
  begin
    ReadLn(pTab, Lign);
    Inc(NoLigneTAB);
    Result:=PurgerAccents(Lign);
  end;
  procedure WriteWarning(const Msg: string);
  begin
    ;
  end;
begin
  Result := -1;
  AfficherMessage(Format('%s.LoadFichierTAB(%s)',[ClassName, FichierTAB]));
  //SetDatabaseName(ExtractFileNameWoExt(FichierTAB));
  ReInitialiser(True);

  // code EPSG par défaut
  SetSystemeCoordonnees(DEFAULT_CODE_EPSG);
  NoLigneTAB := 0;
  NbErrorsInLoading := 0;
  AssignFile(pTAB, FichierTAB);
  ProvListeEntrees := TStringList.Create;
  // création de la première série
  //UneSerie := TObjSerie.Create;
  try
    ProvListeEntrees.Clear;
    ReSet(pTAB);
    while Not Eof(pTAB) do begin
      try // traitement local d'exceptions dans la lecture des lignes
        // réinitialisation du message d'erreur par défaut
        ErrMsg:= rsRD_TAB_MSG_ERR;
        Prefix1:=0;
        LigneTab := LireLigne;

        if (Trim(LigneTab)='') then     // lignes vides
           Prefix1:=-100
        else if (LigneTab[1]='#') then  // commentaires sur une ligne
           Prefix1:=-1000
        else if LigneTab[1]='{' then    // commentaires sur plusieurs lignes
          begin
            AfficherMessage(Format(rsRD_TAB_D_MULTI_OBS,[NoLigneTAB]));
            while Not (LigneTab[1]='}') do LigneTab := PurgerAccents(LireLigne);
            AfficherMessage(Format(rsRD_TAB_F_MULTI_OBS,[NoLigneTAB]));
          end
        else begin
          //LigneTab := PurgerAccents(LireLigne);
          PrmsLn   := split(LigneTab, TAB);
          Prefix1  := StrToIntDef(PrmsLn[0], -110);
        end;
        // routage selon le préfixe
        case Prefix1 of
          -9999: Break; // Arrêt forcé du traitement (utiliser avec précaution)
          -1000: AfficherMessage(Format(rsRD_TAB_LN_OBS,[NoLigneTab]));   // commentaire
           -900: ; // balise de pause
           -110: ; // Ligne invalide; ignorée
           -100: ; // ignorer les lignes vides
            -20: AfficherMessage(Format(rsRD_TAB_LASTSAVES,[PrmsLn[1], PrmsLn[2]])); // horodatage
            -19: self.SetNomEtude(Trim(PrmsLn[1])); // Nouvelle section: Nom de l'étude
            -18: self.SetCommentairesEtude(Trim(PrmsLn[1])); // Nouvelle section: Commentaires de l'étude
            -10: begin
                   // Nouvelle section: Système de coordonnées :
                   // En colonne 3: Ancienne notation ConversApi
                   // En colonne 4: Code EPSG
                   // En colonne 5: Description de la grille
                   try
                     WU := Trim(PrmsLn[3]);
                     AfficherMessage(Format('-- Section -10 trouvee en ligne %d: %s', [NoLigneTAB, WU]));
                     self.SetSystemeCoordonnees(WU);
                     FDoSetDefaulCoordSyst := False;
                   except
                     FDoSetDefaulCoordSyst := True;
                   end;
                 end;

             -6: begin  // Section -6: Entrée
                   ProvListeEntrees.Add(PrmsLn[2]);
                 end;
             -9: begin  // visées en antenne

                   with UneViseeAntenne do begin
                     IDViseeAntenne   := StrToIntDef(PrmsLn[1], 0);

                     Reseau           := StrToIntDef(PrmsLn[2], 0);
                     SerieDepart      := StrToIntDef(PrmsLn[3], 0);
                     PtDepart         := StrToIntDef(PrmsLn[4], 0);
                     Code             := StrToIntDef(PrmsLn[5], 0);
                     Expe             := StrToIntDef(PrmsLn[6], 0);
                     IDTerrainStation := Trim(PrmsLn[7]);
                     Longueur         := StrToFloatDef(PrmsLn[8], 0.00);
                     Azimut           := StrToFloatDef(PrmsLn[9], 0.00);
                     Pente            := StrToFloatDef(PrmsLn[10], 0.00);
                     Commentaires     := Trim(PrmsLn[11]);
                     AfficherMessage(Format('  >> Antenne trouvee: %d - %s', [IDViseeAntenne, IDTerrainStation]));
                   end;
                   AddViseeAntenne(UneViseeAntenne);
                 end;
             -8: begin  // réseaux
                   with UnReseau do begin
                     IdxReseau   := StrToIntDef(PrmsLn[1], 0);
                     ColorReseau := RGB(StrToIntDef(PrmsLn[2], 0),
                                        StrToIntDef(PrmsLn[3], 0),
                                        StrToIntDef(PrmsLn[4], 0));
                     TypeReseau  := StrToIntDef(PrmsLn[5], 0);
                     NomReseau   := Trim(PrmsLn[6]);
                     ObsReseau   := Trim(PrmsLn[7]);
                   end;
                   AddReseau(UnReseau);
                 end;
             -5: begin  // Entrées
                   //nombre d'entrées nul = on définit l'entrée par défaut
                   if (GetNbEntrees = 0) then begin
                     SetDefaultCoords(StrToFloatDef(PrmsLn[2], 0.00),
                                      StrToFloatDef(PrmsLn[3], 0.00),
                                      StrToFloatDef(PrmsLn[4], 0.00)
                                     );
                     SetRefSeriePoint(StrToIntDef(PrmsLn[5],1),
                                      StrtoIntDef(PrmsLn[6], 0)
                                     );

                   end;
                   //Commentaires:=PrmsLn[7];
                   // Ajouter les entrées
                   // le nombre d'entrées retenu est celui décompté dans
                   // la section -5
                   with UneEntree do begin
                     eNumEntree:= GetNbEntrees + 1;
                     // On ajoute l'entrée récupérée en -6
                     try
                       eNomEntree:=ProvListeEntrees.Strings[eNumEntree-1];
                     except
                       AfficherMessage(Format(rsWARNINGENTRYADDED,[eNumEntree]));
                       eNomEntree:= Format(rsRD_TAB_ENTRANCE,[eNumEntree]);
                     end;
                     eXEntree  := StrToFloatDef(PrmsLn[2], 0.00);
                     eYEntree  := StrToFloatDef(PrmsLn[3], 0.00);
                     eZEntree  := StrToFloatDef(PrmsLn[4], 0.00);
                     //------------
                     // entrées non géoréférencées ?
                     if (eXEntree < 100.00) or
                        (eYEntree < 100.00) or
                        (eZEntree < 100.00)
                     then WriteWarning(Format(rsRD_TAB_ENTR_NOGEOREF, [NoLigneTAB,eNumEntree, eNomEntree]));
                     eRefSer   := StrToIntDef(PrmsLn[5],1);
                     eRefSt    := StrtoInt(PrmsLn[6]);
                     if ((eRefSer < 1) or (eRefSt < 0)) then
                        WriteWarning(Format(rsRD_TAB_ENTR_BADLINK,
                             [NoLigneTAB, eNumEntree, eNomEntree, eRefSer, eRefSt]));
                     eObserv   := PrmsLn[7];
                   end;
                   AddEntree(UneEntree);
                   //AfficherMessage('-5 [Entrées]: Ajout entrée');
                   //NbEntrees:=1+NbEntrees;
                 end;
             -4: ;
             -3: ;
             -2: begin // Expés
                   with UneExpe do begin
                     IDExpe        := StrToInt(PrmsLn[1]);
                     if (IDExpe<=0) then begin
                        ErrMsg:=rsRD_TAB_BAD_TRIP;
                        raise Exception.Create(ErrMsg);
                     end;
                     JourExpe      := StrToIntDef(PrmsLn[2],0);
                     MoisExpe      := StrToIntDef(PrmsLn[3],0);
                     AnneeExpe     := StrToIntDef(PrmsLn[4],0);
                     if (JourExpe=0) or (MoisExpe=0) or (AnneeExpe=0) then begin
                       //ErrMsg:=Format('Date incorrecte: %d/%d/%d',[JourExpe,MoisExpe,AnneeExpe]);
                       //raise Exception.Create(ErrMsg);
                       WriteWarning(Format(rsRD_TAB_BAD_DATE,
                                          [NoLigneTAB]));
                       SafeDecodeDate(Now, yyy, mmm, ddd);
                       JourExpe := ddd;
                       MoisExpe := mmm;
                       AnneeExpe:= yyy;

                     end;
                     Speleometre   := PrmsLn[5];             // spéléomètre
                     Speleographe  := PrmsLn[6];             // spéléographe
                     ModeDecl      := StrToInt(PrmsLn[7]);   // déclinaison auto ?
                     Declinaison   := StrToFloatDef(PrmsLn[8], 0.00); // déclinaison
                     Inclinaison   := StrToFloatDef(PrmsLn[9], 0.00); // correction clino x10
                     Couleur       := StrToInt(PrmsLn[10]);  // couleur
                     Commentaire   := PrmsLn[11];            // commentaire

                   end;
                   AddExpe(UneExpe);
                 end;

             -1: begin // Codes
                   with UnCode do begin
                      IDCode     := StrToInt(PrmsLn[1]);    // ID Code
                      if (IDCode<=0) then begin
                        //WriteWarning(Format('WARNING ! (%d) - Numéro de Code incorrect (Valeur: %d) - Mis à %d',[NoLigneTAB, IDCode, FNbCodes]));
                        ErrMsg:=rsRD_TAB_BAD_CODE;
                        raise Exception.Create(ErrMsg);
                      end;
                      GradAz     := StrToFloatDef(PrmsLn[2], 360.00);  // unité boussole
                      GradInc    := StrToFloatDef(PrmsLn[3], 360.00);  // unite  CLINO
                      PsiL       := StrToFloatDef(PrmsLn[4], 0.01);  // precision longueur
                      PsiAz      := StrToFloatDef(PrmsLn[5], 0.1);  // precision azimut
                      PsiP       := StrToFloatDef(PrmsLn[6], 0.1);  // precision pente
                      FactLong   := StrToFloatDef(PrmsLn[7], 1.00)/100;  // Facteur des longueurs
                      AngLimite  := StrToFloatDef(PrmsLn[8], 0.00);  // angle limite
                      Commentaire:= PrmsLn[9];              // commentaire
                      TypeGalerie:= StrToIntDef(PrmsLn[10],0);
                    end;
                    AddCode(UnCode);

                 end;
             0: ; // pas de section 0
         else  // on est dans les séries !
           // Si le préfixe 2 (2e colonne) =-1 =>nouvelle série
           Prefix1 := StrToInt(PrmsLn[0]);
           if (Prefix1 > 0) then
           begin
             Prefix2 := StrToInt(PrmsLn[1]);

             if (Prefix2 = -1) then
             begin
               // si c'est la première série, on crée
               if (Prefix1 = 1) then
               begin
                 UneSerie := TObjSerie.Create;
               end
               else  // sinon on ferme la série courante et on crée la suivante
               begin
                 self.AddSerie(UneSerie);
                 UneSerie := TObjSerie.Create;
               end;
                 with UneSerie do
                 begin
                   SetIndexSerie(Prefix1);
                   SetSeriePtExtremites(StrToInt(PrmsLn[2]),
                                        StrToInt(PrmsLn[3]),
                                        StrToInt(PrmsLn[4]),
                                        StrToInt(PrmsLn[5]));
                   // NbPoints: Non utilisé
                   //NbPoints      :=StrToInt(PrmsLn[6]);
                   SetChanceObstacle(StrToInt(PrmsLn[7]), StrToInt(PrmsLn[8]));
                   SetNomSerie(PrmsLn[9]);
                   SetObsSerie(PrmsLn[10]);
                   SetNoReseau(StrToIntDef(PrmsLn[11],0));
                   // raideur de la série
                   SetRaideur(StrToFloatDef(Prmsln[12], 1.02));
                   // on lit toutes les visées
                 end; //with UneSerie do
               //end; // if (Prefix2 = -1) then
             end else
             begin
               with qVisee do
               begin
                 TypeVisee   := tgDEFAULT;
                 Code        := StrToInt(PrmsLn[2]);
                 Expe        := StrToInt(PrmsLn[3]);
                 Longueur    := StrToFloatDef(PrmsLn[4], 0.00);
                 if (Longueur < 0.00) then begin
                   WriteWarning(Format(rsRD_TAB_NEG_LONG, [NoLigneTAB, Longueur]));
                   Longueur:=Abs(Longueur);
                 end;
                 Azimut      := StrToFloatDef(PrmsLn[5], 0.00);
                 Pente       := StrToFloatDef(PrmsLn[6], 0.00);
                 LG          := StrToFloatDef(PrmsLn[7], 0.00);  //LG
                 LD          := StrToFloatDef(PrmsLn[8], 0.00);  //LD
                 HZ          := StrToFloatDef(PrmsLn[9], 0.00);
                 HN          := StrToFloatDef(PrmsLn[10], 0.00);
                 Commentaires:= PrmsLn[11];
                 IDTerrainStation:=Trim(PrmsLn[12]);
                 blaireau    := StrToIntDef(PrmsLn[13], 0);
                 TypeVisee   := GetTypeDeVisee(blaireau); // galerie fossile supposée
               end; // with qVisee do
               UneSerie.AddVisee(qVisee);
             end;
           end   // if Prefix2
         end; // case Prefix1
      except
        Inc(NbErrorsInLoading);
        WriteWarning('');
        WriteWarning(Format(rsRD_ERROR_LN,[NoLigneTab, ErrMsg]));
        WriteWarning(Format(rsRD_CONTNT_LN,[LigneTab]));
        WriteWarning(rsRD_TAB_FIELD_VALUES);
        for Ex:=0 to High(PrmsLn) do
        begin
          WriteWarning(Format(' PrmsLn[%.2d] = "%s"',[Ex, PrmsLn[Ex]]));
          WriteWarning('');
        end;
      end;
    end; // while Not Eof(pTAB) do begin
    // On ferme la dernière série
    self.AddSerie(UneSerie);
    // si aucun système de coordonné trouvé, on met Lambert 93 par défaut
    if (FDoSetDefaulCoordSyst) then  SetSystemeCoordonnees('LT93');
    // Passage ici ? -> OK
    Result := 666;
  finally
    CloseFile(pTAB);
    ProvListeEntrees.Free;
  end;
end;
// Statut: Opérationnel mais les commentaires des séries et stations sont ignorés
function TToporobotStructure2012.LoadFichierText(const FichierText: string): integer;
const
  // colonnes des champs
  COL_COMMENT     = 00; // parenthèse de commentaire éventuel
  COL_NUM_SECTION = 01; // numéro de section ou numéro de série
  COL_NUM_ITEM    = 02; // numéro d'item (expé, code, point topo)

  COL_CODE        = 03; // code attaché à la station
  COL_SESSION     = 04; // numéro de session (inutilisé)
  COL_EXPE        = 05; // numéro de séance

  COL_VAR_DATA    = 06; // colonne suivant les colonnes de préfixes et index
  COL_NOMENTREE   = COL_VAR_DATA; // nom de l'entrée
  COL_XENTREE     = COL_VAR_DATA;
  COL_YENTREE     = COL_XENTREE + 1;
  COL_ZENTREE     = COL_YENTREE + 1;
  COL_ENT_SER     = COL_ZENTREE + 1;
  COL_ENT_ST      = COL_ENT_SER + 1;

  COL_DATE_EXPE   = COL_VAR_DATA;
  COL_SPELEOMETRE = COL_DATE_EXPE + 1;
  COL_SPELEOGRAPH = COL_SPELEOMETRE + 1;
  COL_MODEDECL    = COL_SPELEOGRAPH + 1;
  COL_DECL_VALUE  = COL_MODEDECL + 1;
  COL_INCL_VALUE  = COL_DECL_VALUE + 1;
  COL_COULEUR     = COL_INCL_VALUE + 1;

  COL_U_AZIMUT    = COL_VAR_DATA;
  COL_U_PENTE     = COL_U_AZIMUT + 1;
  COL_PSI_L       = COL_U_PENTE + 1;
  COL_PSI_AZ      = COL_PSI_L +1;
  COL_PSI_P       = COL_PSI_AZ + 1;
  //COL_OLD_DECL    = COL_PSI_P  + 1;
  COL_FACT_LONG   = COL_PSI_P + 1;
  COL_ANG_LIMITE  = COL_FACT_LONG + 1;

  COL_SER_DEP     = COL_VAR_DATA;
  COL_PT_DEP      = COL_SER_DEP + 1;
  COL_SER_ARR     = COL_PT_DEP + 1;
  COL_PT_ARR      = COL_SER_ARR + 1;
  COL_NB_PTS      = COL_PT_ARR + 1;
  COL_CHANCE      = COL_NB_PTS + 1;
  COL_OBSTACLE    = COL_CHANCE + 1;

  COL_LONG        = COL_VAR_DATA;
  COL_AZ          = COL_LONG + 1;
  COL_P           = COL_AZ + 1;
  COL_LG          = COL_P + 1;
  COL_LD          = COL_LG + 1;
  COL_HZ          = COL_LD + 1;
  COL_HN          = COL_HZ + 1;

  TEMPORARYTABFILE = '~GhtopoTemp.txt';

var
  // fichier d'erreur
  pTEXT        : TextFile;
  // liste provisoire pour entrées
  ProvListeEntrees: TStringList;
  // séries, réseaux, etc
  UneEntree : TEntrance;
  UnReseau  : TReseau;
  UneExpe : TExpe;
  UnCode  : TCode;
  UneSerie: TObjSerie;
  qVisee  : TUneVisee;
  // ligne de données
  NoLigneTEXT,
  NbErrorsInLoading: integer;
  LigneTEXT: string;
  PrmsLn   : TStringArray;
  // la ligne contient une parenthèse de tête ?
  HasParenthesis: boolean;
  // messages d'erreur
  ErrMsg  : string;
  // Préfixes de section
  Prefix1,
  Prefix2 : integer;

  // index de boucles
  i ,n, Ex: integer;
  // divers
  str         : string;
  llanfair_pg : string;
  // dates
  yyy, mmm, ddd: word;
  // Index courant
  IdxCourant: integer;
  IdxCourantStation: integer;
  NomDeLaSerie: String;
  // Conversion de lignes Mac > PC
  procedure ConvertTextMacToPC(var Lin: string);
  var
    pn   : integer;
    ps  : integer;
    s,p : string;
    w1  : integer;
  begin
    // convertir accentués
    for w1:=0 to Length(Lin) do if Lin[w1]=#136 then Lin[w1]:='a';
    for w1:=0 to Length(Lin) do if Lin[w1]=#142 then Lin[w1]:='e';
    for w1:=0 to Length(Lin) do if Lin[w1]=#143 then Lin[w1]:='e';
    for w1:=0 to Length(Lin) do if Lin[w1]=#148 then Lin[w1]:='i';
    for w1:=0 to Length(Lin) do if Lin[w1]=#137 then Lin[w1]:='a';
    for w1:=0 to Length(Lin) do if Lin[w1]=#144 then Lin[w1]:='e';
    for w1:=0 to Length(Lin) do if Lin[w1]=#144 then Lin[w1]:='e';
    for w1:=0 to Length(Lin) do if Lin[w1]=#141 then Lin[w1]:='c';
    //*)
  end;
  // afficher avertissement
  procedure WriteWarning(const wm: string);
  begin
    AfficherMessage(wm);
  end;
begin
  Result := -1;
  // Fichier introuvable ? OK, On sort
  if not FileExists(FichierText) then  begin Result:=-32; Exit; end;

  try // try..finally niveau 0
     //*************************************************************************
     // Pour pallier à tout problème de formats de fichiers textes,
     // et notamments ceux liés aux fins de ligne,
     // on fait une conversion.
     // Pour économiser une variable, on utilise une variable utilisée + tard
     LigneTEXT := ExtractFilePath(FichierText)+TEMPORARYTABFILE;
     AfficherMessage(rsSEPARATOR_LINE);
     AfficherMessage(rsCONV_TAB_MAC_UNIX);

     ConvertTextFile(FichierText, LigneTEXT,
                     tfMAC, tfWINDOWS);
     AfficherMessage(rsSEPARATOR_LINE);
     // nom de la base
     SetDatabaseName(ExtractFileNameOnly(FichierText));
     // Initialisation des listes
     ReInitialiser(True);
     // code EPSG par défaut
     SetSystemeCoordonnees(DEFAULT_CODE_EPSG);
     // début du véritable traitement
     // liste provisoire pour lecture des sections -6
     ProvListeEntrees:=TStringList.Create;
     // ouverture fichier Text
     AssignFile(pTEXT, LigneTEXT);
     try // niveau 1: pour le fichier Text
       Reset(pTEXT);
       // lire tout le fichier
       NoLigneTEXT:=0;
       NbErrorsInLoading:=0;
       ErrMsg:= rsRD_TAB_MSG_ERR;
       IdxCourant := -1;
       while Not Eof(pTEXT) do
       begin
         try // traitement local d'exceptions dans la lecture des lignes
           Prefix1:=0;
           Inc(NoLigneTEXT);
           ReadLn(pTEXT, LigneTEXT);
           //AfficherMessage(LigneTEXT);
           ConvertTextMacToPC(LigneTEXT);
           if Trim(LigneTEXT)=''    then Prefix1:=-100       // lignes vides
           else if LigneTEXT[1]='#' then Prefix1:=-1000     // commentaires sur une ligne
           else
           begin
             // Commentaire = parenthèse de tête présente
             HasParenthesis := (LigneTEXT[1] = '(');
             // attraper le préfixe de section (en évitant une décomposition)
             str := Trim(Copy(LigneTEXT, 2, 5));
             Prefix1   := StrToIntDef(str, -110);
             if (HasParenthesis) then  // On zappe les commentaires (difficultés d'implémentation)
             begin
               //Continue;
               // classique décomposition ...
               PrmsLn      := SplitFixedColLine(LigneTEXT, [1, 2, 7, 13, 17, 21, 25, 80,100]);
               llanfair_pg := Trim(PrmsLn[COL_VAR_DATA]);
               case Prefix1 of
                 -5: begin // entrées

                       UneEntree := GetEntree(IdxCourant);
                       // on concatène les lignes
                       UneEntree.eObserv := UneEntree.eObserv + llanfair_pg;

                       PutEntree(IdxCourant, UneEntree);
                     end;
                 -4: begin // sessions (obsolète)
                     end;
                 -3: ;     // section non affectée
                 -2: begin // expés
                       IdxCourant := GetNbExpes - 1;
                       UneExpe   := GetExpe(IdxCourant);
                       // on concatène les lignes

                       UneExpe.Commentaire := UneExpe.Commentaire + llanfair_pg;
                       PutExpe(IdxCourant, UneExpe);
                     end;
                 -1: begin // code
                       IdxCourant := GetNbCodes - 1;
                       UnCode    := GetCode(IdxCourant);
                       // on concatène les lignes
                       UnCode.Commentaire := UnCode.Commentaire + llanfair_pg;
                       PutCode(IdxCourant, UnCode);
                     end;
               else  // séries
                 // Zapper cette section
                 begin
                   Continue;
                   if (Prefix1 > 0) then
                   begin
                     Prefix2:=StrToInt(PrmsLn[COL_NUM_ITEM]);
                     if (Prefix2 = -1) then
                     begin // tête de série
                       IdxCourant := GetNbSeries - 1;
                       UneSerie := GetSerie(IdxCourant);
                       UneSerie.SetObsSerie(UneSerie.GetObsSerie + llanfair_pg);
                       PutSerie(IdxCourant, UneSerie);

                     end else
                     begin // sinon, station
                       //Continue;
                      // IdxCourant := GetNbSeries - 1;
                       IdxCourantStation := UneSerie.GetNbVisees - 1;
                       qVisee := UneSerie.GetVisee(IdxCourantStation);
                       qVisee.Commentaires := qVisee.Commentaires + llanfair_pg;
                       UneSerie.PutVisee(IdxCourantStation, qVisee);
                       PutSerie(IdxCourant, UneSerie);
                     end;
                   end;
                 end;
               end;
               // on zappe tout le reste ...
               Continue;
             end;  // if HasParenthesis


             case Prefix1 of
                -9999: begin // Arrêt forcé du traitement
                             // utiliser cette balise avec de grandes précautions
                             // et de préférence à la fin d'une série
                         WriteWarning('');
                         WriteWarning(Format(rsRD_TAB_STOP_9999, [NoLigneTEXT]));
                         WriteWarning('');
                         Break;
                       end;
                -1000: AfficherMessage(Format(rsRD_TAB_LN_OBS,[NoLigneTEXT]));  // commentaire
                 -900: ; // balise de pause
                 -110: ; // Ligne invalide; ignorée
                 -100: ; // ignorer les lignes vides
                   -7: AfficherMessage(rsRD_TAB_CLASSEURS); // Section -7 : Classeurs -- Ignoré par GHTopo
                   -6: begin  // Section -6: Entrée
                         PrmsLn := SplitFixedColLine(LigneTEXT, [1, 2, 7, 13, 17, 21, 25, 80, 100]);
                         ProvListeEntrees.Add(PrmsLn[COL_NOMENTREE]);
                       end;
                   -5: begin  // Entrées
                         PrmsLn := SplitFixedColLine(LigneTEXT, [1, 2, 7, 13, 17, 21, 25, 37, 49, 61, 67, 73, 80]);
                         //nombre d'entrées nul = on définit l'entrée par défaut
                         // TODO: Revoir cette zone
                         if (self.GetNbEntrees = 0) then begin
                           SetDefaultCoords(StrToFloatDef(PrmsLn[COL_XENTREE], 0.00),
                                            StrToFloatDef(PrmsLn[COL_YENTREE], 0.00),
                                            StrToFloatDef(PrmsLn[COL_ZENTREE], 0.00)
                                           );

                           SetRefSeriePoint(StrToIntDef(PrmsLn[COL_ENT_SER],1),
                                            StrtoIntDef(PrmsLn[COL_ENT_ST], 0)
                                           );

                          end;
                          // Ajouter les entrées
                          // le nombre d'entrées retenu est celui décompté dans
                          // la section -5
                          with UneEntree do begin
                            eNumEntree:= GetNbEntrees + 1;
                            // On ajoute l'entrée récupérée en -6
                            try
                              eNomEntree:=ProvListeEntrees.Strings[eNumEntree - 1];
                            except
                              AfficherMessage(Format(rsWARNINGENTRYADDED,[eNumEntree]));
                              eNomEntree:= Format(rsRD_TAB_ENTRANCE,[eNumEntree]);
                            end;
                            eXEntree  := StrToFloatDef(PrmsLn[COL_XENTREE], 0.00);
                            eYEntree  := StrToFloatDef(PrmsLn[COL_YENTREE], 0.00);
                            eZEntree  := StrToFloatDef(PrmsLn[COL_ZENTREE], 0.00);

                            //------------
                            // entrées non géoréférencées ?
                            if (eXEntree < 100.00) or
                               (eYEntree < 100.00) or
                               (eZEntree < 100.00)
                            then WriteWarning(Format(rsRD_TAB_ENTR_NOGEOREF,
                                                [NoLigneTEXT,eNumEntree, eNomEntree]));
                            eRefSer   := StrToIntDef(PrmsLn[COL_ENT_SER],1);
                                eRefSt    := StrtoInt(PrmsLn[COL_ENT_ST]);
                                if ((eRefSer < 1) or (eRefSt < 0)) then
                                   WriteWarning(Format(rsRD_TAB_ENTR_BADLINK,
                                        [NoLigneTEXT, eNumEntree, eNomEntree, eRefSer, eRefSt]));
                                eObserv   := '';

                          end; // with UneEntree
                          self.AddEntree(UneEntree);
                          // index courant
                          IdxCourant := GetNbEntrees - 1;
                       end;
                   -4: AfficherMessage('-4'+ rsRD_TAB_IGNORED_SEC); // sessions; ignoré
                   -3: AfficherMessage('-3'+ rsRD_TAB_IGNORED_SEC); // réservé; ignoré
                   -2: begin // Expés
                         //Ajoute l'expé dans la liste
                         PrmsLn := SplitFixedColLine(LigneTEXT, [1, 2, 7, 13, 17, 21, 25, 36, 50, 64, 65, 73, 77, 81,100]);
                          with UneExpe do begin
                            IDExpe        := StrToInt(PrmsLn[COL_NUM_ITEM]);
                            if (IDExpe<=0) then begin
                               ErrMsg:=rsRD_TAB_BAD_TRIP;
                               raise Exception.Create(ErrMsg);
                            end;
                            // décomposition de la date
                            str := Trim(PrmsLn[COL_DATE_EXPE]);
                            JourExpe      := StrToIntDef(Copy(STR, 0, 2), 0);
                            MoisExpe      := StrToIntDef(Copy(STR, 4, 2), 1);
                            AnneeExpe     := StrToIntDef(Copy(STR, 7, 2), 1);
                            if (JourExpe=0) or (MoisExpe=0) or (AnneeExpe=0) then begin
                              WriteWarning(Format(rsRD_TAB_BAD_DATE,
                                                 [NoLigneTEXT]));
                              SafeDecodeDate(Now, yyy, mmm, ddd);
                              JourExpe := ddd;
                              MoisExpe := mmm;
                              AnneeExpe:= yyy;
                            end;
                            Speleometre   := PrmsLn[COL_SPELEOMETRE];             // spéléomètre
                            Speleographe  := PrmsLn[COL_SPELEOGRAPH];             // spéléographe


                            ModeDecl      := StrToIntDef(PrmsLn[COL_MODEDECL], 0);   // déclinaison auto ?
                            Declinaison   := StrToFloatDef(PrmsLn[COL_DECL_VALUE], 0.00); // déclinaison
                            Inclinaison   := StrToFloatDef(PrmsLn[COL_INCL_VALUE], 0.00); // correction clino x10
                            Couleur       := StrToIntDef(PrmsLn[COL_COULEUR], 0);  // couleur
                            Commentaire   := ''; //PrmsLn[11];            // commentaire

                          end;  // with UneExpe
                          AddExpe(UneExpe);
                       end;
                   -1: begin // Codes
                         //                                                           U_AZ
                         //                                                            |   U_P
                         //                                                            |   |   PSI_L
                         //                                                            |   |   |   PSI_A
                         //                                                            |   |   |   |   PSI_P
                         //                                                            |   |   |   |   |   FACT
                         //                                                            |   |   |   |   |   |   ANG_LIM
                         //                                                            |   |   |   |   |   |   |
                         PrmsLn := SplitFixedColLine(LigneTEXT, [1, 2, 7, 13, 17, 21, 25, 33, 41, 49, 57, 65, 73, 81, 100]);
                         with UnCode do
                         begin
                           IDCode     := StrToInt(PrmsLn[COL_NUM_ITEM]);    // ID Code
                           if (IDCode<=0) then
                           begin
                             ErrMsg:=rsRD_TAB_BAD_CODE;
                             raise Exception.Create(ErrMsg);
                           end;
                           GradAz     := StrToFloatDef(PrmsLn[COL_U_AZIMUT], 360.00);  // unité boussole
                           GradInc    := StrToFloatDef(PrmsLn[COL_U_PENTE], 360);  // unite  CLINO
                           PsiL       := StrToFloatDef(PrmsLn[COL_PSI_L], 0.01);  // precision longueur
                           PsiAz      := StrToFloatDef(PrmsLn[COL_PSI_AZ], 0.5);  // precision azimut
                           PsiP       := StrToFloatDef(PrmsLn[COL_PSI_P], 0.5);  // precision pente
                           FactLong   := StrToFloatDef(PrmsLn[COL_FACT_LONG], 1.00)/100.00;  // Facteur des longueurs
                           AngLimite  := StrToFloatDef(PrmsLn[COL_ANG_LIMITE], 0.00);  // angle limite
                           Commentaire:= ''; //PrmsLn[9];              // commentaire
                         end; // with UnCode
                         AddCode(UnCode);
                      end;
                   0: ; // pas de section 0
             else // on est dans les séries
               begin
                 //AfficherMessage(LigneTEXT);
                 PrmsLn := SplitFixedColLine(LigneTEXT, [1, 2, 7, 13, 17, 21, 25, 80, 100]);
                 // Si le préfixe 2 (2e colonne) =-1 =>nouvelle série
                 Prefix1 := StrToInt(PrmsLn[COL_NUM_SECTION]);

                 if (Prefix1 > 0) then
                 begin
                   Prefix2 := StrToInt(PrmsLn[COL_NUM_ITEM]);

                   // C'est le header de série ?
                   // les headers de série comportent deux lignes !
                   if (Prefix2 = -2) then
                   begin

                     // première ligne
                     NomDeLaSerie := Trim(PrmsLn[COL_VAR_DATA]);
                     AfficherMessage(Format('Une serie a ajouter: Prefix1: %d - Prefix2 = %d - %s',[Prefix1, Prefix2, NomDeLaSerie]));
                     // Lecture de la 2e ligne
                     Inc(NoLigneTEXT);
                     ReadLn(pTEXT, LigneTEXT);

                     // décomposition
                     //*******************************************************************************
                     //                                                           SD
                     //                                                            |   PD
                     //                                                            |   |   SA
                     //                                                            |   |   |   PA
                     //                                                            |   |   |   |   NB
                     //                                      Serie                 |   |   |   |   |   CH
                     //                                                            |   |   |   |   |   |   OBS
                     //                                                            |   |   |   |   |   |   |
                     PrmsLn := SplitFixedColLine(LigneTEXT, [1, 2, 7, 13, 17, 21, 25, 33, 41, 49, 57, 65, 73, 81, 100]);
                     //AfficherMessage(Format('%d %d - %s', [Prefix1, Prefix2,  UneSerie.GetNomSerie]));
                     // si c'est la première série, on crée
                     AfficherMessage(LigneTEXT);
                     AfficherMessage(PrmsLn[1]);
                     Prefix1 := StrToInt(PrmsLn[1]);


                     if (Prefix1 = 1) then
                     begin
                       UneSerie := TObjSerie.Create;
                       UneSerie.SetIndexSerie(1);
                       AfficherMessage(Format('Prefix = %d - Serie 1 creee',[Prefix1]));
                     end
                     else  // sinon on ferme la série courante et on crée la suivante
                     begin
                       self.AddSerie(UneSerie);
                       UneSerie := TObjSerie.Create;
                     end;
                     with UneSerie do
                     begin
                       SetIndexSerie(Prefix1);
                       SetNomSerie(NomDeLaSerie);
                       SetSeriePtExtremites(StrToInt(PrmsLn[COL_SER_DEP]),
                                            StrToInt(PrmsLn[COL_PT_DEP]),
                                            StrToInt(PrmsLn[COL_SER_ARR]),
                                             StrToInt(PrmsLn[COL_PT_ARR])
                                           );
                       SetChanceObstacle(StrToInt(PrmsLn[COL_CHANCE]),
                                         StrToInt(PrmsLn[COL_OBSTACLE]));
                       SetNoReseau(0); // non supporté par TOPOROBOT
                       SetObsSerie('');
                       SetRaideur(1.00); // Raideur: Non supporté par TOPOROBOT
                     end; //with UneSerie do
                   end else
                   begin // on lit les visées
                     //                 ('-->'+LigneTab);
                     //*******************************************************************************
                     //                                                           Long
                     //                                                            |   Azimut
                     //                                                            |   |   Pente
                     //                                      Parenthèse            |   |   |   LG
                     //                                      | Serie               |   |   |   |   LD
                     //                                      | |   Station         |   |   |   |   |   HZ
                     //                                      | |   |  Code         |   |   |   |   |   |   HN
                     //                                      | |   |  |    |Session|   |   |   |   |   |   |
                     //                                      | |   |  |    | Expé  |   |   |   |   |   |   |
                     PrmsLn := SplitFixedColLine(LigneTEXT, [1, 2, 7, 13, 17, 21, 25, 33, 41, 49, 57, 65, 73, 81, 100]);

                     with qVisee do
                     begin
                       TypeVisee   := tgDEFAULT;
                       Code        := StrToInt(PrmsLn[COL_CODE]);
                       Expe        := StrToInt(PrmsLn[COL_EXPE]);
                       Longueur    := StrToFloatDef(PrmsLn[COL_LONG], 0.00);
                       if (Longueur < 0.00) then begin
                         WriteWarning(Format(rsRD_TAB_NEG_LONG, [NoLigneTEXT, Longueur]));
                         Longueur:=Abs(Longueur);
                       end;
                       Azimut      := StrToFloatDef(PrmsLn[COL_AZ], 0.00);
                       Pente       := StrToFloatDef(PrmsLn[COL_P], 0.00);
                       LG          := StrToFloatDef(PrmsLn[COL_LG], 0.00);  //LG
                       LD          := StrToFloatDef(PrmsLn[COL_LD], 0.00);  //LD
                       HZ          := StrToFloatDef(PrmsLn[COL_HZ], 0.00);
                       HN          := StrToFloatDef(PrmsLn[COL_HN], 0.00);
                       Commentaires:= '';
                       IDTerrainStation := '';   // Non supporté par TOPOROBOT
                       TypeVisee        := tgDEFAULT; // Non supporté par TOPOROBOT
                     end; // with qVisee do
                     UneSerie.AddVisee(qVisee);

                   end;   // if Prefix2

                 end; // case
               end;
             end;
           end;
           IdxCourantStation := UneSerie.GetNbVisees - 1;
         except
         end;
       end;  //while Not Eof(pTEXT) do
       // On ferme la dernière série
       AddSerie(UneSerie);

       Result := 1;
     finally
       CloseFile(pTEXT);
     end; // niveau 1: pour le fichier Text
  finally
    ProvListeEntrees.Free;

  end; //try..finally niveau 0
end;

procedure TToporobotStructure2012.SaveToFile(const FichierTAB: string;
  const ModeSaveTAB: TModeSaveTAB; const TextFileFormat: TTextFileFormat);
const
  LINE_CODE    = '%d'+TAB+'%d'+TAB+
                 '%.2f'+TAB+'%.2f'+TAB+
                 '%.2f'+TAB+'%.2f'+TAB+'%.2f'+TAB+
                 '%.2f'+TAB+'%.2f'+TAB+
                 '%s';
  LINE_STATION = '%d'+TAB+'%d'+TAB+
                 '%d'+TAB+'%d'+TAB+
                 '%.2f'+TAB+'%.2f'+TAB+'%.2f'+TAB+
                 '%.2f'+TAB+'%.2f'+TAB+'%.2f'+TAB+'%.2f'+TAB+
                 '%s';
var
  ENDL     : string;
  Sr, St, i: integer;
  pTAB: TextFile;
  Entrance: TEntrance;
  Expe: TExpe;
  Code: TCode;
  Serie: TObjSerie;
  Station: TUneVisee;
  s, s1: string;
  R: TReseau;
  VA: TViseeAntenne;
  WU: string;
  procedure WrtLn(const Str: string);
  begin
    Write(pTAB, Str+ENDL);
  end;
  procedure WrtCommentaire(const Str: string);
  begin
    if (ModeSaveTAB = mtabEXTENDEDTAB) then
      Write(pTAB, '#### '+Str+ENDL);
  end;
begin
  case TextFileFormat of  // mise en place des fins de lignes
    tfWINDOWS: begin s:='Windows';   ENDL := #13+#10; end;
    tfMAC    : begin s:='Macintosh'; ENDL := #13    ; end;
    tfUNIX   : begin s:='Unix';      ENDL := #10    ; end;
  end;
  AfficherMessage(Format('TToporobotStructure.SaveFile(%s) as %s format',[FichierTAB, s]));
  try
    // Sauvegarder ancien fichier

    AssignFile(pTAB, FichierTAB);
    ReWrite(pTAB);
    AfficherMessage('   Saving header');
    // Sections ajoutées au format TOPOROBOT
    // Fichiers XTB uniquement
    if (ModeSaveTAB = mtabEXTENDEDTAB) then
    begin
      // Section -20: Date de dernières modifications, version du fichier et du logiciel
      AfficherMessage('--> Horodating saves at #20 section');

      WrtLn(Format('%d'+TAB+'%s'+TAB+'%s'+TAB+'%s',
                   [-20,
                    DateToStr(Now), TimeToStr(Now),
                    self.FDataBaseName
                   ]));
      // Sections -19 et -18: Libellé et commentaires de l'étude (XTB uniquement)
      AfficherMessage('--> Writing general infos at #-19 to #-18 sections');
      WrtLn(Format('%d'+TAB+'%s',
                   [-19, self.FNomEtude]));
      WrtLn(Format('%d'+TAB+'%s',
                   [-18, self.FCommentaireEtude]));

      // Section -10: Système de coordonnées géographiques (XTB uniquement)
      // TODO: Désactivé pour l'instant - A réimplémenter
      AfficherMessage('--> Writing coordinates system at #-10 section');
      WrtLn('');
      WrtCommentaire('Main coordinates system');
      WrtCommentaire('*** Temporarly disabled ***');

      WrtLn('');
      WU := self.GetSystemeCoordonnees();
      WrtCommentaire(Format('%d'+TAB+'%d'+TAB+'%d'+TAB+'%s'+TAB+'%s',
                    [-10, 0, 0, 'EPSG', WU]));
      //*************************************


      // Section -9: Visées en antennes (Format XTB uniquement)
      if (GetNbAntennes > 0) then begin
        AfficherMessage('--> Saving #-9 Antenna shots section');

        WrtLn('');
        WrtCommentaire('Antenna-shots list');
        WrtLn('');
        for i := 1 to GetNbAntennes - 1 do begin
          VA := GetViseeAntenne(i);
          //-9	1	2	1102	10	101	101	GHLMF	12.56	289.00	-14.00	le chat dans la souricière
          WrtLn(Format('%d'+TAB+'%d'+TAB+'%d'+
                            TAB+'%d'+TAB+'%d'+ // série, point
                            TAB+'%d'+TAB+'%d'+ // code, expé
                            TAB+'%s'+          // ID terrain
                            TAB+'%.2f'+TAB+'%.2f'+TAB+'%.2f'+
                            TAB+'%s',
                   [-9, i, VA.Reseau,
                           VA.SerieDepart, VA.PtDepart,
                           VA.Code, VA.Expe,
                           VA.IDTerrainStation,
                           VA.Longueur, VA.Azimut, VA.Pente,
                           VA.Commentaires
                   ]));
        end;

      end;

      // Section -8: Réseaux (Format XTB uniquement)
      AfficherMessage('--> Saving #-8 Networks section');

      WrtLn('');
      WrtCommentaire('Networks list');
      WrtLn('');
      if (GetNbReseaux > 1) then begin
        for i:=1 to GetNbReseaux - 1 do begin
          R:=GetReseau(i);
          WrtLn(Format('%d'+TAB+'%d'+TAB+
                       '%d'+TAB+'%d'+TAB+'%d'+TAB+
                       '%d'+TAB+
                       '%s'+TAB+
                       '%s',
                       [-8,
                        R.IdxReseau,
                        GetRValue(R.ColorReseau),
                        GetGValue(R.ColorReseau),
                        GetBValue(R.ColorReseau),
                        R.TypeReseau,
                        R.NomReseau,
                        R.ObsReseau
                        ]));
        end;
      end;
    end; //  if (ModeSaveTAB = mtabEXTENDEDTAB) then begin
    // Classeur (débrayé)

    //WriteLn(pTAB, Format('%d'+TAB+'%d'+TAB+'%d'+TAB+'%d'+TAB+'%d',[-7,1,1,1,1]));
    //WrtLn(Format('%d'+TAB+'%d'+TAB+'%d'+TAB+'%d'+TAB+'%d',[-7,1,1,1,1]));
    // Sauvegarde des entrées
    WrtLn('');
    WrtCommentaire('Entrances list');
    WrtLn('');
    AfficherMessage('--> Saving #-6 Entrances section');
    for i:=0 to GetNbEntrees - 1 do
    begin
      Entrance:=GetEntree(i);
      WrtLn(Format('%d'+TAB+'%d'+TAB+'%s', [-6, i+1, Entrance.eNomEntree]));
    end;
    AfficherMessage('--> Saving #-5 Entrances section');
    for i:=0 to GetNbEntrees - 1 do
    begin
      Entrance:=GetEntree(i);
      WrtLn(Format('%d'+TAB+'%d'+TAB+
                           '%.2f'+TAB+'%.2f'+TAB+'%.2f'+TAB+
                           '%d'+TAB+'%d'+TAB+'%s',
                          [-5, i+1,
                          Entrance.eXEntree,
                          Entrance.eYEntree,
                          Entrance.eZEntree,
                          Entrance.eRefSer,
                          Entrance.eRefSt,
                          Entrance.eObserv
                          ]));
    end;
    WrtLn('');
    WrtCommentaire('Trips list');
    WrtLn('');
    AfficherMessage('--> Saving Expes');
    if (GetNbExpes > 0) then
    begin
      for i:=1 to GetNbExpes - 1 do begin
        Expe:=GetExpe(i);
        with Expe do begin
          //WriteLn(pTAB, Format('%d'+TAB+'%d'+TAB+
          WrtLn(Format('%d'+TAB+'%d'+TAB+
                             '%d'+TAB+'%d'+TAB+'%d'+TAB+
                             '%s'+TAB+'%s'+TAB+
                             '%d'+TAB+'%.2f'+TAB+
                             '%d'+TAB+'%d'+TAB+ // anciennement '%f'+TAB+'%d'+TAB+
                             '%s',
                             [-2, IDExpe,
                              JourExpe, MoisExpe, AnneeExpe,
                              Speleometre, Speleographe,
                              ModeDecl, Declinaison,
                              0, Couleur, //Inclinaison, Couleur,
                              Commentaire
                             ]));
         end;
      end;
    end;
    // Codes
    //-1	999	400.00	400.00	0.00	0.00	0.00	100.00	-100.00	Fixpunkt
    AfficherMessage('--> Saving Codes');
    WrtLn('');
    WrtCommentaire('Instruments codes list');
    WrtLn('');

    if (GetNbCodes > 1) then
    begin
      for i:=1 to GetNbCodes - 1 do begin
        Code:=GetCode(i); // Laisser tel quel
        with Code do begin
          case ModeSaveTAB of
            mtabEXTENDEDTAB:
              WrtLn(Format(LINE_CODE+TAB+'%d',
                             [-1, IDCode,
                                  GradAz, GradInc,
                                  PsiL, PsiAz, PsiP,
                                  FactLong, AngLimite,
                                  Commentaire,
                                  TypeGalerie
                             ]));
            mtabTOPOROBOT:
              WrtLn(Format(LINE_CODE,
                             [-1, IDCode,
                                  GradAz, GradInc,
                                  0.10, 1.0, 1.0, //PsiL, PsiAz, PsiP,
                                  100.00, 0.00, //FactLong, AngLimite,
                                  SafeTruncateString(Commentaire, 50)
                             ]));
          end;
        end; //with Code
      end;
      WrtLn('');
    end;
    // Séries
    case ModeSaveTAB of
      mtabEXTENDEDTAB:
        AfficherMessage('--> Saving Series at mtabEXTENDEDTAB mode');
      mtabTOPOROBOT:
        AfficherMessage('--> Saving Series at mtabTOPOROBOT mode');
    end;
    WrtLn('');
    WrtCommentaire('Series and stations');
    WrtLn('');


    for Sr:=1 to GetNbSeries - 1 do
    begin
      WrtLn('');
      Serie := GetSerie(Sr);
      AfficherMessage(Format('---> %d : %s (%d pts)',[Serie.GetIndexSerie, Serie.GetNomSerie, Serie.GetNbVisees]));
      with Serie do begin
        // ligne de tête
        // Numero est remplacé par IndexSerie
        // /!\ 'NbPoints' est remplacé par 'NbPoints - 1'
        case ModeSaveTAB of
          mtabEXTENDEDTAB:
            //WriteLn(pTAB, Format('%d'+TAB+'%d'+TAB+
            WrtLn(Format('%d'+TAB+'%d'+TAB+
                                 '%d'+TAB+'%d'+TAB+
                                 '%d'+TAB+'%d'+TAB+
                                 '%d'+TAB+
                                 '%d'+TAB+'%d'+TAB+
                                 '%s'+TAB+'%s'+TAB+
                                 '%d'+TAB+'%.2f',
                                 [GetIndexSerie, -1,
                                  GetNoSerieDep, GetNoPointDep,
                                  GetNoSerieArr, GetNoPointArr,
                                  GetNbVisees - 1,
                                  GetChance, GetObstacle,
                                  GetNomSerie,
                                  GetObsSerie,
                                  GetNoReseau,
                                  GetRaideur
                                 ]));

          mtabTOPOROBOT:
            //WriteLn(pTAB, Format('%d'+TAB+'%d'+TAB+
            WrtLn(Format('%d'+TAB+'%d'+TAB+
                                 '%d'+TAB+'%d'+TAB+
                                 '%d'+TAB+'%d'+TAB+
                                 '%d'+TAB+
                                 '%d'+TAB+'%d'+TAB+
                                 '%s', //+TAB+'%s',
                                 [GetIndexSerie, -1,
                                  GetNoSerieDep, GetNoPointDep,
                                  GetNoSerieArr, GetNoPointArr,
                                  GetNbVisees - 1,
                                  GetChance, GetObstacle,
                                  SafeTruncateString(GetNomSerie, 20)
                                 ]));

        end;

        for St:=0 to Serie.GetNbVisees - 1 do
        begin
          Station:=Serie.GetVisee(St);
          with Station do begin
            case ModeSaveTAB of
              mtabEXTENDEDTAB: begin

                WrtLn(Format(LINE_STATION+TAB+'%s'+TAB+'%d',
                                [Serie.GetIndexSerie, St,
                                 Code, Expe,
                                 Longueur, Azimut, Pente,
                                 LG, LD, HZ, HN,
                                 Commentaires,
                                 IDTerrainStation,
                                 TypeVisee
                             ]));
                end;
              mtabTOPOROBOT: begin

                WrtLn(Format(LINE_STATION,
                                [Serie.GetIndexSerie, St,
                                 Code, Expe,
                                 Longueur, Azimut, Pente,
                                 LG, LD, HZ, HN,
                                 Commentaires
                             ]));
                end;
            end;
          end;
        end;

      end;
    end;
    AfficherMessage('TToporobotStructure.SaveToFile OK');
  finally
    CloseFile(pTAB);
  end;
end;


//* Méthodes relatives aux entrées
procedure TToporobotStructure2012.AddEntree(const AEntree: TEntrance);
var
  pEntree: ^TEntrance;
begin
  New(pEntree);
  pEntree^:=AEntree;
  FTableEntrees.Add(pEntree);
end;
function TToporobotStructure2012.GetEntree(const NumEntree: integer): TEntrance;
var
  pEntree: ^TEntrance;
begin
  pEntree:=FTableEntrees.Items[NumEntree];
  Result:=pEntree^;
end;
procedure TToporobotStructure2012.PutEntree(const NoEntree: integer;
                                            const AEntree: TEntrance);
var
  pEntree: ^TEntrance;
begin
  pEntree := FTableEntrees.Items[NoEntree];
  pEntree^:=AEntree;
  FTableEntrees.Items[NoEntree]:=pEntree;
end;

function TToporobotStructure2012.RemoveEntree(const Idx: integer): boolean;
begin
  Result := False;
  try
    Dispose(FTableEntrees.Items[Idx]);
    FTableEntrees.Delete(Idx);
    Result := True;
  except
  end;
end;


function TToporobotStructure2012.GetNbEntrees: integer;
begin
  Result := FTableEntrees.Count;
end;

function TToporobotStructure2012.FindIdxEntranceByText(const S: string
  ): integer;
begin
  Result := -1;
end;

// méthodes relatives aux expés
//------------------------------------------
procedure TToporobotStructure2012.AddExpe(const LaExpe: TExpe);
var
  UneExpe  : ^TExpe;
begin
  New(UneExpe);
  UneExpe^:=LaExpe;
  FTableExpes.Add(UneExpe);
end;
procedure TToporobotStructure2012.PutExpe(const NoExpe: integer;
                                      const LaExpe: TExpe);
var
  pUneExpe: ^TExpe;
begin
  pUneExpe := FTableExpes.Items[NoExpe];
  pUneExpe^:=LaExpe;
  FTableExpes.Items[NoExpe]:=pUneExpe;
end;
function TToporobotStructure2012.GetExpe(const NumExpe: integer): TExpe;
var
  UneExpe  : ^TExpe;
begin
  UneExpe:=FTableExpes.Items[NumExpe];
  Result:=UneExpe^;
end;
function  TToporobotStructure2012.GetExpeByIndex(const Idx: integer): TExpe;
var
  ii: integer;
  E : TExpe;
begin
  for ii:=0 to FTableExpes.Count-1 do begin
    E:=self.GetExpe(ii);
    if E.IDExpe = Idx then
      Break;
  end;
  Result:=E;
end;

function TToporobotStructure2012.GetNbExpes: integer;
begin
  Result := FTableExpes.Count;
end;

function TToporobotStructure2012.FindIdxExpeByText(const S: string): integer;
begin
  Result := -1;
end;

function TToporobotStructure2012.RemoveExpe(const NoExpe: integer): boolean;
begin
  Result := True;
  try
    Dispose(FTableExpes.Items[NoExpe]);
    FTableExpes.Delete(NoExpe);
    Result := True;
  except
  end;
end;
// méthodes relatives aux codes
//------------------------------------------
procedure TToporobotStructure2012.AddCode(const LeCode: TCode);
var
  pUnCode: ^TCode;
begin
    New(pUnCode);
    pUnCode^:=LeCode;
    FTableCodes.Add(pUnCode);
end;
function TToporobotStructure2012.GetCode(const NoCode: integer): TCode;
var
  pUnCode: ^TCode;
begin
  //New(pUnCode);
  pUnCode:=FTableCodes.Items[NoCode];
  Result:=pUnCode^;
end;
//------------------------------------------
function  TToporobotStructure2012.GetCodeByIndex(const Idx: integer): TCode;
var
  ii: integer;
  C : TCode;
begin
  for ii:=0 to FTableCodes.Count-1 do begin
    C:=self.GetCode(ii);
    if C.IDCode = Idx then
      Break;
  end;
  Result:=C;
end;
procedure TToporobotStructure2012.PutCode(const NoCode: integer;
                                      const LeCode: TCode);
var
  pUnCode: ^TCode;
begin
  //New(pUnCode);     Désactivé ou modifié le 19/02/09
  pUnCode := FTableCodes.Items[NoCode];
  pUnCode^:=LeCode;
  FTableCodes.Items[NoCode]:=pUnCode;
end;

function TToporobotStructure2012.RemoveCode(const NoCode: integer): Boolean;
begin
  Result := False;
  try
    Dispose(FTableCodes.Items[NoCode]);
    FTableCodes.Delete(NoCode);
    Result := True;
  except
  end;
end;
function TToporobotStructure2012.GetNbCodes: integer;
begin
  Result := FTableCodes.Count;
end;

function TToporobotStructure2012.FindIdxCodeByText(const S: string): integer;
begin
  Result := -1;
end;

// méthodes relatives aux réseaux
procedure TToporobotStructure2012.AddReseau(const AReseau: TReseau);
var
  pReseau: ^TReseau;
begin
  New(pReseau);
  pReseau^:=AReseau;
  FTableReseaux.Add(pReseau);
end;


function TToporobotStructure2012.GetReseau(const NumReseau: integer): TReseau;
var
  pReseau: ^TReseau;
begin
  pReseau:=FTableReseaux.Items[NumReseau];
  Result:=pReseau^;
end;

procedure TToporobotStructure2012.PutReseau(const NoReseau: integer;
                                        const AReseau: TReseau);
var
  pReseau: ^TReseau;
begin
  pReseau := FTableReseaux.Items[NoReseau];
  pReseau^:=AReseau;
  FTableReseaux.Items[NoReseau]:=pReseau;
end;

function TToporobotStructure2012.RemoveReseau(const Idx: integer): boolean;
begin
  Result := False;
  try
    Dispose(FTableReseaux.Items[Idx]);
    FTableReseaux.Delete(Idx);
    Result := True;
  except
  end;
end;

function TToporobotStructure2012.GetNbReseaux: integer;
begin
  Result := FTableReseaux.Count;
end;

function TToporobotStructure2012.FindIdxReseauByText(const S: string): boolean;
begin

end;

// méthodes relatives aux antennes
procedure TToporobotStructure2012.AddViseeAntenne(const VA: TViseeAntenne);
var
  pVA: ^TViseeAntenne;
begin
  New(pVA);
  pVA^:=VA;
  FTableViseesAntenne.Add(pVA);
end;

function  TToporobotStructure2012.GetViseeAntenne(const No: integer): TViseeAntenne;
var
  pVA: ^TViseeAntenne;
begin
  pVA:=FTableViseesAntenne.Items[No];
  Result:=pVA^;
end;
procedure TToporobotStructure2012.PutViseeAntenne(const No: integer; const VA: TViseeAntenne);
var
  pVA: ^TViseeAntenne;
begin
  pVA := FTableViseesAntenne.Items[No];
  pVA^:= VA;
  FTableViseesAntenne.Items[No] := pVA;
end;

function TToporobotStructure2012.RemoveViseeAntenne(const No: integer
  ): boolean;
begin
  Result := false;
  try
    Dispose(FTableViseesAntenne.Items[No]);
    FTableViseesAntenne.Delete(No);
    Result := True;
  except
  end;
end;


function TToporobotStructure2012.GetNbAntennes: integer;
begin
  Result := FTableViseesAntenne.Count;
end;

function TToporobotStructure2012.ExistsEntrance(const Idx: integer): boolean;
var
  i: integer;
  EWE: TEntrance;
begin
  Result := False;
  for i:= 0 to self.GetNbEntrees - 1 do
  begin
    EWE := self.GetEntree(i);
    if (EWE.eNumEntree = Idx) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function TToporobotStructure2012.ExistsReseau(const Idx: integer): boolean;
var
  i: integer;
  EWE: TReseau;
begin
  Result := False;
  for i:= 0 to self.GetNbReseaux - 1 do
  begin
    EWE := self.GetReseau(i);
    if (EWE.IdxReseau = Idx) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function TToporobotStructure2012.ExistsCode(const Idx: integer): boolean;
var
  i: integer;
  EWE: TCode;
begin
  AfficherMessage(Format('%s.ExistsCode(%d)', [ClassName, Idx]));
  Result := False;
  for i:= 0 to self.GetNbCodes - 1 do
  begin
    EWE := self.GetCode(i);
    if (EWE.IDCode = Idx) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function TToporobotStructure2012.ExistsExpe(const Idx: integer): boolean;
var
  i: integer;
  EWE: TExpe;
begin
  AfficherMessage(Format('%s.ExistsExpe(%d)', [ClassName,Idx]));
  Result := False;
  for i:= 0 to self.GetNbExpes - 1 do
  begin
    EWE := self.GetExpe(i);
    if (EWE.IDExpe = Idx) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function TToporobotStructure2012.ExistsSerie(const Idx: integer): boolean;
var
  i: integer;
  EWE: TObjSerie;
begin
  Result := False;
  for i:= 0 to self.GetNbSeries - 1 do
  begin
    EWE := self.GetSerie(i);
    if (EWE.GetIndexSerie = Idx) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

//***********************************
// recherche de l'index correspondant à une série
function TToporobotStructure2012.GetIdxSerie(const NumeroSerie: integer): integer;
var
  i: integer;
  Ser: TObjSerie;
begin
  Result:=-1;
  for i:=0 to GetNbSeries - 1 do
  begin
    Ser:=GetSerie(i);
    if (Ser.GetIndexSerie = NumeroSerie) then begin
      Result:=i;
      Break;
    end;
  end;
end;
// recherche du couple série station d'après ID littéral
function  TToporobotStructure2012.FindPtTopo(var Cle: string; var Sr, Pt: integer): boolean;
var
  //Clef: string;
  S, P: integer;
  Q: string;
  Ser: TObjSerie;
  St : TUneVisee;
begin
  Cle := UpperCase(Trim(Cle));
  Sr := -1;
  Pt := -1;
  Result := False;
  for S := 0 to GetNbSeries - 1 do begin
    Ser := GetSerie(S);
    for P := 0 to Ser.GetNbVisees - 1 do begin
      St := Ser.GetVisee(P);
      Q  := UpperCase(Trim(St.IDTerrainStation + '|' + ST.Commentaires));

      if Pos(Cle,  Q) > 0 then begin
         Sr := Ser.GetIndexSerie;
         Pt := P;
         Cle := Q;
         Result := True;
         Exit;
      end;
    end;
  end;
end;
// Infos sur les données topo
// ne nécessite pas de calcul topo
// c'est du fichier de log -> messages en anglais uniquement
function TToporobotStructure2012.GetCaveInfoSeries(const FichierTXT: string): integer;
const
  FMTLNENTREE = '%d'+TAB+'%.2f'+TAB+'%.2f'+TAB+'%.2f'+TAB+'%s';
  FMTLNSER    = '%d'+TAB+'%d'+TAB+'%d'+TAB+'%d'+TAB+'%d'+TAB+'%-60s'+TAB+'%d'+
                 TAB+'%.2f'+TAB+'%.2f';
var
  i, n: integer;
  F: TextFile;

  E: TEntrance;
  Ser: TObjSerie;
  V: TUneVisee;
  D1, DevTotal: double;
  VolumeTotal : double;
  VolumeSerie : double;
  NbPtsTotal: Integer;
  procedure WrtLin(const S: string);
  begin
    WriteLn(F,S);
  end;
begin
  Result:=-1;
  AfficherMessage(Format('%s.GetCaveInfoSeries(%s)',[ClassName, FichierTXT]));
  AssignFile(F, FichierTXT);
  try
    try
      ReWrite(F);
      WrtLin(Format('File: %s created  %s - %s',
                    [FichierTXT, DateToStr(Now), TimeToStr(Now)]));
      WrtLin('');
      WrtLin('Database : ' + FDatabaseName);
      WrtLin('');
      WrtLin('Survey: ' + GetNomEtude);
      WrtLin('');
      WrtLin(Format('List of %d entrances', [GetNbEntrees]));
      WrtLin('');
      for i:=0 to GetNbEntrees - 1 do begin
        E:=GetEntree(i);
       // E.eXEntree;
        WrtLin(Format(FMTLNENTREE,
                      [i, E.eXEntree, E.eYEntree, E.eZEntree,
                       E.eNomEntree]));
      end;
      WrtLin('');
      WrtLin(Format('List of %d series', [GetNbSeries]));
      WrtLin('');
      WrtLin('No'+TAB+'S.Dep'+TAB+'Pt.Dep'+TAB+'S.Arr'+TAB+'Pt.Arr'+TAB+
             'Serie name'+TAB+'Nb points'+TAB+'Length'+
             TAB + 'Estimated volume');
      WrtLin('');
      DevTotal:=0.00;
      NbPtsTotal:=0;
      VolumeTotal:=0.00;
      for i:=0 to GetNbSeries - 1 do
      begin
        Ser:=GetSerie(i);
        // calcul de la longueur, de la surface et du volume de la série
        D1:=0.00;
        VolumeSerie:=0.00;
        for n:=0 to Ser.GetNbVisees - 1 do begin
          V:=Ser.GetVisee(n);
          // les sections considérées pour les volumes
          // sont des ellipses
          case V.TypeVisee of
            tgDEFAULT, tgFOSSILE, tgVADOSE, tgENNOYABLE, tgSIPHON: // galeries naturelles
            begin;
              D1          += V.Longueur; // X += N -> X := X + N
              VolumeSerie += PI * (V.HZ+V.HN) * (V.LD + V.LG) * V.Longueur;
            end;
          else
            ;
          end;
        end;
        VolumeTotal:=VolumeTotal + VolumeSerie;
        DevTotal:=DevTotal+D1;
        NbPtsTotal:=NbPtsTotal + Ser.GetNbVisees - 1;
        WrtLin(Format(FMTLNSER,
                      [Ser.GetIndexSerie,
                       Ser.GetNoSerieDep, Ser.GetNoPointDep,
                       Ser.GetNoSerieArr, Ser.GetNoPointArr,
                       Ser.GetNomSerie,
                       Ser.GetNbVisees - 1,
                       D1, VolumeSerie]));
      end;  // for i:=0 to TableSeries.Count-1
      WrtLin('');
      WrtLin(Format('Total length: %.2f m (%d stations) - Estimated Volume: %.1f m3',
                    [DevTotal, NbPtsTotal, VolumeTotal]));
      Result:=1;
    except
    end;
  finally
    CloseFile(F);
  end;
end;

// l'export vers Visual Topo est désormais du ressort de cette classe
// Difficultés en raison de l'intolérance de VTopo
// Le réseau SAKANY passe le test sans modifications
procedure TToporobotStructure2012.ExportVisualTopo(const FichierTRO: string);
const
  VTOPO_VERSION ='Version 4.9';
  FMTSERSTVTOPO ='%d.%d';
var
  pTRO: TextFile;
  Palette256: TPalette256;
  Entree: TEntrance;
  DefautCouleur: TColor;
  Serie: TObjSerie;
  CurrStation, PrevStation: TUneVisee;
  FromStationID, ToStationID: String;
  S777: String;
  S666: String;
  NoSer: Integer;
  NoSt: Integer;
  Code: TCode;
  Expe: TExpe;
  procedure WriteLnParam(const C: TCode; const E: TExpe);
  const
    DEG_DEC = 'Degd';
    GRADS   = 'Gra';
  var
    UB, UC, Clino: string;
    CC: TColor;
    Decl: Extended;
  begin
    writeLn(pTRO,'');
    //Param Deca Deg Clino Deg 1.0300 Dir,Dir,Dir Std
    case Trunc(C.GradAz) of
      360: UB:='Deg';
      400: UB:='Gra';
    else
      UB:='Deg';
    end;
    case Trunc(C.GradInc) of
       360: begin
              Clino:='Clino';
              UC   := DEG_DEC;
            end;
       361: begin
              Clino:= 'Vulc';
              UC   := DEG_DEC;
            end;
       400: begin
              Clino:='Clino';
              UC   := GRADS;
            end;
       401: begin
              Clino:='Vulc';
              UC   := GRADS;
            end;
       380: begin
              Clino:='Deniv'; // anciennement: Prof
              UC   :='';
            end;
    end;
    Decl:=(E.Declinaison/10) * 400.0 / C.GradAz;
    CC:=Palette256.GetColorByIndex(E.Couleur);
    //if FDefaultColor then CC:=lbColorReseau.Color;
    WriteLn(pTRO, Format('Param Deca %s %s %s %.4f Dir,Dir,Dir %d,%d,%d',
                          [UB, Clino, UC,
                           Decl,
                           GetRValue(CC),
                           GetGValue(CC),
                           GetBValue(CC)
                          ]));
    writeln(pTRO,'');
  end;
begin
  AfficherMessage(Format('%s.ExportVisualTopo(%s)',[ClassName, FichierTRO]));
  // démarrage de la palette
  Palette256 := TPalette256.Create;
  // ouverture fichier VTopo
  AssignFile(pTRO, FichierTRO);
  try
    Palette256.GenerateTOPOROBOTPalette;
    ReWrite(pTRO);
    WriteLn(pTRO, VTOPO_VERSION);
    WriteLn(pTRO, '');

    // Entrée
    Entree := GetEntree(0);
    WriteLn(pTRO, Format('Club %s',['toto']));
    // Trou CDS03,0.000,0.000,0,
    WriteLn(pTRO, Format('Trou %s,%.3f,%.3f,%f,', [Entree.eNomEntree, Entree.eXEntree, Entree.eYEntree, Entree.eZEntree]));
    WriteLn(pTRO, Format('Entree %s',[Entree.eNomEntree]));
    DefautCouleur:= clRED;
    WriteLn(pTRO, Format('Couleur %d,%d,%d',[
                         GetRValue(DefautCouleur),
                         GetGValue(DefautCouleur),
                         GetBValue(DefautCouleur)]));
    // première ligne de données
    WriteLn(pTRO,'Param Deca Deg Clino Deg 0.0000 Dir,Dir,Dir Std');
    Serie:=GetSerie(1);
    WriteLn(pTRO, Format('%10s %10s %.1f   %.1f %.1f %.1f %.2f %.2f %.2f %s',
                               [Format(FMTSERSTVTOPO,[Serie.GetNoSerieDep, Serie.GetNoPointDep]),
                                Format(FMTSERSTVTOPO,[Serie.GetNoSerieDep, Serie.GetNoPointDep]),
                                0.00,
                                0.00,
                                0.00,
                                1.00,
                                1.00,
                                1.00,
                                1.00,
                                'Entrée 1.0']));
    // exportation proprement dite
    for NoSer:=1 to GetNbSeries - 1 do
    begin
      Serie:=GetSerie(NoSer);
      AfficherMessage(Format('--> Export serie: %d - %s',[Serie.GetIndexSerie, Serie.GetNomSerie]));
      FromStationID:=Format(FMTSERSTVTOPO,[Serie.GetNoSerieDep, Serie.GetNoPointDep]);
      for NoSt:=1 to Serie.GetNbVisees - 1 do
      begin
        CurrStation := Serie.GetVisee(NoSt);
        Code:=GetCodeByIndex(CurrStation.Code);
        Expe:=GetExpeByIndex(CurrStation.Expe);
        if ((NoSer=1) and (NoSt=1)) then WriteLnParam(Code, Expe);
        if (CurrStation.Code <> PrevStation.Code) OR
           (CurrStation.Expe <> PrevStation.Expe)
        then WriteLnParam(Code, Expe);
        ToStationID:=Format(FMTSERSTVTOPO,[Serie.GetIndexSerie, NoSt]);
        if (NoSt = Serie.GetNbVisees - 1) then
           ToStationID:=Format(FMTSERSTVTOPO,[Serie.GetNoSerieArr, Serie.GetNoPointArr]);
        if (Trim(CurrStation.Commentaires)='')
        then S777 :=''
        else S777 := Format('%s',[CurrStation.Commentaires]);
        if (Trim(CurrStation.IDTerrainStation) ='')
        then S666 := ''
        else S666:=Format('[%s]',[CurrStation.IDTerrainStation]);
        if (Trim(CurrStation.Commentaires)='')    and
           (Trim(CurrStation.IDTerrainStation)='')
        then
          S777 := ''
        else
          S777 := ';' + S666 + S777 + ';';
        WriteLn(pTRO, Format('%10s %10s %.1f   %.1f %.1f %.1f %.2f %.2f %.2f N %s',
                             [FromStationID,
                              ToStationID,
                              CurrStation.Longueur,
                              CurrStation.Azimut,
                              CurrStation.Pente,
                              CurrStation.LG,
                              CurrStation.LD,
                              CurrStation.HZ,
                              CurrStation.HN,
                              S777]));
        PrevStation    := CurrStation;
        FromStationID  := ToStationID;
      end;
    end;
    // footpage
    WriteLn(pTRO,'');
    WriteLn(pTRO,'[Configuration 4.9]');
    WriteLn(pTRO,'');
    WriteLn(pTRO,'Visual Topo=2,3,-1,-1,-1,-1,1,1,801,610');
    WriteLn(pTRO,'Options=1,1');
    WriteLn(pTRO,'Calcul=0,1,-1,-1,-1,-1,22,22,844,449');
    WriteLn(pTRO,'Options=26,1,0,1,0');
    WriteLn(pTRO,'ExportDxf=0,100,391,1,6,7,4,4,3,3,2,7,9');
    WriteLn(pTRO,'Colonnes=8.00,8.00,8.00,8.00,8.00,8.00,8.00,8.00,8.00,'+
                 '8.00,8.00,2.00,4.00,0.38,8.00,8.00,8.00,8.00,8.00,'+
                 '0.38,0.00,0.00');
    WriteLn(pTRO,'Options=384,1.00,4,0,2,6914,1');
    AfficherMessage(Format('%s.ExportVTOPO(%s) OK',[ClassName, FichierTRO]));
  finally
    CloseFile(pTRO);
    Palette256.Free;
  end;
end;
// calcul des déclinaisons magnétiques
procedure TToporobotStructure2012.CalculerDeclinaisonsMagnetiques(const DoReplaceManualValues: boolean);
var
  DC: TCalculDeclinaisonMagnetique;
  i: Integer;
  EX: TExpe;
  PTout: TPoint2Df;
  Declin: Double;
  WU: Double;
begin
  AfficherMessage(Format('%s.CalculerDeclinaisonsMagnetiques()',[ClassName]));
  DC := TCalculDeclinaisonMagnetique.Create;
  try
    if (not DC.DemarrerDLL) then
    begin
      AfficherMessage('-- Echec de démarrage du calculateur de déclinaisons magnétiques');
      DC.StopperDLL;
      Exit;
    end;
    DC.Initialiser;
    if (not ConversionCoordonneesIsolees(FSystemeDeCoordonneesEPSG, 'WGS84',
                                         self.GetDefaultCoordX, self.GetDefaultCoordY,
                                         PTout)) then
    begin
      AfficherMessage('-- Convertisseur de coordonnées HS --');
      Exit;
    end;
    AfficherMessage(Format('--> Base Coords: %.2f %.2f %.2f > %.8f, %.8f',
                          [self.GetDefaultCoordX, self.GetDefaultCoordY, self.GetDefaultCoordZ,
                           PTout.X, PTout.Y]));


    for i := 0 to self.GetNbExpes - 1 do
    begin
      EX := self.GetExpe(i);
      WU := DC.CalculerDeclimag(PTout.X, PTout.Y, self.GetDefaultCoordZ,
                                EX.JourExpe, EX.MoisExpe, GetAnneeISO(EX.AnneeExpe));
      Declin := GetTOPOROBOTDecliMag(WU);
      EX.Declinaison := Declin;
      EX.ModeDecl    := 0;
      PutExpe(i, EX);
      AfficherMessage(Format('%d: %0.2d/%0.2d/%0.4d - %.6f',[i, EX.JourExpe, EX.MoisExpe, GetAnneeISO(EX.AnneeExpe), Declin]));
    end;
    DC.StopperDLL;
  finally
    DC.Free;
  end;
end;
// TODO: Implanter le contrôle des azimuts et pentes
function TToporobotStructure2012.CheckAzimutByIndex(const Idx: integer; const QAz: double; out MsgErr: string): boolean;
var
  CC: TCode;
  EWE: Boolean;
begin
  MsgErr := '';
  Result := False;
  if (Not ExistsCode(Idx)) then
  begin
    MsgErr := rsMSG_CODE_NOT_FOUND;
  end
  else
  begin
    CC     := GetCodeByIndex(Idx);
    EWE    := IsInRange(QAz, 0.00, CC.GradAz);
    MsgErr := IIF(EWE, '', Utf8ToAnsi(Format(rsMSG_ANGLE_OUT_OF_RANGE, ['Azimut', 0.00, CC.GradAz])));
    Result := EWE;
  end;
end;

function TToporobotStructure2012.CheckPenteByIndex(const Idx: integer; const QP: double; out MsgErr: string): boolean;
var
  CC: TCode;
  EWE: Boolean;
begin
  MsgErr := '';
  Result := False;
  if (Not ExistsCode(Idx)) then
  begin
    MsgErr := rsMSG_CODE_NOT_FOUND;
  end
  else
  begin
    CC := GetCodeByIndex(Idx);
    Result := True;
  end;
end;

// Export vers Thérion
// Pas encore validé
procedure TToporobotStructure2012.ExportVersTherion(const FichierTH: string);
const FMTSTS = 's%d_%d';
var
  THF      : TextFile;
  MySerie  : TObjSerie;
  MyFixPt  : TEntrance;
  // Station de rattachement courante
  MaStationDeRattachement: string;
  i           : integer;
  procedure WrtLn(const Line: string);
  begin
    WriteLn(THF, Line);
  end;
  // définition d'un en-tete de séance
  procedure DefineSeance(const E: TExpe);
  begin
    WrtLn(Format('       # Seance %d: %s',[E.IDExpe, E.Commentaire]));
    WrtLn(Format('       date %.4d.%.2d.%.2d',[ E.AnneeExpe,  E.MoisExpe,  E.JourExpe]));
    WrtLn('       #-------------------');
  end;
  // definition d'un code - Valeur de retour par défaut: -1, sinon, un angle droit
  function DefineCode(const C: TCode): double;
  var
    UnitCompass   : string;
    UnitClino     : string;
    IsBackCompass : string;
    IsBackClino   : string;
    InstrVertical : string; // mesures altimétriques
    DepthMode     : boolean; // si DepthMode=True, ne pas spécifier les unités de clino
    InstrLongueur : string;
    ModeMesures   : string; // normal = longueurs; diving = profondimètres

  begin
    Result := 0.00;
    WrtLn(Format('       # Code %d: %s',[C.IDCode, C.Commentaire]));
    InstrLongueur := 'tape';
    ModeMesures   := 'normal';
    // unités de la boussole
    case Trunc(C.GradAz) of
      359, 360: begin // visées directes en degrés
                  UnitCompass := 'degrees';
                  IsBackCompass :='';
                end;
      399, 400: begin // visées directes en grades
                  UnitCompass := 'grads';
                  IsBackCompass :='';
                end;
      349, 350: begin // visées inverses en degrés
                  UnitCompass := 'degrees';
                  IsBackCompass :='back';
                end;
      389, 390: begin // visées inverses en grades
                  UnitCompass := 'grads';
                  IsBackCompass :='back';
                end;
    else begin
        UnitCompass    := 'degrees';
        IsBackCompass  := '';
      end;
    end;

    WrtLn('       units compass ' + UnitCompass);
    // unités du clinomètres
    InstrVertical := 'clino';
    IsBackClino   := '';
    DepthMode     := false;
    case Trunc(C.GradInc) of
      350: begin // zéro à l'horizontale degré inverse
             UnitClino   := 'degrees';
             IsBackClino := 'back';
           end;
      390: begin // zéro à l'horizontale grade inverse
             UnitClino := 'grads';
             IsBackClino := 'back';
           end;
      360: begin // zéro à l'horizontale degré direct
             UnitClino := 'degrees';
           end;
      400: begin // zéro à l'horizontale grade direct
             UnitClino := 'grads';
           end;
      361: begin // zéro zénithal degré
             UnitClino := 'degrees';
             Result    :=  90.00;
           end;
      401: begin // zéro zénithal grade
             UnitClino := 'grads';
             Result    := 100.00;
           end;
      359: begin // zéro nadiral degré
             UnitClino := 'degrees';
             Result    := -90.00;
           end;
      399: begin // zéro nadiral grade
             UnitClino := 'grads';
             Result    := -100.00;
           end;
      370: begin // pourcentages

           end;
      380: begin // différences d'altitudes
             InstrVertical := 'depthchange';
             ModeMesures   := 'diving';
             DepthMode     := True;
           end;
    else begin
        UnitClino   := 'degrees';
        IsBackClino :='';
      end;
    end;
    if (Not DepthMode) then WrtLn('       units clino ' + UnitClino);
    WrtLn(Format('       data %s from to %s %scompass %s%s right left ceiling floor',
          [ModeMesures,
           InstrLongueur,
           IsBackCompass,
           IsBackClino,
           InstrVertical
          ]));
    WrtLn('       #-------------------');
    WrtLn('');
  end;
  // définition d'une série
  procedure DefineSerie(const S: TObjSerie; const N: integer; const StationDeRattachement: string);

  var
    foo, bar : string;
    Qdr, Incl: double;
    MyExpe   : TExpe;
    MyCode   : TCode;
    MyStation: TUneVisee;
    St       : integer;
    OldIdxExpe  : integer;
    OldIdxCode   : integer;
    OldIDStation: string;
    CurIDStation: string;
    Tag          : string; // Commentaires et ID littéraux de stations
  begin
    //S.NomSerie
    AfficherMessage(Format('----> Serie %d: %s',[S.GetIndexSerie, S.GetNomSerie]));
    WrtLn('');
    WrtLn(Format('    # Serie: %d - %s',[S.GetIndexSerie, S.GetNomSerie]));
    WrtLn('    #========================');
    WrtLn('    centerline');
    WrtLn('');
    // stations
    OldIdxExpe  := -1;
    OldIdxCode  := -1;

    OldIDStation:= Format(FMTSTS, [S.GetNoSerieDep, S.GetNoPointDep]);
    // première station
    MyStation   := S.GetVisee(1);
    MyExpe      := GetExpeByIndex(MyStation.Expe);
    MyCode      := GetCodeByIndex(MyStation.Code);
    OldIdxExpe  := MyExpe.IDExpe;
    OldIdxCode  := MyCode.IDCode;
    DefineSeance(MyExpe);
    Qdr := DefineCode(MyCode);
    for St := 1 to S.GetNbVisees - 1 do
    begin
      MyStation := S.GetVisee(St);
      //if NoSt=Serie.PointsTopo.Count-1 then ToStationID:=Format(FMTSERSTVTOPO,[S.SerieArr, S.PtArr])
      if (St = S.GetNbVisees - 1) then
        CurIDStation := Format(FMTSTS, [S.GetNoSerieArr, S.GetNoPointArr])
      else
        CurIDStation := Format(FMTSTS, [S.GetIndexSerie, St]);
      MyExpe      := GetExpeByIndex(MyStation.Expe);
      MyCode      := GetCodeByIndex(MyStation.Code);
      if (MyExpe.IDExpe <> OldIdxExpe) then
      begin
        DefineSeance(MyExpe);
        OldIdxExpe  := MyExpe.IDExpe;
      end;

      if (MyCode.IDCode <> OldIdxCode) then
      begin
        Qdr := DefineCode(MyCode);
        //DefineSeance(MyExpe);
        OldIdxCode  := MyCode.IDCode;
      end;
      if Trunc(Qdr)=0 then
        Incl := MyStation.Pente
      else begin
        if (Qdr>1.00) then Incl := Qdr - MyStation.Pente
                      else Incl := MyStation.Pente + Qdr;
      end;
      // ID Littéraux
      Tag := '';
      if (MyStation.IDTerrainStation <> '') then
        Tag := Format('# Label: %s ',[MyStation.IDTerrainStation]);
      if (MyStation.Commentaires <> '') then
        Tag := Tag + Format('# Observ: %s', [MyStation.Commentaires]);

      WrtLn(Format('       %s %s %.2f %.2f %.2f  %.2f %.2f  %.2f %.2f  %s',
                   [OldIDStation, CurIDStation,
                    MyStation.Longueur, MyStation.Azimut, Incl,
                    MyStation.LG, MyStation.LD, MyStation.HZ, MyStation.HN,
                    Tag]));
      //--------------------------------------------------------------
      OldIDStation := CurIDStation;
    end;
    WrtLn('');
    WrtLn('    endcenterline');
    foo := Format(FMTSTS, [S.GetNoSerieDep, S.GetNoPointDep]);
    bar := Format('%s@SERIE%d',[foo, S.GetNoSerieDep]);
  end;
begin
  AfficherMessage(Format('%s.ExportToTherion(%s)',[ClassName, FichierTH]));
  AssignFile(THF, FichierTH);
  try
    ReWrite(THF);
    AfficherMessage('--> Header');
    // encodage
    WrtLn('encoding  iso8859-2');
    WrtLn('');
    // date et nom du dossier
    WrtLn(StringOfChar('#', 80));
    WrtLn('## Data file for Therion generated by GHTopo');
    WrtLn('## Folder: ' + FDatabaseName);
    WrtLn('## Date  : ' + DateTimeToStr(Now()));
    WrtLn(StringOfChar('#', 80));
    WrtLn('');
    // centerlines et consorts
    WrtLn(Format('survey Survey001 -title "%s"',[GetNomEtude]));
     // points fixes
    AfficherMessage('--> Fixed points');
    WrtLn('  # Section Entrances and Fixed points');
    WrtLn('  centerline');
    for i := 0 to GetNbEntrees -1 do begin
      MyFixPt := GetEntree(i);
      WrtLn(Format('    fix %s %.2f %.2f %.2f # %s',
                   [ Format(FMTSTS, [MyFixPt.eRefSer, MyFixPt.eRefSt]),
                     MyFixPt.eXEntree,
                     MyFixPt.eYEntree,
                     MyFixPt.eZEntree,
                     MyFixPt.eNomEntree
                   ]));
    end;
    WrtLn('  endcenterline');
    WrtLn('  # End section Entrances and Fixed points');
    WrtLn('  # Section SERIES');
    // séries
    AfficherMessage('--> Series');
    MaStationDeRattachement := '';
    for i := 1 to GetNbSeries-1 do begin
      MySerie := GetSerie(i);
      DefineSerie(MySerie, i, MaStationDeRattachement);
    end;
    WrtLn('  # End section SERIES');
    WrtLn('endsurvey');
  finally
    CloseFile(THF);
  end;
end;
// Lecture/écriture au format XML
// (futur format standard de GHTopo)
procedure TToporobotStructure2012.SaveToXML(const FichierXML: string);
const
  IDX_MINI = 0;
var
  MyDocXML: TXMLDocument;
  GHTopoRoot: TDOMElement;
  SectionGeneral: TDOMElement;
  SubSection: TDOMElement;
  i: Integer;
  E: TEntrance;
  SubSubSection: TDOMElement;
  SectionCodes: TDOMElement;
  C: TCode;
  SectionSeances: TDOMElement;
  EX: TExpe;
  SectionSeries: TDOMElement;
  SER: TObjSerie;
  SubSubSubSection: TDOMElement;
  ST: TUneVisee;
  S: Integer;
  SectionAntennes: TDOMElement;
  ANT: TViseeAntenne;
  SectionReseaux: TDOMElement;
  R: TReseau;
  SectionEntrances: TDOMElement;
  procedure KWYXZ(const S666: string);
  begin
    AfficherMessage(Format('-- Writing "%s" section', [S666]));
  end;

begin
  AfficherMessage(Format('%s.SaveToXML(%s)', [ClassName, FichierXML]));
  MyDocXML := TXMLDocument.Create;
  try
    // racine du document = nom du logiciel
    GHTopoRoot := MyDocXML.CreateElement('GHTopo');
    MyDocXML.AppendChild(GHTopoRoot);
    // section Général
    SectionGeneral := MyDocXML.CreateElement(GTX_KEY_SECTION_GENERAL);
      GHTopoRoot.AppendChild(SectionGeneral);
      SubSection := MyDocXML.CreateElement('Cavite');
      SubSection.SetAttribute('Name', GetNomEtude);
      SubSection.SetAttribute('Description', GetCommentairesEtude);
      SubSection.SetAttribute('Projection', GetSystemeCoordonnees);
      SectionGeneral.AppendChild(SubSection);
    // section Entrées
    SectionEntrances := MyDocXML.CreateElement(GTX_KEY_SECTION_ENTRANCES);
      GHTopoRoot.AppendChild(SectionEntrances);
      KWYXZ(GTX_KEY_SECTION_ENTRANCES);
      for i:= IDX_MINI to GetNbEntrees - 1 do
      begin
        E := GetEntree(i);
        SubSection := MyDocXML.CreateElement(GTX_KEY_ENTRANCE);
          SubSection.SetAttribute(GTX_ATTR_ENTRANCE_IDX      , Format('%d', [E.eNumEntree]));
          SubSection.SetAttribute(GTX_ATTR_ENTRANCE_NAME     , Format('%s', [E.eNomEntree]));
          SubSection.SetAttribute(GTX_ATTR_ENTRANCE_REFSERIE , Format('%d', [E.eRefSer]));
          SubSection.SetAttribute(GTX_ATTR_ENTRANCE_REFPT    , Format('%d', [E.eRefSt]));
          SubSection.SetAttribute(GTX_ATTR_ENTRANCE_X, Format('%.2f', [E.eXEntree]));
          SubSection.SetAttribute(GTX_ATTR_ENTRANCE_Y, Format('%.2f', [E.eYEntree]));
          SubSection.SetAttribute(GTX_ATTR_ENTRANCE_Z, Format('%.2f', [E.eZEntree]));

          SubSection.SetAttribute(GTX_ATTR_ENTRANCE_OBS, Format('%s', [E.eObserv]));
       SectionEntrances.AppendChild(SubSection);
      end;
    // section Réseaux
    SectionReseaux := MyDocXML.CreateElement(GTX_KEY_SECTION_RESEAUX);
      KWYXZ(GTX_KEY_SECTION_RESEAUX);
      GHTopoRoot.AppendChild(SectionReseaux);
      for i := IDX_MINI to GetNbReseaux - 1 do
      begin
        R := GetReseau(i);
        SubSection := MyDocXML.CreateElement(GTX_KEY_RESEAU);
          SubSection.SetAttribute(GTX_ATTR_RESEAU_IDX, Format('%d', [R.IdxReseau]));
          SubSection.SetAttribute(GTX_ATTR_RESEAU_COLOR, Format('%s', [ColorToHTMLColor(R.ColorReseau)]));
          SubSection.SetAttribute(GTX_ATTR_RESEAU_TYPE, Format('%d', [R.TypeReseau]));
          SubSection.SetAttribute(GTX_ATTR_RESEAU_NAME, Format('%s', [R.NomReseau]));
          SubSection.SetAttribute(GTX_ATTR_RESEAU_OBS, Format('%s', [R.ObsReseau]));
       SectionReseaux.AppendChild(SubSection);
      end;
    // section Codes
    SectionCodes := MyDocXML.CreateElement(GTX_KEY_SECTION_CODES);
      KWYXZ(GTX_KEY_SECTION_CODES);
      GHTopoRoot.AppendChild(SectionCodes);
      for i := IDX_MINI to GetNbCodes - 1 do
      begin

        C := GetCode(i);
        SubSection := MyDocXML.CreateElement(GTX_KEY_CODE);
          SubSection.SetAttribute(GTX_ATTR_CODE_NUMERO    , Format('%d', [C.IDCode]));
          SubSection.SetAttribute(GTX_ATTR_CODE_TYPE      , Format('%d', [C.TypeGalerie]));
          SubSection.SetAttribute(GTX_ATTR_CODE_FACT_LONG , Format('%.3f', [C.FactLong]));
          SubSection.SetAttribute(GTX_ATTR_CODE_UCOMPASS  , Format('%.2f', [C.GradAz]));
          SubSection.SetAttribute(GTX_ATTR_CODE_UCLINO    , Format('%.2f', [C.GradInc]));
          SubSection.SetAttribute(GTX_ATTR_CODE_ANGLIMITE , Format('%.2f', [C.AngLimite]));
          SubSection.SetAttribute(GTX_ATTR_CODE_PSI_L     , Format('%.3f', [C.PsiL]));
          SubSection.SetAttribute(GTX_ATTR_CODE_PSI_A     , Format('%.3f', [C.PsiAz]));
          SubSection.SetAttribute(GTX_ATTR_CODE_PSI_P     , Format('%.3f', [C.PsiP]));
          SubSection.SetAttribute(GTX_ATTR_CODE_OBS       , C.Commentaire);
        SectionCodes.AppendChild(SubSection);
      end;
    // section Expés
    SectionSeances := MyDocXML.CreateElement(GTX_KEY_SECTION_EXPES);
      KWYXZ(GTX_KEY_SECTION_EXPES);
      GHTopoRoot.AppendChild(SectionSeances);
      for i := IDX_MINI to GetNbExpes - 1 do
      begin
        EX := GetExpe(i);
        SubSection := MyDocXML.CreateElement(GTX_KEY_EXPE);
          SubSection.SetAttribute(GTX_ATTR_EXPE_NUMERO   , Format('%d', [EX.IDExpe]));
          SubSection.SetAttribute(GTX_ATTR_EXPE_DATE     , Format('%.4d-%.2d-%.2d', [EX.AnneeExpe, EX.MoisExpe, EX.JourExpe]));
          SubSection.SetAttribute(GTX_ATTR_EXPE_IDXCOLOR , Format('%d', [EX.Couleur]));
          SubSection.SetAttribute(GTX_ATTR_EXPE_DECLINAT , Format('%.4f', [EX.Declinaison]));
          SubSection.SetAttribute(GTX_ATTR_EXPE_MODEDECL , Format('%d', [EX.ModeDecl]));
          SubSection.SetAttribute(GTX_ATTR_EXPE_INCLINAT , Format('%.4f', [EX.Inclinaison]));
          SubSection.SetAttribute(GTX_ATTR_EXPE_SURVEY1  , EX.Speleometre);
          SubSection.SetAttribute(GTX_ATTR_EXPE_SURVEY2  , EX.Speleographe);
          SubSection.SetAttribute(GTX_ATTR_EXPE_OBS      , EX.Commentaire);
        SectionSeances.AppendChild(SubSection);
      end;
    // section Séries
    SectionSeries := MyDocXML.CreateElement(GTX_KEY_SECTION_SERIES);
      GHTopoRoot.AppendChild(SectionSeries);
      KWYXZ(GTX_KEY_SECTION_SERIES);
      for i := IDX_MINI to GetNbSeries - 1 do
      begin
        SER := GetSerie(i);
        SubSection := MyDocXML.CreateElement(GTX_KEY_SERIE);
          SubSection.SetAttribute(GTX_ATTR_SERIE_Numero  , Format('%d', [SER.GetIndexSerie]));
          SubSection.SetAttribute(GTX_ATTR_SERIE_NAME    , SER.GetNomSerie);
          SubSection.SetAttribute(GTX_ATTR_SERIE_RESEAU  , Format('%d', [SER.GetNoReseau]));

          SubSection.SetAttribute(GTX_ATTR_SERIE_SERDEP  , Format('%d', [SER.GetNoSerieDep]));
          SubSection.SetAttribute(GTX_ATTR_SERIE_PTDEP   ,  Format('%d', [SER.GetNoPointDep]));
          SubSection.SetAttribute(GTX_ATTR_SERIE_SERARR  , Format('%d', [SER.GetNoSerieArr]));
          SubSection.SetAttribute(GTX_ATTR_SERIE_PTARR   , Format('%d', [SER.GetNoPointArr]));

          SubSection.SetAttribute(GTX_ATTR_SERIE_COLOR   , Format('%s', [ColorToHTMLColor(SER.GetCouleur)]));

          SubSection.SetAttribute(GTX_ATTR_SERIE_CHANCE  , Format('%d', [SER.GetChance]));
          SubSection.SetAttribute(GTX_ATTR_SERIE_OBSTACLE, Format('%d', [SER.GetObstacle]));
          SubSection.SetAttribute(GTX_ATTR_SERIE_OBS     , SER.GetObsSerie);

          SubSection.SetAttribute(GTX_ATTR_SERIE_RAIDEUR , Format('%.4f', [SER.GetRaideur]));
        SectionSeries.AppendChild(SubSection);
        SubSubSection := MyDocXML.CreateElement(GTX_KEY_STATIONS);
        SubSection.AppendChild(SubSubSection);
        for S:=0 to SER.GetNbVisees - 1 do
        begin
          ST := SER.GetVisee(S);
          SubSubSubSection := MyDocXML.CreateElement(GTX_KEY_VISEE);
            SubSubSubSection.SetAttribute(GTX_ATTR_VISEE_ID   , Format('%d.%d', [SER.GetIndexSerie, S]));
            SubSubSubSection.SetAttribute(GTX_ATTR_VISEE_LBL  , Format('%s', [ST.IDTerrainStation]));

            SubSubSubSection.SetAttribute(GTX_ATTR_VISEE_CODE , Format('%d', [ST.Code]));
            SubSubSubSection.SetAttribute(GTX_ATTR_VISEE_EXPE , Format('%d', [ST.Expe]));

            SubSubSubSection.SetAttribute(GTX_ATTR_VISEE_TYPE , Format('%d', [Ord(ST.TypeVisee)]));

            SubSubSubSection.SetAttribute(GTX_ATTR_VISEE_LONG , Format('%.3f', [ST.Longueur]));
            SubSubSubSection.SetAttribute(GTX_ATTR_VISEE_AZ   , Format('%.2f', [ST.Azimut]));
            SubSubSubSection.SetAttribute(GTX_ATTR_VISEE_P    , Format('%.2f', [ST.Pente]));

            SubSubSubSection.SetAttribute(GTX_ATTR_VISEE_LG   , Format('%.2f', [ST.LG]));
            SubSubSubSection.SetAttribute(GTX_ATTR_VISEE_LD   , Format('%.2f', [ST.LD]));
            SubSubSubSection.SetAttribute(GTX_ATTR_VISEE_HZ   , Format('%.2f', [ST.HZ]));
            SubSubSubSection.SetAttribute(GTX_ATTR_VISEE_HN   , Format('%.2f', [ST.HN]));

            SubSubSubSection.SetAttribute(GTX_ATTR_VISEE_OBS  , ST.Commentaires);

          SubSubSection.AppendChild(SubSubSubSection);
        end;
      end;
    // section Antennes
    if (GetNbAntennes > 0) then
    begin
      SectionAntennes := MyDocXML.CreateElement(GTX_KEY_SECTION_ANTENNAS);
      KWYXZ(GTX_KEY_SECTION_ANTENNAS);
      GHTopoRoot.AppendChild(SectionAntennes);
      for i := IDX_MINI to GetNbAntennes - 1 do
      begin
        ANT := GetViseeAntenne(i);
        SubSection := MyDocXML.CreateElement(GTX_KEY_ANTENNA_SHOT);
          SubSection.SetAttribute(GTX_KEY_ANTENNA_NUMERO  , Format('%d', [ANT.IDViseeAntenne]));
          SubSection.SetAttribute(GTX_KEY_ANTENNA_LABEL   , Format('%s', [ANT.IDTerrainStation]));
          SubSection.SetAttribute(GTX_KEY_ANTENNA_NETWORK , Format('%d', [ANT.Reseau]));
          SubSection.SetAttribute(GTX_KEY_ANTENNA_SERIE   , Format('%d', [ANT.SerieDepart]));
          SubSection.SetAttribute(GTX_KEY_ANTENNA_POINT   , Format('%d', [ANT.PtDepart]));
          SubSection.SetAttribute(GTX_KEY_ANTENNA_CODE    , Format('%d', [ANT.Code]));
          SubSection.SetAttribute(GTX_KEY_ANTENNA_TRIP    , Format('%d', [ANT.Expe]));
          SubSection.SetAttribute(GTX_KEY_ANTENNA_LONG    , Format('%.3f', [ANT.Longueur]));
          SubSection.SetAttribute(GTX_KEY_ANTENNA_AZIMUT  , Format('%.2f', [ANT.Azimut]));
          SubSection.SetAttribute(GTX_KEY_ANTENNA_PENTE   , Format('%.2f', [ANT.Pente]));
          SubSection.SetAttribute(GTX_KEY_ANTENNA_OBS     , Format('%s', [ANT.Commentaires]));
        SectionAntennes.AppendChild(SubSection);
      end;
    end;


    // sérialisation du fichier
    WriteXMLFile(MyDocXML, FichierXML);
  finally
    MyDocXML.Free;
  end;
end;
// ouverture d'un fichier XML (gtx)
function  TToporobotStructure2012.LoadFromXML(const FichierXML: string): integer;
var
  MyDocXML: TXMLDocument;
  GHTopoRoot: TDOMElement;
  // items de tables simples
  UneEntree: TEntrance;
  UneExpe: TExpe;
  UnCode: TCode;
  UnReseau: TReseau;
  UneViseeAntenne: TViseeAntenne;
  UneSerie: TObjSerie;
  UneVisee: TUneVisee;
  NbSect: LongWord;
  ListeDesSections: TDOMNodeList;
  MySection: TDOMNode;
  NoSec: Integer;
  SectionReseau: TDOMNode;
  EWE: TDOMNode;
  //--------------
  ListeDesReseaux: TDOMNodeList;
  WU: TDOMNode;
  ListeDesEntrees: TDOMNodeList;
  i: Integer;
  ListeDesExpes: TDOMNodeList;
  ListeDesCodes: TDOMNodeList;
  ListeDesSeries: TDOMNodeList;
  ListeDesAntennes: TDOMNodeList;
  QAT: TDOMNode;
  ListeStations: TDOMNodeList;
  St: Integer;
  WOK: TDOMNode;
  QIN: TDOMNode;
  QMyDate: TDateTime;
  qIdxSerie: Integer;
  qNomSerie: WideString;
  qSerDepart: Integer;
  qPtDepart: Integer;
  qSerArrivee: Integer;
  qPtArrivee: Integer;
  qChance: Integer;
  qObstacle: Integer;
  qReseau: Integer;
  qObsSerie: WideString;
  qRaideur: Extended;
  procedure KWYXZ(const S666: string);
  begin
    AfficherMessage(Format('-- Found "%s" section', [S666]));
  end;
begin
  Result := -1;
  AfficherMessage(Format('%s.LoadFromXML(%s)', [ClassName, FichierXML]));
  // réinit du document
  self.ReInitialiser(False);
  MyDocXML := TXMLDocument.Create;
  try
    ReadXMLFile(MyDocXML, FichierXML);
    GHTopoRoot := MyDocXML.DocumentElement;
    (*
    AfficherMessage(Format('  Document: %s - %s', [GHTopoRoot.NodeName, GHTopoRoot.NodeValue]));
    // Liste des sections
    ListeDesSections := GHTopoRoot.GetChildNodes;
    NbSect := ListeDesSections.Count;

    for NoSec := 0 to NbSect -1 do
    begin
      MySection := ListeDesSections.Item[NoSec];
      AfficherMessage(Format('Cle: %s - Valeur: %s', [MySection.NodeName, MySection.NodeValue]));
      // récupération de la section Réseaux
      EWE := MySection.FindNode(GTX_KEY_SECTION_RESEAUX);
    end;
    //*)
    // lister les entrées
    AfficherMessage('======================');
    EWE := GHTopoRoot.FindNode(GTX_KEY_SECTION_ENTRANCES);
    if (EWE <> Nil) then
    begin
      ListeDesEntrees := EWE.ChildNodes;
      for i := 0 to ListeDesEntrees.Count - 1 do
      begin
        try  WU := ListeDesEntrees.Item[i];
          // un item = un attribut
          QIN := WU.Attributes.GetNamedItem(GTX_ATTR_ENTRANCE_IDX);
          UneEntree.eNumEntree := StrToIntDef(QIN.TextContent, -1);
          QIN := WU.Attributes.GetNamedItem(GTX_ATTR_ENTRANCE_NAME);
          UneEntree.eNomEntree := Trim(QIN.TextContent);
          QIN := WU.Attributes.GetNamedItem(GTX_ATTR_ENTRANCE_REFSERIE);
          UneEntree.eRefSer    := StrToIntDef(QIN.TextContent, -1);
          QIN := WU.Attributes.GetNamedItem(GTX_ATTR_ENTRANCE_REFPT);
          UneEntree.eRefSt     := StrToIntDef(QIN.TextContent, -1);

          QIN := WU.Attributes.GetNamedItem(GTX_ATTR_ENTRANCE_X);
          UneEntree.eXEntree   := StrToFloatDef(QIN.TextContent, -1.00);
          QIN := WU.Attributes.GetNamedItem(GTX_ATTR_ENTRANCE_Y);
          UneEntree.eYEntree   := StrToFloatDef(QIN.TextContent, -1.00);
          QIN := WU.Attributes.GetNamedItem(GTX_ATTR_ENTRANCE_Z);
          UneEntree.eZEntree   := StrToFloatDef(QIN.TextContent, -1.00);
          AfficherMessage(Format('Entree: %d: %s - [%d.%d] %.2f, %.2f, %.2f, %s',
                                 [UneEntree.eNumEntree, UneEntree.eNomEntree,
                                  UneEntree.eRefSer, UneEntree.eRefSt,
                                  UneEntree.eXEntree, UneEntree.eYEntree, UneEntree.eZEntree,
                                  UneEntree.eObserv
                                 ]));
          self.AddEntree(UneEntree);

        except
        end;
      end;
    end;
    // lister les réseaux
    AfficherMessage('======================');
    EWE := GHTopoRoot.FindNode(GTX_KEY_SECTION_RESEAUX);
    if (EWE <> Nil) then
    begin
      ListeDesReseaux := EWE.ChildNodes;
      for i := 0 to ListeDesReseaux.Count - 1 do
      begin
        try
          WU := ListeDesReseaux.Item[i];
          // <Network Name="Réseau principal" Type="0" Color="#000080FF" Numero="0" Comments=""/>
          QIN := WU.Attributes.GetNamedItem(GTX_ATTR_RESEAU_IDX);
          UnReseau.IdxReseau := StrToIntDef(QIN.TextContent, 0);

          QIN := WU.Attributes.GetNamedItem(GTX_ATTR_RESEAU_COLOR);
          UnReseau.ColorReseau := ColorHTMLToColor(QIN.TextContent);
          QIN := WU.Attributes.GetNamedItem(GTX_ATTR_RESEAU_TYPE);
          UnReseau.TypeReseau  := StrToIntDef(QIN.TextContent, 0);
          QIN := WU.Attributes.GetNamedItem(GTX_ATTR_RESEAU_NAME);
          UnReseau.NomReseau   := trim(QIN.TextContent);
          QIN := WU.Attributes.GetNamedItem(GTX_ATTR_RESEAU_OBS);
          UnReseau.ObsReseau   := trim(QIN.TextContent);

          AfficherMessage(Format('Reseau: %d - %d - %X - %s - %s',
                                [UnReseau.IdxReseau, UnReseau.TypeReseau,
                                 UnReseau.ColorReseau,
                                 UnReseau.NomReseau,
                                 UnReseau.ObsReseau
                                ]));

          self.AddReseau(UnReseau);
        except
        end;
      end;
    end;

    // lister les codes
    AfficherMessage('======================');
    EWE := GHTopoRoot.FindNode(GTX_KEY_SECTION_CODES);
    if (EWE <> Nil) then
    begin
      ListeDesCodes := EWE.ChildNodes;
      for i := 0 to ListeDesCodes.Count - 1 do
      begin
        try
          WU := ListeDesCodes.Item[i];
          QIN := WU.Attributes.GetNamedItem(GTX_ATTR_CODE_NUMERO);
          UnCode.IDCode:= StrToIntDef(QIN.TextContent, 0);

          QIN := WU.Attributes.GetNamedItem(GTX_ATTR_CODE_TYPE);
          UnCode.TypeGalerie:= StrToIntDef(QIN.TextContent, 0);

          QIN := WU.Attributes.GetNamedItem(GTX_ATTR_CODE_FACT_LONG);
          UnCode.FactLong:= StrToFloatDef(QIN.TextContent, 1.0000);
          QIN := WU.Attributes.GetNamedItem(GTX_ATTR_CODE_UCOMPASS);
          UnCode.GradAz:= StrToFloatDef(QIN.TextContent, 360.00);
          QIN := WU.Attributes.GetNamedItem(GTX_ATTR_CODE_UCLINO);
          UnCode.GradInc:= StrToFloatDef(QIN.TextContent, 360.00);
          QIN := WU.Attributes.GetNamedItem(GTX_ATTR_CODE_ANGLIMITE);
          UnCode.AngLimite:= StrToFloatDef(QIN.TextContent, 0.00);

          QIN := WU.Attributes.GetNamedItem(GTX_ATTR_CODE_PSI_L);
          UnCode.PsiL:= StrToFloatDef(QIN.TextContent, 0.01);
          QIN := WU.Attributes.GetNamedItem(GTX_ATTR_CODE_PSI_A);
          UnCode.PsiAz:= StrToFloatDef(QIN.TextContent, 0.01);
          QIN := WU.Attributes.GetNamedItem(GTX_ATTR_CODE_PSI_P);
          UnCode.PsiP:= StrToFloatDef(QIN.TextContent, 0.01);

          QIN := WU.Attributes.GetNamedItem(GTX_ATTR_CODE_OBS);
          UnCode.Commentaire:= trim(QIN.TextContent);



          AfficherMessage(Format('Code: %d - %.2f, %.2f - %.3f, %.3f, %.3f - %s',
                                 [UnCode.IDCode,
                                  UnCode.GradAz,
                                  UnCode.GradInc,
                                  UnCode.PsiL,
                                  UnCode.PsiAz,
                                  UnCode.PsiP,
                                  UnCode.Commentaire
                                 ]));
          self.AddCode(UnCode);
        except
        end;
      end;
    end;
    AfficherMessage(datetostr(DateSQLToDatePascal('2012-11-25')));
    // lister les expés
    AfficherMessage('======================');
    EWE := GHTopoRoot.FindNode(GTX_KEY_SECTION_EXPES);
    if (EWE <> Nil) then
    begin
      ListeDesExpes := EWE.ChildNodes;
      for i := 0 to ListeDesExpes.Count - 1 do
      begin
        try
          WU := ListeDesExpes.Item[i];
          QIN := WU.Attributes.GetNamedItem(GTX_ATTR_EXPE_NUMERO);
          UneExpe.IDExpe:= StrToIntDef(QIN.TextContent, 0);
          QIN := WU.Attributes.GetNamedItem(GTX_ATTR_EXPE_DATE);
          QMyDate := DateSQLToDatePascal(Trim(QIN.TextContent));
          UneExpe.AnneeExpe := YearOf(QMyDate);
          UneExpe.MoisExpe  := MonthOf(QMyDate);
          UneExpe.JourExpe  := DayOf(QMyDate);
          QIN := WU.Attributes.GetNamedItem(GTX_ATTR_EXPE_IDXCOLOR);
          UneExpe.Couleur:= StrToIntDef(QIN.TextContent, 0);

          QIN := WU.Attributes.GetNamedItem(GTX_ATTR_EXPE_SURVEY1);
          UneExpe.Speleometre:= Trim(QIN.TextContent);
          QIN := WU.Attributes.GetNamedItem(GTX_ATTR_EXPE_SURVEY2);
          UneExpe.Speleographe:= Trim(QIN.TextContent);

          QIN := WU.Attributes.GetNamedItem(GTX_ATTR_EXPE_MODEDECL);
          UneExpe.ModeDecl := StrToIntDef(QIN.TextContent, 0);

          QIN := WU.Attributes.GetNamedItem(GTX_ATTR_EXPE_DECLINAT);
          UneExpe.Declinaison:= StrToFloatDef(QIN.TextContent, 0.00);
          QIN := WU.Attributes.GetNamedItem(GTX_ATTR_EXPE_INCLINAT);
          UneExpe.Inclinaison:= StrToFloatDef(QIN.TextContent, 0.00);

          QIN := WU.Attributes.GetNamedItem(GTX_ATTR_EXPE_OBS);
          UneExpe.Commentaire:= Trim(QIN.TextContent);
          AfficherMessage(Format('Expe: %d - Date: %s - %d - %s, %s - %s',
                                 [UneExpe.IDExpe,
                                  DateToStr(EncodeDate(UneExpe.AnneeExpe, UneExpe.MoisExpe, UneExpe.JourExpe)),
                                  UneExpe.Couleur,
                                  UneExpe.Speleometre, UneExpe.Speleographe,
                                  UneExpe.Commentaire
                                 ]));
          self.AddExpe(UneExpe);
        except
        end;
      end;
    end;
    // lister les séries
    AfficherMessage('======================');
    EWE := GHTopoRoot.FindNode(GTX_KEY_SECTION_SERIES);
    if (EWE <> Nil) then
    begin
      ListeDesSeries := EWE.ChildNodes;
      AfficherMessage(Format('%d séries', [ListeDesSeries.Count]));
      for i := 0 to ListeDesSeries.Count - 1 do
      begin
        UneSerie := TObjSerie.Create;
        try
          UneSerie.ClearSerie;
          WU  := ListeDesSeries.Item[i];
          // un item = un attribut
          QIN := WU.Attributes.GetNamedItem(GTX_ATTR_SERIE_Numero);
          qIdxSerie    := StrToIntDef(QIN.TextContent, 0);
          UneSerie.SetIndexSerie(qIdxSerie);

          QIN := WU.Attributes.GetNamedItem(GTX_ATTR_SERIE_NAME);
          qNomSerie    := Trim(QIN.TextContent);
          UneSerie.SetNomSerie(qNomSerie);

          QIN := WU.Attributes.GetNamedItem(GTX_ATTR_SERIE_OBS);
          qObsSerie    := Trim(QIN.TextContent);
          UneSerie.SetObsSerie(qObsSerie);


          QIN         := WU.Attributes.GetNamedItem(GTX_ATTR_SERIE_SERDEP);
          qSerDepart  := StrToIntDef(QIN.TextContent, 0);
          QIN         := WU.Attributes.GetNamedItem(GTX_ATTR_SERIE_PTDEP);
          qPtDepart   := StrToIntDef(QIN.TextContent, 0);
          QIN         := WU.Attributes.GetNamedItem(GTX_ATTR_SERIE_SERARR);
          qSerArrivee := StrToIntDef(QIN.TextContent, 0);
          QIN         := WU.Attributes.GetNamedItem(GTX_ATTR_SERIE_PTARR);
          qPtArrivee  := StrToIntDef(QIN.TextContent, 0);
          UneSerie.SetSeriePtExtremites(qSerDepart, qPtDepart,
                                        qSerArrivee, qPtArrivee);

          QIN         := WU.Attributes.GetNamedItem(GTX_ATTR_SERIE_CHANCE);
          qChance     := StrToIntDef(QIN.TextContent, 0);
          QIN         := WU.Attributes.GetNamedItem(GTX_ATTR_SERIE_OBSTACLE);
          qObstacle   := StrToIntDef(QIN.TextContent, 0);
          UneSerie.SetChanceObstacle(qChance, qObstacle);

          QIN         := WU.Attributes.GetNamedItem(GTX_ATTR_SERIE_RESEAU);
          qReseau     := StrToIntDef(QIN.TextContent, 0);
          UneSerie.SetNoReseau(qReseau);

          // DONE: Raideur implémentée
          QIN         := WU.Attributes.GetNamedItem(GTX_ATTR_SERIE_RAIDEUR);
          qRaideur    := StrToFloatDef(QIN.TextContent, 1.00);
          UneSerie.SetRaideur(qRaideur);

          // TODO: Implanter la raideur et la couleur
          UneSerie.SetCouleur(clBlue);


          // les stations
          QAT := WU.FindNode(GTX_KEY_STATIONS);
          if (QAT <> NIL) then
          begin
            ListeStations := QAT.ChildNodes;
            //AfficherMessage(Format('Série %d %s - %d stations', [UneSerie.GetIndexSerie, UneSerie.GetNomSerie, ListeStations.Count]));
            for St := 0 to ListeStations.Count - 1 do
            begin
              try
                WOK := ListeStations.Item[St];

                QIN := WOK.Attributes.GetNamedItem(GTX_ATTR_VISEE_ID);
                UneVisee.NoVisee := St; // Attribut XML 'ID' non utilisé //StrToIntDef(QIN.TextContent, St); // -1 = numérotation auto

                QIN := WOK.Attributes.GetNamedItem(GTX_ATTR_VISEE_LBL);
                UneVisee.IDTerrainStation := Trim(QIN.TextContent);

                QIN := WOK.Attributes.GetNamedItem(GTX_ATTR_VISEE_TYPE);
                UneVisee.TypeVisee := GetTypeDeVisee(StrToIntDef(QIN.TextContent, 0));

                QIN := WOK.Attributes.GetNamedItem(GTX_ATTR_VISEE_CODE);
                UneVisee.Code := StrToIntDef(QIN.TextContent, 0);
                QIN := WOK.Attributes.GetNamedItem(GTX_ATTR_VISEE_EXPE);
                UneVisee.Expe := StrToIntDef(QIN.TextContent, 0);
                //*)
                //UneVisee.Code := 1;
                //UneVisee.Expe := 1;


                QIN := WOK.Attributes.GetNamedItem(GTX_ATTR_VISEE_LONG);
                UneVisee.Longueur := strToFloatDef(QIN.TextContent, 0.001);
                QIN := WOK.Attributes.GetNamedItem(GTX_ATTR_VISEE_AZ);
                UneVisee.Azimut := strToFloatDef(QIN.TextContent, 0.00);
                QIN := WOK.Attributes.GetNamedItem(GTX_ATTR_VISEE_P);
                UneVisee.Pente := strToFloatDef(QIN.TextContent, 0.00);

                QIN := WOK.Attributes.GetNamedItem(GTX_ATTR_VISEE_LG);
                UneVisee.LG := strToFloatDef(QIN.TextContent, 0.00);
                QIN := WOK.Attributes.GetNamedItem(GTX_ATTR_VISEE_LD);
                UneVisee.LD := strToFloatDef(QIN.TextContent, 0.00);
                QIN := WOK.Attributes.GetNamedItem(GTX_ATTR_VISEE_HZ);
                UneVisee.HZ := strToFloatDef(QIN.TextContent, 0.00);
                QIN := WOK.Attributes.GetNamedItem(GTX_ATTR_VISEE_HN);
                UneVisee.HN := strToFloatDef(QIN.TextContent, 0.00);

                QIN := WOK.Attributes.GetNamedItem(GTX_ATTR_VISEE_OBS);
                UneVisee.Commentaires := Trim(QIN.TextContent);
                //*)
                (*
                AfficherMessage(Format('-- Station: %d [%s] - %d - [%d, %d] - L = %.2f, Az = %.2f, P = %.2f - %.2f, %.2f, %.2f, %.2f - %s',
                                      [UneVisee.NoVisee, UneVisee.IDTerrainStation,
                                       UneVisee.TypeGalerie, UneVisee.Code, UneVisee.Expe,
                                       UneVisee.Longueur, UneVisee.Azimut, UneVisee.Pente,
                                       UneVisee.LG, UneVisee.LD, UneVisee.HZ, UneVisee.HN,
                                       UneVisee.Commentaires
                                      ]));
                //*)
                // ajout visée
                UneSerie.AddVisee(UneVisee);
              except
              end;
            end; // for stations
            self.AddSerie(UneSerie);
          end;
        except // try séries
        end;
      end;  // for séries
    end;
    // lister les antennes
    AfficherMessage('======================');
    EWE := GHTopoRoot.FindNode(GTX_KEY_SECTION_ANTENNAS);
    if (EWE <> Nil) then
    begin
      ListeDesAntennes := EWE.ChildNodes;
      for i := 0 to ListeDesAntennes.Count - 1 do
      begin
        try
          WU := ListeDesAntennes.Item[i];
          AfficherMessage(Format('Antenne: %s - Valeur: %s', [WU.NodeName, WU.TextContent]));
          QIN := WU.Attributes.GetNamedItem(GTX_KEY_ANTENNA_NUMERO);
          UneViseeAntenne.IDViseeAntenne := StrToIntDef(QIN.TextContent, -1); // -1 = numérotation auto
          QIN := WU.Attributes.GetNamedItem(GTX_KEY_ANTENNA_LABEL);
          UneViseeAntenne.IDTerrainStation := Trim(QIN.TextContent);
          QIN := WU.Attributes.GetNamedItem(GTX_KEY_ANTENNA_NETWORK);
          UneViseeAntenne.Reseau := StrToIntDef(QIN.TextContent, 0);

          QIN := WU.Attributes.GetNamedItem(GTX_KEY_ANTENNA_SERIE);
          UneViseeAntenne.SerieDepart := StrToIntDef(QIN.TextContent, 0);
          QIN := WU.Attributes.GetNamedItem(GTX_KEY_ANTENNA_POINT);
          UneViseeAntenne.PtDepart    := StrToIntDef(QIN.TextContent, 0);

          QIN := WU.Attributes.GetNamedItem(GTX_KEY_ANTENNA_CODE);
          UneViseeAntenne.Code := StrToIntDef(QIN.TextContent, 0);
          QIN := WU.Attributes.GetNamedItem(GTX_KEY_ANTENNA_TRIP);
          UneViseeAntenne.Expe := StrToIntDef(QIN.TextContent, 0);

          QIN := WU.Attributes.GetNamedItem(GTX_KEY_ANTENNA_LONG);
          UneViseeAntenne.Longueur := StrToFloatDef(QIN.TextContent, 0.001);
          QIN := WU.Attributes.GetNamedItem(GTX_KEY_ANTENNA_AZIMUT);
          UneViseeAntenne.Azimut   := StrToFloatDef(QIN.TextContent, 0.00);
          QIN := WU.Attributes.GetNamedItem(GTX_KEY_ANTENNA_PENTE);
          UneViseeAntenne.Pente    := StrToFloatDef(QIN.TextContent, 0.00);

          QIN := WU.Attributes.GetNamedItem(GTX_KEY_ANTENNA_OBS);
          UneViseeAntenne.Commentaires := Trim(QIN.TextContent);
          AfficherMessage(Format('-- Antenne: %d [%s] - %d - [%d, %d] - L = %.2f, Az = %.2f, P = %.2f - %s',
                                  [UneViseeAntenne.IDViseeAntenne, UneViseeAntenne.IDTerrainStation,
                                   UneViseeAntenne.Reseau, UneViseeAntenne.Code, UneViseeAntenne.Expe,
                                   UneViseeAntenne.Longueur, UneViseeAntenne.Azimut, UneViseeAntenne.Pente,
                                   UneViseeAntenne.Commentaires
                                  ]));
          self.AddViseeAntenne(UneViseeAntenne);
        except
        end;
      end;
    end;
    // paramètres par défaut
    UneEntree := GetEntree(0);
    self.SetDatabaseName(ExtractFileNameOnly(FichierXML));
    self.SetCommentairesEtude('');
    self.SetRefSeriePoint(UneEntree.eRefSer, UneEntree.eRefSt);
    self.SetDefaultCoords(UneEntree.eXEntree, UneEntree.eYEntree, UneEntree.eZEntree);

    Result := self.GetNbSeries;
  finally
    MyDocXML.Free;
  end;
end;
end.

