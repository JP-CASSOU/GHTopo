unit CodeCalculTopo;
//****************************************************
// Projet     : GHTopo
// Modules    : Code de calcul topo
// Licence    : General Public License - WITHOUT WARRANTY
// Auteur     : JP CASSOU
// OS         : Windows 9x - Linux
// Langage    : Free-Pascal 2.6.x sous Lazarus
//-----------------------------------------------------
// Module     : ToporobotClasses:
//            : Structure des données d'un dossier Toporobot
// Compatibilité intégrale avec le standard Toporobot 1994.
// 22/08/2012 : Refonte du noyau de GHTopo (ToporobotClasses)
//              en séparant le SGBD et le code de calcul
// 24/08/2012 : Code de calcul validé.
// 14/02/2013 : Réacculturation au code de GHTopo. Petites corrections
//-----------------------------------------------------

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
  ToporobotClasses2012, ObjetSerie,
  UnitClassPalette,
  Common,
  Classes, SysUtils, Graphics, math,
  Forms; // pour Application

// Jonctions Toporobot
type TJonction = record
  IDJonction  : string;
  NoNoeud     : integer;
end;
// Noeuds
type TNoeud = record
  IDNoeud : integer;
  XN      : double;
  YN      : double;
  ZN      : double;
end;

// LISTE DES BRANCHES
// structure Branches (ancienne méthode, suffisante ici)
// Table des branches
type TBranche = record
  NoSerie       : integer;
  NoReseau        : integer;
  NoBranche     : integer;
  NomBranche    : string;
  NoeudDepart   : integer;
  NoeudArrivee  : integer;
  Rigidite      : double;  // module de raideur, égal à 1.00 par défaut
  PointsTopo    : TList;
  NbPoints      : integer;
  DeltaX        : double;
  DeltaY        : double;
  DeltaZ        : double;
  XDepart       : double;
  YDepart       : double;
  ZDepart       : double;
  XArrivee      : double;
  YArrivee      : double;
  ZArrivee      : double;
end;
  // Classe table Branches
type TTableBranches = class(TList);

// LISTES SIMPLES
// Classe pour la table des jonctions
type TTableJonctions = class(TList);
//Table des Noeuds
type TTableNoeuds = array of TNoeud;


// Code de calcul
type

{ TCodeDeCalcul }

TCodeDeCalcul = class
  private
    FDocumentToporobot: TToporobotStructure2012;
    FTableJonctions   : TTableJonctions;
    // gestion table des branches
    FTableBranches    : TTableBranches;
    // table des noeuds
    //Table des Noeuds
    FTableNoeuds : TTableNoeuds;

    procedure CalculContoursGaleries(const FichierTOP: string);
    procedure CalculerAccroissements;
    procedure CalculerCoordNoeuds;
    function GetNbBranches: integer;
    procedure RepartirEcarts;


  public
    // fichier de sortie TOP
    FFichierTOP: string;
    function  Initialiser(const DT: TToporobotStructure2012; const OutputTOP: string): boolean;
    procedure ViderTables;
    procedure Finaliser;
    procedure CalculerVisee(var MaVisee: TUneVisee; var DX, DY, DZ: double);
    // gestion des jonctions
    procedure AddJunction(const Jnct: TJonction);
    function  GetJonction(const NoJonction: integer): TJonction;
    function  GetNbJonctions: integer;
    function  RecenserJonctions: integer;
    // gestions des branches
    procedure AddBranche(const LaBranche: TBranche);
    function  GetBranche(const NumBrch: integer): TBranche;
    procedure PutBranche(const NoBrch: integer; const LaBranche: TBranche);
    procedure AddBrStation(const NoBr: integer; const LaVisee: TUneVisee);
    function  GetBrStation(const NoBr, NoSt: integer):TUneVisee;
    procedure PutBrStation(const NoBr, NoSt: integer; const LaVisee: TUneVisee);

    // gestion des noeuds
    function  GetNoeud(const Serie, Station: integer): integer;
    function  GetIDNoeud(const Idx: integer): string;
    // branches
    procedure RecenserBranches;
    // lancer le calcul
    function  Calculer: boolean;

end;

implementation
const FMT_NDSER_PT = '%d/%d';


//******************************************************************************
(* TCodeDeCalcul *)

procedure TCodeDeCalcul.Finaliser;
begin
  AfficherMessage(Format('%s.Finaliser',[self.ClassName]));
end;

{ TCodeDeCalcul }
function TCodeDeCalcul.Initialiser(const DT: TToporobotStructure2012; const OutputTOP: string): boolean;
begin
  Result := False;
  try
    FDocumentToporobot := DT;
    FFichierTOP := OutputTOP;
    // création des tables
    FTableJonctions := TTableJonctions.Create;
    FTableBranches  := TTableBranches.Create;
    ViderTables;
    Result := True;
  except

  end;
end;
// Lancer le calcul
function  TCodeDeCalcul.Calculer: boolean;
var
  UneVisee: ^TUneVisee;
  Branche: TBranche;
begin
  // Purger la table des branches
  New(UneVisee);
  UneVisee^ := EmptyVisee('');
  ViderTables;

  AfficherMessage(rsPURGE_TABLE_BRCH);
  with Branche do begin
    PointsTopo:=TList.Create;
    PointsTopo.Clear;
    PointsTopo.Add(UneVisee);
    NoBranche:=0;
    NomBranche:='Point 0';
    NoeudDepart:=0;
    NoeudArrivee:=0;
  end;
  AddBranche(Branche);
  AfficherMessage(Format('%s.Calculer',[self.ClassName]));
  Result := False;

  RecenserJonctions;
  RecenserBranches;
  CalculerAccroissements;  // calcul des accroissements des branches
  CalculerCoordNoeuds;     // calcul matriciel des coordonnées des noeuds
  AfficherMessage('-- 001');
  RepartirEcarts;          // répartition parallèle des écarts
  AfficherMessage('-- 002');
  CalculContoursGaleries(FFichierTOP);  // calcul des contours des galeries
end;


function TCodeDeCalcul.GetNbBranches: integer;
begin
  Result := FTableBranches.Count;
end;



procedure TCodeDeCalcul.ViderTables;
var
  i: integer;
begin
  // vidage table des jonctions
  try
    if (FTableJonctions.Count > 0) then
      for i := 0 to FTableJonctions.Count - 1 do Dispose(FTableJonctions.Items[i]);
    // vidage table des branches
    if (FTableBranches.Count > 0) then
      for i := 0 to FTableBranches.Count - 1 do Dispose(FTableBranches.Items[i]);
  except
  end;
  FTableJonctions.Clear;
  FTableBranches.Clear;
end;

// méthodes relatives aux jonctions
function TCodeDeCalcul.GetJonction(const NoJonction: integer): TJonction;
var
  pJonction: ^TJonction;
begin
  pJonction:=FTableJonctions.Items[NoJonction];
  Result:=pJonction^;
end;

function TCodeDeCalcul.GetNbJonctions: integer;
begin
  Result := FTableJonctions.Count;
end;

procedure TCodeDeCalcul.AddJunction(const Jnct: TJonction);
var
  pJonct: ^TJonction;
begin
  New(pJonct);
  pJonct^:=Jnct;
  FTableJonctions.Add(pJonct);
end;
//*************************************************************
// recherche de toutes les entrées, jonctions et culs de sacs
// DONE: Support des visées en antenne OK
function TCodeDeCalcul.RecenserJonctions: integer;
var
  Entree : TEntrance;
  Serie  : TObjSerie;
  Station: TUneVisee;
  ViseeAntenne: TViseeAntenne;
  Ser, St: integer;
  ListeJnct: TStringList;
  J    : TJonction;
begin
  AfficherMessage(Format('%s.RecenserJonctions',[self.ClassName]));
  result := -1;
  AfficherMessage(rsRECENSEM_JONC);
  ListeJnct:=TStringList.Create;
  try
    ListeJnct.Clear;
    ListeJnct.Sorted:=True;
    ListeJnct.Duplicates:=dupIgnore;
    // Les coordonnées d'accrochage des entrées de cavités sont forcément des jonctions
    AfficherMessage(Format('-- %d entrances', [FDocumentToporobot.GetNbEntrees]));
    for Ser:=0 to FDocumentToporobot.GetNbEntrees - 1 do begin
      Entree:=FDocumentToporobot.GetEntree(Ser);
      ListeJnct.Add(Format(FMT_NDSER_PT,[Entree.eRefSer,Entree.eRefSt]));
    end; //*)
    AfficherMessage(Format('-- %d series', [FDocumentToporobot.GetNbSeries]));
    // Les série/point de départ/arrivée sont forcément des jonctions (noeuds)
    for Ser:=0 to FDocumentToporobot.GetNbSeries - 1 do begin
      Serie:=FDocumentToporobot.GetSerie(Ser);
      ListeJnct.Add(Format(FMT_NDSER_PT,[Serie.GetNoSerieDep,  Serie.GetNoPointDep]));
      ListeJnct.Add(Format(FMT_NDSER_PT,[Serie.GetNoSerieArr,  Serie.GetNoPointArr]));
    end;
    AfficherMessage(Format('-- %d antenna shots', [FDocumentToporobot.GetNbAntennes]));
    // Les départs de visées en antenne sont aussi des jonctions
    if (FDocumentToporobot.GetNbAntennes > 1) then begin
      for Ser := 1 to FDocumentToporobot.GetNbAntennes - 1 do begin
        ViseeAntenne := FDocumentToporobot.GetViseeAntenne(Ser);
        ListeJnct.Add(Format(FMT_NDSER_PT,[ViseeAntenne.SerieDepart,  ViseeAntenne.PtDepart]));
      end;
    end;
    // en interne, l'ID de noeud est un littéral
    for St:=0 to ListeJnct.Count - 1 do begin
      //AfficherMessage(ListeJnct.Strings[St]);
      J.IDJonction:=ListeJnct.Strings[St];
      J.NoNoeud:=St;
      AddJunction(J);
    end;
    for St:=0 to GetNbJonctions - 1 do begin
      J := GetJonction(St);
      //AfficherMessage(format('-- > Jonction: %s = %d', [J.IDJonction, J.NoNoeud]));
    end;
    Application.ProcessMessages;
    // On passe ici ? OK, c'est bon
    Result := GetNbJonctions;
    AfficherMessage(format('-- > %d junctions', [Result]));
  finally
    // on libère la table provisoire
    ListeJnct.Free;
  end;
end;

procedure TCodeDeCalcul.AddBranche(const LaBranche: TBranche);
var
  UneBranche : ^TBranche;
  UneVisee   : ^TUneVisee;
begin
  New(UneBranche);
  UneBranche^:=LaBranche;
  with FTableBranches do
  begin
    // créer la table des stations
    UneBranche.PointsTopo:=TList.Create;
    UneBranche.PointsTopo.Clear;
    New(UneVisee);
    UneVisee^ := EmptyVisee('');
    UneBranche.PointsTopo.Add(UneVisee);
    UneBranche.NbPoints := UneBranche.PointsTopo.Count;
    Add(UneBranche);
  end;
end;

function TCodeDeCalcul.GetBranche(const NumBrch: integer): TBranche;
var
  pBch: ^TBranche;
begin;
  pBch := FTableBranches.Items[NumBrch];
  Result:=pBch^;
end;

procedure TCodeDeCalcul.PutBranche(const NoBrch: integer; const LaBranche: TBranche);
var
  pBch: ^TBranche;
begin
  pBch  := FTableBranches.Items[NoBrch];
  pBch^ := LaBranche;
  FTableBranches.Items[NoBrch]:=pBch;
end;

procedure TCodeDeCalcul.AddBrStation(const NoBr: integer; const LaVisee: TUneVisee);
var
  Visee  : ^TUneVisee;
  Branche: ^TBranche;
begin
   Branche:=FTableBranches.Items[NoBr];  // attrape la branche
   New(Visee);                           // opérations d'ajout de la visée
   Visee^:=LaVisee;
   Branche.PointsTopo.Add(Visee);
   Branche.NbPoints:=Branche.PointsTopo.Count;
   FTableBranches.Items[NoBr]:=Branche;  // met la liste à jour
end;
function TCodeDeCalcul.GetBrStation(const NoBr, NoSt: integer):TUneVisee;
var
  Visee  : ^TUneVisee;
  Branche: ^TBranche;
begin
  Branche := FTableBranches.Items[NoBr];
  Visee   :=Branche.PointsTopo.Items[NoSt];
  Result  :=Visee^;
end;
procedure TCodeDeCalcul.PutBrStation(const NoBr, NoSt: integer;
                                     const LaVisee: TUneVisee);
var
  Visee  : ^TUneVisee;
  Branche: ^TBranche;
begin;
  Branche:=FTableBranches.Items[NoBr];
  // ancienne version
  //New(Visee); //-- Removed 14/02/2013
  //Visee^:=LaVisee;
  //Branche.PointsTopo.Items[NoSt]:=Visee;
  // nouvelle version
  Visee := Branche.PointsTopo.Items[NoSt]; // récupérer la visée (ne pas créer de nouvel objet)
  Visee^:=LaVisee;                         // affectation
  Branche.PointsTopo.Items[NoSt]:=Visee;   // mise à jour

end;


function TCodeDeCalcul.GetNoeud(const Serie, Station: integer): integer;
var
  i   : integer;
  Jonc: TJonction;
  S   : string;
begin
  Result:=-1;
  for i:=0 to GetNbJonctions - 1 do
  begin
    Jonc := GetJonction(i);
    S:=Format(FMT_NDSER_PT,[Serie, Station]);
    if (S = Jonc.IDJonction) then
    begin
      Result:=i;
      exit;
    end;
  end;
  //*)
end;

function TCodeDeCalcul.GetIDNoeud(const Idx: integer): string;
var
  J: TJonction;
begin
  J:=GetJonction(Idx);
  Result:=J.IDJonction;
end;

// calcul des accroissements pour une visée
// factorisation du code
procedure TCodeDeCalcul.CalculerVisee(var MaVisee: TUneVisee; var DX, DY, DZ: double);
const
  TWOPI = 2*PI;
  PI_2  = PI/2;
  PI_200 = PI/200;
var
  CorrectionPenteInRadians: Double;

  LeCode: TCode;
  LaExpe: TExpe;

  ucc   : integer;
  udd   : double;


  RX, RY, RZ: double;
  UB, UC, LP: double;

  Az1: double;
  Pente1: double;
begin
  LeCode := FDocumentToporobot.GetCodeByIndex(MaVisee.Code);
  LaExpe := FDocumentToporobot.GetExpeByIndex(MaVisee.Expe);

  ucc:=Round(LeCode.GradAz);
  if (ucc = 0) then LeCode.GradAz:=360.00;
  ucc:=Round(LeCode.GradInc);
  if (ucc = 0) then LeCode.GradInc:=360.00;
  ucc:=Round(LeCode.GradAz);
  udd:=400.00;
      //WriteLn(Format('LeCode.GradAz = %f udd = %f',[udd]));
      case ucc of

        359, 360: begin // visées directes en degrés
           UB := TWOPI/360.00;
           Az1:=MaVisee.Azimut * UB + LaExpe.Declinaison  * PI_200;
        end;
	399, 400: begin // visées directes en grades
           UB := TWOPI/400.00;
           Az1:=MaVisee.Azimut * UB + LaExpe.Declinaison  * PI_200;
        end;
        349, 350: begin  // visées inverses en degrés
           UB := TWOPI/360.00;
           Az1:=MaVisee.Azimut * UB + LaExpe.Declinaison  * PI_200;
           Az1:=PI+Az1;
        end;
        389, 390: begin // visées inverses en grades
           UB := TWOPI/400.00;
           Az1:=MaVisee.Azimut * UB + LaExpe.Declinaison  * PI_200;
           Az1:=PI+Az1;
        end;
      end;


      (* Déterminer si on travaille en zénithal*)
      ucc:=Round(LeCode.GradInc);
      if (Abs(LaExpe.Inclinaison) > 0.01) then
        AfficherMessage(FloatToStr(LaExpe.Inclinaison));

      // Correction erreurs systématiques des pentes
      CorrectionPenteInRadians := GradToRad(LaExpe.Inclinaison / 10.00);

      case ucc of
	360, 400: begin // zéro à l'horizontale
          UC := TWOPI/ucc;
          // corrections d'erreurs
          //Pente1 := (Visee.Pente + LaExpe.Inclinaison) * UC;
          Pente1 := CorrectionPenteInRadians + MaVisee.Pente  * UC;
	  LP := MaVisee.Longueur * Cos(Pente1);
          //RX := LP * Sin((Visee.Azimut + LaExpe.Declinaison) * UB);
          //RY := LP * Cos((Visee.Azimut + LaExpe.Declinaison) * UB);
          RX := LP * Sin(Az1);
          RY := LP * Cos(Az1);
          RZ := MaVisee.Longueur * Sin(Pente1);
	end;
	361, 401: begin // zéro zénithal
          UC := TWOPI/(ucc - 1);
          Pente1 := PI_2 - (CorrectionPenteInRadians + MaVisee.Pente * UC);
          LP := MaVisee.Longueur * Cos(Pente1);
	  //RX := LP * Sin((Visee.Azimut + LaExpe.Declinaison) * UB);
          //RY := LP * Cos((Visee.Azimut + LaExpe.Declinaison) * UB);
          RX := LP * Sin(Az1);
          RY := LP * Cos(Az1);
          RZ := MaVisee.Longueur * Sin(Pente1);
	end;
	359, 399: begin // zéro nadiral
          UC := TWOPI/(ucc + 1);
	  LP := MaVisee.Longueur * Cos(-(PI_2 - (MaVisee.Pente * UC)));
	  //RX := LP * Sin((Visee.Azimut + LaExpe.Declinaison) * UB);
          //RY := LP * Cos((Visee.Azimut + LaExpe.Declinaison) * UB);
          RX := LP * Sin(Az1);
          RY := LP * Cos(Az1);
          Pente1 := -(PI_2 - (CorrectionPenteInRadians + MaVisee.Pente * UC));
          RZ := MaVisee.Longueur * Sin(Pente1); // Fixé le 31/05.

	end;
	370: begin // pourcentages
          udd:= ArcTan(MaVisee.Pente);
          LP := MaVisee.Longueur * Cos(udd);
          //RX := LP * Sin((Visee.Azimut + LaExpe.Declinaison) * UB);
          //RY := LP * Cos((Visee.Azimut + LaExpe.Declinaison) * UB);
          RX := LP * Sin(Az1);
          RY := LP * Cos(Az1);

          RZ := MaVisee.Longueur * Sin(udd);
	end;
	380: begin // différences d'altitudes
	            // Note: Les diférences d'altitudes sont stockées ds Visee.Pente
	  udd:=ArcTan2(MaVisee.Pente, MaVisee.Longueur);
	  try
            // DONE: Formule de calcul de LP corrigée le 15/10/2010
            LP := sqrt(MaVisee.Longueur * MaVisee.Longueur - MaVisee.Pente * MaVisee.Pente);
            RX := LP * Sin(Az1);
            RY := LP * Cos(Az1);
            RZ := MaVisee.Pente;
          except
            AfficherMessage(Format('** ERREUR ** Shot: %s: L=%.2f AZ=%.2f P=%.2f - RX = %.2f - RY = %.2f - LP = %.2f',
                                   [MaVisee.IDTerrainStation,
                                    MaVisee.Longueur, MaVisee.Azimut, MaVisee.Pente,
                                    RX, RY, LP]));
          end;
          //AfficherMessage(Format('Dénivellations: Valeur init: %.2f; Angle = %.2f - Dénivelé = %.2f',[Visee.Pente, udd, RZ]));
        end;
        350, 390:
        begin // visées inverses en degrés/grades (ucc = UniteClino -10);
          UC := TWOPI/(ucc + 10);
          Pente1 := CorrectionPenteInRadians - MaVisee.Pente * UC;
          LP := MaVisee.Longueur * Cos(Pente1);
          //RX := LP * Sin((Visee.Azimut + LaExpe.Declinaison) * UB);
          //RY := LP * Cos((Visee.Azimut + LaExpe.Declinaison) * UB);
          RX := LP * Sin(Az1);
          RY := LP * Cos(Az1);
          RZ := MaVisee.Longueur * Sin(Pente1);
        end;
      else begin // zéro horizontal par défault
          UC := TWOPI/360.00;
          Pente1 := CorrectionPenteInRadians + MaVisee.Pente * UC;
	  LP := MaVisee.Longueur * Cos(Pente1);

          RX := LP * Sin(Az1);
          RY := LP * Cos(Az1);
          RZ := MaVisee.Longueur * Sin(Pente1);
	end;
      end;
      //==============
      DX := DX + RX;
      DY := DY + RY;
      DZ := DZ + RZ;
      MaVisee.X:=RX;
      MaVisee.Y:=RY;
      MaVisee.Z:=RZ;
end;

// découpage des séries en branches
procedure TCodeDeCalcul.RecenserBranches;
const
  STR_FMT_STATIONS = '>> %d/%d |%.3d %.3d| %.2f %.2f %.2f | %.2f %.2f %.2f %.2f | %s';
var
  Entr : TEntrance;
  Serie: TObjSerie;
  Visee: TUneVisee;
  Ser, Vis: integer;
  l1, a1, p1: double;
  //pTXT : TextFile;

  Br   : integer;
  Nd   : integer;
  Branche0,
  Branche1: TBranche;
  NbItems: LongInt;
  pUneVisee: ^TUneVisee; //
  QDeltaX: Extended;
  QDeltaY: Extended;
  QDeltaZ: Extended;
begin
  AfficherMessage(rsFINDING_BRANCHES);
  // première branche
  with Branche0 do begin
    NoSerie     :=1;
    NoReseau    :=0;
    NoBranche   :=1;
    NomBranche  :=Format('Branche %d',[1]);
    NoeudDepart :=0;
    NoeudArrivee:=1;
    Rigidite    :=1.00;
    DeltaX:=0.01;
    DeltaY:=0.01;
    DeltaZ:=0.01;
  end;

  AddBranche(Branche0);
  Br:=2;
  //***********************
  // balayage des séries
  for Ser:=1 to FDocumentToporobot.GetNbSeries -1 do
  begin
    if (ser mod 50 = 0) then AfficherMessage(Format('--> Serie: %d/%d',[ser, FDocumentToporobot.GetNbSeries - 1]));
    Serie:=FDocumentToporobot.GetSerie(Ser);
    //Nd:=GetNoeud(Serie.SerieDep, Serie.PtDep);
    Nd:=GetNoeud(Serie.GetNoSerieDep, Serie.GetNoPointDep);

    // début de série = nouvelle branche
    //++++++++++++++++++++
    // Numéro de série = ID de la série
    Branche0.NoSerie:=Serie.GetIndexSerie;
    // Réseau
    Branche0.NoReseau := Serie.GetNoReseau;
    //++++++++++++++++++++
    Branche0.NoBranche:=Br;
    Branche0.NomBranche:=Format('Branche %d',[Br]);
    Branche0.NoeudDepart:=Nd;
    Branche0.Rigidite   := 1.0; //Serie.Raideur;
    AddBranche(Branche0);
    for Vis:=1 to Serie.GetNbVisees -1 do
    begin
      Visee := Serie.GetVisee(Vis);
      Nd    :=GetNoeud(Serie.GetIndexSerie, Vis);
      //Visee.NoViseeSer:=Vis;
      Visee.NoVisee:=Vis;
      //Visee.TypeGalerie := 0; // type de galerie (provisoire)
      AddBrStation(Br, Visee);
      Branche1:=GetBranche(Br);
      Branche0.PointsTopo:=Branche1.PointsTopo;
      Branche0.NbPoints:=Branche1.NbPoints;
      if ((Nd>-1) AND (Vis < Serie.GetNbVisees-1)) then
      begin
        Branche0.NoeudArrivee:=Nd;
        PutBranche(Br, Branche0);
        Inc(Br);
        //Br:=Br+1;
        Branche0.NoBranche:=Br;
        Branche0.NomBranche:=Format('Branche %d',[Br]);
        Branche0.NoeudDepart:=Nd;
        AddBranche(Branche0);
      end;
      // fin de série = fin de branche: cloturer la branche
      if Vis = Serie.GetNbVisees-1 then begin
        Nd:=GetNoeud(Serie.GetNoSerieArr, Serie.GetNoPointArr);
        Branche0.NoeudArrivee:=Nd;
        PutBranche(Br, Branche0);
        Inc(Br);
        //Br:=Br+1;
      end;
    end;
  end;

  //**********************************************************
  // ajout des barres fictives (entrées de cavités)
  AfficherMessage(rsADDENTRANCES);
  // La première entrée est déjà prise en compte
  for Ser:=0 to FDocumentToporobot.GetNbEntrees - 1 do
  begin   // initialement: for i:=1 to ...
    Entr:= FDocumentToporobot.GetEntree(Ser);
    Branche0.NoSerie      :=Entr.eRefSer;
    Branche0.NoReseau     :=0;
    Branche0.NoeudDepart  :=GetNoeud(1,0);
    Branche0.NoeudArrivee :=GetNoeud(Entr.eRefSer, Entr.eRefSt);
    Branche0.Rigidite     :=1000.0;
    QDeltaX := Entr.eXEntree - FDocumentToporobot.GetDefaultCoordX;
    QDeltaY := Entr.eYEntree - FDocumentToporobot.GetDefaultCoordY;
    QDeltaZ := Entr.eZEntree - FDocumentToporobot.GetDefaultCoordZ;
    Branche0.DeltaX       := QDeltaX; //Entr.eDeltaX;
    Branche0.DeltaY       := QDeltaY; //Entr.eDeltaY;
    Branche0.DeltaZ       := QDeltaZ; //Entr.eDeltaZ;
    //AfficherMessage(Format('--> %d: %f %f %f',[Ser, Entr.eDeltaX, Entr.eDeltaY, Entr.eDeltaZ]));
    Visee.Code:=0; //1
    Visee.Expe:=0; //1

    l1:=0.0;
    a1:=0.00;
    p1:=0.00;
    //GetBearingInc(Entr.eDeltaX, Entr.eDeltaY, Entr.eDeltaZ,
    GetBearingInc(QDeltaX, QDeltaY, QDeltaZ,
                  l1, a1, p1,
                  400,400);
    Visee.NoVisee  := 1;
    Visee.Longueur := l1;
    Visee.Azimut   := a1;
    Visee.Pente    := p1;
    Visee.LD       :=0.00;
    Visee.LG       :=Visee.LD;
    Visee.HZ       :=Visee.LD;
    Visee.HN       :=Visee.LD;
    Visee.Commentaires:='';
    Visee.IDTerrainStation:=Format(FMTSERST,[Entr.eRefSer, Entr.eRefSt]);

    Visee.TypeVisee := tgENTRANCE;
    Visee.Commentaires:=SafeTruncateString(Entr.eNomEntree, 15);
    AddBranche(Branche0);
    AddBrStation(Br, Visee);
    //PutBranche(Br, Branche0);
    Visee:=GetBrStation(Br, 1);
    Inc(Br);
    //Br:=Br+1;
  end;
  //*)
  // affichage de contrôle
  (*
  for Br:=0 to GetNbBranches - 1 do begin
    Branche0 := GetBranche(Br);
    AfficherMessage(Format('Br %d: %d %d (%s > %s) - %.2f %.2f %.2f',
                           [Br, Branche0.NoeudDepart, Branche0.NoeudArrivee,
                            GetIDNoeud(Branche0.NoeudDepart),
                            GetIDNoeud(Branche0.NoeudArrivee),

                            Branche0.DeltaX, Branche0.DeltaY, Branche0.DeltaZ]));
  end; //*)
  //**********************************************************
  AfficherMessage(rsFINDING_BRANCHES + ' OK');
end;

// calcul des accroissements sur les trois axes des branches
// TODO: Vérifier ce code au niveau des corrections sur pentes.
procedure TCodeDeCalcul.CalculerAccroissements;
var
  Br, Vs : integer;
  Branche: TBranche;
  Visee  : TUneVisee;
  DX, DY, DZ: double;
begin
  for Br:=1 to GetNbBranches - 1 do
  begin
    Branche:=GetBranche(Br);
    DX:=0.0; DY:=0.0; DZ:=0.0;
    for Vs:=0 to Branche.PointsTopo.Count-1 do
    begin
      Visee:=GetBrStation(Br, Vs);
      CalculerVisee(Visee, DX, DY, DZ);
      PutBrStation(Br, Vs, Visee);
    end; //for Vs:=0 to Branche.PointsTopo.Count-1 do begin
    Branche.DeltaX:=DX;
    Branche.DeltaY:=DY;
    Branche.DeltaZ:=DZ;
    PutBranche(Br, Branche);
  end; // for Br:=1 to TableBranches.Count-1 do begin
  // affichage de contrôle
  (*
  for Br:=0 to GetNbBranches - 1 do begin
    Branche := GetBranche(Br);
    AfficherMessage(Format('Br %d: %d %d (%s > %s) - %.2f %.2f %.2f',
                           [Br, Branche.NoeudDepart, Branche.NoeudArrivee,
                            GetIDNoeud(Branche.NoeudDepart),
                            GetIDNoeud(Branche.NoeudArrivee),

                            Branche.DeltaX, Branche.DeltaY, Branche.DeltaZ]));
  end; //*)
end;
//-----------------------------------------------------------------------------
// calcul des coordonnées des noeuds
// par la méthode matricielle.
procedure TCodeDeCalcul.CalculerCoordNoeuds;
const NBSTEPS = 6;
var
  tm: TDateTime;
  Hour, Min, Sec, MSec: word;
  t: integer;

  A : TVector;
  Delta : TVector;
  W_Mtx : TVector; // matrice de pondération diagonale
  R : TMatrix;
  B : TMatrix;
  n : integer;
  Branche: TBranche;
  function GetMaxNode: integer;
  var
    Branche: TBranche;
    i      : integer;
    M      : integer;
  begin
    M := Low(Integer);
    for i:=1 to GetNbBranches-1 do
    begin
      Branche:= GetBranche(i);
      if (Branche.NoeudDepart > M)  then M := Branche.NoeudDepart;
      if (Branche.NoeudArrivee > M) then M := Branche.NoeudArrivee;
    end;
    Result:=M;
  end;
  // accroissements des branches
  procedure MakeAVector(const Axe: Byte);
  var
    k: integer;
    Branche: TBranche;
  begin
    SetLength(A, 0);
    SetLength(A, GetNbBranches+1);
    for k:=1 to GetNbBranches-1 do
    begin
      Branche:=GetBranche(k);
      case Axe of
        1: A[k]:=Branche.DeltaX;
        2: A[k]:=Branche.DeltaY;
        3: A[k]:=Branche.DeltaZ;
      end;
    end;
    A[1]:=0.01;
  end;
  // matrice de pondération
  procedure MakeWMatrix;
  var
    i: integer;
  begin
    SetLength(W_mtx, 0);
    SetLength(W_mtx, GetNbBranches+1);
    for i:=1 to GetNbBranches-1 do
      W_mtx[i]:=Getbranche(i).Rigidite;  // paramètre de raideur de la branche
      //W_mtx[i]:=1.0;
  end;
  // second membre
  procedure MakeDeltaVector;
  var
    i,k    : integer;
    ww     : double;
  begin;
    SetLength(Delta, 0);
    SetLength(Delta, n+1);
    for i:=1 to n do
    begin
      ww := 0;
      for k:=1 to GetNbBranches-1 do begin
        //ww:=ww+R[k,i]*A[k];
        ww := ww + R[k,i] * A[k] * W_mtx[k];
      end;
      Delta[i]:=ww;
    end;
  end;
  // matrice de connexion
  procedure MakeRMatrix;
  var
    i,j: integer;
    Branche: TBranche;
  begin
    SetLength(R, 0,0);
    SetLength(R, GetNbBranches, n+1);
    for i:=1 to GetNbBranches-1 do
    begin
      if (i MOD 200 = 0) then AfficherMessage(Format('Ligne %d / %d',[i, GetNbBranches]));
      Branche:=GetBranche(i);
      for j:=1 to N do
      begin
             if (Branche.NoeudDepart  = j) then R[i,j] := -1
        else if (Branche.NoeudArrivee = j) then R[i,j] := +1
        else                                    R[i,j] :=  0;
      end;
    end;
  end;
  procedure FinalizeMatrices;
  begin
    SetLength(R,0,0);
    SetLength(Delta,0);
    SetLength(W_mtx, 0);
    SetLength(A,0);
    SetLength(B,0,0);
  end;

  procedure MakeBMatrix;
  var
    i,j,k: integer;
    ww: double;
    //f: TextFile;
    q: integer;
    LowIndex  : array of Integer;
    HighIndex : array of Integer;

  begin
    AfficherMessage(rsBUILDMATRICE);

    SetLength(B, 0,0);
    SetLength(B, n+1, n+1);
    SetLength(LowIndex,0);
    SetLength(HighIndex,0);
    // rechercher les index mini et maxi des valeurs non nulles de RMatrix

    SetLength(LowIndex,n+1);
    SetLength(HighIndex,n+1);
    AfficherMessage('---> '+ rsFIND_SUM_LIMITS);
    for i:=1 to n do begin
      if (i MOD 200 = 0) then AfficherMessage(Format(rsLINEOFNB,[i, n]));
      for j:=1 to GetNbBranches-1 do begin
        if Abs(R[j, i]) > 0 then begin
          LowIndex[i]:=j;
          Break;
        end;
      end;
    end;
    for i:=1 to n do begin
      if (i MOD 200 = 0) then AfficherMessage(Format(rsLINEOFNB,[i, n]));
      for j:=GetNbBranches-1 downto 1 do begin
        if Abs(R[j,i]) > 0 then begin
          HighIndex[i]:=j;
          Break;
        end;
      end;
    end;
    q:=0;
    for i:=1 to n do begin
      q:=q+1+(HighIndex[i]-LowIndex[i]);
    end;
    AfficherMessage(Format(rsNB_NON_NUL_TERMES,[q]));
    AfficherMessage(Format(rsPR100_NON_NULS,
                            [100* q /(GetNbBranches*n),
                             GetNbBranches, n,
                             GetNbBranches * n]));

    for i:=1 to n do begin
      if (i MOD 200 = 0) then AfficherMessage(Format(rsLINEOFNB,[i, n]));
      for j:=1 to i do begin
        ww:=0;
        for k:=LowIndex[i] to HighIndex[i] do
          ww:=ww+R[k,i]*R[k,j]*W_mtx[k];
        B[i,j]:=ww;
      end;
    end;
    tm:=Now-tm;

    for i:=1 to N-1 do
      for j:=1+i to N do
        B[i,j]:=B[j,i];
    SetLength(LowIndex,0);
    SetLength(HighIndex,0);
  end;
  procedure SolveMatrix(const Axe: byte);
  var
    i, j, k   : integer;
    vv        : double;
    ww        : double;
    V_Matrix  : TMatrix; // matrice locale de factorisation
    S_Vector  : TVector; // vecteur local complémentaire
    XX        : TVector;
  begin
    try
      SetLength(XX, 0);
      SetLength(XX, n+1);
      SetLength(V_Matrix, 0,0);
      SetLength(V_Matrix, n+1, n+1);
      SetLength(S_Vector, 0);
      SetLength(S_Vector, GetNbBranches+1);
      for i:=1 to n do XX[i]:=0.0;
      // descente: V.V* = A
      AfficherMessage(rsDESCENTE);
      for i:=1 to n do
      begin
        if (i mod 100 = 0) then AfficherMessage(Format(rsLINEOFNB,[i, n]));
        vv:=0;
        for k:=1 to i-1 do
          vv:=vv+sqr(V_Matrix[i,k]);
        V_Matrix[i,i]:=Sqrt(abs(B[i,i]-vv));
        for j:=1+i to n do
        begin
          ww:=0;
          for k:=1 to i-1 do
            ww:=ww + V_Matrix[i,k] * V_Matrix[j,k];
          V_Matrix[j,i] := (B[i,j] - ww) / (V_Matrix[i,i] + 1e-24);
        end;
      end;
      //ShowMessage('Descente OK');
      // second membre; triangularisation
      AfficherMessage(rsTRIANGULARISATION);
      for i:=1 to n do
      begin
        if (i mod 200 = 0) then AfficherMessage(Format(rsLINEOFNB,[i, n]));
        ww := 0;
        for k:=1 to i-1 do ww := ww + V_Matrix[i,k] * S_Vector[k];
        S_Vector[i]  := (Delta[i] - ww) / (V_Matrix[i,i] + 1e-24);   // Dans l'actuelle version de GHTopo (Delphi 5)
      end;
      // remontée du système; inconnues recherchées
      AfficherMessage(rsREMONTEE);
      for i:=n downto 1 do
      begin
        if (i mod 200 = 0) then AfficherMessage(Format(rsLINEOFNB,[i, n]));
        ww := 0;
        for k:=1+i to n do
          ww:=ww+ V_Matrix[k,i] * XX[k];
        XX[i]:=(S_Vector[i] - ww) / (V_Matrix[i,i] + 1e-24);
      end;
      XX[0] := XX[1];

      // Table des Noeuds
      for i:=1 to n do
      begin
        XX[i]:=XX[i]-XX[0];
        FTableNoeuds[i].IDNoeud:=i;
        if (i mod 200 = 0) then AfficherMessage(Format(rsLINEOFNB,[i, n]));
        case Axe of
        1: FTableNoeuds[i].XN:=XX[i]+ FDocumentToporobot.GetDefaultCoordX;
        2: FTableNoeuds[i].YN:=XX[i]+ FDocumentToporobot.GetDefaultCoordY;
        3: FTableNoeuds[i].ZN:=XX[i]+ FDocumentToporobot.GetDefaultCoordZ;
        end;
      end;
      // ++++++++++++++++
      // Passage par cette ligne = c'est OK
      AfficherMessage(Format(rsCOORDONNEES_OK,[n]));
    finally
      AfficherMessage(rsDEL_TEMP_MATRIX);
      SetLength(V_Matrix,0,0);
      SetLength(S_Vector, 0);
      SetLength(XX,0);
    end;
  end;
  procedure WriteTableNodesInFile(const Fichier: string);
  var
   pTXT: TextFile;
    q: integer;
  begin
    AssignFile(pTXT, Fichier);
    try
      ReWrite(pTXT);
      WriteLn(pTXT, 'Nodes table - Calculated at '+ DateTimeToStr(Now()));
      WriteLn(pTXT, rsNODES_COORDS_FOR_DB + FDocumentToporobot.GetDatabaseName);
      WriteLn(pTXT, StringOfChar('=', 60));
      for q:=Low(FTableNoeuds) to High(FTableNoeuds) do begin
        WriteLn(pTXT, Format('%d'+TAB+'%.2f'+TAB+'%.2f'+TAB+'%.2f',[q,
                                          FTableNoeuds[q].XN,
                                          FTableNoeuds[q].YN,
                                          FTableNoeuds[q].ZN
                                          ]));

      end;
    finally
      CloseFile(pTXT);
    end;
  end;
begin
  tm:=Now;
  n:=GetMaxNode;
  SetLength(FTableNoeuds, 0);
  SetLength(FTableNoeuds, n+1);
  AfficherMessage(rsCALCULNODES);
  AfficherMessage(rsBINDMATRIX);
  AfficherMessage(Format(rsSTEP_CALC_01,[1,NBSTEPS]));
  MakeRMatrix; // matrice d'assemblage
  AfficherMessage(Format(rsSTEP_CALC_02,[2,NBSTEPS]));
  MakeWMatrix; // matrice de pondération
  tm:=Now;
  AfficherMessage(Format(rsSTEP_CALC_03,[3,NBSTEPS]));

  MakeBMatrix; // matrice de compensation

  AfficherMessage(Format(rsSTEP_CALC_04,[4,NBSTEPS]));

  for t:=1 to 3 do
  begin
    AfficherMessage('');
    AfficherMessage(AnsiToUtf8(rsFACTORISEMATRICE));
    AfficherMessage(rsAXIS+Chr(87+t));
    //MakeRMatrix;
    MakeAVector(t);
    AfficherMessage(AnsiToUtf8(rs2NDMEMBER));
    MakeDeltaVector;
    AfficherMessage(AnsiToUtf8(rsCOMPESMATRIX));
    SolveMatrix(t);
  end;
  tm:=Now-tm;
  WriteTableNodesInFile(ChangeFileExt(FDocumentToporobot.GetDatabaseName, '.nds'));
  // assignation des écarts
  AfficherMessage('');
  AfficherMessage(AnsiToUtf8(Format(rsSTEP_CALC_05,[5,NBSTEPS])));
  for t:=1 to GetNbBranches - 1 do
  begin
    Branche := GetBranche(t);
    with Branche do
    begin
      XDepart:=FTableNoeuds[NoeudDepart].XN;
      YDepart:=FTableNoeuds[NoeudDepart].YN;
      ZDepart:=FTableNoeuds[NoeudDepart].ZN;
      XArrivee:=FTableNoeuds[NoeudArrivee].XN;
      YArrivee:=FTableNoeuds[NoeudArrivee].YN;
      ZArrivee:=FTableNoeuds[NoeudArrivee].ZN;
    end;
    PutBranche(t,Branche);
  end;
  AfficherMessage('');
  AfficherMessage(AnsiToUtf8(Format(rsSTEP_CALC_06,[6,NBSTEPS])));
  AfficherMessage(AnsiToUtf8(rsFREE_TEMP_VARS));
  FinalizeMatrices;
end;
// répartition des écarts de fermeture entre les noeuds
procedure TCodeDeCalcul.RepartirEcarts;
var
  i, s: integer;
  V, V1, V2: TUneVisee;
  Branche: TBranche;
  EcartX : double;
  EcartY : double;
  EcartZ : double;
  EpsX, EpsY, EpsZ: double;
  Xo, Yo, Zo: double;
  XXo,YYo, ZZo: double;

  R      : double;
begin;

  AfficherMessage('');
  AfficherMessage(AnsiToUtf8(rsREPARTIR_ECARTS));
  for i:=1 to GetNbBranches - 1 do
  begin
    Branche:=GetBranche(i);
    with Branche do begin
      EcartX:= (XArrivee - XDepart) - DeltaX;
      EcartY:= (YArrivee - YDepart) - DeltaY;
      EcartZ:= (ZArrivee - ZDepart) - DeltaZ;
      // calcul de R
      R:=1e-10;
      for s:=0 to PointsTopo.Count-1 do
      begin
        V:=GetBrStation(i, s);
        R:=R+Sqrt(v.x*v.x + v.y*v.y + v.z*v.z);
      end;
      // mettre l'azimut de V[1] dans V[0]
      if PointsTopo.Count>1 then
      begin
        V1:=GetBrStation(i,0);
        V2:=GetBrStation(i,1);
        V1.Azimut:=V2.Azimut;
        PutBrStation(i, 0, V1);
      end;
      // calcul de répartition
      EpsX:=EcartX/R;
      EpsY:=EcartY/R;
      EpsZ:=EcartZ/R;
      Xo:=Branche.XDepart;
      Yo:=Branche.YDepart;
      Zo:=Branche.ZDepart;
      R:=1e-10;
      XXo:=Branche.XDepart;
      YYo:=Branche.YDepart;
      ZZo:=Branche.ZDepart;
      for s:=0 to PointsTopo.Count-1 do begin
        V:=GetBrStation(i, s);
        R:=R+Sqrt(v.x*v.x + v.y*v.y + v.z*v.z);
        XXo:=XXo+V.X;
        YYo:=YYo+V.Y;
        ZZo:=ZZo+V.Z;

        Xo:=XXo + R * EpsX;
        Yo:=YYo + R * EpsY;
        Zo:=ZZo + R * EpsZ;

        v.X :=Xo;
        v.Y :=Yo;
        v.Z :=Zo;
        PutBrStation(i, s, V);

      end;
    end;
  end;    // for i, Br
end;

// calcul des contours de galeries
procedure TCodeDeCalcul.CalculContoursGaleries(const FichierTOP: string);
var
  N           : integer;
  Br, St, i, j: integer;
  Branche: TBranche;
  V1, V2      : TUneVisee;
  Entite      : TEntite;
  Expe        : TExpe;
  Code        : TCode;
  pTOP        : file of TEntite;
  AReseau     : TReseau;
  s: string;
  AlphaG, AlphaD: double;
  TabVisee    : array of TStation;
  ca, sa: double;
  VA          : TViseeAntenne;
  Palette256  : TPalette256;
  // code et expé
  //QQ_CODE_IDX,
  //QQ_EXPE_IDX : integer;
  function SetColorVisee(const Idx: Integer): TColor;
  begin
    try
      Result:=Palette256.GetColorByIndex(Idx);
    except
      Result:=clSilver;
    end;
  end;
  // TODO: Vérifier cette fonction et l'enrichir
  // DONE: Support des visées en antenne OK
  function CalcViseeAntenne(const VA: TViseeAntenne; const QNo: integer): TEntite;
  var
    VS      : TUneVisee;
    NumNoeud: integer;
    ANoeud  : TNoeud;
    DX, DY, DZ: double;
    s       : string;
  begin
    NumNoeud := GetNoeud(VA.SerieDepart, VA.PtDepart);
    Affichermessage(Format('Found node: %d (%d/%d) - Nb of junctions: %d',[NumNoeud, VA.SerieDepart, VA.PtDepart, GetNbJonctions]));
    ANoeud   := FTableNoeuds[NumNoeud];
    Result.Une_Station_1_X := ANoeud.XN;
    DX := ANoeud.XN; DY := ANoeud.YN; DZ := ANoeud.ZN;
    //DX:=0.0; DY:=0.0; DZ:=0.0;
    VS.Code            := VA.Code;
    VS.Expe            := VA.Expe;
    VS.Longueur        := VA.Longueur;
    VS.Azimut          := VA.Azimut;
    VS.LG := 0.00;
    VS.LG := 0.00;
    VS.HZ := 0.00;
    VS.HN := 0.00;
    VS.Commentaires     := VA.Commentaires;
    VS.IDTerrainStation := VA.IDTerrainStation;
    VS.TypeVisee        := tgTYPE_ENTITES_VISEES_ANTENNE; //7; // les antennes sont des cheminements spéciaux

    CalculerVisee(VS, DX, DY, DZ);

    DX := ANoeud.XN + VS.X;
    DY := ANoeud.YN + VS.Y;
    DZ := ANoeud.ZN + VS.Z;
    // coordonnées extrémités de la visée
    with Result do begin
      // extrémités de visée
      Une_Station_1_X := ANoeud.XN;
      Une_Station_1_Y := ANoeud.YN;
      Une_Station_1_Z := ANoeud.ZN;
      //----------
      Une_Station_2_X := DX;
      Une_Station_2_Y := DY;
      Une_Station_2_Z := DZ;
      // volumes
      //-----------------------
      X1PD := ANoeud.XN;
      Y1PD := ANoeud.YN;
      Z1PH := ANoeud.ZN;
      X1PG := ANoeud.XN;
      Y1PG := ANoeud.YN;
      Z1PB := ANoeud.ZN;

      X2PD := DX;
      Y2PD := DY;
      Z2PH := DZ;
      X2PG := DX;
      Y2PG := DY;
      Z2PB := DZ;
    end;
    // attributs de visée
    // Série 0 affectée aux visées en antenne

    with Result do begin
      // TODO: Etudier une numérotation pour GHCaveDraw
      // risque de conflit de numéro avec les séries à départ en n.0
      Drawn          := True;
      IdxReseau      := VA.Reseau;
      Entite_Serie   := -QNo;      // Antenne -> numéro de série négatif.
      Entite_Station := 0;         // Antenne -> numéro de visée nul
      // libellé
      s:= String(VA.IDTerrainStation);
      //----------------------------------------------
      StrLCopy(ID_Litteral_Pt, pchar(s), length(s));;

      //ID_Litteral_Pt := '';
      ColorReseau    := COULEUR_VISEE_ANTENNE;
      // Couleur d'une visée en antenne:
      ColorEntite    := COULEUR_VISEE_ANTENNE;

      Type_Entite    := tgTYPE_ENTITES_VISEES_ANTENNE;
      // TODO: Implémenter le support des dates
      DateLeve       := Now();
      eCode          := VA.Code;
      eExpe          := VA.Expe;
    end;
  end;
begin;

  // chargement de la palette
  AfficherMessage('');
  AfficherMessage(AnsiToUtf8(rsCALCULCONTOURS));
  Palette256:=TPalette256.Create;
  try
    Palette256.GenerateTOPOROBOTPalette;

    AssignFile(pTOP, FichierTOP);
    ReWrite(pTOP);
    N:=0;


    AfficherMessage(AnsiToUtf8(rsSCAN_BRCHS));
    for Br:=1 to GetNbBranches - 1 do begin
      Branche:=GetBranche(Br);
      with Branche do begin
        SetLength(TabVisee, 2+PointsTopo.Count);
        // extraire le réseau
        AReseau := FDocumentToporobot.GetReseau(Branche.NoReseau);
        for St:=0 to PointsTopo.Count-1 do
        begin
          V1:=GetBrStation(Br, St);
          with TabVisee[St] do begin
            NumPoint:=V1.NoVisee; //  NumPoint:=V1.NoViseeSer;
            j:=V1.Expe;
            Expe:=FDocumentToporobot.GetExpeByIndex(j);
            j:=V1.Code;
            Code:=FDocumentToporobot.GetCodeByIndex(j);
            stCode := Code.IDCode;
            stExpe := Expe.IDExpe;

            Couleur:=SetColorVisee(Expe.Couleur);

            TypeGalerie := V1.TypeVisee;
            j:=Expe.AnneeExpe;
            Date:=SafeEncodeDate(j, Expe.MoisExpe, Expe.JourExpe); //TODO Corriger dates

            Longueur:=V1.Longueur;
            Azimut  :=V1.Azimut;
            Pente   :=V1.Pente;

            LD      :=V1.LD;
            LG      :=V1.LG;
            HZ      :=V1.HZ;
            HN      :=V1.HN;
            //*)
            Commentaire := V1.Commentaires;
            TabVisee[St].IDTerrainStation := V1.IDTerrainStation;

            X:=V1.X;
            Y:=V1.Y;
            Z:=V1.Z;

          end; // with TabVisee[St] do begin
        end;   // for St:=0 to PointsTopo.Count-1 do begin
        TabVisee[0].LD:=TabVisee[1].LD;
        TabVisee[0].LG:=TabVisee[1].LG;
        TabVisee[0].HZ:=TabVisee[1].HZ;
        TabVisee[0].HN:=TabVisee[1].HN;
        TabVisee[0].TypeGalerie:=TabVisee[1].TypeGalerie;

        TabVisee[PointsTopo.Count]   :=TabVisee[PointsTopo.Count-1];
        TabVisee[PointsTopo.Count].X := TabVisee[PointsTopo.Count-1].X +
                                          (TabVisee[PointsTopo.Count-1].X-
                                           TabVisee[PointsTopo.Count-2].X);
        TabVisee[PointsTopo.Count].Y := TabVisee[PointsTopo.Count-1].Y +
                                          (TabVisee[PointsTopo.Count-1].Y-
                                           TabVisee[PointsTopo.Count-2].Y);

        // calcul contours
        for St:=1 to PointsTopo.Count-1 do begin
          if St=1 then begin
            AlphaD:=CalculerAngles(TabVisee[St].X-TabVisee[St-1].X,
                                   TabVisee[St].Y-TabVisee[St-1].Y,
                                   TabVisee[St].X-TabVisee[St-1].X,
                                   TabVisee[St].Y-TabVisee[St-1].Y);
            AlphaG:=AlphaD+PI
            //Retourner_Angles(Alpha1, Alpha2);
          end;
          with Entite do begin
                      // expé& et code
            eCode := TabVisee[St].stCode;
            eExpe := TabVisee[St].stExpe;

            //s:=String(Format('%d/%d',[Br, St]));
            //----------------------------------------------
            // Modif 02/09/06
            // on définit les ID littéraux des stations
            //s:=String(Format(FMTSERST,[Branche.NoSerie, TabVisee[St].NumPoint]));
            s:= String(GetIDStation(Branche.NoSerie, TabVisee[St]));
            //----------------------------------------------
            StrLCopy(ID_Litteral_Pt, pchar(s), length(s));;
            //s:=String(Format('%d.%d',[Branche.NoSerie, TabVisee[St].NumPoint]));
            //StrLCopy(ID_Litteral_Pt, pchar(s), length(s));;
            // réseau
            IdxReseau   := AReseau.IdxReseau;
            ColorReseau := AReseau.ColorReseau;
            // Extraire la couleur
            ColorEntite:=TabVisee[St].Couleur;

            Type_Entite:=TabVisee[St].TypeGalerie; //2
            DateLeve:=TabVisee[St].Date;

            //DateLeve:=Now;
            Entite_Serie   :=Branche.NoSerie;
            Entite_Station :=TabVisee[St].NumPoint;

            Une_Station_1_X:=TabVisee[St-1].X ;
            Une_Station_1_Y:=TabVisee[St-1].Y ;
            Une_Station_1_Z:=TabVisee[St-1].Z ;
            Une_Station_2_X:=TabVisee[St].X ;
            Une_Station_2_Y:=TabVisee[St].Y ;
            Une_Station_2_Z:=TabVisee[St].Z ;

            X1PD:=Une_Station_1_X + TabVisee[St-1].LD*Cos(AlphaD);
            Y1PD:=Une_Station_1_Y + TabVisee[St-1].LD*sin(AlphaD);
            X1PG:=Une_Station_1_X + TabVisee[St-1].LG*Cos(AlphaG);
            Y1PG:=Une_Station_1_Y +TabVisee[St-1].LG*Sin(AlphaG);
            Z1PH:=Une_Station_1_Z +TabVisee[St-1].HZ;
            Z1PB:=Une_Station_1_Z -TabVisee[St-1].HN;
            AlphaD:=CalculerAngles(TabVisee[St].X-TabVisee[St-1].X,
                                   TabVisee[St].Y-TabVisee[St-1].Y,
                                   TabVisee[St+1].X-TabVisee[St].X,
                                   TabVisee[St+1].Y-TabVisee[St].Y);
            AlphaG:=AlphaD+PI;
            X2PD:=Une_Station_2_X + TabVisee[St].LD*Cos(AlphaD);
            Y2PD:=Une_Station_2_Y + TabVisee[St].LD*sin(AlphaD);
            X2PG:=Une_Station_2_X + TabVisee[St].LG*Cos(AlphaG);
            Y2PG:=Une_Station_2_Y + TabVisee[St].LG*Sin(AlphaG);
            Z2PH:=Une_Station_2_Z +TabVisee[St].HZ;
            Z2PB:=Une_Station_2_Z -TabVisee[St].HN;

          end;
          Seek(pTOP, N);
          Write(pTOP, Entite);
          Inc(N);

        end;
        SetLength(TabVisee, 0);
      end; // with branche
    end;   // for Br

    AfficherMessage(rsSCAN_BRCHS + ' OK');

    // calculer les visées en antenne
    // DONE: Support des visées en antenne OK

    AfficherMessage(AnsiToUtf8(rsCALCUL_ANTENNES));
    if (FDocumentToporobot.GetNbAntennes > 1) then begin
      for i := 1 to FDocumentToporobot.GetNbAntennes - 1 do begin
        VA := FDocumentToporobot.GetViseeAntenne(i);
        Entite := CalcViseeAntenne(VA, i);
        Seek(pTOP, N);
        Write(pTOP, Entite);
        Inc(N);
      end
    end else begin
      AfficherMessage(' --> No antenna shots');
    end;
    // visée bidon de fin
    with Entite do begin
      ColorEntite:=clBlack;
      Type_Entite:=tgSURFACE;
      Une_Station_1_X:=0;
      Une_Station_1_Y:=0;
      Une_Station_1_Z:=0;
      Une_Station_2_Z:=0;
      Une_Station_2_X:=0;
      Une_Station_2_Y:=0;
      ID_Litteral_Pt:='';
      X1PD:=0;   X1PG:=0;  X2PD:=0;  X2PG:=0; Z1PH:=0; Z2PH:=0;
      Y1PD:=0;   Y1PG:=0;  Y2PD:=0;  Y2PG:=0; Z1PB:=0; Z2PB:=0;
    end;
    Seek(pTOP, N);
    Write(pTOP, Entite);

    //*)

    //AfficherMessage('Contours galeries OK');
    // vider les tables des branches = purge des sous-listes
    for i:=0 to GetNbBranches - 1 do begin
      Branche:=GetBranche(i);
      for j:=0 to Branche.PointsTopo.Count-1 do Dispose(Branche.PointsTopo.Items[j]);
      Branche.PointsTopo.Clear;
      PutBranche(i, Branche);
    end;
    // destruction des pointeurs de branches
    for i:=0 to GetNbBranches - 1 do
    begin
      Dispose(FTableBranches.Items[i]);
    end;
    FTableBranches.Clear;
    AfficherMessage('Libération palette de couleurs');
  // libération de la palette
  finally
    CloseFile(pTOP);
    Palette256.Free;
    AfficherMessage(rsCALCULCONTOURS + ' OK');
  end;
end;

end.


//************************
{ TObjBranche }
(*
procedure TObjBranche.ClearBranche;
var
  ii: Integer;
begin
  try
    if (self.Count > 0) then for ii := 0 to self.Count - 1 do Dispose(Items[ii]);
  finally
    self.Clear;
  end;
end;

function TObjBranche.GetNbPoints: integer;
begin
  Result := self.Count;
end;

procedure TObjBranche.AddVisee(const V: TUneVisee);
var
  pV: ^TUneVisee;
begin
  New(pV);
  pV^ := V;
  self.Add(pV);
end;


function TObjBranche.GetVisee(const Idx: integer): TUneVisee;
var
  pV: ^TUneVisee;
begin
  pV := self.Items[Idx];
  Result := pV^;
end;

procedure TObjBranche.PutVisee(const Idx: integer; const V: TUneVisee);
var
  pV: ^TUneVisee;
begin
  pV := self.Items[Idx]; // Récupérer le pointeur ^^
  pV^ := V;
  self.Items[Idx] := pV;
end;

procedure TObjBranche.SetNoSerie(const N: integer);
begin
  FNoSerie:=N;
end;

procedure TObjBranche.SetNoBranche(const N: integer);
begin
  FNoBranche:=N;
end;

procedure TObjBranche.SetNoReseau(const N: integer);
begin
  FNoReseau:=N;
end;

procedure TObjBranche.SetNomBranche(const N: string);
begin
  FNomBranche:=N;
end;

procedure TObjBranche.SetNoeudDepart(const N: integer);
begin
  FNoeudDepart := N;
end;

procedure TObjBranche.SetNoeudArrivee(const N: integer);
begin
  FNoeudArrivee := N;

end;

procedure TObjBranche.SetRigidite(const R: double);
begin
  FRigidite:= R;
end;

procedure TObjBranche.SetDelta(const P: TPoint3Df);
begin
  FDelta  := P;
end;

procedure TObjBranche.SetCoordDepart(const P: TPoint3Df);
begin
  FDepart := P;
end;

procedure TObjBranche.SetCoordArrivee(const P: TPoint3Df);
begin
  FArrivee := P;
end;

function TObjBranche.GetNoSerie: integer;
begin
  Result := FNoSerie;
end;

function TObjBranche.GetNoBranche: integer;
begin
  Result := FNoBranche;
end;

function TObjBranche.GetNoReseau: integer;
begin
  Result := FNoReseau;
end;

function TObjBranche.GetNomBranche: string;
begin
  Result := FNomBranche;
end;

function TObjBranche.GetNoeudDepart: integer;
begin
  Result := FNoeudDepart;
end;

function TObjBranche.GetNoeudArrivee: integer;
begin
  Result := FNoeudArrivee;
end;

function TObjBranche.GetRigidite: double;
begin
  Result := FRigidite;
end;

function TObjBranche.GetDelta: TPoint3Df;
begin
  Result := FDelta;
end;

function TObjBranche.GetCoordDepart: TPoint3Df;
begin
  Result := FDepart;
end;

function TObjBranche.GetCoordArrivee: TPoint3Df;
begin
  Result := FArrivee;
end;
//*)
