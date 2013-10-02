unit ObjetSerie;
// Date: 24/08/2012
// Contient l'objet TObjetSerie, utilisé par:
// - ToporobotClasses2012.pas
// - CodeCalculTopo.pas
// - Le cadre Series, nouvelle version
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
  Common,
  Classes, SysUtils, Graphics;
type

{ TObjSerie }

 TObjSerie = class(TList)
  private
    FIndexSerie   : integer;
    FSerieDep    : integer;
    FPtDep       : integer;
    FSerieArr    : integer;
    FPtArr       : integer;
    FReseau      : integer; // Réseau
    FChances     : integer;
    FObstacles   : integer;
    FCouleur     : TColor;
    FRaideur     : double;  // module de rigidité, égal à 1.00 par défaut
    FNomSerie    : String;
    FCommentaires: string;

  public
    procedure ClearSerie;
    function  GetNbVisees: integer;
    procedure AddVisee(const V: TUneVisee); overload;
    procedure AddVisee(const qNoVisee, qCode, qExpe: integer;
                       const qTypeVisee: TTypeDeVisee;
                       const qLong, qAz, qP: double;
                       const qLG, qLD, qHZ, qHN: double;
                       const qIDTerrain, qComments: string); overload;
    function  GetVisee(const Idx: integer): TUneVisee;
    procedure PutVisee(const Idx: integer; const V: TUneVisee);
    procedure DeleteVisee(const Idx: integer);

    procedure SetIndexSerie(const N: integer);

    procedure SetNomSerie(const S: string);
    procedure SetObsSerie(const S: string);
    procedure SetNoSerieDep(const N: integer);
    procedure SetNoPointDep(const N: integer);
    procedure SetNoSerieArr(const N: integer);
    procedure SetNoPointArr(const N: integer);
    procedure SetNoReseau(const N: integer);
    procedure SetChance(const N: integer);
    procedure SetObstacle(const N: integer);
    procedure SetCouleur(const C: TColor);
    procedure SetRaideur(const R: double);

    procedure SetChanceObstacle(const C, O: integer);
    procedure SetNomObsSerie(const N, S: string);
    procedure SetSeriePtArr(const S, P: integer);
    procedure SetSeriePtDep(const S, P: integer);
    procedure SetSeriePtExtremites(const Sd, Pd, Sa, Pa: integer);

    function GetIndexSerie: integer;
    function GetNomSerie  : string;
    function GetObsSerie  : string;
    function GetNoSerieDep: integer;
    function GetNoPointDep: integer;
    function GetNoSerieArr: integer;
    function GetNoPointArr: integer;
    function GetNoReseau  : integer;
    function GetChance    : integer;
    function GetObstacle  : integer;
    function GetCouleur   : TColor;
    function GetRaideur   : double;



end;

implementation
{ TObjSerie }

procedure TObjSerie.ClearSerie;
var
  ii: Integer;
begin
  try
    if (self.Count > 0) then for ii := 0 to self.Count - 1 do Dispose(Items[ii]);
  finally
    self.Clear;
  end;
end;



function TObjSerie.GetNbVisees: integer;
begin
  Result := self.Count;
end;

procedure TObjSerie.AddVisee(const V: TUneVisee); overload;
var
  pV: ^TUneVisee;
begin
  New(pV);
  pV^ := V;
  self.Add(pV);
end;
procedure TObjSerie.AddVisee(const qNoVisee, qCode, qExpe: integer;
                             const qTypeVisee: TTypeDeVisee;
                             const qLong, qAz, qP: double;
                             const qLG, qLD, qHZ, qHN: double;
                             const qIDTerrain, qComments: string); overload;
var
  WU: TUneVisee;
begin
  //WU.NoVisee     := self.Count - 1;//qNoVisee;
  WU.Code        := qCode;
  WU.Expe        := qExpe;
  WU.TypeVisee   := qTypeVisee;
  WU.Longueur    := qLong;
  WU.Azimut      := qAz;
  WU.Pente       := qP;
  WU.LG          := qLG;
  WU.LD          := qLD;
  WU.HZ          := qHZ;
  WU.HN          := qHN;
  WU.IDTerrainStation := qIDTerrain;
  WU.Commentaires     := qComments;
  WU.X := 0.00;
  WU.Y := 0.00;
  WU.Z := 0.00;
  self.AddVisee(WU);
end;


function TObjSerie.GetVisee(const Idx: integer): TUneVisee;
var
  pV: ^TUneVisee;
begin
  pV := self.Items[Idx];
  Result := pV^;
end;

procedure TObjSerie.PutVisee(const Idx: integer; const V: TUneVisee);
var
  pV: ^TUneVisee;
begin
  pV := self.Items[Idx]; // Récupérer le pointeur ^^
  pV^ := V;
  self.Items[Idx] := pV;
end;

procedure TObjSerie.DeleteVisee(const Idx: integer);
begin
  Dispose(Items[Idx]);
  self.Delete(Idx);
end;

procedure TObjSerie.SetIndexSerie(const N: integer);
begin
  FIndexSerie:=N;
end;

procedure TObjSerie.SetNomSerie(const S: string);
begin
  FNomSerie := S;
end;

procedure TObjSerie.SetObsSerie(const S: string);
begin
  FCommentaires := S;
end;

procedure TObjSerie.SetNomObsSerie(const N, S: string);
begin
  FNomSerie := N;
  FCommentaires := S;
end;

procedure TObjSerie.SetNoSerieDep(const N: integer);
begin
  FSerieDep := N;
end;

procedure TObjSerie.SetNoPointDep(const N: integer);
begin
  FPtDep := N;
end;

procedure TObjSerie.SetNoSerieArr(const N: integer);
begin
  FSerieArr := N;
end;

procedure TObjSerie.SetNoPointArr(const N: integer);
begin
  FPtArr    := N;
end;

procedure TObjSerie.SetSeriePtDep(const S, P: integer);
begin
  FSerieDep := S;
  FPtDep    := P;
end;
procedure TObjSerie.SetSeriePtArr(const S, P: integer);
begin
  FSerieArr := S;
  FPtArr    := P;
end;
procedure TObjSerie.SetSeriePtExtremites(const Sd, Pd, Sa, Pa: integer);
begin
  //AfficherMessage(Format('Depart: %d.%d - Arrivee: %d.%d',[Sd, Pd, Sa, Pa]));
  FSerieDep := Sd;
  FPtDep    := Pd;
  FSerieArr := Sa;
  FPtArr    := Pa;
end;



procedure TObjSerie.SetNoReseau(const N: integer);
begin
  FReseau := N;
end;

procedure TObjSerie.SetChance(const N: integer);
begin
  FChances := N;
end;

procedure TObjSerie.SetObstacle(const N: integer);
begin
  FObstacles := N;
end;
procedure TObjSerie.SetChanceObstacle(const C, O: integer);
begin
  FChances := C;
  FObstacles := O;
end;

procedure TObjSerie.SetCouleur(const C: TColor);
begin
  FCouleur := C;
end;

procedure TObjSerie.SetRaideur(const R: double);
begin
  FRaideur := R;
end;

function TObjSerie.GetIndexSerie: integer;
begin
  Result := FIndexSerie;
end;

function TObjSerie.GetNomSerie: string;
begin
  Result := FNomSerie;
end;

function TObjSerie.GetObsSerie: string;
begin
  Result := FCommentaires;
end;

function TObjSerie.GetNoSerieDep: integer;
begin
  Result := FSerieDep;
end;

function TObjSerie.GetNoPointDep: integer;
begin
  Result := FPtDep;
end;

function TObjSerie.GetNoSerieArr: integer;
begin
  Result := FSerieArr;

end;

function TObjSerie.GetNoPointArr: integer;
begin
  Result := FPtArr;

end;

function TObjSerie.GetNoReseau: integer;
begin
  Result := FReseau;

end;

function TObjSerie.GetChance: integer;
begin
  Result := FChances;

end;

function TObjSerie.GetObstacle: integer;
begin
  Result := FObstacles;
end;

function TObjSerie.GetCouleur: TColor;
begin
  Result := FCouleur;

end;

function TObjSerie.GetRaideur: double;
begin
  Result := FRaideur;
end;
end.

