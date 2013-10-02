unit ConvertisseurJPC;
// shunte l'unité Donnee.pas
//
// Statut: Opérationnel. A valider de manière approfondie.
// TODO: Nettoyer le code de certaines unités, dont les fonctions appelant TSream
// DONE: Le symbole degré, qui posait de gros problèmes, est remplacé par 'd'
// DONE: Seuls les degrés décimaux sont utilisés bien que traités en interne
{$mode delphi}
interface

uses
  outils,
  createur,
  ellipsoide,
  datum,
  projection,
  multiple,
  Common, StructuresDonnees,
  Classes, Contnrs,
  SysUtils;

type

  { TConvertisseurJPC }

  { TConversionSysteme }

  TConversionSysteme = class


  private
    // liste des systèmes géodésiques
    FListeDesSystemes: array of string;
    FNbreSystemes    : integer;

    fNomJeuDeDonnees: string;
    fLesEllipsoide: TObjectList;
    fLesDatum: TObjectList;
    fLesProjection: TObjectList;
    fLesMultiple: TObjectList;

    function GenererListeDesSystemes: boolean;
    // conversions
    function Conversion(NomSource, NomDest, XSource, YSource: string;
      IndiceSource: integer; var IndiceDest: integer; var XDest, YDest: string;
      UniteAngle: TUniteAngle): boolean;
    function ConversionNum(NomSource, NomDest: string; XSource, YSource: real;
      IndiceSource: integer; var IndiceDest: integer; var XDest, YDest: real
      ): boolean;

    function DepuisGeocentrique(XGeo, YGeo, ZGeo: real; NomDest: string;
      var IndiceDest: integer; var XDest, YDest: string; UniteAngle: TUniteAngle
      ): boolean;
    function DepuisGeocentriqueNum(XGeo, YGeo, ZGeo: real; NomDest: string;
      var IndiceDest: integer; var XDest, YDest: real): boolean;
    function VersGeocentrique(NomSource, XSource, YSource: string;
      IndiceSource: integer; var XGeo, YGeo, ZGeo: real): boolean;
    function VersGeocentriqueNum(NomSource: string; XSource, YSource: real;
      IndiceSource: integer; var XGeo, YGeo, ZGeo: real): boolean;

     // ellipsoides
    function GetNbEllipsoides: integer;
    function GetEllipsoide(const Idx: integer): TEllipsoide;
    function GetIdxEllipsoideByName(const qNom: string): integer;
    procedure AddEllipsoideByParameters(const qNom, qDescription: string; const a, e: double);
     // datums
    function GetNbDatums: integer;
    function GetDatum(const Idx: integer): TDatum;
    function GetIdxDatumByName(const qNom: string): integer;
    procedure AddDatumByParameters(const qNom, qDescription, qNomEllipsoide: string;
      const qDX, qDY, qDZ, qRotX, qRotY, qRotZ, qScale: double;
      const qGrilleCorrection: string);
     // projections
    function GetNbProjections: integer;
    function GetProjection(const Idx: integer): TProjection;
    function GetIdxProjectionByName(const qNom: string): integer;
    procedure AddProjectionByParameters(const qNatureProj, qNom, qDescription, qDatum: string; const qParametres: TStringArray);
    // multiples
    function GetNbMultiples: integer;
    function GetMultiple(const Idx: integer): TMultiple;
    function GetIdxMultipleByName(const qNom: string): integer;
    procedure AddMultipleByParameters(const qNatureMultiple, qNom, qDescription, qDatum: string; const qParametres: TStringArray);
  public
    constructor Create;
    destructor Destroy;



    // chargement du fichier de paramétrage
    procedure LoadFromFichier(const Filename: string);
    //**************************************************************************
    // Procédures communes aux autres convertisseurs
    function Initialiser: boolean;
    procedure Finaliser;
    // attraper le nombre de projections
    function GetNbSystemes: integer;
    // attraper une projection donnée
    function GetNomSysteme(const Idx: integer): string;

    // conversion d'un point
    function ConversionSyst1ToSyst2(const SystSrc, SystCible: string; const MyPoint: TProjUV): TProjUV;

  end;

implementation

const
  aWGS84: real = 6378137;
  eWGS84: real = 0.08181919106;
  //  bWGS84 : real = aWGS84*sqrt(1-sqr(eWGS84));
  bWGS84: real = 6335439;
const
  { Scaling value radiant to decimal degree }
  RAD_TO_DEG   =	57.29577951308232;
  { Scaling value decimal degree to radiant }
  DEG_TO_RAD   =	0.0174532925199432958;


{ TConvertisseurJPC }
procedure TConversionSysteme.Finaliser; // pour compatibilité
begin
  ;
end;

function TConversionSysteme.Initialiser: boolean;
var
  i: integer;
  ELL: TEllipsoide;
  DTM: TDatum;
  PRJ: TProjection;
  p: Integer;
  QNb: Integer;
  WU: String;
  MyMULT: TMultiple;
  FichierProjections: String;
begin
  Result := False;
  FichierProjections := ExtractFilePath(ParamStr(0)) + '0_ProjectionsUsuelles.txt';
  try
    AfficherMessage(format('%s.Initialiser("%s"', [ClassName, FichierProjections]));
    DecimalSeparator := '.';
    // ajouter ellipsoides par défaut
    AddEllipsoideByParameters('GRS80', 'GRS 1980', aWGS84, eWGS84);
    // ajouter datums par défaut
    AddDatumByParameters('WGS84', 'World Geodesic System', 'GRS80', 0, 0, 0, 0, 0, 0, 1, '');
    // charger depuis fichier
    LoadFromFichier(FichierProjections);
    // générer la liste des systèmes de coordonnées
    GenererListeDesSystemes;
    // lister les ellispsoides (provisoire)
    (*
    AfficherMessage(Format('-- Liste des %d ellipsoides', [GetNbEllipsoides]));
    for i := 0 to GetNbEllipsoides - 1 do
    begin
      ELL := GetEllipsoide(i);
      AfficherMessage(Format('   %d: %s (%s) - a = %.2f - e = %.15f',
        [i, ELL.Nom, ELL.Description, ELL.a, ELL.e]));
    end;
    //*)
    // Lister les progjections et datums
    //--------------------------------------------------------------------------
    // lister les datums (provisoire)
    (*
    AfficherMessage(Format('-- Liste des %d datums', [GetNbDatums]));
    for i := 0 to GetNbDatums - 1 do
    begin
      DTM := GetDatum(i);
      AfficherMessage(Format(
        '   %d: %s (%s) (%s)- %.0f, %.0f, %.0f - %.0f %.0f %.0f - %.0f',
        [i, DTM.Nom, DTM.Description, DTM.Ellipsoide.Nom,
        DTM.DeltaX, DTM.DeltaY, DTM.DeltaZ,
        DTM.RotX, DTM.RotY, DTM.RotZ, DTM.Scale]));
    end;
    // lister les projections
    AfficherMessage(Format('-- Liste des %d projections', [GetNbProjections]));
    for i:= 0 to GetNbProjections - 1 do
    begin
      PRJ := GetProjection(i);
      AfficherMessage(Format('Type proj: %s, Nom: %s, Description: %s, Datum: %s', [PRJ.Nature, PRJ.Nom, PRJ.Description, PRJ.Datum.Nom]));
      QNb := PRJ.Nb_ParamReal;
      if (QNb > 0) then
      begin
        WU := 'Parametre reels: ';
        for p := 0 to QNb - 1 do WU := WU + Format('P%d = %.12f, ', [p, PRJ.ParamReal[p]]);
        AfficherMessage(WU);
      end;
      QNb := PRJ.Nb_ParamInteger;
      if (QNb > 0) then
      begin
        WU := 'Parametre entiers: ';
        for p := 0 to QNb - 1 do WU := WU + Format('P%d = %d, ', [p, PRJ.ParamInteger[p]]);
        AfficherMessage(WU);
      end;
      QNb := PRJ.Nb_ParamAngle;
      if (QNb > 0) then
      begin
        WU := 'Parametre angles: ';
        for p := 0 to QNb - 1 do WU := WU + Format('P%d = %s, ', [p, PRJ.ParamAngle[p]]);
        AfficherMessage(WU);
      end;
      QNb := PRJ.Nb_ParamBoolean;
      if (QNb > 0) then
      begin
        WU := 'Parametre booleens: ';
        for p := 0 to QNb - 1 do WU := WU + Format('P%d = %s, ', [p, BoolToStr(PRJ.ParamBoolean[p], 'V', 'F')]);
        AfficherMessage(WU);
      end;


    end;
     // lister les projections
    AfficherMessage(Format('-- Liste des %d projections multiples', [GetNbMultiples]));
    for i:= 0 to GetNbMultiples - 1 do
    begin
      MyMULT := GetMultiple(i);
      AfficherMessage(Format('Type proj: %s, Nom: %s, Description: %s, Datum: %s', [MyMULT.Nature, MyMULT.Nom, MyMULT.Description, MyMULT.Datum.Nom]));
      QNb := MyMULT.Nb_ParamReal;
      if (QNb > 0) then
      begin
        WU := 'Parametre reels: ';
        for p := 0 to QNb - 1 do WU := WU + Format('P%d = %.12f, ', [p, MyMULT.ParamReal[p]]);
        AfficherMessage(WU);
      end;
      QNb := MyMULT.Nb_ParamInteger;
      if (QNb > 0) then
      begin
        WU := 'Parametre entiers: ';
        for p := 0 to QNb - 1 do WU := WU + Format('P%d = %d, ', [p, MyMULT.ParamInteger[p]]);
        AfficherMessage(WU);
      end;
      QNb := MyMULT.Nb_ParamAngle;
      if (QNb > 0) then
      begin
        WU := 'Parametre angles: ';
        for p := 0 to QNb - 1 do WU := WU + Format('P%d = %s, ', [p, MyMULT.ParamAngle[p]]);
        AfficherMessage(WU);
      end;
      QNb := MyMULT.Nb_ParamBoolean;
      if (QNb > 0) then
      begin
        WU := 'Parametre booleens: ';
        for p := 0 to QNb - 1 do WU := WU + Format('P%d = %s, ', [p, BoolToStr(MyMULT.ParamBoolean[p], 'V', 'F')]);
        AfficherMessage(WU);
      end;
    end;
    //*)
    Result := True;
  except
  end;
  //AfficherMessage(Format('-- Liste des %d datums', [GetNbEllipsoides]));

end;

constructor TConversionSysteme.Create;
begin
  fLesEllipsoide := TObjectList.Create;
  fLesDatum := TObjectList.Create;
  fLesProjection := TObjectList.Create;
  fLesMultiple := TObjectList.Create;
  fLesEllipsoide.Clear;
  fLesDatum.Clear;
  fLesProjection.Clear;
  fLesMultiple.Clear;
end;

destructor TConversionSysteme.Destroy;
begin
  fLesMultiple.Free;
  fLesProjection.Free;
  fLesDatum.Free;
  fLesEllipsoide.Free;
end;
// ellipsoides
function TConversionSysteme.GetNbEllipsoides: integer;
begin
  Result := fLesEllipsoide.Count;
end;

function TConversionSysteme.GetEllipsoide(const Idx: integer): TEllipsoide;
begin
  Result := fLesEllipsoide.Items[Idx] as TEllipsoide;
end;

function TConversionSysteme.GetIdxEllipsoideByName(const qNom: string): integer;
var
  i: integer;
  EWE: TEllipsoide;
begin
  Result := -1;
  for i := 0 to GetNbEllipsoides - 1 do
  begin
    EWE := GetEllipsoide(i);
    if (LowerCase(EWE.Nom) = LowerCase(qNom)) then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

procedure TConversionSysteme.AddEllipsoideByParameters(const qNom, qDescription: string;
  const a, e: double);
var
  EWE: TEllipsoide;
begin
  EWE := TEllipsoide.Create;
  EWE.SetParameters(qNom, qDescription, a, e);
  fLesEllipsoide.Add(EWE);
end;

// datums
function TConversionSysteme.GetNbDatums: integer;
begin
  Result := fLesDatum.Count;
end;

function TConversionSysteme.GetDatum(const Idx: integer): TDatum;
begin
  Result := fLesDatum.Items[Idx] as TDatum;
end;

function TConversionSysteme.GetIdxDatumByName(const qNom: string): integer;
var

  i: Integer;
  EWE: TDatum;
begin
  Result := -1;
  for i := 0 to GetNbDatums - 1 do
  begin
    EWE := GetDatum(i);
    if (LowerCase(EWE.Nom) = LowerCase(qNom)) then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

// Datum  NTF  Nouvelle  Triangulation  Francaise  Clarke80  -168  -60  320  *
procedure TConversionSysteme.AddDatumByParameters(
  const qNom, qDescription, qNomEllipsoide: string;
  const qDX, qDY, qDZ, qRotX,
  qRotY, qRotZ, qScale: double;
  const qGrilleCorrection: string);
var
  MyDatum: TDatum;
  WU: integer;
begin
  MyDatum := TDatum.Create;
  try
    MyDatum.Nom := qNom;
    MyDatum.Description := qDescription;
    // transfo 7 paramètres
    MyDatum.DeltaX := qDX;
    MyDatum.DeltaY := qDY;
    MyDatum.DeltaZ := qDZ;
    MyDatum.RotX := qRotX;
    MyDatum.RotY := qRotY;
    MyDatum.RotZ := qRotZ;
    MyDatum.Scale := qScale;
    // Ellipsoide non trouvé => on sort en libérant le TDatum créé
    WU := GetIdxEllipsoideByName(qNomEllipsoide);
    if (WU = -1) then Raise ERangeError.CreateFmt('Ellipsoide %s inconnu dans le datum %s',  [qNomEllipsoide, qNom]);

    MyDatum.Ellipsoide := GetEllipsoide(WU);
    MyDatum.Valid := True;
    fLesDatum.Add(MyDatum);
  except
    MyDatum.Free;
  end;
end;

function TConversionSysteme.GetNbProjections: integer;
begin
  Result := fLesProjection.Count;
end;

function TConversionSysteme.GetProjection(const Idx: integer): TProjection;
begin
  Result := fLesProjection.Items[Idx] as TProjection;
end;

function TConversionSysteme.GetIdxProjectionByName(const qNom: string): integer;
var
  i: Integer;
  EWE: TProjection;
begin
  Result := -1;
  for i := 0 to GetNbProjections - 1 do
  begin
    EWE := GetProjection(i);
    if (LowerCase(EWE.Nom) = LowerCase(qNom)) then
    begin
      Result := i;
      Exit;
    end;
  end;

end;

procedure TConversionSysteme.AddProjectionByParameters(const qNatureProj, qNom, qDescription, qDatum: string;
                                                      const qParametres: TStringArray);
var
  MyProj: TProjection;
  WU: Integer;
  Compteur: Integer;
  i: Integer;
  EWE: String;
  q: Integer;
begin
  try
    (*
    AfficherMessage(Format('Type proj: %s, Nom: %s, Description: %s, Datum: %s', [qNatureProj, qNom, qDescription, qDatum]));
    EWE := '';
    for q := 0 to 10 do EWE := EWE + Format('P[%d] = "%s"; ', [q, qParametres[q]]);
    AfficherMessage(EWE);
    //*)
    MyProj := CreateProjection(qNatureProj);
    //                  0                       0       1               2       3       4       5
    //Projection	Lambert_Conique_Secant=	LT93	Lambert-93	WGS84	700000	6600000	3	46.50	44	49
    MyProj.Nom          := qNom;
    MyProj.Description  := qDescription;
    WU := GetIdxDatumByName(qDatum);

    if (WU = -1) then Raise ERangeError.CreateFmt('Datum %s inconnu dans la projection %s',  [qDatum, qNom]);
    MyProj.Datum := GetDatum(WU);
    // décodage des paramètres
    Compteur := 0;
    AfficherMessage(Format('%d reels', [MyProj.Nb_ParamReal]));
    for i := 0 to MyProj.Nb_ParamReal - 1    do MyProj.ParamReal[i]    := StrToFloatDef(qParametres[i], 0.00);
    Inc(Compteur, MyProj.Nb_ParamReal);
    AfficherMessage(Format('%d entiers', [MyProj.Nb_ParamInteger]));
    for I := 0 to MyProj.Nb_ParamInteger - 1 do MyProj.ParamInteger[I] := StrToInt(qParametres[I + Compteur]);
    Inc(Compteur, MyProj.Nb_ParamInteger);
    AfficherMessage(Format('%d angles', [MyProj.Nb_ParamAngle]));
    for I := 0 to MyProj.Nb_ParamAngle - 1   do MyProj.ParamAngle[I]   := Trim(qParametres[I + Compteur])+'d';
    Inc(Compteur, MyProj.Nb_ParamAngle);
    AfficherMessage(Format('%d booleens', [MyProj.Nb_ParamBoolean]));
    for I := 0 to MyProj.Nb_ParamBoolean - 1 do MyProj.ParamBoolean[I] := (qParametres[I + Compteur] = '1');
    MyProj.Valid := True;
    fLesProjection.Add(MyProj);
  except
    AfficherMessage('Projection invalide');
    MyProj.Free;
  end;
end;

// Multiples
function TConversionSysteme.GetNbMultiples: integer;
begin
  Result := fLesMultiple.Count;
end;

function TConversionSysteme.GetMultiple(const Idx: integer): TMultiple;
begin
  Result := fLesMultiple.Items[Idx] as TMultiple;
end;

function TConversionSysteme.GetIdxMultipleByName(const qNom: string): integer;
var
  i: Integer;
  EWE: TMultiple;
begin
  Result := -1;
  for i := 0 to GetNbMultiples - 1 do
  begin
    EWE := GetMultiple(i);
    if (LowerCase(EWE.Nom) = LowerCase(qNom)) then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

procedure TConversionSysteme.AddMultipleByParameters(const qNatureMultiple,
  qNom, qDescription, qDatum: string; const qParametres: TStringArray);
var
  MyMultiple: TMultiple;
  WU: Integer;
  Compteur: Integer;
  i: Integer;
begin
  try
    (*
    AfficherMessage(Format('Type proj: %s, Nom: %s, Description: %s, Datum: %s', [qNatureProj, qNom, qDescription, qDatum]));
    EWE := '';
    for q := 0 to 10 do EWE := EWE + Format('P[%d] = "%s"; ', [q, qParametres[q]]);
    AfficherMessage(EWE);
    //*)
    MyMultiple := CreateMultiple(qNatureMultiple);
    //                  0                       0       1               2       3       4       5
    //Projection	Lambert_Conique_Secant=	LT93	Lambert-93	WGS84	700000	6600000	3	46.50	44	49
    MyMultiple.Nom          := qNom;
    MyMultiple.Description  := qDescription;
    WU := GetIdxDatumByName(qDatum);

    if (WU = -1) then Raise ERangeError.CreateFmt('Datum %s inconnu dans la projection %s',  [qDatum, qNom]);
    MyMultiple.Datum := GetDatum(WU);
    // décodage des paramètres
    Compteur := 0;
    AfficherMessage(Format('%d reels', [MyMultiple.Nb_ParamReal]));
    for i := 0 to MyMultiple.Nb_ParamReal - 1    do MyMultiple.ParamReal[i]    := StrToFloatDef(qParametres[i], 0.00);
    Inc(Compteur, MyMultiple.Nb_ParamReal);
    AfficherMessage(Format('%d entiers', [MyMultiple.Nb_ParamInteger]));
    for I := 0 to MyMultiple.Nb_ParamInteger - 1 do MyMultiple.ParamInteger[I] := StrToInt(qParametres[I + Compteur]);
    Inc(Compteur, MyMultiple.Nb_ParamInteger);
    AfficherMessage(Format('%d angles', [MyMultiple.Nb_ParamAngle]));
    for I := 0 to MyMultiple.Nb_ParamAngle - 1   do MyMultiple.ParamAngle[I]   := Trim(qParametres[I + Compteur])+'d';
    Inc(Compteur, MyMultiple.Nb_ParamAngle);
    AfficherMessage(Format('%d booleens', [MyMultiple.Nb_ParamBoolean]));
    for I := 0 to MyMultiple.Nb_ParamBoolean - 1 do MyMultiple.ParamBoolean[I] := (qParametres[I + Compteur] = '1');
    MyMultiple.Valid := True;
    fLesMultiple.Add(MyMultiple);
  except
    AfficherMessage('Projection invalide');
    MyMultiple.Free;
  end;

end;




//------------------------------------------------------------------------------
procedure TConversionSysteme.LoadFromFichier(const Filename: string);
var
  LS: TStringList;
  EWE, WU: TStringArray;
  i, n: integer;
begin
  AfficherMessage('-- Charger depuis : ' + Filename);
  LS := TStringList.Create;
  try
    LS.Clear;
    LS.LoadFromFile(Filename);
    if (LS.Count = 0) then
      Exit;
    for i := 0 to LS.Count - 1 do
    begin
      EWE := Split(Trim(LS.Strings[i]), #9);
      case IndexOfString(EWE[0], True, ['ellipsoide', 'datum',
          'projection', 'multiple']) of
        0:
        begin
          // # ELLIPSOIDES
          //Nom  Description  a  e
          //Ellipsoide  Clarke80  Clarke_1880  6378249.2  0.0824832568
          AddEllipsoideByParameters(EWE[1],
            EWE[2],
            StrToFloatDef(EWE[3], -1.00),
            StrToFloatDef(EWE[4], -1.00));
        end;
        1:
        begin
          //# DATUMS
          //#   Nom  Description  Ellipsoide  TX  TY  TZ  RX  RY  RZ  Scale  Grille de correction
          //Datum  NTF  Nouvelle Triangulation Francaise  Clarke80  -168  -60  320  0  0  0  1.00  *
          AddDatumByParameters(EWE[1], EWE[2], EWE[3],
            StrToFloatDef(EWE[4], 0.00),
            StrToFloatDef(EWE[5], 0.00),
            StrToFloatDef(EWE[6], 0.00),
            StrToFloatDef(EWE[7], 0.00),
            StrToFloatDef(EWE[8], 0.00),
            StrToFloatDef(EWE[9], 0.00),
            StrToFloatDef(EWE[10], 1.00),
            EWE[11]);
        end;
        2:
        begin

          //                                    0       1               2       3       4       5       6       7       8
          //0           1                       2       3               4       5       6       7
          //Projection	Lambert_Conique_Secant=	LT93	Lambert-93	WGS84	700000	6600000	3	46.50	44	49
          for n := 0 to high(WU) do WU[n] := '';
          for n := 5 to High(EWE) do WU[n-5] := EWE[n];


          AddProjectionByParameters(EWE[1], EWE[2], EWE[3], EWE[4], WU);

        end;
        3:

        begin
          for n := 0 to high(WU) do WU[n] := '';
          for n := 5 to High(EWE) do WU[n-5] := EWE[n];


          AddMultipleByParameters(EWE[1], EWE[2], EWE[3], EWE[4], WU);

        end;
        else
        begin // ignorer
          ;
        end;

      end;
    end;
  finally
    LS.Free;
  end;
end;



//******************************************************************************
function TConversionSysteme.DepuisGeocentrique(XGeo, YGeo, ZGeo : real; NomDest : string;
                        var IndiceDest : integer; var XDest, YDest : string;
                                       UniteAngle : TUniteAngle) : boolean;
// réalise une conversion depuis les coordonnées géocentriques (en WGS84) vers le système Dest (paramètres chaines)
// NomDest est l'identifiant du système destination
// X et Y sont les coordonnées
// s'il s'agit d'une projection, ce sont des mètres
// IndiceDestination indique le numéro de fuseau
// s'il s'agit d'une projection, ce sont des mètres
// IndiceDestination est indéfini et inutilisé
// s'il s'agit d'un Datum, X indique la Latitude et Y la Longitude
// IndiceSource=0 pour une origine à Greenwich, =1 pour Paris
// UniteAngle indique l'unité d'angle à utiliser pour la destination
var
  I : integer;
  Lat, Long : real;
begin
  result:=DepuisGeocentriqueNum(XGeo, YGeo, ZGeo, NomDest, IndiceDest, Lat, Long);
  I:= GetIdxDatumByName(NomDest);
  if I<>-1 then
  begin // c'est un datum
    XDest:=AngleToStr(Lat,UniteAngle);
    YDest:=AngleToStr(Long,UniteAngle);
  end
  else
  begin // c'est une projection (simple ou multiple)
    XDest:=Format('%.3f',[Lat]);
    YDest:=Format('%.3f',[Long]);
  end;
end;

function TConversionSysteme.VersGeocentriqueNum(NomSource : string; XSource, YSource : real;
            IndiceSource : integer; var XGeo, YGeo, ZGeo : real) : boolean;
// même chose que TDonnees.VersGeocentrique mais travail avec des nombre au lieu de chaînes
// réalise une conversion depuis le système Source vers les coordonnées géocentriques (en WGS84) (paramètres numériques)
// NomSource est l'identifiants du système source
// X et Y sont les coordonnées
// s'il s'agit d'une projection, ce sont des mètres
// IndiceSource indique le numéro de fuseau
// s'il s'agit d'une projection, ce sont des mètres
// IndiceSource est inutilisé
// s'il s'agit d'un Datum, X indique la Latitude et Y la Longitude
// IndiceSource=0 pour une origine à Greenwich, =1 pour Paris
var
  Idx : integer;
  Echec : boolean;
  Lat, Long : real;
  MyDatum      : TDatum;
  MyProjection : TProjection;
  MyMultiple   : TMultiple;
begin
  Echec :=true;
  Idx   :=GetIdxDatumByName(NomSource);
  if (Idx <> -1) then
  begin // c'est un datum
    MyDatum := GetDatum(Idx);
    MyDatum.VersCartesien(XSource, YSource, (IndiceSource=1), XGeo,YGeo, ZGeo);
    Echec:=false;
  end
  else
  begin // c'est une projection (simple ou mutiple)
    Idx := GetIdxProjectionByName(NomSource);
    if (Idx <> -1) then
    begin // c'est une projection simple
      MyProjection := GetProjection(Idx);
      MyProjection.VersLatLong(XSource, YSource, Lat, Long);
      MyProjection.Datum.VersCartesien(Lat, Long, false, XGeo, YGeo, ZGeo);
      Echec:=false;
    end
    else
    begin // ça devrait être une projection multiple
      Idx := GetIdxMultipleByName(NomSource);
      if (Idx <> -1) then
      begin
        MyMultiple := GetMultiple(Idx);
        MyMultiple.VersLatLong(XSource, YSource, IndiceSource, Lat, Long);
        MyMultiple.Datum.VersCartesien(Lat, Long, false, XGeo, YGeo, ZGeo);
        Echec:=false;
      end;
    end;
  end;
  result:=not Echec;
end;


function TConversionSysteme.DepuisGeocentriqueNum(XGeo, YGeo, ZGeo : real; NomDest : string;
                        var IndiceDest : integer; var XDest, YDest : real) : boolean;
// même chose que TDonnees.DepuisGeocentrique mais travail avec des nombre au lieu de chaînes
// réalise une conversion depuis les coordonnées géocentriques (en WGS84) vers le système Dest (paramètres numériques)
// NomDest est l'identifiant du système destination
// X et Y sont les coordonnées
// s'il s'agit d'une projection, ce sont des mètres
// IndiceDestination indique le numéro de fuseau
// s'il s'agit d'une projection, ce sont des mètres
// IndiceDestination est indéfini et inutilisé
// s'il s'agit d'un Datum, X indique la Latitude et Y la Longitude
// IndiceSource=0 pour une origine à Greenwich, =1 pour Paris
// UniteAngle indique l'unité d'angle à utiliser pour la destination
var
  Idx : integer;
  Echec : boolean;
  Lat, Long : real;
  MyDatum      : TDatum;
  MyProjection : TProjection;
  MyMultiple   : TMultiple;
begin
  Echec:=true;
  Idx := GetIdxDatumByName(NomDest);
  if (Idx <> -1) then
  begin // c'est un datum
    MyDatum := GetDatum(Idx);
    MyDatum.VersLatLong(XGeo, YGeo, ZGeo, (IndiceDest=1), XDest, YDest);
    Echec:=false;
  end
  else
  begin // c'est une projection (simple ou mutiple)
    Idx := GetIdxProjectionByName(NomDest);
    if (Idx <> -1) then
    begin // c'est une projection simple
      MyProjection := GetProjection(Idx);
      MyProjection.Datum.VersLatLong(XGeo, YGeo, ZGeo, false, Lat, Long);
      MyProjection.VersGrille(Lat, Long, XDest, YDest);
      Echec:=false;
    end
    else
    begin // ça devrait être une projection multiple
      Idx := GetIdxMultipleByName(NomDest);
      if (Idx <> -1) then
      begin
        MyMultiple := GetMultiple(Idx);
        MyMultiple.Datum.VersLatLong(XGeo, YGeo, ZGeo, false, Lat, Long);
        MyMultiple.VersGrille(Lat, Long, XDest, YDest, IndiceDest);
        Echec:=false;
      end;
    end;
  end;
  result:=not Echec;
end;


function TConversionSysteme.Conversion(NomSource, NomDest, XSource, YSource : string;
                     IndiceSource : integer; var IndiceDest : integer;
                     var XDest, YDest : string; UniteAngle : TUniteAngle) : boolean;
// réalise une conversion entre les systèmes Source et Dest
// NomSource et NomDest sont les Nom identifiants les systèmes source et destination
// X et Y sont les coordonnées
// s'il s'agit d'une projection, ce sont des mètres
// IndiceSource et IndiceDestination indiquent les numéros de fuseau
// s'il s'agit d'une projection, ce sont des mètres
// IndiceSource et IndiceDestination sont indéfinis et inutilisés
// s'il s'agit d'un Datum, X indique la Latitude et Y la Longitude
// IndiceSource=0 pour une origine à Greenwich, =1 pour Paris
// UniteAngle indique l'unité d'angle à utiliser pour la destination
var
  Xcart, Ycart, Zcart : real;
begin
  result:= VersGeocentrique(NomSource, XSource, YSource, IndiceSource, Xcart, Ycart, Zcart) and
           DepuisGeocentrique(Xcart, Ycart, Zcart, NomDest, IndiceDest, XDest, YDest, UniteAngle);
end;

function TConversionSysteme.ConversionNum(NomSource, NomDest : string; XSource, YSource : real;
                     IndiceSource : integer; var IndiceDest : integer;
                     var XDest, YDest : real) : boolean;
// même chose que TDonnees.Conversion mais travail avec des nombre au lieu de chaînes
// NomSource et NomDest sont les Nom identifiants les systèmes source et destination
// X et Y sont les coordonnées
// s'il s'agit d'une projection, ce sont des mètres
// s'il s'agit d'un datum, ce sont des radians
// IndiceSource et IndiceDestination indiquent les numéros de fuseau
// s'il s'agit d'une projection, ce sont des mètres
// IndiceSource et IndiceDestination sont indéfinis et inutilisés
// s'il s'agit d'un Datum, X indique la Latitude et Y la Longitude (en radian)
// IndiceSource=0 pour une origine à Greenwich, =1 pour Paris
// UniteAngle indique l'unité d'angle à utiliser pour la destination
var
  Xcart, Ycart, Zcart : real;
begin
  result:=VersGeocentriqueNum(NomSource, XSource, YSource, IndiceSource, Xcart, Ycart, Zcart) and
     DepuisGeocentriqueNum(Xcart, Ycart, Zcart, NomDest, IndiceDest, XDest, YDest);
end;

// réalise une conversion depuis le système Source vers les coordonnées géocentriques (en WGS84) (paramètres chaines)
function TConversionSysteme.VersGeocentrique(NomSource, XSource, YSource : string; IndiceSource : integer; var XGeo, YGeo, ZGeo : real) : boolean;
// réalise une conversion depuis le système Source vers les coordonnées géocentriques (en WGS84) (paramètres chaines)
// NomSource est l'identifiants du système source
// X et Y sont les coordonnées
// s'il s'agit d'une projection, ce sont des mètres
// IndiceSource indique le numéro de fuseau
// s'il s'agit d'une projection, ce sont des mètres
// IndiceSource est inutilisé
// s'il s'agit d'un Datum, X indique la Latitude et Y la Longitude
// IndiceSource=0 pour une origine à Greenwich, =1 pour Paris
var
  Idx : integer;
  Lat, Long : real;
begin
  Idx := GetIdxDatumByName(NomSource);
  if Idx<>-1 then
  begin // c'est un datum, i.e. XSource et YSource sont des angles
    try
      Lat:=StrToAngle(XSource);
    except
      raise EConvertError.Create('Latitude de départ incorrecte, exemple 2d24''17.036''''');
    end;
    try
      Long:=StrToAngle(YSource);
    except
      raise EConvertError.Create('Longitude de départ incorrecte, exemple 2d24''17.036''''');
    end;
  end
  else
  begin // c'est une projection i.e. XSource et YSource sont des mètres
    try
      Lat := StrToFloat(XSource);
    except
      raise EConvertError.Create('Easting de départ incorrecte, exemple 231500');
    end;
    try
      Long := StrToFloat(YSource);
    except
      raise EConvertError.Create('Northing de départ incorrecte, exemple 231500');
    end;
  end;
  result:=VersGeocentriqueNum(NomSource, Lat, Long, IndiceSource, XGeo, YGeo, ZGeo);
end;
//******************************************************************************
function TConversionSysteme.GenererListeDesSystemes: boolean;
var
  LS: TStringList;
  i: Integer;
begin
  AfficherMessage(Format('%s.GenererListeDesSystemes', [ClassName]));
  result := False;
  SetLength(FListeDesSystemes, 0);
  LS := TStringList.Create;
  try
    LS.Clear;
    // on ne charge pas les ellipsoides
    // ici, on ne crée pas de variables intermédiaires pour récupérer un simple nom
    for i:= 0 to GetNbDatums - 1      do LS.Add(GetDatum(i).Nom);
    for i:= 0 to GetNbProjections - 1 do LS.Add(GetProjection(i).Nom);
    for i:= 0 to GetNbMultiples - 1   do LS.Add(GetMultiple(i).Nom);
    // tableau des systèmes
    FNbreSystemes := LS.Count;
    SetLength(FListeDesSystemes, FNbreSystemes);
    for i:= 0 to FNbreSystemes - 1 do FListeDesSystemes[i] := LS.Strings[i];
    // vidage
    LS.Clear;
    //--------------
    Result := True;
  finally
    LS.Free;
  end;
end;

function TConversionSysteme.GetNbSystemes: integer;
begin
  Result := FNbreSystemes;
end;

function TConversionSysteme.GetNomSysteme(const Idx: integer): string;
begin
  Result := FListeDesSystemes[Idx];
end;
// le convertisseur lui-même lol
function TConversionSysteme.ConversionSyst1ToSyst2(const SystSrc, SystCible: string;
                                                  const MyPoint: TProjUV): TProjUV;
var
  IndiceSource, IndiceDest : integer;
  EWE : TProjUV;
begin
  //AfficherMessage(Format('Conversion: %s->%s - %.2f, %.2f', [SystSrc, SystCible, MyPoint.U, MyPoint.V]));
  EWE.U := MyPoint.U;
  EWE.V := MyPoint.V;

  IndiceSource := 0;
  IndiceDest   := 0;
  // passage en radians si on est en WGS84
  if (SystSrc = 'WGS84') then
  begin
    EWE.U := MyPoint.U * DEG_TO_RAD;
    EWE.V := MyPoint.V * DEG_TO_RAD;
  end;
  ConversionNum(Trim(SystSrc), Trim(SystCible),
                        EWE.U, EWE.V,
                        IndiceSource, IndiceDest,
                        Result.U, Result.V);
  // passage en degrés
  if (SystCible = 'WGS84') then
  begin
    Result.U := RAD_TO_DEG * Result.U;
    Result.V := RAD_TO_DEG * Result.V;
  end;

  //AfficherMessage(Format('--> %.2f, %.2f', [Result.U, Result.V]));
end;

end.
