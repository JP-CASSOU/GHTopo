unit ConversionsCoordonneesESibert;
// Date: 27/05/2013
// Wrapper pour les conversions de coordonnées
// utilisant le source de E. Sibert

// Paramétrage de UTM dans 00_donnees.txt
// Multiple "Transverse Mercator Multiple"="UTM29" "Universal Transverse Mercator" "WGS84" 0.9996 500000 1 0 -3° 6°
// Multiple "Transverse Mercator Multiple"="UTM30" "Universal Transverse Mercator" "WGS84" 0.9996 500000 1 0  3° 6°
// Multiple "Transverse Mercator Multiple"="UTM31" "Universal Transverse Mercator" "WGS84" 0.9996 500000 1 0  9° 6°
// Multiple "Transverse Mercator Multiple"="UTM32" "Universal Transverse Mercator" "WGS84" 0.9996 500000 1 0  15° 6°

{$INCLUDE CompilationParameters.inc}

interface

uses
  StructuresDonnees,
  Classes, SysUtils,
  //ellipsoide,
  donnee,
  outils,
  datum;


type  // TConversionSysteme est une classe TList car elle contient un tableau de grilles.
  TConversionSysteme = class(TList)
      function  Initialiser: boolean;
      procedure Finaliser;
      function GetGeoGrid(const Idx: integer): TParametresEPSG;
      function GetGeoGridByCodeEPSG(const QCodeEPSG: integer): TParametresEPSG;
      function GetIndexByCodeEPSG(const QCodeEPSG: integer): integer;
      function GetNbreGrilles: integer;

    private

      //procedure ViderLesGrilles;
      //function  DemarrerDLL: boolean;          FDonnees
      //procedure StopperDLL;
      //function PreparerLesGrilles: integer;
      //FDonnees : TDonnees;
      function GetTDonneesDatum(const Idx: integer): TDatum;

    public
      // attraper le nombre de projections
      function GetNbSystemes: integer;
      // attraper une projection donnée
      function GetNomSysteme(const Idx: integer): string;
      // conversions d'un système à l'autre
      function Conversion_EPSG_To_EPSG(const Src, Tgt: integer; const MyPoint: TProjUV): TProjUV;
      // conversion d'un système à l'autres (version ESibert)
      function ConversionSyst1ToSyst2(const SystSrc, SystCible: string; const MyPoint: TProjUV): TProjUV;
  end;

implementation
uses
  Forms, Common; // pour AfficherMessage
const
  { Scaling value radiant to decimal degree }
  RAD_TO_DEG   =	57.29577951308232;
  { Scaling value decimal degree to radiant }
  DEG_TO_RAD   =	0.0174532925199432958;


function TConversionSysteme.Initialiser: boolean;
var
  Chemin: String;
  Flux: TStream;
  i: Integer;
  QDatum: TDatum;
  ListeProposition : TStringList;
  Separations: integer;
  EWE: String;
  MyEllip: TObject;
  MyDatum: TObject;
begin
  AfficherMessage(format('%s.Initialiser', [ClassName]));
  AfficherMessage('==============================');
  Result := False;
  defaultFormatSettings.DecimalSeparator := '.';
  Donnees := TDonnees.Create;
  Donnees.NomJeuDonnees := 'JeuDeDonnees001';

  try
    Chemin := ExtractFilePath(ParamStr(0)) +'00_donnees.txt';
    AfficherMessage('-- Premier essai: ' + Chemin);
    Flux:=TFileStream.Create(Chemin,fmOpenRead);
    try
      Donnees.LoadFromStream(Flux);
    finally
      Flux.Free;
    end;
  except // si on n'a pas réussi à charger donnees.txt,
         // on essaie de charger france.txt
    try
      Chemin:= ExtractFilePath(ParamStr(0)) +'00_france.txt';
      AfficherMessage('-- Deuxième essai: ' + Chemin);
      Flux:=TFileStream.Create(Chemin,fmOpenRead);
      try
        Donnees.LoadFromStream(Flux);
      finally
        Flux.Free;
      end;
    except
    end;
  end;
  //*)
  {
  // version avec TDonnees.LoadFromFile
  //Chemin := ExtractFilePath(ParamStr(0)) +'00_donnees.txt';
  //if (Not Donnees.LoadFromFile(Chemin)) then Exit;
  //----------------------------------------------------------------------------
  // on crée notre propre jeu de données !
  // ellipsoides
  Donnees.AddEllipsoideWithParams('Clarke80'    , 'Clarke 1880'  , 6378249.2, 0.0824832568, True);
  Donnees.AddEllipsoideWithParams('Hayford 09'  , 'Hayford 1909' , 6378388.0, 0.08199189, True);
  Donnees.AddEllipsoideWithParams('Plessis'     , 'Plessis 1840' , 6376523.0, 0.0804334508, True);
  AfficherMessage('Ellipsoides:');
  for i:=0 to Donnees.GetNbEllipsoide - 1 do
  begin
    MyEllip := Donnees.GetEllipsoide(i);
    AfficherMessage(Format('Ellipsoide %i: %s - (%s) - a = %.2f e = %.12f', [
                           TEllipsoide(MyEllip).Nom,
                           TEllipsoide(MyEllip).Description,
                           TEllipsoide(MyEllip).a,
                           TEllipsoide(MyEllip).e
                            ]));
  end;

  EWE := ExtractFilePath(ParamStr(0)) + 'gr3df97a.txt';
  AfficherMessage(ewe);
  //Datum="NTF" "" "" -168 -60 320 "Donnees_Geodesiques\gr3df97a.txt"
  Donnees.AddDatumWithParams('NTF', 'Nouvelle Triangulation Francaise', 'Clarke80',
                             -168, -60, 320,
                             0.0, 0.0, 0.0, 0.0,
                             True,
                             EWE);
  Donnees.AddDatumWithParams('ED50', 'European Datum', 'Hayford 09',
                             -84, -97, -117,
                             0.0, 0.0, 0.0, 0.0,
                             True,
                             EWE);
  Donnees.AddDatumWithParams('ATG', 'Ancienne triangulation francaise', 'Plessis',
                             1118, 23, 66,
                             0.0, 0.0, 0.0, 0.0,
                             True,
                             EWE);
  AfficherMessage('---------------');
  AfficherMessage(Format('Datums: %d', [Donnees.GetNbDatum ]));
  for i:= 0 to Donnees.GetNbDatum - 1 do
  begin
    AfficherMessage('Datum : '+ inttostr(i));

    MyDatum := Donnees.GetDatum(i);
    AfficherMessage(Format('Datum %i: %s - (%s) - %s - dx = %.2f dy = %.2f dz = %.2f - Grid: %s', [
                           TDatum(MyDatum).Nom,
                           TDatum(MyDatum).Description,
                           TDatum(MyDatum).Ellipsoide,
                           TDatum(MyDatum).DeltaX,
                           TDatum(MyDatum).DeltaY,
                           TDatum(MyDatum).DeltaZ,
                           TDatum(MyDatum).FileGrille
                            ]));
    //*)
  end;

  Exit;
  //*}
  // lister les systèmes (contrôle)
  if (Donnees.ListerLesSystemes <> -1) then
  begin
    AfficherMessage(Format('%d systemes', [Donnees.GetNbDeSystemes]));
    AfficherMessage('==============================');
    for i:=0 to Donnees.GetNbDeSystemes - 1 do
    begin
      EWE := Donnees.GetNomDeProjection(i);
      AfficherMessage(Format('%d: %s', [i, EWE]));
    end;
    AfficherMessage('==============================');
    Result := True;
  end;
end;

procedure TConversionSysteme.Finaliser;
begin
  try

  finally
    Donnees.Free;
  end;
end;

function TConversionSysteme.GetGeoGrid(const Idx: integer): TParametresEPSG;
begin

end;

function TConversionSysteme.GetGeoGridByCodeEPSG(const QCodeEPSG: integer
  ): TParametresEPSG;
begin
  //Result := 0;
end;

function TConversionSysteme.GetIndexByCodeEPSG(const QCodeEPSG: integer
  ): integer;
begin
  Result := 0;
end;

function TConversionSysteme.GetNbreGrilles: integer;
begin
  Result := 0;
end;

function TConversionSysteme.GetTDonneesDatum(const Idx: integer): TDatum;
begin
  Result := (Donnees.LesDatum[Idx] as TDatum);
end;

function TConversionSysteme.GetNbSystemes: integer;
begin
  Result := Donnees.GetNbDeSystemes;
end;

function TConversionSysteme.GetNomSysteme(const Idx: integer): string;
begin
  Result := Donnees.GetNomDeProjection(Idx);
end;

function TConversionSysteme.Conversion_EPSG_To_EPSG(const Src, Tgt: integer;
  const MyPoint: TProjUV): TProjUV;

var
  SrcStr: String;
  TgtStr: String;
  function GetNameESibertSyst(const EPSGCode: integer): string;
  var
    WU: Integer;
  begin
    case EPSGCode of
      4326 : Result := 'WGS84';
      2154 : Result := 'LT 93';
      27571: Result := 'LT 1';
      27572: Result := 'LT 2';
      27573: Result := 'LT 3';
      27574: Result := 'LT 4';
      3942 .. 3950:
        begin
          WU := EPSGCode mod 100;
          Result := Format('CC%d', [WU]);
        end;
    else
      Result := '';
    end;
  end;

begin

  SrcStr := GetNameESibertSyst(Src);
  TgtStr := GetNameESibertSyst(Tgt);
  AfficherMessage(Format('%d->%d: %s->%s: %.2f, %.2f', [Src, Tgt, SrcStr, TgtStr, MyPoint.U, MyPoint.V]));
  Result := ConversionSyst1ToSyst2(SrcStr, TgtStr, MyPoint);

end;

function TConversionSysteme.ConversionSyst1ToSyst2(const SystSrc, SystCible: string;
                                                   const MyPoint: TProjUV): TProjUV;
var
  IndiceSource, IndiceDest : integer;
  EWE : TProjUV;
begin
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
  Donnees.ConversionNum(Trim(SystSrc), Trim(SystCible),
                        EWE.U, EWE.V,
                        IndiceSource, IndiceDest,
                        Result.U, Result.V);
  // passage en degrés
  if (SystCible = 'WGS84') then
  begin
    Result.U := RAD_TO_DEG * Result.U;
    Result.V := RAD_TO_DEG * Result.V;
  end;
end;

end.

