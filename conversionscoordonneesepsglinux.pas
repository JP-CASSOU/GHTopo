unit ConversionsCoordonneesEPSGLinux;
// Date: 10/05/2013
// Wrapper pour les conversions de coordonnées
// Version Linux

{$INCLUDE CompilationParameters.inc}


interface

uses
  StructuresDonnees,
  Classes, SysUtils;
const
  { Scaling value radiant to decimal degree }
  RAD_TO_DEG   =	57.29577951308232;
  { Scaling value decimal degree to radiant }
  DEG_TO_RAD   =	0.0174532925199432958;

Type
 { Point record and analoga }
 TProjUV = Record U,V:Double End;
 { Point record and analoga }
 TProjXY = TProjUV;
 { Point record and analoga }
 TProjLP = TProjUV;
 { Pointer to a projection }
 PProjPJ = Pointer;
 { Pointer to a double array like double *mat; in C}
 PDoubleArray  = ^TDoubleArray;
 { Array like double *vec; in C}
 TDoubleArray  = Array [0..0] Of Double;
 { Pointer to a character array like char *chr; in C}
 PCharArray    = Array Of Pointer;
 { Array like char *chr; in C}
 TCharArray    = Array [0..0] Of PChar;
 { 32 bit word }
 TLong         = LongWord;



type

{ TConversionSysteme }
// TConversionSysteme est une classe TList car elle contient un tableau de grilles.
TConversionSysteme = class(TList)
    function  Initialiser: boolean;
    procedure Finaliser;

  private
    FpjLT93        :PProjPJ;
    procedure ViderLesGrilles;
    function  DemarrerDLL: boolean;
    procedure StopperDLL;
    function PreparerLesGrilles: integer;

  public
    // gestion de la liste des grilles
    procedure AddGeoGrid(const G: TParametresEPSG);
    function GetGeoGrid(const Idx: integer): TParametresEPSG;
    function GetGeoGridByCodeEPSG(const QCodeEPSG: integer): TParametresEPSG;
    function GetIndexByCodeEPSG(const QCodeEPSG: integer): integer;
    function GetNbreGrilles: integer;

    // conversions d'un système à l'autre
    function Conversion_EPSG_To_EPSG(const Src, Tgt: integer; const MyPoint: TProjUV): TProjUV;
end;

implementation
uses
  Common; // pour AfficherMessages

// paramètres des projections usuelles (France métropolitaine)
const
  // Lambert93, projection par défaut
  PARAMS_EPSG_2154 = '+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs';

  function SetLongLat(const U, V:Double): TProjUV;
Begin
  Result.u:= DEG_TO_RAD * u;
  Result.v:= DEG_TO_RAD * v;
End;

(* TConversionSysteme *)


function TConversionSysteme.Initialiser: Boolean;
begin
  AfficherMessage(Format('%s.Initialiser',[ClassName]));
  Result := False;
  try

    //----------------------------
    // préparer les grilles
    //PreparerLesGrilles();

    Result := True;
  except

  end;
end;

procedure TConversionSysteme.Finaliser;
begin
  ViderLesGrilles;
end;

procedure TConversionSysteme.ViderLesGrilles;
var
  ii: integer;
  EWE: TParametresEPSG;
begin
  AfficherMessage(Format('%s.ViderLesGrilles',[ClassName]));
  try
    if (self.Count > 0) then
    begin
      for ii := 0 to self.Count - 1  do
      begin
        try
          EWE := self.GetGeoGrid(ii);

        except
        end;
      end;
      for ii := 0 to self.Count - 1 do Dispose(self.Items[ii]);
    end;


  finally
    self.Clear;
  end;
end;

function TConversionSysteme.DemarrerDLL: boolean;
var
  cnt: Integer;
  S: String;
begin
  Result := False;
  try

  except
  end;
end;

procedure TConversionSysteme.StopperDLL;
begin
  ;
end;

function TConversionSysteme.PreparerLesGrilles: integer;
var
  EWE: TParametresEPSG;
  ii: Integer;
begin
  Result := -1;
  try
    ViderLesGrilles();
    // les grilles usuelles
    EWE.CodeEPSG   := 4807;
    EWE.Parameters := '+proj=longlat +a=6378249.2 +b=6356515 +towgs84=-168,-60,320,0,0,0,0 +pm=paris +no_defs  no_defs';
    EWE.Comments   := 'NTF';
//    EWE.ProjPointer:=  _pj_init_plus_path(PChar(EWE.Parameters), '.\\nad');
    AddGeoGrid(EWE);

    EWE.CodeEPSG   := 4326;
    EWE.Parameters := '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs';
    EWE.Comments   := 'WGS84';
//    EWE.ProjPointer:=  _pj_init_plus_path(PChar(EWE.Parameters), '.\\nad');
    AddGeoGrid(EWE);

    EWE.CodeEPSG   := 2154;
    EWE.Parameters := '+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs';
    EWE.Comments   := 'RGF93 / Lambert-93';
//    EWE.ProjPointer:=  _pj_init_plus_path(PChar(EWE.Parameters), '.\\nad');
    AddGeoGrid(EWE);

    EWE.CodeEPSG   := 27561;
    EWE.Parameters := '+proj=lcc +lat_1=49.50000000000001 +lat_0=49.50000000000001 +lon_0=2.33722917 +k_0=0.999877341 +x_0=600000 +y_0=200000 +a=6378249.2 +b=6356515 +towgs84=-168,-60,320,0,0,0,0 +pm=paris +units=m +no_defs';
    EWE.Comments   := 'NTF (Paris) / Lambert Nord France';
//    EWE.ProjPointer:=  _pj_init_plus_path(PChar(EWE.Parameters), '.\\nad');
    AddGeoGrid(EWE);

    EWE.CodeEPSG   := 27562;
    EWE.Parameters := '+proj=lcc +lat_1=46.8 +lat_0=46.8 +lon_0=2.33722917 +k_0=0.99987742 +x_0=600000 +y_0=200000 +a=6378249.2 +b=6356515 +towgs84=-168,-60,320,0,0,0,0 +pm=paris +units=m +no_defs';
    EWE.Comments   := 'NTF (Paris) / Lambert Centre France';

//    EWE.ProjPointer:=  _pj_init_plus_path(PChar(EWE.Parameters), '.\\nad');
    AddGeoGrid(EWE);

    EWE.CodeEPSG   := 27563;
    EWE.Parameters := '+proj=lcc +lat_1=44.10000000000001 +lat_0=44.10000000000001 +lon_0=2.33722917 +k_0=0.999877499 +x_0=600000 +y_0=200000 +a=6378249.2 +b=6356515 +towgs84=-168,-60,320,0,0,0,0 +pm=paris +units=m +no_defs';
    EWE.Comments   := 'NTF (Paris) / Lambert Sud France';
//    EWE.ProjPointer:= _pj_init_plus_path(PChar(EWE.Parameters), '.\\nad');
    AddGeoGrid(EWE);

    EWE.CodeEPSG   := 27564;
    EWE.Parameters := '+proj=lcc +lat_1=42.16500000000001 +lat_0=42.16500000000001 +lon_0=2.33722917 +k_0=0.99994471 +x_0=234.358 +y_0=185861.369 +a=6378249.2 +b=6356515 +towgs84=-168,-60,320,0,0,0,0 +pm=paris +units=m +no_defs';
    EWE.Comments   := 'NTF (Paris) / Lambert Corse';
//    EWE.ProjPointer:= _pj_init_plus_path(PChar(EWE.Parameters), '.\\nad');
    AddGeoGrid(EWE);

    EWE.CodeEPSG   := 27571;
    EWE.Parameters := '+proj=lcc +lat_1=49.50000000000001 +lat_0=49.50000000000001 +lon_0=2.33722917 +k_0=0.999877341 +x_0=600000 +y_0=1200000 +a=6378249.2 +b=6356515 +towgs84=-168,-60,320,0,0,0,0 +pm=paris +units=m +no_defs';
    EWE.Comments   := 'NTF (Paris) / Lambert zone I';
//    EWE.ProjPointer:= _pj_init_plus_path(PChar(EWE.Parameters), '.\\nad');
    AddGeoGrid(EWE);

    EWE.CodeEPSG   := 27572;
    EWE.Parameters := '+proj=lcc +lat_1=46.8 +lat_0=46.8 +lon_0=2.33722917 +k_0=0.99987742 +x_0=600000 +y_0=2200000 +a=6378249.2 +b=6356515 +towgs84=-168,-60,320,0,0,0,0 +pm=paris +units=m +no_defs';
    EWE.Comments   := 'NTF (Paris) / Lambert zone II';
//    EWE.ProjPointer:= _pj_init_plus_path(PChar(EWE.Parameters), '.\\nad');
    AddGeoGrid(EWE);

    EWE.CodeEPSG   := 27573;
    EWE.Parameters := '+proj=lcc +lat_1=44.10000000000001 +lat_0=44.10000000000001 +lon_0=2.33722917 +k_0=0.999877499 +x_0=600000 +y_0=3200000 +a=6378249.2 +b=6356515 +towgs84=-168,-60,320,0,0,0,0 +pm=paris +units=m +no_defs';
    //EWE.Parameters := '+proj=lcc +lat_1=44.10000000000001 +lat_0=44.10000000000001 +lon_0=2.33722917 +k_0=0.999877499 +x_0=599940 +y_0=3200000 +a=6378249.2 +b=6356515 +towgs84=-168,-60,320,0,0,0,0 +pm=paris +units=m +no_defs';

    EWE.Comments   := 'NTF (Paris) / Lambert zone III';
//    EWE.ProjPointer:= _pj_init_plus_path(PChar(EWE.Parameters), '.\\nad');
    AddGeoGrid(EWE);

    EWE.CodeEPSG   := 27574;
    EWE.Parameters := '+proj=lcc +lat_1=42.16500000000001 +lat_0=42.16500000000001 +lon_0=2.33722917 +k_0=0.99994471 +x_0=234.358 +y_0=4185861.369 +a=6378249.2 +b=6356515 +towgs84=-168,-60,320,0,0,0,0 +pm=paris +units=m +no_defs';
    EWE.Comments   := 'NTF (Paris) / Lambert zone IV';
//    EWE.ProjPointer:= _pj_init_plus_path(PChar(EWE.Parameters), '.\\nad');
    AddGeoGrid(EWE);
    // création des grilles UTM pour la France
    for ii := 29 to 34 do begin
      EWE.CodeEPSG   := 32600 + ii;
      EWE.Parameters := Format('+proj=utm +zone=%d +ellps=WGS84 +datum=WGS84 +units=m +no_defs', [ii]);
      EWE.Comments   := Format('UTM fuseau %d',[ii]);
//      EWE.ProjPointer:= _pj_init_plus_path(PChar(EWE.Parameters), '.\\nad');
      AddGeoGrid(EWE);
    end;
    // projection Google lololol
    EWE.CodeEPSG     := 379009;
    EWE.Parameters   := '+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs';
    EWE.Comments     := 'Projection Google';
//    EWE.ProjPointer:= _pj_init_plus_path(PChar(EWE.Parameters), '.\\nad');
    AddGeoGrid(EWE);
    // contrôle
    AfficherMessage('--> Grilles disponibles');
    Result := self.Count;
    if (self.Count = 0) then Exit;
    for ii := 0 to self.Count - 1 do
    begin
      EWE := self.GetGeoGrid(ii);
      AfficherMessage(Format('---- %d: %s', [EWE.CodeEPSG, EWE.Comments]));
    end;

  except
  end;
end;

procedure TConversionSysteme.AddGeoGrid(const G: TParametresEPSG);
var
  pG : ^TParametresEPSG;
begin
  New(pG);
  pG^ := G;
  self.Add(pG);
end;
function TConversionSysteme.GetGeoGrid(const Idx: integer): TParametresEPSG;
var
  pG : ^TParametresEPSG;
begin
  pG := self.Items[Idx];
  Result := pG^;
end;

function TConversionSysteme.GetGeoGridByCodeEPSG(const QCodeEPSG: integer): TParametresEPSG;
var
  i: integer;
  EWE: TParametresEPSG;
begin
  Result.CodeEPSG := -1;
  Result.Comments := '** Inconnu **';
  try
    for i := 0 to self.Count - 1 do
    begin
      EWE := self.GetGeoGrid(i);
      if (EWE.CodeEPSG = QCodeEPSG) then
      begin
        Result := EWE;
        Exit;
      end;
    end;
    //AfficherMessage(Format(' -- GetGeoGridByCodeEPSG: %d - %s',[EWE.CodeEPSG, EWE.Comments]));
  except
    Result.CodeEPSG   := -1;
    Result.Comments   := '*** Unknown ***';
    Result.Parameters :='';
    //Result.ProjPointer:= nil;
  end;
end;

function TConversionSysteme.GetIndexByCodeEPSG(const QCodeEPSG: integer): integer;
var
  EWE: TParametresEPSG;
  ii: Integer;
begin
  Result := -1;
  if (self.Count = 0) then exit;
  for ii := 0 to self.Count - 1 do
  begin
    EWE := self.GetGeoGrid(ii);
    if (EWE.CodeEPSG = QCodeEPSG) then
    begin
      Result := ii;
      Exit;
    end;
  end;
end;

function TConversionSysteme.GetNbreGrilles: integer;
begin
  Result := self.Count;
end;

// conversion générale d'une grille EPSG à une autre
function TConversionSysteme.Conversion_EPSG_To_EPSG(const Src, Tgt: integer;
  const MyPoint: TProjUV): TProjUV;
var
  DELTA_X, DELTA_Y  : Double;
  SystEPSGSrc, SystEPSGTgt: TParametresEPSG;
  EWE, WU : TProjUV;
  QPointUV: TProjUV;
begin
  //AfficherMessage(Format(' -- Conversion_EPSG_To_EPSG: %d->%d - %.8f %.8f',[Src, Tgt, MyPoint.U, MyPoint.V]));
  if (Src = Tgt) then // aucune conversion
  begin
    Result := MyPoint;
    Exit;
  end;

  if (Src = 4326) then // conversion WGS84 vers système cible
  begin
    WU.U := DEG_TO_RAD * MyPoint.U;
    WU.V := DEG_TO_RAD * MyPoint.V;

    SystEPSGTgt := self.GetGeoGridByCodeEPSG(Tgt);
    Exit;
  end;
  if (Tgt = 4326) then // conversion vers WGS84
  begin
    // correction d'une erreur systématique
    // provisoire
    case Src of
      27573: // Lambert 3
        begin
          DELTA_X := -59.930;
          DELTA_Y := 0.00; //  2.120;
        end
      else // UTM: OK, pas de corrections
        begin
          DELTA_X := 0.00;
          DELTA_Y := 0.00; //  2.120;
        end;
    end;
    QPointUV.U := MyPoint.U + DELTA_X;
    QPointUV.V := MyPoint.V + DELTA_Y;

    SystEPSGSrc := self.GetGeoGridByCodeEPSG(Src);
//    WU := _pj_inv(QPointUV, SystEPSGSrc.ProjPointer);
    Result.U := RAD_TO_DEG * WU.U;
    Result.V := RAD_TO_DEG * WU.V;
    Exit;
  end;
  // autres cas
  SystEPSGSrc := self.GetGeoGridByCodeEPSG(Src);
  SystEPSGTgt := self.GetGeoGridByCodeEPSG(Tgt);
  // passe 1: Conversion en WGS84
  QPointUV.U := MyPoint.U + DELTA_X;
  QPointUV.V := MyPoint.V + DELTA_Y;
//  WU  := _pj_inv(QPointUV, SystEPSGSrc.ProjPointer);     // vers WGS84
//  EWE := _pj_fwd(WU, SystEPSGTgt.ProjPointer);          // vers EPSG:xxxx
  Result := EWE;
end;





end.

