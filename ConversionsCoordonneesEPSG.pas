unit ConversionsCoordonneesEPSG;
// Date: 27/05/2013
// Wrapper pour les conversions de coordonnées
// utilisant Proj4s (proj447.dll) mais peut évoluer vers un système indépendant
// (cette librairie est un logiciel libre)
// /!\ Proj4 est probablement bogué (60 m de décalage en X et 2 m en Y pour certaines projections)
//------------------------------------------------------------------------------

{$INCLUDE CompilationParameters.inc}


interface

uses
  StructuresDonnees,
  Classes, SysUtils,
  Common,
  //ProjApi447,
  uDsCoordinates;

const LIB_PROJ_4S = 'proj447.dll';
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


Function  _pj_init_plus_path(const Args: PChar; const Path: PChar):
          PProjPJ ;cdecl;external LIB_PROJ_4S;

{ forward projection normally Longitude Latitude to plain xy Coordinates }
Function  _pj_fwd(ProjLP:TProjLP; projPJ:PProjPJ):TProjXY;
          cdecl;external LIB_PROJ_4S;

{ inverse projection normally plain xy coordinates to longitude latitude coordinates }
Function  _pj_inv(ProjXY:TProjXY; projPJ:PProjPJ):TProjLP;
          cdecl;external LIB_PROJ_4S;

Function  _pj_transform(src,dst:PProjPJ;point_count:TLong;point_offset:Integer;
          x,y,z:PDoubleArray):Integer;cdecl;external LIB_PROJ_4S;

{..compare two datums }
Function  _pj_compare_datums(srcdefn,dstdefn:PProjPJ ):Integer
          ;cdecl;external LIB_PROJ_4S;
{ Free the allocated projections }
Function  _pj_free(projPJ:PProjPJ):Integer;cdecl;external LIB_PROJ_4S;

{ Get the error code }
Function  _pj_strerrno(errno:Integer):PChar;cdecl;external LIB_PROJ_4S;

{ Get the error code number }
Function  _pj_get_errno_ref:Integer;cdecl;external LIB_PROJ_4S;


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
    function Conversion_EPSG_To_EPSG(const Src, Tgt: integer; const MyPoint: TProjUV): TProjUV;
  public
    // gestion de la liste des grilles
    procedure AddGeoGrid(const G: TParametresEPSG);
    function GetGeoGrid(const Idx: integer): TParametresEPSG;
    function GetGeoGridByCodeEPSG(const QCodeEPSG: integer): TParametresEPSG;
    function GetIndexByCodeEPSG(const QCodeEPSG: integer): integer;
    function GetNbreGrilles: integer;
    function GetNomSysteme(const Idx: integer): string;

    // conversions d'un système à l'autre
    function ConversionSyst1ToSyst2(const Src, Tgt: string; const MyPoint: TProjUV): TProjUV;

end;

implementation
var
 hDLL: THandle;

 Ptr_pj_init_plus_path: function (const Args: PChar; const Path: PChar): PProjPJ ;cdecl;

 { forward projection normally Longitude Latitude to plain xy Coordinates }
 Ptr_pj_fwd           : function(ProjLP:TProjLP; projPJ:PProjPJ):TProjXY; cdecl;

 { inverse projection normally plain xy coordinates to longitude latitude coordinates }
 Ptr_pj_inv           : function(ProjXY:TProjXY; projPJ:PProjPJ):TProjLP; cdecl;

 Ptr_pj_transform     : function(src,dst:PProjPJ;point_count:TLong;point_offset:Integer; x,y,z:PDoubleArray):Integer;cdecl;

 {..compare two datums }
 Ptr_pj_compare_datums: function(srcdefn,dstdefn:PProjPJ ):Integer;cdecl;
 { Free the allocated projections }
 Ptr_pj_free          : function(projPJ:PProjPJ):Integer;cdecl;

 { Get the error code }
 Ptr_pj_strerrno      : function(errno:Integer):PChar;cdecl;
 { Get the error code number }
 Ptr_pj_get_errno_ref :  function():Integer;cdecl;




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
    AfficherMessage(' --> Recherche du fichier DLL');
    AfficherMessage(Format('%s.Initialiser(%s)',[ClassName, LIB_PROJ_4S]));
    if (not FileExists(LIB_PROJ_4S)) then begin
      AfficherMessage(Format('--> Fichier %s non trouvé', [LIB_PROJ_4S]));
      Exit;
    end;
    AfficherMessage(' --> Démarrage de la DLL');
    if (Not DemarrerDLL) then Exit;
    //----------------------------
    // préparer les grilles
    PreparerLesGrilles();

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
          _pj_free(EWE.ProjPointer);
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
    // Lambert 93                  +proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs
    FpjLT93 := _pj_init_plus_path(PARAMS_EPSG_2154, '.\\nad');
    // Essai de paramétrage d'une projection
    If (FpjLT93 = NIL) Then Begin
      // check error conditions
      cnt:=_pj_get_errno_ref;
      S:=String(_pj_strerrno(cnt));
      AfficherMessage(Format('--> Echec en démarrage de la DLL: %d - %s', [cnt, S]));
      Exit;
    End;
    AfficherMessage(Format('--> %s prêt pour les traitements', [LIB_PROJ_4S]));
    Result := True;
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
    EWE.ProjPointer:=  _pj_init_plus_path(PChar(EWE.Parameters), '.\\nad');
    AddGeoGrid(EWE);

    EWE.CodeEPSG   := 4326;
    EWE.Parameters := '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs';
    EWE.Comments   := 'WGS84';
    EWE.ProjPointer:=  _pj_init_plus_path(PChar(EWE.Parameters), '.\\nad');
    AddGeoGrid(EWE);

    EWE.CodeEPSG   := 2154;
    EWE.Parameters := '+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs';
    EWE.Comments   := 'RGF93 / Lambert-93';
    EWE.ProjPointer:=  _pj_init_plus_path(PChar(EWE.Parameters), '.\\nad');
    AddGeoGrid(EWE);

    EWE.CodeEPSG   := 27561;
    EWE.Parameters := '+proj=lcc +lat_1=49.50000000000001 +lat_0=49.50000000000001 +lon_0=2.33722917 +k_0=0.999877341 +x_0=600000 +y_0=200000 +a=6378249.2 +b=6356515 +towgs84=-168,-60,320,0,0,0,0 +pm=paris +units=m +no_defs';
    EWE.Comments   := 'NTF (Paris) / Lambert Nord France';
    EWE.ProjPointer:=  _pj_init_plus_path(PChar(EWE.Parameters), '.\\nad');
    AddGeoGrid(EWE);

    EWE.CodeEPSG   := 27562;
    EWE.Parameters := '+proj=lcc +lat_1=46.8 +lat_0=46.8 +lon_0=2.33722917 +k_0=0.99987742 +x_0=600000 +y_0=200000 +a=6378249.2 +b=6356515 +towgs84=-168,-60,320,0,0,0,0 +pm=paris +units=m +no_defs';
    EWE.Comments   := 'NTF (Paris) / Lambert Centre France';
    EWE.ProjPointer:=  _pj_init_plus_path(PChar(EWE.Parameters), '.\\nad');
    AddGeoGrid(EWE);

    EWE.CodeEPSG   := 27563;
    EWE.Parameters := '+proj=lcc +lat_1=44.10000000000001 +lat_0=44.10000000000001 +lon_0=2.33722917 +k_0=0.999877499 +x_0=600000 +y_0=200000 +a=6378249.2 +b=6356515 +towgs84=-168,-60,320,0,0,0,0 +pm=paris +units=m +no_defs';
    EWE.Comments   := 'NTF (Paris) / Lambert Sud France';
    EWE.ProjPointer:= _pj_init_plus_path(PChar(EWE.Parameters), '.\\nad');
    AddGeoGrid(EWE);

    EWE.CodeEPSG   := 27564;
    EWE.Parameters := '+proj=lcc +lat_1=42.16500000000001 +lat_0=42.16500000000001 +lon_0=2.33722917 +k_0=0.99994471 +x_0=234.358 +y_0=185861.369 +a=6378249.2 +b=6356515 +towgs84=-168,-60,320,0,0,0,0 +pm=paris +units=m +no_defs';
    EWE.Comments   := 'NTF (Paris) / Lambert Corse';
    EWE.ProjPointer:= _pj_init_plus_path(PChar(EWE.Parameters), '.\\nad');
    AddGeoGrid(EWE);

    EWE.CodeEPSG   := 27571;
    EWE.Parameters := '+proj=lcc +lat_1=49.50000000000001 +lat_0=49.50000000000001 +lon_0=2.33722917 +k_0=0.999877341 +x_0=600000 +y_0=1200000 +a=6378249.2 +b=6356515 +towgs84=-168,-60,320,0,0,0,0 +pm=paris +units=m +no_defs';
    EWE.Comments   := 'NTF (Paris) / Lambert zone I';
    EWE.ProjPointer:= _pj_init_plus_path(PChar(EWE.Parameters), '.\\nad');
    AddGeoGrid(EWE);

    EWE.CodeEPSG   := 27572;
    EWE.Parameters := '+proj=lcc +lat_1=46.8 +lat_0=46.8 +lon_0=2.33722917 +k_0=0.99987742 +x_0=600000 +y_0=2200000 +a=6378249.2 +b=6356515 +towgs84=-168,-60,320,0,0,0,0 +pm=paris +units=m +no_defs';
    EWE.Comments   := 'NTF (Paris) / Lambert zone II';
    EWE.ProjPointer:= _pj_init_plus_path(PChar(EWE.Parameters), '.\\nad');
    AddGeoGrid(EWE);

    EWE.CodeEPSG   := 27573;
    EWE.Parameters := '+proj=lcc +lat_1=44.10000000000001 +lat_0=44.10000000000001 +lon_0=2.33722917 +k_0=0.999877499 +x_0=600000 +y_0=3200000 +a=6378249.2 +b=6356515 +towgs84=-168,-60,320,0,0,0,0 +pm=paris +units=m +no_defs';
    //EWE.Parameters := '+proj=lcc +lat_1=44.10000000000001 +lat_0=44.10000000000001 +lon_0=2.33722917 +k_0=0.999877499 +x_0=599940 +y_0=3200000 +a=6378249.2 +b=6356515 +towgs84=-168,-60,320,0,0,0,0 +pm=paris +units=m +no_defs';

    EWE.Comments   := 'NTF (Paris) / Lambert zone III';
    EWE.ProjPointer:= _pj_init_plus_path(PChar(EWE.Parameters), '.\\nad');
    AddGeoGrid(EWE);

    EWE.CodeEPSG   := 27574;
    EWE.Parameters := '+proj=lcc +lat_1=42.16500000000001 +lat_0=42.16500000000001 +lon_0=2.33722917 +k_0=0.99994471 +x_0=234.358 +y_0=4185861.369 +a=6378249.2 +b=6356515 +towgs84=-168,-60,320,0,0,0,0 +pm=paris +units=m +no_defs';
    EWE.Comments   := 'NTF (Paris) / Lambert zone IV';
    EWE.ProjPointer:= _pj_init_plus_path(PChar(EWE.Parameters), '.\\nad');
    AddGeoGrid(EWE);
    // création des grilles UTM pour la France
    for ii := 29 to 34 do begin
      EWE.CodeEPSG   := 32600 + ii;
      EWE.Parameters := Format('+proj=utm +zone=%d +ellps=WGS84 +datum=WGS84 +units=m +no_defs', [ii]);
      EWE.Comments   := Format('UTM fuseau %d',[ii]);
      EWE.ProjPointer:= _pj_init_plus_path(PChar(EWE.Parameters), '.\\nad');
      AddGeoGrid(EWE);
    end;
    // projection Google lololol
    EWE.CodeEPSG     := 379009;
    EWE.Parameters   := '+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs';
    EWE.Comments     := 'Projection Google';
    EWE.ProjPointer:= _pj_init_plus_path(PChar(EWE.Parameters), '.\\nad');
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
    Result.ProjPointer:= nil;
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

function TConversionSysteme.GetNomSysteme(const Idx: integer): string;
begin

end;

function TConversionSysteme.ConversionSyst1ToSyst2(const Src, Tgt: string; const MyPoint: TProjUV): TProjUV;
var
  MySrc: Integer;
  MyTgt: Integer;
begin
  try
    MySrc := StrToInt(Src);
    MyTgt := StrToInt(Tgt);
    Result := Conversion_EPSG_To_EPSG(MySrc, MyTgt, MyPoint);
  finally
    Result.U := -1.00;
    Result.V := -1.00;
  end;
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
    Result := _pj_fwd(WU, SystEPSGTgt.ProjPointer);
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
    WU := _pj_inv(QPointUV, SystEPSGSrc.ProjPointer);
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
  WU  := _pj_inv(QPointUV, SystEPSGSrc.ProjPointer);     // vers WGS84
  EWE := _pj_fwd(WU, SystEPSGTgt.ProjPointer);          // vers EPSG:xxxx
  Result := EWE;
end;





end.

