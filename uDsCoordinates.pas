//------------------------------------------------------------------------------
// DSpatial Projection Object
//------------------------------------------------------------------------------
Unit uDsCoordinates;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

Interface

Uses SysUtils, ProjApi447;

//------------------------------------------------------------------------------
Const
 cPjErrorInitPath ='Initialization file "proj_def.dat" in path "%s" not found!';
 cPjErrorInitLibrary ='Initialization of proj toolkit failed!';

 cPjDefaultProjection = '+proj=lcc +ellps=WGS84';

//------------------------------------------------------------------------------
Type
 TLookupEntry = Record
  Ident:String;
  Comment:String;
 End;
//------------------------------------------------------------------------------
Const
 cProjections : Array [0..120] Of TLookupEntry  = (
     (Ident: 'aea';    Comment: 'Albers Equal Area'),
     (Ident: 'aeqd';   Comment: 'Azimuthal Equidistant'),
     (Ident: 'airy';   Comment: 'Airy'),
     (Ident: 'aitoff'; Comment: 'Aitoff'),
     (Ident: 'alsk';   Comment: 'Mod. Stererographics of Alaska'),
     (Ident: 'apian';  Comment: 'Apian Globular I'),
     (Ident: 'august'; Comment: 'August Epicycloidal'),
     (Ident: 'bacon';  Comment: 'Bacon Globular'),
     (Ident: 'bipc';   Comment: 'Bipolar conic of western hemisphere'),
     (Ident: 'boggs';  Comment: 'Boggs Eumorphic'),
     (Ident: 'bonne';  Comment: 'Bonne (Werner lat_1=90)'),
     (Ident: 'cass';   Comment: 'Cassini'),
     (Ident: 'cc';     Comment: 'Central Cylindrical'),
     (Ident: 'cea';    Comment: 'Equal Area Cylindrical'),
     (Ident: 'chamb';  Comment: 'Chamberlin Trimetric'),
     (Ident: 'collg';  Comment: 'Collignon'),
     (Ident: 'crast';  Comment: 'Craster Parabolic (Putnins P4)'),
     (Ident: 'denoy';  Comment: 'Denoyer Semi-Elliptical'),
     (Ident: 'eck1';   Comment: 'Eckert I'),
     (Ident: 'eck2';   Comment: 'Eckert II'),
     (Ident: 'eck3';   Comment: 'Eckert III'),
     (Ident: 'eck4';   Comment: 'Eckert IV'),
     (Ident: 'eck5';   Comment: 'Eckert V'),
     (Ident: 'eck6';   Comment: 'Eckert VI'),
     (Ident: 'eqc';    Comment: 'Equidistant Cylindrical (Plate Caree)'),
     (Ident: 'eqdc';   Comment: 'Equidistant Conic'),
     (Ident: 'euler';  Comment: 'Euler'),
     (Ident: 'fahey';  Comment: 'Fahey'),
     (Ident: 'fouc';   Comment: 'Foucaut'),
     (Ident: 'fouc_s'; Comment: 'Foucaut Sinusoidal'),
     (Ident: 'gall';   Comment: 'Gall (Gall Stereographic)'),
     (Ident: 'gins8';  Comment: 'Ginsburg VIII (TsNIIGAiK)'),
     (Ident: 'gn_sinu';Comment: 'General Sinusoidal Series'),
     (Ident: 'gnom';   Comment: 'Gnomonic'),
     (Ident: 'goode';  Comment: 'Goode Homolosine'),
     (Ident: 'gs48';   Comment: 'Mod. Stererographics of 48 U.S.'),
     (Ident: 'gs50';   Comment: 'Mod. Stererographics of 50 U.S.'),
     (Ident: 'hammer'; Comment: 'Hammer & Eckert-Greifendorff'),
     (Ident: 'hatano'; Comment: 'Hatano Asymmetrical Equal Area'),
     (Ident: 'imw_p';  Comment: 'International Map of the World Polyconic'),
     (Ident: 'kav5';   Comment: 'Kavraisky V'),
     (Ident: 'kav7';   Comment: 'Kavraisky VII'),
     (Ident: 'labrd';  Comment: 'Laborde'),
     (Ident: 'laea';   Comment: 'Lambert Azimuthal Equal Area'),
     (Ident: 'lagrng'; Comment: 'Lagrange'),
     (Ident: 'larr';   Comment: 'Larrivee'),
     (Ident: 'lask';   Comment: 'Laskowski'),
     (Ident: 'latlong';Comment: 'Lat/long (Geodetic)'),
     (Ident: 'longlat';Comment: 'Lat/long (Geodetic)'),
     (Ident: 'lcc';    Comment: 'Lambert Conformal Conic'),
     (Ident: 'leac';   Comment: 'Lambert Equal Area Conic'),
     (Ident: 'lee_os'; Comment: 'Lee Oblated Stereographic'),
     (Ident: 'loxim';  Comment: 'Loximuthal'),
     (Ident: 'lsat';   Comment: 'Space oblique for LANDSAT'),
     (Ident: 'mbt_s';  Comment: 'McBryde-Thomas Flat-Polar Sine (No. 1)'),
     (Ident: 'mbt_fps';Comment: 'McBryde-Thomas Flat-Pole Sine (No. 2)'),
     (Ident: 'mbtfpp'; Comment: 'McBride-Thomas Flat-Polar Parabolic'),
     (Ident: 'mbtfpq'; Comment: 'McBryde-Thomas Flat-Polar Quartic'),
     (Ident: 'mbtfps'; Comment: 'McBryde-Thomas Flat-Polar Sinusoidal'),
     (Ident: 'merc';   Comment: 'Mercator'),
     (Ident: 'mil_os'; Comment: 'Miller Oblated Stereographic'),
     (Ident: 'mill';   Comment: 'Miller Cylindrical'),
     (Ident: 'mpoly';  Comment: 'Modified Polyconic'),
     (Ident: 'moll';   Comment: 'Mollweide'),
     (Ident: 'murd1';  Comment: 'Murdoch I'),
     (Ident: 'murd2';  Comment: 'Murdoch II'),
     (Ident: 'murd3';  Comment: 'Murdoch III'),
     (Ident: 'nell';   Comment: 'Nell'),
     (Ident: 'nell_h'; Comment: 'Nell-Hammer'),
     (Ident: 'nicol';  Comment: 'Nicolosi Globular'),
     (Ident: 'nsper';  Comment: 'Near-sided perspective'),
     (Ident: 'nzmg';   Comment: 'New Zealand Map Grid'),
     (Ident: 'ob_tran';Comment: 'General Oblique Transformation'),
     (Ident: 'ocea';   Comment: 'Oblique Cylindrical Equal Area'),
     (Ident: 'oea';    Comment: 'Oblated Equal Area'),
     (Ident: 'omerc';  Comment: 'Oblique Mercator'),
     (Ident: 'ortel';  Comment: 'Ortelius Oval'),
     (Ident: 'ortho';  Comment: 'Orthographic'),
     (Ident: 'pconic'; Comment: 'Perspective Conic'),
     (Ident: 'poly';   Comment: 'Polyconic (American)'),
     (Ident: 'putp1';  Comment: 'Putnins P1'),
     (Ident: 'putp2';  Comment: 'Putnins P2'),
     (Ident: 'putp3';  Comment: 'Putnins P3'),
     (Ident: 'putp3p'; Comment: 'Putnins P3p'),
     (Ident: 'putp4p'; Comment: 'Putnins P4p'),
     (Ident: 'putp5';  Comment: 'Putnins P5'),
     (Ident: 'putp5p'; Comment: 'Putnins P5p'),
     (Ident: 'putp6';  Comment: 'Putnins P6'),
     (Ident: 'putp6p'; Comment: 'Putnins P6p'),
     (Ident: 'qua_aut';Comment: 'Quartic Authalic'),
     (Ident: 'robin';  Comment: 'Robinson'),
     (Ident: 'rpoly';  Comment: 'Rectangular Polyconic'),
     (Ident: 'sinu';   Comment: 'Sinusoidal (Sanson-Flamsteed)'),
     (Ident: 'somerc'; Comment: 'Swiss. Obl. Mercator'),
     (Ident: 'stere';  Comment: 'Stereographic'),
     (Ident: 'tcc';    Comment: 'Transverse Central Cylindrical'),
     (Ident: 'tcea';   Comment: 'Transverse Cylindrical Equal Area'),
     (Ident: 'tissot'; Comment: 'Tissot'),
     (Ident: 'tmerc';  Comment: 'Transverse Mercator'),
     (Ident: 'tpeqd';  Comment: 'Two Point Equidistant'),
     (Ident: 'tpers';  Comment: 'Tilted perspective'),
     (Ident: 'ups';    Comment: 'Universal Polar Stereographic'),
     (Ident: 'urm5';   Comment: 'Urmaev V'),
     (Ident: 'urmfps'; Comment: 'Urmaev Flat-Polar Sinusoidal'),
     (Ident: 'utm';    Comment: 'Universal Transverse Mercator (UTM)'),
     (Ident: 'vandg';  Comment: 'van der Grinten I'),
     (Ident: 'vandg2'; Comment: 'van der Grinten II'),
     (Ident: 'vandg3'; Comment: 'van der Grinten III'),
     (Ident: 'vandg4'; Comment: 'van der Grinten IV'),
     (Ident: 'vitk1';  Comment: 'Vitkovsky I'),
     (Ident: 'wag1';   Comment: 'Wagner I (Kavraisky VI)'),
     (Ident: 'wag2';   Comment: 'Wagner II'),
     (Ident: 'wag3';   Comment: 'Wagner III'),
     (Ident: 'wag4';   Comment: 'Wagner IV'),
     (Ident: 'wag5';   Comment: 'Wagner V'),
     (Ident: 'wag6';   Comment: 'Wagner VI'),
     (Ident: 'wag7';   Comment: 'Wagner VII'),
     (Ident: 'weren';  Comment: 'Werenskiold I'),
     (Ident: 'wink1';  Comment: 'Winkel I'),
     (Ident: 'wink2';  Comment: 'Winkel II'),
     (Ident: 'wintri'; Comment: 'Winkel Tripel')
    );

//------------------------------------------------------------------------------
Type
TEllipsoides = (ellMerit, ellSGS85, ellGRS80, ellIAU76, ellAiry, ellAPL49,
                ellNWL9D, ellModAriy, ellAndrae, ellAustSA, ellGRS67,
                ellBessel, ellBesselNamibia, ellClark66, ellClark80,
                ellCPM, ellDelambre, ellEngelis, ellEverest30, ellEverest48,
                ellEverest56, ellEverest69, ellEverestSS, ellFischer60,
                ellFischer60m, ellFischer68, ellHelmert, ellHough,
                ellInternational, ellKrassovsky, ellKaula, ellLerch,
                ellMaupertius, ellNewInternational, ellPlessis, ellSEAsia,
                ellWalbeck, ellWGS60, ellWGS66, ellWGS72, ellWGS84, ellSphere);
//------------------------------------------------------------------------------
Const
 cEllipsoides : Array [TEllipsoides] Of TLookupEntry = (
    (Ident: 'MERIT';   Comment:'MERIT 1983'),
    (Ident: 'SGS85';   Comment:'Soviet Geodetic System 85'),
    (Ident: 'GRS80';   Comment:'GRS 1980(IUGG, 1980)'),
    (Ident: 'IAU76';   Comment:'IAU 1976'),
    (Ident: 'airy';    Comment:'Airy 1830'),
    (Ident: 'APL4.9';  Comment:'Appl. Physics. 1965'),
    (Ident: 'NWL9D';   Comment:'Naval Weapons Lab., 1965'),
    (Ident: 'mod_airy';Comment:'Modified Airy'),
    (Ident: 'andrae';  Comment:'Andrae 1876 (Den., Iclnd.)'),
    (Ident: 'aust_SA'; Comment:'Australian Natl & S. Amer. 1969'),
    (Ident: 'GRS67';   Comment:'0 GRS 67(IUGG 1967)'),
    (Ident: 'bessel';  Comment:'Bessel 1841'),
    (Ident: 'bess_nam';Comment:'Bessel 1841 (Namibia)'),
    (Ident: 'clrk66';  Comment:'Clarke 1866'),
    (Ident: 'clrk80';  Comment:'Clarke 1880 mod.'),
    (Ident: 'CPM';     Comment:'Comm. des Poids et Mesures 1799'),
    (Ident: 'delmbr';  Comment:'Delambre 1810 (Belgium)'),
    (Ident: 'engelis'; Comment:'Engelis 1985'),
    (Ident: 'evrst30'; Comment:'Everest 1830'),
    (Ident: 'evrst48'; Comment:'Everest 1948'),
    (Ident: 'evrst56'; Comment:'Everest 1956'),
    (Ident: 'evrst69'; Comment:'Everest 1969'),
    (Ident: 'evrstSS'; Comment:'Everest (Sabah & Sarawak)'),
    (Ident: 'fschr60'; Comment:'Fischer (Mercury Datum) 1960'),
    (Ident: 'fschr60m';Comment:'Modified Fischer 1960'),
    (Ident: 'fschr68'; Comment:'Fischer 1968'),
    (Ident: 'helmert'; Comment:'Helmert 1906'),
    (Ident: 'hough';   Comment:'Hough'),
    (Ident: 'intl';    Comment:'International 1909 (Hayford)'),
    (Ident: 'krass';   Comment:'Krassovsky, 1942'),
    (Ident: 'kaula';   Comment:'Kaula 1961'),
    (Ident: 'lerch';   Comment:'Lerch 1979'),
    (Ident: 'mprts';   Comment:'Maupertius 1738'),
    (Ident: 'new_intl';Comment:'New International 1967'),
    (Ident: 'plessis'; Comment:'Plessis 1817 (France)'),
    (Ident: 'SEasia';  Comment:'Southeast Asia'),
    (Ident: 'walbeck'; Comment:'Walbeck'),
    (Ident: 'WGS60';   Comment:'WGS 60'),
    (Ident: 'WGS66';   Comment:'WGS 66'),
    (Ident: 'WGS72';   Comment:'WGS 72'),
    (Ident: 'WGS84';   Comment:'WGS 84'),
    (Ident: 'sphere';  Comment:'Normal Sphere (r=6370997)'));

//------------------------------------------------------------------------------
//
//------------------------------------------------------------------------------
Type
 TLookupUnitEntry = Record
  Ident:String;
  Scale:Double;
  Comment:String;
 End;
//------------------------------------------------------------------------------
 TPlainUnits = (puKilometer, puMeter, puDeciMeter, puCentiMeter, pMilliMeter,
                puIntlNauticalMile, puIntlInch, puIntlFoot, puIntlYard,
                puIntlStatuteMile, puIntlFathom, puIntChain, puIntlLink,
                puUSSurvInch, puUSSurvFoot, puUSSurvYard, puUSSurvChain,
                puUSSurvStatuteMile, puIndianYard, puIndianFoot, puIndianChain);
//------------------------------------------------------------------------------
Const
 cPlainUnits : Array [TPlainUnits] Of TLookupUnitEntry = (
  (Ident: 'km';     Scale:1000.0;    Comment:'Kilometer'),
  (Ident: 'm';      Scale: 1.0;      Comment:'Meter'),
  (Ident: 'dm';     Scale: 1/10;     Comment:'Decimeter'),
  (Ident: 'cm';     Scale: 1/100;    Comment:'Centimeter'),
  (Ident: 'mm';     Scale: 1/1000;   Comment:'Millimeter'),
  (Ident: 'kmi';    Scale: 1852.0;   Comment:'International Nautical Mile'),
  (Ident: 'in';     Scale: 0.0254;   Comment:'International Inch'),
  (Ident: 'ft';     Scale: 0.3048;   Comment:'International Foot'),
  (Ident: 'yd';     Scale: 0.9144;   Comment:'International Yard'),
  (Ident: 'mi';     Scale: 1609.344; Comment:'International Statute Mile'),
  (Ident: 'fath';   Scale: 1.8288;   Comment:'International Fathom'),
  (Ident: 'ch';     Scale: 20.1168;  Comment:'International Chain'),
  (Ident: 'link';   Scale: 0.201168; Comment:'International Link'),
  (Ident: 'us-in';  Scale: 1./39.37; Comment:'U.S. Surveyor´s Inch'),
  (Ident: 'us-ft';  Scale: 0.304800609601219; Comment:'U.S. Surveyor´s Foot'),
  (Ident: 'us-yd';  Scale: 0.914401828803658; Comment:'U.S. Surveyor´s Yard'),
  (Ident: 'us-ch';  Scale: 20.11684023368047; Comment:'U.S. Surveyor´s Chain'),
  (Ident: 'us-mi';  Scale: 1609.347218694437; Comment:'U.S. Surveyor´s Statute Mile'),
  (Ident: 'ind-yd'; Scale: 0.91439523;  Comment:'Indian Yard'),
  (Ident: 'ind-ft'; Scale: 0.30479841;  Comment:'Indian Foot'),
  (Ident: 'ind-ch'; Scale: 20.11669506; Comment:'Indian Chain'));

//------------------------------------------------------------------------------
Type
 TCoordinateTranslator = class (TObject)
  Private
   fPJ   : PProjPJ;
   fProjStr: String;
   fPathStr: String;
  Public
   Constructor Create(aPathStr:String);
   Destructor  Destroy ; Override;
   Function    Open(Parameter:String):Boolean;
   Function    CalcForward(Lon,Lat:Double;Var P:TProjXY):Boolean;
   Function    CalcInverse(X,Y:Double;Var D:TProjUV):Boolean;
 End;

//------------------------------------------------------------------------------
Implementation

//------------------------------------------------------------------------------
// Initialize the Projection
//------------------------------------------------------------------------------
Constructor TCoordinateTranslator.Create(aPathStr:String);
 Begin
  Inherited Create;
  If Not FileExists(aPathStr+'\proj_def.dat') Then
   raise Exception.Create(Format(cPjErrorInitPath,[aPathStr]));
  fProjStr:=cPjDefaultProjection;
  fPJ := _pj_init_plus_path(PChar(fProjStr),PChar(aPathStr));
  If fPJ=NIL Then  raise Exception.Create(cPjErrorInitLibrary);
  fPathStr:=aPathStr;
 End;

//------------------------------------------------------------------------------
//
//------------------------------------------------------------------------------
Destructor TCoordinateTranslator.Destroy;
 Begin
  If fPJ<>NIL Then _pj_free(fPJ);
  Inherited Destroy;
 End;

//------------------------------------------------------------------------------
//
//------------------------------------------------------------------------------
Function TCoordinateTranslator.Open(Parameter:String):Boolean;
 Begin
  If fPJ<>NIL Then _pj_free(fPJ);
  fPJ := _pj_init_plus_path(PChar(Parameter),PChar(fPathStr));
  If fPJ=NIL Then  raise Exception.Create(cPjErrorInitLibrary);
 End;

//------------------------------------------------------------------------------
//
//------------------------------------------------------------------------------
Function TCoordinateTranslator.CalcForward(Lon,Lat:Double;Var P:TProjXY):Boolean;
 Var D:TProjUV;
 Begin
  If fPJ=NIL Then  raise Exception.Create(cPjErrorInitLibrary);
  D.U:=Lon*Deg_TO_RAD;
  D.V:=Lat*Deg_TO_RAD;
  P:=_pj_fwd(D,fPJ);
 End;

//------------------------------------------------------------------------------
//
//------------------------------------------------------------------------------
Function TCoordinateTranslator.CalcInverse(X,Y:Double;Var D:TProjUV):Boolean;
 Var P:TProjXY;
 Begin
  If fPJ=NIL Then  raise Exception.Create(cPjErrorInitLibrary);
  P.U:=X;P.V:=Y; D:=_pj_inv(P,fPJ);
  D.U:=D.U*RAD_TO_DEG;
  D.V:=D.V*RAD_TO_DEG;
 End;

//------------------------------------------------------------------------------
end.
//------------------------------------------------------------------------------
