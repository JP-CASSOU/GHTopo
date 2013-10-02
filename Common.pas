unit Common;
{$INCLUDE CompilationParameters.inc}
// Fonctions communes
// Date: 19/04/2013
// Statut: Fonctionnel
// Des détails à revoir

interface
uses
  StructuresDonnees,
  SysUtils, Classes,
  Grids, Clipbrd, LCLType, // pour les grilles et le presse-papiers
  math,
  Graphics;




//type TTabStopArray  = array of const; // pour lignes à colonnage fixe



//-----------------------------------------------
// fonction d'affichage vers la console
procedure AfficherMessage(const Msg: string);



// obtenir l'année std depuis année abrégées
function GetAnneeISO(const QAnnee: word): word;
// déclinaison magnétique en mode TOPOROBOT
function GetTOPOROBOTDecliMag(const DecInDegrees: double): double;

// retourne le type de visée
function GetTypeDeVisee(const T: integer): TTypeDeVisee;

// fonction d'incrémentation lexicographique
function IncrementString(const S0: string): string;

//****************************
// fonctions communes
//****************************
// encodage/Décodage sécurisé des dates
function  SafeEncodeDate(Year, Month, Day: Word): TDateTime;
procedure SafeDecodeDate(Date: TDateTime; var Year, Month, Day: Word);

procedure Swap(var V1, V2: integer); overload;
procedure Swap(var V1, V2: Double); overload;
procedure Swap(var V1, V2: string); overload;


function IsInRange(const Value: Extended;
                   const MinValue, MaxValue: Extended): Boolean; overload;
function IsInRange(const Value: integer;
                   const MinValue, MaxValue: integer): Boolean; overload;

function IIF(const Condition: boolean; const V1, V2: boolean): boolean; overload;
function IIF(const Condition: boolean; const V1, V2: integer): integer; overload;
function IIF(const Condition: boolean; const V1, V2: Extended): Extended; overload;
function IIF(const Condition: boolean; const V1, V2: String): String; overload;

function HasValue(const Value: integer; const ArrValues: array of integer): boolean;
function DateIsCorrect(const Y, M, D: word): Boolean;
//----------------------------------
function ProduitVectoriel(const Vect1, Vect2: TPoint3Df;
                          const Normalized: Boolean):TPoint3Df;
function CalculerAngles(const X1, Y1,
                              X2, Y2: double): double;
function Hypot2D(const DX, DY: Double): Double;
function Hypot3D(const DX, DY, DZ: Double): Double;

function GetAzimut(const dx, dy: Double; const Unite: double): double;

procedure GetBearingInc(const dx, dy, dz: double;
                        var Dist, Az, Inc: double;
                        const fUB, fUC: Double);
//*****************************************
//-------------------------------------
function RGB(const R, G, B: byte): integer;
function GetRValue(const Coul: TColor): Byte;
function GetGValue(const Coul: TColor): Byte;
function GetBValue(const Coul: TColor): Byte;
function GetFloatRValue(const C: TColor): double;
function GetFloatGValue(const C: TColor): double;
function GetFloatBValue(const C: TColor): double;
function Acad2RGB(const n : integer) : tColor; // palette par défaut d'Autocad
function RGB2Acad(const C: TColor) : Byte;
function GetSVGColor(const C: TColor): string;

//-------------------------------------
// routines de couleurs

function GetPCColor(const MC: TMacintoshColor): TColor; overload;
function GetPCColor(const mR, mG, mB: word):TColor; overload;

function GetMacColor(const PCC: TColor): TMacintoshColor;
function GetBYTEColor(const Coul: TColor): TColor3b;

function GetColorDegrade(const z: Double;
                         const zmin, zmax: Double;
                         const Coul1, Coul2: TColor): TColor;
function GetNTSCGrayScale(const C: TColor): Byte;

// Retourne sous forme de couleur OpenGL la couleur passée en argument
function GetGLColor(const Coul: TColor; const Alpha: Double): TGLColor;

// Retourne sous forme de couleur Pascal la couleur OpenGL passée en argument
function GetPASColor(const Coul: TGLColor): TColor;
function PascalToGLColor(const Coul: TColor): TGLColor;
//*************
// routines de texte
//function Replace
function EnsureMakeFilename(const S: string): string;
function EnlevePerluete(const s: string): string;
function IndexOfString(const S: string; const IsEqual: boolean; const Strs: array of string): integer;
function ChooseString(const Index: integer; const Strs: array of string): string;
function SafeTruncateString(const Str: string; const L: integer): String;



procedure DrawTriangleWithDegrade(const V1, V2, V3: TPoint3DF;
                                  const C1, C2, C3: TColor);

function GetDeclimag(const InitialDate, CurrentDate: TDateTime;
                     const InitialDeclimag, Variation: Double):Double;

// Purge les accents d'un texte
function PurgerAccents(const S: string): string;


// Convertir un fichier texte vers le format désiré
function ConvertTextFile(const InputFileName, OutputFilename: string;
                         const InputFormat, OutputFormat: TTextFileFormat): boolean;

// extraction de paramètres d'une chaine
function Split(const Str: string; const Sep: string):TStringArray;
// splitter une ligne à positions fixes
function SplitFixedColLine(const MyLine: string; const Tabs: array of integer): TStringArray;
// obtenir un tableau 2D à partir d'une string
function GetStringArray2D(const S: string; const SepCol: string): TStringArray2D;


// formater un filtre de fichier
// Arguments pairs du tableau: Nom du filtre
// Arguments impairs: Filtre
function FormatFileFilters(const FF: array of string; const WithAll: boolean): string;

// proposer une équidistance en fonction de l'étendue du réseau
function ProposerEquidistanceDef(const C1, C2: TPoint3Df; const Defaut: double): Double;

// test d'intersection de deux rectangle
function IntersectRectangles(const R1, R2: TRect2Df): boolean;


// fonctions de rappel
type TCallbackProc = procedure of object;



// description d'un système de coordonnées
function DescribeEPSGSystem(const CodeEPSG: integer): string;

// formatter nombre avec séparateurs de milliers
function FormatterNombreAvecSepMilliers(const X: double): string;

// couleur RGBA au format KML
function KMLColor(const R, G, B, A: byte): string;
// couleur RGBA au format KML
function ColorToHTMLColor(const Couleur: TColor): string;
function ColorHTMLToColor(const CouleurHTML: string): TColor;

//
function GetIDStation(const N: integer; const St: TStation): string;
// calcul localisé de coordonnées
function ConversionCoordonneesIsolees(const Src, Tgt: string; const QX, QY: double; out PointConverted: TPoint2Df): boolean;
function ConversionTableauCoordonneesIsolees(const Src, Tgt: string; const ArrCoordsInput: TArrayPoints2Df; out ArrPointsConverted: TArrayPoints2Df): boolean;

// sauvegarde et copie de grilles
function GRDCCopierUnTableau(const Grille: TStringGrid): boolean;
function GRDCollerDepuisClipBoard(const Grille: TStringGrid; const HasTitres: boolean): boolean;
function GRDSauverUnTableauEnCSV(const Grille: TStringGrid; const FichierCSV: string): boolean;
// dates SQL et vice-versa
function DatePascalToDateSQL(const QDate: TDateTime): string;
function DateSQLToDatePascal(const QDateSQL: string): TDateTime;

// version du XML et encodage
function FormatterVersionEncodageXML(const version: string; const encodage: string): string; inline;


implementation
uses
  {$IFDEF MSWINDOWS}
    Forms,
    {$IFDEF USE_CONVERTISSEUR_EPSG}
      ConversionsCoordonneesEPSG, // API Conversapi
    {$ENDIF}
    {$IFDEF USE_CONVERTISSEUR_ESIBERT}
      ConversionsCoordonneesESibert, // API E Sibert
    {$ENDIF}
    {$IFDEF USE_CONVERTISSEUR_JPC}
      ConvertisseurJPC, // API JPC
    {$ENDIF}

    frmJournal;
  {$ENDIF}
  {$IFDEF LINUX}
    Forms,
    ConversionsCoordonneesESibert,
    frmJournal;
  {$ENDIF}


// formatter nombre avec séparateurs de milliers
// sécurisé avec AnsiToUTF8
function FormatterNombreAvecSepMilliers(const X: double): string;
begin
  Result := AnsiToUtf8(Format('%.0n',[X]));
end;

function KMLColor(const R, G, B, A: byte): string;
begin
  Result := Format('<color>%.2X%.2X%.2X%.2X</color>',[A, B, G, R]);
end;

function ColorToHTMLColor(const Couleur: TColor): string;
var
  R, V, B: byte;
begin
  R := Red(Couleur);
  V := Green(Couleur);
  B := Blue(Couleur);
  Result := Format('#%.2X%.2X%.2X', [R, V, B]);
end;
function ColorHTMLToColor(const CouleurHTML: string): TColor;
var
  R, V, B: byte;
  WU: String;
begin
  WU := Trim(CouleurHTML);
  WU[1] := '$';
  Result := StringToColor(WU);
end;



// Détermine si la valeur figure dans la liste
function HasValue(const Value: integer; const ArrValues: array of integer): boolean;
var
  i: integer;
begin
  Result:=False;
  for i:=Low(arrValues) to High(ArrValues) do begin
    if (Value = ArrValues[i]) then begin
      Result:=True;
      Exit;
    end;
  end;
end;
// vérifie si la date est correcte
function DateIsCorrect(const Y, M, D: word): Boolean;
var
  D1: TDateTime;
begin
  Result:=False;
  try
    D1:=SafeEncodeDate(Y,M,D);
    Result:=True;
  except
    Result:=False;
  end;
end;
//fonction de split
function Split(const Str: string; const Sep: string):TStringArray;
var
  pn   : integer;
  ps   : integer;
  S    : string;

begin
  for pn:=0 to High(Result) do Result[pn]:='';
  S:=Str;
  ps:=0;
  try
    pn:=0;
    repeat
     if pn>High(Result) then Break;
     ps:=Pos(Sep, S);
     //s:=Copy(s,0, ps-1);
     Result[pn]:=Trim(Copy(s,0, ps-1));
     Inc(pn);
     s:=Copy(s, 1+ps, Length(s));
    until ps=0;
    Result[pn-1]:=Trim(s);
  except
  end;
end;

// splitter une ligne à positions fixes
// DONE: Ecrire cette routine SplitFixedColLine
// Fonctionnement OK
function SplitFixedColLine(const MyLine: string; const Tabs: array of integer): TStringArray;
var
  pn   : integer;
  ps   : integer;
  S    : string;

begin
  // vider tout
  for pn:=0 to High(Result) do Result[pn]:='';
  //ps := 1;
  for pn := 0 to High(Tabs)-1 do begin
    Result[pn] := Copy(MyLine, Tabs[pn], (Tabs[pn+1] - Tabs[pn]));
    //ps := Tabs[pn];
  end;
end;

// Fonctions d'échange
procedure Swap(var V1, V2: integer); overload;
var Tmp: integer;
begin
  Tmp:=V1;
  V1:=V2;
  V2:=Tmp;
end;
procedure Swap(var V1, V2: Double); overload;
var Tmp: Double;
begin
  Tmp:=V1;
  V1:=V2;
  V2:=Tmp;
end;
procedure Swap(var V1, V2: string); overload;
var Tmp: string;
begin
  Tmp:=V1;
  V1:=V2;
  V2:=Tmp;
end;


// calcul de déclinaison magnétique
function GetDeclimag(const InitialDate, CurrentDate: TDateTime;
                     const InitialDeclimag, Variation: Double):Double;
const INTERVALYEARS=10;
var
  y,m,d: word;
  d10a: TDateTime;
  d10d: double;
  p: double;
begin
  AfficherMessage(Format('GetDeclimag(%s, %s, %f, %f)',
                         [DateToStr(InitialDate),
                          DateToStr(CurrentDate),
                          InitialDeclimag, Variation]));
  SafeDecodeDate(InitialDate,Y,M,D);
  d10a:=SafeEncodedate(Y+INTERVALYEARS, M, D);
  d10a:=d10a - InitialDate;
  //d:=CurrentDate - InitialDate;
  d10d:=INTERVALYEARS * Variation;
  p:=d10d / d10a;


  Result:=InitialDeclimag + p * (CurrentDate - InitialDate);
end;

// afficher un message de contrôle
procedure AfficherMessage(const Msg: string);
begin

  {$IFDEF POCKET_GHTOPO}
     Form1.lbProcessing.Caption := PurgerAccents(Msg);
  {$ELSE}
     try
       with dlgProcessing.ListBox1 do begin
          if Items.Count > MAX_LINES_LOG then
            Items.Delete(0);
          Items.Add(TimeToStr(Now()) +' | ' + AnsiToUtf8(Msg));
          ItemIndex:=dlgProcessing.ListBox1.Items.Count-1;
          Refresh;
        end;
     except
     end;
  {$ENDIF}
  // Vidage de la pompe à messages
  // indispensable sous WinCE
  // (son absence est une des ppales causes de plantages)
  Application.ProcessMessages;
end;

// choisir une chaine en fonction d'une valeur
function ChooseString(const Index: integer; const Strs: array of string): string;
begin
  try
    if (Index<0) or (Index>High(Strs)) then begin
      Result:=Format('** Erroneous index: %d **',[Index]);
      Exit;
    end;
    Result:=Strs[Index];
  finally
  end;
end;
function IndexOfString(const S: string; const IsEqual: boolean; const Strs: array of string): integer;
var
  i: integer;
  C: boolean;
begin
  Result:=-1;
  try
    for i:=Low(Strs) to High(Strs) do
    begin
      if (IsEqual) then C := (LowerCase(S) = LowerCase(Strs[i]))
                   else C := (Pos(S, Strs[i]) > 0);
      if (C) then
      begin
        Result:=i;
        Exit;
      end;
    end;
  except
    Result:=-1;
  end;
end;
function Hypot3D(const DX, DY, DZ: Double): Double;
begin
  Result:=Sqrt(dx*dx+dy*dy+dz*dz);
end;
function Hypot2D(const DX, DY: Double): Double;
begin
  Result:=Sqrt(dx*dx+dy*dy);
end;
// retourne un azimut
function GetAzimut(const dx, dy: Double; const Unite: double): double;
var
  a: double;
begin
  a:=ArcTan2(dy, dx+1e-12);
  if a<0 then a:=a+2*PI;
  a:=0.50*PI-a;
  if a<0 then a:=a+2*PI;
  Result:=a*0.50*Unite/pi;
end;


// retourne la longueur, direction et pente pour dx, dy, dz
procedure GetBearingInc(const dx, dy, dz: double;
                        var Dist, Az, Inc: double;
                        const fUB, fUC: Double);
var
  dp: Double;
begin;
  dp  :=Hypot2D(dx, dy);
  Dist:=Hypot2D(dp,dz);
  Inc :=ArcTan2(dz, dp)*0.5*fUC/pi;
  //Az  :=(0.5*PI-ArcTan2(dY,dx))*0.5*fUB/pi;
  Az:=GetAzimut(dx,dy, fUB);
end;
// couleur OpenGL
function PascalToGLColor(const Coul: TColor): TGLColor;
const
  m = 1/256;
var
  c: TGLColor;
begin
  c.R:=GetRValue(Coul)* m;
  c.G:=GetGValue(Coul)* m;
  c.B:=GetBValue(Coul)* m;
  c.A:=1.00;
  Result:=c;
end;

// enlève les perluettes
function EnlevePerluete(const s: string): string;
var
  p: integer;
  st: string;
begin
  st:=s;
  p:=Pos('&',st);
  if p=0 then
  begin
    Result:=st;
    Exit;
  end;
  Delete(st,p,1);
  Result:=st;
end;
function ProduitVectoriel(const Vect1, Vect2: TPoint3Df;
                          const Normalized: Boolean):TPoint3Df;
var
  v: TPoint3Df;
  r: Extended;
begin
  v.X:=Vect1.Y*Vect2.Z-Vect1.Z*Vect2.Y;
  v.Y:=Vect1.Z*Vect2.X-Vect1.X*Vect2.Z;
  v.Z:=Vect1.X*Vect2.Y-Vect1.Y*Vect2.X;
  if Normalized then begin
    r:=sqrt(Sqr(v.x)+sqr(v.y)+sqr(v.z))+1e-12;
    v.X:=v.x/r;
    v.y:=v.y/r;
    v.z:=v.z/r;

  end;
  Result:=v;

end;
function CalculerAngles(const X1, Y1,
                              X2, Y2: double): double;
var
  V1, V2, W: TPoint3Df;
begin
  // vecteur V1           vecteur V2        vecteur w
  V1.X:=X1;               V2.X:=X2;         W.X :=0;
  V1.Y:=Y1;               V2.Y:=Y2;         W.Y :=0;
  V1.Z:=0;                V2.Z:=0;          W.Z :=1;
  // produits vectoriels
  v1:=ProduitVectoriel(v1,w,True);
  v2:=ProduitVectoriel(v2,w,True);
  //composition vectorielle
  w.x:=v1.x+v2.X;
  w.y:=v1.y+v2.Y;
  w.z:=v1.z+v2.z;
  // angles
  Result:=ArcTan2(w.y+1e-12, w.x+1e-12);
end;
//****************************************************************************
// fonctions de couleurs recadrées sur intervalle [0.00; 1.00]
function GetFloatRValue(const C: TColor): double;
begin
  Result:=GetRValue(C) / 256;
end;
function GetFloatGValue(const C: TColor): double;
begin
  Result:=GetGValue(C) / 256;
end;
function GetFloatBValue(const C: TColor): double;
begin
  Result:=GetBValue(C) / 256;
end;
//*****************************************
// Conversions couleurs PC<>Mac
function GetPCColor(const MC: TMacintoshColor): TColor; overload;
begin
  Result:=RGB(MC.R shr 8,
              MC.G shr 8,
              MC.B shr 8);

end;
function GetPCColor(const mR, mG, mB: word):TColor; overload;
begin
  Result:=RGB(mR shr 8,
              mG shr 8,
              mB shr 8);
end;

function GetMacColor(const PCC: TColor): TMacintoshColor;
begin
  Result.R:=GetRValue(PCC) * 256;
  Result.G:=GetGValue(PCC) * 256;
  Result.B:=GetBValue(PCC) * 256;
end;
//-------------------------------------
//-------------------------------------
function GetBYTEColor(const Coul: TColor): TColor3b;
begin
  Result.R:=GetRValue(Coul);
  Result.G:=GetGValue(Coul);
  Result.B:=GetBValue(Coul);
end;
//----------------------------------------------------------------
// dégradé de couleurs
function GetColorDegrade(const z: Double;
                         const zmin, zmax: Double;
                         const Coul1, Coul2: TColor): TColor;
var
  D: Double;
  H: Double;
  C1, C2, C: TColor3b;
  //DC       : TColor3b;
  DR, DG, DB : SmallInt;
begin
  D:=zmax-zmin;
  if Abs(D)<1e-8 then
  begin
    Result:=Coul1;
    Exit;
  end;
  H:=(z-zmin)/D;


  c1:=GetBYTEColor(Coul1);
  c2:=GetBYTEColor(Coul2);
  DR:=C2.R-C1.R;
  DG:=C2.G-C1.G;
  DB:=C2.B-C1.B;

  C.R:=Round(C1.R + H * DR);
  C.G:=Round(C1.G + H * DG);
  C.B:=Round(C1.B + H * DB);

  Result:=RGB(C.R, C.G, C.B);
end;
//-------------------------------------------------------
//Convertit une couleur en niveaux de gris; méthode NTSC.
function GetNTSCGrayScale(const C: TColor): Byte;
var
  cl: TColor3b;
begin
  cl:=GetBYTEColor(C);
  Result:=round(0.30 * cl.R + 0.59 * cl.G + 0.11 * cl.B);
end;



//-------------------------------------
function RGB(const R, G, B: byte): integer;
begin
  Result:=(R or (G shl 8) or (B shl 16));
end;
function GetRValue(const Coul: TColor): Byte;
begin
  Result:= Coul;
end;
function GetGValue(const Coul: TColor): Byte;
begin
  Result:= Coul shr 8;
end;
function GetBValue(const Coul: TColor): Byte;
begin
  Result:= Coul shr 16;
end;

function RGB2Acad(const C: TColor): Byte;
var drmin,dgmin,dbmin,
    CouleurR,
    CouleurG,
    CouleurB,
    ACcolor,
    ACcolorR,
    ACcolorG,
    ACcolorB,
    dColor,
    res    : integer;
begin
    Result:=0;

    dRmin := 99999999;
    dGmin := 99999999;
    dBmin := 99999999;

    CouleurR := GetRValue(C);
    CouleurG := GetGValue(C);
    CouleurB := GetBValue(C);
    for res := 1 to 255 do begin
        ACcolor := Acad2RGB(res);
        ACcolorR := GetRValue(ACcolor);
        ACcolorG := GetRValue(ACcolor);
        ACcolorB := GetBValue(ACcolor);
        dColor := abs(ACcolorR-CouleurR)+
                  abs(ACcolorG-CouleurG)+
                  abs(ACcolorB-CouleurB);
        if dColor<dRmin then begin
            dRmin := dColor;
            result := res;
        end;
    end;
end;

function GetSVGColor(const C: TColor): string;
var
  R,G,B: Double;
begin
  R:=100.0*GetFloatRValue(C);
  G:=100.0*GetFloatGValue(C);
  B:=100.0*GetFloatBValue(C);
  //Result:=Format('#%X%X%X',[R,G,B]);
  Result:=Format(' rgb(%.2f%%, %.2f%%, %.2f%%)',
                 [R,G,B]);

end;

function Acad2RGB(const n : integer) : tColor; // palette par défaut d'Autocad
var r,g,b,
    c,d,u : integer;
    C1 : Tcolor;  // RGB(r, g ,b)
    const StandCol : array[0..9] of tcolor =
        (clBlack ,clRed,$0001E1F3{clYellow},$0000C800{clLime},$00FDE302{clAqua},
         clBlue,clFuchsia,clBlack,clGray,clLtGray);
    const FullPalete : array[0..23] of tcolor =
        ($0000FF,$0040FF,$0080FF,$00C0FF,
         $00FFFF,$00FFC0,$00FF80,$00FF40,
         $00FF00,$40FF00,$80FF00,$C0FF00,
         $FFFF00,$FFC000,$FF8000,$FF4000,
         $FF0000,$FF0040,$FF0080,$FF00C0,
         $FF00FF,$C000FF,$8000FF,$4000FF);//..$0000FF (retour au début)
begin
    c := n mod 256; // au cas ou ?
    if c<10 then C1 := StandCol[c]
    else begin
        d := ((c-10) div 10);// 0..23
        // Couleur de base à corriger
        C1 :=FullPalete[d mod 24];
        // Correction:--------------------------------
        d := c div 10; // dizaines
        u := c-d*10; // unités
        // séparation des couleurs RGB
        b := (C1 and $FF0000) shr 16;
        g := (C1 and $00FF00) shr 8;
        r := (C1 and $0000FF);
        //Plus clair pour les impairs
        if ((u div 2)*2<>u) then begin
            b := b + ((255-b) div 2);
            g := g + ((255-g) div 2);
            r := r + ((255-r) div 2);
        end;
        // Plus foncé si u grand
        b := b*4 div (u+4);
        g := g*4 div (u+4);
        r := r*4 div (u+4);
        // Couleur corrigée:---------------------------
        C1 := RGB(r,g,b);

    end;
    result := C1;
end;
// Retourne sous forme de couleur OpenGL la couleur passée en argument
function GetGLColor(const Coul: TColor; const Alpha: Double): TGLColor;
const
  m = 1/256;
begin
  Result.R:=GetRValue(Coul)* m;
  Result.G:=GetGValue(Coul)* m;
  Result.B:=GetBValue(Coul)* m;
  Result.A:=Alpha;
end;

// Retourne sous forme de couleur Pascal la couleur OpenGL passée en argument
function GetPASColor(const Coul: TGLColor): TColor;
const
  m = 256;

begin
  Result := RGB(Round(Coul.R * m),
                Round(Coul.G * m),
                Round(Coul.B * m));
end;


//-------------------------------------
function IsInRange(const Value: Extended;
                   const MinValue, MaxValue: Extended): Boolean; overload;
begin
  Result := ((Value >= MinValue) and
             (Value <= MaxValue));
end;
function IsInRange(const Value: integer;
                   const MinValue, MaxValue: integer): Boolean; overload;
begin
  Result := ((Value >= MinValue) and
             (Value <= MaxValue));
end;


//-------------------------------------
function IIF(const Condition: boolean; const V1, V2: integer): integer; overload;
begin
  if Condition then Result:=V1 else Result:=V2;
end;
function IIF(const Condition: boolean; const V1, V2: Extended): Extended; overload;
begin
  if Condition then Result:=V1 else Result:=V2;
end;
function IIF(const Condition: boolean; const V1, V2: boolean): boolean; overload;
begin
  if Condition then Result:=V1 else Result:=V2;
end;
function IIF(const Condition: boolean; const V1, V2: String): String; overload;
begin
  if Condition then Result:=V1 else Result:=V2;
end;

// dessiner un triangle dégradé
procedure DrawTriangleWithDegrade(const V1, V2, V3: TPoint3DF;
                                  const C1, C2, C3: TColor);

begin
;
end;
// construire un nom de fichier valide en virant les accents et caractères spéciaux
function EnsureMakeFilename(const S: string): string;
var
  Lin: String;
  L: Integer;
  procedure RemplacerCar(const Caracts: string);
  var
    w: Integer;
    C: Integer;
    QC: Char;
    Nb: Integer;
  begin
    Nb := Length(Caracts);
    for C := 1 to Nb do
    begin
      QC := Caracts[C];
      for w := 1 to L do if (Lin[w] = QC) then Lin[w] := '_';
    end;
  end;
begin
  Lin := Trim(PurgerAccents(S)); //
  L   := Length(Lin);
  if (L = 0) then begin Result :=''; Exit; end;
  // on déquille certains caractères spéciaux en remplaçant par des '_'
  RemplacerCar('^& #\"''(=+*/<>)[:,;!.%?]{~|`@}$');
  Result := Lin;
end;

// Purge les accents d'un texte
function PurgerAccents(const S: string): string;
var
  w1 : integer;
  L  : integer;
  Lin: string;
begin
  Lin := S;
  L   := Length(Lin);
  if L = 0 then begin Result :=''; Exit; end;
  // convertir accentués
    for w1:=0 to L do if Lin[w1]=#136 then Lin[w1]:='à';
    for w1:=0 to L do if Lin[w1]=#142 then Lin[w1]:='é';
    for w1:=0 to L do if Lin[w1]=#143 then Lin[w1]:='è';
    for w1:=0 to L do if Lin[w1]=#148 then Lin[w1]:='î';
    for w1:=0 to L do if Lin[w1]=#137 then Lin[w1]:='â';
    for w1:=0 to L do if Lin[w1]=#144 then Lin[w1]:='ê';
    for w1:=0 to L do if Lin[w1]=#144 then Lin[w1]:='ë';
    for w1:=0 to L do if Lin[w1]=#141 then Lin[w1]:='ç';
    //*)
    // virer les accents, ce qui pose bcp de problèmes
    // avec la LCL de WinCE
    for w1:=0 to L do if Lin[w1]='é'  then Lin[w1]:='e';
    for w1:=0 to L do if Lin[w1]='è'  then Lin[w1]:='e';
    for w1:=0 to L do if Lin[w1]='ê'  then Lin[w1]:='e';
    for w1:=0 to L do if Lin[w1]='ë'  then Lin[w1]:='e';

    for w1:=0 to L do if Lin[w1]='à'  then Lin[w1]:='a';
    for w1:=0 to L do if Lin[w1]='â'  then Lin[w1]:='a';
    for w1:=0 to L do if Lin[w1]='ä'  then Lin[w1]:='a';

    for w1:=0 to L do if Lin[w1]='ï'  then Lin[w1]:='i';
    for w1:=0 to L do if Lin[w1]='î'  then Lin[w1]:='i';

    for w1:=0 to L do if Lin[w1]='ô'  then Lin[w1]:='o';
    for w1:=0 to L do if Lin[w1]='ö'  then Lin[w1]:='o';

    for w1:=0 to L do if Lin[w1]='û'  then Lin[w1]:='u';
    for w1:=0 to L do if Lin[w1]='ü'  then Lin[w1]:='u';
    for w1:=0 to L do if Lin[w1]='ù'  then Lin[w1]:='u';
  Result := Lin;
end;

// Convertir un fichier texte vers le format désiré
// cette variante utilise un TStringList
function ConvertTextFile(const InputFileName, OutputFilename: string;
                         const InputFormat, OutputFormat: TTextFileFormat): boolean;
var

  FO  : TextFile;
  ENDL: string;   // fin de ligne
  ALine: string;
  i: integer;
begin
  Result:=False;
  if Not(FileExists(InputFileName)) then Exit;
  case OutputFormat of
    tfWINDOWS: ENDL:=#13+#10;
    tfUNIX   : ENDL:=#10;
    tfMAC    : ENDL:=#13;
  end;
  with TStringList.Create do
  begin
  try
    Clear;
    LoadFromFile(InputFileName);
    AssignFile(FO, OutputFilename);
    ReWrite(FO);
    try
      try
        for i:=0 to Count-1 do begin
          //ALine:=Trim(Strings[i])+ENDL;
          ALine := Strings[i]+ENDL; // ne PAS purger la chaîne !!!
          Write(FO, ALine);
        end;
        Result:=True;
      except
      end;
    finally
      CloseFile(FO);
    end;
  finally // with TStringList
    Clear;
    Free;
  end;
  end;
end;
// troncature sécurisée d'un texte à la longueur L
function SafeTruncateString(const Str: string; const L: integer): String;
begin
 if Length(Str)>L then
   Result:=Trim(Copy(Str, 1, L))
 else
   Result:=Trim(Str);
end;

function GetAnneeISO(const QAnnee: word): word;
const
  PIVOT = 50; // année 1950 = pivot pour les années sur deux chiffres
begin
  Result := QAnnee;
  if (QAnnee >  1950) then Exit; // année sur quatre chiffres -->[]
  if (QAnnee <= PIVOT) then Result := 2000 + QAnnee
                      else Result := 1900 + QAnnee;

end;
// déclinaison magnétique en mode TOPOROBOT (en grades, voir aussi pb de signe)
function GetTOPOROBOTDecliMag(const DecInDegrees: double): double;
begin
  Result := degtograd(DecInDegrees);
end;

function GetTypeDeVisee(const T: integer): TTypeDeVisee;
begin
  try
    Result := TTypeDeVisee(T);
  except
    Result := tgDEFAULT;
  end;
end;

// fonction d'incrémentation lexicographique
// Si le string ne comporte que des lettres, on ajoute un indice à la fin
// Si le string comporte un préfixe et un indice, on incrémente l'indice
function IncrementString(const S0: string): string;
var
  i,q: integer;
  Groupe: string;
  Prefix: string;
  Reste : string;
  Index : integer;
  procedure DecomposeLitteral;
  const
    Seps =':./-_,;';
  var
    a,b: integer;

  begin
    Groupe:='';
    for a:=1 to length(Seps) do begin
      b:=Pos(Seps[a], S0);
      if b>0 then begin // si le séparateur est trouvé, on sort le résultat
        Groupe:=Copy(S0, 1, b-1)+Seps[a];
        Break;
      end;
    end;
    if Groupe='' then
      Reste:=S0
    else
      Reste:=Copy(S0, b+1, Length(S0));
  end;
begin
  // Si chaine vide, on renvoie rien
  if S0='' then begin Result:=''; exit; end;
  // rechercher un groupe (série, etc ...)
  // séparé par un ":", ".", "/", "-", "_"
  DecomposeLitteral;
  // rechercher un chiffre
  Q:=-1;
  for i:=1 to Length(Reste) do begin
    if IsInRange(Ord(Reste[i]), Ord('0'), Ord('9')+1) then begin
      Q:=i;
      Break;
    end;
  end;
  // Si chiffre trouvé:
  if Q>0 then begin
    Prefix:=Copy(Reste, 1, Q-1);
    Index :=StrToIntDef(Copy(Reste, Q, Length(Reste)-Q+1),0);

  end else begin
    Prefix:=Trim(Reste);
    Index:=0;
  end;
  AfficherMessage(Format('IncrementString: G=%s S=%s - P=%s - I=%d',[Groupe, Reste, Prefix, Index]));
  Result:=Format('%s%s%d',[Groupe, Prefix, Index+1]);
end;


// formater un filtre de fichier
// Arguments pairs du tableau: Nom du filtre
// Arguments impairs: Filtre
function FormatFileFilters(const FF: array of string; const WithAll: boolean): string;
var
  i: integer;
begin
  Result:='';
  for i:=0 to High(FF) div 2 do
    Result:=Result + Format('%s (%s)|%s|',[FF[i shl 1], FF[1+i shl 1], FF[1+i shl 1]]);
  // supprimer le 'pipe' de fin
  System.Delete(Result, Length(Result),1);
  if WithAll then
    {$IFDEF FRENCH_MESSAGES}  Result:=Result+'|Tous (*.*)|*.*'; {$ENDIF}
    {$IFDEF ENGLISH_MESSAGES} Result:=Result+'|All (*.*)|*.*'; {$ENDIF}
    {$IFDEF SPANISH_MESSAGES} Result:=Result+'|Todos (*.*)|*.*'; {$ENDIF}
end;

// Suggestion d'une équidistance en fonction de l'etendue totale du réseau
function ProposerEquidistanceDef(const C1, C2: TPoint3Df; const Defaut: double): Double;
var
  d: double;
begin
  Result:=Defaut;
  try

    d:=Hypot3D(C2.X - C1.X,
               C2.Y - C1.Y,
               C2.Z - C1.Z);
    d:=d/10;
    Result:=d;
    if IsInRange(d,    0.00,   10.00) then Result:=10.00;
    if IsInRange(d,   10.00,   25.00) then Result:=25.00;
    if IsInRange(d,   25.00,   50.00) then Result:=50.00;
    if IsInRange(d,   50.00,  100.00) then Result:=100.00;
    if IsInRange(d,  100.00,  200.00) then Result:=200.00;
    if IsInRange(d,  200.00,  250.00) then Result:=250.00;
    if IsInRange(d,  250.00,  500.00) then Result:=500.00;
    if IsInRange(d,  500.00, 1000.00) then Result:=1000.00;
    if IsInRange(d, 1000.00, 2000.00) then Result:=2000.00;
    if IsInRange(d, 2000.00, 5000.00) then Result:=5000.00;
    if IsInRange(d, 5000.00,10000.00) then Result:=10000.00;
    if IsInRange(d,10000.00,20000.00) then Result:=20000.00;
    // protection 'anti-nul'
    if (Result < 5.00) then Result := 5.00;
  except
    Result:=Defaut;
  end;
end;


//******************************************************************************

// test d'intersection de deux rectangles
{

   |---------------------------------------------------------------|
   |                                    |-------------------------||
   |                                    |                         ||
   |                                    |                         ||
   ||---------------------------------------|                     ||
   ||                                   |---|---------------------||
   ||                                       |                      |
   ||                                       |                      |
   ||                                       |                      |
   ||                                       |                      |
   ||---------------------------------------|                      |
   |---------------------------------------------------------------|


}
function IntersectRectangles(const R1, R2: TRect2Df): boolean;
var
  BoundingBox: TRect2Df;  // boite englobante
  LB, HB     : double;    // taille de la boite englobante
  LM, HM     : double;    // taille du plus petit rectangle contenant
                          // les deux rectangles R1 et R2 en diagonale

begin
  Result:=False;
  BoundingBox.X1 := Min(R1.X1, R2.X1);
  BoundingBox.Y1 := Min(R1.Y1, R2.Y1);

  BoundingBox.X2 := Max(R1.X2, R2.X2);
  BoundingBox.Y2 := Max(R1.Y2, R2.Y2);

  LB:= BoundingBox.X2 - BoundingBox.X1;
  HB:= BoundingBox.Y2 - BoundingBox.Y1;

  LM:=(R1.X2 - R1.X1) + (R2.X2 - R2.X1);
  HM:=(R1.Y2 - R1.Y1) + (R2.Y2 - R2.Y1);

  Result:= (LM>LB) and (HM>HB);
end;


// obtenir un tableau 2D à partir d'une string
function GetStringArray2D(const S: string; const SepCol: string): TStringArray2D;
var
  i,j: integer;
  P : integer;
  S1: string;
  MesLignes: TStringArray;
begin
  //NbLignes := 0;
  // pour ne pas être perturbé par les débuts de ligne
  P := Pos(SepCol,S);
  if (P>0) then S1 := '$' + SepCol + S + SepCol+'$';

  
  S1 := Trim(S);
  // purger
  for i:=0 to MAX_SIZE_PARAM_ARRAY do
    for j:=0 to MAX_SIZE_PARAM_ARRAY do
      Result[i,j] :='';

  // virer les retour-chariots
  P:=0;
  P:=Pos(#13, S1);
  while (P>0) do begin
    System.Delete(S1, P, 1);
    P:=Pos(#13, S1);
  end;

  MesLignes := Split(S, #10);
  for i:=0 to MAX_SIZE_PARAM_ARRAY do
    AfficherMessage(Format('>>GetStringArray2D: MesLignes[%d] = %s',[i, MesLignes[i]]));
  // découper les lignes
  for i:=0 to MAX_SIZE_PARAM_ARRAY do
    Result[i] := Split(MesLignes[i], SepCol);
  // contrôle:
  (*
  for i:=0 to MAX_SIZE_PARAM_ARRAY do
    for j:= 0 to MAX_SIZE_PARAM_ARRAY do
      Result[i][j]
  //*)
end;


// encodage/Décodage sécurisé des dates
function  SafeEncodeDate(Year, Month, Day: Word): TDateTime;
begin
  if (Year < 100) then  begin
    if (Year < 50) then
      Year := 2000 + Year
    else
      Year := 1900 + Year;
  end;
  Result := EncodeDate(Year, Month, Day);
end;
procedure SafeDecodeDate(Date: TDateTime; var Year, Month, Day: Word);
begin
  DecodeDate(Date, Year, Month, Day);
end;



function DescribeEPSGSystem(const CodeEPSG: integer): string;
var
  FuseauUTM: integer;
begin
  if      (CodeEPSG = 0)     then    Result := 'Local UCS'
  else if (CodeEPSG = 2154)  then    Result := 'Lambert 93 France'
  else if (CodeEPSG = 27561) then    Result := 'NTF (Paris) / Lambert North France'
  else if (CodeEPSG = 27562) then    Result := 'NTF (Paris) / Lambert Centre France'
  else if (CodeEPSG = 27563) then    Result := 'NTF (Paris) / Lambert South France'
  else if (CodeEPSG = 27564) then    Result := 'NTF (Paris) / Lambert Corsica'
  else if (CodeEPSG = 27571) then    Result := 'NTF (Paris) / Lambert zone I'
  else if (CodeEPSG = 27572) then    Result := 'NTF (Paris) / Lambert zone II'
  else if (CodeEPSG = 27573) then    Result := 'NTF (Paris) / Lambert zone III'
  else if (CodeEPSG = 27574) then    Result := 'NTF (Paris) / Lambert zone IV'
  else if (CodeEPSG = 379009) then   Result := 'Google Projection'
  // UTM
  else if (Trunc(CodeEPSG / 100) = 326) then
  begin
    FuseauUTM := CodeEPSG MOD 100;
    Result := Format('UTM zone %d', [FuseauUTM]);
  end
  else Result := 'Unknown projection';

end;

//-----------------
function GetIDStation(const N: integer; const St: TStation): string;
var
  miaou: string;
begin
  miaou:=Trim(St.IDTerrainStation);
  if miaou = '' then
    Result:= Format(FMTSERST,[N, St.NumPoint])
  else
    Result:=miaou;
end;

//*************
// calcul localisé de coordonnées
// pour une seule paire de coordonnées
function ConversionCoordonneesIsolees(const Src, Tgt: string; const QX, QY: double; out PointConverted: TPoint2Df): boolean;
var
  WU, EWE: TProjUV;
begin
  Result := False;
  with TConversionSysteme.Create do
  begin
    try
      if (Initialiser) then
      begin
        WU.U := QX;
        WU.V := QY;
        // TODO: ConversionCoordonneesIsolees
        //EWE := Conversion_EPSG_To_EPSG(Src, Tgt, WU);
        PointConverted.X := EWE.U;
        PointConverted.Y := EWE.V;
      end;
      Finaliser;
      Result := True;
    finally
      Free;
    end;
  end;
  //*)
end;


// pour un tableau de coordonnées
function ConversionTableauCoordonneesIsolees(const Src, Tgt: string; const ArrCoordsInput: TArrayPoints2Df; out ArrPointsConverted: TArrayPoints2Df): boolean;
var
  i, Nb: integer;
  WU, EWE: TProjUV;
begin
  Result := False;
  Nb := High(ArrCoordsInput) + 1;
  if (Nb < 0) then Exit;
  SetLength(ArrPointsConverted, Nb);
  with TConversionSysteme.Create do
  begin
    try
      if (Initialiser) then
      begin
        for i := 0 to Nb - 1 do
        begin
          WU.U := ArrCoordsInput[i].X;
          WU.V := ArrCoordsInput[i].Y;
          EWE := ConversionSyst1ToSyst2(Src, Tgt, WU);
          ArrPointsConverted[i].X := EWE.U;
          ArrPointsConverted[i].Y := EWE.V;
        end;
      end;
      Finaliser;
      Result := True;
    finally
      Free;
    end;
  end;
end;

// version du XML et encodage
function FormatterVersionEncodageXML(const version: string; const encodage: string): string; inline;
begin
  Result := Format('<?xml version="%s" encoding="%s"?>', [version, encodage]);
end;




//******************************************************************************
// copie et sauvegarde de tableaux
//******************************************************************************
{$IFDEF GRIDS_SRC_MODIFIED}
  function GRDCopierUnTableau(const Grille: TStringGrid): boolean;
  begin
    Result := Grille.CopyToClipboard;
  end;
  function GRDCollerDepuisClipBoard(const Grille: TStringGrid): boolean;
  begin
    Result := False;
    TODO: Implanter cette fonction
  end;

  function GRDSauverUnTableauEnCSV(const Grille: TStringGrid; const FichierCSV: string): boolean;
  begin
    Result := Grille.SaveToCSV(FichierCSV);
  end;
{$ELSE}
  function GRDCCopierUnTableau(const Grille: TStringGrid): boolean;
  var
    WU: String;
    l, c: Integer;
    ClipBoard: TClipboard;
  begin
    WU := '';
    ClipBoard := TClipboard.Create(ctClipboard);
    try
      ClipBoard.Clear;
      // contenu
      for l := 0 to Grille.RowCount - 1 do
      begin
        for c := 0 to Grille.ColCount - 1 do
        begin
          WU := WU + Grille.Cells[c, l] + #9;
        end;
        WU := WU + #13#10;
      end;
      ClipBoard.AsText := WU;

    finally
      ClipBoard.Free;
    end;
  end;

  function GRDCollerDepuisClipBoard(const Grille: TStringGrid; const HasTitres: boolean): boolean;
  var
    ClipBoard: TClipboard;
    LesLignes: TStringList;
    EWE: String;
    P: SizeInt;
    Lin : String;
    i, j: Integer;
    ValeursColonnes: TStringArray;
    R: Integer;
  begin
    Result := False;
    ClipBoard := TClipboard.Create;
    LesLignes := TStringList.Create;
    try
      LesLignes.Clear;
      try
        EWE := Trim(ClipBoard.AsText) + #13+#10;
        // découpage du texte
        repeat
          P := Pos(#10, EWE);
          Lin := Trim(Copy(EWE, 0, P-1));
          EWE := Copy(EWE, P+1, Length(EWE));
          //AfficherMessage('--:' + Lin + ': Reste: '+ EWE);
          // on zappe les lignes vides
          if (Lin <> '') then LesLignes.Add(Lin);

        until (P = 0);
        // traitement des lignes
      if (LesLignes.Count = 0) then Exit;
      Grille.ColCount := 1 + MAX_SIZE_PARAM_ARRAY + 2; // nb de colonnes = taille d'un TStringArray

      // on n'implémente pas un tableur => grille brute
      Grille.FixedCols   := 1;
      Grille.FixedRows   := 1;
      // remplissage des colonnes par défaut
      for j := 1 to Grille.ColCount - 1 do Grille.Cells[j, 0] := inttostr(j);
      if (HasTitres) then
      begin
        Grille.RowCount := LesLignes.Count;
      end
      else
      begin
        Grille.RowCount := LesLignes.Count + 1;
      end;

      // numéros de lignes avec un traitement d'erreur intégré
      try
        for i := 1 to Grille.RowCount - 1 do Grille.Cells[0, i] := inttostr(i);
      except
        ;
      end;
      for i:= 0 to LesLignes.Count -1 do
      begin
        AfficherMessage(Format(' %d: %s', [i, LesLignes[i]]));
        ValeursColonnes := Split(LesLignes[i], #9);
        for j := 0 to High(ValeursColonnes) do
        begin
          if (HasTitres) then R := i else R := i+1;
          Grille.Cells[j+1, R] := ValeursColonnes[j];
        end;
      end;
    except
    end;
    LesLignes.Clear;
  finally
    LesLignes.Free;
    ClipBoard.Free;
  end;






  end;

  function GRDSauverUnTableauEnCSV(const Grille: TStringGrid; const FichierCSV: string): boolean;
  var
   fp: TextFile;
   WU: String;
   l, c: Integer;
  begin
   Result := False;
   AssignFile(fp, FichierCSV);
   try
     Rewrite(fp);
     // contenu
     for l := 0 to Grille.RowCount - 1 do
     begin
       WU := '';
       for c := 0 to Grille.ColCount - 1 do
       begin
         WU := WU + Grille.Cells[c, l] + #9;
       end;
       WriteLn(fp, Trim(WU));
     end;
     Result := True;
   finally
     Closefile(fp)
   end;
  end;
  // Dates SQL
  function DatePascalToDateSQL(const QDate: TDateTime): string;
  var
    YYYY, MM, DD: word;
  begin
    DecodeDate(QDate, YYYY, MM, DD);
    Result := Format('%.4d-%.2d-%.2d', [YYYY, MM, DD]);
  end;

  function DateSQLToDatePascal(const QDateSQL: string): TDateTime;
  var
    YYYY, MM, DD: word;
  begin
    try
      YYYY := StrToInt(Copy(QDateSQL, 1, 4));
      // années sur deux chiffres
      if (YYYY < 100) then
      begin
        if (YYYY < 50) then  YYYY := 2000 + YYYY
                       else  YYYY := 1900 + YYYY;
      end;
      MM   := StrToInt(Copy(QDateSQL, 6, 2));
      DD   := StrToInt(Copy(QDateSQL, 9, 2));
      Result := EncodeDate(YYYY, MM, DD);
    except
      Result := TDateTime(0.0);
    end;
  end;

{$ENDIF}

end.
