unit SVGCanvasUnit;
interface
uses
  SysUtils,
  Classes,
  Common,
  Graphics;
type TSVGPoint2Df = record
  X: double;
  Y: double;
end;

type TFontSVGProperties = record
  Name  : string;
  Size  : integer;
  Height: integer;
  Color : TColor;
  Style : TFontStyles;
end;
type TPenSVGProperties = record
  Name    : string;
  Color   : TColor;
  fWidth  : double;
  nWidth  : integer;
  Descro: String  ;
end;
type TBrushSVGProperties = record
  Color   : TColor;
  Alpha   : integer;
  Descro: String;
end;

type

{ TSVGCanvas }

TSVGCanvas = class
    constructor Create;
    destructor  Destroy; override;
  private
    FpSVGFILE: TextFile;
    FNbWrittenLines: integer;
    FScale: double;
    FFont : TFontSVGProperties;
    FPen  : TPenSVGProperties;
    FBrush: TBrushSVGProperties;
    FXMin,
    FXMax,
    FYMin,
    FYMax : Double;

    // définition des couleurs et fontes
    procedure SetPen(const Value: TPenSVGProperties);
    function  GetPen: TPenSVGProperties;
    procedure SetBrush(const Value: TBrushSVGProperties);
    function  GetBrush: TBrushSVGProperties;
    procedure SetFont(const Value: TFontSVGProperties);
    procedure SetDefaultPen(const Value: TPenSVGProperties);
    procedure SetDefaultBrush(const Value: TBrushSVGProperties);
    procedure WriteLine(const S: string);

  public
    function  InitializeDocument(const FileName, Commentaire: string): boolean;
    procedure FinalizeDocument;
    // limites du dessin
    procedure SetDrawingBounds(const X1, Y1, X2, Y2: Double);
    // échelle
    procedure SetScale(const E: double);
    //****************************
    // définition de commentaires
    procedure WriteCommentaire(const s: string);
    procedure WriteCommand(const s: string);
    // définition de couches
    procedure BeginLayer(const LayerName: string);
    procedure EndLayer(const LayerName: string);
    // routines de dessin
    procedure MoveTo(const X,Y: Double);
    procedure LineTo(const X,Y: Double);
    procedure DrawPoint(const X,Y: Double);
    procedure DrawLine(const X1, Y1, X2, Y2: Double);
    procedure DrawCircle(const XC, YC, R: double);
    procedure DrawPolylign(const Points: array of TSVGPoint2Df);
    procedure DrawPolygon(const Points: array of TSVGPoint2Df);
    procedure DrawBorderedPolygon(const Points: array of TSVGPoint2Df);
    procedure TextOut(const X,Y: Double; const Text: string);
    function  SVGColor(const C:TColor):string;
    //*)


end;

implementation


procedure DisplayMsg(const Str: string);
begin
  ;//WriteLn(Str);
end;


constructor TSVGCanvas.Create;
begin
 inherited Create;
 DisplayMsg(Format('%s.Create',[ClassName]));
 try

 except
 end;
end;
destructor TSVGCanvas.Destroy;
begin
 DisplayMsg(Format('%s.Free',[ClassName]));
 inherited Destroy;
end;
procedure TSVGCanvas.WriteLine(const S: string);
begin
  WriteLn(FpSVGFILE, s);
end;
//**** Définition des pinceaux et couleurs
function  TSVGCanvas.SVGColor(const C:TColor):string;
var
  r,g,b: byte;
begin
  R:=GetRValue(C); G:=GetGValue(C); B:=GetBValue(C);
  Result:=Format('#%.2X%.2X%.2X',[R,G,B]);
end;

function TSVGCanvas.GetPen: TPenSVGProperties;
begin
  Result:=FPen;
end;
procedure TSVGCanvas.SetPen(const Value: TPenSVGProperties);
begin
  FPen:=Value;
  FPen.Descro := Format('stroke: %s; stroke-width: %d;',[SVGColor(FPen.Color), round(FPen.fWidth)]);
end;
procedure TSVGCanvas.SetBrush(const Value: TBrushSVGProperties);
begin
  FBrush:=Value;
  FBrush.Descro:=Format('fill="%s";',[SVGColor(FBrush.Color)]);
  // if >sans remplissage> then FBrushStr:='fill="none"';
end;
function  TSVGCanvas.GetBrush: TBrushSVGProperties;
begin
  Result:=FBrush;
end;

procedure TSVGCanvas.SetFont(const Value: TFontSVGProperties);
begin
  ;
end;
procedure TSVGCanvas.SetDefaultPen(const Value: TPenSVGProperties);
begin
;
end;
procedure TSVGCanvas.SetDefaultBrush(const Value: TBrushSVGProperties);
begin
;
end;
//*******************
// échelle
procedure TSVGCanvas.SetScale(const E: double);
begin
  FScale := E;
end;

procedure TSVGCanvas.SetDrawingBounds(const X1, Y1, X2, Y2: Double);
begin
  FXMin:=X1;
  FYMin:=Y1;
  FXMax:=X2;
  FYMax:=Y2;
end;
procedure TSVGCanvas.WriteCommentaire(const s: string);
begin
  WriteLine(Format('<!-- %s -->',[s]));
end;
procedure TSVGCanvas.WriteCommand(const s: string);
begin
  WriteLine(Trim(S));
end;

function  TSVGCanvas.InitializeDocument(const FileName, Commentaire: string): boolean;
begin
  Result:=False;
  FNbWrittenLines:=0;
  assignFile(FpSVGFILE, FileName);
  try
    ReWrite(FpSVGFILE);
    //-------------------------------
    // écriture de l'en tête ici
    // ------------------------------
    WriteLine('<?xml version ="1.0"?>');
    WriteLine('<!-- PostScript File generated by GHTopo');
    WriteLine(Format(' File    : %s',[FileName]));
    WriteLine(Format(' Date    : %s',[DateToStr(Now)]));
    WriteLine(Format(' Comments: %s',[Commentaire]));
    WriteLine('-->');


    // le dessin
    WriteLine(Format('<svg width="%f" height="%f" viewbox="%f %f %f %f">',
                     [FXMax - FXMin,
                      FYMax - FYMin,
                      FXMin, FYMin, FXMax, FYMax]));


    Result:=True;
  except
    DisplayMsg('Error initializing text file');
    CloseFile(FpSVGFILE);
  end;
end;
procedure TSVGCanvas.FinalizeDocument;
begin
  try
    WriteLine('</svg>');
  finally
    CloseFile(FpSVGFILE);
  end;
end;
//********* Définition de couches
procedure TSVGCanvas.BeginLayer(const LayerName: string);
begin
  WriteLine(Format('<!-- Begin Layer: %s -->',[LayerName]));
  WriteLine(Format('<g id="%s">',[LayerName]));
end;
procedure TSVGCanvas.EndLayer(const LayerName: string);
begin
  WriteLine('</g>');
  WriteLine(Format('<!-- End Layer: %s -->',[LayerName]));
end;
//********* routines de dessin
procedure TSVGCanvas.MoveTo(const X,Y: Double);
begin
  ;
end;

procedure TSVGCanvas.LineTo(const X,Y: Double);  (* peu utilisé *)
begin
  ;
end;

procedure TSVGCanvas.DrawPoint(const X,Y: Double);
begin
 ;
end;
procedure TSVGCanvas.DrawCircle(const XC, YC, R: double);
begin
;
end;
procedure TSVGCanvas.DrawLine(const X1, Y1, X2, Y2: Double);
begin

  WriteLine(Format('   <line x1="%f" y1="%f" x2="%f" y2="%f" style="%s"/>',
                   [X1, -Y1,
                    X2, -Y2,
                    FPen.Descro]));
end;
// dessin d'une polyligne
procedure TSVGCanvas.DrawPolylign(const Points: array of TSVGPoint2Df);
var
  i: integer;
begin
  if (High(Points)<1) then Exit;
  (*
  for i:=1 to High(Points) do
  //*)
end;
// dessin d'un polygone plein sans bordure
procedure TSVGCanvas.DrawPolygon(const Points: array of TSVGPoint2Df);
var
  i: integer;
begin
  if (High(Points)<1) then Exit;
  //WriteLine('newpath');

  WriteLine(Format('   <polygon style="fill-rule: nonzero; fill: %s; stroke: none"',
                   [SVGColor(FBrush.Color)]));
  WriteLine(Format('      points="%f, %f ',[Points[0].X, -Points[0].Y]));

  for i:=1 to High(Points) do
    WriteLine(Format('              %f, %f ',[Points[i].X, -Points[i].Y]));
  WriteLine('   "/>');

end;
// dessin d'un polygone plein avec bordure
procedure TSVGCanvas.DrawBorderedPolygon(const Points: array of TSVGPoint2Df);
var
  i: integer;
begin
  if (High(Points)<1) then Exit;

end;
procedure TSVGCanvas.TextOut(const X,Y: Double; const Text: string);
begin
  WriteLine(Format('<text x="%f" y="%f">%s</text>',[X, -Y,Text]));
end;
end.
