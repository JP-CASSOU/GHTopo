unit CadreSelectThetaPhi;

{$INCLUDE CompilationParameters.inc}

interface

uses
  StructuresDonnees,
  Common,
  Classes, SysUtils, FileUtil, Forms, Controls,
  Graphics, ExtCtrls, StdCtrls;

type

  { TCdrTriedreThetaPhi }

  TCdrTriedreThetaPhi = class(TFrame)
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    Panel1: TPanel;
    Vue: TPaintBox;
    procedure Button1Click(Sender: TObject);
    procedure FrameMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Panel1Exit(Sender: TObject);
    procedure Panel1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure VueClick(Sender: TObject);
    procedure VueMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure VueMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure VuePaint(Sender: TObject);
    function  CalcThetaPhi(const QX, QY: integer): TPoint2Df;
  private
    FCurrentMousePos: TPoint;
    FCanDraw: boolean;
    FTheta, FPhi: double;
    // paramètres de la matrice de transformation
    Aux: array[1..8] of Double;

    FProcRefreshXY: TProcedureOfObject;
    FProcDoAction: TProcedureOfObject;

    procedure DessinTriedre;
  public
    { public declarations }
    procedure SetProcRefreshXY(const P: TProcedureOfObject);
    procedure SetProcDoAction(const P: TProcedureOfObject);
    procedure SetParamsTriedre(const Theta, Phi: double);

    function GetTheta: double;
    function GetPhi: double;

  end;

implementation

{$R *.lfm}




{ TCdrTriedreThetaPhi }

procedure TCdrTriedreThetaPhi.Button1Click(Sender: TObject);
begin
  Vue.Invalidate;
end;

procedure TCdrTriedreThetaPhi.FrameMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FCurrentMousePos.X := X;
  FCurrentMousePos.Y := Y;
end;

procedure TCdrTriedreThetaPhi.Panel1Exit(Sender: TObject);
begin

end;

procedure TCdrTriedreThetaPhi.Panel1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
end;
(* Rappel du fonctionnement de l'événement OnClick en Lazarus:

Cet événement ne s'exécute en fait que lorsque le bouton est relâché. Il
équivaut au OnMouseUp, non implémenté.
Mettre dans cet événement le code du OnMouseUp
//*)
procedure TCdrTriedreThetaPhi.VueClick(Sender: TObject);
begin
  //AfficherMessage('EWEjkdfhj');
  if (Assigned(FProcDoAction)) then FProcDoAction();
end;

procedure TCdrTriedreThetaPhi.VueMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);

var
  EWE: TPoint2Df;
begin
  if (Shift = [ssLeft]) then
  begin
    EWE := CalcThetaPhi(X, Y);
    SetParamsTriedre(EWE.X, EWE.Y);
    if (Assigned(FProcRefreshXY)) then FProcRefreshXY;
  end;
end;

procedure TCdrTriedreThetaPhi.VueMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
end;

procedure TCdrTriedreThetaPhi.VuePaint(Sender: TObject);
begin
  DessinTriedre;
end;

function TCdrTriedreThetaPhi.CalcThetaPhi(const QX, QY: integer): TPoint2Df;
const
  X1: double = -360.00; X2: double = 360.00;
  Y1: double =  -90.00; Y2: double = 90.00;
var
  Rx: double;
  Ry: double;

begin
  try
    Rx := (X2 - X1) / Vue.Width;
    Ry := (Y2 - Y1) / Vue.Height;
    Result.X := Rx * QX + X1;
    Result.Y := Ry * (Vue.Height - QY) + Y1;
    // contrôle des limites
    if (Result.X < X1) then Result.X := X1;
    if (Result.Y < Y1) then Result.Y := Y1;
    if (Result.X > X2) then Result.X := X2;
    if (Result.Y > Y2) then Result.Y := Y2;


    AfficherMessage(Format('CalcThetaPhi(%d, %d -> %f, %f)', [QX, QY, Result.X, Result.Y]));
    if (Assigned(FProcRefreshXY)) then FProcRefreshXY;
  except

  end;
end;

procedure TCdrTriedreThetaPhi.DessinTriedre;
var
  R: TRect;
  TmpBuffer: TBitmap;
  // dessin du référentiel
  procedure DrawReferentiel;
  const
    rSz = 40;
    rXo = 5 + rSz;


  var
    rYo       : integer;
    X1, X2, X3: TPoint3Df;
    R1, R2, R3: TPoint2Df;
    Rp        : TPoint;
  begin
    // calculs préliminaires
    X1.X  := 1.0; X1.Y  := 0.0; X1.Z  := 0.0; //X1.T  := 1.0;
    X2.X  := 0.0; X2.Y  := 1.0; X2.Z  := 0.0; //X2.T  := 1.0;
    X3.X  := 0.0; X3.Y  := 0.0; X3.Z  := 1.0; //X3.T  := 1.0;
    // transformations
    R1.X := -X1.X * Aux[1] +
             X1.Y * Aux[3];
    R1.Y := -X1.X * Aux[5] +
            -X1.Y * Aux[6] +
             X1.Z * Aux[4];
    R2.X := -X2.X * Aux[1] +
             X2.Y * Aux[3];
    R2.Y := -X2.X * Aux[5] +
            -X2.Y * Aux[6] +
             X2.Z * Aux[4];
    R3.X := -X3.X * Aux[1] +
             X3.Y * Aux[3];
    R3.Y := -X3.X * Aux[5] +
            -X3.Y * Aux[6] +
             X3.Z * Aux[4];
    with TmpBuffer.Canvas do begin
      rYo := Height - rSz shl 1;
      Brush.Color:=clWhite;
      Font.Color:=clBlue;

      Pen.Color:=clRed;
      MoveTo(rXo, rYo);

      Rp.x := rXo + round(rSz * R1.X);
      Rp.y := rYo - round(rSz * R1.Y);
      LineTo(Rp.X, Rp.Y);
      TextOut(Rp.x+2, Rp.y+2,'X');


      MoveTo(rXo, rYo);
      Rp.x := rXo + round(rSz * R2.X);
      Rp.y := rYo - round(rSz * R2.Y);
      LineTo(Rp.X, Rp.Y);
      TextOut(Rp.x+2, Rp.y+2,'Y');

      MoveTo(rXo, rYo);
      Rp.x := rXo + round(rSz * R3.X);
      Rp.y := rYo - round(rSz * R3.Y);
      LineTo(Rp.X, Rp.Y);
      TextOut(Rp.x+2, Rp.y+2,'Z');

    end;
  end;
begin
  if (not FCanDraw) then exit;
  TmpBuffer := TBitmap.Create;
  begin
    try
      TmpBuffer.Canvas.Brush.Color := clYellow;
      TmpBuffer.Canvas.Pen.Color := clRed;
      TmpBuffer.Height   := vue.Height;
      TmpBuffer.Width    := Vue.Width;
      R.Left   := Vue.Left;
      R.Top    := Vue.Top;
      R.Bottom := Vue.Left + Vue.Height;
      R.Right  := Vue.Top + Vue.Width;
      TmpBuffer.Canvas.FillRect(R);
      DrawReferentiel;
      vue.Canvas.CopyRect(R, TmpBuffer.Canvas, R);
    finally
      TmpBuffer.Free;
    end;
  end;
end;

procedure TCdrTriedreThetaPhi.SetProcRefreshXY(const P: TProcedureOfObject);
begin
  FProcRefreshXY := P;
end;

procedure TCdrTriedreThetaPhi.SetProcDoAction(const P: TProcedureOfObject);
begin
  FProcDoAction := P;
end;

procedure TCdrTriedreThetaPhi.SetParamsTriedre(const Theta, Phi: double);
const PI180 = PI / 180;
begin
  FCanDraw := False;
  try
    FTheta := Theta;
    FPhi   := Phi;
    Aux[1] := Sin(FTheta * PI180);
    Aux[2] := Sin(FPhi   * PI180);  // | -sin(A)           +cos(A)         0           0 | | x |   | X |
    Aux[3] := Cos(FTheta * PI180);  // |                                                 | |   |   |   |
    Aux[4] := Cos(FPhi   * PI180);  // | -cos(A).sin(P)    -sin(A).sin(P)  cos(A)      0 | | y |   | Y |
    Aux[5] := Aux[3] * Aux[2];      // |                                                 |*|   | = |   |
    Aux[6] := Aux[1] * Aux[2];      // | -cos(A).cos(P)    -sin(A).cos(P)  -sin(P)     R | | z |   | Z |
    Aux[7] := Aux[3] * Aux[4];      // |                                                 | |   |   |   |
    Aux[8] := Aux[1] * Aux[4];      // | 0                 0               0           1 | | 1 |   | 1 |

    FCanDraw:=True;
    Vue.Invalidate;
  except
  end;

end;

function TCdrTriedreThetaPhi.GetTheta: double;
begin
  Result := FTheta;
end;

function TCdrTriedreThetaPhi.GetPhi: double;
begin
  Result := FPhi;
end;

end.

