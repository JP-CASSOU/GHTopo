Unit NombresComplexes;
{$mode DELPHI}{$H+}
interface
type
  TComplex = record
    x, y : double;
  end;



function CplxZero : TComplex;
function CplxAddition (const A, B : TComplex) : TComplex;
function CplxMultiplication(const A, B : TComplex) : TComplex;
function CplxMultiplicationScalaire(const A : TComplex; const R : double) : TComplex;
function CplxDivision(const A, B : TComplex) : TComplex;
function CplxPowerInt(const Z : TComplex; const N : integer) : TComplex;
function CplxSinus(const A : TComplex) : TComplex;
procedure CplxPolar(const Z : TComplex; var Module, Argument : double);
implementation

uses
  Math;

function CplxZero : TComplex;
begin
  result.x := 0;
  result.y := 0;
end;

function CplxAddition (const A, B : TComplex) : TComplex;
begin
  result.x := A.x + B.x;
  result.y := A.y + B.y;
end;

function CplxMultiplication(const A, B : TComplex) : TComplex;
begin
  result.x := A.x * B.x - A.y * B.y;
  result.y := A.x * B.y + A.y * B.x;
end;

function CplxMultiplicationScalaire(const A : TComplex; const R : double) : TComplex;
begin
  result.x := A.x * R;
  result.y := A.y * R;
end;

function CplxDivision(const A, B : TComplex) : TComplex;
var
  Delta : double;
begin
  Delta:=1/(sqr(B.x)+sqr(B.y));
  result.x:=(A.x*B.x+A.y*B.y)*Delta;
  result.y:=(A.y*B.x-A.x*B.y)*Delta;
end;

function CplxPowerInt(const Z : TComplex; const N : integer) : TComplex;
var
  I : integer;
begin
  //assert(N>=0,'La puissance doit être positive ou nulle dans CplxPowerInt');
  result.x := 1;
  result.y := 0;
  for I := 1 to N do
    result := CplxMultiplication(result, Z);
end;

function CplxSinus(const A : TComplex) : TComplex;
begin
  result.X := Sin(A.x)*Cosh(A.y);
  result.Y := Cos(A.x)*Sinh(A.y);
end;

procedure CplxPolar(const Z : TComplex; var Module, Argument : double);
begin
  Module := sqrt(sqr(Z.x) + sqr(Z.y));
  Argument := Arctan2(Z.x, Z.y);
end;

end.

