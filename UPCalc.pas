Unit UPCalc;
{ unit de calcul avec interception des erreurs }
{$mode DELPHI}{$H+}

INTERFACE

Uses SysUtils;

Const
 MaxPow = 100;

 mRadian = 0;
 mDegre  = 1;
 ModeAngle:Integer=mRadian;

Var
 PCalcErrCode : Word;     { code d'erreur }
 PCalcLastErr : Word;

{ gestions des erreurs }
Function  GetPCalcErrorMsg:String;  { renvoie le message d'erreur }
Function  IsPCalcError:Boolean;     { indique la prsence d'une erreur }

{ oprations arithmtiques }
Function PAdd(x1,x2:Double):Double;
Function PSub(x1,x2:Double):Double;
Function PMul(x1,x2:Double):Double;
Function PDiv(x1,x2:Double):Double;
Function POpp(x:Double):Double;

{ fonctions d'une variable relle prdfinies }
Function PArcTan(x:Double):Double;    { arc tangente }
Function PCos(x:Double):Double;       { cosinus }
Function PSin(x:Double):Double;       { sinus }
Function PTan(x:Double):Double;       { tangente }
Function PLn(x:Double):Double;        { logarithme }
Function PExp(x:Double):Double;       { exponentielle }
Function PSqr(x:Double):Double;       { carré }
Function PSqrt(x:Double):Double;      { racine carrée }

{ autres fonctions }
Function PSh(x:Double):Double;        { sinus hyperbolique }
Function PCh(x:Double):Double;        { cosinus hyperbolique }
Function PTh(x:Double):Double;        { tangente hyperbolique }
Function PArcSin(x:Double):Double;    { arc sinus }
Function PArcCos(x:Double):Double;    { arc cosinus }
Function PArgSh(x:Double):Double;     { argument sinus hyperbolique }
Function PArgCh(x:Double):Double;     { argument cosinus hyperbolique }
Function PArgTh(x:Double):Double;     { argument tangente hyperbolique }
Function PLog(x:Double):Double;       { logarithme dcimal }
Function PPuiss(x1,x2:Double):Double; { x1 puissance x2 }
Function PRound(x:Double):Double;     { arrondi entier de x }
Function PFrac(x:Double):Double;      { partie fractionnaire de x }
Function PINT(x:Double):Double;       { partie entire de x }
Function PABS(x:Double):Double;       { valeur absolue de x }
Function PRANDOM(x:Double):Double;    { nombre tir au hasard infrieur  x }

Function PHypot(x,y:Double):Double;    { hypotnuse }
Function PArcTan2(x,y:Double):Double;  { angle }

Function PMod(x,y:Double):Double;     { reste de la division de x par y }
Function PSign(x:Double):Double;      { signe de x }
Function PMin(x,y:Double):Double;     { minimum }
Function PMax(x,y:Double):Double;     { maximum }

Function PRad2Deg(x:Double):Double;
Function PDeg2Rad(x:Double):double;

Procedure ModeDegre;

IMPLEMENTATION

Const
 cInvalidOp=1;
 cZeroDivide=2;
 cOverflow=3;
 cUnderflow=4;

Procedure ModeDegre;
Begin
 ModeAngle:=mDegre;
End;

Function GetPCalcErrorMsg:String;
Begin
 case PCalcLastErr of
    cUnderflow  : GetPCalcErrorMsg:='Dpassement de capacit.';
    cZeroDivide : GetPCalcErrorMsg:='Division par zro.';
    cOverflow   : GetPCalcErrorMsg:='Dpassement de capacit.';
    cInvalidOp  : GetPCalcErrorMsg:='Opration non valide.';
    else GetPCalcErrorMsg:='R.A.S';
 end;
End;

Function IsPCalcError:Boolean;
Begin
 PCalcLastErr:=PCalcErrCode;
 PCalcErrCode:=0;
 IsPCalcError:=PCalcLastErr<>0;
End;

{ conversions angulaires }

Function PRad2Deg(x:Double):Double;
Begin
 Result:=PDiv(PMul(x,180),pi);
End;

Function PDeg2Rad(x:Double):Double;
Begin
 Result:=PDiv(PMul(x,pi),180);
End;

{ oprations arithmtiques }

Function PAdd(x1,x2:Double):Double;
Begin
 try
  Result:=x1+x2;
 except
  on EOverflow do
   begin Result:=0; PCalcErrCode:=cOverflow; end;
  on EUnderflow do
   begin Result:=0; PCalcErrCode:=cUnderflow; end;
 end;
End;

Function PSub(x1,x2:Double):Double;
Begin
 try
  Result:=x1-x2;
 except
  on EOverflow do
   begin Result:=0; PCalcErrCode:=cOverflow; end;
  on EUnderflow do
   begin Result:=0; PCalcErrCode:=cUnderflow; end;
 end;
End;

Function PMul(x1,x2:Double):Double;
Begin
 try
  Result:=x1*x2;
 except
  on EOverflow do
   begin Result:=0; PCalcErrCode:=cOverflow; end;
  on EUnderflow do
   begin Result:=0; PCalcErrCode:=cUnderflow; end;
 end;
End;

Function PDiv(x1,x2:Double):double;
Begin
 try
  if x2<>0 then Result:=x1/x2
  else
   begin
    PCalcErrCode:=cZeroDivide;
    Result:=0;
   end;
 except
  on EOverflow do
   begin Result:=0; PCalcErrCode:=cOverflow; end;
  on EUnderflow do
   begin Result:=0; PCalcErrCode:=cUnderflow; end;
 end;
End;

Function POpp(x:double):double;
Begin
 Result:=-x;
End;

{ fonctions d'une variable relle prdfinies }

Function PArcTan(x:double):double;    { arc tangente }
Var t:double;
Begin
 try
  t:=ArcTan(x);
  case ModeAngle of
   mRadian : Result:=t;
   else Result:=PRad2Deg(t);
  end;
 except
  on EOverflow do
   begin Result:=0; PCalcErrCode:=cOverflow; end;
  on EUnderflow do
   begin Result:=0; PCalcErrCode:=cUnderflow; end;
  on EZeroDivide do
   begin Result:=0; PCalcErrCode:=cZeroDivide; end;
  on EInvalidOp do
   begin Result:=0; PCalcErrCode:=cInvalidOp; end;
 end;
End;

Function PCos(x:double):double;       { cosinus }
Var t:double;
Begin
 try
  case ModeAngle of
   mRadian : t:=x;
   else t:=PDeg2Rad(x);
   end;
  Result:=cos(t);
 except
  on EOverflow do
   begin Result:=0; PCalcErrCode:=cOverflow; end;
  on EUnderflow do
   begin Result:=0; PCalcErrCode:=cUnderflow; end;
  on EZeroDivide do
   begin Result:=0; PCalcErrCode:=cZeroDivide; end;
  on EInvalidOp do
   begin Result:=0; PCalcErrCode:=cInvalidOp; end;
 end;
End;

Function PSin(x:double):double;       { sinus }
Var t:double;
Begin
 try
  case ModeAngle of
   mRadian : t:=x;
   else t:=PDeg2Rad(x);
   end;
  Result:=Sin(t);
 except
  on EOverflow do
   begin Result:=0; PCalcErrCode:=cOverflow; end;
  on EUnderflow do
   begin Result:=0; PCalcErrCode:=cUnderflow; end;
  on EZeroDivide do
   begin Result:=0; PCalcErrCode:=cZeroDivide; end;
  on EInvalidOp do
   begin Result:=0; PCalcErrCode:=cInvalidOp; end;
 end;
End;

Function PLn(x:double):double;        { logarithme }
Begin
 try
  if x>0 then Result:=Ln(x)
  else
   begin
    PCalcErrCode:=cInvalidOp;
    Result:=0;
   end;
 except
  on EOverflow do
   begin Result:=0; PCalcErrCode:=cOverflow; end;
  on EUnderflow do
   begin Result:=0; PCalcErrCode:=cUnderflow; end;
  on EZeroDivide do
   begin Result:=0; PCalcErrCode:=cZeroDivide; end;
 end;
End;

Function PExp(x:Double):Double;       { exponentielle }
Begin
 try
  Result:=Exp(x);
 except
  on EOverflow do
   begin Result:=0; PCalcErrCode:=cOverflow; end;
  on EUnderflow do
   begin Result:=0; PCalcErrCode:=cUnderflow; end;
  on EZeroDivide do
   begin Result:=0; PCalcErrCode:=cZeroDivide; end;
  on EInvalidOp do
   begin Result:=0; PCalcErrCode:=cInvalidOp; end;
 end;
End;

Function PSqr(x:Double):Double;       { carr }
Begin
 try
  Result:=Sqr(x);
 except
  on EOverflow do
   begin Result:=0; PCalcErrCode:=cOverflow; end;
  on EUnderflow do
   begin Result:=0; PCalcErrCode:=cUnderflow; end;
  on EZeroDivide do
   begin Result:=0; PCalcErrCode:=cZeroDivide; end;
  on EInvalidOp do
   begin Result:=0; PCalcErrCode:=cInvalidOp; end;
 end;
End;

Function PSqrt(x:Double):Double;      { racine carre }
Begin
 try
  if x>=0 then Result:=Sqrt(x)
  else
   begin
    PCalcErrCode:=cInvalidOp;
    Result:=0;
   end;
 except
  on EOverflow do
   begin Result:=0; PCalcErrCode:=cOverflow; end;
  on EUnderflow do
   begin Result:=0; PCalcErrCode:=cUnderflow; end;
  on EZeroDivide do
   begin Result:=0; PCalcErrCode:=cZeroDivide; end;
  on EInvalidOp do
   begin Result:=0; PCalcErrCode:=cInvalidOp; end;
 end;
End;

{ autres fonctions }

Function PTan(x:Double):Double;       { tangente }
Begin
 PTan:=PDiv(PSin(x),PCos(x));
End;

Function PCh(x:Double):Double;        { cosinus hyperbolique }
Begin
 PCh:=PDiv(PAdd(PExp(x),PExp(-x)),2);
End;

Function PSh(x:Double):Double;        { sinus hyperbolique }
Begin
 PSh:=PDiv(PSub(PExp(x),PExp(-x)),2);
End;

Function PTh(x:Double):Double;        { tangente hyperbolique }
Var r1,r2:Double;
Begin
 r1:=PSub(PExp(x),PExp(-x));
 r2:=PAdd(PExp(x),PExp(-x));
 PTh:=PDiv(r1,r2);
End;

Function PArcSin(x:Double):Double;    { arc sinus }
Begin
 if x=1 then
  begin
   case ModeAngle of
    mRadian : PArcSin:=pi/2;
    else PArcSin:=90;
   end;
   exit;
  end;
 if x=-1 then
  begin
   case ModeAngle of
    mRadian : PArcSin:=-pi/2;
    else PArcSin:=-90;
   end;
   exit;
  end;
 PArcSin:=PArcTan(PDiv(x,PSqrt(PSub(1,PSqr(x)))));
End;

Function PArcCos(x:Double):Double;    { arc cosinus }
Var t:Double;
Begin
 if x=0 then
  begin
   case ModeAngle of
    mRadian : PArcCos:=pi/2;
    else PArcCos:=90;
   end;
   exit;
  end;
 t:=PArcTan(PDiv(PSqrt(PSub(1,PSqr(x))),x));
 if x<0 then
  begin
   case ModeAngle of
    mRadian : t:=PAdd(pi,t);
    else t:=PAdd(180,t);
    end;
   end;
  PArcCos:=t;
End;

Function PArgSh(x:Double):Double;     { argument sinus hyperbolique }
Var r:Double;
Begin
 r:=PSqrt(PAdd(PSqr(x),1));
 if x<0
    then PArgSh:=PLn(PSub(r,x))
    else PArgSh:=PLn(PAdd(x,r));
End;

Function PArgCh(x:Double):Double;     { argument cosinus hyperbolique }
Begin
 PArgCh:=PLn(PAdd(x,PSqrt(PSub(PSqr(x),1))));
End;

Function PArgTh(x:Double):Double;     { argument tangente hyperbolique }
Begin
 PArgTh:=PMul(0.5,PLn(PDiv(PAdd(1,x),PSub(1,x))));
End;

Function PLog(x:Double):Double;       { logarithme dcimal }
Begin
 PLog:=PDiv(PLn(x),PLn(10));
End;

Function PPuiss(x1,x2:Double):Double;     { x1 puissance x2 }
Var k,n:Integer;
    r:Double;
Begin
 if (PFrac(x2)=0) and (abs(x2)<MaxPow) then
  begin
   n:=round(x2);
   k:=0;
   r:=1;
   while (k<abs(n)) and (PCalcErrCode=0) do
    begin
     r:=PMul(r,x1);
     inc(k);
    end;
   if PCalcErrCode=0 then
    begin
     if n>0 then PPuiss:=r
     else PPuiss:=PDiv(1,r);
    end
   else PPuiss:=0;
  end
 else PPuiss:=PExp(PMul(x2,PLn(x1)));
End;

Function PRound(x:Double):Double;
Begin
 try
  Result:=Round(x);
 except
  on EOverflow do
   begin Result:=0; PCalcErrCode:=cOverflow; end;
  on EUnderflow do
   begin Result:=0; PCalcErrCode:=cUnderflow; end;
  on EZeroDivide do
   begin Result:=0; PCalcErrCode:=cZeroDivide; end;
  on EInvalidOp do
   begin Result:=0; PCalcErrCode:=cInvalidOp; end;
 end;
End;

Function PFrac(x:Double):Double;
Begin
 PFrac:=Frac(x);
End;

Function PINT(x:Double):Double;
Begin
 if x>=0 then PINT:=Int(x)
 else PINT:=Int(x)-1;
End;

Function PABS(x:Double):Double;
Begin
 PABS:=Abs(x);
End;

Function PRANDOM(x:Double):Double;
Begin
 PRANDOM:=PMUL(x,random);
End;

Function PHypot(x,y:Double):Double;  { hypotnuse }
Begin
 PHypot:=PSqrt(PAdd(PSqr(x),PSqr(y)));
End;

Function PArcTan2(x,y:Double):Double;  { angle }
Var t:Double;
Begin
 if x=0 then t:=pi/2
 else t:=PArcTan(PDiv(y,x));
 case ModeAngle of
  mRadian : PArcTan2:=t;
  else PArcTan2:=PRad2Deg(t);
  end;
End;

Function PMod(x,y:Double):Double;
Begin
 if y=0 then PMod:=x
 else PMod:=PSub(x,PMul(PRound(PDiv(x,y)),y));
End;

Function PSign(x:Double):Double;
Begin
 if x=0 then PSign:=0
 else if x>0 then PSign:=1 else PSign:=-1;
End;

Function PMin(x,y:Double):Double;
Begin
 if x<y then PMin:=x else PMin:=y;
End;

Function PMax(x,y:Double):Double;
Begin
 if x>y then PMax:=x else PMax:=y;
End;

END.

{                          Fin du fichier UPCALC.PAS                        }

