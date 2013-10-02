Unit UEval;
{ arbre de calcul avec dictionnaire }
{ K.B. mai 1996 }
{$mode DELPHI}{$H+}

INTERFACE

Uses
  Common,
  SysUtils,
  UPCalc,
  Dialogs;

Const
 MaxSynTreeBranches = 100;

 { erreurs de syntaxe }
 erOk            = 0;
 erSyntaxe       = 1;
 erSymbole       = 2;
 erDuplicate     = 3;
 erFinInattendue = 4;
 erEvalMem       = 5;
 erParenthese    = 6;
 erMotInconnu    = 7;
 erNombre        = 8;
 erCarInconnu    = 9;
 erDefinition    = 10;

 { types de donn‚es }
 idConst = 1;
 idVar   = 2;
 idFX    = 3;
 idFXY   = 4;
 idNode  = 5;

Type

 PReal = ^Double;

 PCalcNode = ^TCalcNode;

 MathFX  = Function(x:Double):Double;
 MathFXY = Function(x,y:Double):Double;

 { élément du dictionnaire }
 PMathItem=^TMathItem;
 TMathItem=record
  case Id:Byte of
   idConst   : (nombre  : Double);
   idVar     : (value   : Double);
   idFX      : (adrfx   : MathFX);
   idFXY     : (adrfxy  : MathFXY);
   idNode    : (pnode   : PCalcNode);
   end;

 PSynTreeBranches = ^TSynTreeBranches;
 TSynTreeBranches = array[1..100] of PCalcNode;

 TCalcNode = object
  NbSynTreeBranches:Byte;
  SynTreeBranches:PSynTreeBranches;
  Constructor Init(br:Integer);
  Destructor Done; virtual;
  Function  GetVal:Double; virtual;
  Procedure Accroche(p:PCalcNode; i:Integer);
  Function Duplicate:PCalcNode; virtual;
  End;

 PConstNode = ^TConstNode;
 TConstNode = object(TCalcNode)
  Value:Double;
  Constructor Init(r:Double);
  Function GetVal:Double; virtual;
  Function Duplicate:PCalcNode; virtual;
  End;

 PVarNode = ^TVarNode;
 TVarNode = object(TCalcNode)
  Adresse: PReal;
  Constructor Init(ad:PReal);
  Function GetVal:Double; virtual;
  Function Duplicate:PCalcNode; virtual;
  End;

 PFXNode = ^TFXNode;
 TFXNode = object(TCalcNode)
  CalcProc:MathFX;
  Constructor Init(ad:MathFX);
  Function GetVal:Double; virtual;
  Function Duplicate:PCalcNode; virtual;
  End;

 PFXYNode = ^TFXYNode;
 TFXYNode = object(TCalcNode)
  CalcProc:MathFXY;
  Constructor Init(ad:MathFXY);
  Function GetVal:Double; virtual;
  Function Duplicate:PCalcNode; virtual;
  End;

 { nom de fonctions ou de variables }
 TIdentItem = String[20];

 { dictionnaire }
 PMotDico = ^TMotDico;
 TMotDico = record
  Nom  : TIdentItem;
  Next : PMotDico;
  Item : TMathItem;
  End;

 { dictionnaire des mots d‚finis }
 PMathDico = ^TMathDico;
 TMathDico = object
  Table : array['A'..'Z'] of PMotDico;
  Constructor Init;
  { initialisation }
  Destructor Done; virtual;
  { lib‚ration m‚moire }
  Function  Trouve(Id:TIdentItem) : PMotDico;
  { recherche d'un mot }
  Function  AjouterMot(Id:TIdentItem; It:TMathItem):Boolean;
  { ajouter un mot }
  Procedure AjouterConstante(Id:TIdentItem; r:Double);
  { ajouter une constante }
  Procedure AjouterFonction(Id:TIdentItem; f:Pointer);
  { ajouter une fonction prédéfinie }
  Procedure AjouterFormule(Id:TIdentItem; S:String);
  { ajouter une formule définie par S }
  Procedure DefVar(Id:TIdentItem; r:Double);
  { ajouter ou modifier une variable }
  Procedure EffacerMot(Id:TIdentItem);
  { supprimer un mot }
  End;

Var
 ParseErrorFlag:Byte;

Function BasicMathDico:PMathDico;
{ d‚finit les fonctions math‚matiques de base }
Function ExtendedMathDico:PMathDico;
{ d‚finit les fonctions math‚matiques de base et les fonctions hyperboliques }
Function GetNode(S:String; dic:PMathDico):PCalcNode;
{ construit l'arbre de calcul associ‚ … S }
Function EvalExpr(S:String; Var erreur:Boolean):Double;
{ ‚value S ; erreur est ‚gal … true si une erreur s'est produite }
Function GetParseErrorMsg:String;
{ message d'erreur de syntaxe; remet ParseErrorFlag … 0 }
Function FormateReal(R:Double;deci:Byte):String;
{ ‚crit le nombre r‚el contenu dans R avec dec chiffres aprŠs la virgule
  supprime les 0 en fin de chaŒne }

IMPLEMENTATION

Const
 { r‚sultats de comparaisons }
 cInf  = 0;
 cEgal = 1;
 cSup  = 2;

Function GetParseErrorMsg:String;
{ renvoie un message d'erreur }
Begin
 case ParseErrorFlag of
  erOk            : GetParseErrorMsg:='';
  erSyntaxe       : GetParseErrorMsg:='Erreur de syntaxe.';
  erSymbole       : GetParseErrorMsg:='Symbole inconnu.';
  erDuplicate     : GetParseErrorMsg:='Identificateur d‚j… utilis‚.';
  erFinInattendue : GetParseErrorMsg:='Expression attendue.';
  erEvalMem       : GetParseErrorMsg:='M‚moire insuffisante.';
  erParenthese    : GetParseErrorMsg:='Erreur de parenthŠses.';
  erMotInconnu    : GetParseErrorMsg:='Mot inconnu.';
  erNombre        : GetParseErrorMsg:='Nombre mal ‚crit.';
  erCarInconnu    : GetParseErrorMsg:='CaractŠre incorrect.';
  erDefinition    : GetParseErrorMsg:='Mot mal d‚fini.';
  else GetParseErrorMsg:='';
  end;
 ParseErrorFlag:=0;
End;

Function FormateReal(R:Double;deci:Byte):String;
{ mise en forme des r‚sultats pour affichage }
Var
  Fmt: string;
Begin
  Fmt:=Format('%df',[deci]);
  Result:=Format('%'+Fmt,[R]);
End;

Function MajChar(c:Char):Char;
Begin
 case c of
  'a','à'     : MajChar:='A';
  'e','é','è' : MajChar:='E';
  'i'         : MajChar:='I';
  'o'         : MajChar:='O';
  'u'         : MajChar:='U';
  'ç': MajChar:='C';
  else MajChar:=upcase(c);
  end;
End;

Function BasicMathDico:PMathDico;
{ cr‚e un dictionnaire contenant les fonctions math‚matiques de base }
Var UserDico:PMathDico;
Begin
 UserDico:=New(PMathDico,Init);
 if UserDico<>nil
  then with UserDico^ do
        begin
         AjouterConstante('PI',pi);
         AjouterFonction('FRAC',@PFrac);
         AjouterFonction('ENT',@PINT);
         AjouterFonction('ABS',@PABS);
         AjouterFonction('HASARD',@PRANDOM);
         AjouterFonction('ARRONDI',@PROUND);
         AjouterFonction('RACINE',@PSQRT);
         AjouterFonction('CARRE',@PSQR);
         AjouterFonction('SIN',@PSIN);
         AjouterFonction('COS',@PCOS);
         AjouterFonction('TAN',@PTAN);
         AjouterFonction('ARCSIN',@PARCSIN);
         AjouterFonction('ARCCOS',@PARCCOS);
         AjouterFonction('ARCTAN',@PARCTAN);
        end;
 BasicMathDico:=UserDico;
End;

Function ExtendedMathDico:PMathDico;
{ cr‚e un dictionnaire contenant les fonctions de bases et les fonctions
  hyperboliques }
Var UserDico:PMathDico;
Begin
 UserDico:=BasicMathDico;
 if UserDico<>nil
  then with UserDico^ do
        begin
         AjouterFonction('ARGSH',@PARGSH);
         AjouterFonction('ARGCH',@PARGCH);
         AjouterFonction('ARGTH',@PARGTH);
         AjouterFonction('EXP',@PEXP);
         AjouterFonction('LN',@PLN);
         AjouterFonction('LOG',@PLOG);
         AjouterFonction('SINH',@PSH);
         AjouterFonction('COSH',@PCH);
         AjouterFonction('TANH',@PTH);
        end;
 ExtendedMathDico:=UserDico;
End;

{ objet TMathDico }

Constructor TMathDico.Init;
Var c : Char;
Begin
 for c:='A' to 'Z' do Table[c]:=nil;
End;

Destructor TMathDico.Done;
Var c : Char;
    D,P : PMotDico;
Begin
 for c:='A' to 'Z' do
  begin
   D:=Table[c];
   while D<>nil do
    begin
     if D^.Item.Id=idNode
      then if D^.Item.PNode<>nil then dispose(D^.Item.PNode,done);
     P:=D^.Next;
     dispose(D);
     D:=P;
    end;
  end;
End;

Function TMathDico.AjouterMot(Id:TIdentItem; It:TMathItem):Boolean;
{ ajoute un nouveau mot sans v‚rifier son existence dans le dictionnaire }
Var M,D : PMotDico;
    i   : Integer;
Begin
 { mettre le nom en majuscule }
 for i:=1 to length(Id) do Id[i]:=MajChar(Id[i]);
 { v‚rifier la 1Šre lettre }
 if (Id[1]<'A') or (Id[1]>'Z')
  then begin
        AjouterMot:=false;
        exit;
       end;
 { r‚server m‚moire }
 New(M);
 if M=nil
  then begin
        AjouterMot:=false;
        exit;
       end
  else AjouterMot:=true;
 { copier valeurs }
 M^.Nom:=Id;
 M^.Next:=nil;
 M^.Item:=It;
 D:=Table[M^.Nom[1]];
 if D=nil
  then Table[M^.Nom[1]]:=M
  else begin
        while D^.Next<>nil do D:=D^.Next;
        D^.Next:=M;
       end;
End;

Procedure TMathDico.AjouterFonction(Id:TIdentItem; f:Pointer);
{ ajoute une fonction au dictionnaire; adresse est un pointeur vers
  la fonction de type MathFunction=Function(x:Real):Double }
Var Item : TMathItem;
Begin
 Item.Id:=idFX;
 Item.adrfx:=MathFX(f);
 AjouterMot(Id,Item);
End;

Procedure TMathDico.AjouterConstante(Id:TIdentItem; r:Double);
{ ajoute une constante }
Var Item : TMathItem;
Begin
 Item.Id:=idConst;
 Item.Nombre:=r;
 AjouterMot(Id,Item);
End;

Procedure TMathDico.AjouterFormule(Id:TIdentItem; S:String);
Var Item : TMathItem;
    node : PCalcNode;
Begin
 Item.Id:=idNode;
 Item.PNode:=GetNode(S,@Self);
 if ParseErrorFlag=0
  then AjouterMot(Id,Item)
  else if Item.PNode<>nil then dispose(Item.PNode,done);
End;

Procedure TMathDico.DefVar(Id:TIdentItem; r:double);
{ donne la valeur r … la variable Id }
Var i : Integer;
    M : PMotDico;
    Item : TMathItem;
Begin
 { mettre en majuscule }
 for i:=1 to length(Id) do Id[i]:=MajChar(Id[i]);
 { chercher le mot dans le distionnaire }
 M:=Trouve(Id);
 if M<>nil
  then begin  { mot trouv‚ }
        M^.Item.Id:=idVar;
        M^.Item.Value:=r;
       end
  else begin   { mot nouveau }
        Item.Id:=idVar;
        Item.Value:=r;
        AjouterMot(Id,Item);
       end;
End;

Procedure TMathDico.EffacerMot(Id:TIdentItem);
Var D,P : PMotDico;
    ok  : Boolean;
    i   : Integer;
Begin
 for i:=1 to length(Id) do Id[i]:=MajChar(Id[i]);
 D:=Table[Id[1]];
 if D=nil then exit;
 if D^.Nom=Id
  then begin
        Table[Id[1]]:=D^.Next;
        dispose(D);
       end
  else begin
        P:=D^.Next;
        ok:=false;
        while not ok and (P<>nil) do
         begin
          if P^.Nom=Id
           then begin
                 D^.Next:=P^.Next;
                 dispose(P);
                 ok:=true;
                end
           else begin
                 D:=P;
                 P:=D^.Next;
                end;
         end;
        end;
End;

Function TMathDico.Trouve(Id:TIdentItem) : PMotDico;
Var D  : PMotDico;
    ok : Boolean;
Begin
 if (Id[1]<'A') or (Id[1]>'Z')
  then begin
        Trouve:=nil;
        exit;
       end;
 D:=Table[Id[1]];
 ok:=false;
 while not ok and (D<>nil) do
  begin
   if D^.Nom=Id then ok:=true
    else D:=D^.Next;
  end;
 Trouve:=D;
End;

{ objet TCalcNode }

Constructor TCalcNode.Init(br:Integer);
Var i:Integer;
Begin
 NbSynTreeBranches:=br;
 if br=0
  then SynTreeBranches:=nil
  else begin
        getmem(SynTreeBranches,br*sizeof(PCalcNode));
        for i:=1 to br do SynTreeBranches^[i]:=nil;
       end;
End;

Destructor TCalcNode.Done;
Var i:Integer;
Begin
 if NbSynTreeBranches>0
  then begin
        for i:=1 to NbSynTreeBranches do
         if SynTreeBranches^[i]<>nil
          then dispose(SynTreeBranches^[i],done);
        freemem(SynTreeBranches,NbSynTreeBranches*sizeof(PCalcNode));
       end;
End;

Function TCalcNode.GetVal:Double;
Begin
 GetVal:=0;
End;

Procedure TCalcNode.Accroche(p:PCalcNode; i:Integer);
Begin
 if SynTreeBranches^[i]<>nil
  then dispose(SynTreeBranches^[i],done);
 SynTreeBranches^[i]:=p;
End;

Function TCalcNode.Duplicate:PCalcNode;
Begin
 Duplicate:=nil;
End;

{ objet TConstNode }

Constructor TConstNode.Init(r:double);
Begin
 TCalcNode.Init(0);
 Value:=r;
End;

Function TConstNode.GetVal:Double;
Begin
 GetVal:=Value;
End;

Function TConstNode.Duplicate:PCalcNode;
Begin
 Duplicate:=New(PConstNode,Init(Value));
End;

{ objet TVarNode }

Constructor TVarNode.Init(ad:PReal);
Begin
 TCalcNode.Init(0);
 Adresse:=ad;
End;

Function TVarNode.GetVal:Double;
Begin
 GetVal:=Adresse^;
End;

Function TVarNode.Duplicate:PCalcNode;
Begin
 Duplicate:=New(PVarNode,Init(Adresse));
End;

{ objet TFXNode }

Constructor TFXNode.Init(ad:MathFX);
Begin
 TCalcNode.Init(1);
 CalcProc:=ad;
End;

Function TFXNode.GetVal:Double;
Var r:Double;
Begin
 r:=SynTreeBranches^[1]^.GetVal;
 if PCalcErrCode<>0
  then GetVal:=0
  else GetVal:=CalcProc(r);
End;

Function TFXNode.Duplicate:PCalcNode;
Var node:PFXNode;
    i:Integer;
Begin
 node:=New(PFXNode,Init(CalcProc));
 for i:=1 to NbSynTreeBranches do
   node^.SynTreeBranches^[i]:=SynTreeBranches^[i]^.Duplicate;
 Duplicate:=node;
End;

{ objet TFXYNode }

Constructor TFXYNode.Init(ad:MathFXY);
Begin
 TCalcNode.Init(2);
 CalcProc:=ad;
End;

Function TFXYNode.GetVal:Double;
Var r1,r2:Real;
Begin
 r1:=SynTreeBranches^[1]^.GetVal;
 if PCalcErrCode<>0
  then GetVal:=0
  else begin
        r2:=SynTreeBranches^[2]^.GetVal;
        if PCalcErrCode<>0
         then GetVal:=0
         else GetVal:=CalcProc(r1,r2);
       end;
End;

Function TFXYNode.Duplicate:PCalcNode;
Var node:PFXYNode;
    i:Integer;
Begin
 node:=New(PFXYNode,Init(CalcProc));
 for i:=1 to NbSynTreeBranches do
   node^.SynTreeBranches^[i]:=SynTreeBranches^[i]^.Duplicate;
 Duplicate:=node;
End;

Function GetChar(Var S:String; i:Integer):Char;
Begin
 if i>ord(S[1])
  then GetChar:=#0
  else GetChar:=S[i];
End;

Procedure Strip(Var S:String; Var i:Integer);
Begin
 while GetChar(S,i)=' ' do inc(i);
End;

Function LireNombre(Var S:String; Var i:Integer):Real;
Var car  : Char;
    W    : String[80];
    err  : integer;
    r    : Double;
    p    : Integer;
Begin
 p:=1;
 W:='';
 W[p]:=S[i];
 inc(i);
 inc(p);
 car:=GetChar(S,i);
 while (car>='0') and (car<='9') do
  begin
   W[p]:=car;
   inc(i);
   inc(p);
   car:=GetChar(S,i);
  end;
 if car='.'
  then begin
        W[p]:=car;
        inc(i);
        inc(p);
        car:=GetChar(S,i);
        while (car>='0') and (car<='9') do
         begin
          W[p]:=car;
          inc(i);
          inc(p);
          car:=GetChar(S,i);
         end;
        end;
 if car='E'
  then begin
        W[p]:=car;
        inc(i);
        inc(p);
        car:=GetChar(S,i);
        if (car='+') or (car='-')
         then begin
               W[p]:=car;
               inc(i);
               inc(p);
               car:=GetChar(S,i);
              end;
        while (car>='0') and (car<='9') do
         begin
          W[p]:=car;
          inc(i);
          inc(p);
          car:=GetChar(S,i);
         end;
       end;
 W[0]:=chr(pred(p));
 val(W,r,err);
 if err<>0
  then begin
        r:=0;
        ParseErrorFlag:=erNombre;
       end;
 LireNombre:=r;
End;

Function LireMot(Var S:String; Var i:Integer):String;
Var W:String[80];
    p:Integer;
    car:Char;
Begin
 p:=1;
 W[p]:=GetChar(S,i);
 inc(i);
 inc(p);
 car:=GetChar(S,i);
 while ((car>='A') and (car<='Z')) or ((car>='0') and (car<='9')) do
  begin
   W[p]:=car;
   inc(p);
   inc(i);
   car:=GetChar(S,i);
  end;
 W[0]:=chr(pred(p));
 LireMot:=W;
End;

Function LireFacteur(Var S:String; Var i:Integer; dic:PMathDico):PCalcNode;
 forward;

Function MakeNode(Var S:String; Var i:Integer; dic:PMathDico):PCalcNode;
 forward;

Function LireItem(Var S:String; Var i:Integer; dic:PMathDico):PCalcNode;
Var car   : Char;
    pnode : PCalcNode;
    aux   : PCalcNode;
    r     : Real;
    W     : String[80];
    M     : PMotDico;
Begin
 pnode:=nil;
 Strip(S,i);
 car:=GetChar(S,i);
 case car of
  #0 : ParseErrorFlag:=erFinInattendue;
  '+':
   begin
    inc(i);
    pnode:=LireFacteur(S,i,dic);
   end;
  '-':
   begin
    inc(i);
    aux:=LireFacteur(S,i,dic);
    if ParseErrorFlag=0 then
     begin
      pnode:=New(PFXNode,Init(POpp));
      pnode^.Accroche(aux,1);
     end
    else if aux<>nil then dispose(aux,done);
   end;
  '(':
   begin
    inc(i);
    pnode:=MakeNode(S,i,dic);
    Strip(S,i);
    car:=GetChar(S,i);
    if car<> ')' then
     ParseErrorFlag:=erParenthese
    else inc(i);
   end;
  '0'..'9':
   begin
    r:=LireNombre(S,i);
    if ParseErrorFlag=0
     then pnode:=New(PConstNode,Init(r));
   end;
  'A'..'Z':
   if dic<>nil then
    begin
     W:=LireMot(S,i);
     M:=Dic^.Trouve(W);
     if M=nil then
      begin
       ParseErrorFlag:=erMotInconnu;
      end
     else
      begin
       case M^.Item.Id of
        idConst : pnode:=New(PConstNode,Init(M^.Item.Nombre));
        idVar   : pnode:=New(PVarNode,Init(PReal(@M^.Item.Value)));
        idNode  : pnode:=M^.Item.PNode^.Duplicate;
        idFX    :
         begin
          Strip(S,i);
          car:=GetChar(S,i);
          if car='(' then
           begin
            inc(i);
            aux:=MakeNode(S,i,dic);
            if ParseErrorFlag=0 then
             begin
              Strip(S,i);
              car:=GetChar(S,i);
              if car=')' then
               begin
                inc(i);
                pnode:=New(PFXNode,Init(M^.Item.adrfx));
                pnode^.Accroche(aux,1);
               end
              else
               begin
                ParseErrorFlag:=erParenthese;
                if aux<>nil then dispose(aux,done);
               end;
             end
            else if aux<>nil then dispose(aux,done);
           end
          else ParseErrorFlag:=erParenthese;
         end;  { cas idFx }
        else ParseErrorFlag:=erDefinition;
        end; { Case M^.Item.Id }
      end;
     end { dic<>nil }
    else ParseErrorFlag:=erMotInconnu;
   else ParseErrorFlag:=erCarInconnu;
  end;
 if ParseErrorFlag=0 then
  begin
   Strip(S,i);
   car:=GetChar(S,i);
   if car='ý' then
    begin
     aux:=pnode;
     pnode:=New(PFXNode,Init(PSQR));
     pnode^.Accroche(aux,1);
     inc(i);
    end;
 end;
 LireItem:=pnode;
End;

Function LireFacteur(Var S:String; Var i:Integer; dic:PMathDico):PCalcNode;
Var
 FirstNode, aux1, aux2: PCalcNode;
 car: Char;
Begin
 FirstNode:=LireItem(S,i,dic);
 aux1:=FirstNode;
 Strip(S,i);
 car:=GetChar(S,i);
 while (car='^') and (ParseErrorFlag=0) do
  begin
   inc(i);
   Strip(S,i);
   aux2:=LireItem(S,i,dic);
   if ParseErrorFlag=0
    then begin
          FirstNode:=New(PFXYNode,Init(PPuiss));
          FirstNode^.Accroche(aux1,1);
          FirstNode^.Accroche(aux2,2);
          aux1:=FirstNode;
          Strip(S,i);
          car:=GetChar(S,i);
         end
    else if aux2<>nil
          then dispose(aux2,done);
  end;
 if ParseErrorFlag=0
  then LireFacteur:=FirstNode
  else begin
        if FirstNode<>nil then dispose(FirstNode,Done);
        LireFacteur:=nil;
       end;
End;

Function LireTerme(Var S:String; Var i:Integer; dic:PMathDico):PCalcNode;
Var
 FirstNode, aux1, aux2: PCalcNode;
 car: Char;
Begin
 FirstNode:=LireFacteur(S,i,dic);
 aux1:=FirstNode;
 Strip(S,i);
 car:=GetChar(S,i);
 if not (car in ['*','/'])
  then if not (car in [#0, '+', '-', ')'])
        then begin
              car:='*';
              dec(i);
             end;
 while (car in ['*','/']) and (ParseErrorFlag=0) do
  begin
   inc(i);
   aux2:=LireFacteur(S,i,dic);
   if ParseErrorFlag=0
    then begin
          case car of
           '*': FirstNode:=New(PFXYNode,Init(PMul));
           '/': FirstNode:=New(PFXYNode,Init(PDiv));
           end;
          FirstNode^.Accroche(aux1,1);
          FirstNode^.Accroche(aux2,2);
          aux1:=FirstNode;
          Strip(S,i);
          car:=GetChar(S,i);
          if not (car in ['*','/'])
           then if not (car in [#0, '+', '-', ')'])
                 then begin
                       car:='*';
                       dec(i);
                      end;
         end
    else if aux2<>nil
          then dispose(aux2,done);
  end;
 if ParseErrorFlag=0
  then LireTerme:=FirstNode
  else begin
        if FirstNode<>nil then dispose(FirstNode,Done);
        LireTerme:=nil;
       end;
End;

Function MakeNode(Var S:String; Var i:Integer; dic:PMathDico):PCalcNode;
Var
 FirstNode, aux1, aux2: PCalcNode;
 car: Char;
Begin
 Strip(S,i);
 FirstNode:=LireTerme(S,i,dic);
 aux1:=FirstNode;
 Strip(S,i);
 car:=GetChar(S,i);
 while (car in ['+','-']) and (ParseErrorFlag=0) do
  begin
   inc(i);
   aux2:=LireTerme(S,i,dic);
   if ParseErrorFlag=0 then
    begin
     case car of
      '+': FirstNode:=New(PFXYNode,Init(PAdd));
      '-': FirstNode:=New(PFXYNode,Init(PSub));
      end;
     FirstNode^.Accroche(aux1,1);
     FirstNode^.Accroche(aux2,2);
     aux1:=FirstNode;
     Strip(S,i);
     car:=GetChar(S,i);
    end
   else if aux2<>nil then dispose(aux2,done);
  end;
 if ParseErrorFlag=0 then
  MakeNode:=FirstNode
 else
  begin
   if FirstNode<>nil then dispose(FirstNode,Done);
   MakeNode:=nil;
  end;
End;

Function GetNode(S:String; dic:PMathDico):PCalcNode;
Var W:String;
    i:Integer;
Begin
 W:=S;
 for i:=1 to length(W) do W[i]:=MajChar(W[i]);
 i:=1;
 GetNode:=MakeNode(W,i,dic);
End;

Function EvalExpr(S:String; Var Erreur:Boolean):double;
Var
 Node:PCalcNode;
 dic :PMathDico;
Begin
 try
   if trim(S)='' then S:='0';
   if S[1]='.' then S:='0'+S;
   ParseErrorFlag:=0;
   dic:=ExtendedMathDico;
   Node:=GetNode(S,dic);
   if ParseErrorFlag=0
    then begin
          Result:=Node^.GetVal;
          Erreur:=IsPCalcError;
         end
    else begin
          Result:=0;
          Erreur:=true;
         end;
   if Node<>nil
    then dispose(Node,done);
   if dic<>nil
    then dispose(dic,done);
   AfficherMessage(Format('Formula parser: %s = %s',[S,FloatToStr(Result)]));
 except
   Result := 0.00;
 end;
End;

BEGIN
 ParseErrorFlag:=0;
END.

{                          Fin du fichier UEval.Pas                         }
