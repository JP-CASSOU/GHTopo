unit UnitHelpSystem;
{$INCLUDE CompilationParameters.inc}

interface

uses
  Common,
  SysUtils, Classes;

// constantes pour la lecture du fichier
const
  BEGINSECTION = '<Section>';
  BEGININDEX   = '<Index>';
  BEGINTOPICS  = '<Topics>';
  BEGINTITLE   = '<Title>';
  BEGINTEXTE   = '<Texte>';

// objet Section
type TSection = record
  Index   : Integer;
  Topics  : string;
  Title   : string;
  Texte   : string;
end;
// liste des sections
type

{ TListeSections }

 TListeSections = class(TList)
  private

  public
    constructor Create;
    destructor Destroy; override;
    procedure  ClearListe;

end;

// objet HelpFile
type

{ THelpFileStructure }

 THelpFileStructure = class

  private
    FListeSections: TListeSections;
    FCurrentIndex: integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddSection(const S: TSection);
    function  GetSection(const I: integer):TSection;
    function SearchIndexSection(const Topix: string): integer;
    function LoadFromFile(const FichierAide: string): Boolean;

    function GetNbreSections: integer;
end;


implementation

//-----------------------
// Classe TListeSections
constructor TListeSections.Create;
begin
  inherited Create;
  AfficherMessage(Format('--> %s.Create',[ClassName]));
end;
destructor TListeSections.Destroy;
begin
  AfficherMessage(Format('--> %s.Destroy',[ClassName]));
inherited Destroy;
end;

procedure TListeSections.ClearListe;
var
  i: Integer;
begin
  try
    for i := 0 to self.Count - 1 do Dispose(Items[i]);

  finally
    self.Clear;
  end;
end;

//------------------------------------
// Classe THelpFileStructure
procedure THelpFileStructure.AddSection(const S: TSection);
var
  pS: ^TSection;
begin
  //AfficherMessage(Format('%s.Addsection',[ClassName]));
  with FListeSections do begin
    New(pS);
    pS^:=S;
    Add(pS);

  end;
end;
function THelpFileStructure.GetSection(const I: integer):TSection;
var
  q: integer;
  pS:^TSection;
  WU: Integer;
begin
  WU := GetNbreSections;
  with FListeSections do begin
    if (WU = 0) then Exit;
    q := i;
    if (q < 0) then q:=0;
    if (q > (WU - 1)) then q := WU - 1;
    pS := Items[q];
    Result := pS^;
  end;
end;


//------------------------------------
// Classe THelpFileStructure
constructor THelpFileStructure.Create;
begin
  inherited Create;
  AfficherMessage(Format('%s.Create',[ClassName]));
  FListeSections:=TListeSections.Create;
  FListeSections.Clear;

end;
destructor THelpFileStructure.Destroy;
var
  i: integer;
begin
  with FListeSections do
  begin
    ClearListe;
    Free;
  end;
  AfficherMessage(Format('%s.Destroy',[ClassName]));
  inherited Destroy;
end;
//-----------------------------
function THelpFileStructure.LoadFromFile(const FichierAide: string): boolean;
const
  TEMPFILE='TOTO.TXT';
var
  pHelpFile: TextFile;
  Sect: TSection;
  i1, NbLines: integer;
  Line: string;
  BalEnd, BalEnd2: string;
  function SetEnBalise(const Balise: string): string;
  var S1: string;
  begin
    S1:=Balise;
    Insert('/', S1, 2);
    Result:=S1;
  end;
  function HasBalise(const S, Balise: string): boolean;
  begin
    Result:=(Pos(Balise, S)>0);
  end;
  function ReadLine: string;
  var s1: string;
  begin
    ReadLn(pHelpFile, s1);
    Result:=Trim(s1);
  end;
  function ExtractText(const S, Balise: string): string;
  var
    P,Q: integer;
    BalEnd: string;
  begin
    P:=Pos(Balise, S) + Length(Balise);
    Q:=Pos(SetEnBalise(Balise), S);
    Result:=Trim(Copy(S,P, Q-P));
  end;
  // conversion d'un fichier texte
  // sans se soucier des formats de fin de ligne
  function GenerateSafeTextFile: boolean;
  var
    i: integer;
    pTXT: TextFile;
  begin
    AfficherMessage('--> Conversion du format texte');
    Result:=False;
    with TStringList.Create do begin
      try
       try
         LoadFromFile(FichierAide);
         AssignFile(pTXT, TEMPFILE);
         ReWrite(pTXT);
         for i:=0 to Count-1 do begin
           WriteLn(pTXT, Strings[i]);
         end;
         Result:=True;
       except
         AfficherMessage('--> Erreur de conversion');
       end;
      finally
        CloseFile(pTXT);
        Free;
      end;
    end;
  end;
begin
  Result:=False;
  AfficherMessage(Format('--> LoadFromFile: %s',[FichierAide]));
  if Not(FileExists(FichierAide)) then Exit;
  if Not(GenerateSafeTextFile) then Exit;
  //AssignFile(pHelpFile, FFichierHLP);
  AssignFile(pHelpFile, TEMPFILE);
  ReSet(pHelpFile);
  NbLines:=0;

  try
   try
     BalEnd:=SetEnBalise(BEGINSECTION);
     BalEnd2:=SetEnBalise(BEGINTEXTE);
     while Not(Eof(pHelpFile)) do begin
       NbLines:=NbLines+1;
       Line := ReadLine;
       if (HasBalise(Line, BEGINSECTION)) then begin
         //AfficherMessage('-------------------');
         //AfficherMessage('Nouvelle section');
         // on récupère l'index
         Sect.Index  :=StrToInt(ExtractText(ReadLine, BEGININDEX));
         // on récupère le topic
         Sect.Topics :=ExtractText(ReadLine, BEGINTOPICS);
         // on récupère le titre
         Sect.Title  :=ExtractText(ReadLine, BEGINTITLE);

         //AfficherMessage(ExtractText(ReadLine, BEGININDEX));
         Sect.Texte:='';
         while (Pos(BalEnd, Line)=0) do begin
           Line:=ReadLine;
           //AfficherMessage(Line);
           if Line='' then Sect.Texte:=Sect.Texte+#10
                      else Sect.Texte:=Sect.Texte+Line;
           //end;
         end;
         Sect.Texte:=ExtractText(Sect.Texte, BEGINTEXTE);

         //AfficherMessage('-------------------');
         //AfficherMessage('Fin de section');
         // on ajoute la section à la liste
         AddSection(Sect);
       end;
       //WriteLn(Format('%.5d | %s',[NbLines,ReadLine]));

     end;
     // fin de la lecture; contrôle des données

     for i1:=0 to FListeSections.Count-1 do begin
       Sect:=GetSection(i1);
       AfficherMessage(Format('%d : %d - %s - %s',[i1,Sect.Index, Sect.Topics, Sect.Title]));
     end;
     //*)
     // destruction du fichier temporaire
     if (FileExists(TEMPFILE)) then DeleteFile(TEMPFILE);
     //-------------------
     // c'est OK
     Result:=True;
   except
   end;
  finally
    CloseFile(pHelpFile);
  end;
end;

function THelpFileStructure.GetNbreSections: integer;
begin
  Result := FListeSections.Count;
end;

function THelpFileStructure.SearchIndexSection(const Topix: string): integer;
var
  i: integer;
  s: TSection;
begin
  AfficherMessage(Format('%s.SearchIndexSection(%s)',[ClassName, Topix]));
  Result:=-1;
  try
    // si le fichier est invalide, sortie avec échec
    if (GetNbreSections = 0) then Exit;
    // si le topic est vide, alors afficher l'aide par défaut
    if Trim(Topix)='' then begin
      FCurrentIndex:=0;
      Result:=0;
      Exit;
    end;
    for i:=0 to GetNbreSections - 1 do begin
      S:=GetSection(i);
      // insensibilité à la casse
      if (UpperCase(Trim(Topix)) = UpperCase(Trim(S.Topics))) then begin
        FCurrentIndex:=i;
        Result:=i;
        Exit;
      end;
    end;
  except
  end;
end;

end.




interface

implementation

end.
 
