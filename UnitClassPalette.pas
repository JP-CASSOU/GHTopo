unit UnitClassPalette;
// Date: 19/04/2012
// Statut: Fonctionnel


{$mode DELPHI}{$H+}
{$PACKRECORDS 1}
interface

uses

  {$IFDEF FRENCH_MESSAGES}
     UnitMessages_fr,
  {$ENDIF}
  {$IFDEF ENGLISH_MESSAGES}
     UnitMessages_en,
  {$ENDIF}
  {$IFDEF SPANISH_MESSAGES}
     UnitMessages_sp,
  {$ENDIF}
  StructuresDonnees,
  Common,
  Graphics,
  SysUtils, Classes;

type TPalette256 = class
    procedure GenerateWeb216Palette;
    procedure GenerateACADPalette;
    procedure GenerateWindowsPalette;
    procedure GenerateTOPOROBOTPalette;
    procedure GenerateGrayScalePalette;

    function  LoadPaletteFromFile: integer;
    procedure SavePaletteToFile(const FPAL: string);
    function  GetColorByIndex(const Idx: Integer):TColor;
    procedure Free;
  private
    FFichierPalette: string;
    FColorArray    : array[0..255] of TColor;

  public
    property FichierPalette: string read FFichierPalette
                                    write FFichierPalette;

end;

implementation

procedure TPalette256.GenerateTOPOROBOTPalette;
const
  M13107 = 13107;
  M4369  = 4369;
var
  M: TMacintoshColor;
  i,j,k,q,r: integer;
begin
  AfficherMessage(Format('%s.GenerateTOPOROBOTPalette',[ClassName]));
  for i:=0 to High(FColorArray) do FColorArray[i]:=clWhite;
  FColorArray[0]:=clWhite;
  FColorArray[1]:=clBlack;
  FColorArray[2]:=GetPCColor(30583, 30583, 30583);
  FColorArray[3]:=GetPCColor(21845, 21845, 21845);
  FColorArray[4]:=GetPCColor(65535, 65535, 0);
  FColorArray[5]:=GetPCColor(65535, 26214, 0); {5}
  FColorArray[6]:=GetPCColor(56797, 0, 0); {6}
  FColorArray[7]:=GetPCColor(65535, 0, 39321); {7}
  FColorArray[8]:=GetPCColor(26214, 0, 39321); {8}
  FColorArray[9]:=GetPCColor(0, 0, 56797); {9}
  FColorArray[10]:=GetPCColor(0, 39321, 65535); {10}
  FColorArray[11]:=GetPCColor(0, 61166, 0); {11}
  FColorArray[12]:=GetPCColor(0, 26214, 0); {12}
  for i:=1 to 2 do
    FColorArray[12+i]:=GetPCColor(M13107*(i+1), M13107 * i, M13107 * (i-1));
  FColorArray[15]:=GetPCColor(48059, 48059, 48059);
  for i:=16 to 19 do
    FColorArray[i]:=GetPCColor(5*M13107, 5*M13107, M13107 * (20-i));
  // palette calculée
  // jusqu'… la couleur 36
  q:=19;
  for i:=4 downto 2 do
    for j:=5 downto 0 do begin
      q:=q+1;
      FColorArray[q]:=GetPCColor(5*M13107, i*M13107, j*M13107);
    end;
  q:=36;
  for i:=5 downto 0 do begin
    q:=q+1;
    FColorArray[q]:=GetPCColor(5*M13107, M13107, i*13107);
  end;
  //**************** OK jusqu'… la couleur 42
  for i:=5 downto 4 do begin
    q:=q+1;
    FColorArray[q]:=GetPCColor(5*M13107,0, i*13107);
  end;
  for i:=2 downto 0 do begin
    q:=q+1;
    FColorArray[q]:=GetPCColor(5*M13107,0, i*13107);
  end;
  // OK jusqu'… la couleur 47
  for j:=5 downto 0 do
    for k:=5 downto 0 do begin
      q:=q+1;
      FColorArray[q]:=GetPCColor(4*M13107,j*M13107, k*M13107);
    end;
  // OK jusqu'… la couleur 83
  for j:=5 downto 3 do
    for k:=5 downto 0 do begin
      q:=q+1;
      FColorArray[q]:=GetPCColor(3*M13107,j*M13107, k*M13107);
    end;
  // OK jusqu'… la couleur 101
  for k:=5 downto 2 do begin
    q:=q+1;
    FColorArray[q]:=GetPCColor(3*M13107,2*M13107, k*M13107);
  end;
  q:=q+1;
  FColorArray[q]:=GetPCColor(3*M13107,2*M13107, 0);
  // OK jusqu'… la couleur 106
  for j:=1 downto 0 do
    for k:=5 downto 0 do begin
      q:=q+1;
      FColorArray[q]:=GetPCColor(3*M13107,j*M13107, k*M13107);
    end;
  // OK jusqu'… la couleur 118
  for j:=5 downto 1 do
    for k:=5 downto 0 do begin
      q:=q+1;
      FColorArray[q]:=GetPCColor(2*M13107,j*M13107, k*M13107);
    end;
  // OK jusqu'… la couleur 147
  q:=q-1;
  for k:=5 downto 4 do begin
    q:=q+1;
    FColorArray[q]:=GetPCColor(2*M13107,0, k*M13107);
  end;
  for k:=2 downto 0 do begin
    q:=q+1;
    FColorArray[q]:=GetPCColor(2*M13107,0, k*M13107);
  end;
  // OK jusqu'… la couleur 152.
  for j:=5 downto 0 do
    for k:=5 downto 0 do begin
      q:=q+1;
      FColorArray[q]:=GetPCColor(M13107,j*M13107, k*M13107);
    end;
  //OK jusqu'… la couleur 188
  for j:=5 downto 4 do
    for k:=5 downto 0 do begin
      q:=q+1;
      FColorArray[q]:=GetPCColor(0,j*M13107, k*M13107);
    end;
  //OK jusqu'… la couleur 200
  for k:=4 downto 0 do begin
    q:=q+1;
    FColorArray[q]:=GetPCColor(0, 3*M13107, k*M13107);
  end;
  //OK jusqu'… la couleur 205
  for k:=5 downto 1 do begin
    q:=q+1;
    FColorArray[q]:=GetPCColor(0, 2*M13107, k*M13107);
  end;
  for k:=5 downto 0 do begin
    q:=q+1;
    FColorArray[q]:=GetPCColor(0, M13107, k*M13107);
  end;
  for k:=5 downto 1 do begin
    q:=q+1;
    FColorArray[q]:=GetPCColor(0, 0, k*M13107);
  end;
  // OK jusqu'… la couleur 221
  q:=q+1; FColorArray[q]:=GetPCColor(61166, 0, 0);
  q:=q+1; FColorArray[q]:=GetPCColor(48059, 0, 0);
  r:=0;
  for i:=1 to 7 do begin
    q:=q+1;
    r:=r + IIF((i mod 2 <>0), M4369, 2*M4369);
    FColorArray[q]:=GetPCColor(48059 - r, 0, 0);
  end;
  // OK jusqu'… la couleur 230
  q:=q+1; FColorArray[q]:=GetPCColor(0, 56797, 0);
  q:=q+1; FColorArray[q]:=GetPCColor(0, 48059, 0);
  r:=0;
  for j:=1 to 7 do begin
    q:=q+1;
    r:=r + IIF((j mod 2 <>0), M4369, 2*M4369);
    FColorArray[q]:=GetPCColor(0, 48059 - r, 0);
  end;
  // OK jusqu'… la couleur 239
  q:=q+1; FColorArray[q]:=GetPCColor(0, 0, 61166);
  q:=q+1; FColorArray[q]:=GetPCColor(0, 0, 48059);
  r:=0;
  for j:=1 to 7 do begin
    q:=q+1;
    r:=r + IIF((j mod 2 <>0), M4369, 2*M4369);
    FColorArray[q]:=GetPCColor(0, 0,48059 - r);
  end;
  r:=61166; FColorArray[q+1]:=GetPCColor(r,r,r);
  r:=56797; FColorArray[q+2]:=GetPCColor(r,r,r);
  r:=43690; FColorArray[q+3]:=GetPCColor(r,r,r);
  r:=34952; FColorArray[q+4]:=GetPCColor(r,r,r);
  for i:=1 to 3 do begin
    r:=r div 2;
    FColorArray[q+4+i]:=GetPCColor(r,r,r);
  end;
end;
procedure TPalette256.GenerateGrayScalePalette;
var
  i: byte;
begin
 AfficherMessage(ClassName+'.GenerateGrayScalePalette');
 for i:=0 to High(FColorArray) do begin
   FColorArray[i]:=RGB(i,i,i);
 end;
end;


procedure TPalette256.GenerateWindowsPalette;
begin
  AfficherMessage(ClassName+'.GenerateWindowsPalette');
  {$IFDEF MSWINDOWS}
   // getpalette
  {$ENDIF}
  {$IFDEF FREEPASCAL}
    AfficherMessage('  --> Unimplemented generation by algorithm');
    AfficherMessage('  --> Use Windows.Palette file');

  {$ENDIF}
end;
procedure TPalette256.GenerateACADPalette;
var
  i: integer;
begin
  AfficherMessage(ClassName+'.GenerateACADPalette');
  for i:=0 to 255 do
     FColorArray[i]:=Acad2RGB(i);


end;
procedure TPalette256.GenerateWeb216Palette;
var
  i, j, k  : integer;
  r,g,b    : byte;
  IdxColor : integer;
begin
  AfficherMessage(ClassName+'.GenerateWeb216Palette');
  for i:=1 to 6 do
    for j:=1 to 6 do
      for k:=1 to 6 do
        begin
          IdxColor:=(i-1)*36+
                    (j-1)*6+
                    (k-1);
          r:=255 - (i-1) * 51;
          g:=255 - (j-1) * 51;
          b:=255 - (k-1) * 51;
          FColorArray[IdxColor]:=RGB(r,g,b);
        end;
   // compléter la palette avec des blancs
   for i:=216 to 255 do
     FColorArray[i]:=clSilver;
end;

function TPalette256.LoadPaletteFromFile: integer;
var
  i: integer;
  pPAL: TextFile;
  Lin : string;
  M   : TMacintoshColor;
  PrmsLn: array[0..5] of string;
  procedure ScanLine;
  var
    pn   : integer;
    ps  : integer;
    s,p : string;
    w1: integer;
  begin
    for pn:=0 to 5 do
      PrmsLn[pn]:='';
    pn:=0;
    Lin:=TRIM(Lin);
    repeat
     ps:=Pos(' ', Lin);
     s:=Copy(Lin,0, ps-1);
     PrmsLn[pn]:=s;
     Inc(pn);
     Lin:=Copy(Lin, 1+ps, Length(Lin));
     //ShowMessage(Lin);
    until ps=0;
    PrmsLn[pn-1]:=Lin;
  end;
begin
  AfficherMessage(Format('%s.LoadPaletteFromFile(%s)',
                         [ClassName, FFichierPalette]));
  try
    Result:=-1;
    if Not(FileExists(FFichierPalette)) then begin
      Result:=-1;
      Exit;
    end;
    AssignFile(pPAL, FFichierPalette);
    Reset(pPAL);
    FColorArray[0]:=clWhite;
    FColorArray[1]:=clBlack;
    i:=1;

    while Not Eof(pPAL) do begin
      ReadLn(pPAL, Lin);
      ScanLine;
      M.R:=StrToIntDef(PrmsLn[0],0);
      M.G:=StrToIntDef(PrmsLn[1],0);
      M.B:=StrToIntDef(PrmsLn[2],0);
      Inc(i);
      FColorArray[i]:=GetPCColor(M);
    end;
    Result:=1;
  finally
    CloseFile(pPAL);
  end;
End;

procedure TPalette256.SavePaletteToFile(const FPAL: string);
var
  pPAL: TextFile;
  i: integer;
  M: TMacintoshColor;
begin
  AfficherMessage(ClassName+'.SavePaletteToFile');
  try
    AssignFile(pPAL, FPAL);
    ReWrite(pPAL);
    for i:=2 to 255 do begin
      M:=GetMacColor(FColorArray[i]);
      WriteLn(pPAL,Format('%d %d %d {%d}',[M.R, M.G, M.B,i]));
    end;
  finally
    CloseFile(pPAL);
  end;
end;
function TPalette256.GetColorByIndex(const Idx: integer):TColor;
var t: integer;
begin
  if (Idx>=0) or (Idx<255) then
    Result:=FColorArray[Idx]
  else
    Result:=clSilver;
end;

procedure TPalette256.Free;
begin
  AfficherMessage(ClassName+'.Free');
  inherited Free;
end;
end.

