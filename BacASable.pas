unit BacASable;
{$INCLUDE CompilationParameters.inc}
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
  StructuresDonnees, ToporobotClasses2012, ObjetSerie, CodeCalculTopo, Common,
  CallDialogsStdVersion, CadreSerieIndependant, Classes, SysUtils, FileUtil,
  curredit, Forms, Controls, Graphics, Dialogs, StdCtrls, FileCtrl,
  PairSplitter, ComCtrls, ExtCtrls, Grids,
  // pour la déclinaison magnétique
  {$IFDEF MSWINDOWS}
    UnitWrapperDeclimag,
  {$ENDIF}

  {$IFDEF LINUX}
    UnitWrapperDeclimagLinux,
  {$ENDIF}


  CadreSelectThetaPhi;

type

  { TdlgBacASable }

  TdlgBacASable = class(TForm)
    btnSelectAttrTexte: TButton;
    btnDeclimag: TButton;
    btnCoordonneesIsolees: TButton;
    btnConvertTableauCoords: TButton;
    btnCalculDeclimags: TButton;
    Button15: TButton;
    Button16: TButton;
    CdrTriedreThetaPhi1: TCdrTriedreThetaPhi;
    editTheta: TCurrencyEdit;
    editPhi: TCurrencyEdit;
    editCoordX: TCurrencyEdit;
    editCoordY: TCurrencyEdit;
    editEPSG: TCurrencyEdit;
    GroupBox1: TGroupBox;
    PageControl1: TPageControl;
    TabSheet3: TTabSheet;
    procedure btnSelectAttrTexteClick(Sender: TObject);
    procedure Button15Click(Sender: TObject);
    procedure Button16Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
    FDocTopo: TToporobotStructure2012;
    FCodeCalcul: TCodeDeCalcul;
    procedure SetThetaPhiFromCdrRefXYZ;
    procedure ActionOnExitRefXYZ;


    //function toto: TPoint2Df
  public
  end;

var
  dlgBacASable: TdlgBacASable;

implementation

{$R *.lfm}

{ TdlgBacASable }

procedure TdlgBacASable.SetThetaPhiFromCdrRefXYZ;
begin
  //AfficherMessage('SetThetaPhiFromCdrRefXYZ');
  editTheta.Value := CdrTriedreThetaPhi1.GetTheta;
  editPhi.Value   := CdrTriedreThetaPhi1.GetPhi;
end;

procedure TdlgBacASable.ActionOnExitRefXYZ;
begin
  //ShowMessage('ActionOnExitRefXYZ');
end;

procedure TdlgBacASable.FormCreate(Sender: TObject);
begin
  FDocTopo := TToporobotStructure2012.Create;
  FDocTopo.ReInitialiser(True);
end;



procedure TdlgBacASable.btnSelectAttrTexteClick(Sender: TObject);
var
  EWE: TTexteAttributs;
begin
  EWE.FontName     := 'Arial';
  EWE.FontColor    := clRed;
  EWE.FontStyle    := [fsBold, fsItalic];
  EWE.BackColor    := clYellow;
  EWE.HauteurTexte := 3.50;
  EWE.Position     := 7;
  EWE.AngleRot     := 62;

  EWE := SelectAttributsTexte(EWE);

end;



procedure TdlgBacASable.Button15Click(Sender: TObject);
begin
  CdrTriedreThetaPhi1.SetParamsTriedre(editTheta.Value, editPhi.Value);
end;

procedure TdlgBacASable.Button16Click(Sender: TObject);
begin
  CdrTriedreThetaPhi1.SetProcRefreshXY(SetThetaPhiFromCdrRefXYZ);
  CdrTriedreThetaPhi1.SetProcDoAction(ActionOnExitRefXYZ);
end;



procedure TdlgBacASable.FormDestroy(Sender: TObject);
begin
  FDocTopo.Finaliser;
  FDocTopo.Free;
end;


end.

