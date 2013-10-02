unit CadreReseau;
// Date: 26/04/2012
// Statut: Prêt à être incorporé
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
  Common, StructuresDonnees,
  CallDialogsStdVersion,
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, curredit;

type

  { TCdrReseaux }

  TCdrReseaux = class(TFrame)
    cmbTypeReseau: TComboBox;
    editNomReseau: TEdit;
    editObsReseau: TEdit;
    editIdxReseau: TCurrencyEdit;
    lbObsReseau: TLabel;
    lbNomReseau: TLabel;
    lbColorReseau: TLabel;
    lbIdxReseau: TLabel;
    lbTypeReseau: TLabel;
    btnColorReseau: TStaticText;
    procedure btnColorReseauClick(Sender: TObject);
    procedure FrameEnter(Sender: TObject);
  private
    { private declarations }
    { Déclarations privées }
    FReseau: TReseau;
    FModifie: boolean;

    procedure InitCaptions;
    procedure PutReseauInForm;

  public
    { public declarations }
    procedure SetReseau(const R: TReseau; const DoInitCaptions: boolean);
    function GetReseauFromForm: TReseau;
    function  IsModified: boolean;
    procedure SetModified(const m: boolean);
  end; 

implementation

{$R *.lfm}
procedure TCdrReseaux.SetReseau(const R: TReseau; const DoInitCaptions: boolean);
begin
  FReseau := R;
  if (DoInitCaptions) then InitCaptions;
  PutReseauInForm;
end;

procedure TCdrReseaux.InitCaptions;
begin
  lbIdxReseau.Caption    := AnsiToUtf8(rsCDR_RESEAU_LBIDX); ;
  lbNomReseau.Caption    := AnsiToUtf8(rsCDR_RESEAU_NAME);
  lbObsReseau.Caption    := AnsiToUtf8(rsLBL_COMMENTS);
  lbColorReseau.Caption  := AnsiToUtf8(rsCOLOR);
  lbTypeReseau.Caption   := AnsiToUtf8(rsCDR_RESEAU_TYPE);
  with cmbTypeReseau do begin
    Clear;
    Items.Add(AnsiToUtf8(rsCDR_RESEAU_CB0));
    Items.Add(AnsiToUtf8(rsCDR_RESEAU_CB1));
    Items.Add(AnsiToUtf8(rsCDR_RESEAU_CB2));
    Items.Add(AnsiToUtf8(rsCDR_RESEAU_CB3));
    Items.Add(AnsiToUtf8(rsCDR_RESEAU_CB4));
    Items.Add(AnsiToUtf8(rsCDR_RESEAU_CB5));
    Items.Add(AnsiToUtf8(rsCDR_RESEAU_CB6));
    ItemIndex := 0;
  end;
end;
procedure TCdrReseaux.PutReseauInForm;
begin
  FModifie := False;
  editIdxReseau.AsInteger:= FReseau.IdxReseau;
  btnColorReseau.Color   := FReseau.ColorReseau;
  editNomReseau.Text     := AnsiToUtf8(FReseau.NomReseau);
  editObsReseau.Text     := AnsiToUtf8(FReseau.ObsReseau);
  cmbTypeReseau.ItemIndex := FReseau.TypeReseau;
end;

procedure TCdrReseaux.btnColorReseauClick(Sender: TObject);
begin
  btnColorReseau.Color := ChooseColor(btnColorReseau.Color);
end;

procedure TCdrReseaux.FrameEnter(Sender: TObject);
begin
  FModifie := True;
  AfficherMessage('Element modifie');
end;

function  TCdrReseaux.GetReseauFromForm: TReseau;
begin
  with Result do begin
    IdxReseau     := editIdxReseau.AsInteger;
    ColorReseau   := btnColorReseau.Color;
    NomReseau     := editNomReseau.Text;
    ObsReseau     := editObsReseau.Text;
    TypeReseau    := cmbTypeReseau.ItemIndex;
  end;
  FReseau := Result;
end;

function TCdrReseaux.IsModified: boolean;
begin
  Result := FModifie;
end;

procedure TCdrReseaux.SetModified(const m: boolean);
begin
  FModifie:=m;
end;

end.

