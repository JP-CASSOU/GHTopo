unit CadreFiltres;
// Saisie du MétaFiltre
// Date: 26/06/2012
// Statut: Nouveauté - Développement en cours
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
  Common,
  CallDialogsStdVersion,
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls;

type TProcWithoutParams = procedure of object;
type

  { TCdrMetaFiltre }

  TCdrMetaFiltre = class(TFrame)
    btnHelpFiltres: TButton;
    btnApply: TButton;
    editFiltres: TEdit;
    Label1: TLabel;
    Panel1: TPanel;
    procedure btnApplyClick(Sender: TObject);
    procedure btnHelpFiltresClick(Sender: TObject);
    procedure editFiltresKeyPress(Sender: TObject; var Key: char);
  private
    FProcApplyMetaFiltre : TProcWithoutParams;
  public
    { public declarations }
    procedure SetProcApplyMetaFiltre(const FP: TProcWithoutParams);
    procedure SetFiltre(const MyFiltre: string);
    function  GetFiltre: string;

  end; 

implementation

{$R *.lfm}

{ TCdrMetaFiltre }

procedure TCdrMetaFiltre.btnHelpFiltresClick(Sender: TObject);
begin
  DisplayHelpSystem('METAFILTRE');
end;

procedure TCdrMetaFiltre.editFiltresKeyPress(Sender: TObject; var Key: char);
begin
  try
    if (Key = #13) then FProcApplyMetaFiltre;
  except
  end;
end;

procedure TCdrMetaFiltre.btnApplyClick(Sender: TObject);
begin
  AfficherMessage('666');
  try
    FProcApplyMetaFiltre;
  except

  end;
end;

procedure TCdrMetaFiltre.SetProcApplyMetaFiltre(const FP: TProcWithoutParams);
begin
  FProcApplyMetaFiltre := FP;
end;

procedure TCdrMetaFiltre.SetFiltre(const MyFiltre: string);
begin
  editFiltres.Text := MyFiltre;
end;

function TCdrMetaFiltre.GetFiltre: string;
begin
  Result := Trim(editFiltres.Text);
end;

end.

