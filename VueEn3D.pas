unit VueEn3D;
// Date: 18/06/2013
// Statut: Fonctionnel mais portage non terminé
// DONE: Les contrôleurs de la vue doivent être remaniés
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
  Common, CallDialogsStdVersion, Classes, SysUtils, FileUtil, Forms, Controls,
  Graphics, Dialogs, ExtCtrls, StdCtrls, Spin, Buttons, ActnList, CadreVue3D,
  CadreFiltres, CadreHistoAltitudes, UnitEntites, pagemngr, curredit;

type

  { TdlgVue3D }

  TdlgVue3D = class(TForm)
    acRedess: TAction;
    acGraphicAltitudes: TAction;
    acVue3D: TAction;
    acVueYZ: TAction;
    acVueXZ: TAction;
    acVueXY: TAction;
    ActionList1: TActionList;
    BitBtn1: TBitBtn;
    btnRegenererVue: TButton;
    CdrHistoAltitudes1: TCdrHistoAltitudes;
    CdrMetaFiltre1: TCdrMetaFiltre;
    CdrVue3D1: TCdrVue3D;
    editMagnification: TCurrencyEdit;
    ImageList1: TImageList;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Panel1: TPanel;
    editTheta: TSpinEdit;
    editPhi: TSpinEdit;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    procedure acGraphicAltitudesExecute(Sender: TObject);
    procedure acRedessExecute(Sender: TObject);
    procedure acVue3DExecute(Sender: TObject);
    procedure acVueXYExecute(Sender: TObject);
    procedure acVueXZExecute(Sender: TObject);
    procedure acVueYZExecute(Sender: TObject);
    procedure btnRegenererVueClick(Sender: TObject);
    procedure editPhiChange(Sender: TObject);
    procedure editPhiKeyPress(Sender: TObject; var Key: char);
    procedure editThetaChange(Sender: TObject);
    procedure editThetaClick(Sender: TObject);
    procedure editThetaKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
  private
    procedure ReconstruireVue;

    procedure RedessVue3D;
    procedure PreSetVue3D(const QTheta, QPhi: double);

  public
    { public declarations }
    function InitaliserVue3D(const FileName: string): boolean;

  end; 

var
  dlgVue3D: TdlgVue3D;

implementation

{$R *.lfm}


procedure TdlgVue3D.btnRegenererVueClick(Sender: TObject);
begin
  RedessVue3D;
end;

procedure TdlgVue3D.acRedessExecute(Sender: TObject);
begin
  RedessVue3D;
end;

procedure TdlgVue3D.acGraphicAltitudesExecute(Sender: TObject);
begin
  CdrHistoAltitudes1.Visible := not CdrHistoAltitudes1.Visible;
end;

procedure TdlgVue3D.acVue3DExecute(Sender: TObject);
begin
  PreSetVue3D(45, 32);
end;

procedure TdlgVue3D.acVueXYExecute(Sender: TObject);
begin
  PreSetVue3D(270, 90);
end;

procedure TdlgVue3D.acVueXZExecute(Sender: TObject);
begin
  PreSetVue3D(270, 0);
end;

procedure TdlgVue3D.acVueYZExecute(Sender: TObject);
begin
  PreSetVue3D(0, 0);
end;

procedure TdlgVue3D.editPhiChange(Sender: TObject);
begin
  RedessVue3D;
end;


procedure TdlgVue3D.editPhiKeyPress(Sender: TObject; var Key: char);
begin
  if (key = #13) then RedessVue3D;
end;

procedure TdlgVue3D.editThetaChange(Sender: TObject);
begin
  RedessVue3D;
end;

procedure TdlgVue3D.editThetaClick(Sender: TObject);
begin

end;


procedure TdlgVue3D.editThetaKeyPress(Sender: TObject; var Key: char);
begin
  if (key = #13) then RedessVue3D;
end;

procedure TdlgVue3D.FormShow(Sender: TObject);
begin
  CdrMetaFiltre1.SetProcApplyMetaFiltre(RedessVue3D);
  CdrMetaFiltre1.SetFiltre('');
  editTheta.Value     := 45;
  editPhi.value       := 31;
  editMagnification.Value := 1.00;
  ReconstruireVue;

end;

procedure TdlgVue3D.ReconstruireVue;
begin
  RedessVue3D;
end;

procedure TdlgVue3D.RedessVue3D;
var
  EWE: TTableDesEntites;
  WU : String;
begin
  EWE := CdrVue3D1.GetPointerTableEntites();
  WU  := CdrMetaFiltre1.GetFiltre();
  CdrHistoAltitudes1.Initialise(EWE, WU);
  CdrVue3D1.RegenererVue(editTheta.Value, editPhi.Value, 1.00, editMagnification.Value, WU, True);
end;

procedure TdlgVue3D.PreSetVue3D(const QTheta, QPhi: double);
begin
  CdrVue3D1.RegenererVue(QTheta, QPhi, 1.00, editMagnification.Value, CdrMetaFiltre1.GetFiltre(), True);
  editTheta.Value := trunc(QTheta);
  editPhi.Value   := trunc(QPhi);
end;


function TdlgVue3D.InitaliserVue3D(const FileName: string): boolean;
begin
  self.Caption := AnsiToUtf8(EnlevePerluete(rsRENDU3D)) + ': ' + ExtractFileName(Filename);
  Result := CdrVue3D1.Initialise(FileName);
end;


end.

