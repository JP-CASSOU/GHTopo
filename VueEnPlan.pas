unit VueEnPlan;
// Date: 19/04/2012
// Statut: Fonctionnel - Portage en cours
// TODO: Continuer le portage
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
  CallDialogsStdVersion,
  Common,
  StructuresDonnees,
  CdrDessin2D,
  CadreHistoAltitudes,
  UnitEntites,
  ClasseMaillage,
  Classes,
  SysUtils, FileUtil, Forms,
  Controls, Graphics, Dialogs, ComCtrls, ActnList, StdCtrls, ExtCtrls, Buttons;



type

  { TfrmVue2D }

  TfrmVue2D = class(TForm)
    acPanVue: TAction;
    acMetaFiltre: TAction;
    acDistance: TAction;
    acParametresVue: TAction;
    acHistoDirections: TAction;
    acHistoAltitudes: TAction;
    acRefreshVue: TAction;
    acPrint: TAction;
    acChargerMaillage: TAction;
    acVue3DFiltree: TAction;
    acFindstation: TAction;
    acVue3DOpenGLFiltree: TAction;
    acZoomWindow: TAction;
    acZoomAll: TAction;
    ActionList1: TActionList;
    CadreDessin2D1: TCadreDessin2D;
    CdrHistoAltitudes1: TCdrHistoAltitudes;
    ImageList1: TImageList;
    SpeedButton1: TSpeedButton;
    SpeedButton10: TSpeedButton;
    SpeedButton11: TSpeedButton;
    SpeedButton12: TSpeedButton;
    SpeedButton13: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SpeedButton8: TSpeedButton;
    SpeedButton9: TSpeedButton;
    OngletsVues: TTabControl;
    procedure acChargerMaillageExecute(Sender: TObject);
    procedure acDistanceExecute(Sender: TObject);
    procedure acFindstationExecute(Sender: TObject);
    procedure acHistoAltitudesExecute(Sender: TObject);
    procedure acHistoDirectionsExecute(Sender: TObject);
    procedure acMetaFiltreExecute(Sender: TObject);
    procedure acPanVueExecute(Sender: TObject);
    procedure acParametresVueExecute(Sender: TObject);
    procedure acRefreshVueExecute(Sender: TObject);
    procedure acPrintExecute(Sender: TObject);
    procedure acVue3DFiltreeExecute(Sender: TObject);
    procedure acVue3DOpenGLFiltreeExecute(Sender: TObject);
    procedure acZoomAllExecute(Sender: TObject);
    procedure acZoomWindowExecute(Sender: TObject);
    procedure editFiltresChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormShow(Sender: TObject);
    procedure OngletsVuesChange(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
  private
    { private declarations }
    // tag indiquant si on peut fermer la fenêtre
    FCanCloseWindow: Boolean;
    FNomFichierTOP: string;
    FMonMaillage: TMaillage;
    procedure DispVues3DMetafiltrees(const M: TTypeVue3D);
  public
    { public declarations }
    function  InitialiserVueEnPlan(const Filename: string; const CanCloseWindow: boolean): boolean;
    procedure FinaliserVueEnPlan;
  end; 

var
  frmVue2D: TfrmVue2D;

implementation

{$R *.lfm}

procedure TfrmVue2D.acZoomAllExecute(Sender: TObject);
begin
  CadreDessin2D1.ResetVue;
end;

procedure TfrmVue2D.acZoomWindowExecute(Sender: TObject);
begin
  if (CadreDessin2D1.GetCanDraw) then CadreDessin2D1.SetModeTravail(mtZOOM);
end;





procedure TfrmVue2D.editFiltresChange(Sender: TObject);
begin
end;

procedure TfrmVue2D.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if (FCanCloseWindow) then
  begin
    CanClose := QuestionOuiNon('Quit 2D Viewer');
  end
  else
  begin
    CanClose:=False;
    ShowMessage(AnsiToUtf8(rsNOCANCLOSEWND));
  end;
end;

procedure TfrmVue2D.FormShow(Sender: TObject);
begin
  self.Position:= poDesigned;
  self.top     := 20;
  self.Height  := Screen.Height - 80 - self.Top;
  self.Width   := Screen.Height;
  self.Left    := Screen.Width - self.Width - 20;


end;

procedure TfrmVue2D.OngletsVuesChange(Sender: TObject);
begin
  CadreDessin2D1.SetCurrentIdxOnglet(OngletsVues.TabIndex);
end;

procedure TfrmVue2D.SpeedButton3Click(Sender: TObject);
begin

end;




procedure TfrmVue2D.acMetaFiltreExecute(Sender: TObject);
begin
end;

procedure TfrmVue2D.acDistanceExecute(Sender: TObject);
begin
  if (CadreDessin2D1.GetCanDraw) then CadreDessin2D1.SetModeTravail(mtDISTANCE);
end;

procedure TfrmVue2D.acChargerMaillageExecute(Sender: TObject);
begin
  with TOpenDialog.Create(Application) do
  begin
    try
      InitialDir := ExtractFilePath(ParamStr(0));
      Defaultext := '.mai';
      Filter     := 'Fichiers maillages (*.mai)|*.mai|Tous|*.*';
      if (Execute) then
      begin
        if (FMonMaillage.LireFichierMaillage(FileName) >= 0) then
        begin
          CadreDessin2D1.SetMaillagePointer(FMonMaillage);
        end;
      end;
    finally
    end;
  end;
end;

procedure TfrmVue2D.acFindstationExecute(Sender: TObject);
begin
  CadreDessin2D1.FindAndCenterStation();
end;

procedure TfrmVue2D.acHistoAltitudesExecute(Sender: TObject);
var
  EWE: TTableDesEntites;
  WU: String;
begin
  CdrHistoAltitudes1.Visible := not CdrHistoAltitudes1.Visible;
  if (CdrHistoAltitudes1.Visible) then
  begin
    EWE := CadreDessin2D1.GetPointerTableEntites;
    WU  := CadreDessin2D1.GetMetaFiltre;
    CdrHistoAltitudes1.Initialise(EWE, WU);
  end;
end;

procedure TfrmVue2D.acHistoDirectionsExecute(Sender: TObject);
begin
  CadreDessin2D1.DisplayOrHideHistoDirections();
end;

procedure TfrmVue2D.acPanVueExecute(Sender: TObject);
begin
  if (CadreDessin2D1.GetCanDraw) then CadreDessin2D1.SetModeTravail(mtPANVUE);
end;

procedure TfrmVue2D.acParametresVueExecute(Sender: TObject);
var
  WU: TVue2DParams;
  Idx: integer;
begin
  Idx := CadreDessin2D1.GetCurrentOngletIdx;
  WU  := CadreDessin2D1.GetOngletByIndex(Idx);
  WU := ParametrerOngletVue2D(WU);
  CadreDessin2D1.PutOngletByIndex(Idx, WU);
  CadreDessin2D1.RedessinEcran;
end;

procedure TfrmVue2D.acRefreshVueExecute(Sender: TObject);
begin
  CadreDessin2D1.RedessinEcran;
end;

procedure TfrmVue2D.acPrintExecute(Sender: TObject);
begin
  DisplayCentreImpression(FNomFichierTOP);
end;


procedure TfrmVue2D.acVue3DFiltreeExecute(Sender: TObject);
begin
  DispVues3DMetafiltrees(tv3dGDI);
end;

procedure TfrmVue2D.acVue3DOpenGLFiltreeExecute(Sender: TObject);
begin
  DispVues3DMetafiltrees(tv3dOPENGL);
end;

function TfrmVue2D.InitialiserVueEnPlan(const Filename: string; const CanCloseWindow: boolean): boolean;
var
  c1, c2: TPoint3Df;
  o: Integer;
  Ong: TVue2DParams;
begin
  FNomFichierTOP := Filename;
  FCanCloseWindow:= CanCloseWindow;
  self.Caption := AnsiToUtf8(EnlevePerluete(rsVUEPLAN)) + ': ' + ExtractFileName(Filename);
  Result := CadreDessin2D1.Initialize(FileName);
  c1 := CadreDessin2D1.GetXYZMini;
  c2 := CadreDessin2D1.GetXYZMaxi;
  // onglets
  OngletsVues.Tabs.Clear;
  for o := 0 to CadreDessin2D1.GetNbOngletsVues - 1 do
  begin
    Ong := CadreDessin2D1.GetOngletByIndex(o);
    OngletsVues.Tabs.Add(Ong.ongName);
  end;
  // objet maillage
  FMonMaillage := TMaillage.Create;
  FMonMaillage.Initialiser;
end;

procedure TfrmVue2D.FinaliserVueEnPlan;
begin
  CadreDessin2D1.Finalise;
  FMonMaillage.Finaliser;
  FMonMaillage.Free;
end;
// Afficher les vues 3D métafiltrées en mode OGL ou GDI
procedure TfrmVue2D.DispVues3DMetafiltrees(const M: TTypeVue3D);
var
  TmpFileTOP: string;
  EWE: LongInt;
begin
  TmpFileTOP:=ExtractFilePath(ParamStr(0)) + '_FilteredFile.top';
  EWE := CadreDessin2D1.Generate3DMetafiltered(TmpFileTop);

  if (EWE > 0) then
  begin
    case M of
      tv3dGDI   : DisplayVue3DGDI(TmpFileTOP);
      tv3dOPENGL: DisplayVue3DOpenGL(TmpFileTOP);
    end;

  end else
  begin
    ShowMessage(Format('Echec en génération de %s (%d)',[TmpFileTOP, EWE]));

  end;
end;

end.

