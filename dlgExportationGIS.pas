unit dlgExportationGIS;
// Export vers SIG et le logiciel GHCaveDraw
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
  StructuresDonnees,
  {$IFDEF MSWINDOWS}
    {$IFDEF USE_CONVERTISSEUR_EPSG}
      ConversionsCoordonneesEPSG, // API Conversapi
    {$ENDIF}
    {$IFDEF USE_CONVERTISSEUR_ESIBERT}
      ConversionsCoordonneesESibert, // API ESIBERT
    {$ENDIF}
    {$IFDEF USE_CONVERTISSEUR_JPC}
      ConvertisseurJPC, // API JPC
    {$ENDIF}
  {$ENDIF}
  {$IFDEF LINUX}
    ConversionsCoordonneesEPSGLinux,
  {$ENDIF}

  Common,
  UnitEntites,
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, EditBtn, ColorBox, Buttons, curredit;
type TKeyValue = record
  K: string;
  V: string;
end;

type

  { TdlgExportSIG }

  TdlgExportSIG = class(TForm)
    BitBtn1: TBitBtn;
    btnProcess: TButton;
    btnSaveGIS: TButton;
    chkUseColorGroupes: TCheckBox;
    chkSilhouette: TCheckBox;
    editFileName: TEdit;
    editFiltres: TEdit;
    editPrefixStations: TEdit;
    editXo: TCurrencyEdit;
    editYo: TCurrencyEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lbCouleurSilhouette: TStaticText;
    LbStepProcess: TLabel;
    lsbSystemesCoordonnees: TListBox;
    pnlProgression: TPanel;
    ProgressBar1: TProgressBar;
    rdgrpTarget: TRadioGroup;
    procedure btnProcessClick(Sender: TObject);
    procedure btnSaveGISClick(Sender: TObject);
    procedure chkUseColorGroupesChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lbCouleurSilhouetteClick(Sender: TObject);
    procedure rdgrpTargetClick(Sender: TObject);
  private
    { private declarations }
    FTableEntites: TTableDesEntites;
    // convertisseur EPSG
    FConvertisseurEPSG: TConversionSysteme;
    FKVA : array[0..5] of TKeyValue;
    //FKVB : array[0..6] of TKeyValue;
    function  SetFileFilter(const Idx: integer): string;
    procedure SetExtensionFichier(const Idx: integer);
    procedure AfficherProgression(const Done, Starting, Ending: Int64; const Etape: string);
  public
    { public declarations }
    function Initialise(const FileName: string): boolean;
  end; 

var
  dlgExportSIG: TdlgExportSIG;

implementation

{$R *.lfm}

{ TdlgExportSIG }

procedure TdlgExportSIG.rdgrpTargetClick(Sender: TObject);
begin
  SetExtensionFichier(rdgrpTarget.ItemIndex);
  SetFileFilter(rdgrpTarget.ItemIndex);
end;

function TdlgExportSIG.SetFileFilter(const Idx: integer): string;
const QFMT = '%s (*.%s)|*.%s';
begin
  Result := Format(QFMT, [FKVA[Idx].K, FKVA[Idx].V, FKVA[Idx].V]);
end;

procedure TdlgExportSIG.SetExtensionFichier(const Idx: integer);

begin
  AfficherMessage(Format('SetFilterFichier: %d', [Idx]));
  editFileName.Text := ChangeFileExt(editFileName.Text, '.' + FKVA[Idx].V);

end;

procedure TdlgExportSIG.AfficherProgression(const Done, Starting, Ending: Int64; const Etape: string);
begin
   try
     ProgressBar1.Min := Starting;
     ProgressBar1.Max := Ending;
     ProgressBar1.Position := Done;
     LbStepProcess.Caption := Etape;
     Application.ProcessMessages;
   except
     ;
   end;
end;

procedure TdlgExportSIG.btnProcessClick(Sender: TObject);
var
  WU: TOutputFormatGIS;
  {$IFDEF USE_CONVERTISSEUR_EPSG}
     EWE: String;
  {$ENDIF}
  {$IFDEF USE_CONVERTISSEUR_ESIBERT}
     EWE: string;
  {$ENDIF}
  {$IFDEF USE_CONVERTISSEUR_JPC}
     EWE: string;
  {$ENDIF}

begin

  case rdgrpTarget.ItemIndex of
    0: WU := gisGHCAVEDRAW;
    1: WU := gisKML;
    2: wu := gisGPX;
    3: wu := gisOSM;
    4: wu := gisCARTO_EXPLOREUR;
    5: wu := gisMEMORY_MAP;
  end;
  {$IFDEF USE_CONVERTISSEUR_EPSG}
    //ConversionsCoordonneesEPSG, // API Conversapi

    EWE := lsbSystemesCoordonnees.Items[lsbSystemesCoordonnees.ItemIndex];
    AfficherMessage(Format('Pret a exporter vers GIS: %d entites',[FTableEntites.GetNbEntites]));
    FTableEntites.ExportForCarto(editFileName.Text,
                                 FConvertisseurEPSG,
                                 EWE,
                                 wu,
                                 chkSilhouette.Checked,
                                 editXo.Value, editYo.Value,
                                 trim(editFiltres.Text),
                                 trim(editPrefixStations.Text),
                                 lbCouleurSilhouette.Color,
                                 chkUseColorGroupes.Checked);

  {$ENDIF}
  {$IFDEF USE_CONVERTISSEUR_ESIBERT}
     EWE := lsbSystemesCoordonnees.Items[lsbSystemesCoordonnees.ItemIndex];
     FTableEntites.ExportForCarto(editFileName.Text,
                                 FConvertisseurEPSG,
                                 EWE,
                                 wu,
                                 chkSilhouette.Checked,
                                 editXo.Value, editYo.Value,
                                 trim(editFiltres.Text),
                                 trim(editPrefixStations.Text),
                                 lbCouleurSilhouette.Color,
                                 chkUseColorGroupes.Checked);
  {$ENDIF}
  {$IFDEF USE_CONVERTISSEUR_JPC}
     EWE := lsbSystemesCoordonnees.Items[lsbSystemesCoordonnees.ItemIndex];
     FTableEntites.ExportForCarto(editFileName.Text,
                                 FConvertisseurEPSG,
                                 EWE,
                                 wu,
                                 chkSilhouette.Checked,
                                 editXo.Value, editYo.Value,
                                 trim(editFiltres.Text),
                                 trim(editPrefixStations.Text),
                                 lbCouleurSilhouette.Color,
                                 chkUseColorGroupes.Checked);
  {$ENDIF}
end;

procedure TdlgExportSIG.btnSaveGISClick(Sender: TObject);
begin
  with TSaveDialog.Create(Application) do
  begin
    try
      Initialdir := ExtractFileDir(editFileName.Text);
      Filter     := SetFileFilter(rdgrpTarget.ItemIndex);
      Options    := Options + [ofOverwritePrompt];
      DefaultExt := ChooseString(rdgrpTarget.ItemIndex,
                                [FKVA[0].V,
                                 FKVA[1].V,
                                 FKVA[2].V,
                                 FKVA[3].V,
                                 FKVA[4].V,
                                 FKVA[5].V
                                ]);
      if (Execute) then editFileName.Text := FileName;
    finally
      Free;
    end;
  end;
end;

procedure TdlgExportSIG.chkUseColorGroupesChange(Sender: TObject);
var
  EWE: boolean;
begin
  EWE := not(chkUseColorGroupes.Checked);
  chkSilhouette.Enabled       := EWE;
  lbCouleurSilhouette.Enabled := EWE;
end;

procedure TdlgExportSIG.FormDestroy(Sender: TObject);
begin
  try
    FConvertisseurEPSG.Finaliser;

  finally
    FConvertisseurEPSG.Free;
  end;

end;

procedure TdlgExportSIG.lbCouleurSilhouetteClick(Sender: TObject);
begin
  lbCouleurSilhouette.Color := ChooseColor(lbCouleurSilhouette.Color);
end;


function TdlgExportSIG.Initialise(const FileName: string): boolean;
var
  i: integer;

  {$IFDEF USE_CONVERTISSEUR_EPSG}
     EWE: TParametresEPSG;
  {$ENDIF}
  {$IFDEF USE_CONVERTISSEUR_ESIBERT}
     EWE: String;
  {$ENDIF}
  {$IFDEF USE_CONVERTISSEUR_JPC}
     EWE: String;
  {$ENDIF}

begin
  Result := False;
  self.Caption := rsEXPORT_GIS;
  // nom de fichier
  AfficherMessage(Format('%s.Initialise(%s)',[ClassName, FileName]));
  // type de sortie
  FKVA[0].K := 'Polygonale pour GHCaveDraw'                ; FKVA[0].V := 'gcd';
  FKVA[1].K := 'Google Earth KML'                          ; FKVA[1].V := 'kml';
  FKVA[2].K := 'Standard GPX'                              ; FKVA[2].V := 'gpx';
  FKVA[3].K := 'OpenStreetMap'                             ; FKVA[3].V := 'osm';
  FKVA[4].K := 'Carto Exploreur (France uniquement)'       ; FKVA[4].V := 'csv';
  FKVA[5].K := 'Memory Map (France uniquement)'            ; FKVA[5].V := 'csv';
  // système de coordonnées
  FConvertisseurEPSG := TConversionSysteme.Create;
  if (not FConvertisseurEPSG.Initialiser) then
  begin
    ShowMessage(AnsiToUtf8(rsMSG_PROJ4S_FAIL));
    Exit;
  end;
  lsbSystemesCoordonnees.Clear;

  {$IFDEF USE_CONVERTISSEUR_EPSG}
    for i := 0 to FConvertisseurEPSG.GetNbreGrilles - 1 do
    begin
      EWE := FConvertisseurEPSG.GetGeoGrid(i);
      lsbSystemesCoordonnees.Items.Add(Format('EPSG:%d - %s',[EWE.CodeEPSG, EWE.Comments]));
    end;

  {$ENDIF}
  {$IFDEF USE_CONVERTISSEUR_ESIBERT}
    for i := 0 to FConvertisseurEPSG.GetNbSystemes - 1 do
    begin
      EWE := FConvertisseurEPSG.GetNomSysteme(i);
      lsbSystemesCoordonnees.Items.Add(EWE);
    end;
  {$ENDIF}
  {$IFDEF USE_CONVERTISSEUR_JPC}
    for i := 0 to FConvertisseurEPSG.GetNbSystemes - 1 do
    begin
      EWE := FConvertisseurEPSG.GetNomSysteme(i);
      lsbSystemesCoordonnees.Items.Add(EWE);
    end;
  {$ENDIF}

  lsbSystemesCoordonnees.ItemIndex := NATIONAL_GEODESIC_SYSTEM_IDX;
  FTableEntites := TTableDesEntites.Create;
  try
    FTableEntites.SetProcDisplayProgression(self.AfficherProgression);
    if (FTableEntites.LoadEntites(FileName, True) = 0) then
    begin
      ShowMessage(AnsiToUtf8(rsMSG_TABLE_ENTITY_FAIL));
      Exit;
    end;
    // remplissage des radiobox
    rdgrpTarget.Items.Clear;
    for i := 0 to High(FKVA) do
    begin
      rdgrpTarget.Items.Add(FKVA[i].K);
    end;
    rdgrpTarget.ItemIndex := 2; // pour GPX
    editFileName.Text := ChangeFileExt(FileName, '.gcd');
    SetExtensionFichier(0);
    // OK ?
    Result := True;
  except
  end;
end;

end.

