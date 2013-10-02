unit BarreMenuPpale;
{$INCLUDE CompilationParameters.inc}

// GHTopo version Free-Pascal: Barre principale
// Date: 19/04/2012
// Statut: Fonctionnel - Passage possible en contexte de production.
// Ajouts en cours au fil du portage

// DONE au 27/06/2012:
// - Sauvegarde et ouverture
// - Gestionnaire de données opérationnel
// - Centre d'impression
// - Export GPX et SIG OK
// - Fenêtres permanentes base et vue 2D
// - Visualisateur OpenGL
// - Convertisseur de coordonnées

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
  //UPCalc, // pour la calculette de barre d'outils
  Common,
  StructuresDonnees,
  ToporobotClasses2012,
  ClasseMaillage,
  CodeCalculTopo,
  CallDialogsStdVersion,
  {$IFDEF USE_FRONTAL_BDD_COMPACT} // frontal avec panneau 'magnétoscope'
     dlgProjectManagerCompact,
  {$ENDIF}
  {$IFDEF USE_FRONTAL_BDD_WIDESCREEN}
     dlgProjectManagerWideScreen,
  {$ENDIF}
  VueEnPlan,          // vue en plan (persistante)
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics,
  Dialogs, ActnList, Menus, ComCtrls, StdCtrls;

type

  { TBarreMenuPpal }

  TBarreMenuPpal = class(TForm)
    acQSave: TAction;
    acSaveAs: TAction;
    acOpen: TAction;
    acQuit: TAction;
    acNew: TAction;
    acPrint: TAction;
    acEditeur: TAction;
    acExportTherion: TAction;
    acRecents: TAction;
    acReload: TAction;
    acExportGraphique: TAction;
    acDisplayErrorsXTB: TAction;
    acCheckBase: TAction;
    acCompile: TAction;
    ac3DView: TAction;
    acRendu3D: TAction;
    acStats: TAction;
    acInfoCavite: TAction;
    acNodesCoordinates: TAction;
    acAbout: TAction;
    acHelpNews: TAction;
    acExportGIS: TAction;
    acExportGHCaveDraw: TAction;
    acSnapShot: TAction;
    Action1: TAction;
    Action2: TAction;
    acCloseDocument: TAction;
    Action3: TAction;
    Action4: TAction;
    Action5: TAction;
    acSandBox: TAction;
    acExportVTopo: TAction;
    acSibertConvertisseur: TAction;
    acHelpSystem: TAction;
    acToolCalculette: TAction;
    acVuePlan: TAction;
    ActionList1: TActionList;
    editCalculRapide: TEdit;
    ImageList1: TImageList;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    mnuHelp: TMenuItem;
    mnuTools: TMenuItem;
    mnuWindow: TMenuItem;
    mnuTopographie: TMenuItem;
    mnuFichier: TMenuItem;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    ToolButton17: TToolButton;
    ToolButton18: TToolButton;
    ToolButton19: TToolButton;
    ToolButton2: TToolButton;
    ToolButton20: TToolButton;
    ToolButton21: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton8: TToolButton;
    procedure ac3DViewExecute(Sender: TObject);
    procedure acAboutExecute(Sender: TObject);
    procedure acCloseDocumentExecute(Sender: TObject);
    procedure acCompileExecute(Sender: TObject);
    procedure acExportGISExecute(Sender: TObject);
    procedure acExportVTopoExecute(Sender: TObject);
    procedure acHelpSystemExecute(Sender: TObject);
    procedure acNewExecute(Sender: TObject);
    procedure acOpenExecute(Sender: TObject);
    procedure acPrintExecute(Sender: TObject);
    procedure acQuitExecute(Sender: TObject);
    procedure acRendu3DExecute(Sender: TObject);
    procedure acSandBoxExecute(Sender: TObject);
    procedure acSaveAsExecute(Sender: TObject);
    procedure acSibertConvertisseurExecute(Sender: TObject);
    procedure acStatsExecute(Sender: TObject);
    procedure acToolCalculetteExecute(Sender: TObject);
    procedure editCalculRapideKeyPress(Sender: TObject; var Key: char);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    // nom du fichiers de la base
    FNomFichierXTB: string;
    FNomFichierTOP: string;

    //FLastFolder  : string;
    // fenêtres de gestionnnaire de la base
    {$IFDEF USE_FRONTAL_BDD_COMPACT} // frontal avec panneau 'magnétoscope'
      VuePM      : TdlgProjectManagerStd;
    {$ENDIF}
    {$IFDEF USE_FRONTAL_BDD_WIDESCREEN}
      VuePM      : TdlgProjectManagerWdScr;
    {$ENDIF}
    VueEnPlan  : TfrmVue2D;
    // fenêtre de la vue en plan



    FCanDisplayPlan: Boolean;
    FDocumentToporobot: TToporobotStructure2012;
    //FCurrentFilePath: string;
    function CalculerLeReseau(const OutputTOP: string): boolean;
    procedure CloseAllDocuments;
    procedure ActiverMenus(const R90Mode: boolean);
    procedure ChargerLaTopo(const FC: string; const DoCalc: boolean);
    procedure CloseVues;
    procedure CreateByAssistant;
  public
    { public declarations }
  end; 

var
  BarreMenuPpal: TBarreMenuPpal;
  FDocTopoOpened : boolean; // doit être visible de partout
implementation
uses
  UEval,  // pur l'outil Calculette rapide
  dlgAssistantCavite, // assistant nouvelle cavité
  frmJournal, // console de suivi
  BacASable;

{ TBarreMenuPpal }
// wrapper pour le calcul du réseau
function TBarreMenuPpal.CalculerLeReseau(const OutputTOP: string): boolean;
var
  CodeCalcul: TCodeDeCalcul;
  EWE: Boolean;
begin
  AfficherMessage('CalculerLeReseau: '+ OutputTOP);
  Result := False;
  if (FDocTopoOpened) then
  begin
    CodeCalcul := TCodeDeCalcul.Create;
    try
      CodeCalcul.Initialiser(FDocumentToporobot, OutputTOP);
      EWE := CodeCalcul.Calculer;
      CodeCalcul.Finaliser;
      Result := EWE;
    finally
      CodeCalcul.Free;
    end;
  end;

end;

// création de topo via l'assistant
procedure TBarreMenuPpal.CreateByAssistant;
begin
  with TdlgAssistantNouveau.Create(Application) do begin
   try
    if (InitNewDocument) then
    begin
      ShowModal;
      if (DocCreated) then
      begin
        AfficherMessage(rsASSISTANT_SUCCESS);
        AfficherMessage(rsASSISTANT_RELOAD);
        ChargerLaTopo(GetXTBFileName(), True);
      end;
    end else
      ShowMessage(rsASSISTANT_ECHEC);
   finally
     Release;
   end;
  end;
end;

// fermer tous les documents
procedure TBarreMenuPpal.CloseAllDocuments;
begin
  if Not(FDocTopoOpened) then Exit;
  AfficherMessage('CloseAllDocuments');
  FDocTopoOpened:=False;
  FDocumentToporobot.ClearListeSeries;          // vidage des séries
  FDocumentToporobot.ViderTablesSimples;        // vidage autres tables

  //CloseVues;
  ActiverMenus(False);
  self.Caption:=Format(rsMAINMENUCAPTION,['Untitled']);
end;

// activer/désactiver options de menus
procedure TBarreMenuPpal.ActiverMenus(const R90Mode: boolean);
begin
  acCloseDocument.Visible            := R90Mode;

  acQSAVE.Visible            := R90Mode;
  acSaveAs.Visible           := R90Mode;

  acPrint.Visible            := R90Mode;
  acEditeur.Visible          := R90Mode;
  acExportVTopo.Visible            := R90Mode;
  acExportGIS.Visible        := R90Mode;
  acExportTherion.Visible    := R90Mode;
  acExportGHCaveDraw.Visible := R90Mode;


  acReload.Visible           := R90Mode;
  acCheckBase.Visible        := R90Mode;
  acExportGraphique.Visible  := R90Mode;
  acDisplayErrorsXTB.Visible := R90Mode;
  // acPlotting.Visible         := R90Mode; Traceur CN - Obsolète

  mnuTopographie.Visible     := R90Mode;
  mnuWindow.Visible          := R90Mode;

  acVuePlan.Visible          := R90Mode;
  ac3DView.Visible           := R90Mode;
  acCompile.Visible          := R90Mode;
  acExportGraphique.Visible  := R90Mode;
  acRendu3D.Visible          := R90Mode;
  acInfoCavite.Visible       := R90Mode;
  acStats.Visible            := R90Mode;

  //lbSystCoords.Visible       := R90Mode;
  acSnapShot.Visible         := R90Mode;
  //*)
end;

procedure TBarreMenuPpal.FormCloseQuery(Sender: TObject; var CanClose: boolean
  );
begin
  CanClose:=False;
  if MessageDlg(AnsiToUtf8(rsGHTOPO_QUIT), mtConfirmation, [mbYes, mbNo],0)=mrYES
  then begin
    if FDocTopoOpened then begin
      //CloseVues;
      //UpDateIni;
      FDocumentToporobot.Finaliser;
    end
    else
      ShowMessage(rsMSG_NOFILEOPENED);
    AfficherMessage(rsENDOFHADES);
    CanClose:=True;
  end;
end;

procedure TBarreMenuPpal.acQuitExecute(Sender: TObject);
begin
  self.Close;
end;

procedure TBarreMenuPpal.acRendu3DExecute(Sender: TObject);
begin
  if (FDocTopoOpened) then DisplayVue3DOpenGL(ChangeFileExt(FDocumentToporobot.GetDatabaseName, '.top'));
end;

procedure TBarreMenuPpal.acSandBoxExecute(Sender: TObject);
begin
  with TdlgBacASable.Create(Application) do
  begin
    try
      ShowModal;

    finally
      Release;
    end;
  end;
end;

procedure TBarreMenuPpal.acSaveAsExecute(Sender: TObject);
var
  EWE: String;
begin
  if (not FDocTopoOpened) then Exit;
  with TSaveDialog.Create(Application) do
  begin
    try
      InitialDir := ExtractFilePath(ParamStr(0));
      Filter := rsGHTOPO_FILE_FILTER;
      DefaultExt := '.xtb';
      Options := Options + [ofOverwritePrompt];
      if (Execute) then
      begin
        case FilterIndex of
          1: FDocumentToporobot.SaveToFile(FileName,mtabEXTENDEDTAB, tfWINDOWS);
          2: begin
               FDocumentToporobot.SaveToFile(FileName,mtabTOPOROBOT, tfMAC);
               EWE :=ChangeFileExt(FileName,'.xtb');
               ShowMessageFmt('Données complètes dans %s',[EWE]);
               FDocumentToporobot.SaveToFile(EWE, mtabEXTENDEDTAB, tfWINDOWS);
            end;
          3: FDocumentToporobot.SaveToXML(FileName); // futur format standard de GHTopo
        end; // case FilterIndex
      end;
    finally
      Free;
    end;
  end;

end;

procedure TBarreMenuPpal.acSibertConvertisseurExecute(Sender: TObject);
begin
  CallConvertisseurSibert;
end;

procedure TBarreMenuPpal.acStatsExecute(Sender: TObject);
begin
  if (FDocTopoOpened) then DisplayStatistiques(ChangeFileExt(FDocumentToporobot.GetDatabaseName, '.top'));
end;



procedure TBarreMenuPpal.acToolCalculetteExecute(Sender: TObject);
var
  Coords        : TPoint2Df;
  ResultatCalcul: double;
  Declimag      : double;
begin
  Coords.X := 0.00;
  Coords.Y := 0.00;
  ResultatCalcul := 0.00;
  Declimag       := 0.00;
  CallCalculette(editCalculRapide.Text, Coords, ResultatCalcul, Declimag);
end;


procedure TBarreMenuPpal.editCalculRapideKeyPress(Sender: TObject; var Key: char);
var
  YaErreur: boolean;
  WU: Double;
begin
  YaErreur := false;
  if (Key = #13) then
  begin
    WU := EvalExpr(trim(editCalculRapide.Text), YaErreur);
    if (YaErreur) then ShowMessage('Erreur dans la formule')
                  else editCalculRapide.Text := FloatToStr(WU);

  end;
end;

procedure TBarreMenuPpal.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin

end;

procedure TBarreMenuPpal.acOpenExecute(Sender: TObject);
begin
  if FDocTopoOpened then begin
    case MessageDlg(AnsiToUtf8(rsWARN_FILEALREADYOPEN),
                  mtConfirmation, [mbYes, mbNO],0) of
    mrYes: begin
             ;
           end;
    mrNO : Exit;
    end;
  end;

  with TOpenDialog.Create(Application) do
  begin
    try
      InitialDir:= ExtractFilePath(ParamStr(0));
      Filter := rsGHTOPO_FILE_FILTER; //'Fichiers GHTopo |*.xtb; *.tab; *.Text|Tous |*.*';
      FileName   := 'Topographie1.xtb';
      if (Execute) then
      begin
        CloseAllDocuments;
        ChargerLaTopo(Filename, QuestionOuiNon('Recalculer le réseau'));

        AfficherMessage('PRET');
      end;
    finally
      Free;
    end;
  end;
end;

procedure TBarreMenuPpal.acPrintExecute(Sender: TObject);
begin
  showmessage(FDocumentToporobot.GetDatabaseName);
  if (FDocTopoOpened) then DisplayCentreImpression(ChangeFileExt(FDocumentToporobot.GetDatabaseName, '.top'));
end;

procedure TBarreMenuPpal.ac3DViewExecute(Sender: TObject);
begin
  ShowMessage(FDocumentToporobot.GetDatabaseName);
  if (FDocTopoOpened) then DisplayVue3DGDI(ChangeFileExt(FDocumentToporobot.GetDatabaseName, '.top'));
end;

procedure TBarreMenuPpal.acAboutExecute(Sender: TObject);
begin
  APropos;
end;

procedure TBarreMenuPpal.acCloseDocumentExecute(Sender: TObject);
begin
  if FDocTopoOpened then
  begin
    case MessageDlg(rsSAVECHANGES, mtConfirmation,
                  [mbYes, mbNo, mbCancel],0) of
    mrYes: begin
             // sauvegarder ici
             // tout fermer
             CloseAllDocuments;

           end;
    mrNo : begin
             CloseAllDocuments;
           end;
    mrCancel: exit;
    end;
  end;
end;

procedure TBarreMenuPpal.acCompileExecute(Sender: TObject);
begin
  CalculerLeReseau(FNomFichierTOP);
  FCanDisplayPlan := VueEnPlan.InitialiserVueEnPlan(ChangeFileExt(FDocumentToporobot.GetDatabaseName, '.top'), False);
  if (FCanDisplayPlan) then
  begin
    VueEnPlan.Width  := Trunc(0.80 * Screen.Width);
    VueEnPlan.Height := Trunc(0.80 * Screen.Height);
    VueEnPlan.Left   := Screen.Width - 10 - VueEnPlan.Width;
    VueEnPlan.Top    := self.Height + 2;
    VueEnPlan.Show;
  end;

end;

procedure TBarreMenuPpal.acExportGISExecute(Sender: TObject);
begin
  if (Not FDocTopoOpened) then Exit;
  DisplayDlgExportGIS(ChangeFileExt(FDocumentToporobot.GetDatabaseName, '.top'));


end;

procedure TBarreMenuPpal.acExportVTopoExecute(Sender: TObject);
var
  FileNameVTOPO: string;
begin
  if (not FDocTopoOpened) then Exit;
  FileNameVTOPO := FDocumentToporobot.GetDatabaseName;
  with TSaveDialog.Create(Application) do
  begin
    try
      InitialDir := ExtractFilePath(ParamStr(0));
      Filter     := rsVTOPO_FILE_FILTER;
      DefaultExt := '.tro';
      Options    := Options + [ofOverwritePrompt];
      FileName   := ExtractFileNameOnly(FileNameVTOPO);
      if (execute) then FDocumentToporobot.ExportVisualTopo(FileName);
    finally
      free;
    end;
  end;

end;

procedure TBarreMenuPpal.acHelpSystemExecute(Sender: TObject);
begin
  DisplayHelpSystem('');
end;



procedure TBarreMenuPpal.acNewExecute(Sender: TObject);
begin
  if (FDocTopoOpened) then begin
    ShowMessage('Un document est déjà ouvert');
    Exit;
  end;
  CreateByAssistant;
end;


procedure TBarreMenuPpal.FormCreate(Sender: TObject);
begin
  // création du conteneur ToporobotStructure
  FDocumentToporobot := TToporobotStructure2012.Create;
  // création des fenêtres permanentes
  dlgProcessing := TdlgProcessing.Create(Application);
  {$IFDEF USE_FRONTAL_BDD_COMPACT} // frontal avec panneau 'magnétoscope'
     VuePM := TdlgProjectManagerStd.Create(Application);
  {$ENDIF}
  {$IFDEF USE_FRONTAL_BDD_WIDESCREEN}
     VuePM := TdlgProjectManagerWdScr.Create(Application);
  {$ENDIF}

  VueEnPlan := TfrmVue2D.Create(Application);
  // barre de calcul rapide
  //lbCalculetteRapide.Caption := rsCALCURAPIDE;
  //-------------------------------------------------

   AfficherMessage(rsCHOOSELANGUAGE);
  mnuFichier.Caption := AnsiToUTF8(rsMNU_FILE);
    acNew.Caption    := AnsiToUTF8(rsNEW);
    acOpen.Caption   := AnsiToUTF8(rsOPEN);
    acQSAVE.Caption  := AnsiToUTF8(rsSAVE);
    acSaveAs.Caption := AnsiToUTF8(rsSAVEAS);
    acCloseDocument.Caption  := AnsiToUTF8(rsCLOSE);
    acPrint.Caption  := AnsiToUTF8(rsPRINT);
    acEditeur.Caption:= AnsiToUTF8(rsEDITTAB);
    acExportVTopo.Caption  := AnsiToUTF8(rsVTOPO);
    acExportTherion.Caption := AnsiToUTF8(rsTHERION);
    acExportGIS.Caption     := AnsiToUtf8(rsEXPORT_GIS);

    acRecents.Caption:= AnsiToUTF8(rsRECENT_DOCS);
    acReload.Caption := AnsiToUTF8(rsRELOAD);
    acExportGraphique.Caption := AnsiToUTF8(rsEXPORTGRAPHIQUE);
    acDisplayErrorsXTB.Caption:= AnsiToUTF8(rsERRINFOXTB);
    acQuit.Caption   := AnsiToUTF8(rsGHTOPO_QUIT);
  mnuTopographie.Caption:= AnsiToUTF8(rsMNU_TOPOGRAPHIE);
    acCheckBase.Caption := AnsiToUTF8(rsCHECKBASE);
    acCompile.Caption:= AnsiToUTF8(rsCOMPILE);
    acVuePlan.Caption:= AnsiToUTF8(rsVUEPLAN);
    ac3DView.Caption := AnsiToUTF8(rsVUE3D);
    acRendu3D.Caption:= AnsiToUTF8(rsRENDU3D);
    acStats.Caption  := AnsiToUTF8(rsSTATISTIQUES);
    acInfoCavite.Caption:= AnsiToUTF8(rsINFOCAVITE);
    acNodesCoordinates.Caption := AnsiToUTF8(rsNODESCOORDINATES);
  mnuWindow.Caption  := AnsiToUtf8(rsMNU_WINDOW);
  mnuTools.Caption   := AnsiToUTF8(rsMNU_TOOLS);
    acToolCalculette.Caption  := AnsiToUTF8(rsDLG_CALC_TITLE);
  mnuHelp.Caption    := AnsiToUTF8(rsMNU_HELP);
    acAbout.Caption  := AnsiToUTF8(rsABOUT);
    acHelpNews.Caption := AnsiToUTF8(rsHLPNEWS);
    //acInfoCavite.Caption:=rsINFOCAVITE;
  // affecter aux bulles d'aide les captions
  afficherMessage(rsMSGASSIGNCAPTIONS);
  ActiverMenus(False);
  FDocTopoOpened := False;
end;

procedure TBarreMenuPpal.FormDestroy(Sender: TObject);
begin
  // destruction des fenêtres permanentes
  try
    FDocumentToporobot.Finaliser;
    showmessage('FDocuToporobot finalisé OK');
  finally
    FDocumentToporobot.Free;
    AfficherMessage('Fin GHTopo');


    dlgProcessing.Release;
    VuePM.Release;
    VueEnPlan.Release;
  end;

  inherited;
end;

// Chargement de la topo
procedure TBarreMenuPpal.ChargerLaTopo(const FC: string; const DoCalc: boolean);
begin
  FNomFichierXTB := FC;
  FNomFichierTOP := ChangeFileExt(FNomFichierXTB, '.top');
  //ShowMessage(FNomFichierXTB + ' - ' + FNomFichierTOP);

  with dlgProcessing do
  begin
    dlgProcessing.FormStyle:=fsStayOnTop; // mettre la console en avant-plan
    ListBox1.Clear;
    with FDocumentToporobot do
    begin
      //ReInitialiser;
      SetDatabaseName(FNomFichierXTB);
      // vérifier si c'est un fichier Text
      if (Pos('.Text', FC)>0) then
      begin
        if LoadFichierText(FC)<0 then
        begin
          ShowMessage('Le fichier comporte des erreurs - Voir le rapport');
          DisplayTextEditor(ExtractFilePath(ParamStr(0))+
                            ChangeFileExt(ExtractFileName(GetDatabaseName),'.err'),
                            True);
          // Echec = on détruit l'objet
          FDocumentToporobot.Finaliser;
          Exit;
        end;
      end
      else if (Pos('.gtx', FC) > 0) then // fichier GHTopo XML ?
      begin
        if (LoadFromXML(FC)<0) then
        begin
         // ShowMessage('Format XML en cours de programmation');
          (*
          ShowMessage('Le fichier comporte des erreurs - Voir le rapport');
          DisplayTextEditor(ExtractFilePath(ParamStr(0))+
                            ChangeFileExt(ExtractFileName(GetDatabaseName),'.err'),
                            True);
          //*)
          // Echec = on détruit l'objet
          FDocumentToporobot.Finaliser;
          Exit;
        end;
      end
      else
      begin // sinon, c'est un fichier supposé Tab ou XTB
        if LoadFichierTab(FC)<0 then
        begin
          ShowMessage('Le fichier comporte des erreurs - Voir le rapport');
          DisplayTextEditor(ExtractFilePath(ParamStr(0))+
                            ChangeFileExt(ExtractFileName(GetDatabaseName),'.err'),
                            True);
          // Echec = on détruit l'objet
          FDocumentToporobot.Finaliser;
          Exit;
        end;
      end;

      //ShowMessage(DatabaseName);
      self.Caption:=Format(rsMAINMENUCAPTION,
                           [ExtractFileName(FC)]);
      FDocTopoOpened:=True;
      Application.ProcessMessages;
      // calculer le réseau dans tous les cas
      if (DoCalc) then CalculerLeReseau(FNomFichierTOP);
      // afficher le gestionnaire de la BDD
      Application.ProcessMessages;
      AfficherMessage('Préparation du frontal de la base de données');
      with VuePM do
      begin

        {$IFDEF USE_FRONTAL_BDD_COMPACT} // frontal avec panneau 'magnétoscope'
          if (Not Init(FDocumentToporobot, 0, 0, 1, 1, 1, 0)) then
          begin
            ShowMessage(AnsiToUtf8(rsMSG_PROJ_MANAGER_FAIL));
          end;
        {$ENDIF}

        {$IFDEF USE_FRONTAL_BDD_WIDESCREEN} // frontal avec liste latérale permanente
          if (Not Init(FDocumentToporobot)) then
          begin
            ShowMessage(AnsiToUtf8(rsMSG_PROJ_MANAGER_FAIL));
          end;
        {$ENDIF}
        Left := 2;
        Top  := Screen.Height - VuePM.Height - 50;
        show;
      end;

      //(*** A REACTIVER APRES PORTAGE ***
      // afficher la vue en plan
      //Showmessage(FNomFichierTOP);
      FCanDisplayPlan := VueEnPlan.InitialiserVueEnPlan(FNomFichierTOP, False);
      if (FCanDisplayPlan) then
      begin
        VueEnPlan.Width  := Trunc(0.80 * Screen.Width);
        VueEnPlan.Height := Trunc(0.80 * Screen.Height);
        VueEnPlan.Left   := Screen.Width - 10 - VueEnPlan.Width;
        VueEnPlan.Top    := self.Height + 20;
        VueEnPlan.Show;
      end;
      // Journal
      dlgProcessing.Left    := 2;
      dlgProcessing.Top     := self.Height + 20;
      dlgProcessing.Height  := (Screen.Height - self.Height) div 2;
      //************************)
      ActiverMenus(True);
     end; // with FDocumentToporobot
   end; // with dlgProcessings
   //dlgProcessing.SetProgressWindow(10, Screen.Height - 120, 700, 70,fsNormal);
   //rendre visibles toutes les options des menus
   ActiverMenus(True);
   dlgProcessing.FormStyle:=fsNormal;
   Application.ProcessMessages;


end;


procedure TBarreMenuPpal.FormShow(Sender: TObject);
begin
  defaultFormatSettings.DecimalSeparator := '.';
  dlgProcessing.Show;
  with self do
  begin
    Top:=0;
    left:=0;
    Width:=Screen.Width;
    //ClientHeight:=ToolBar1.Height;
    ClientHeight:=60;
    FDocTopoOpened  := False;
    FCanDisplayPlan := False;
    Caption:=Format(rsMAINMENUCAPTION,['Untitled']);
    mnuTopographie.Visible:=False;
    mnuWindow.Visible:=False;
  end;
  // dossier courant

end;





procedure TBarreMenuPpal.CloseVues;
begin
  VueEnPlan.FinaliserVueEnPlan;
end;

{$R *.lfm}


end.

