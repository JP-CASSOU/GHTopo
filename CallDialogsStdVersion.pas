unit CallDialogsStdVersion;
 // CallDialogs pour la version std de GHTopo

 // Date: 19/04/2012
 // Statut: Fonctionnel
 // Ajouts en cours au fil du portage
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
  StructuresDonnees,
  ToporobotClasses2012,
  UnitEntites,
  Forms, Dialogs, Types, Controls, Graphics,
  Classes, SysUtils;
// Sauvegarde de fichier
function DlgSaveAs(const TitreDlg, QInitialDir, QFilter, QDefaultExt: string;
                   var QFileName: string): boolean;
// Question Oui-Non
function QuestionOuiNon(const Msg: string): boolean;
// appel de l'outil Calculette
function CallCalculette(const Expression: string; var Coords: TPoint2Df; out ResultatCalcul: double; out Declimag: double): boolean;

// éditeur de texte interne
procedure DisplayTextEditor(const FileTexte: string; const EditMode: boolean);
// sélection expé, code, ... dans liste
function  SelectionDansListe(const DT: TToporobotStructure2012;
                            const Mode: TModeSelectionListe; const Idx: integer; const UseInternalIndex: boolean): integer;
// sélection de couleur Toporobot
function SelectionCouleurToporobot(const OldIdx: integer): Integer;
// statistiques
function DisplayStatistiques(const FileName: string): boolean;
// vues 3D GDI
procedure DisplayVue3DGDI(const FileName: string);
// vue 3D OpenGL
procedure DisplayVue3DOpenGL(const FileName: string);
// vue en plan (dialogue temporaire)
procedure DisplayVueEnPlan(const FileName: string);
// choix d'une couleur
function ChooseColor(const OldColor: TColor): TColor;
// choix d'un type de galerie
function ChooseTypeVisee(const T: integer): integer;
// centre d'impression
procedure DisplayCentreImpression(const FileName: string);
// a Propos
procedure APropos;
// exportation GPX, KML, etc ...
procedure DisplayDlgExportGIS(const FichierTableEntites: string);
// rechercher une station  depuis série+station (boite de dialogue
function GetStationDlgBySerSt(const MyTableEntites: TTableDesEntites;
                              const Ser, St: integer;
                              const Cle: string;
                              var   E: TEntite): boolean;

// paramétrage d'un onglet de vue 2D
function ParametrerOngletVue2D(const O: TVue2DParams): TVue2DParams;
// définition d'attributs de texte pour imprimante
function SelectAttributsTexte(const AT: TTexteAttributs): TTexteAttributs;

// appel du convertisseur Sibert (sandbox)
procedure CallConvertisseurSibert;
// système d'aide
procedure DisplayHelpSystem(const Topics: string);

implementation
uses
  // pour la déclinaison magnétique
  {$IFDEF MSWINDOWS}
    UnitWrapperDeclimag,
    VueEn3DOpenGL,         // visualisateur OpenGL
  {$ENDIF}
  {$IFDEF LINUX}
    UnitWrapperDeclimagLinux,
  {$ENDIF}
  VueEnPlan,           // pour le plan
  VueEn3D,             // vues 3D

  dlgToporobotColor,   // sélecteur de couleur
  dlgSelectDansListes, // sélection dans une liste
  dlgStatistiques,     // stats
  dlgSelectionTypeVisee, // type visée
  CentreImpression,      // outil d'impression
  AboutBox,              // A propos
  dlgHelpSystem,         // système d'aide
  dlgExportationGIS,     // exportation SIG
  //dlgInputSerieSt,       // recherche de station

  dlgCalculette,         // calculette
  dlgParametrerOngletVue2D, // dialogue de configuration d'une vue 2D
  MiniTextEditor,        // éditeur de texte incorporé
  dlgSetTexteAttributs,  // selecteur de fontes imprimante
  dlgSibertConvertisseur; // pilote pour le convertisseur Sibert

// appel du convertisseur Sibert
procedure CallConvertisseurSibert;
begin
  with TdlgConvertisseurSibert.Create(Application) do
  begin
    try
      ShowModal;

    finally
      Free;
    end;
  end;
end;
// calculette et convertisseur de coordonnées
function CallCalculette(const Expression: string; var Coords: TPoint2Df; out ResultatCalcul: double; out Declimag: double): boolean;
begin
  Result := False;
  ResultatCalcul := 0.00;
  with TfrmCalculette.Create(Application) do
  begin
    try
      SetDefaultExpression(Expression);
      SetCoordonnees(Coords);
      ShowModal;
      if (ModalResult = mrOK) then
      begin
        ResultatCalcul:= GetResultatCalcul;
        Declimag      := GetDeclimag;
        Coords        := GetCoordonnees;
        Result := True;
      end;
    finally
      Release;
    end;
  end;

end;

// Editeur de texte interne
// TODO: L'implémenter (fonction vide)
//EditMode=0 : Consultation; 1 = Edition;
procedure DisplayTextEditor(const FileTexte: string; const EditMode: boolean);
begin
  with TdlgEditeur.Create(Application) do begin
    try
      if (InitialiseEditor(FileTexte, EditMode)) then
        ShowModal;
    finally
      Free;
    end;
  end;
  //*)
end;

function DlgSaveAs(const TitreDlg, QInitialDir, QFilter, QDefaultExt: string;
                   var QFileName: string): boolean;
begin
  Result := False;
  with TSaveDialog.Create(Application) do
  begin
    try
      Title := TitreDlg;
      FileName := QFileName;
      Options := Options + [ofOverwritePrompt];
      InitialDir := QInitialDir;
      Filter := QFilter;
      DefaultExt := QDefaultExt;
      if (Execute) then begin
        QFileName := FileName;
        Result := True;
      end;
    finally
      Free;
    end;
  end;
end;

// Question Oui-Non
function QuestionOuiNon(const Msg: string): boolean;
begin
  Result := (MessageDlg(Msg, mtConfirmation,[mbYES, mbNO], 0) = mrYES);
end;

// sélection expé, code, ... dans liste
// DONE: Traiter les index - Retourne désormais les index TOPOROBOT (pouvant être <=> des index internes)
function SelectionDansListe(const DT: TToporobotStructure2012; const Mode: TModeSelectionListe; const Idx: integer; const UseInternalIndex: boolean): integer;
begin
  // ancien index = en cas d'abandon, reprend cet index
  Result := Idx;
  with TdlgSelectElement.Create(Application) do
  begin
    try
      if (InitialiseListeSelection(DT, Mode, Idx, UseInternalIndex)) then
      begin
        ShowModal;
        if (ModalResult = mrOK) then
        begin
          Result := GetIndexElement(UseInternalIndex); //  Result := GetNumeroElement;
        end;
      end
      else
      begin
        ShowMessage(rsMATCHNOTFOUND);
      end;
    finally
      Release;
    end;
  end;
end;

// sélection de couleurs issues de palettes
// Version simplifiée: les autres palettes ne sont pas utilisées bien
// qu'opérationnelles. La palette AutoCAD est utilisée dans un autre module
function SelectionCouleurToporobot(const OldIdx: integer): integer;
begin
  Result := OldIdx;
  with TDialogSelTopoColor.Create(Application) do
  begin
    try
      Initialize(OldIdx);
      ShowModal;
      if (ModalResult = mrOK) then
      begin
        Result  := GetIdxColor;
      end;
    finally
      Release;
    end;
  end;
end;

// vues 3D
// Fonctionnel mais les contrôleurs doivent être changés.
procedure DisplayVue3DGDI(const FileName: string);
begin
  with TdlgVue3D.Create(Application) do
  begin
    try
      if (InitaliserVue3D(FileName)) then
      begin
        ShowModal;
      end
      else
      begin
        ShowMessage(rsMSG_VUE3D_FAIL);
      end;
    finally
      Release;
    end;
  end;
end;

// vue 3D OpenGL
procedure DisplayVue3DOpenGL(const FileName: string);
begin
  {$IFDEF MSWINDOWS}

    with TdlgVue3DOpenGL.Create(Application) do
    begin
      try
        if (InitaliserVue3DOpenGL(FileName)) then
        begin
          ShowModal;
        end
        else
        begin
          ShowMessage(AnsiToUtf8(rsMSG_VUE3D_FAIL));
        end;
      finally
        Release;
      end;
    end;
  {$ENDIF}
  {$IFDEF LINUX}
     ShowMessage('Pas de visualisateur OpenGL pour l''instant');
  {$ENDIF}
end;

// vue en plan (dialogue temporaire)
procedure DisplayVueEnPlan(const FileName: string);
begin
  with TfrmVue2D.Create(Application) do
  begin
    try
      if (InitialiserVueEnPlan(FileName, True)) then
      begin
        ShowModal;
      end
      else
      begin
        ShowMessage(AnsiToUtf8(rsMSG_VUE2D_FAIL));
      end;
    finally
      Release;
    end;
  end;
end;
// choix d'une couleur RGB
function ChooseColor(const OldColor: TColor): TColor;
begin
  Result := OldColor;
  with TColorDialog.Create(Application) do
  begin
    try
      Color := OldColor;
      if (Execute) then Result := Color;

    finally
      Free;
    end;
  end;
end;
// statistiques
function DisplayStatistiques(const FileName: string): boolean;
begin
  with TdlgStats.Create(Application) do
  begin
    try
      if (InitialiserDialogStats(FileName)) then
      begin
        ShowModal;
      end
      else
      begin
        ShowMessage(AnsiToUtf8(rsMSG_STATS_DLG_FAIL));
      end;
    finally
      Release;
    end;
  end;
end;
// choix d'un type de galerie
function ChooseTypeVisee(const T: integer): integer;
begin
  Result := T;
  with TdlgSelectTypeVisee.Create(Application) do
  begin
    try
      SetTypeVisee(T);
      ShowModal;
      if (ModalResult = mrOK) then Result := GetTypeVisee();
    finally
      Release;
    end;
  end;

end;
// centre d'impression
procedure DisplayCentreImpression(const FileName: string);
begin
  with TdlgPrintingCenter.Create(Application) do
  begin
    try
      if (Initialise(FileName)) then ShowModal;
    finally
      Release;
    end;
  end;
end;
// a Propos
procedure APropos;
begin
  with TdlgAbout.Create(Application) do
  begin
    try
      ShowModal;
    finally
      Release;
    end;
  end;
end;
// exportation GPX, KML, etc ...
procedure DisplayDlgExportGIS(const FichierTableEntites: string);
begin
  with TdlgExportSIG.Create(Application) do
  begin
    try
      if (Initialise(FichierTableEntites)) then ShowModal;
    finally
      Free;
    end;
  end;

end;
// rechercher une station  depuis série+station (boite de dialogue
function GetStationDlgBySerSt(const MyTableEntites: TTableDesEntites;
                              const Ser, St: integer;
                              const Cle: string;
                              var   E  : TEntite): boolean;
begin
  Result := False;
  E.Entite_Serie    := -1;
  E.Entite_Station  := -1;
  (*
  with TfrmInputSerSt.Create(Application) do begin
    try
      SetSerieStationCle(Ser, St, Cle);

      ShowModal;
      //ShowMessageFmt('Dans GetStationDlgBySerSt: %d/%d - %s, %d',[MyNoSerie, MyNoStation, MyCle, MyMode]);
      if (ModalResult = mrOK) then begin

        case GetModeRecherche of
          0: E := MyTableEntites.GetEntiteFromCle(GetCle());
          1: E := MyTableEntites.GetEntiteFromSerSt(GetNoSerie(), GetNoStation());
        end;
        Result := True;
      end;

      //AfficherMessage(Format('GetStationDlgBySerSt: %d %d',[Result.Entite_Serie,Result.Entite_Station]));
    finally
      Release;
    end;
  end;
  //*)
end;


function ParametrerOngletVue2D(const O: TVue2DParams): TVue2DParams;
begin
  Result := O;
  with TfrmParametrerOngletVue2D.Create(Application) do
  begin
    try
      SetValuesOnglet(O);
      ShowModal;
      if (ModalResult = mrOK) then Result := GetValuesOnglet;
    finally
      Release;
    end;

  end;
end;

function SelectAttributsTexte(const AT: TTexteAttributs): TTexteAttributs;
begin
  Result := AT;
  with TfrmSelectTexteAttributs.Create(Application) do
  begin
    try
      SetTexteAttributs(AT);
      ShowModal;
      if (ModalResult = mrOK) then Result := GetTexteAttributs;
    finally
      Release;
    end;
  end;
end;


procedure DisplayHelpSystem(const Topics: string);
var
  s: string;
begin
  with TfrmHelpSystem.Create(Application) do begin
  try
    {$IFDEF FRENCH_MESSAGES}
       s:=ExtractFilePath(ParamStr(0))+'HelpFile_fr.txt';
    {$ENDIF}
    {$IFDEF ENGLISH_MESSAGES}
       s:=ExtractFilePath(ParamStr(0))+'HelpFile_en.txt';
    {$ENDIF}
    {$IFDEF SPANISH_MESSAGES}
       s:=ExtractFilePath(ParamStr(0))+'HelpFile_es.txt';
    {$ENDIF}
    if Init(s, Topics) then
      ShowModal
    else
      ShowMessage(AnsiToUtf8(Format(rsMSG_FILENOTFOUND,[s])));
  finally
    Release;
  end;
  end;
end;


end.

