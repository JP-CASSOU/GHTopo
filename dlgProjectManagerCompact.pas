unit dlgProjectManagerCompact;
// Date: 03/05/2013
// Statut: Partiellement Fonctionnel
//
// Parties opérationnelles:
//*************************
// Tables    Listes   Navigation   Ajout   Modifs  Tris  Suppressions  Recherche
// Entrées     X          X          X       X       0        X           0
// Réseaux     X          X          X       X       0        X           0
// Expés       X          X          X       X       X        X           0
// Codes       X          X          X       X       X        X           0
// Séries      X          X          X       X       X        X           X
// Antennes    X          X          X       X       N/A      X           N/A

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
  {$IFDEF MSWINDOWS}
    ConversionsCoordonneesEPSG,
  {$ENDIF}
  {$IFDEF LINUX}
    ConversionsCoordonneesEPSGLinux,
  {$ENDIF}

  CallDialogsStdVersion, Common,
  StructuresDonnees,
  ToporobotClasses2012, ObjetSerie,

  CadreNavigateurDB, CadreCode, CadreEntrance, CadreReseau, CadreExpe,
  CadreViseesAntenne, CadreSerieIndependant, Classes, SysUtils, FileUtil,
  curredit, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls, ExtCtrls;

type

  { TdlgProjectManagerStd }

  TdlgProjectManagerStd = class(TForm)
    btnSelectEPSG: TButton;
    btnCalcAllDeclimags: TButton;
    Button3: TButton;
    CdrAntennes1: TCdrAntennes;
    CdrCode1: TCdrCode;
    CdrEntreeCavites1: TCdrEntreeCavites;
    CdrExpe1: TCdrExpe;
    CdrNavigateurDBSeries: TCdrNavigateurDB;
    CdrNavigateurDBEntrees: TCdrNavigateurDB;
    CdrNavigateurDBReseaux: TCdrNavigateurDB;
    CdrNavigateurDBCodes: TCdrNavigateurDB;
    CdrNavigateurDBExpes: TCdrNavigateurDB;
    CdrReseaux1: TCdrReseaux;
    CdrSerieIndependant1: TCdrSerieIndependant;
    editCodeEPSG: TCurrencyEdit;
    editCommentaireEtude: TMemo;
    editNomEtude: TEdit;
    lbCodeEPSG: TLabel;
    lbCommentaireEtude: TLabel;
    lbNomEtude: TLabel;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel10: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    pnlCdrSerie: TPanel;
    tabshtMaintenance: TTabSheet;
    tabshtGeneral: TTabSheet;
    tabshtReseaux: TTabSheet;
    tabshtSeries: TTabSheet;
    tabshtCodes: TTabSheet;
    tabshtExpes: TTabSheet;
    tabshtEntrees: TTabSheet;
    tabshtAntennes: TTabSheet;
    procedure btnCalcAllDeclimagsClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure btnSelectEPSGClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);

  private
    FDocumentToporobot: TToporobotStructure2012;
    FNumeroEntree    : integer;
    FNumeroReseau    : integer;
    FNumeroCode      : integer;
    FNumeroExpe      : integer;
    FNumeroSerie     : integer;
    FNumeroStation   : integer;

    FNbEntrees    : integer;
    FNbReseaux    : integer;
    FNbCodes      : integer;
    FNbExpes      : integer;
    FNbSeries     : integer;
    FNbStations   : integer;

    // ajouts
    function AddNewCode: boolean;
    function AddNewEntrance: boolean;
    function AddNewExpe: boolean;
    function AddNewReseau: boolean;
    function AddNewSerie: boolean;
    // DONE: Ajout de série
    // mises à jour
    procedure ApplyModifsCodes(const Idx: Integer);
    procedure ApplyModifsEntrances(const Idx: Integer);
    procedure ApplyModifsExpes(const Idx: Integer);
    procedure ApplyModifsReseau(const Idx: Integer);
    procedure ApplyModifsSerie(const Idx: Integer);   // TODO: Modif de série
    procedure qSortCodes;
    procedure qSortExpes;


    // sélection dans listes
    function SelectCodeInListe(const Idx: integer): integer;
    function SelectEntreeInListe(const Idx: integer): integer;
    function SelectExpeInListe(const Idx: integer): integer;
    function SelectReseauInListe(const Idx: integer): integer;
    function SelectSerieInListe(const Idx: integer): integer;
    //-----------------
    procedure SetCurrentCode(const Idx: integer);
    procedure SetCurrentEntree(const Idx: integer);
    procedure SetCurrentExpe(const Idx: integer);
    procedure SetCurrentReseau(const Idx: integer);
    procedure SetCurrentSerie(const Idx: integer);


    procedure SetDocumentToporobot(const TD: TToporobotStructure2012);
    procedure SetNumeroEntree(const n: integer);
    procedure SetNumeroReseau(const n: integer);
    procedure SetNumeroCode(const n: integer);
    procedure SetNumeroExpe(const n: integer);
    procedure SetNumeroSerie(const n: integer);
    procedure SetNumeroStation(const n: integer);
    // fonction de tri
    procedure qSortSeries;
    // fonctions de recherche
    function  FindSerie(const NumSerie: integer): integer;
    procedure SupprimerAntenne(const N: integer);
    procedure SupprimerCode(const N: integer);
    procedure SupprimerEntree(const N: integer);
    procedure SupprimerExpe(const N: integer);
    procedure SupprimerReseau(const N: integer);
    // fonctions de suppression
    procedure SupprimerSerie(const N: integer);



    { private declarations }
  public
    { public declarations }
    function Init(const TD: TToporobotStructure2012;
                  const QNoEntree, QNoReseau,
                        QNoCode, QNoExpe,
                        QNoSerie, QNoStation: integer): boolean;


  end; 

var
  dlgProjectManagerStd: TdlgProjectManagerStd;

implementation
const USE_INTERNAL_INDEX = True;



{$R *.lfm}
// sélections d'éléments dans des listes
function TdlgProjectManagerStd.SelectEntreeInListe(const Idx: integer): integer;
begin
  Result := SelectionDansListe(FDocumentToporobot, mslENTRANCES, Idx, USE_INTERNAL_INDEX);
end;

function TdlgProjectManagerStd.SelectSerieInListe(const Idx: integer): integer;
begin
  Result := SelectionDansListe(FDocumentToporobot, mslSERIE, Idx, USE_INTERNAL_INDEX);

end;
function TdlgProjectManagerStd.SelectReseauInListe(const Idx: integer): integer;
begin
  Result := SelectionDansListe(FDocumentToporobot, mslRESEAUX, Idx, USE_INTERNAL_INDEX);
end;
function  TdlgProjectManagerStd.SelectCodeInListe(const Idx: integer): integer;
begin
  Result := SelectionDansListe(FDocumentToporobot, mslCODE, Idx, USE_INTERNAL_INDEX);
end;
function  TdlgProjectManagerStd.SelectExpeInListe(const Idx: integer): integer;
begin
  Result := SelectionDansListe(FDocumentToporobot, mslEXPE, Idx, USE_INTERNAL_INDEX);
end;
// navigation
procedure TdlgProjectManagerStd.SetCurrentSerie(const Idx: integer);
var
  Serie: TObjSerie;
begin
  Serie := FDocumentToporobot.GetSerie(Idx);
  CdrSerieIndependant1.Initialise(FDocumentToporobot, Serie);
end;
procedure  TdlgProjectManagerStd.SetCurrentExpe(const Idx: integer);
var
  E: TExpe;
begin
  E := FDocumentToporobot.GetExpe(Idx);
  CdrExpe1.SetExpe(E, False);
end;
procedure  TdlgProjectManagerStd.SetCurrentCode(const Idx: integer);
var
  C: TCode;
begin
  C := FDocumentToporobot.GetCode(Idx);
  CdrCode1.SetCode(C, False);
end;
procedure  TdlgProjectManagerStd.SetCurrentEntree(const Idx: integer);
var
  E: TEntrance;
begin
  E := FDocumentToporobot.GetEntree(Idx);
  CdrEntreeCavites1.SetEntrance(E, False);
end;
procedure  TdlgProjectManagerStd.SetCurrentReseau(const Idx: integer);
var
  R: TReseau;
begin
  R := FDocumentToporobot.GetReseau(Idx);
  CdrReseaux1.SetReseau(R, False);
end;




// initialisation
function TdlgProjectManagerStd.Init(const TD: TToporobotStructure2012;
                                 const QNoEntree, QNoReseau,
                                       QNoCode, QNoExpe,
                                       QNoSerie, QNoStation: integer): boolean;
var
  E: TEntrance;
  R: TReseau;
  C: TCode;
  S: TExpe;
  Ser: TObjSerie;
  ii: Integer;
  EWE: TParametresEPSG;
begin
  Result := False;
  try
    SetDocumentToporobot(TD);
    // titre de la fenêtre
    self.Caption := AnsiToUtf8(rsWND_DATABASE) + ': ' + TD.GetDatabaseName;
    // nombre d'entités
    FNbEntrees    := FDocumentToporobot.GetNbEntrees;
    FNbReseaux    := FDocumentToporobot.GetNbReseaux;
    FNbCodes      := FDocumentToporobot.GetNbCodes;
    FNbExpes      := FDocumentToporobot.GetNbExpes;
    FNbSeries     := FDocumentToporobot.GetNbSeries;

    SetNumeroEntree(QNoEntree);
    SetNumeroReseau(QNoReseau);
    SetNumeroCode(QNoCode);
    SetNumeroExpe(QNoExpe);
    SetNumeroSerie(QNoSerie);
    SetNumeroStation(QNoStation);
    // Navigateurs
    with CdrNavigateurDBEntrees do begin
      Initialiser(FDocumentToporobot, mbddDISABLED, 0, FNbEntrees, QNoEntree);
      SetProcAddNewItem(AddNewEntrance);
      SetProcApplyChanges(ApplyModifsEntrances);
      SetProcGoto(SetCurrentEntree);
      SetProcRemove(SupprimerEntree);
      SetProcSearch(nil);
      SetProcSelectInListe(SelectEntreeInListe);
      SetProcSort(nil);
    end;
    with CdrNavigateurDBReseaux do begin
      Initialiser(FDocumentToporobot, mbddDISABLED, 0, FNbReseaux, QNoReseau);
      SetProcAddNewItem(AddNewReseau);
      SetProcApplyChanges(ApplyModifsReseau);
      SetProcGoto(SetCurrentReseau);
      SetProcRemove(SupprimerReseau);
      SetProcSearch(nil);
      SetProcSelectInListe(SelectReseauInListe);
      SetProcSort(nil);
    end;
    with CdrNavigateurDBCodes do begin
      Initialiser(FDocumentToporobot, mbddDISABLED, 1, FNbCodes, QNoCode);
      SetProcAddNewItem(AddNewCode);
      SetProcApplyChanges(ApplyModifsCodes);
      SetProcGoto(SetCurrentCode);
      SetProcRemove(SupprimerCode);
      SetProcSearch(nil);
      SetProcSelectInListe(SelectCodeInListe);
      SetProcSort(qSortCodes);
    end;
    with CdrNavigateurDBExpes do begin
      Initialiser(FDocumentToporobot, mbddDISABLED, 1, FNbExpes, QNoExpe);
      SetProcAddNewItem(AddNewExpe);
      SetProcApplyChanges(ApplyModifsExpes);
      SetProcGoto(SetCurrentExpe);
      SetProcRemove(SupprimerExpe);
      SetProcSearch(nil);
      SetProcSelectInListe(SelectExpeInListe);
      SetProcSort(qSortExpes);

    end;
    with CdrNavigateurDBSeries do begin
      Initialiser(FDocumentToporobot, mbddDISABLED, 1, FNbSeries, QNoSerie);
      SetProcAddNewItem(AddNewSerie);
      SetProcApplyChanges(ApplyModifsSerie);       // ApplyModifsSerie
      SetProcGoto(SetCurrentSerie);
      SetProcRemove(SupprimerSerie);
      SetProcSearch(FindSerie);
      SetProcSelectInListe(SelectSerieInListe);
      SetProcSort(qSortSeries);
    end;



    // cadres de formulaire
    E := FDocumentToporobot.GetEntree(QNoEntree);
    R := FDocumentToporobot.GetReseau(QNoReseau);
    C := FDocumentToporobot.GetCode(QNoCode);
    S := FDocumentToporobot.GetExpe(QNoExpe);
    Ser := FDocumentToporobot.GetSerie(1);
    CdrEntreeCavites1.SetEntrance(E, True);
    CdrReseaux1.SetReseau(R, True);
    CdrCode1.SetCode(C, True);
    CdrExpe1.SetExpe(S, True);
    CdrAntennes1.Initialise(TD);
    CdrSerieIndependant1.Initialise(FDocumentToporobot, Ser);
    // onglet General
    editCodeEPSG.AsInteger    := TD.GetEPSGSystemeCoordonnees;
    editNomEtude.Text         := AnsiToUtf8(TD.GetNomEtude);
    editCommentaireEtude.Text := AnsiToUtf8(TD.GetCommentairesEtude);
    Result := True;
    AfficherMessage(Format('Initialisation de %s: %s', [ClassName, IIF(Result, 'OK', 'KO')]));
  except
  end;

end;

//**********************************
// Procédures d'Ajouts
function TdlgProjectManagerStd.AddNewEntrance: boolean;
var
  E: TEntrance;
begin
  afficherMessage(Format('%s.AddNewEntrance',[ClassName]));
  Result:=False;
  try
    with E do begin
      eNumEntree  :=FDocumentToporobot.GetNbEntrees;
      eNomEntree  :='Nouvelle entrée';
      eXEntree    := 0.00;
      eYEntree    := 0.00;
      eZEntree    := 0.00;
      eRefSer     := 1;
      eRefSt      := 0;
      eObserv     := '';
    end;

    FDocumentToporobot.AddEntree(E);
    Result:=True;
  except
  end;
end;
function TdlgProjectManagerStd.AddNewReseau  : boolean;
var
  R: TReseau;
begin
  Result:=False;
  try
    with R do begin
      IdxReseau    :=FDocumentToporobot.GetNbReseaux;
      ColorReseau  :=clBlue;
      NomReseau    :='Nouveau réseau';
      ObsReseau    :='';
    end;
    FDocumentToporobot.AddReseau(R);
    Result:=True;
  except
  end;
end;
function TdlgProjectManagerStd.AddNewCode: boolean;
var
  C: TCode;
begin
  afficherMessage(Format('%s.AddNewCode',[ClassName]));
  Result:=False;
  try
    with C do begin
      IDCode   := FDocumentToporobot.GetNbCodes;
      GradAz   := 360.00;
      GradInc  := 360.00;
      PsiL     := 0.01;
      PsiAz    := 1.00;
      PsiP     := 1.00;
      FactLong := 1.00;
      AngLimite:= 0.00;
      TypeGalerie := 0;
      Commentaire := '';
    end;
    FDocumentToporobot.AddCode(C);
    Result:=True;
  except
  end;
end;
function TdlgProjectManagerStd.AddNewExpe: boolean;
var
  E: TExpe;
begin
  afficherMessage(Format('%s.AddNewExpe',[ClassName]));
  Result:=False;
  try
    with E do begin
      IDExpe    := FDocumentToporobot.GetNbExpes;
      JourExpe  := 1;
      MoisExpe  := 1;
      AnneeExpe := 2005;
      Speleometre  := '';
      Speleographe := '';
      ModeDecl     := 0;
      Declinaison  := 0.00;
      Inclinaison  := 0.00;
      Couleur      := 6; // orange
      Commentaire  := '';
    end;
    FDocumentToporobot.AddExpe(E);
    Result:=True;
  except
  end;
end;


function TdlgProjectManagerStd.AddNewSerie: boolean;
var
  Ser: TObjSerie;
  St : TuneVisee;
begin
  afficherMessage(Format('%s.AddNewSerie',[ClassName]));
  Result:=False;
  try
    Ser := TObjSerie.Create;
    with Ser do begin
      SetIndexSerie(FDocumentToporobot.GetNbSeries);
      SetNomSerie('Nouvelle serie');
      SetSeriePtExtremites(GetIndexSerie, 1, GetIndexSerie, 0);
      SetNoReseau(0);
      SetChanceObstacle(3, 0);
      SetObsSerie('');
      ClearSerie;
      AddVisee(EmptyVisee('Added'));
      FDocumentToporobot.AddSerie(Ser);
    end;
    Result:=True;
    AfficherMessage('-- Serie added');
  except
  end;
end;

// Appliquer modifications des formulaires dans la base de données

procedure TdlgProjectManagerStd.ApplyModifsEntrances(const Idx: Integer);
var
  E: TEntrance;
begin
  E:=CdrEntreeCavites1.GetEntranceFromForm;
  FDocumentToporobot.PutEntree(Idx, E);
end;
procedure TdlgProjectManagerStd.ApplyModifsReseau(const Idx: Integer);
var
  R: TReseau;
begin
  R:=CdrReseaux1.GetReseauFromForm;
  FDocumentToporobot.PutReseau(Idx, R);
end;
procedure TdlgProjectManagerStd.ApplyModifsCodes(const Idx: Integer);
var
  C: TCode;
begin
  C:=CdrCode1.GetCodeFromForm;
  FDocumentToporobot.PutCode(Idx, C);
end;
procedure TdlgProjectManagerStd.ApplyModifsExpes(const Idx: Integer);
var
  E: TExpe;
begin
  E:=CdrExpe1.GetExpeFromForm;
  FDocumentToporobot.PutExpe(Idx, E);
end;
procedure TdlgProjectManagerStd.ApplyModifsSerie(const Idx: Integer);
begin
  with CdrSerieIndependant1 do
  begin
    if (ImplementerModifs) then RefreshTableaux;
  end;
  //*)
end;

//*)
///************************************
procedure TdlgProjectManagerStd.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  CanClose:=False;
  ShowMessage(AnsiToUtf8(rsNOCANCLOSEWND));
end;

procedure TdlgProjectManagerStd.Button1Click(Sender: TObject);
begin
  FDocumentToporobot.SetEPSGSystemeCoordonnees(editCodeEPSG.AsInteger);
end;

procedure TdlgProjectManagerStd.btnCalcAllDeclimagsClick(Sender: TObject);
begin
  FDocumentToporobot.CalculerDeclinaisonsMagnetiques(True);
end;

procedure TdlgProjectManagerStd.btnSelectEPSGClick(Sender: TObject);
var
  EWE: TParametresEPSG;
begin
  EWE.CodeEPSG := editCodeEPSG.AsInteger;
  EWE := SelectSystemeEPSG(EWE);
  editCodeEPSG.AsInteger := EWE.CodeEPSG;
end;

procedure TdlgProjectManagerStd.Button3Click(Sender: TObject);
begin
  FDocumentToporobot.SetEPSGSystemeCoordonnees(editCodeEPSG.AsInteger);
  FDocumentToporobot.SetNomEtude(Utf8ToAnsi(editNomEtude.Text));
  FDocumentToporobot.SetCommentairesEtude(Utf8ToAnsi(editCommentaireEtude.Text));
end;


procedure TdlgProjectManagerStd.FormCreate(Sender: TObject);
begin
  tabshtGeneral.Caption  := AnsiToUtf8(rsTBS_GENERAL);
  tabshtEntrees.Caption  := AnsiToUtf8(rsTBS_ENTRANCE);
  tabshtCodes.Caption    :=  AnsiToUtf8(rsTBS_CODES);
  TabShtExpes.Caption    :=  AnsiToUtf8(rsTBS_TRIPS);
  TabShtSeries.Caption   :=  AnsiToUtf8(rsTBS_SERIES);
  TabShtReseaux.Caption  :=  AnsiToUtf8(rsTBS_RESEAUX);
  tabshtAntennes.Caption := AnsiToUtf8(rsTBS_ANTENNES);
  tabshtMaintenance.Caption := AnsiToUtf8(rsTBS_MAINTENANCE);


  lbNomEtude.Caption           := AnsiToUtf8(rsLB_NOM_ETUDE);
  lbCommentaireEtude.Caption   := AnsiToUtf8(rsLB_COMMENTAIRE_ETUDE);
  lbCodeEPSG.Caption           := AnsiToUtf8(rsLB_CODE_EPSG);
  btnSelectEPSG.Caption        := AnsiToUtf8(rsBTN_SELECT_EPSG);

  btnCalcAllDeclimags.Caption  := AnsiToUtf8(rsBTN_CALC_DECLIMAGS);
end;



// procédures de recherche
function TdlgProjectManagerStd.FindSerie(const NumSerie: integer): integer;
begin
  Result := FDocumentToporobot.GetIdxSerie(NumSerie);
end;
// passage de la référence sur le document topo
procedure TdlgProjectManagerStd.SetDocumentToporobot(const TD: TToporobotStructure2012);
begin
  FDocumentToporobot := TD;
end;
// Définition des numéros courants
procedure TdlgProjectManagerStd.SetNumeroEntree(const n: integer);
begin
  FNumeroEntree := n;
end;
procedure TdlgProjectManagerStd.SetNumeroReseau(const n: integer);
begin
  FNumeroReseau := n;
end;

procedure TdlgProjectManagerStd.SetNumeroCode(const n: integer);
begin
  FNumeroCode := n;
end;

procedure TdlgProjectManagerStd.SetNumeroExpe(const n: integer);
begin
  FNumeroExpe := n;
end;

procedure TdlgProjectManagerStd.SetNumeroSerie(const n: integer);
begin
  FNumeroSerie := n;
end;

procedure TdlgProjectManagerStd.SetNumeroStation(const n: integer);
begin
  FNumeroStation := n;
end;
// Procédures de tri
procedure TdlgProjectManagerStd.qSortSeries;
begin
  FDocumentToporobot.SortSeries;
  self.SetCurrentSerie(1); // on se pose sur la série 1

end;
procedure TdlgProjectManagerStd.qSortCodes;
begin
  FDocumentToporobot.SortCodes;
  self.SetCurrentCode(1); // on se pose sur la série 1
end;
procedure TdlgProjectManagerStd.qSortExpes;
begin
  FDocumentToporobot.SortExpes;
  self.SetCurrentExpe(1); // on se pose sur la série 1
end;


// Procédures de suppression
procedure TdlgProjectManagerStd.SupprimerSerie(const N: integer);
begin
  FDocumentToporobot.RemoveSerie(N);
  CdrNavigateurDBSeries.Initialiser(FDocumentToporobot, mbddDISABLED, 0, FDocumentToporobot.GetNbSeries, -1);
end;
procedure TdlgProjectManagerStd.SupprimerCode(const N: integer);
begin
  FDocumentToporobot.RemoveCode(N);
  CdrNavigateurDBSeries.Initialiser(FDocumentToporobot, mbddDISABLED, 0, FDocumentToporobot.GetNbCodes, -1);
end;
procedure TdlgProjectManagerStd.SupprimerExpe(const N: integer);
begin
  FDocumentToporobot.RemoveExpe(N);
  CdrNavigateurDBSeries.Initialiser(FDocumentToporobot, mbddDISABLED, 0, FDocumentToporobot.GetNbExpes, -1);
end;
procedure TdlgProjectManagerStd.SupprimerEntree(const N: integer);
begin
  FDocumentToporobot.RemoveEntree(N);
  CdrNavigateurDBSeries.Initialiser(FDocumentToporobot, mbddDISABLED, 0, FDocumentToporobot.GetNbEntrees, -1);
end;
procedure TdlgProjectManagerStd.SupprimerReseau(const N: integer);
begin
  FDocumentToporobot.RemoveReseau(N);
  CdrNavigateurDBSeries.Initialiser(FDocumentToporobot, mbddDISABLED, 0, FDocumentToporobot.GetNbReseaux, -1);
end;
procedure TdlgProjectManagerStd.SupprimerAntenne(const N: integer);
begin
  FDocumentToporobot.RemoveViseeAntenne(N);
  CdrNavigateurDBSeries.Initialiser(FDocumentToporobot, mbddDISABLED, 0, FDocumentToporobot.GetNbAntennes, -1);
end;





end.
