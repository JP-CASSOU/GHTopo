unit dlgProjectManagerWideScreen;
// Frontal de gestion adapté aux écrans larges.
// Comporte une liste latérale permanente
// 05/05/2013: Début du développement
// Statut: OK
// 19/06/2013: Mise en place des confirmations de déplacement dans les listes

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
  StructuresDonnees, ToporobotClasses2012, ObjetSerie, Common,
  UnitClassPalette,

  CallDialogsStdVersion, CadreSerieIndependant, CadreCode, CadreExpe,
  CadreEntrance, CadreReseau, CadreViseesAntenne,
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls, PairSplitter, types, LCLType, ActnList, Buttons;

type

  { TdlgProjectManagerWdScr }

  TdlgProjectManagerWdScr = class(TForm)
    acAddItem: TAction;
    acApplyModifs: TAction;
    acDeleteItem: TAction;
    acFind: TAction;
    acSort: TAction;
    ActionList1: TActionList;
    Button3: TButton;
    CdrAntennes1: TCdrAntennes;
    CdrCode1: TCdrCode;
    CdrEntreeCavites1: TCdrEntreeCavites;
    CdrExpe1: TCdrExpe;
    CdrReseaux1: TCdrReseaux;
    CdrSerieIndependant1: TCdrSerieIndependant;
    chkConfirmMove: TCheckBox;
    editSystemeCoords: TEdit;
    editCommentaireEtude: TMemo;
    editNomEtude: TEdit;
    grbxListes: TGroupBox;
    hcColsTitres: THeaderControl;
    ImageList1: TImageList;
    lbCommentaireEtude: TLabel;
    lbNomEtude: TLabel;
    lsbListe: TListBox;
    PageControl1: TPageControl;
    PairSplitter1: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    SpeedButton10: TSpeedButton;
    SpeedButton12: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SpeedButton8: TSpeedButton;
    SpeedButton9: TSpeedButton;
    tabshtAntennes: TTabSheet;
    tabshtSeries: TTabSheet;
    tabshtGeneral: TTabSheet;
    tabshtEntrees: TTabSheet;
    tabshtReseaux: TTabSheet;
    tabshtCodes: TTabSheet;
    tabshtExpes: TTabSheet;
    procedure acAddItemExecute(Sender: TObject);
    procedure acApplyModifsExecute(Sender: TObject);
    procedure acSortExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure hcColsTitresSectionResize(HeaderControl: TCustomHeaderControl;
      Section: THeaderSection);
    procedure lsbListeClick(Sender: TObject);
    procedure lsbListeDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure PageControl1Change(Sender: TObject);
  private
    { private declarations }
    FDocumentToporobot: TToporobotStructure2012;
    FPalette256       : TPalette256;

    FModeBDD: TModeBDD;

    FNumeroEntree    : integer;
    FNumeroReseau    : integer;
    FNumeroCode      : integer;
    FNumeroExpe      : integer;
    FNumeroSerie     : integer;
    FNumeroStation   : integer;

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
    procedure InitHeaderListe(const QModeBDD: TModeBDD);
    procedure Lister(const QModeBDD: TModeBDD);
    procedure ListerLesCodes(const QIdx: integer);
    procedure ListerLesEntrees(const QIdx: integer);
    procedure ListerLesExpes(const QIdx: integer);
    procedure ListerLesReseaux(const QIdx: integer);
    procedure ListerLesSeries(const QIdx: integer);
    procedure qSortCodes;
    procedure qSortExpes;
    procedure qSortSeries;


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

    // fonctions de recherche
    function  FindSerie(const NumSerie: integer): integer;
    procedure SupprimerAntenne(const N: integer);
    procedure SupprimerCode(const N: integer);
    procedure SupprimerEntree(const N: integer);
    procedure SupprimerExpe(const N: integer);
    procedure SupprimerReseau(const N: integer);
    // fonctions de suppression
    procedure SupprimerSerie(const N: integer);

  public
    { public declarations }
    function Init(const TD: TToporobotStructure2012): boolean;
  end;

var
  dlgProjectManagerWdScr: TdlgProjectManagerWdScr;

implementation
const USE_INTERNAL_INDEX = True;



{$R *.lfm}
// sélections d'éléments dans des listes
function TdlgProjectManagerWdScr.SelectEntreeInListe(const Idx: integer): integer;
begin
  Result := SelectionDansListe(FDocumentToporobot, mslENTRANCES, Idx, USE_INTERNAL_INDEX);
end;

function TdlgProjectManagerWdScr.SelectSerieInListe(const Idx: integer): integer;
begin
  Result := SelectionDansListe(FDocumentToporobot, mslSERIE, Idx, USE_INTERNAL_INDEX);

end;
function TdlgProjectManagerWdScr.SelectReseauInListe(const Idx: integer): integer;
begin
  Result := SelectionDansListe(FDocumentToporobot, mslRESEAUX, Idx, USE_INTERNAL_INDEX);
end;
function  TdlgProjectManagerWdScr.SelectCodeInListe(const Idx: integer): integer;
begin
  Result := SelectionDansListe(FDocumentToporobot, mslCODE, Idx, USE_INTERNAL_INDEX);
end;
function  TdlgProjectManagerWdScr.SelectExpeInListe(const Idx: integer): integer;
begin
  Result := SelectionDansListe(FDocumentToporobot, mslEXPE, Idx, USE_INTERNAL_INDEX);
end;
// navigation
procedure TdlgProjectManagerWdScr.SetCurrentSerie(const Idx: integer);
var
  Serie: TObjSerie;
begin
  Serie := FDocumentToporobot.GetSerie(Idx);
  CdrSerieIndependant1.Initialise(FDocumentToporobot, Serie);
end;

procedure TdlgProjectManagerWdScr.SetDocumentToporobot(
  const TD: TToporobotStructure2012);
begin
  FDocumentToporobot := TD;
end;




function TdlgProjectManagerWdScr.FindSerie(const NumSerie: integer): integer;
begin
  Result := 0;
end;

procedure TdlgProjectManagerWdScr.SupprimerAntenne(const N: integer);
begin

end;

procedure TdlgProjectManagerWdScr.SupprimerCode(const N: integer);
begin

end;

procedure TdlgProjectManagerWdScr.SupprimerEntree(const N: integer);
begin

end;

procedure TdlgProjectManagerWdScr.SupprimerExpe(const N: integer);
begin

end;

procedure TdlgProjectManagerWdScr.SupprimerReseau(const N: integer);
begin

end;

procedure TdlgProjectManagerWdScr.SupprimerSerie(const N: integer);
begin
  FDocumentToporobot.RemoveSerie(N);
end;

procedure  TdlgProjectManagerWdScr.SetCurrentExpe(const Idx: integer);
var
  E: TExpe;
begin
  E := FDocumentToporobot.GetExpe(Idx);
  CdrExpe1.SetExpe(E, False);
end;
procedure  TdlgProjectManagerWdScr.SetCurrentCode(const Idx: integer);
var
  C: TCode;
begin
  C := FDocumentToporobot.GetCode(Idx);
  CdrCode1.SetCode(C, False);
end;
procedure  TdlgProjectManagerWdScr.SetCurrentEntree(const Idx: integer);
var
  E: TEntrance;
begin
  E := FDocumentToporobot.GetEntree(Idx);
  CdrEntreeCavites1.SetEntrance(E, False);
end;
procedure  TdlgProjectManagerWdScr.SetCurrentReseau(const Idx: integer);
var
  R: TReseau;
begin
  R := FDocumentToporobot.GetReseau(Idx);
  CdrReseaux1.SetReseau(R, False);
end;


function TdlgProjectManagerWdScr.AddNewCode: boolean;
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

//*****************************************************************************
function TdlgProjectManagerWdScr.AddNewEntrance: boolean;
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
function TdlgProjectManagerWdScr.AddNewReseau  : boolean;
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

function TdlgProjectManagerWdScr.AddNewExpe: boolean;
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



function TdlgProjectManagerWdScr.AddNewSerie: boolean;
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

procedure TdlgProjectManagerWdScr.ApplyModifsEntrances(const Idx: Integer);
var
  E: TEntrance;
begin
  E:=CdrEntreeCavites1.GetEntranceFromForm;
  FDocumentToporobot.PutEntree(Idx, E);
end;
procedure TdlgProjectManagerWdScr.ApplyModifsReseau(const Idx: Integer);
var
  R: TReseau;
begin
  R:=CdrReseaux1.GetReseauFromForm;
  FDocumentToporobot.PutReseau(Idx, R);
end;
procedure TdlgProjectManagerWdScr.ApplyModifsCodes(const Idx: Integer);
var
  C: TCode;
begin
  C:=CdrCode1.GetCodeFromForm;
  FDocumentToporobot.PutCode(Idx, C);
end;
procedure TdlgProjectManagerWdScr.ApplyModifsExpes(const Idx: Integer);
var
  E: TExpe;
begin
  E:=CdrExpe1.GetExpeFromForm;
  FDocumentToporobot.PutExpe(Idx, E);
end;
procedure TdlgProjectManagerWdScr.ApplyModifsSerie(const Idx: Integer);
begin
  with CdrSerieIndependant1 do
  begin
    if (ImplementerModifs) then RefreshTableaux;
  end;
  //*)
end;

// Suppression d'éléments


//FDocumentToporobot.RemoveSerie(N);
// CdrNavigateurDBSeries.Initialiser(FDocumentToporobot, mbddDISABLED, 0, FDocumentToporobot.GetNbSeries, -1);

// Tris

procedure TdlgProjectManagerWdScr.qSortCodes;
begin
  FDocumentToporobot.SortCodes;
end;

procedure TdlgProjectManagerWdScr.qSortExpes;
begin
  FDocumentToporobot.SortExpes;
end;

procedure TdlgProjectManagerWdScr.qSortSeries;
begin
  FDocumentToporobot.SortSeries;
end;




procedure TdlgProjectManagerWdScr.FormCreate(Sender: TObject);
begin
  tabshtGeneral.Caption  := AnsiToUtf8(rsTBS_GENERAL);
  tabshtEntrees.Caption  := AnsiToUtf8(rsTBS_ENTRANCE);
  tabshtCodes.Caption    :=  AnsiToUtf8(rsTBS_CODES);
  TabShtExpes.Caption    :=  AnsiToUtf8(rsTBS_TRIPS);
  TabShtSeries.Caption   :=  AnsiToUtf8(rsTBS_SERIES);
  TabShtReseaux.Caption  :=  AnsiToUtf8(rsTBS_RESEAUX);
  tabshtAntennes.Caption := AnsiToUtf8(rsTBS_ANTENNES);
  //tabshtMaintenance.Caption := AnsiToUtf8(rsTBS_MAINTENANCE);
  FPalette256 := TPalette256.Create;
  FPalette256.GenerateTOPOROBOTPalette;
end;

procedure TdlgProjectManagerWdScr.acApplyModifsExecute(Sender: TObject);
begin
  case FModeBDD of
    mbddENTRANCES:
      begin
        ApplyModifsEntrances(FNumeroEntree);
        ListerLesEntrees(FNumeroEntree);
      end;
    mbddRESEAUX:
      begin
        ApplyModifsReseau(FNumeroReseau);
        ListerLesReseaux(FNumeroReseau);
      end;
    mbddCODES:
      begin
        ApplyModifsCodes(FNumeroCode);
        ListerLesCodes(FNumeroCode);
      end;
    mbddEXPES:
      begin
        ApplyModifsExpes(FNumeroExpe);
        ListerLesExpes(FNumeroExpe);
      end;
    mbddSERIES:
      begin
        ApplyModifsSerie(FNumeroSerie);
        ListerLesSeries(FNumeroSerie);
      end;
  end;

end;

procedure TdlgProjectManagerWdScr.acSortExecute(Sender: TObject);
begin
  case FModeBDD of
    mbddENTRANCES:
      begin
        ; //ListerLesEntrees(0);
      end;
    mbddRESEAUX:
      begin
        ; //ApplyModifsReseau(FNumeroReseau);
        ; //ListerLesReseaux(FNumeroReseau);
      end;
    mbddCODES:
      begin
        FDocumentToporobot.SortCodes;
        ListerLesCodes(0);
      end;
    mbddEXPES:
      begin
        FDocumentToporobot.SortExpes;
        ListerLesExpes(0);
      end;
    mbddSERIES:
      begin
        FDocumentToporobot.SortSeries;
        ListerLesSeries(0);
      end;
  end;


end;




procedure TdlgProjectManagerWdScr.acAddItemExecute(Sender: TObject);
begin
  case FModeBDD of
    mbddENTRANCES:
      begin
        AddNewEntrance;
        ListerLesEntrees(-1);
      end;
    mbddRESEAUX:
      begin
        AddNewReseau;
        ListerLesReseaux(-1);
      end;
    mbddCODES:
      begin
        AddNewCode;
        ListerLesCodes(-1);
      end;
    mbddEXPES:
      begin
        AddNewExpe;
        ListerLesExpes(-1);
      end;
    mbddSERIES:
      begin
        AddNewSerie;
        ListerLesSeries(-1);
      end;
  end;

end;

procedure TdlgProjectManagerWdScr.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  CanClose:=False;
  ShowMessage(AnsiToUtf8(rsNOCANCLOSEWND));
end;

procedure TdlgProjectManagerWdScr.FormDestroy(Sender: TObject);
begin
  // Ne pas mettre de code dans cette zone (bug inexplicable empêchant la fermeture de GHTopo)
  //AfficherMessage('Libération du frontal BD');
  //FPalette256.Free;
  //inherited;
end;

procedure TdlgProjectManagerWdScr.FormShow(Sender: TObject);
begin
  PageControl1.ActivePageIndex := 5;
  // dimensionnement en fonction de l'écran
  self.Position:= poDesigned;
  self.top     := 120;
  self.Left    := 10;
  self.Width   := Screen.Width - 100;
  self.Height  := Screen.Height - 100 - self.Top;
  //
end;

procedure TdlgProjectManagerWdScr.hcColsTitresSectionResize(
  HeaderControl: TCustomHeaderControl; Section: THeaderSection);
begin
  lsbListe.Invalidate;
end;

procedure TdlgProjectManagerWdScr.lsbListeClick(Sender: TObject);
begin
  case FModeBDD of
    mbddENTRANCES:
      begin
        // Le code suivant semble déréférencer la liste des cadres.
        if (CdrEntreeCavites1.IsModified) then
        begin
          case MessageDlg(rsMSG_SAVECHANGES, mtConfirmation, [mbYES, mbNo, mbCancel], 0) of
            mrYES:
              begin
                ApplyModifsEntrances(FNumeroEntree);
                ListerLesEntrees(FNumeroEntree);
                FNumeroEntree := lsbListe.ItemIndex;
                SetCurrentEntree(FNumeroEntree);
              end;
            mrNo:
              begin
                CdrEntreeCavites1.SetModified(False);
                FNumeroEntree := lsbListe.ItemIndex;
                SetCurrentEntree(FNumeroEntree);
              end;
            mrCancel:
              begin
                CdrEntreeCavites1.SetFocus;
              end;
          end;
        end
        else
        begin
          FNumeroEntree := lsbListe.ItemIndex;
          SetCurrentEntree(FNumeroEntree);
        end;
        //*)
      end;
    mbddRESEAUX:
      begin
        // Le code suivant semble déréférencer la liste des cadres.
        if (CdrReseaux1.IsModified) then
        begin
          case MessageDlg(rsMSG_SAVECHANGES, mtConfirmation, [mbYES, mbNo, mbCancel], 0) of
            mrYES:
              begin
                ApplyModifsReseau(FNumeroReseau);
                ListerLesReseaux(FNumeroReseau);
                FNumeroReseau := lsbListe.ItemIndex;
                SetCurrentReseau(FNumeroReseau);
              end;
            mrNo:
              begin
                CdrReseaux1.SetModified(False);
                FNumeroReseau := lsbListe.ItemIndex;
                SetCurrentReseau(FNumeroReseau);
              end;
            mrCancel:
              begin
                CdrReseaux1.SetFocus;
              end;
          end;
        end
        else
        begin
          FNumeroReseau := lsbListe.ItemIndex;
          SetCurrentReseau(FNumeroReseau);
        end;
        //*)

      end;
    mbddCODES:
      begin
        // Le code suivant semble déréférencer la liste des cadres.
        if (CdrCode1.IsModified) then
        begin
          case MessageDlg(rsMSG_SAVECHANGES, mtConfirmation, [mbYES, mbNo, mbCancel], 0) of
            mrYES:
              begin
                ApplyModifsCodes(FNumeroCode);
                ListerLesCodes(FNumeroCode);
                FNumeroCode := lsbListe.ItemIndex;
                SetCurrentCode(FNumeroCode);
              end;
            mrNo:
              begin
                CdrCode1.SetModified(False);
                FNumeroCode := lsbListe.ItemIndex;
                SetCurrentCode(FNumeroCode);
              end;
            mrCancel:
              begin
                CdrCode1.SetFocus;
              end;
          end;
        end
        else
        begin
          FNumeroCode := lsbListe.ItemIndex;
          SetCurrentCode(FNumeroCode);
        end;
        //*)
      end;
    mbddEXPES:
      begin

        // Le code suivant semble déréférencer la liste des cadres.
        if (CdrExpe1.IsModified) then
        begin
          case MessageDlg(rsMSG_SAVECHANGES, mtConfirmation, [mbYES, mbNo, mbCancel], 0) of
            mrYES:
              begin
                ApplyModifsExpes(FNumeroExpe);
                ListerLesExpes(FNumeroExpe);
                FNumeroExpe := lsbListe.ItemIndex;
                SetCurrentExpe(FNumeroExpe);
              end;
            mrNo:
              begin
                CdrExpe1.SetModified(False);
                FNumeroExpe := lsbListe.ItemIndex;
                SetCurrentExpe(FNumeroExpe);
              end;
            mrCancel:
              begin
                CdrExpe1.SetFocus;
              end;
          end;
        end
        else
        begin
          FNumeroExpe := lsbListe.ItemIndex;
          SetCurrentExpe(FNumeroExpe);
        end;
        //*)
      end;

    mbddSERIES:
      begin
        // Le code suivant semble déréférencer la liste des cadres.
        if (CdrSerieIndependant1.IsModified) then
        begin
          case MessageDlg(rsMSG_SAVECHANGES, mtConfirmation, [mbYES, mbNo, mbCancel], 0) of
            mrYES:
              begin
                ApplyModifsSerie(FNumeroSerie);
                ListerLesSeries(FNumeroSerie);
                FNumeroSerie := lsbListe.ItemIndex;
                SetCurrentSerie(FNumeroSerie);
              end;
            mrNo:
              begin
                CdrSerieIndependant1.SetModified(False);
                FNumeroSerie := lsbListe.ItemIndex;
                SetCurrentSerie(FNumeroSerie);
              end;
            mrCancel:
              begin
                CdrSerieIndependant1.SetFocus;
              end;
          end;
        end
        else
        begin
          FNumeroSerie := lsbListe.ItemIndex;
          SetCurrentSerie(FNumeroSerie);
        end;
        //*)
      end;

  end;
end;


procedure TdlgProjectManagerWdScr.PageControl1Change(Sender: TObject);
begin
  case PageControl1.TabIndex of
    0: grbxListes.Visible := False;
    1: Lister(mbddENTRANCES);
    2: Lister(mbddRESEAUX);
    3: Lister(mbddCODES);
    4: Lister(mbddEXPES);
    5: Lister(mbddSERIES);
    6: grbxListes.Visible := False;
  end;
end;





function TdlgProjectManagerWdScr.Init(const TD: TToporobotStructure2012): boolean;
var
  E: TEntrance;
  R: TReseau;
  C: TCode;
  S: TExpe;
  Ser: TObjSerie;
begin
  AfficherMessage(Format('%s.Init("%s")', [ClassName, TD.GetDatabaseName]));
  FModeBDD         := mbddSERIES;
  FNumeroEntree    := 0;
  FNumeroReseau    := 0;
  FNumeroCode      := 0;
  FNumeroExpe      := 0;
  FNumeroSerie     := 1;
  FNumeroStation   := 0;

  Result := False;
  try
    SetDocumentToporobot(TD);
    // titre de la fenêtre
    self.Caption := AnsiToUtf8(rsWND_DATABASE) + ': ' + TD.GetDatabaseName;


    // cadres de formulaire
    E := FDocumentToporobot.GetEntree(0);
    R := FDocumentToporobot.GetReseau(0);
    C := FDocumentToporobot.GetCode(0);
    S := FDocumentToporobot.GetExpe(0);

    Ser := FDocumentToporobot.GetSerie(1);
    CdrEntreeCavites1.SetEntrance(E, True);
    CdrReseaux1.SetReseau(R, True);
    CdrCode1.SetCode(C, True);
    CdrExpe1.SetExpe(S, True);
    CdrAntennes1.Initialise(TD);
    AfficherMessage('--- 004');
    CdrSerieIndependant1.Initialise(FDocumentToporobot, Ser);
    // onglet General
    editSystemeCoords.Text    := TD.GetSystemeCoordonnees;
    editNomEtude.Text         := AnsiToUtf8(TD.GetNomEtude);
    editCommentaireEtude.Text := AnsiToUtf8(TD.GetCommentairesEtude);
    Result := True;
    // lister
    Lister(FModeBDD);

  except
  end;
  AfficherMessage(Format('Initialisation de %s: %s', [ClassName, IIF(Result, 'OK', 'KO')]));

end;
// wrapper pour listes
procedure TdlgProjectManagerWdScr.Lister(const QModeBDD: TModeBDD);
begin
  FModeBDD := QModeBDD;
  InitHeaderListe(QModeBDD);

  case QModeBDD of
    mbddENTRANCES: ListerLesEntrees(FNumeroEntree);
    mbddRESEAUX  : ListerLesReseaux(FNumeroReseau);
    mbddSERIES: ListerLesSeries(FNumeroSerie);
    mbddEXPES : ListerLesExpes(FNumeroExpe);
    mbddCODES : ListerLesCodes(FNumeroCode);
  end;
  grbxListes.Visible := True;
end;
// lister les entrées
procedure TdlgProjectManagerWdScr.ListerLesEntrees(const QIdx: integer);
var
  i: Integer;
  E: TEntrance;
  EWE: Integer;
begin
  EWE := QIdx;
  lsbListe.Enabled:=False;
  lsbListe.Clear;
  for i:= 0 to FDocumentToporobot.GetNbEntrees - 1 do
  begin
    E := FDocumentToporobot.GetEntree(i);
    lsbListe.Items.Add(Format('%d: %.0f, %.0f, %.0f - %s', [E.eNumEntree, E.eXEntree, E.eYEntree, E.eZEntree, E.eNomEntree]));
  end;
  if (QIdx = -1) then
  begin
    EWE := FDocumentToporobot.GetNbEntrees - 1;
    FNumeroEntree:= EWE;
  end;
  lsbListe.ItemIndex := EWE;
  lsbListe.Enabled:=True;
end;
// lister les réseaux
procedure TdlgProjectManagerWdScr.ListerLesReseaux(const QIdx: integer);
var
  i: Integer;
  R: TReseau;
  EWE: Integer;
begin
  EWE := QIdx;
  lsbListe.Enabled:=False;
  lsbListe.Clear;
  for i:= 0 to FDocumentToporobot.GetNbReseaux- 1 do
  begin
    R := FDocumentToporobot.GetReseau(i);
    lsbListe.Items.Add(Format('%d: %s', [R.IdxReseau, R.NomReseau]));
  end;
  if (QIdx = -1) then
  begin
    EWE := FDocumentToporobot.GetNbReseaux - 1;
    FNumeroReseau := EWE;
  end;
  lsbListe.ItemIndex := EWE;
  lsbListe.Enabled:=True;
end;

// lister les séries
procedure TdlgProjectManagerWdScr.ListerLesSeries(const QIdx: integer);
var
  i: Integer;
  S: TObjSerie;
  EWE: Integer;
begin
  EWE := QIdx;
  lsbListe.Enabled:=False;
  lsbListe.Clear;
  for i:= 0 to FDocumentToporobot.GetNbSeries - 1 do
  begin
    S := FDocumentToporobot.GetSerie(i);
    lsbListe.Items.Add(Format('%d: %s', [S.GetIndexSerie, S.GetNomSerie]));
  end;
  if (QIdx = -1) then
  begin
    EWE := FDocumentToporobot.GetNbSeries - 1;
    FNumeroSerie := EWE;
  end;
  lsbListe.ItemIndex := EWE;
  lsbListe.Enabled:=True;
end;
// lister les expés
procedure TdlgProjectManagerWdScr.ListerLesExpes(const QIdx: integer);
var
  i: Integer;
  E: TExpe;
  EWE: Integer;
begin
  EWE := QIdx;
  lsbListe.Enabled:=False;
  lsbListe.Clear;
  for i:= 0 to FDocumentToporobot.GetNbExpes - 1 do
  begin
    E := FDocumentToporobot.GetExpe(i);
    lsbListe.Items.Add(Format('%d: %.2d/%.2d/%.4d - %s', [E.IDExpe, E.JourExpe, E.MoisExpe, E.AnneeExpe, E.Commentaire]));
  end;
  if (QIdx = -1) then
  begin
    EWE := FDocumentToporobot.GetNbExpes - 1;
    FNumeroExpe := EWE;
  end;
  lsbListe.ItemIndex := EWE;
  lsbListe.Enabled:=True;
end;
// lister les codes
procedure TdlgProjectManagerWdScr.ListerLesCodes(const QIdx: integer);
var
  i: Integer;
  C: TCode;
  EWE: Integer;

begin
  EWE := QIdx;
  lsbListe.Enabled:=False;
  lsbListe.Clear;
  for i:= 0 to FDocumentToporobot.GetNbCodes - 1 do
  begin
    C := FDocumentToporobot.GetCode(i);
    lsbListe.Items.Add(Format('%d: %s', [C.IDCode, C.Commentaire]));
  end;
  if (QIdx = -1) then
  begin
    EWE := FDocumentToporobot.GetNbCodes - 1;
    FNumeroCode := EWE;
  end;
  lsbListe.ItemIndex := EWE;
  lsbListe.Enabled:=True;
end;

// initialisation header
procedure TdlgProjectManagerWdScr.InitHeaderListe(const QModeBDD: TModeBDD);
var
 ht: THeaderSection;
 FNbItems: Integer;
 miou: String;

 procedure AjouterTitreColonne(const Titre: string; const LGMin, LG: integer);

 begin
   ht := hcColsTitres.Sections.Add;
   ht.Text := Titre;
   ht.MinWidth := LGMin;
   ht.Width    := LG;

 end;
begin
// purge des titres des headers
  hcColsTitres.Sections.Clear;
  // titres
  case QModeBDD of
    mbddENTRANCES: begin
                  miou := AnsiToUtf8(rsTBS_ENTRANCE);
                  FNbItems          := FDocumentToporobot.GetNbEntrees;
                  grbxListes.Caption := Format('%s (%d items)', [miou, FNbItems]);
                  with hcColsTitres do begin
                    AjouterTitreColonne('ID', 40, 40);
                    AjouterTitreColonne('Entrée', 150, 400);
                    AjouterTitreColonne('Réf', 80, 80);
                    AjouterTitreColonne('X', 80, 100);
                    AjouterTitreColonne('Y', 80, 100);
                    AjouterTitreColonne('Z', 80, 100);
                  end;

                end;
    mbddRESEAUX: begin
                  miou := AnsiToUtf8(rsTBS_RESEAUX);
                  FNbItems := FDocumentToporobot.GetNbReseaux;
                  grbxListes.Caption := Format('%s (%d items)', [miou, FNbItems]);
                  with hcColsTitres do begin
                    AjouterTitreColonne('ID', 40, 40);
                    AjouterTitreColonne('Couleur', 60, 60);
                    AjouterTitreColonne('Réseau', 150, 300);
                  end;
                end;
    mbddCODES:   begin
                  miou := AnsiToUtf8(rsTBS_CODES);
                  FNbItems := FDocumentToporobot.GetNbCodes;
                  grbxListes.Caption := Format('%s (%d items)', [miou, FNbItems]);
                  with hcColsTitres do begin
                    AjouterTitreColonne('ID', 40, 40);
                    AjouterTitreColonne('Azimuts', 80, 80);
                    AjouterTitreColonne('Pentes', 80, 80);
                    AjouterTitreColonne('Commentaires', 150, 300);
                  end;
                end;
    mbddEXPES:   begin
                  miou := AnsiToUtf8(rsTBS_TRIPS);
                  FNbItems := FDocumentToporobot.GetNbExpes;
                  grbxListes.Caption := Format('%s (%d items)', [miou, FNbItems]);
                  with hcColsTitres do begin
                    AjouterTitreColonne('ID', 40, 40);
                    AjouterTitreColonne('Couleur', 80, 80);
                    AjouterTitreColonne('Date', 90, 90);
                    AjouterTitreColonne('Spéléomètre', 100, 250);
                    AjouterTitreColonne('Spéléographe', 100, 250);
                    AjouterTitreColonne('Déclinaison', 70, 70);
                    AjouterTitreColonne('Commentaires', 150, 300);
                  end;


                end;
    mbddSERIES:  begin
                  miou := AnsiToUtf8(rsTBS_SERIES);
                  FNbItems := FDocumentToporobot.GetNbSeries;
                  grbxListes.Caption := Format('%s (%d items)', [miou, FNbItems]);
                  with hcColsTitres do begin
                    AjouterTitreColonne('ID', 40, 40);
                    AjouterTitreColonne('Départ', 70, 70);
                    AjouterTitreColonne('Arrivée', 70, 70);
                    AjouterTitreColonne('Nom', 100, 300);
                    AjouterTitreColonne('Réseau', 100, 300);
                    AjouterTitreColonne('Nb points', 60, 60);
                 end;
                end;
  end;
end;

procedure TdlgProjectManagerWdScr.lsbListeDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
const
    Q4  = 4; // compensation du décalage entre le header et la liste
    mg  = 1;
    // lgr = 25;
  var

    C: TColor;
    m: integer;
    r: TRect;
    rs: TReseau;
    es: TEntrance;
    cs: TCode;
    ss: TExpe;
    sr: TObjSerie;
    miou: string;
    procedure DessineFiletColonne(const TB: integer);
    begin
      with lsbListe do begin
        Canvas.MoveTo(TB, ARect.Top);
        Canvas.LineTo(TB, ARect.Bottom);
      end;
    end;
    // bg = couleur de fond; tc= couleur du texte
    procedure DessineItem(const bg,tc: TColor);
    VAR
      //TB: integer;
      HS: THeaderSection;
    begin
      case FModeBDD of
        mbddENTRANCES:
          begin
            with lsbListe do
            begin

              //Canvas.Brush.Color:=clWhite;
              Canvas.FillRect(ARect);

              (*
              canvas.Pen.Color:=clBlack;
              canvas.Brush.Color:=rs.ColorReseau;
              canvas.Rectangle(R);
              //Canvas.Brush.Color:=clWhite;
              //*)
              Canvas.Brush.Color:=bg;
              Canvas.Font.Color :=tc;
              Canvas.Pen.Color  :=clSilver; // pour les filets




              //TB := hcColsTitres.hea
              HS := hcColsTitres.Sections.Items[0];  // ID entrée
              canvas.TextOut(HS.Left + 4, ARect.Top+1,  Format('%d',[es.eNumEntree]));
              HS := hcColsTitres.Sections.Items[1];  // Nom entrée
              DessineFiletColonne(HS.Left - Q4);
              canvas.TextOut(HS.Left + 4, ARect.Top+1, AnsiToUtf8(es.eNomEntree));
              HS := hcColsTitres.Sections.Items[2];
              DessineFiletColonne(HS.Left - Q4);
              canvas.TextOut(HS.Left + 4, ARect.Top+1, Format('%d.%d',[es.eRefSer, es.eRefSt]));
              HS := hcColsTitres.Sections.Items[3];
              DessineFiletColonne(HS.Left - Q4);
              canvas.TextOut(HS.Left + 4, ARect.Top+1, FormatterNombreAvecSepMilliers(es.eXEntree));
              HS := hcColsTitres.Sections.Items[4];
              DessineFiletColonne(HS.Left - Q4);
              canvas.TextOut(HS.Left + 4, ARect.Top+1, FormatterNombreAvecSepMilliers(es.eYEntree));
              HS := hcColsTitres.Sections.Items[5];
              DessineFiletColonne(HS.Left - Q4);
              canvas.TextOut(HS.Left + 4, ARect.Top+1, FormatterNombreAvecSepMilliers(es.eZEntree));
              DessineFiletColonne(HS.Right);


            end;
          end;
        mbddRESEAUX:
          begin
            with lsbListe do
            begin
              // offset:=Rect.Left + 30 + lgr;
              //Canvas.Brush.Color:=clWhite;
              Canvas.FillRect(ARect);
              canvas.Pen.Color:=clSilver;
              Canvas.Font.Color :=tc;

              HS := hcColsTitres.Sections.Items[0];  // ID réseau
              canvas.TextOut(HS.Left + 4, ARect.Top+1,  Format('%d',[rs.IdxReseau]));
              HS := hcColsTitres.Sections.Items[1];  // Couleur réseau
              DessineFiletColonne(HS.Left - Q4);
              canvas.Pen.Color:=clBlack;
              canvas.Brush.Color:=rs.ColorReseau; // couleur du réseau
              r.Left :=HS.Left;
              r.Right:=HS.Right - 8;
              r.Top  :=ARect.Top + mg;
              r.Bottom:=ARect.Bottom - mg;
              canvas.Rectangle(R);
              Canvas.Brush.Color:=bg;
              canvas.Pen.Color:=clSilver;
              HS := hcColsTitres.Sections.Items[2];  // Nom réseau
              DessineFiletColonne(HS.Left - Q4);


              //Canvas.Font.Color :=clBlack;
              Canvas.Font.Color :=tc;
              canvas.TextOut(HS.Left + 4, ARect.Top+1, AnsiToUtf8(rs.NomReseau));
            end;
          end;
        mbddCODES:
          with lsbListe do begin
            begin
              Canvas.FillRect(ARect);
              HS := hcColsTitres.Sections.Items[0];  // Nom réseau

              Canvas.Brush.Color:=bg;
              Canvas.Font.Color :=tc;
              canvas.Pen.Color:=clSilver;

              canvas.TextOut(HS.Left + 4  , ARect.Top+1, Format('%d', [cs.IDCode]));

              HS := hcColsTitres.Sections.Items[1];  // az
              DessineFiletColonne(HS.Left - Q4);
              canvas.TextOut(HS.Left + 4  , ARect.Top+1, Format('%.0f', [cs.GradAz]));
              HS := hcColsTitres.Sections.Items[2];  // p
              DessineFiletColonne(HS.Left - Q4);
              canvas.TextOut(HS.Left + 4, ARect.Top+1, Format('%.0f', [cs.Gradinc]));
              HS := hcColsTitres.Sections.Items[3];  // Commentaires
              DessineFiletColonne(HS.Left - Q4);
              canvas.TextOut(HS.Left + 4, ARect.Top+1, AnsiToUtf8(cs.Commentaire));
            end;
          end;
        mbddEXPES:
          with lsbListe do begin
            begin
              try
                miou := DateToStr(SafeEncodeDate(ss.AnneeExpe,ss.MoisExpe,ss.JourExpe));
              except
                miou := '01/01/2000';
              end;

              Canvas.FillRect(ARect);
              canvas.Pen.Color  :=clSilver;
              Canvas.Font.Color :=tc;

              HS := hcColsTitres.Sections.Items[0];  // ID expé
              canvas.TextOut(HS.Left + 4, ARect.Top+1,  Format('%d',[ss.IDExpe]));
              HS := hcColsTitres.Sections.Items[1];  // Couleur expé
              DessineFiletColonne(HS.Left - Q4);
              // couleur de l'expé
              canvas.Pen.Color:=clBlack;
              canvas.Brush.Color:=FPalette256.GetColorByIndex(ss.Couleur);

              r.Left :=HS.Left;
              r.Right:=HS.Left + 32;
              r.Top  :=ARect.Top + mg;
              r.Bottom:=ARect.Bottom - mg;
              canvas.Rectangle(R);

              Canvas.Brush.Color:=bg;
              Canvas.TextOut(HS.Left + 32 + 4, ARect.Top+1, Format('%d',[ss.Couleur]));

              //Canvas.Brush.Color:=bg;

              canvas.Pen.Color:=clSilver;

              HS := hcColsTitres.Sections.Items[2];  // date
              DessineFiletColonne(HS.Left - Q4);
              canvas.TextOut(HS.Left + 4, ARect.Top+1, miou);

              HS := hcColsTitres.Sections.Items[3];  // spéléomètre
              DessineFiletColonne(HS.Left - Q4);
              canvas.TextOut(HS.Left + 4, ARect.Top+1, AnsiToUtf8(ss.Speleometre));

              HS := hcColsTitres.Sections.Items[4];  // spéléographe
              DessineFiletColonne(HS.Left - Q4);
              canvas.TextOut(HS.Left + 4, ARect.Top+1, AnsiToUtf8(ss.Speleographe));

              HS := hcColsTitres.Sections.Items[5];  // déclinaison
              DessineFiletColonne(HS.Left - Q4);
              canvas.TextOut(HS.Left + 4, ARect.Top+1, Format('%.3f',[ss.Declinaison]));


              HS := hcColsTitres.Sections.Items[6];  // commentaires
              DessineFiletColonne(HS.Left - Q4);
              canvas.TextOut(HS.Left + 4, ARect.Top+1, AnsiToUtf8(ss.Commentaire));





            end;
          end;
        mbddSERIES:
            begin
              with lsbListe do
              begin
                Canvas.FillRect(ARect);
                canvas.Pen.Color:=clSilver;
                Canvas.Font.Color :=tc;

                HS := hcColsTitres.Sections.Items[0];  // ID série
                canvas.TextOut(HS.Left + 4, ARect.Top+1,  Format('%d',[sr.GetIndexSerie]));
                HS := hcColsTitres.Sections.Items[1];  // Départ
                DessineFiletColonne(HS.Left - Q4);
                canvas.TextOut(HS.Left + 4, ARect.Top+1,  Format('%d.%d',[sr.GetNoSerieDep, sr.GetNoPointDep]));
                HS := hcColsTitres.Sections.Items[2];  // Arrivée
                DessineFiletColonne(HS.Left - Q4);
                canvas.TextOut(HS.Left + 4, ARect.Top+1,  Format('%d.%d',[sr.GetNoSerieArr, sr.GetNoPointArr]));
                HS := hcColsTitres.Sections.Items[3];  // nom
                DessineFiletColonne(HS.Left - Q4);
                canvas.TextOut(HS.Left + 4, ARect.Top+1, AnsiToUtf8(sr.GetNomSerie));
                // réseau
                HS := hcColsTitres.Sections.Items[4];  // nom
                DessineFiletColonne(HS.Left - Q4);
                rs := FDocumentToporobot.GetReseau(sr.GetNoReseau);

                canvas.Pen.Color:=clBlack;
                canvas.Brush.Color:=rs.ColorReseau;
                r.Left :=HS.Left;
                r.Right:=HS.Left + 32;
                r.Top  :=ARect.Top + mg;
                r.Bottom:=ARect.Bottom - mg;
                canvas.Rectangle(R);
                Canvas.Brush.Color:= bg;
                Canvas.TextOut(HS.Left + 32 + 4, ARect.Top+1, AnsiToUtf8(rs.NomReseau));

                Canvas.Brush.Color:=bg;

                canvas.Pen.Color:=clSilver;

                HS := hcColsTitres.Sections.Items[5];  // Nb points
                DessineFiletColonne(HS.Left - Q4);
                canvas.TextOut(HS.Left + 4, ARect.Top+1,  Format('%d',[sr.GetNbVisees]));
              end;
            end;
        end; //case FModeSelection of
    end;
begin
  try
    case FModeBDD of
      mbddENTRANCES: es := FDocumentToporobot.GetEntree(Index);
      mbddRESEAUX  : rs := FDocumentToporobot.GetReseau(Index);
      mbddCODES    : cs := FDocumentToporobot.GetCode(Index);
      mbddEXPES    : ss := FDocumentToporobot.getExpe(Index);
      mbddSERIES   : sr := FDocumentToporobot.GetSerie(Index);

    end; //case FModeSelection of
    //ShowMessageFmt('%d',[Index]);

    with lsbListe do begin

      canvas.brush.color:=clwhite;
      //canvas.fillrect(rect);
      //on affiche le texte
      DessineItem(clwhite, clBlack);
      //affichage lorsque la ligne est sélectionnée
      if (odSelected in state) then begin
        canvas.brush.color:=clBlue;
        //canvas.fillrect(rect);
        //canvas.font.color:=clblue;
        //canvas.font.style:=Listbox1.canvas.font.style +[fsbold]+[fsitalic];
        DessineItem(clBlue, clWhite);
        //canvas.textout(rect.left+30,rect.top+2,listbox1.items[index]);
      end;

    end;

  except
  end;
end;


end.

