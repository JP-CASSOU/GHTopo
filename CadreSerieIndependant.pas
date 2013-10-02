unit CadreSerieIndependant;

{$INCLUDE CompilationParameters.inc}
// Date: 24/08/2012
// Version n'utilisant pas TToporobotstructure sauf pour les contrôles
// Statut: En cours de portage
// 18/06/2013: Gestion des déplacements dans la grille OK
// 19/06/2013: Contrôles de validité


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
  ToporobotClasses2012,   // pour contrôle de validité des codes et expés
  ObjetSerie,
  UEval, Common, CallDialogsStdVersion,
  Classes, SysUtils, FileUtil, Forms, Dialogs, Controls, ExtCtrls, Grids,
  StdCtrls, ActnList, Menus, curredit, LCLType,
  Clipbrd, Buttons;

type

  { TCdrSerieIndependant }

  TCdrSerieIndependant = class(TFrame)
    acAddLine: TAction;
    acEraseLine: TAction;
    acReplierTableur: TAction;
    acPasteFromClipboard: TAction;
    acCopierTableau: TAction;
    Action1: TAction;
    acUndoCopy: TAction;
    ActionList1: TActionList;
    btnGetSerStDepartByID: TButton;
    btnGetSerStArriveeByID: TButton;
    btnSelectReseau: TButton;
    Button1: TButton;
    chkLocked: TCheckBox;
    cmbChance: TComboBox;
    cmbObstacle: TComboBox;
    editRaideur: TCurrencyEdit;
    editCommentaire: TEdit;
    editNumeroReseau: TCurrencyEdit;
    editPointArrivee: TCurrencyEdit;
    editSerieDepart: TCurrencyEdit;
    editNomSerie: TEdit;
    editSerieArrivee: TCurrencyEdit;
    editPointDepart: TCurrencyEdit;
    editNumeroSerie: TCurrencyEdit;
    ImageList1: TImageList;
    imgHeaderColonnes: TImage;
    Label1: TLabel;
    Label2: TLabel;
    lbLongueurCumulee: TStaticText;
    lbNbStations: TLabel;
    lblReseau: TLabel;
    lblChances: TLabel;
    lblObstacles: TLabel;
    lblSerie: TLabel;
    lblSerieName: TLabel;
    lblDepart: TLabel;
    lblArrivee: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    Panel1: TPanel;
    pnlTableur: TPanel;
    grdStations: TStringGrid;
    lbIDLStDep: TStaticText;
    lbIDLStArr: TStaticText;
    lbInternalNumSerie: TStaticText;
    PopupMenu1: TPopupMenu;
    lbMessages: TStaticText;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    procedure acAddLineExecute(Sender: TObject);
    procedure acCopierTableauExecute(Sender: TObject);
    procedure acEraseLineExecute(Sender: TObject);
    procedure acPasteFromClipboardExecute(Sender: TObject);
    procedure acReplierTableurExecute(Sender: TObject);
    procedure acUndoCopyExecute(Sender: TObject);
    procedure btnCopyGridClick(Sender: TObject);
    procedure btnGetSerStDepartByIDClick(Sender: TObject);
    procedure btnGetSerStArriveeByIDClick(Sender: TObject);
    procedure btnSelectReseauClick(Sender: TObject);
    procedure FrameEnter(Sender: TObject);
    procedure grdStationsClick(Sender: TObject);

    procedure grdStationsDblClick(Sender: TObject);
    procedure grdStationsKeyDown(Sender: TObject; var Key: word;
      Shift: TShiftState);
  private
    { private declarations }
    FDocuToporobot: TToporobotStructure2012;

    FCurrentSerie: TObjSerie;
    FTableurDeplie: boolean;
    FLastItemFound: integer;
    FLastTextToFind: string;

    FModifie: boolean;


    procedure AttribuerExtremiteLibre(const S, P: integer);
    function ErrorInStation(const CurrColonne, CurrLigne: integer;
      const QValeur: double): integer;
    procedure InsertGrdLines(const N: integer);
    procedure SetCurrentSerie(const QS: TObjSerie);
    procedure InitCadre;
    procedure InitCaptions;

    procedure Undocopy(const MySelection: TGridRect; const AutoIncremented: boolean);

  public
    { public declarations }
    procedure Initialise(const DT: TToporobotStructure2012; const QS: TObjSerie);
    function GetCurrentSerie: TObjSerie;

    procedure RefreshTableaux;
    function ImplementerModifs: boolean;
    function IsModified: boolean;
    procedure SetModified(const m: boolean);
  end;

implementation

{$R *.lfm}
uses frmImportClipboardToSerie; // pour le gestionnaire de presse papiers

const
  //NB_LINES          = 500;
  NB_COLS = 13;
  NUM_COL_IDTERRAIN = 1;
  NUM_COL_TYPEGAL = 2;
  NUM_COL_CODE = 3;
  NUM_COL_EXPE = 1 + NUM_COL_CODE;
  NUM_COL_L = 1 + NUM_COL_EXPE;
  NUM_COL_A = 2 + NUM_COL_EXPE;
  NUM_COL_P = 3 + NUM_COL_EXPE;
  NUM_COL_LG = 4 + NUM_COL_EXPE;
  NUM_COL_LD = 5 + NUM_COL_EXPE;
  NUM_COL_HZ = 6 + NUM_COL_EXPE;
  NUM_COL_HN = 7 + NUM_COL_EXPE;
  NUM_COL_OBS = 8 + NUM_COL_EXPE;

  WDTH_COL_CODE = 40;
  WDTH_COL_EXPE = 40;
  WDTH_COL_L = 120;
  WDTH_COL_AZ = 80;
  WDTH_COL_P = 80;
  WDTH_COL_LG = 40;
  WDTH_COL_LD = WDTH_COL_LG;
  WDTH_COL_HZ = WDTH_COL_LG;
  WDTH_COL_HN = WDTH_COL_LG;


// Un TToporobotStructure2012 est indispensable pour l'appel
// des sélecteurs de Code et de Expe
procedure TCdrSerieIndependant.Initialise(const DT: TToporobotStructure2012;
  const QS: TObjSerie);
begin
  FModifie := False;
  FDocuToporobot := DT;
  SetCurrentSerie(QS);
  InitCaptions;
  InitCadre;
  RefreshTableaux;
end;

procedure TCdrSerieIndependant.SetCurrentSerie(const QS: TObjSerie);
begin
  FCurrentSerie := QS;
end;

function TCdrSerieIndependant.GetCurrentSerie: TObjSerie;
begin
  Result := FCurrentSerie;
end;

procedure TCdrSerieIndependant.InitCaptions;
begin
  FTableurDeplie := False;
  FLastItemFound := 0;
  FLastTextToFind := '';
  //btnValider.Caption  :=rsCDR_SERIE_VALIDATE;
  lblSerie.Caption := AnsiToUtf8(rsCDR_SERIE_NUMERO);
  lblSerieName.Caption := AnsiToUtf8(rsCDR_SERIE_NAME);
  lblDepart.Caption := AnsiToUtf8(rsCDR_SERIE_DEPART);
  lblArrivee.Caption := AnsiToUtf8(rsCDR_SERIE_ARRIVEE);
  lblChances.Caption := AnsiToUtf8(rsCDR_SERIE_CHANCE);
  lblObstacles.Caption := AnsiToUtf8(rsCDR_SERIE_OBSTACLE);
  lblReseau.Caption := AnsiToUtf8(rsCDR_SERIE_LB_RESEAU);
  acCopierTableau.Caption := AnsiToUtf8(rsCDR_SERIE_BTN_GRD_COPY);
  //chkLocked.Caption   :=rsCDR_SERIE_LOCKED;
  with cmbChance do
  begin
    Clear;
    Items.Add(AnsiToUtf8(rsCDR_SERIE_CHANCE0));
    Items.Add(AnsiToUtf8(rsCDR_SERIE_CHANCE1));
    Items.Add(AnsiToUtf8(rsCDR_SERIE_CHANCE2));
    Items.Add(AnsiToUtf8(rsCDR_SERIE_CHANCE3));
  end;
  with cmbObstacle do
  begin
    Clear;
    Items.Add(AnsiToUtf8(rsCDR_SERIE_OBSTACLE0));
    Items.Add(AnsiToUtf8(rsCDR_SERIE_OBSTACLE1));
    Items.Add(AnsiToUtf8(rsCDR_SERIE_OBSTACLE2));
    Items.Add(AnsiToUtf8(rsCDR_SERIE_OBSTACLE3));
    Items.Add(AnsiToUtf8(rsCDR_SERIE_OBSTACLE4));
    Items.Add(AnsiToUtf8(rsCDR_SERIE_OBSTACLE5));
    Items.Add(AnsiToUtf8(rsCDR_SERIE_OBSTACLE6));
    Items.Add(AnsiToUtf8(rsCDR_SERIE_OBSTACLE7));
    Items.Add(AnsiToUtf8(rsCDR_SERIE_OBSTACLE8));
    Items.Add(AnsiToUtf8(rsCDR_SERIE_OBSTACLE9));
    // spécifique GHTopo ( équivalent à AUTRE pour Toporobot)
    Items.Add(AnsiToUtf8(rsCDR_SERIE_OBSTACLE10));
    Items.Add(AnsiToUtf8(rsCDR_SERIE_OBSTACLE11));
    Items.Add(AnsiToUtf8(rsCDR_SERIE_OBSTACLE12));
    Items.Add(AnsiToUtf8(rsCDR_SERIE_OBSTACLE13));
    DropDownCount := cmbObstacle.Items.Count;
  end;
  acPasteFromClipboard.Caption := AnsiToUtf8(rsCDR_SERIE_PASTE);

end;

procedure TCdrSerieIndependant.InitCadre;
const
  LARGEUR_COLONNES_EXPE_CODE = 37;
  LARGEUR_COLONNES_LAP = 77;
  LARGEUR_COLONNES_LRUD = 49;
  NB_LINES = 500;
var
  i: integer;
  cc: integer;
  WU: integer;
  EWE: integer;
begin
  imgHeaderColonnes.Left := 187;
  // stations topo
  with grdStations do
  begin
    cmbChance.ItemIndex := 0;
    cmbObstacle.ItemIndex := 0;

    RowCount := 1 + NB_LINES;

    EWE := FCurrentSerie.GetNbVisees;
    // si la série ne comporte que la visée 0, ajouter une ligne vierge
    // RowCount := IIF((EWE > 1), EWE, EWE + 1);

    ColCount := NB_COLS;
    cc := 0;
    ColWidths[0] := 60;
    ColWidths[NUM_COL_IDTERRAIN] := 90; //60
    ColWidths[NUM_COL_TYPEGAL] := 30;
    ColWidths[NUM_COL_L] := LARGEUR_COLONNES_LAP;
    ColWidths[NUM_COL_A] := LARGEUR_COLONNES_LAP;
    ColWidths[NUM_COL_P] := LARGEUR_COLONNES_LAP;

    for i := NUM_COL_CODE to NUM_COL_EXPE do
      ColWidths[i] := LARGEUR_COLONNES_EXPE_CODE;
    for i := NUM_COL_LG to NUM_COL_HN do
      ColWidths[i] := LARGEUR_COLONNES_LRUD;
    for i := 0 to NUM_COL_OBS - 1 do
      cc := cc + ColWidths[i];

    ColWidths[NUM_COL_OBS] := Width - cc - 40;

    for i := 0 to RowCount - 1 do
      Cells[0, i] := Format(FMTSERST, [FCurrentSerie.GetIndexSerie, i]);
    WU := grdStations.Left + grdStations.GridLineWidth * 2;

    WU := WU + grdStations.ColWidths[0] + 1;
    WU := WU + grdStations.ColWidths[1] + 1;
    WU := WU + grdStations.ColWidths[2] + 1;
    //lbCode.Left       := WU;
    WU := WU + grdStations.ColWidths[3] + 1;
    //lbExpe.Left       := WU;
    WU := WU + grdStations.ColWidths[4] + 1;
    // Options de la grille
    // Sous Delphi, utiliser l'option goAlwaysShowEditor
    Options := [goAlwaysShowEditor, goAutoAddRows,
      goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine,
      goRangeSelect, goEditing, goTabs]; // ,goAlwaysShowEditor
    // Options de la grille
    //Options:=[goFixedVertLine,goFixedHorzLine,goVertLine,goHorzLine,
    //          goRangeSelect,goEditing,goTabs,goAlwaysShowEditor];


    // vider les cases ID Terrain
    lbIDLStDep.Caption := '';
    lbIDLStArr.Caption := '';

    // se mettre en tête de tableau
    Row := 0;
    Col := 1;
    TopRow := Row;

  end;
end;



procedure TCdrSerieIndependant.RefreshTableaux;
var
  i: integer;
  V: TUneVisee;
begin
  // en-tête
  editNumeroSerie.AsInteger := FCurrentSerie.GetIndexSerie;
  editNumeroReseau.AsInteger := FCurrentSerie.GetNoReseau;
  editNomSerie.Text := AnsiToUtf8(FCurrentSerie.GetNomSerie);
  editSerieDepart.AsInteger := FCurrentSerie.GetNoSerieDep;
  editPointDepart.AsInteger := FCurrentSerie.GetNoPointDep;
  editSerieArrivee.AsInteger := FCurrentSerie.GetNoSerieArr;
  editPointArrivee.AsInteger := FCurrentSerie.GetNoPointArr;
  editRaideur.Value := FCurrentSerie.GetRaideur;
  cmbChance.ItemIndex := FCurrentSerie.GetChance;
  cmbObstacle.ItemIndex := FCurrentSerie.GetObstacle;
  // tableau des points
  lbNbStations.Caption := Format('%d stations', [FCurrentSerie.GetNbVisees]);
  for i := 1 to grdStations.ColCount - 1 do
    grdStations.Cols[i].Clear;
  for i := 0 to grdStations.RowCount - 1 do
    grdStations.Cells[0, i] := Format(FMTSERST, [FCurrentSerie.GetIndexSerie, i]);
  //exit;
  for i := 0 to FCurrentSerie.GetNbVisees - 1 do
  begin
    V := FCurrentSerie.GetVisee(i);
    //V:=FCurrentDocTopo.GetStation(FNumeroSerie,i);
    try
      with grdStations do
      begin
        Cells[NUM_COL_IDTERRAIN, i] := V.IDTerrainStation;

        Cells[NUM_COL_TYPEGAL, i] := format('%d', [Ord(V.TypeVisee)]);
        Cells[NUM_COL_CODE, i] := format('%d', [V.Code]);
        Cells[NUM_COL_EXPE, i] := format('%d', [V.Expe]);
        Cells[NUM_COL_L, i] := format('%.2f', [V.Longueur]);
        Cells[NUM_COL_A, i] := format('%.2f', [V.Azimut]);
        Cells[NUM_COL_P, i] := format('%.2f', [V.Pente]);

        Cells[NUM_COL_LG, i] := format('%.2f', [V.LG]);
        Cells[NUM_COL_LD, i] := format('%.2f', [V.LD]);
        Cells[NUM_COL_HZ, i] := format('%.2f', [V.HZ]);
        Cells[NUM_COL_HN, i] := format('%.2f', [V.HN]);
        Cells[NUM_COL_OBS, i] := AnsiToUtf8(V.Commentaires);
      end;
    except
    end;
  end;
  grdStations.Col := 1;
  grdStations.Row := 1;
  AfficherMessage('RefreshTableaux OK');
  grdStations.Col := 1;
  grdStations.Row := 1;
end;

//******************************************************************************
procedure TCdrSerieIndependant.grdStationsDblClick(Sender: TObject);
var
  P: TPoint;
  QCol, QRow: integer;
  Q: integer;
  S: string;
  UnCode: TCode;
  UneExpe: TExpe;
begin
  //******************************************************
  // DONE: Ce code fonctionne mais l'événement OnDblClick est intercepté
  //       par l'éditeur de texte incorporé
  // Source: http://www.developpez.net/forums/d299139/ \n
  //         environnements-developpement/delphi/selection-cellule-tstringgrid-double-click/

  P := Mouse.CursorPos;
  //GetCursorPos(P) ;
  P := grdStations.ScreenToClient(P);
  grdStations.MouseToCell(P.X, P.Y, Qcol, QRow);
  //ShowMessage(Format('Cellule(%d, %d) = %s', [Qcol, QRow, grdStations.Cells[Qcol, QRow]]));


  //******************************************************
  //ShowMessage('ewe');
  if grdStations.Row = 0 then
    Exit;
  case grdStations.Col of
    NUM_COL_TYPEGAL: // sélection d'un type de galerie
    begin
      q := StrToIntDef(grdStations.Cells[NUM_COL_TYPEGAL, grdStations.Row], 0);
      q := ChooseTypeVisee(q);
      grdStations.Cells[NUM_COL_TYPEGAL, grdStations.Row] := Format('%d', [q]);

    end;
    NUM_COL_CODE:
    begin // sélectionner un code
      q := StrToIntDef(grdStations.Cells[NUM_COL_CODE, grdStations.Row], 1);
      // TODO: A réactiver après largage de l'ancienne version de TToporobotStructure

      q := SelectionDansListe(FDocuToporobot, mslCODE, q, False);
      if (q > 0) then
        UnCode := FDocuToporobot.GetCodeByIndex(q)
      else
        UnCode := FDocuToporobot.GetCodeByIndex(1);
      //*)


      grdStations.Cells[NUM_COL_CODE, grdStations.Row] :=
        Format('%d', [UnCode.IDCode]);

    end;
    NUM_COL_EXPE:
    begin // sélectionner une expé
      q := StrToIntDef(grdStations.Cells[NUM_COL_EXPE, grdStations.Row], 1);
      // TODO: A réactiver après largage de l'ancienne version de TToporobotStructure

      q := SelectionDansListe(FDocuToporobot, mslEXPE, q, False);
      if (q > 0) then
        UneExpe := FDocuToporobot.GetExpeByIndex(Q)
      else
        UneExpe := FDocuToporobot.GetExpeByIndex(1);
      //*)
      grdStations.Cells[NUM_COL_EXPE, grdStations.Row] :=
        Format('%d', [UneExpe.IDExpe]);

    end;
    NUM_COL_OBS:
    begin // commentaire station
      s := grdStations.Cells[NUM_COL_OBS, grdStations.Row];
      if InputQuery(rsINPUT_COMMENTAIRE_TITRE, rsINPUT_COMMENTAIRE_MSG, s) then
        grdStations.Cells[NUM_COL_OBS, grdStations.Row] := s;
    end;
    else;
  end;
end;


procedure TCdrSerieIndependant.grdStationsKeyDown(Sender: TObject;
  var Key: word; Shift: TShiftState);
var
  S: string;
  Err: boolean;
  Q: integer;
  EWE: double;
begin
  // /!\: Propriété AutoAdvance doit être désactivée
  //------------------------
  //ShowMessage('wu');
  with grdStations do
  begin
    //EditorMode := True;
    // Colonne 1: Tous caractères interdits sauf ceux spécifiés
    if Col = 1 then
    begin
      if EditorMode then
      begin // EditorMode = activé lorsqu'on saisit une valeur
        //              désactivé lors de la validation ou le déplacement
        if not (Key in [13, 8, Ord('A') .. Ord('Z'),
          Ord('a') .. Ord('z'), Ord('0') .. Ord('9'),
          Ord('.'), Ord('-'), Ord('_'),
          Ord('+')]) then
          Key := 0;
      end;
    end;
    // Valeurs numériques:
    // Adaptation aux claviers sans pavé numérique (portables)
    // la lettre M est remplacée par un signe moins
    if Col in [NUM_COL_L, NUM_COL_A, NUM_COL_P, NUM_COL_LG,
      NUM_COL_LD, NUM_COL_HZ, NUM_COL_HN] then
    begin
      if ((Key = Ord('M')) or (Key = Ord('m'))) then
        Key := Ord('-');
    end;
    // appui sur les touches
    case key of
      VK_RETURN:
      begin
        AfficherMessage(Format('<RETURN> pressé (Colonne: %d)', [Col]));
        // si on se trouve en colonne Long..Pente .. LRUD,
        // on évalue l'expression si la cellule contient un signe égal en tête
        if (Col in [NUM_COL_EXPE, NUM_COL_CODE, NUM_COL_L,
          NUM_COL_A, NUM_COL_P, NUM_COL_LG, NUM_COL_LD,
          NUM_COL_HZ, NUM_COL_HN]) then
        begin

          S := Trim(Cells[Col, Row]);
          if S = '' then
            S := '0.00'; // cellule vide => valeur 0.00

          if S[1] = '=' then
            System.Delete(S, 1, 1); // pour ancienne méthode et habitudes de tableur
          EWE := EvalExpr(S, Err);
          if (Col in [NUM_COL_EXPE, NUM_COL_CODE]) then
            Cells[Col, Row] := Format('%d', [Trunc(EWE)])
          else
            Cells[Col, Row] := Format('%f', [EWE]);
          // erreur = se replacer sur la case concernée
          Q := ErrorInStation(Col, Row, EWE);
          if (Q > 0) then
            Col := Q - 1; // recule d'une colonne pour contrebalancer l'incrément de colonne
        end;
        // si on arrive en fin de ligne,
        // on passe à la suivante
        // en recopiant le type de visée, l'Expé et le Code
        // et on se positionne sur la colonne longueur

        if ((Col + 1) > NUM_COL_OBS) then
        begin
          Row := Row + 1;
          Cells[NUM_COL_IDTERRAIN, Row] :=
            IncrementString(Cells[NUM_COL_IDTERRAIN, Row - 1]);
          //Cells[NUM_COL_IDTERRAIN,Row]:=Cells[NUM_COL_IDTERRAIN, Row-1];
          Cells[NUM_COL_TYPEGAL, Row] := Cells[NUM_COL_TYPEGAL, Row - 1]; // type visée
          Cells[NUM_COL_CODE, Row] := Cells[NUM_COL_CODE, Row - 1];      // code
          Cells[NUM_COL_EXPE, Row] := Cells[NUM_COL_EXPE, Row - 1];      // expé


          Col := NUM_COL_EXPE;

        end;
        Col := Col + 1;
      end;
    end;
  end;
  //*)
end;

// En fonction des paramètres de la ligne, vérifie les données
function TCdrSerieIndependant.ErrorInStation(const CurrColonne, CurrLigne: integer;
  const QValeur: double): integer;
var
  E: TExpe;
  C: TCode;
  L: double;
  MsgErr: string;
begin
  Result := -1;
  MsgErr := 'OK';
  with grdStations do
  begin
    case CurrColonne of
      NUM_COL_CODE:
      begin
        if (not FDocuToporobot.ExistsCode(Trunc(QValeur))) then
        begin
          Result := CurrColonne;
          MsgErr := rsCDR_SERIE_MSG_ERR_CODE_NOT_FOUND;
        end;
      end;
      NUM_COL_EXPE:
      begin
        if (not FDocuToporobot.ExistsExpe(Trunc(QValeur))) then
        begin
          Result := CurrColonne;
          MsgErr := rsCDR_SERIE_MSG_ERR_EXPE_NOT_FOUND;
        end;
      end;
      NUM_COL_L:
      begin
        if (not IsInRange(QValeur, 0.009, MAX_LENGTH_VISEE_4_TOPOROBOT)) then
        begin
          Result := CurrColonne;
          MsgErr := Format(rsCDR_SERIE_MSG_ERR_LONG, [MAX_LENGTH_VISEE_4_TOPOROBOT]);
        end;
      end;
      NUM_COL_LG .. NUM_COL_HN:
      begin
        if (QValeur < 0.00) then
        begin
          Result := CurrColonne;
          MsgErr := rsCDR_SERIE_MSG_ERR_LRUD;
        end;
      end
      else
        ;
    end;
    lbMessages.Caption := AnsiToUtf8(MsgErr);
    AfficherMessage(Format('%s.ErrorInStation: L%dC%d: %.2f = %d - %s',
      [ClassName, CurrColonne, CurrLigne, QValeur, Result, MsgErr]));

  end;

end;

procedure TCdrSerieIndependant.btnGetSerStDepartByIDClick(Sender: TObject);
var
  S, P: integer;
  IDL, Q: string;
begin
  if InputQuery(rsDLG_FIND_PT_BY_ID_TITLE, rsDLG_FIND_PT_BY_ID_PROMPT, IDL) then
  begin
    if FDocuToporobot.FindPtTopo(IDL, S, P) then
    begin
      editSerieDepart.AsInteger := S;
      editPointDepart.AsInteger := P;

      IDL := '   ' + IDL;
      S := Pos('|', IDL);
      Q := Trim(Copy(IDL, 1, S - 1));
      lbIDLStDep.Caption := Q;
      //if S>0 then
      grdStations.Cells[1, 0] := Q;

    end
    else
      ShowMessage(AnsiToUtf8(rsMATCHNOTFOUND));
  end;
end;

procedure TCdrSerieIndependant.acAddLineExecute(Sender: TObject);
var
  i, n: integer;
  NBL: string;
begin
  NBL := '1';
  if InputQuery(rsCDR_SERIE_INSERTLINE, rsCDR_SERIE_NBLINES,
    NBL) then
  begin
    n := StrToIntDef(NBL, 1);
    if IsInRange(N, 1, 50) then
    begin
      InsertGrdLines(N);
    end; // if
  end; // if inputquery
end;

procedure TCdrSerieIndependant.acCopierTableauExecute(Sender: TObject);
begin
  GRDCCopierUnTableau(grdStations);
end;

procedure TCdrSerieIndependant.acEraseLineExecute(Sender: TObject);
var
  i, j: integer;
begin
  with grdStations do
  begin
    if (QuestionOuiNon(Format('Ecraser ligne %d', [Row]))) then
    begin
      for i := Row to RowCount - 1 do
      begin
        for j := 1 to ColCount - 1 do
          Cells[j, i] := Cells[j, i + 1];
      end;
    end;
  end;
end;

procedure TCdrSerieIndependant.acPasteFromClipboardExecute(Sender: TObject);
var
  i, j: integer;
  ClipBoard: TClipboard;
  ValuesArray: TStringArray2D;
  Line0: integer;
  EWE: string;
begin
  with TdlgImportClipboardToSerie.Create(Application) do
  begin
    try
      ShowModal;
    finally
      Release;
    end;
  end;

  (*
  ClipBoard := TClipboard.Create(ctClipboard);
  try
   if (ClipBoard.HasFormat(CF_TEXT)) then
   begin

     AfficherMessage('> Collage interne');
     AfficherMessage('>> Le presse-papiers contient du texte');
     AfficherMessage('>> Contenu:');
     EWE := ClipBoard.AsText;
     // remplacer les virgules par des points
     EWE := StringReplace(EWE, ',', '.', [rfReplaceAll, rfIgnoreCase]);
     AfficherMessage(EWE);
     ValuesArray := GetStringArray2D(EWE, TAB);
     AfficherMessage('>> ----------');

      // affichage dans le tableau
      with grdStations do begin
        Line0 := Row;
        for i := 0 to MAX_SIZE_PARAM_ARRAY do begin
          for j := 0 to MAX_SIZE_PARAM_ARRAY do begin
            try
              grdStations.Cells[j+1,
                                i+grdStations.Selection.Top] := ValuesArray[i,j];

            except // échec ? Ligne suivante
              ;
            end;
          end; // for j
        end; // for i
      end; // with grdStations do begin


    end else
    begin
      ShowMessage('Presse-papiers vide ou invalide');
    end;
  finally
    ClipBoard.Free;
  end;
  //*)
end;

procedure TCdrSerieIndependant.acReplierTableurExecute(Sender: TObject);
const
  H1 = 64;
  H2 = 160;
  DT = H2 - H1;
begin
  FTableurDeplie := not (FTableurDeplie);
  if (FTableurDeplie) then
  begin
    pnlTableur.Top := H1;
    pnlTableur.Height := pnlTableur.Height + DT;
  end
  else
  begin
    pnlTableur.Top := H2;
    pnlTableur.Height := pnlTableur.Height - DT;
  end;
end;

procedure TCdrSerieIndependant.InsertGrdLines(const N: integer);
var
  i, j: integer;
begin
  with grdStations do
  begin
    AfficherMessage(Format('%s.InsertGrdLine', [ClassName]));
    for i := RowCount - 1 downto Row + N do
    begin
      for j := 1 to ColCount - 1 do
        Cells[j, i] := Cells[j, i - N];
    end;
    for i := ROW to ROW + N - 1 do
    begin
      for j := NUM_COL_L to NUM_COL_HN do
        Cells[j, i] := '';
      Cells[NUM_COL_OBS, i] := Format('** Inseree (%d) **', [1 + i - Row]);
    end;
  end;
end;

procedure TCdrSerieIndependant.acUndoCopyExecute(Sender: TObject);
begin
  Undocopy(grdStations.Selection, False);
end;

procedure TCdrSerieIndependant.btnCopyGridClick(Sender: TObject);
begin
  GRDCCopierUnTableau(grdStations);
end;

// recopie vers le bas
procedure TCdrSerieIndependant.Undocopy(const MySelection: TGridRect;
  const AutoIncremented: boolean);
var
  i, j, Nb: integer;
begin
  with grdStations do
  begin
    Nb := MySelection.Bottom - MySelection.Top;
    if Nb = 0 then
      Nb := StrToIntDef(InputBox(acUndoCopy.Caption, 'Nombre de lignes', IntToStr(Nb)), 1);

    for i := MySelection.Top + 1 to MySelection.Top + Nb do
    begin
      for j := MySelection.Left to MySelection.Right do
        Cells[j, i] := Cells[j, MySelection.Top];

    end;
    // auto incrémentation
    if AutoIncremented then
    begin
      for i := MySelection.Top + 1 to MySelection.Top + Nb do
      begin
        Cells[NUM_COL_IDTERRAIN, i] := IncrementString(Cells[NUM_COL_IDTERRAIN, i - 1]);
      end;
    end;
  end; // with grdstations
end;

procedure TCdrSerieIndependant.btnGetSerStArriveeByIDClick(Sender: TObject);
var
  S, P: integer;
  IDL, Q: string;
begin
  IDL := '';
  if InputQuery(AnsiToUtf8(rsDLG_FIND_PT_BY_ID_TITLE),
    AnsiToUtf8(rsDLG_FIND_PT_BY_ID_PROMPT), IDL) then
  begin
    if FDocuToporobot.FindPtTopo(IDL, S, P) then
    begin
      editSerieArrivee.AsInteger := S;
      editPointArrivee.AsInteger := P;
      //lbIDLStArr.Caption   := IDL;

      IDL := '   ' + IDL;
      S := Pos('|', IDL);
      Q := Trim(Copy(IDL, 1, S - 1));
      lbIDLStArr.Caption := Q;

    end
    else
      ShowMessage(AnsiToUtf8(rsMATCHNOTFOUND));
  end;
end;

procedure TCdrSerieIndependant.btnSelectReseauClick(Sender: TObject);
begin
  //TODO: A reactiver après inertage de TToporobotStructure
  editNumeroReseau.AsInteger :=
    SelectionDansListe(FDocuToporobot,
    mslRESEAUX,
    editNumeroReseau.AsInteger,
    False);
end;

procedure TCdrSerieIndependant.FrameEnter(Sender: TObject);
begin
  FModifie := True;
  AfficherMessage('Element modifie');
end;

procedure TCdrSerieIndependant.grdStationsClick(Sender: TObject);
var
  L: extended;
  i: integer;
  EWE: extended;
begin
  L := 0.00;
  with grdStations do
  begin
    if (Row > 0) then
    begin
      for i := 1 to Row do
      begin
        EWE := StrToFloatDef(Cells[NUM_COL_L, i], 0.00);
        L := L + EWE;
      end;
    end;
    lbLongueurCumulee.Caption := Format('%.2f m', [L]);
  end;
end;



// calculer et attribuer extrémité libre
// DONE: Ajout de la variable wu pour éviter réutilisation de var de boucle en dehors de celle-ci
procedure TCdrSerieIndependant.AttribuerExtremiteLibre(const S, P: integer);
var
  b: boolean;
  i: integer;
  wu: integer;
begin
  if chkLocked.Checked then
    Exit;
  //if (S>0) or (P>0) then Exit;
  wu := 0;
  with grdStations do
  begin
    for i := 0 to RowCount - 1 do
    begin
      wu := i;
      b := (i > 0) and (Cells[NUM_COL_L, i] = '') and
        (Cells[NUM_COL_A, i] = '') and (Cells[NUM_COL_P, i] = '');
      if b then
        Break;
    end; // for
    editPointArrivee.AsInteger := wu - 1;
    editSerieArrivee.AsInteger := editNumeroSerie.AsInteger;

  end;
end;

// Sauvegarde modifs formulaire et tableau
function TCdrSerieIndependant.ImplementerModifs: boolean;
var
  i, Q: integer;
  S: TObjSerie;
  b: boolean;
  V: TUneVisee;
  // vérification des stations
  function CheckAStation(const AStation: TUneVisee): integer;
  var
    AMin, AMax: double;
    CC: TCode;
    EE: TExpe;

  begin
    Result := 0;
    // code et expé
    // TODO: Revoir la méthode de contrôle
    // (fonctions de la forme ExistsCode(const Idx: integer): boolean; )
    if not IsInRange(AStation.Code, 0, FDocuToporobot.GetNbCodes) then
      ShowMessageFmt('Code invalide en station %d', [i]);
    if not IsInRange(AStation.Expe, 0, FDocuToporobot.GetNbExpes) then
      ShowMessageFmt('Séance invalide en station %d', [i]);
    // longueur, ne doit pas dépasser 160 m
    if not IsInRange(AStation.Longueur, 0.00, 160.00) then
      ShowMessageFmt('Longueur hors limites en station %d', [i]);
    CC := FDocuToporobot.GetCode(AStation.Code);
    // azimuts
    AMin := 0.00;
    case Trunc(CC.GradAz) of
      359, 360: AMax := 360.00; // directes degrés
      399, 400: AMax := 400.00; // directes grades
      349, 350: AMax := 360.00; // inverses degrés
      389, 390: AMax := 400.00; // inverses grades
    end;
    if not IsInRange(AStation.Azimut, AMin, AMax) then
      ShowMessageFmt('Azimut incorrect en station %d', [i]);
    // pentes
    case Trunc(CC.GradInc) of
      360:
      begin
        AMin := -90.00;
        AMax := 90.00;
      end; // zéro horizontal degré, directe
      400:
      begin
        AMin := -100.00;
        AMax := 100.00;
      end; // zéro horizontal grade, directe
      361:
      begin
        AMin := 0.00;
        AMax := 180.00;
      end; // zéro zénithal degrés
      401:
      begin
        AMin := 0.00;
        AMax := 200.00;
      end; // zéro zénithal grades
      359:
      begin
        AMin := 0.00;
        AMax := 180.00;
      end; // zéro nadiral degrés
      399:
      begin
        AMin := 0.00;
        AMax := 200.00;
      end; // zéro nadiral grades
      370:
      begin
        AMin := -580.00;
        AMax := 580.00;
      end; // pourcentage; +/- 80° max
      380:
      begin
        AMin := -160.00;
        AMax := 160.00;
      end; // dénivelés; maxi=long max visée
      350:
      begin
        AMin := -90.00;
        AMax := 90.00;
      end; // zéro horizontal degré, inverse
      390:
      begin
        AMin := 100.00;
        AMax := 100.00;
      end; // zéro horizontal grade, inverse
      else
      begin
        AMin := -100.00;
        AMax := 100.00;
      end;
    end;
    if not IsInRange(AStation.Pente, AMin, AMax) then
      ShowMessageFmt('Pente incorrecte en station %d', [i]);
    // largeurs et hauteurs
    if (not IsInRange(AStation.LG, 0.00, 200.00)) or
      (not IsInRange(AStation.LD, 0.00, 200.00)) or
      (not IsInRange(AStation.HZ, 0.00, 400.00)) or
      (not IsInRange(AStation.HN, 0.00, 400.00)) then
      ShowMessageFmt('Largeur ou hauteur incorrecte en station %d', [i]);
  end;

begin
  {$DEFINE ACTIVETRY}
  {$UNDEF ACTIVETRY}
  AfficherMessage(Format('%s.ImplementerModifs', [ClassName]));
  Result := False;
  // calcul éventuel estrémités de série
  AttribuerExtremiteLibre(editSerieArrivee.AsInteger, editPointArrivee.AsInteger);
  {$IFDEF ACTIVETRY}
  try
  {$ENDIF}
    //FSerie:=GetSerieFromForm;
    //sauvegarde du formulaire:
    with FCurrentSerie do
    begin
      SetIndexSerie(editNumeroSerie.AsInteger);
      SetNomSerie(Trim(Utf8ToAnsi(editNomSerie.Text)));
      SetNoReseau(editNumeroReseau.AsInteger);
      SetSeriePtExtremites(editSerieDepart.AsInteger, editPointDepart.AsInteger,
        editSerieArrivee.AsInteger, editPointArrivee.AsInteger);
      SetChanceObstacle(cmbChance.ItemIndex, cmbObstacle.ItemIndex);
      SetRaideur(editRaideur.Value);
      SetObsSerie(Trim(editCommentaire.Text));
      // Purger la liste des points topos
      ClearSerie;
    end;
    // analyser le tableur:
    // critère de sortie: Colonne 1 à 5 vides ou nulles

    with grdStations do
    begin
      AfficherMessage('  Checking values');
      // si les colonnes Long, Az et P sont vides,
      // la fin de la table des stations est supposée atteinte
      for i := 0 to RowCount - 1 do
      begin
        b := (i > 0) and (Cells[NUM_COL_L, i] = '') and
          (Cells[NUM_COL_A, i] = '') and (Cells[NUM_COL_P, i] = '');
        if b then
          Break;
        // analyse ligne
        V.NoVisee := i;
        V.IDTerrainStation := UpperCase(Trim(Cells[NUM_COL_IDTERRAIN, i]));

        V.TypeVisee := GetTypeDeVisee(StrToIntDef(Cells[NUM_COL_TYPEGAL, i], 0));
        V.Code := StrToIntDef(Cells[NUM_COL_CODE, i], 0);
        V.Expe := StrToIntDef(Cells[NUM_COL_EXPE, i], 0);
        V.Longueur := StrToFloatDef(Cells[NUM_COL_L, i], 0);
        V.Azimut := StrToFloatDef(Cells[NUM_COL_A, i], 0);
        V.Pente := StrToFloatDef(Cells[NUM_COL_P, i], 0);
        V.LG := StrToFloatDef(Cells[NUM_COL_LG, i], 0);
        V.LD := StrToFloatDef(Cells[NUM_COL_LD, i], 0);
        V.HZ := StrToFloatDef(Cells[NUM_COL_HZ, i], 0);
        V.HN := StrToFloatDef(Cells[NUM_COL_HN, i], 0);
        V.Commentaires := Cells[NUM_COL_OBS, i];



        AfficherMessage(Format(
          '   St: %d,  %d  %d - %.2f %.2f %.2f - %.2f %.2f %.2f %.2f  "%s"',
          [V.NoVisee, V.Code, V.Expe,
          V.Longueur, V.Azimut, V.Pente,
          V.LG, V.LD, V.HZ, V.HN,
          V.Commentaires]));
        FCurrentSerie.AddVisee(V);
      end;
    end;
    // nombre de stations
    // Tout est OK: On met Result à TRUE;
    FModifie := False;
    AfficherMessage(Format('%s.ImplementerModifs OK', [ClassName]));
    Result := True;
  {$IFDEF ACTIVETRY}
  except
    AfficherMessage(Format('%s.ImplementerModifs KO', [ClassName]));
  end;
  {$ENDIF}
end;

function TCdrSerieIndependant.IsModified: boolean;
begin
  Result := FModifie;
end;

procedure TCdrSerieIndependant.SetModified(const m: boolean);
begin
  FModifie := m;
end;


end.
