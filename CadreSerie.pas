unit CadreSerie;
{$INCLUDE CompilationParameters.inc}
// Date: 19/04/2012
// Statut: Obsolète (remplacé par CadreSerieIndépendant)

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
  UEval, Common, CallDialogsStdVersion, StructuresDonnees, ToporobotClasses,
  Classes, SysUtils, FileUtil, Forms, Dialogs, Controls, ExtCtrls, Grids,
  StdCtrls, ActnList, Menus, curredit, LCLType;

type

  { TCdrSerie }

  TCdrSerie = class(TFrame)
    acAddLine: TAction;
    acEraseLine: TAction;
    acReplierTableur: TAction;
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
    imgHeaderColonnes: TImage;
    Label1: TLabel;
    Label2: TLabel;
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
    Panel1: TPanel;
    pnlTableur: TPanel;
    grdStations: TStringGrid;
    lbIDLStDep: TStaticText;
    lbIDLStArr: TStaticText;
    lbInternalNumSerie: TStaticText;
    PopupMenu1: TPopupMenu;
    procedure acAddLineExecute(Sender: TObject);
    procedure acEraseLineExecute(Sender: TObject);
    procedure acReplierTableurExecute(Sender: TObject);
    procedure acUndoCopyExecute(Sender: TObject);
    procedure btnGetSerStDepartByIDClick(Sender: TObject);
    procedure btnGetSerStArriveeByIDClick(Sender: TObject);
    procedure btnSelectReseauClick(Sender: TObject);
    procedure grdStationsDblClick(Sender: TObject);
    procedure grdStationsEditingDone(Sender: TObject);
    procedure grdStationsKeyPress(Sender: TObject; var Key: char);
    procedure grdStationsUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
  private
    { private declarations }
    FCurrentDocTopo    : TToporobotStructure;
    FNumeroInterneSerie: integer;
    FNumeroToporobotSerie: integer;
    FCurrentSerie      : TUneSerie;
    FTableurDeplie     : boolean;
    FLastItemFound     : integer;
    FLastTextToFind    : string;


    procedure AttribuerExtremiteLibre(const S, P: integer);
    function ErrorInStation(const R: integer): integer;
    procedure InsertGrdLines(const N: integer);

    procedure SetCurrentDocTopo(const DT: TToporobotStructure);

    procedure InitCadre;
    procedure InitCaptions;
    function SetTypeGalerie(const t: byte): byte;
    procedure Undocopy(const MySelection: TGridRect;
      const AutoIncremented: boolean);            procedure SetCurrentSerie(const QS: TUneSerie);
  public
    { public declarations }
    function GetCurrentSerie: TUneSerie;

    procedure SetNumeroInterneSerie(const N: integer);
    procedure Initialise(const DT: TToporobotStructure; const QS: TUneSerie; const NoSerie: integer);
    procedure RefreshTableaux;
    function ImplementerModifs: boolean;

  end; 

implementation
{$R *.lfm}

const
  NB_LINES          = 500;
  NB_COLS           = 13;
  NUM_COL_IDTERRAIN = 1;
  NUM_COL_TYPEGAL   = 2;
  NUM_COL_CODE      = 3;
  NUM_COL_EXPE      = 1+NUM_COL_CODE;
  NUM_COL_L         = 1+NUM_COL_EXPE;
  NUM_COL_A         = 2+NUM_COL_EXPE;
  NUM_COL_P         = 3+NUM_COL_EXPE;
  NUM_COL_LG        = 4+NUM_COL_EXPE;
  NUM_COL_LD        = 5+NUM_COL_EXPE;
  NUM_COL_HZ        = 6+NUM_COL_EXPE;
  NUM_COL_HN        = 7+NUM_COL_EXPE;
  NUM_COL_OBS       = 8+NUM_COL_EXPE;

  WDTH_COL_CODE = 40;
  WDTH_COL_EXPE = 40;
  WDTH_COL_L    = 120;
  WDTH_COL_AZ   = 80;
  WDTH_COL_P  = 80;
  WDTH_COL_LG = 40;
  WDTH_COL_LD = WDTH_COL_LG;
  WDTH_COL_HZ = WDTH_COL_LG;
  WDTH_COL_HN = WDTH_COL_LG;






procedure TCdrSerie.SetNumeroInterneSerie(const N: integer);
begin
  FNumeroInterneSerie := N;
end;

function TCdrSerie.SetTypeGalerie(const t: byte): byte;
begin
  case t of
    0: Result:=tgDEFAULT; // = tgFOSSILE
    1: Result:=tgENTRANCE;
    2: Result:=tgFOSSILE;
    3: Result:=tgVADOSE;
    4: Result:=tgENNOYABLE;
    5: Result:=tgSIPHON;
    6: Result:=tgFIXPOINT;
    7: Result:=tgSURFACE;
    8: Result:=tgTUNNEL;
    9: Result:=tgMINE;
  else
    Result:=tgDEFAULT;
  end;
end;

procedure TCdrSerie.Initialise(const DT: TToporobotStructure; const QS: TUneSerie; const NoSerie: integer);
begin
  SetCurrentDocTopo(DT);
  SetCurrentSerie(QS);
  SetNumeroInterneSerie(NoSerie);
  InitCaptions;
  InitCadre;
  RefreshTableaux;
end;
procedure TCdrSerie.SetCurrentDocTopo(const DT: TToporobotStructure);
begin
  FCurrentDocTopo := DT;
end;

procedure TCdrSerie.SetCurrentSerie(const QS: TUneSerie);
begin
  FCurrentSerie := QS;
  FNumeroToporobotSerie  := FCurrentSerie.IndexSerie;   // TODO: Vérifier si c'est le numéro interne
end;
function TCdrSerie.GetCurrentSerie: TUneSerie;
begin
  Result := FCurrentSerie;
end;

procedure TCdrSerie.InitCaptions;
begin
 FTableurDeplie := False;
 FLastItemFound := 0;
 FLastTextToFind := '';
 //btnValider.Caption  :=rsCDR_SERIE_VALIDATE;
 lblSerie.Caption    := AnsiToUtf8(rsCDR_SERIE_NUMERO);
 lblSerieName.Caption:= AnsiToUtf8(rsCDR_SERIE_NAME);
 lblDepart.Caption   := AnsiToUtf8(rsCDR_SERIE_DEPART);
 lblArrivee.Caption  := AnsiToUtf8(rsCDR_SERIE_ARRIVEE);
 lblChances.Caption  := AnsiToUtf8(rsCDR_SERIE_CHANCE);
 lblObstacles.Caption:= AnsiToUtf8(rsCDR_SERIE_OBSTACLE);
 lblReseau.Caption   := AnsiToUtf8(rsCDR_SERIE_LB_RESEAU);
 //chkLocked.Caption   :=rsCDR_SERIE_LOCKED;
 with cmbChance do begin
   Clear;
   Items.Add(AnsiToUtf8(rsCDR_SERIE_CHANCE0));
   Items.Add(AnsiToUtf8(rsCDR_SERIE_CHANCE1));
   Items.Add(AnsiToUtf8(rsCDR_SERIE_CHANCE2));
   Items.Add(AnsiToUtf8(rsCDR_SERIE_CHANCE3));
 end;
 with cmbObstacle do begin
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

end;

procedure TCdrSerie.InitCadre;
const
  LARGEUR_COLONNES_EXPE_CODE = 37;
  LARGEUR_COLONNES_LAP       = 77;
  LARGEUR_COLONNES_LRUD      = 49;
var
  i: integer;
  cc: integer;
  WU: integer;
begin
  imgHeaderColonnes.Left := 187;

  // stations topo
  with grdStations do begin
    cmbChance.ItemIndex:=0;
    cmbObstacle.ItemIndex:=0;
    RowCount:=1+NB_LINES;
    ColCount:=NB_COLS;
    cc:=0;
    ColWidths[0]:=60;
    ColWidths[NUM_COL_IDTERRAIN]:= 90; //60
    ColWidths[NUM_COL_TYPEGAL]  := 30;
    ColWidths[NUM_COL_L]        := LARGEUR_COLONNES_LAP;
    ColWidths[NUM_COL_A]        := LARGEUR_COLONNES_LAP;
    ColWidths[NUM_COL_P]        := LARGEUR_COLONNES_LAP;

    for i:=NUM_COL_CODE to NUM_COL_EXPE do ColWidths[i]:=LARGEUR_COLONNES_EXPE_CODE;
    for i:=NUM_COL_LG to NUM_COL_HN do ColWidths[i]:=LARGEUR_COLONNES_LRUD;
    for i:=0 to NUM_COL_OBS-1 do cc:=cc+ColWidths[i];

    ColWidths[NUM_COL_OBS]:=Width-cc-40;

    for i:=0 to RowCount-1 do
      Cells[0,i]:=Format(FMTSERST,[FNumeroToporobotSerie,i]);
    WU :=grdStations.Left + grdStations.GridLineWidth * 2;

    WU    := WU + grdStations.ColWidths[0] + 1;
    WU    := WU + grdStations.ColWidths[1] + 1;
    WU    := WU + grdStations.ColWidths[2] + 1;
    //lbCode.Left       := WU;
    WU    := WU + grdStations.ColWidths[3] + 1;
    //lbExpe.Left       := WU;
    WU    := WU + grdStations.ColWidths[4] + 1;
    // Options de la grille
    // Sous Delphi, utiliser l'option goAlwaysShowEditor
    Options:=[goFixedVertLine,goFixedHorzLine,goVertLine,goHorzLine,
              goRangeSelect, goEditing, goTabs]; // ,goAlwaysShowEditor
    // Options de la grille
    //Options:=[goFixedVertLine,goFixedHorzLine,goVertLine,goHorzLine,
    //          goRangeSelect,goEditing,goTabs,goAlwaysShowEditor];


    // vider les cases ID Terrain
    lbIDLStDep.Caption :='';
    lbIDLStArr.Caption :='';

    // se mettre en tête de tableau
    Row := 0;
    Col := 1;
    TopRow := Row;
    //

  end;
end;



procedure TCdrSerie.RefreshTableaux;
var
  i: integer;
  V: TUneVisee;
begin
  // en-tête
  editNumeroSerie.AsInteger  := FCurrentSerie.IndexSerie;
  lbInternalNumSerie.Caption := Format('%d', [FNumeroInterneSerie]);
  editNumeroReseau.AsInteger := FCurrentSerie.Reseau;
  editNomSerie.Text          := FCurrentSerie.NomSerie;
  editSerieDepart.AsInteger  := FCurrentSerie.SerieDep;
  editPointDepart.AsInteger  := FCurrentSerie.PtDep;
  editSerieArrivee.AsInteger := FCurrentSerie.SerieArr;
  editPointArrivee.AsInteger := FCurrentSerie.PtArr;
  editRaideur.Value          := FCurrentSerie.Raideur;
  cmbChance.ItemIndex        := FCurrentSerie.Chances;
  cmbObstacle.ItemIndex      := FCurrentSerie.Obstacles;
  // tableau des points
  for i:=1 to grdStations.ColCount-1 do
    grdStations.Cols[i].Clear;
  for i:=0 to grdStations.RowCount-1 do
    grdStations.Cells[0,i]:=Format(FMTSERST,[FCurrentSerie.IndexSerie,i]);
  lbNbStations.Caption := Format('%d stations',[FCurrentSerie.PointsTopo.Count]);
   // tableau des points
  for i:=1 to grdStations.ColCount-1 do
    grdStations.Cols[i].Clear;
  for i:=0 to grdStations.RowCount-1 do
    grdStations.Cells[0,i]:=Format(FMTSERST,[FCurrentSerie.IndexSerie,i]);

  //exit;
  for i:=0 to FCurrentSerie.PointsTopo.Count-1 do begin
    V:=FCurrentDocTopo.GetStation(FNumeroInterneSerie, i);
    //V:=FCurrentDocTopo.GetStation(FNumeroSerie,i);
    try
    with grdStations do begin
      Cells[NUM_COL_IDTERRAIN,i]:=V.IDTerrainStation;

      Cells[NUM_COL_TYPEGAL,i]:=format('%d',[V.TypeGalerie]);
      Cells[NUM_COL_CODE,i]:=format('%d',[V.Code]);
      Cells[NUM_COL_EXPE,i]:=format('%d',[V.Expe]);
      Cells[NUM_COL_L,i]:=format('%.2f',[V.Longueur]);
      Cells[NUM_COL_A,i]:=format('%.2f',[V.Azimut]);
      Cells[NUM_COL_P,i]:=format('%.2f',[V.Pente]);

      Cells[NUM_COL_LG,i]:=format('%.2f',[V.LG]);
      Cells[NUM_COL_LD,i]:=format('%.2f',[V.LD]);
      Cells[NUM_COL_HZ,i]:=format('%.2f',[V.HZ]);
      Cells[NUM_COL_HN,i]:=format('%.2f',[V.HN]);
      Cells[NUM_COL_OBS,i]:=AnsiToUtf8(V.Commentaires);
    end;
    except
    end;
  end;
  grdStations.Col:=1;
  grdStations.Row:=1;
  AfficherMessage('RefreshTableaux OK');
  grdStations.Col:=1;
  grdStations.Row:=1;
end;

//******************************************************************************
procedure TCdrSerie.grdStationsDblClick(Sender: TObject);
var
  P :  TPoint ;
  QCol, QRow: integer ;
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

  P:=Mouse.CursorPos;
  //GetCursorPos(P) ;
  P:=grdStations.ScreenToClient(P);
  grdStations.MouseToCell(P.X, P.Y, Qcol, QRow);
  //ShowMessage(Format('Cellule(%d, %d) = %s', [Qcol, QRow, grdStations.Cells[Qcol, QRow]]));


  //******************************************************
  //ShowMessage('ewe');
  if grdStations.Row=0 then Exit;
  case grdStations.Col of
    NUM_COL_TYPEGAL: // sélection d'un type de galerie
       begin
         q:=StrToIntDef(grdStations.Cells[NUM_COL_TYPEGAL, grdStations.Row], 0);
         q := ChooseTypeVisee(q);
         grdStations.Cells[NUM_COL_TYPEGAL, grdStations.Row] := Format('%d',[q]);

       end;
    NUM_COL_CODE: begin // sélectionner un code
         q:=StrToIntDef(grdStations.Cells[NUM_COL_CODE, grdStations.Row],1);


         // DisplayInfoCode(UnCode);
         // TODO: Fonction ChoisirDansListes() à vérifier
         q := SelectionDansListe(FCurrentDocTopo, mslCODE, q);
         if (q>0) then
           UnCode := FCurrentDocTopo.GetCodeByIndex(q)
         else
           UnCode := FCurrentDocTopo.GetCodeByIndex(1);



         grdStations.Cells[NUM_COL_CODE, grdStations.Row] := Format('%d', [UnCode.IDCode]);

       end;
    NUM_COL_EXPE: begin // sélectionner une expé
         q:=StrToIntDef(grdStations.Cells[NUM_COL_EXPE, grdStations.Row],1);
         //UneExpe:=FDocTopo.GetExpeByIndex(Q);
         // DisplayInfoExpe(UneExpe);

         q := SelectionDansListe(FCurrentDocTopo, mslEXPE, q);
         if (q>0) then
           UneExpe := FCurrentDocTopo.GetExpeByIndex(Q)
         else
           UneExpe := FCurrentDocTopo.GetExpeByIndex(1);

         AfficherMessage(Format('%d - %d',[q, UneExpe.IDExpe]));



         grdStations.Cells[NUM_COL_EXPE, grdStations.Row] := Format('%d', [UneExpe.IDExpe]);


       end;
    NUM_COL_OBS:
       begin // commentaire station
         s := grdStations.Cells[NUM_COL_OBS, grdStations.Row];
         if InputQuery(rsINPUT_COMMENTAIRE_TITRE, rsINPUT_COMMENTAIRE_MSG, s) then
            grdStations.Cells[NUM_COL_OBS, grdStations.Row]:=s;
       end;
  else;
  end;
end;

procedure TCdrSerie.grdStationsEditingDone(Sender: TObject);
var
  S: string;
  Err: Boolean;
begin
  //showmessage('OnEditingDone '+ grdStations.Cells[grdStations.Col, grdStations.Row]);

  with grdStations do
  begin
    S:=Trim(Cells[Col, Row]);
    case Col of
      NUM_COL_IDTERRAIN:
      begin

      end;
      NUM_COL_TYPEGAL:
      begin
        ;
      end;
      NUM_COL_CODE:
      begin


      end;
      NUM_COL_EXPE:
      begin

      end;
      NUM_COL_L, NUM_COL_A, NUM_COL_P,
      NUM_COL_LG, NUM_COL_LD, NUM_COL_HZ, NUM_COL_HN :
      begin
        if (S = '') then S := '0.00'; // cellule vide => valeur 0.00
        if (S[1] = '=') then System.Delete(S,1,1); // pour ancienne méthode et habitudes de tableur
        Cells[Col, Row]:=Format('%f', [EvalExpr(S, Err)]);
      end;
    else

    end;
  end;
end;

// En fonction des paramètres de la ligne, vérifie les données
function TCdrSerie.ErrorInStation(const R: integer): integer;
var
  E: TExpe;
  C: TCode;
  L: Double;
begin
  Result:=-1;
  with grdStations do begin
    // vérification des types de galeries
    (*
    // vérification des codes
    if Not IsInRange(StrToFloatDef(Cells[NUM_COL_CODE, R],-1),
                     0,
                     FDocTopo.TableCodes.Count-1) then begin
      Result:=1;
      Exit;
    end;
    // vérification des expés
    if Not IsInRange(StrToFloatDef(Cells[NUM_COL_EXPE,R],-1),
                     0,
                     FDocTopo.TableExpes.Count-1) then begin
      Result:=2;
      Exit;
    end;
    //*)
    //(* vérification des longueurs

    L:=StrToFloatDef(Cells[NUM_COL_L, R],-1.0);
    AfficherMessage('64 - ' + floattostr(L));
    if Not IsInRange(L, 0.009, 160.00) then begin
      Result:=NUM_COL_L;
      exit;
    end;
    //*)
  end;

end;
// TODO: Résoudre les problèmes de comportement de la grille
procedure TCdrSerie.grdStationsKeyPress(Sender: TObject; var Key: char);
var
  Q: integer;
  Err: boolean;
  S: string;
begin
  //Exit;
  //------------------------
  //ShowMessage('wu');
  with grdStations do begin
    //EditorMode := True;
    // Colonne 1: Tous caractères interdits sauf ceux spécifiés
    if Col = 1 then begin
      if EditorMode then begin // EditorMode = activé lorsqu'on saisit une valeur
                               //              désactivé lors de la validation ou le déplacement
        if not (Key in [#13, #8,
                        'A'.. 'Z', 'a' .. 'z',
                        '0' .. '9', '.',
                        '-', '_', '+',
                        '''',
                        '#', '@']) then Key:=#0;
      end;
    end;
    // Valeurs numériques:
    // Adaptation aux claviers sans pavé numérique (portables)
    // la lettre M est remplacée par un signe moins
    if Col in [NUM_COL_L, NUM_COL_A, NUM_COL_P,
               NUM_COL_LG, NUM_COL_LD, NUM_COL_HZ, NUM_COL_HN] then begin
      if ((Key ='M') or (Key='m')) then Key:='-';
    end;
    // appui sur les touches
    case key of
      #13:
        begin

          // si on se trouve en colonne Long..Pente,
          // on évalue l'expression si la cellule contient un signe égal en tête
          if Col in [NUM_COL_L, NUM_COL_A, NUM_COL_P,
                     NUM_COL_LG, NUM_COL_LD, NUM_COL_HZ, NUM_COL_HN] then begin

            S:=Trim(Cells[Col, Row]);
            if S='' then S := '0.00'; // cellule vide => valeur 0.00

            if S[1]='=' then System.Delete(S,1,1); // pour ancienne méthode et habitudes de tableur
            Cells[Col, Row]:=Format('%f', [EvalExpr(S, Err)]);
            // vérification de la ligne
            if Col in [NUM_COL_L] then begin
              Q:=ErrorInStation(Row);
              if Q >0 then begin
                //Row:=Row-1;
                Col:=Q;
                // mise en surbrillance de la valeur erronée

                exit;
              end;
            end;

          end;
          // si on arrive en fin de ligne,
          // on passe à la suivante
          // en recopiant le type de visée, l'Expé et le Code
          // et on se positionne sur la colonne longueur

          if Col+1>NUM_COL_OBS then begin
            Row:=Row+1;
            Cells[NUM_COL_IDTERRAIN, Row]:=IncrementString(Cells[NUM_COL_IDTERRAIN, Row-1]);
            //Cells[NUM_COL_IDTERRAIN,Row]:=Cells[NUM_COL_IDTERRAIN, Row-1];
            Cells[NUM_COL_TYPEGAL,Row]:=Cells[NUM_COL_TYPEGAL,Row-1]; // type visée
            Cells[NUM_COL_CODE,Row]:=Cells[NUM_COL_CODE, Row-1];      // code
            Cells[NUM_COL_EXPE,Row]:=Cells[NUM_COL_EXPE, Row-1];      // expé


            Col:=NUM_COL_EXPE;

          end;
          Col:=Col+1;
        end;
    end;
  end;
end;

procedure TCdrSerie.grdStationsUTF8KeyPress(Sender: TObject;
  var UTF8Key: TUTF8Char);
begin
  ShowMessage('WU - grdStationsUTF8KeyPress');
end;

procedure TCdrSerie.btnGetSerStDepartByIDClick(Sender: TObject);
var
  S, P: integer;
  IDL, Q : string;
begin
  if InputQuery(rsDLG_FIND_PT_BY_ID_TITLE, rsDLG_FIND_PT_BY_ID_PROMPT, IDL) then
  begin
    if FCurrentDocTopo.FindPtTopo(IDL, S, P) then
    begin
      editSerieDepart.AsInteger := S;
      editPointDepart.AsInteger := P;

      IDL := '   '+ IDL;
      S:=Pos('|',IDL);
      Q := Trim(Copy(IDL, 1, S-1));
      lbIDLStDep.Caption   := Q;
      //if S>0 then
      grdStations.Cells[1,0] := Q;

    end else
      ShowMessage(AnsiToUtf8(rsMATCHNOTFOUND));
  end;
end;

procedure TCdrSerie.acAddLineExecute(Sender: TObject);
var
  i, n: integer;
  NBL: string;
begin
  NBL:='1';
  if InputQuery(rsCDR_SERIE_INSERTLINE,
                rsCDR_SERIE_NBLINES,
                NBL) then
  begin
    n:=StrToIntDef(NBL, 1);
    if IsInRange(N, 1, 50) then
    begin
        InsertGrdLines(N);
    end; // if
  end; // if inputquery
end;

procedure TCdrSerie.acEraseLineExecute(Sender: TObject);
var
  i,j: integer;
begin
  with grdStations do
  begin
    if (QuestionOuiNon(Format('Ecraser ligne %d',[Row]))) then
    begin
      for i:=Row to RowCount-1 do
      begin
        for j:=1 to ColCount-2 do Cells[j,i]:=Cells[j,i+1];
      end;
    end;
  end;
end;

procedure TCdrSerie.acReplierTableurExecute(Sender: TObject);
const
  H1 = 64;
  H2 = 160;
  DT = H2 - H1;
begin
  FTableurDeplie:=Not(FTableurDeplie);
  if (FTableurDeplie) then begin
    pnlTableur.Top:=H1;
    pnlTableur.Height:=pnlTableur.Height+DT;
  end else begin
    pnlTableur.Top:=H2;
    pnlTableur.Height:=pnlTableur.Height-DT;
  end;
end;

procedure TCdrSerie.InsertGrdLines(const N: integer);
var
  i,j: integer;
begin
  with grdStations do begin
    AfficherMessage(Format('%s.InsertGrdLine',[ClassName]));
    //if (Row = RowCount-N) then Exit;
    for i:=RowCount-1 downto Row+N do begin
      for j:=1 to ColCount-1 do
        Cells[j,i]:=Cells[j,i-N];
    end;
    for i:=ROW to ROW+N-1 do begin
      for j:=NUM_COL_L to NUM_COL_HN do Cells[j,i]:='';
      Cells[NUM_COL_OBS,i]:=Format('** Inseree (%d) **', [1+i-Row]);
    end;
  end;
end;

procedure TCdrSerie.acUndoCopyExecute(Sender: TObject);
begin
  Undocopy(grdStations.Selection, False);
end;
// recopie vers le bas
procedure TCdrSerie.Undocopy(const MySelection: TGridRect;
                             const AutoIncremented: boolean);
var
  i,j, Nb: integer;
begin
  with grdStations do begin
    Nb:=MySelection.Bottom - MySelection.Top;
    if Nb=0 then
      Nb:=StrToIntDef(InputBox(acUndoCopy.Caption, 'Nombre de lignes',IntToStr(Nb)),1);

    for i:=MySelection.Top+1 to MySelection.Top + Nb do begin
      for j:=MySelection.Left to MySelection.Right do
        Cells[j,i]:=Cells[j, MySelection.Top];

    end;
    // auto incrémentation
    if AutoIncremented then begin
      for i:=MySelection.Top+1 to MySelection.Top + Nb do begin
        Cells[NUM_COL_IDTERRAIN, i]:=IncrementString(Cells[NUM_COL_IDTERRAIN, i-1]);
      end;
    end;
  end; // with grdstations
end;

procedure TCdrSerie.btnGetSerStArriveeByIDClick(Sender: TObject);
var
  S, P: integer;
  IDL, Q : string;
begin
  IDL := '';
  if InputQuery(AnsiToUtf8(rsDLG_FIND_PT_BY_ID_TITLE),
                AnsiToUtf8(rsDLG_FIND_PT_BY_ID_PROMPT), IDL) then
  begin
    if FCurrentDocTopo.FindPtTopo(IDL, S, P) then
    begin
      editSerieArrivee.AsInteger  := S;
      editPointArrivee.AsInteger  := P;
      //lbIDLStArr.Caption   := IDL;

      IDL := '   '+ IDL;
      S:=Pos('|',IDL);
      Q := Trim(Copy(IDL, 1, S-1));
      lbIDLStArr.Caption   := Q;


    end else
      ShowMessage(AnsiToUtf8(rsMATCHNOTFOUND));
  end;
end;

procedure TCdrSerie.btnSelectReseauClick(Sender: TObject);
begin
  editNumeroReseau.AsInteger := SelectionDansListe(FCurrentDocTopo, mslRESEAUX, editNumeroReseau.AsInteger);
end;



// calculer et attribuer extrémité libre
// DONE: Ajout de la variable wu pour éviter réutilisation de var de boucle en dehors de celle-ci
procedure TCdrSerie.AttribuerExtremiteLibre(const S,P: integer);
var
  b: boolean;
  i: integer;
  wu: integer;
begin
  if chkLocked.Checked then Exit;
  //if (S>0) or (P>0) then Exit;
  wu := 0;
  with grdStations do begin
    for i:= 0 to RowCount-1 do begin
	  wu := i;
      b:=(i>0) and
         (Cells[NUM_COL_L,i]='') and
         (Cells[NUM_COL_A,i]='') and
         (Cells[NUM_COL_P,i]='');
      if b then Break;
    end; // for
    editPointArrivee.AsInteger  := wu-1;
    editSerieArrivee.AsInteger  := editNumeroSerie.AsInteger;
    //
  end;
end;

// Sauvegarde modifs formulaire et tableau
function TCdrSerie.ImplementerModifs: boolean;
var
  i, Q: integer;
  S: TUneSerie;
  b: boolean;
  V: TUneVisee;
  EWE: Extended;
  // vérification des stations
  function CheckAStation(const AStation: TUneVisee): integer;
  var
    AMin, AMax: Double;
    CC: TCode;
    EE: TExpe;

  begin
    Result:=0;
    // code et expé
    if Not IsInRange(AStation.Code, 0, FCurrentDocTopo.NbCodes)  then
       ShowMessageFmt('Code invalide en station %d',[i]);
    if Not IsInRange(AStation.Expe, 0, FCurrentDocTopo.NbExpes) then
       ShowMessageFmt('Séance invalide en station %d',[i]);
    // longueur, ne doit pas dépasser 160 m
    if Not IsInRange(AStation.Longueur,0.00, 160.00) then
       ShowMessageFmt('Longueur hors limites en station %d',[i]);
    CC:=FCurrentDocTopo.GetCode(AStation.Code);
    // azimuts
    AMin:=0.00;
    case Trunc(CC.GradAz) of
      359,360: AMax:=360.00; // directes degrés
      399,400: AMax:=400.00; // directes grades
      349,350: AMax:=360.00; // inverses degrés
      389,390: AMax:=400.00; // inverses grades
    end;
    if Not IsInRange(AStation.Azimut, AMin, AMax) then
       ShowMessageFmt('Azimut incorrect en station %d',[i]);
    // pentes
    case Trunc(CC.GradInc) of
      360: begin AMin:=-90.00 ; AMax:=  90.00; end; // zéro horizontal degré, directe
      400: begin AMin:=-100.00; AMax:= 100.00; end; // zéro horizontal grade, directe
      361: begin AMin:=  0.00;  AMax:= 180.00; end; // zéro zénithal degrés
      401: begin AMin:=  0.00;  AMax:= 200.00; end; // zéro zénithal grades
      359: begin AMin:=  0.00;  AMax:= 180.00; end; // zéro nadiral degrés
      399: begin AMin:=  0.00;  AMax:= 200.00; end; // zéro nadiral grades
      370: begin AMin:=-580.00; AMax:= 580.00; end; // pourcentage; +/- 80° max
      380: begin AMin:=-160.00; AMax:= 160.00; end; // dénivelés; maxi=long max visée
      350: begin AMin:=-90.00 ; AMax:=  90.00; end; // zéro horizontal degré, inverse
      390: begin AMin:=100.00 ; AMax:= 100.00; end; // zéro horizontal grade, inverse
    else
      begin AMin:=-100.00; AMax:= 100.00; end;
    end;
    if Not IsInRange(AStation.Pente, AMin, AMax) then
      ShowMessageFmt('Pente incorrecte en station %d',[i]);
    // largeurs et hauteurs
    if (Not IsInRange(AStation.LG, 0.00, 200.00)) or
       (Not IsInRange(AStation.LD, 0.00, 200.00)) or
       (Not IsInRange(AStation.HZ, 0.00, 400.00)) or
       (Not IsInRange(AStation.HN, 0.00, 400.00)) then
      ShowMessageFmt('Largeur ou hauteur incorrecte en station %d',[i]);
  end;
begin
  {$DEFINE ACTIVETRY}
  {$UNDEF ACTIVETRY}
  AfficherMessage(Format('%s.ImplementerModifs',[ClassName]));
  result:=False;
  // calcul éventuel estrémités de série
  AttribuerExtremiteLibre(editSerieArrivee.AsInteger, editPointArrivee.AsInteger);
  {$IFDEF ACTIVETRY}
  try
  {$ENDIF}
    //FSerie:=GetSerieFromForm;
    //sauvegarde du formulaire:
    with FCurrentSerie do begin
      IndexSerie   := editNumeroSerie.AsInteger;
      NomSerie     := editNomSerie.Text;
      SerieDep     := editSerieDepart.AsInteger;
      SerieArr     := editSerieArrivee.AsInteger;
      PtDep        := editPointDepart.AsInteger;
      PtArr        := editPointArrivee.AsInteger;
      Reseau       := editNumeroReseau.AsInteger;
      Chances      := cmbChance.ItemIndex;
      Obstacles    := cmbObstacle.ItemIndex;
      Commentaires := editCommentaire.Text;
      Raideur      := editRaideur.Value;
      // Purger la liste des points topos
      for i:=0 to PointsTopo.Count-1 do
        Dispose(PointsTopo.Items[i]);
      PointsTopo.Clear;
    end;
    // analyser le tableur:
    // critère de sortie: Colonne 1 à 5 vides ou nulles

    with grdStations do begin
      AfficherMessage('  Checking values');
      // si les colonnes Long, Az et P sont vides,
      // la fin de la table des stations est supposée atteinte
      for i:=0 to RowCount-1 do begin
        (*
        b:=(i>0) and
            (Cells[NUM_COL_L,i]='') and
            (Cells[NUM_COL_A,i]='') and
            (Cells[NUM_COL_P,i]='');
        //*)
        EWE := StrToFloatDef(Cells[NUM_COL_L,i], 0.00);
        //EWE *= StrToFloatDef(Cells[NUM_COL_A,i], 0.00);
        //EWE *= StrToFloatDef(Cells[NUM_COL_P,i], 0.00);



        if (EWE > 0) then Break;
        // analyse ligne
        V.NoVisee:=i;
        V.IDTerrainStation:=UpperCase(Trim(Cells[NUM_COL_IDTERRAIN,i]));

        V.TypeGalerie:=SetTypeGalerie(StrToIntDef(Cells[NUM_COL_TYPEGAL,i],-1));
        V.Code:=StrToIntDef(Cells[NUM_COL_CODE,i],0);
        V.Expe:=StrToIntDef(Cells[NUM_COL_EXPE,i],0);
        V.Longueur:=StrToFloatDef(Cells[NUM_COL_L,i],0);
        V.Azimut  :=StrToFloatDef(Cells[NUM_COL_A,i],0);
        V.Pente   :=StrToFloatDef(Cells[NUM_COL_P,i],0);
        V.LG:=StrToFloatDef(Cells[NUM_COL_LG,i],0);
        V.LD:=StrToFloatDef(Cells[NUM_COL_LD,i],0);
        V.HZ:=StrToFloatDef(Cells[NUM_COL_HZ,i],0);
        V.HN:=StrToFloatDef(Cells[NUM_COL_HN,i],0);
        V.Commentaires:=Cells[NUM_COL_OBS,i];



        AfficherMessage(Format('   St: %d,  %d  %d - %.2f %.2f %.2f - %.2f %.2f %.2f %.2f  "%s"',
                               [V.NoVisee,V.Code,V.Expe,
                                V.Longueur,V.Azimut,V.Pente,
                                V.LG,V.LD,V.HZ,V.HN,
                                V.Commentaires]));
        FCurrentDocTopo.AddStation(FNumeroInterneSerie, V);
      end;
    end;
    // nombre de stations
    FCurrentSerie.NbPoints:=FCurrentSerie.PointsTopo.Count;
    FCurrentDocTopo.PutSerie(FNumeroInterneSerie, FCurrentSerie);
    // Tout est OK: On met Result à TRUE;
    AfficherMessage(Format('%s.ImplementerModifs OK',[ClassName]));
    Result:=True;
  {$IFDEF ACTIVETRY}
  except
    AfficherMessage(Format('%s.ImplementerModifs KO',[ClassName]));
  end;
  {$ENDIF}
end;

end.

