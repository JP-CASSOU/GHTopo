unit CadreListeNavigateurDB;
// Version 'liste' du navigateur DB
// Date: 03/05/2013
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
  Graphics,
  UnitClassPalette,
  StructuresDonnees,
  ToporobotClasses2012,
  ObjetSerie,
  CallDialogs, Dialogs, Classes, SysUtils, FileUtil, Forms, Controls, ActnList,
  Buttons, StdCtrls, ButtonPanel, ComCtrls, curredit, types, LCLType;

// procédures de navigation et de modification de l'enregistrement courant
type TProcSort          = procedure of object;
type TProcGoto          = procedure (const Idx: integer) of object;
type TProcApplyChanges  = procedure (const Idx: integer) of object;
type TProcAddNewItem    = function: boolean of object;
type TProcSearch        = function(const Idx: integer): integer of object;
type TProcRemove        = procedure (const Idx: integer) of object;
type TProcSelectInListe = function(const Idx: integer): integer of Object;


type

  { TCdrListeNavigateurDB }

  TCdrListeNavigateurDB = class(TFrame)
    acAddItem: TAction;
    acApplyModifs: TAction;
    acDeleteItem: TAction;
    acFind: TAction;
    acSort: TAction;
    ActionList1: TActionList;
    chkConfirmMove: TCheckBox;
    hcColsTitres: THeaderControl;
    ImageList1: TImageList;
    lbNbItems: TLabel;
    lsbItems: TListBox;
    SpeedButton10: TSpeedButton;
    SpeedButton12: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SpeedButton8: TSpeedButton;
    SpeedButton9: TSpeedButton;
    procedure acAddItemExecute(Sender: TObject);
    procedure acApplyModifsExecute(Sender: TObject);
    procedure acDeleteItemExecute(Sender: TObject);
    procedure acSelectInListeExecute(Sender: TObject);
    procedure acSortExecute(Sender: TObject);
    procedure lsbItemsClick(Sender: TObject);
  private
    { private declarations }
    FDocuTopo: TToporobotStructure2012;
    FModeBDD : TModeBDD;
    FMax: integer;
    FPosition: integer;
    FMyPalette: TPalette256;


    FProcApplyChanges  : TProcApplyChanges;
    FProcAddNewItem    : TProcAddNewItem;
    FProcRemove        : TProcRemove;

    FProcSelectInListe : TProcSelectInListe;
    FProcSearch        : TProcSearch;
    FProcSort          : TProcSort;
    FProcGoto          : TProcGoto;

  public
    { public declarations }
    procedure Initialiser(const FDocToporobot: TToporobotStructure2012; const QMode: TModeBDD; const QPosition: integer);
    procedure SetProcAddNewItem(const Proc: TProcAddNewItem);
    procedure SetProcApplyChanges(const Proc: TProcApplyChanges);
    procedure SetProcGoto(const Proc: TProcGoto);
    procedure SetProcRemove(const Proc: TProcRemove);
    procedure SetProcSearch(const Proc: TProcSearch);
    procedure SetProcSelectInListe(const Proc: TProcSelectInListe);
    procedure SetProcSort(const Proc: TProcSort);

    procedure SafeAddRecord;
    procedure SetSafeCurrRecord(const Idx: integer);
  end;

implementation

{$R *.lfm}
const
  MSG_CALLBACK_ATTENDU = '[programmer] Expected: Callback function';

{ TCdrListeNavigateurDB }

procedure TCdrListeNavigateurDB.lsbItemsClick(Sender: TObject);
begin
  SetSafeCurrRecord(lsbItems.ItemIndex);
end;


procedure TCdrListeNavigateurDB.acAddItemExecute(Sender: TObject);
begin
  if MessageDlg(AnsiToUtf8(rsCDR_NAVIG_DB_DO_ADD), mtConfirmation,[mbYES, mbNO],0)=mrYES then
    SafeAddRecord;
end;

procedure TCdrListeNavigateurDB.acApplyModifsExecute(Sender: TObject);
begin
  try
    FProcApplyChanges(FPosition);
    Initialiser(FDocuTopo, FModeBDD, FPosition);
  except
    ShowMessage(MSG_CALLBACK_ATTENDU);
  end;
end;

procedure TCdrListeNavigateurDB.acDeleteItemExecute(Sender: TObject);
begin
  if Not Assigned(FProcRemove) then begin
      ShowMessage(MSG_CALLBACK_ATTENDU);
      Exit;
    end;
  if QuestionOuiNon(AnsiToUtf8(rsCDR_NAVIG_DB_DO_DELETE)) then
  begin
    FProcRemove(FPosition);
    Initialiser(FDocuTopo, FModeBDD, FPosition);
  end;
end;

procedure TCdrListeNavigateurDB.acSelectInListeExecute(Sender: TObject);
var
  miou: integer;
begin
  try
    // TODO: Utiliser les nouvelles versions des sélecteurs de listes
    // (basés sur les numéros réels d'items et non les index internes)
    miou := FProcSelectInListe(FPosition);
    if (miou>0) then SetSafeCurrRecord(miou);
  except
    ShowMessage(MSG_CALLBACK_ATTENDU);
  end;
end;

procedure TCdrListeNavigateurDB.acSortExecute(Sender: TObject);
begin
  if Not Assigned(FProcSort) then begin
      ShowMessage(MSG_CALLBACK_ATTENDU);
      Exit;
    end;
  if QuestionOuiNon(AnsiToUtf8(rsCDR_NAVIG_DB_DO_SORT)) then
  begin
    FProcSort;
    Initialiser(FDocuTopo, FModeBDD, 1);
  end;
end;


procedure TCdrListeNavigateurDB.Initialiser(const FDocToporobot: TToporobotStructure2012; const QMode: TModeBDD; const QPosition: integer);
var
  i: Integer;
  S: TObjSerie;
  EWE: Integer;
  QNbItems: Integer;
  E: TExpe;
  C: TCode;
  procedure AjouterTitreColonne(const Titre: string; const LG: integer);
  var
    ht: THeaderSection;
  begin
    ht := hcColsTitres.Sections.Add;
    ht.Text := Titre;
    ht.MinWidth := LG;
  end;
begin
  AfficherMessage('--- Initialiser');
  FModeBDD  := QMode;
  FDocuTopo := FDocToporobot;
  lsbItems.Clear;
  // palette
  try
    FMyPalette.Free; // libération sécurisée
  except
    ;
  end;
  FMyPalette := TPalette256.Create;
  FMyPalette.GenerateTOPOROBOTPalette;
  // header
  case QMode of
    mbddSERIES:
    begin
      QNbItems := FDocToporobot.GetNbSeries;
      hcColsTitres.Sections.Clear;
      with hcColsTitres do
      begin
        AjouterTitreColonne('ID', 40);
        AjouterTitreColonne('Départ', 70);
        AjouterTitreColonne('Arrivée', 70);
        AjouterTitreColonne('Nom', 400);
        AjouterTitreColonne('Réseau', 300);
        AjouterTitreColonne('Nb points', 80);
      end;
    end;

    mbddEXPES:
      begin
        QNbItems := FDocToporobot.GetNbExpes;

        with hcColsTitres do begin
          AjouterTitreColonne('ID', 40);
          AjouterTitreColonne('Couleur', 80);
          AjouterTitreColonne('Date', 90);
          AjouterTitreColonne('Spéléomètre', 140);
          AjouterTitreColonne('Spéléographe', 140);
          AjouterTitreColonne('Déclinaison', 70);
          AjouterTitreColonne('Commentaires', 500);
        end;
      end;
  end;
  case QMode of
    mbddSERIES:
      begin
        FMax := FDocuTopo.GetNbSeries - 1;
        for i := 0 to FMax do
        begin
          S := FDocuTopo.GetSerie(i);
          lsbItems.Items.add(Format('%d : %s', [S.GetIndexSerie ,AnsiToUtf8(S.GetNomSerie)]));
        end;
      end;
    mbddEXPES:
      begin
        FMax := FDocuTopo.GetNbExpes - 1;
        for i := 0 to FMax do
        begin
          E := FDocuTopo.GetExpe(i);
          lsbItems.Items.add(Format('%d : %.2d/%.2d/%.4d : %s', [E.IDExpe , E.JourExpe, E.MoisExpe, E.AnneeExpe, E.Commentaire]));
        end;
      end;
    mbddCODES:
      begin
        FMax := FDocuTopo.GetNbCodes - 1;
        for i := 0 to FMax do
        begin
          C := FDocuTopo.GetCode(i);
          lsbItems.Items.add(Format('%d : %s', [C.IDCode , C.Commentaire]));
        end;
      end;
  end;
  if (QPosition = -1) then EWE := FMax else EWE := QPosition;
  if (EWE > FMax) then EWE := FMax;

  SetSafeCurrRecord(EWE);
  lsbItems.ItemIndex := EWE;
end;

// assignation des procédures de callback
procedure TCdrListeNavigateurDB.SetProcSort(const Proc: TProcSort);
begin
  FProcSort := Proc;
end;



procedure TCdrListeNavigateurDB.SetProcGoto(const Proc: TProcGoto);
begin
  FProcGoto := Proc;
end;
procedure TCdrListeNavigateurDB.SetProcApplyChanges(const Proc: TProcApplyChanges);
begin
  FProcApplyChanges := Proc;
end;
procedure TCdrListeNavigateurDB.SetProcAddNewItem(const Proc: TProcAddNewItem);
begin
  FProcAddNewItem := Proc;
end;
procedure TCdrListeNavigateurDB.SetProcSearch(const Proc: TProcSearch);
begin
  FProcSearch := Proc;
end;
procedure TCdrListeNavigateurDB.SetProcRemove(const Proc: TProcRemove);
begin
  FProcRemove := Proc;
end;
procedure TCdrListeNavigateurDB.SetProcSelectInListe(const Proc: TProcSelectInListe);
begin
  FProcSelectInListe := Proc;
end;

procedure TCdrListeNavigateurDB.SetSafeCurrRecord(const Idx: integer);
begin
  try
    //ShowMessage(inttostr(idx));
    FProcGoto(Idx);
    FPosition := Idx;
  except
    ; //Message(MSG_CALLBACK_ATTENDU);
  end;
end;
procedure TCdrListeNavigateurDB.SafeAddRecord;
begin
  try
    if (FProcAddNewItem) then
    begin
      Initialiser(FDocuTopo, FModeBDD, -1);
    end;
  except
    ; //ShowMessage(MSG_CALLBACK_ATTENDU);
  end;
end;

end.


//******************************************************************************

// dessin des items
procedure TCdrListeNavigateurDB.lsbItemsDrawItem(Control: TWinControl;
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
    with lsbItems do begin
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
    AfficherMessage(lsbItems.Items[Index]);
    case FModeBDD of
      mbddENTRANCES:
        begin
          with lsbItems do begin
            Canvas.FillRect(ARect);
            Canvas.Brush.Color:=bg;
            Canvas.Font.Color :=tc;
            Canvas.Pen.Color  :=clSilver; // pour les filets
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
            with lsbItems do
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
          begin
            with lsbItems do begin
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
          begin
            try
              miou := DateToStr(SafeEncodeDate(ss.AnneeExpe,ss.MoisExpe,ss.JourExpe));
            except
              miou := '01/01/2000';
            end;
            with lsbItems do
            begin
              Canvas.FillRect(ARect);
              canvas.Pen.Color  :=clSilver;
              Canvas.Font.Color :=tc;

              HS := hcColsTitres.Sections.Items[0];  // ID expé
              canvas.TextOut(HS.Left + 4, ARect.Top+1,  Format('%d',[ss.IDExpe]));
              HS := hcColsTitres.Sections.Items[1];  // Couleur expé
              DessineFiletColonne(HS.Left - Q4);
              // couleur de l'expé
              canvas.Pen.Color:=clBlack;
              canvas.Brush.Color:= FMyPalette.GetColorByIndex(ss.Couleur);
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
            with lsbItems do
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
              rs := FDocuTopo.GetReseau(sr.GetNoReseau);

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
      AfficherMessage('-------------');
    end;

begin
  case FModeBDD of
    mbddENTRANCES: es := FDocuTopo.GetEntree(Index);
    mbddRESEAUX  : rs := FDocuTopo.GetReseau(Index);
    mbddCODES    : cs := FDocuTopo.GetCode(Index);
    mbddEXPES    : ss := FDocuTopo.getExpe(Index);
    mbddSERIES   : sr := FDocuTopo.GetSerie(Index);
  end; //case FModeSelection of
  with lsbItems do begin
    canvas.brush.color:=clwhite;
    //canvas.fillrect(rect);
    //on affiche le texte
    DessineItem(clwhite, clBlack);
    //affichage lorsque la ligne est sélectionnée
    if (odSelected in state) then begin
      canvas.brush.color:=clBlue;
      DessineItem(clBlue, clWhite);
    end;
  end;
end;

end.

