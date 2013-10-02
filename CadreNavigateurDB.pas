unit CadreNavigateurDB;
// Ce composant 'façade de magnétoscope' permet de gérer une table
// Date: 14/02/2013
// Statut: Fonctionnel.
// DONE: Support i18n
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
  CallDialogs, Dialogs, Classes, SysUtils, FileUtil, Forms, Controls, ActnList,
  Buttons, StdCtrls, curredit;

// procédures de navigation et de modification de l'enregistrement courant
type TProcSort          = procedure of object;
type TProcGoto          = procedure (const Idx: integer) of object;
type TProcApplyChanges  = procedure (const Idx: integer) of object;
type TProcAddNewItem    = function: boolean of object;
type TProcSearch        = function(const Idx: integer): integer of object;
type TProcRemove        = procedure (const Idx: integer) of object;
type TProcSelectInListe = function(const Idx: integer): integer of Object;


type

  { TCdrNavigateurDB }

  TCdrNavigateurDB = class(TFrame)
    acNext: TAction;
    acFirst: TAction;
    acPrevious10: TAction;
    acPrevious: TAction;
    acNext10: TAction;
    acLast: TAction;
    acAddItem: TAction;
    acDeleteItem: TAction;
    acSelectInListe: TAction;
    acFind: TAction;
    acSort: TAction;
    acApplyModifs: TAction;
    ActionList1: TActionList;
    chkConfirmMove: TCheckBox;
    editPositionRecord: TCurrencyEdit;
    ImageList1: TImageList;
    SpeedButton1: TSpeedButton;
    SpeedButton10: TSpeedButton;
    SpeedButton11: TSpeedButton;
    SpeedButton12: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SpeedButton8: TSpeedButton;
    SpeedButton9: TSpeedButton;
    lbNbreElements: TStaticText;
    procedure acAddItemExecute(Sender: TObject);
    procedure acApplyModifsExecute(Sender: TObject);
    procedure acDeleteItemExecute(Sender: TObject);
    procedure acFindExecute(Sender: TObject);
    procedure acFirstExecute(Sender: TObject);
    procedure acLastExecute(Sender: TObject);
    procedure acNext10Execute(Sender: TObject);
    procedure acNextExecute(Sender: TObject);
    procedure acPrevious10Execute(Sender: TObject);
    procedure acPreviousExecute(Sender: TObject);
    procedure acSelectInListeExecute(Sender: TObject);
    procedure acSortExecute(Sender: TObject);
  private
    { private declarations }
    FMin: integer;
    FMax: integer;
    FPosition: integer;
    FProcSort          : TProcSort;
    FProcGoto          : TProcGoto;
    FProcApplyChanges  : TProcApplyChanges;
    FProcAddNewItem    : TProcAddNewItem;
    FProcSearch        : TProcSearch;
    FProcRemove        : TProcRemove;
    FProcSelectInListe : TProcSelectInListe;

    function FMoveConfirmated: byte;
    procedure GotoFoundRecord;
    procedure SafeAddRecord;



    procedure SetSafeCurrRecord(const Idx: integer);
  public
    { public declarations }
    procedure Initialiser(const FDocToporobot: TToporobotStructure2012; const QMode: TModeBDD; const QStart, QNb, QPosition: integer);

    procedure SetProcAddNewItem(const Proc: TProcAddNewItem);
    procedure SetProcApplyChanges(const Proc: TProcApplyChanges);
    procedure SetProcGoto(const Proc: TProcGoto);
    procedure SetProcRemove(const Proc: TProcRemove);
    procedure SetProcSearch(const Proc: TProcSearch);
    procedure SetProcSelectInListe(const Proc: TProcSelectInListe);
    procedure SetProcSort(const Proc: TProcSort);
  end; 

implementation

{$R *.lfm}
const
  MSG_CALLBACK_ATTENDU = '[programmer] Expected: Callback function';
// assignation des procédures de callback
procedure TCdrNavigateurDB.SetProcSort(const Proc: TProcSort);
begin
  FProcSort := Proc;
end;

procedure TCdrNavigateurDB.SetProcGoto(const Proc: TProcGoto);
begin
  FProcGoto := Proc;
end;
procedure TCdrNavigateurDB.SetProcApplyChanges(const Proc: TProcApplyChanges);
begin
  FProcApplyChanges := Proc;
end;
procedure TCdrNavigateurDB.SetProcAddNewItem(const Proc: TProcAddNewItem);
begin
  FProcAddNewItem := Proc;
end;
procedure TCdrNavigateurDB.SetProcSearch(const Proc: TProcSearch);
begin
  FProcSearch := Proc;
end;
procedure TCdrNavigateurDB.SetProcRemove(const Proc: TProcRemove);
begin
  FProcRemove := Proc;
end;
procedure TCdrNavigateurDB.SetProcSelectInListe(const Proc: TProcSelectInListe);
begin
  FProcSelectInListe := Proc;
end;

// nombre d'éléments et position initiale
// Position initiale = entier positif ou nul; -1 = inchangé
// Cette version du navigateur n'utilise pas FDocToporobot ni QMode
procedure TCdrNavigateurDB.Initialiser(const FDocToporobot: TToporobotStructure2012; const QMode: TModeBDD; const QStart, QNb, QPosition: integer);
begin
  FMin := QStart;
  FMax := QNb - 1 ;
  if (QPosition > 0) then FPosition := QPosition;
  lbNbreElements.Caption:= format('%d',[FMax + 1]);
end;

{ TCdrNavigateurDB }
// si la ckeckbox est activée, demande confirmation avant de changer d'item
// Résultat:
// 0 = Déplacement interdit
// 1 = Déplacement autorisé avec checkbox désactivée
// 2 = Déplacement autorisé avec checkbox activée avec sauvegarde
// 3 = Déplacement autorisé avec checkbox activée sans sauvegarde
function  TCdrNavigateurDB.FMoveConfirmated: byte;
begin
  Result:=0;
  if (chkConfirmMove.Checked) then begin
    case MessageDlg(rsSAVECHANGES, mtConfirmation, [mbYES, mbNO, mbCANCEL],0) of
      mrYES    : Result:= 2;
      mrNO     : Result:= 3;
      mrCANCEL : Result:= 0;
    end;
  end else begin
    Result:=1;
  end;
end;
procedure TCdrNavigateurDB.GotoFoundRecord;
var
  F666: integer;
begin
  try
    if Not Assigned(FProcGoto) then begin
      ShowMessage(MSG_CALLBACK_ATTENDU + ' dans GotoFoundRecord');
      Exit;
    end;

    F666:=StrToIntDef(InputBox('Recherche de série', 'Numéro', '1'), 1);
    F666:= FProcSearch(F666);
    if F666 = -1 then begin
      ShowMessage(rsMATCHNOTFOUND);
      Exit;
    end;
    //ShowMessage(Format('%d: %d', [F666, FMoveConfirmated]));
    case FMoveConfirmated of
       0   :;
       1, 3: SetSafeCurrRecord(F666);
       2   : begin
               FProcApplyChanges(FPosition);
               SetSafeCurrRecord(F666);
             end;
    end;
  except
    //ShowMessage('[programmeur] Attendu: Fonction de callback dans GotoFoundRecord');
  end;

end;

procedure TCdrNavigateurDB.SafeAddRecord;
begin
  try
    if FProcAddNewItem then begin
      FMax:=FMax + 1;
      SetSafeCurrRecord(FMax);
    end;
  except
    ShowMessage(MSG_CALLBACK_ATTENDU);
  end;
end;
procedure TCdrNavigateurDB.SetSafeCurrRecord(const Idx: integer);
var
  q: integer;
begin

  try
    q:=Idx;
    if (q < FMin) then q:=FMin;
    if (q > FMax) then q:=FMax;
    editPositionRecord.AsInteger:=q;
    FProcGoto(q);

    FPosition:=q;
  except
    ShowMessage(MSG_CALLBACK_ATTENDU);
  end;
end;

procedure TCdrNavigateurDB.acSelectInListeExecute(Sender: TObject);
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

procedure TCdrNavigateurDB.acSortExecute(Sender: TObject);
begin
  if Not Assigned(FProcSort) then begin
      ShowMessage(MSG_CALLBACK_ATTENDU);
      Exit;
    end;
  if QuestionOuiNon(AnsiToUtf8(rsCDR_NAVIG_DB_DO_SORT)) then
  begin
    FProcSort;
  end;
end;


procedure TCdrNavigateurDB.acDeleteItemExecute(Sender: TObject);
begin
  if Not Assigned(FProcRemove) then begin
      ShowMessage(MSG_CALLBACK_ATTENDU);
      Exit;
    end;
  if QuestionOuiNon(AnsiToUtf8(rsCDR_NAVIG_DB_DO_DELETE)) then
  begin
    FProcRemove(FPosition);
    SetSafeCurrRecord(FPosition);
  end;
end;

procedure TCdrNavigateurDB.acAddItemExecute(Sender: TObject);
begin
  if MessageDlg(AnsiToUtf8(rsCDR_NAVIG_DB_DO_ADD), mtConfirmation,[mbYES, mbNO],0)=mrYES then
    SafeAddRecord;
end;

procedure TCdrNavigateurDB.acApplyModifsExecute(Sender: TObject);
begin
  try
    FProcApplyChanges(FPosition);
  except
    ShowMessage(MSG_CALLBACK_ATTENDU);
  end;
end;

procedure TCdrNavigateurDB.acFindExecute(Sender: TObject);
begin
  case FMoveConfirmated of
    0   :;
    1, 3: GotoFoundRecord;
    2   : begin
            FProcApplyChanges(FPosition);
            GotoFoundRecord;
          end;
  end;
end;

procedure TCdrNavigateurDB.acFirstExecute(Sender: TObject);
begin
  case FMoveConfirmated of
    0   :;
    1, 3: SetSafeCurrRecord(0);
    2   : begin
            FProcApplyChanges(FPosition);
            SetSafeCurrRecord(0);
          end;
  end;
end;

procedure TCdrNavigateurDB.acLastExecute(Sender: TObject);
begin
  case FMoveConfirmated of
    0   :;
    1, 3: SetSafeCurrRecord(FMax);
    2   : begin
            FProcApplyChanges(FPosition);
            SetSafeCurrRecord(FMax);
          end;
  end;
end;

procedure TCdrNavigateurDB.acNext10Execute(Sender: TObject);
begin
  case FMoveConfirmated of
    0   :;
    1, 3: SetSafeCurrRecord(FPosition + 10);
    2   : begin
            FProcApplyChanges(FPosition);
            SetSafeCurrRecord(FPosition + 10);
          end;
  end;
end;

procedure TCdrNavigateurDB.acNextExecute(Sender: TObject);
begin
  case FMoveConfirmated of
    0   :;
    1, 3: SetSafeCurrRecord(FPosition + 1);
    2   : begin
            FProcApplyChanges(FPosition);
            SetSafeCurrRecord(FPosition + 1);
          end;
  end;

end;

procedure TCdrNavigateurDB.acPrevious10Execute(Sender: TObject);
begin
  case FMoveConfirmated of
    0   :;
    1, 3: SetSafeCurrRecord(FPosition - 10);
    2   : begin
            FProcApplyChanges(FPosition);
            SetSafeCurrRecord(FPosition - 10);
          end;
  end;
end;

procedure TCdrNavigateurDB.acPreviousExecute(Sender: TObject);
begin
  case FMoveConfirmated of
    0   :;
    1, 3: SetSafeCurrRecord(FPosition - 1);
    2   : begin
            FProcApplyChanges(FPosition);
            SetSafeCurrRecord(FPosition - 1);
          end;
  end;
end;

end.

