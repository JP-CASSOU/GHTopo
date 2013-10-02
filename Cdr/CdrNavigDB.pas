unit CdrNavigDB;

{$mode delphi}{$H+}

interface

uses
  Classes, Dialogs, Controls, CallDialogs,
  SysUtils, FileUtil, LResources, Forms, StdCtrls, ActnList;

  // procédures de navigation et de modification de l'enregistrement courant
  type TProcSort         = procedure of object;
  type TProcGoto         = procedure (const Idx: integer) of object;
  type TProcApplyChanges = procedure (const Idx: integer) of object;
  type TProcAddNewItem   = function: boolean of object;
  type TProcSearch       = function(const Idx: integer): integer of object;
  type TProcRemove       = procedure (const Idx: integer) of object;
  type TProcSelectInListe = function(const Idx: integer): integer of Object;

type

  { TCadreNavigDB }

  TCadreNavigDB = class(TFrame)
    acFirst: TAction;
    ac10Prev: TAction;
    acPrev: TAction;
    acNext: TAction;
    ac10Next: TAction;
    acLast: TAction;
    acNewItem: TAction;
    acValidate: TAction;
    ActionList1: TActionList;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    chkConfirm: TCheckBox;
    lbTotal: TStaticText;
    //************************
    procedure ac10NextExecute(Sender: TObject);
    procedure ac10PrevExecute(Sender: TObject);
    procedure acFirstExecute(Sender: TObject);
    procedure acLastExecute(Sender: TObject);
    procedure acNewItemExecute(Sender: TObject);
    procedure acNextExecute(Sender: TObject);
    procedure acPrevExecute(Sender: TObject);
    procedure acValidateExecute(Sender: TObject);

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

    function  MoveConfirmated: byte;


  public
    { public declarations }
    procedure SetBounds(const QMin, QMax: integer);
    // fonctions de callback
    procedure SetProcGoto(const MyProcGoto: TProcGoto);
    procedure SetProcApplyChanges(const MyApplyChanges: TProcApplyChanges);
    procedure SetProcAddNewItem(const MyProcAddNewItem: TProcAddNewItem);
    procedure SetSafeCurrRecord(const Idx: integer);


  end; 

implementation

{ TCadreNavigDB }
// fonctions de callback
procedure TCadreNavigDB.SetProcGoto(const MyProcGoto: TProcGoto);
begin
  FProcGoto := MyProcGoto;
end;
procedure TCadreNavigDB.SetProcApplyChanges(const MyApplyChanges: TProcApplyChanges);
begin
  FProcApplyChanges := MyApplyChanges;
end;

procedure TCadreNavigDB.SetProcAddNewItem(const MyProcAddNewItem: TProcAddNewItem);
begin
  FProcAddNewItem := MyProcAddNewItem;
end;

// si la ckeckbox est activée, demande confirmation avant de changer d'item
// Résultat:
// 0 = Déplacement interdit
// 1 = Déplacement autorisé avec checkbox désactivée
// 2 = Déplacement autorisé avec checkbox activée avec sauvegarde
// 3 = Déplacement autorisé avec checkbox activée sans sauvegarde
function  TCadreNavigDB.MoveConfirmated: byte;
begin
  Result:=0;
  if chkConfirm.Checked then begin
    case MessageDlg('Sauver modifs', mtConfirmation, [mbYES, mbNO, mbCANCEL],0) of
      mrYES    : Result:= 2;
      mrNO     : Result:= 3;
      mrCANCEL : Result:= 0;
    end;
  end else begin
    Result:=1;
  end;
end;
procedure TCadreNavigDB.SetBounds(const QMin, QMax: integer);
begin
  FMin := QMin;
  FMax := QMax;
end;

procedure TCadreNavigDB.SetSafeCurrRecord(const Idx: integer);
var
  q: integer;
begin
  q:=Idx;
  if q<FMin then q:=FMin;
  if q>FMax then q:=FMax;
  FProcGoto(q);
  FPosition:=q;

  lbTotal.Caption:=Format('%d',[FMax]);
end;

//=========================
procedure TCadreNavigDB.acFirstExecute(Sender: TObject);
begin
  case MoveConfirmated of
    0   :;
    1, 3: SetSafeCurrRecord(0);
    2   : begin
            FProcApplyChanges(FPosition);
            SetSafeCurrRecord(0);
          end;
  end;
end;

procedure TCadreNavigDB.ac10PrevExecute(Sender: TObject);
begin
  case MoveConfirmated of
    0   :;
    1, 3: SetSafeCurrRecord(FPosition - 10);
    2   : begin
            FProcApplyChanges(FPosition);
            SetSafeCurrRecord(FPosition - 10);
          end;
  end;
end;

procedure TCadreNavigDB.acPrevExecute(Sender: TObject);
begin
  case MoveConfirmated of
    0   :;
    1, 3: SetSafeCurrRecord(FPosition - 1);
    2   : begin
            FProcApplyChanges(FPosition);
            SetSafeCurrRecord(FPosition - 1);
          end;
  end;
end;

procedure TCadreNavigDB.acValidateExecute(Sender: TObject);
begin
  try
    FProcApplyChanges(FPosition);
  except
    ShowMessage('[programmeur] Attendu: Fonction de callback');
  end;

end;



procedure TCadreNavigDB.acNextExecute(Sender: TObject);
begin
  case MoveConfirmated of
    0   :;
    1, 3: SetSafeCurrRecord(FPosition + 1);
    2   : begin
            FProcApplyChanges(FPosition);
            SetSafeCurrRecord(FPosition + 1);
          end;
  end;
end;

procedure TCadreNavigDB.ac10NextExecute(Sender: TObject);
begin
  case MoveConfirmated of
    0   :;
    1, 3: SetSafeCurrRecord(FPosition + 10);
    2   : begin
            FProcApplyChanges(FPosition);
            SetSafeCurrRecord(FPosition + 10);
          end;
  end;
end;

procedure TCadreNavigDB.acLastExecute(Sender: TObject);
begin
  case MoveConfirmated of
    0   :;
    1, 3: SetSafeCurrRecord(FMax);
    2   : begin
            FProcApplyChanges(FPosition);
            SetSafeCurrRecord(FMax);
          end;
  end;
end;

procedure TCadreNavigDB.acNewItemExecute(Sender: TObject);
begin
  if Assigned(FProcAddNewItem) then begin
    if QuestionOuiNon('Ajouter item') then
      if FProcAddNewItem then begin
        SetSafeCurrRecord(FMax);
      end;
  end;
end;

{ TCadreNavigDB }

initialization
  {$I CdrNavigDB.lrs}

end.

