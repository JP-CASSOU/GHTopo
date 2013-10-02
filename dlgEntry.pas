unit dlgEntry;

{$mode delphi}{$H+}

interface

uses
  Math,
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls;

type

  { TDialogEntry }

  TDialogEntry = class(TForm)
    editValeur: TEdit;
    lbCaption: TLabel;
    lbStdKeybd: TStaticText;
    lbBounds: TStaticText;

    procedure FormDestroy(Sender: TObject);

  private
    { private declarations }
    FModeInput: integer;
    FIntMini, FIntMaxi: integer;
    FRealMini, FRealMaxi: double;
    FCaption: string;
    FValue  : string;

    FButtons: array of TButton;
    UnBouton: TButton;
    procedure ProcAddCharacter(Sender: TObject);
    procedure ProcRemoveEndCharacter(Sender: TObject);
    procedure ProcClearEdit(Sender: TObject);
    function  ProcValidateEdit(Sender: TObject): boolean;

    function  CreateButton(const X, Y, L, H: integer;
                                       const MyCaption: string;
                                       const MyModalResult: TModalResult;
                                       ProcButtonClick: TNotifyEvent): TButton;
    procedure SetCursorAtEndText;
  public
    { public declarations }
    procedure QSetBounds(const Min, Max: string);
    procedure QSetCaption(const S: string);
    procedure QSetValue(const V: string);
    function  QGetValue: string;
    procedure QSetModeInput(const m:integer);

    procedure InitClavier(const Mode: integer);



  end; 

var
  DialogEntry: TDialogEntry;

implementation
procedure TDialogEntry.QSetBounds(const Min, Max: string);
begin
  case FModeInput of
    0: Exit;
    1: begin
         FIntMini := StrToIntDef(Min, 0);
         FIntMaxi := StrToIntDef(Max, 0);
         lbBounds.Caption:=Format('%d .. %d', [FIntMini, FIntMaxi]);

       end;
    2: begin
         FRealMini:= StrToFloatDef(Min, 0.00);
         FRealMaxi:= StrToFloatDef(Max, 0.00);
         lbBounds.Caption:=Format('%.2f .. %.2f', [FRealMini, FRealMaxi]);
       end;
  end;

end;

procedure TDialogEntry.QSetModeInput(const m:integer);
begin
  FModeInput:=m;
end;
procedure TDialogEntry.SetCursorAtEndText;
begin
  editValeur.SelStart  := Length(editValeur.Text);
  editValeur.SelLength := 0;
end;

function TDialogEntry.ProcValidateEdit(Sender: TObject): boolean;
begin
  Result := False;
  case FModeInput of
     0: Exit;
     1: begin

        end;
     2: begin

        end;

   end;
  //ShowMessage(inttostr(ord(ModalResult)));
  //self.Close;
end;

procedure TDialogEntry.ProcClearEdit(Sender: TObject);
begin
  editValeur.Text := '';
end;

procedure TDialogEntry.ProcAddCharacter(Sender: TObject);
var
  WU: string;
  Ch: string;
begin
   WU := Trim(editValeur.Text);
   Ch := Trim(TButton(Sender).Caption);
   // signe moins en tête de chaine uniquement
   if (Ch = '-') AND (Length(WU) > 0) then Exit;

   // un seul point toléré
   if ((Ch = '.') AND (Pos('.', WU) > 0)) then Exit;

   editValeur.Text := WU + Ch;


   //editValeur.SetFocus;
   SetCursorAtEndText; // met le curseur en fin de texte
end;
procedure TDialogEntry.ProcRemoveEndCharacter(Sender: TObject);
var
  WU: string;
begin
   //ShowMessageFmt('WU %s', [TButton(Sender).Caption]);
   WU := Trim(editValeur.Text);
   if (Length(WU) > 0) then System.Delete(WU, Length(WU), 1);

   editValeur.Text := Trim(WU);
   editValeur.SelStart := Length(editValeur.Text);
   //editValeur.SetFocus;
end;

procedure TDialogEntry.QSetCaption(const S: string);

begin
  lbCaption.Caption := S;

end;

procedure TDialogEntry.QSetValue(const V: string);
var
  WU: double;
begin
  editValeur.Text := V;
   // si un zéro est passé, on efface le texte
  if FModeInput > 0 then begin
    WU := StrToFloatDef(editValeur.Text, 0.00);
    if IsZero(WU) then editValeur.Text := '';;
  end;
end;
function TDialogEntry.QGetValue: string;
begin
  Result := Trim(editValeur.Text);
end;


procedure TDialogEntry.InitClavier(const Mode: integer);
var
  i, j: integer;
  W, H: integer;
  L   : integer;
  X, Y: integer;
begin
  if FModeInput = 0 then Exit;
  lbStdKeybd.Visible:=False;

  SetLength(FButtons, 15);
  X := 1;
  H := 42;
  Y := editValeur.Top + editValeur.Height + 4;
  L := (self.Width div 3);
  W := L - 4;
  for i := 1 to 9 do begin
    FButtons[i] := CreateButton(X, Y, W, H, chr(48+i), mrNone, ProcAddCharacter);
    X := X + L;
    if (i MOD 3) = 0 then begin
      Y := Y + H + 4;
      X := 1;
    end;
  end;
  FButtons[0]  := CreateButton(X, Y, W, H, '-', mrNone, ProcAddCharacter);
  X := X + L;
  FButtons[10] := CreateButton(X, Y, W, H, '0', mrNone, ProcAddCharacter);
  X := X + L;
  FButtons[11] := CreateButton(X, Y, W, H,  defaultFormatSettings.DecimalSeparator, mrNone, ProcAddCharacter);
  X := 1;  Y := Y + H + 4;
  FButtons[12] := CreateButton(X, Y, W, H, '<==', mrNone, ProcRemoveEndCharacter);
  X := X + L;
  FButtons[13] := CreateButton(X, Y, W, H, 'CLR', mrNone, ProcClearEdit);
  X := X + L;
  FButtons[14] := CreateButton(X, Y, W, H, 'Enter', mrOK, nil);
  // curseur en fin de texte
  SetCursorAtEndText;


end;

procedure TDialogEntry.FormDestroy(Sender: TObject);
var i: integer;
begin
  try
    if (SizeOf(FButtons) > 0) then begin

      for i:= 0 to High(FButtons) do begin
         //ShowMessage(Inttostr(i));
         FButtons[i].Free;

      end;
      SetLength(FButtons, 0);
    end;
  finally
  end;
  Inherited;
end;

function TDialogEntry.CreateButton(const X, Y, L, H: integer;
                                   const MyCaption: string;
                                   const MyModalResult: TModalResult;
                                   ProcButtonClick: TNotifyEvent): TButton;
begin
  Result := TButton.Create(self);
  Result.Parent := self; // indispensable: c'est ceci qui affiche le contrôle
  Result.Width  := L;
  Result.Height := H;
  Result.Left   := X;
  Result.Top     := Y;
  Result.Caption := MyCaption;
  Result.Visible := True;
  Result.ModalResult:=MyModalResult;
  Result.OnClick := ProcButtonClick;

end;

initialization
  {$I dlgEntry.lrs}

end.

