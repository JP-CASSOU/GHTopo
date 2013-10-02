unit dial_import;

interface

uses SysUtils, Classes, Graphics, Forms,
  Buttons, ExtCtrls, Controls, StdCtrls, CheckLst;

type
  TDlgImportation = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    Bevel1: TBevel;
    CheckTout: TCheckBox;
    CheckListBoxImport: TCheckListBox;
    ButtonTout: TButton;
    ButtonAucun: TButton;
    procedure CheckToutClick(Sender: TObject);
    procedure ButtonToutClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure Prepare(Nom_Fichier : string);
  end;

var
  DlgImportation: TDlgImportation;

implementation

{$R *.dfm}

procedure TDlgImportation.CheckToutClick(Sender: TObject);
begin
  CheckListBoxImport.Enabled:=not CheckTout.Checked;
  ButtonTout.Enabled:=not CheckTout.Checked;
  ButtonAucun.Enabled:=not CheckTout.Checked;
end;

procedure TDlgImportation.ButtonToutClick(Sender: TObject);
var
  I : integer;
begin
  for I:=0 to CheckListBoxImport.Items.Count-1 do
    CheckListBoxImport.Checked[I]:=(Sender=ButtonTout);
end;

procedure TDlgImportation.Prepare(Nom_Fichier : string);
var
  F : textFile;
  S : string;
begin
  CheckTout.Checked:=true;
  CheckToutClick(self);
  assignFile(F,Nom_Fichier);
  CheckListBoxImport.Items.Clear;
  reset(F);
  try
    while not Eof(F) do
    begin
      readln(F,S);
      if (S<>'') and (S[1]<>'''') then
        CheckListBoxImport.Items.Add(S);
    end;
  finally
    ButtonToutClick(ButtonTout);
    CloseFile(F);
  end;  
end;

end.
