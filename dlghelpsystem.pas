unit dlgHelpSystem;
// Système d'aide. Fonctionnalités de modification non implémentées (peu utilisées).

{$INCLUDE CompilationParameters.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, UnitHelpSystem;

type

  { TfrmHelpSystem }

  TfrmHelpSystem = class(TForm)
    BitBtn1: TBitBtn;
    cmbTitles: TComboBox;
    memoText: TMemo;
    procedure cmbTitlesChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
    FHelpFile : THelpFileStructure;
    procedure PutSectionInForm(const S: TSection);
    procedure PutSectionInFormByIdx(const Idx: integer);
  public
    { public declarations }
    function Init(const HelpFile: string; const QTopics: string): boolean;
  end;

var
  frmHelpSystem: TfrmHelpSystem;

implementation

{$R *.lfm}

{ TfrmHelpSystem }

procedure TfrmHelpSystem.FormCreate(Sender: TObject);
begin
  FHelpFile := THelpFileStructure.Create;
end;

procedure TfrmHelpSystem.cmbTitlesChange(Sender: TObject);
begin
  PutSectionInFormByIdx(cmbTitles.ItemIndex);
end;

procedure TfrmHelpSystem.FormDestroy(Sender: TObject);
begin
  FHelpFile.Free;
end;

function TfrmHelpSystem.Init(const HelpFile: string; const QTopics: string): boolean;
var
  EWE: Boolean;
  Idx: Integer;
  i: Integer;
  Sect: TSection;
begin
  EWE := False;
  with FHelpFile do begin
    try
      EWE := LoadFromFile(HelpFile);
      if (EWE) then
      begin

        //FCanEditing:=False;
        if (FHelpFile.GetNbreSections > 0) then
        begin
          cmbTitles.Clear;
          for i:=0 to FHelpFile.GetNbreSections - 1 do
          begin
            Sect := FHelpFile.GetSection(i);
            cmbTitles.Items.Add(AnsiToUtf8(Sect.Title));
          end;
          Idx := FHelpFile.SearchIndexSection(QTopics);
          if (Idx < 0) then Idx := 0;
          cmbTitles.ItemIndex := Idx;
          PutSectionInFormByIdx(cmbTitles.ItemIndex);
        end;
      end;
      result := EWE;
    except
    end;
  end;

end;
procedure TfrmHelpSystem.PutSectionInForm(const S: TSection);
var
  PS: integer;
  S1, S2: string;
begin
  //editIndex.Text  :=Format('%d',[S.Index]);
  //editTopics.Text := AnsiToUtf8(S.Topics);
  //editTitre.Text  := AnsiToUtf8(S.Title);
  self.Caption  := Format('Sect. %d: <%s>: %s', [S.Index, AnsiToUtf8(S.Topics), AnsiToUtf8(S.Title)]);
  s1:=S.Texte;
  memoText.Lines.Clear;
  PS:= Pos(#10, s1);
  memoText.Visible:=False;
  while (PS>0) do begin
    s2:=Trim(Copy(s1, 1, PS-1));
    memoText.Lines.Add(AnsiToUtf8(s2));
    s1:=Copy(s1, PS+1, Length(s1)-PS+1);
    PS:=Pos(#10, s1);
  end;
  memoText.Lines.Add(Trim(AnsiToUtf8(s1)));
  memoText.Visible:=True;

end;
procedure TfrmHelpSystem.PutSectionInFormByIdx(const Idx: integer);
var
  S: TSection;
begin
  S:=FHelpFile.GetSection(Idx);
  PutSectionInForm(S);
end;
end.

