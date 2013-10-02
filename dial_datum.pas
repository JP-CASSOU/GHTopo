unit dial_datum;

interface

uses SysUtils, Classes, Graphics, Forms,
  Buttons, ExtCtrls, Controls, StdCtrls, Dialogs,
  datum;

type
  TDlgDatum = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    Bevel1: TBevel;
    Label1: TLabel;
    EditNom: TEdit;
    Label2: TLabel;
    EditDescription: TEdit;
    Label3: TLabel;
    ComboEllipsoide: TComboBox;
    Label4: TLabel;
    Label5: TLabel;
    EditDeltaX: TEdit;
    Label6: TLabel;
    EditDeltaY: TEdit;
    Label7: TLabel;
    EditDeltaZ: TEdit;
    CheckSept: TCheckBox;
    Label8: TLabel;
    Label9: TLabel;
    EditRotX: TEdit;
    Label10: TLabel;
    EditRotY: TEdit;
    Label11: TLabel;
    EditRotZ: TEdit;
    Label12: TLabel;
    EditEchelle: TEdit;
    Label13: TLabel;
    EditGrille: TEdit;
    ButtonGrille: TButton;
    OpenDialogGrille: TOpenDialog;
    procedure CheckSeptClick(Sender: TObject);
    procedure EditNomEnter(Sender: TObject);
    procedure EditDeltaXEnter(Sender: TObject);
    procedure EditDeltaYEnter(Sender: TObject);
    procedure EditDeltaZEnter(Sender: TObject);
    procedure EditRotXEnter(Sender: TObject);
    procedure EditRotYEnter(Sender: TObject);
    procedure EditRotZEnter(Sender: TObject);
    procedure EditEchelleEnter(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ComboEllipsoideChange(Sender: TObject);
    procedure EditDeltaXExit(Sender: TObject);
    procedure EditDeltaYExit(Sender: TObject);
    procedure EditDeltaZExit(Sender: TObject);
    procedure EditRotXExit(Sender: TObject);
    procedure EditRotYExit(Sender: TObject);
    procedure EditRotZExit(Sender: TObject);
    procedure EditEchelleExit(Sender: TObject);
    procedure EditNomExit(Sender: TObject);
    procedure EditDescriptionExit(Sender: TObject);
    procedure ButtonGrilleClick(Sender: TObject);
    procedure EditGrilleExit(Sender: TObject);
  private
    { Private declarations }
    Memoire : string;
  public
    { Public declarations }
    Datum, Source : TDatum;
  end;

var
  DlgDatum: TDlgDatum;

implementation

{$R *.dfm}

uses
  donnee, ellipsoide;

procedure TDlgDatum.CheckSeptClick(Sender: TObject);
begin
  EditRotX.Enabled:=CheckSept.Checked;
  EditRotY.Enabled:=CheckSept.Checked;
  EditRotZ.Enabled:=CheckSept.Checked;
  EditEchelle.Enabled:=CheckSept.Checked;
  Datum.Reduit:=not CheckSept.Checked;
  if CheckSept.Checked then
  begin
    EditRotXExit(Sender);
    EditRotYExit(Sender);
    EditRotZExit(Sender);
    EditEchelleExit(Sender);
  end;
end;

procedure TDlgDatum.EditNomEnter(Sender: TObject);
begin
  Memoire:=EditNom.Text;
end;

procedure TDlgDatum.EditDeltaXEnter(Sender: TObject);
begin
  Memoire:=EditDeltaX.Text;
end;

procedure TDlgDatum.EditDeltaYEnter(Sender: TObject);
begin
  Memoire:=EditDeltaY.Text;
end;

procedure TDlgDatum.EditDeltaZEnter(Sender: TObject);
begin
  Memoire:=EditDeltaZ.Text;
end;

procedure TDlgDatum.EditRotXEnter(Sender: TObject);
begin
  Memoire:=EditRotX.Text;
end;

procedure TDlgDatum.EditRotYEnter(Sender: TObject);
begin
  Memoire:=EditRotY.Text;
end;

procedure TDlgDatum.EditRotZEnter(Sender: TObject);
begin
  Memoire:=EditRotZ.Text;
end;

procedure TDlgDatum.EditEchelleEnter(Sender: TObject);
begin
  Memoire:=EditEchelle.Text;
end;

procedure TDlgDatum.FormShow(Sender: TObject);
var
  I : integer;
begin
  EditNom.Text:=Datum.Nom;
  EditDescription.Text:=Datum.Description;
  // ellipsoïde
  ComboEllipsoide.Items.Clear;
  for I:=0 to Donnees.LesEllipsoide.Count-1 do
  begin
    ComboEllipsoide.Items.AddObject((Donnees.LesEllipsoide[I] as TEllipsoide).Nom,
                                    Donnees.LesEllipsoide[I]);
    if Donnees.LesEllipsoide[I]=Datum.Ellipsoide then
      ComboEllipsoide.ItemIndex:=I;
  end;

  EditDeltaX.Text:=Format('%.6g',[Datum.DeltaX]);
  EditDeltaY.Text:=Format('%.6g',[Datum.DeltaY]);
  EditDeltaZ.Text:=Format('%.6g',[Datum.DeltaZ]);

  if not Datum.Reduit then
  begin
    EditRotX.Text:=Format('%.6g',[Datum.RotX]);
    EditRotY.Text:=Format('%.6g',[Datum.RotY]);
    EditRotZ.Text:=Format('%.6g',[Datum.RotZ]);
    EditEchelle.Text:=Format('%.6g',[Datum.Scale]);
  end
  else begin
    EditRotX.Text:='0';
    EditRotY.Text:='0';
    EditRotZ.Text:='0';
    EditEchelle.Text:='0';
  end;

  EditGrille.Text:=Datum.FileGrille;

  CheckSept.Checked:=not Datum.Reduit;
  CheckSeptClick(Sender);
end;

procedure TDlgDatum.ComboEllipsoideChange(Sender: TObject);
begin
  Datum.Ellipsoide:=TEllipsoide(ComboEllipsoide.Items.Objects[ComboEllipsoide.ItemIndex]);
end;

procedure TDlgDatum.EditDeltaXExit(Sender: TObject);
begin
  try
    Datum.DeltaX:=StrToFloat(EditDeltaX.Text);
  except
    ShowMessage('Valeur de DeltaX incorrecte, exemple 125.365');
    EditDeltaX.Text:=Memoire;
    EditDeltaX.SetFocus;
  end;
end;

procedure TDlgDatum.EditDeltaYExit(Sender: TObject);
begin
  try
    Datum.DeltaY:=StrToFloat(EditDeltaY.Text);
  except
    ShowMessage('Valeur de DeltaY incorrecte, eYemple 125.365');
    EditDeltaY.Text:=Memoire;
    EditDeltaY.SetFocus;
  end;
end;

procedure TDlgDatum.EditDeltaZExit(Sender: TObject);
begin
  try
    Datum.DeltaZ:=StrToFloat(EditDeltaZ.Text);
  except
    ShowMessage('Valeur de DeltaZ incorrecte, exemple 125.365');
    EditDeltaZ.Text:=Memoire;
    EditDeltaZ.SetFocus;
  end;
end;

procedure TDlgDatum.EditRotXExit(Sender: TObject);
begin
  try
    Datum.RotX:=StrToFloat(EditRotX.Text);
  except
    ShowMessage('Valeur de RotX incorrecte, exemple 125.365');
    EditRotX.Text:=Memoire;
    EditRotX.SetFocus;
  end;
end;

procedure TDlgDatum.EditRotYExit(Sender: TObject);
begin
  try
    Datum.RotY:=StrToFloat(EditRotY.Text);
  except
    ShowMessage('Valeur de RotY incorrecte, exemple 125.365');
    EditRotY.Text:=Memoire;
    EditRotY.SetFocus;
  end;
end;

procedure TDlgDatum.EditRotZExit(Sender: TObject);
begin
  try
    Datum.RotZ:=StrToFloat(EditRotZ.Text);
  except
    ShowMessage('Valeur de RotZ incorrecte, exemple 125.365');
    EditRotZ.Text:=Memoire;
    EditRotZ.SetFocus;
  end;
end;

procedure TDlgDatum.EditEchelleExit(Sender: TObject);
begin
  try
    Datum.Scale:=StrToFloat(EditEchelle.Text);
  except
    ShowMessage('Valeur de Echelle incorrecte, exemple 125.365');
    EditEchelle.Text:=Memoire;
    EditEchelle.SetFocus;
  end;
end;

procedure TDlgDatum.EditNomExit(Sender: TObject);
var
  Lautre : TDatum;
begin
  Datum.Nom:=EditNom.Text;
  if not Donnees.Inconnu(Datum.Nom) then
  begin
    Lautre:=TDatum(donnees.UnDatum(Datum.Nom));
    if (Lautre=nil) or (Lautre<>source) then
    begin
      ShowMessage('Nom déjà utilisé');
      EditNom.Text:=Memoire;
      EditNom.SetFocus;
    end;
  end;
end;

procedure TDlgDatum.EditDescriptionExit(Sender: TObject);
begin
  Datum.Description:=EditDescription.Text;
end;

procedure TDlgDatum.ButtonGrilleClick(Sender: TObject);
begin
  if EditGrille.Text<>'' then
    OpenDialogGrille.FileName:=EditGrille.Text;
  if OpenDialogGrille.Execute then
  begin
    EditGrille.Text:=OpenDialogGrille.FileName;
    Datum.FileGrille:=EditGrille.Text;
  end;  
end;

procedure TDlgDatum.EditGrilleExit(Sender: TObject);
begin
  Datum.FileGrille:=EditGrille.Text;
end;

end.
