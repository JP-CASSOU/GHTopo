unit dial_ellipse;

interface

uses SysUtils, Classes, Graphics, Forms,
  Buttons, ExtCtrls, Controls, StdCtrls, Dialogs,
  ellipsoide;

type
  TDlgEllipsoide = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    Bevel1: TBevel;
    Label1: TLabel;
    EditNom: TEdit;
    Label2: TLabel;
    EditDescription: TEdit;
    Label3: TLabel;
    EditA: TEdit;
    Label4: TLabel;
    EditB: TEdit;
    Label5: TLabel;
    EditE: TEdit;
    Label6: TLabel;
    EditF: TEdit;
    procedure FormShow(Sender: TObject);
    procedure EditAExit(Sender: TObject);
    procedure EditBExit(Sender: TObject);
    procedure EditEExit(Sender: TObject);
    procedure EditFExit(Sender: TObject);
    procedure EditNomExit(Sender: TObject);
    procedure EditDescriptionExit(Sender: TObject);
    procedure EditNomEnter(Sender: TObject);
    procedure EditAEnter(Sender: TObject);
    procedure EditBEnter(Sender: TObject);
    procedure EditEEnter(Sender: TObject);
    procedure EditFEnter(Sender: TObject);
  private
    { Private declarations }
    Memoire : string;
  public
    { Public declarations }
    Ellipse, Source : TEllipsoide;
  end;

var
  DlgEllipsoide: TDlgEllipsoide;

implementation

{$R *.dfm}

uses
  donnee;

procedure TDlgEllipsoide.FormShow(Sender: TObject);
begin
  EditNom.Text:=Ellipse.Nom;
  EditDescription.Text:=Ellipse.Description;
  EditA.Text:=Format('%.9g',[Ellipse.A]);
  EditB.Text:=Format('%.9g',[Ellipse.B]);
  EditE.Text:=Format('%.9g',[Ellipse.E]);
  EditF.Text:=Format('%.9g',[Ellipse.InvF]);
end;

procedure TDlgEllipsoide.EditAExit(Sender: TObject);
begin
  try
    Ellipse.A:=StrToFloat(EditA.Text);
    EditB.Text:=Format('%.9g',[Ellipse.B]);
  except
    ShowMessage('Valeur de a incorrecte, exemple 6371365.32');
    EditA.Text:=Memoire;
    EditA.SetFocus;
  end;
end;

procedure TDlgEllipsoide.EditBExit(Sender: TObject);
begin
  try
    Ellipse.B:=StrToFloat(EditB.Text);
    EditE.Text:=Format('%.9g',[Ellipse.E]);
    EditF.Text:=Format('%.9g',[Ellipse.InvF]);
  except
    ShowMessage('Valeur de b incorrecte, exemple 6371365.32');
    EditB.Text:=Memoire;
    EditB.SetFocus;
  end;
end;

procedure TDlgEllipsoide.EditEExit(Sender: TObject);
begin
  try
    Ellipse.E:=StrToFloat(EditE.Text);
    EditB.Text:=Format('%.9g',[Ellipse.B]);
    EditF.Text:=Format('%.9g',[Ellipse.InvF]);
  except
    ShowMessage('Valeur de e incorrecte, exemple 0.0823554');
    EditE.Text:=Memoire;
    EditE.SetFocus;
  end;
end;

procedure TDlgEllipsoide.EditFExit(Sender: TObject);
begin
  try
    Ellipse.InvF:=StrToFloat(EditF.Text);
    EditB.Text:=Format('%.9g',[Ellipse.B]);
    EditE.Text:=Format('%.9g',[Ellipse.E]);
  except
    ShowMessage('Valeur de 1/f incorrecte, exemple 287.65');
    EditF.Text:=Memoire;
    EditF.SetFocus;
  end;
end;

procedure TDlgEllipsoide.EditNomExit(Sender: TObject);
var
  Lautre : TEllipsoide;
begin
  Ellipse.Nom:=EditNom.Text;
  Lautre:=TEllipsoide(donnees.UnEllipsoide(Ellipse.Nom));
//  if donnees.UnEllipsoide(Ellipse.Nom)<>nil then
  if (Lautre<>nil) and (Lautre<>source) then
  begin
    ShowMessage('Nom déjà utilisé');
    EditNom.Text:=Memoire;
    EditNom.SetFocus;
  end;
end;

procedure TDlgEllipsoide.EditDescriptionExit(Sender: TObject);
begin
  Ellipse.Description:=EditDescription.Text;
end;

procedure TDlgEllipsoide.EditNomEnter(Sender: TObject);
begin
  Memoire:=EditNom.Text;
end;

procedure TDlgEllipsoide.EditAEnter(Sender: TObject);
begin
  Memoire:=EditA.Text;
end;

procedure TDlgEllipsoide.EditBEnter(Sender: TObject);
begin
  Memoire:=EditB.Text;
end;

procedure TDlgEllipsoide.EditEEnter(Sender: TObject);
begin
  Memoire:=EditE.Text;
end;

procedure TDlgEllipsoide.EditFEnter(Sender: TObject);
begin
  Memoire:=EditF.Text;
end;

end.
