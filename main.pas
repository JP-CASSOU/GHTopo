unit main;

interface

uses
  SysUtils, Types, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    OpenDialog1: TOpenDialog;
    Button2: TButton;
    GroupBox1: TGroupBox;
    Label9: TLabel;
    ComboSource: TComboBox;
    Label10: TLabel;
    EditDescription1: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    EditXSource: TEdit;
    EditYSource: TEdit;
    Label7: TLabel;
    EditFuseauSource: TEdit;
    CheckSourceParis: TCheckBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    ComboDestination: TComboBox;
    Label11: TLabel;
    EditDescription2: TEdit;
    Label12: TLabel;
    Label13: TLabel;
    EditXDest: TEdit;
    EditYDest: TEdit;
    Label14: TLabel;
    EditFuseauDest: TEdit;
    CheckDestParis: TCheckBox;
    RadioUnite: TRadioGroup;
    Button3: TButton;
    EditConvergence: TEdit;
    Label4: TLabel;
    EditAlteration: TEdit;
    Label5: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ComboSourceChange(Sender: TObject);
    procedure ComboDestinationChange(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure ComboSourceExit(Sender: TObject);
  private
    { Déclarations privées }
    fSortie : boolean;
    fSortieSender : TObject;
    procedure ChangementDonnees;
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

uses
{$IFDEF MSWINDOWS}
  windows,
{$ENDIF}
  donnee, outils, listing, fichier_ini;

{$R *.dfm}


procedure TForm1.FormCreate(Sender: TObject);
var
  Flux : TStream;
  Chemin : string;
{$IFDEF MSWINDOWS}
  FISize,dummy:   Dword;
  Buff:     pointer;
  BuffSize: integer;
  FI:       PVSFixedFileInfo;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  BuffSize := GetFileVersionInfoSize(PChar(Application.ExeName), dummy);
  if BuffSize = 0 then exit;  // Fail: Version not managed

  GetMem(Buff, BuffSize);
  try
    GetFileVersionInfo(PChar(Application.ExeName), dummy, BuffSize, Buff);
    if VerQueryValue(Buff, '\', Pointer(FI), FISize) then begin
      Caption:=Format('Convertisseur de coordonnées - %d.%d.%d.%d',[FI.dwFileVersionMS shr 16,
                               FI.dwFileVersionMS AND $FFFF,
                               FI.dwFileVersionLS shr 16,
                               FI.dwFileVersionLS AND $FFFF]);
    end;
  finally
    FreeMem(Buff);
  end;
{$ENDIF}


  fSortie:=false;

  DecimalSeparator:='.';
  Donnees:=TDonnees.Create;
  if FichierIni.AutoSave then
  begin
    try
      Chemin:=FichierIni.Chemin+'donnees.txt';
      Flux:=TFileStream.Create(Chemin,fmOpenRead);
      try
        Donnees.LoadFromStream(Flux);
      finally
        Flux.Free;
      end;
    except // si on n'a pas réussi à charger donnees.txt,
           // on essaie de charger france.txt
      try
        Chemin:=ExtractFilePath(Application.ExeName)+'france.txt';
        Flux:=TFileStream.Create(Chemin,fmOpenRead);
        try
          Donnees.LoadFromStream(Flux);
        finally
          Flux.Free;
        end;
      except
      end;
    end;
  end;
  ChangementDonnees;

end;

procedure TForm1.ChangementDonnees;
var
  ListeProposition : TStringList;
  Separations : integer;
begin
  ListeProposition:=TStringList.Create;
  Donnees.ListeSystemes(ListeProposition,Separations);
  ComboSource.Items.Assign(ListeProposition);
  ComboSource.ItemIndex:=0;
  ComboSource.Tag:=Separations;
  ComboDestination.Items.Assign(ListeProposition);
  ComboDestination.ItemIndex:=0;
  ComboDestination.Tag:=Separations;
  ListeProposition.Free;
  ComboSourceChange(self);
  ComboDestinationChange(self);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  DlgParametre.ShowModal;
  ChangementDonnees;
end;

procedure TForm1.FormDestroy(Sender: TObject);
var
  Flux : TStream;
  Chemin : string;
begin
  if FichierIni.AutoSave then
  begin
    try
      Chemin:=FichierIni.Chemin+'donnees.txt';
      Flux:=TFileStream.Create(Chemin,fmCreate);
      Donnees.SaveToStream(Flux);
      Flux.Free;
    except
      ShowMessageFmt('Problème à l''enregistrement des données dans %s',[Chemin]);
    end;
  end;
  Donnees.Free;
end;

procedure TForm1.ComboSourceChange(Sender: TObject);
begin
  EditDescription1.Text:=Donnees.LaDescription(ComboSource.Items[ComboSource.ItemIndex]);
  if ComboSource.ItemIndex<Donnees.LesDatum.Count then
  begin
    Label2.Caption:='&Latitude';
    Label3.Caption:='Lon&gitude';
    CheckSourceParis.Enabled:=true;
  end
  else begin
    Label2.Caption:='&Easting (m)';
    Label3.Caption:='&Northing (m)';
    CheckSourceParis.Enabled:=false;
  end;
  EditFuseauSource.Enabled:=
    (ComboSource.ItemIndex>=Donnees.LesDatum.Count+Donnees.LesProjection.Count);
end;

procedure TForm1.ComboDestinationChange(Sender: TObject);
begin
  EditDescription2.Text:=Donnees.LaDescription(ComboDestination.Items[ComboDestination.ItemIndex]);
  if ComboDestination.ItemIndex<Donnees.LesDatum.Count then
  begin
    Label12.Caption:='&Latitude';
    Label13.Caption:='Lon&gitude';
    CheckDestParis.Enabled:=true;
//    RadioUnite.Enabled:=true;
  end
  else begin
    Label12.Caption:='Easting (m)';
    Label13.Caption:='Northing (m)';
//    RadioUnite.Enabled:=false;
    CheckDestParis.Enabled:=false;
  end;
  EditFuseauDest.Enabled:=
    (ComboDestination.ItemIndex>=Donnees.LesDatum.Count+Donnees.LesProjection.Count);
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  SX, SY : string;
  UniteAngle : TUniteAngle;
  NumSource, NumDest : integer;
  Facteur : real;
begin
  case RadioUnite.ItemIndex of
    0 : UniteAngle:=uaDegre;
    1 : UniteAngle:=uaDM;
    2 : UniteAngle:=uaDMS;
    3 : UniteAngle:=uaGrade;
  else
    Raise EConvertError.Create('Unité d''angle inconnue');
  end;
  if ComboSource.ItemIndex<Donnees.LesDatum.Count then
  begin
    if CheckSourceParis.Checked then
      NumSource:=1
    else
      NumSource:=0;
  end
  else begin
    try
      NumSource:=StrToInt(EditFuseauSource.Text);
    except
      if EditFuseauSource.Enabled then
      begin
//        ShowMessage('Le numéro de fuseau de départ est incorrecte, exemple 12');
        EditFuseauSource.SetFocus;
        Raise EConvertError.Create('Le numéro de fuseau de départ est incorrecte, exemple 12');
      end
      else
        NumSource:=0;
    end;
  end;
  if ComboDestination.ItemIndex<Donnees.LesDatum.Count then
    if CheckDestParis.Checked then
      NumDest:=1
    else
      NumDest:=0;
  Donnees.Conversion(ComboSource.Text, ComboDestination.Text,
                     EditXSource.Text, EditYSource.Text,
                     NumSource, NumDest,
                     SX, SY, UniteAngle);
  EditXDest.Text:=SX;
  EditYDest.Text:=SY;
  Donnees.Convergence(ComboSource.Text, ComboDestination.Text,
                     EditXSource.Text, EditYSource.Text,
                     NumSource,
                     SX, UniteAngle);
  EditConvergence.Text:=SX;
  Donnees.Alteration(ComboSource.Text, ComboDestination.Text,
                     EditXSource.Text, EditYSource.Text,
                     NumSource,
                     Facteur);
  EditAlteration.Text:=FloatToStr(Facteur);
  if ComboDestination.ItemIndex>=Donnees.LesDatum.Count+Donnees.LesProjection.Count then
    EditFuseauDest.Text:=IntToStr(NumDest);

end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.ComboSourceExit(Sender: TObject);
begin
  fSortie:=true;
  fSortieSender:=sender;
end;

end.


