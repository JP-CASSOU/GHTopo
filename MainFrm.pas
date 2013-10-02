unit MainFrm;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, dlgProjection;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    CheckSourceParis: TCheckBox;
    CheckDestParis: TCheckBox;
    ComboSource: TComboBox;
    ComboDestination: TComboBox;
    EditXDest: TEdit;
    EditYDest: TEdit;
    EditConvergence: TEdit;
    EditAlteration: TEdit;
    EditXSource: TEdit;
    EditYSource: TEdit;
    EditDescription2: TEdit;
    EditFuseauSource: TEdit;
    EditDescription1: TEdit;
    EditFuseauDest: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    RadioUnite: TRadioGroup;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ComboDestinationChange(Sender: TObject);
    procedure ComboSourceChange(Sender: TObject);
    procedure ComboSourceExit(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
    fSortie : boolean;
    fSortieSender : TObject;
    procedure ChangementDonnees;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation
uses
  donnee, outils, listing, fichier_ini;

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  with TDlgProjections do;
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

procedure TForm1.ComboSourceChange(Sender: TObject);
begin
  EditDescription1.Text:=Donnees.LaDescription(ComboSource.Items[ComboSource.ItemIndex]);
  if ComboSource.ItemIndex<Donnees.LesDatum.Count then
  begin
    Label3.Caption:='&Latitude';
    Label3.Caption:='Lon&gitude';
    CheckSourceParis.Enabled:=true;
  end
  else begin
    Label3.Caption:='&Easting (m)';
    Label3.Caption:='&Northing (m)';
    CheckSourceParis.Enabled:=false;
  end;
  EditFuseauSource.Enabled:=
    (ComboSource.ItemIndex>=Donnees.LesDatum.Count+Donnees.LesProjection.Count);
end;

procedure TForm1.ComboSourceExit(Sender: TObject);
begin
  fSortie:=true;
  fSortieSender:=sender;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
 Flux : TStream;
 Chemin : string;

begin

 fSortie:=false;

 defaultFormatSettings.DecimalSeparator:='.';
 Donnees:=TDonnees.Create;
 if FichierIni.AutoSave then
 begin
   try
     //Chemin:=FichierIni.Chemin+'donnees.txt';
     Chemin := ExtractFilePath(ParamStr(0)) +'donnees.txt';

     Flux:=TFileStream.Create(Chemin,fmOpenRead);
     try
       Donnees.LoadFromStream(Flux);
     finally
       Flux.Free;
     end;
   except // si on n'a pas réussi à charger donnees.txt,
          // on essaie de charger france.txt
     try
       Chemin:= ExtractFilePath(ParamStr(0)) +'france.txt';
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

procedure TForm1.FormDestroy(Sender: TObject);
var
  Chemin: String;
  Flux: TFileStream;
begin
  if FichierIni.AutoSave then
  begin
    try
      Chemin := ExtractFilePath(ParamStr(0)) +'donnees.txt';
      Flux:=TFileStream.Create(Chemin,fmCreate);
      Donnees.SaveToStream(Flux);
      Flux.Free;
    except
      ShowMessageFmt('Problème à l''enregistrement des données dans %s',[Chemin]);
    end;
  end;
  Donnees.Free;
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

end.

