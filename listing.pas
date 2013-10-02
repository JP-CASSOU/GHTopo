unit listing;

interface

uses SysUtils, Classes, Graphics, Forms,
  Buttons, ExtCtrls, Controls, StdCtrls, ComCtrls, Dialogs;

type
  TDlgParametre = class(TForm)
    OKBtn: TButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    ListEllips: TListBox;
    ButtonAjoutEllips: TButton;
    Button1: TButton;
    Button2: TButton;
    CheckAutomatique: TCheckBox;
    ButtonOuvrir: TButton;
    ButtonEnregistrer: TButton;
    ListDatum: TListBox;
    ButtonAjoutDatum: TButton;
    Button4: TButton;
    Button5: TButton;
    ListSimple: TListBox;
    ButtonAjoutProjection: TButton;
    Button7: TButton;
    Button8: TButton;
    ListMultiple: TListBox;
    ButtonAjoutMultiple: TButton;
    Button10: TButton;
    Button11: TButton;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    procedure ButtonOuvrirClick(Sender: TObject);
    procedure ButtonEnregistrerClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ButtonAjoutEllipsClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure ButtonAjoutDatumClick(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure ButtonAjoutProjectionClick(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure ButtonAjoutMultipleClick(Sender: TObject);
    procedure CheckAutomatiqueClick(Sender: TObject);
  private
    { Private declarations }
    procedure Rafraichir;
  public
    { Public declarations }
  end;

var
  DlgParametre: TDlgParametre;

implementation

{$R *.dfm}

uses
  dial_import, donnee, ellipsoide, datum, projection, multiple,
  dial_ellipse, dial_datum, dial_projection,
  fichier_ini;

procedure TDlgParametre.ButtonOuvrirClick(Sender: TObject);
var
  Flux : TStream;
  I : integer;
begin
  if OpenDialog.Execute then
  begin
    try
      DlgImportation.Prepare(OpenDialog.FileName);
      if DlgImportation.ShowModal=mrOk then
      begin
        try
          if DlgImportation.CheckTout.Checked then
          begin // importation complète du fichier
            Flux:=TFileStream.Create(OpenDialog.FileName,fmOpenRead);
            try
              Donnees.LoadFromStream(Flux);
            finally
              Flux.Free;
            end;
          end
          else with DlgImportation do
          begin // importation des lignes sélectionnées uniquement
            for I:=0 to CheckListBoxImport.Items.Count-1 do
              if CheckListBoxImport.Checked[I] then
              try
                Donnees.AjoutLigne(CheckListBoxImport.Items[I]);
              except
              end;  
          end;
        except
        end;
        Rafraichir;
      end;
    except
      ShowMessageFmt('Problème de lecture du fichier %s',[OpenDialog.FileName]);
    end;
  end;
end;

procedure TDlgParametre.ButtonEnregistrerClick(Sender: TObject);
var
  Flux : TStream;
begin
  if SaveDialog.Execute then
  begin
    try
      Flux:=TFileStream.Create(SaveDialog.FileName,fmCreate);
      Donnees.SaveToStream(Flux);
      Flux.Free;
    except
      ShowMessageFmt('Problème à l''enregistrement des données dans %s',[SaveDialog.FileName]);
    end;
  end;
end;

procedure TDlgParametre.Rafraichir;
var
  I : integer;
begin
  ListEllips.Items.Clear;
  with Donnees.LesEllipsoide do
    for I:=0 to Count-1 do
      ListEllips.Items.AddObject((Items[I] as TEllipsoide).Nom, Items[I]);

  ListDatum.Items.Clear;
  with Donnees.LesDatum do
    for I:=0 to Count-1 do
      ListDatum.Items.AddObject((Items[I] as TDatum).Nom, Items[I]);

  ListSimple.Items.Clear;
  with Donnees.LesProjection do
    for I:=0 to Count-1 do
      ListSimple.Items.AddObject((Items[I] as TProjection).Nom, Items[I]);

  ListMultiple.Items.Clear;
  with Donnees.LesMultiple do
    for I:=0 to Count-1 do
      ListMultiple.Items.AddObject((Items[I] as TMultiple).Nom, Items[I]);

end;

procedure TDlgParametre.FormShow(Sender: TObject);
begin
  CheckAutomatique.Checked:=FichierIni.AutoSave;
  Rafraichir;
end;

procedure TDlgParametre.ButtonAjoutEllipsClick(Sender: TObject);
var
  Nvx_Ellipsoide : TEllipsoide;
  Compteur : integer;
begin
  Nvx_Ellipsoide:=TEllipsoide.Create;
  if Sender=ButtonAjoutEllips then
  begin // nouvel ellipsoïde
    with Nvx_Ellipsoide do
    begin
      a:=6371000;
      Invf:=287;
      Compteur:=1;
      while Donnees.UnEllipsoide(Format('Ellipsoide%d',[Compteur]))<>nil do
        inc(Compteur);
      Nom:=Format('Ellipsoide%d',[Compteur]);
      Description:='';
    end;
    DlgEllipsoide.Source:=nil;
  end
  else begin // ellipsoïde existant
    case ListEllips.ItemIndex of
      -1 : begin
        ShowMessage('Veuillez sélectionner l''ellipsoïde à modifier');
        abort;
      end;
      0 : begin
        ShowMessage('Vous n''êtes pas autorisé à modifier l''ellipsoïde GRS 80');
        abort;
      end;
      else begin
        DlgEllipsoide.Source:=TEllipsoide(ListEllips.Items.Objects[ListEllips.ItemIndex]);
        Nvx_Ellipsoide.Assign(DlgEllipsoide.Source);
      end;
    end;
  end;
  DlgEllipsoide.Ellipse:=Nvx_Ellipsoide;
  if DlgEllipsoide.ShowModal=mrOk then
  begin
    if Sender=ButtonAjoutEllips then
      Donnees.LesEllipsoide.Add(Nvx_Ellipsoide)
    else begin
      with DlgEllipsoide.Source do
      begin
        a:=Nvx_Ellipsoide.a;
        Invf:=Nvx_Ellipsoide.Invf;
        Nom:=Nvx_Ellipsoide.Nom;
        Description:=Nvx_Ellipsoide.Description;
      end;
      Nvx_Ellipsoide.Free;
    end;
    Rafraichir;
  end
  else
    Nvx_Ellipsoide.Free;
end;

procedure TDlgParametre.Button2Click(Sender: TObject);
begin
  case ListEllips.ItemIndex of
    -1 : ShowMessage('Veuillez sélectionner l''ellipsoïde avant de le supprimer');
    0 : ShowMessage('Vous n''êtes pas autorisé à supprimer l''ellipsoïde GRS 80');
    else begin
      if MessageDlg(Format('Etes-vous sûr de vouloir supprimer l''ellipsoïde %s',
             [ListEllips.Items[ListEllips.ItemIndex]]),mtWarning,[mbOk,mbCancel],0)=mrOk then
      begin
        if Donnees.RetireEllipsoide(ListEllips.Items[ListEllips.ItemIndex]) then
          Rafraichir
        else
          ShowMessage('Impossible de supprimer cet ellipsoïde qui est utilisé par certains datums');
      end;
    end;
  end;
end;

procedure TDlgParametre.Button5Click(Sender: TObject);
begin
  case ListDatum.ItemIndex of
    -1 : ShowMessage('Veuillez sélectionner le datum avant de le supprimer');
    0 : ShowMessage('Vous n''êtes pas autorisé à supprimer le datum WGS84');
    else begin
      if MessageDlg(Format('Etes-vous sûr de vouloir supprimer le datum %s',
             [ListDatum.Items[ListDatum.ItemIndex]]),mtWarning,[mbOk,mbCancel],0)=mrOk then
      begin
        if Donnees.RetireDatum(ListDatum.Items[ListDatum.ItemIndex]) then
          Rafraichir
        else
          ShowMessage('Impossible de supprimer ce datum qui est utilisé par certaines projections.');
      end;
    end;
  end;
end;

procedure TDlgParametre.ButtonAjoutDatumClick(Sender: TObject);
var
  Nvx_Datum : TDatum;
  Compteur : integer;
begin
  Nvx_Datum:=TDatum.Create;
  if Sender=ButtonAjoutDatum then
  begin // nouveau datum
    with Nvx_Datum do
    begin
      DeltaX:=0;
      DeltaY:=0;
      DeltaZ:=0;
      Reduit:=true;
      Ellipsoide:=TEllipsoide(Donnees.UnEllipsoide('GRS 80'));

      Compteur:=1;
      while not Donnees.Inconnu(Format('Datum%d',[Compteur])) do
        inc(Compteur);
      Nom:=Format('Datum%d',[Compteur]);
      Description:='';
    end;
    DlgDatum.Source:=nil;
  end
  else begin // datum existant
    case ListDatum.ItemIndex of
      -1 : begin
        ShowMessage('Veuillez sélectionner le datum à modifier');
        abort;
      end;
      0 : begin
        ShowMessage('Vous n''êtes pas autorisé à modifier le datum WGS84');
        abort;
      end;
      else begin
        DlgDatum.Source:=TDatum(ListDatum.Items.Objects[ListDatum.ItemIndex]);
        Nvx_Datum.Assign(DlgDatum.Source);
      end;
    end;
  end;
  DlgDatum.Datum:=Nvx_Datum;
  if DlgDatum.ShowModal=mrOk then
  begin
    if Sender=ButtonAjoutDatum then
      Donnees.LesDatum.Add(Nvx_Datum)
    else begin
      DlgDatum.Source.Assign(Nvx_Datum);
      Nvx_Datum.Free;
    end;
    Rafraichir;
  end
  else
    Nvx_Datum.Free;
end;

procedure TDlgParametre.Button8Click(Sender: TObject);
// suppression d'une projection simple
var
  Candidat : TProjection;
  Nom : string;
begin
  if ListSimple.ItemIndex=-1 then
    ShowMessage('Veuillez sélectionner la projection avant de la supprimer')
  else begin
    Nom:=ListSimple.Items[ListSimple.ItemIndex];
    if MessageDlg(Format('Etes-vous sûr de vouloir supprimer la projection %s',
             [Nom]),mtWarning,[mbOk,mbCancel],0)=mrOk then
    begin
      Candidat:=TProjection(Donnees.UneProjection(Nom));
      Donnees.LesProjection.Remove(Candidat);
      Rafraichir
    end;
  end;
end;

procedure TDlgParametre.ButtonAjoutProjectionClick(Sender: TObject);
begin
  if Sender=ButtonAjoutProjection then
    DlgProjection.Prepare(nil, false)
  else
    DlgProjection.Prepare(TProjection(ListSimple.Items.Objects[ListSimple.ItemIndex]), false);
  if DlgProjection.ShowModal=mrOk then
  begin
    if Sender=ButtonAjoutProjection then
      Donnees.LesProjection.Add(DlgProjection.Resultat)
    else with Donnees.LesProjection do
      Insert(Remove(ListSimple.Items.Objects[ListSimple.ItemIndex]),
             DlgProjection.Resultat);
    Rafraichir;
  end;
end;

procedure TDlgParametre.Button11Click(Sender: TObject);
// suppression d'une projection multiple
var
  Candidat : TMultiple;
  Nom : string;
begin
  if ListMultiple.ItemIndex=-1 then
    ShowMessage('Veuillez sélectionner la projection avant de la supprimer')
  else begin
    Nom:=ListMultiple.Items[ListMultiple.ItemIndex];
    if MessageDlg(Format('Etes-vous sûr de vouloir supprimer la projection %s',
             [Nom]),mtWarning,[mbOk,mbCancel],0)=mrOk then
    begin
      Candidat:=TMultiple(Donnees.UneProjection(Nom));
      Donnees.LesMultiple.Remove(Candidat);
      Rafraichir
    end;
  end;
end;

procedure TDlgParametre.ButtonAjoutMultipleClick(Sender: TObject);
begin
  if Sender=ButtonAjoutMultiple then
    DlgProjection.Prepare(nil, true)
  else
    DlgProjection.Prepare(TMultiple(ListMultiple.Items.Objects[ListMultiple.ItemIndex]), true);
  if DlgProjection.ShowModal=mrOk then
  begin
    if Sender=ButtonAjoutMultiple then
      Donnees.LesMultiple.Add(DlgProjection.Resultat)
    else with Donnees.LesMultiple do
      Insert(Remove(ListMultiple.Items.Objects[ListMultiple.ItemIndex]),
             DlgProjection.Resultat);
    Rafraichir;
  end;
end;

procedure TDlgParametre.CheckAutomatiqueClick(Sender: TObject);
begin
  FichierIni.AutoSave:=CheckAutomatique.Checked;
end;

end.
