unit dial_projection;

interface

uses SysUtils, Classes, Graphics, Forms,
  Buttons, ExtCtrls, Controls, StdCtrls, Grids, Dialogs,
  ancetre, projection, multiple;

type
  TDlgProjection = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    Bevel1: TBevel;
    Label1: TLabel;
    EditNom: TEdit;
    Label2: TLabel;
    EditDescription: TEdit;
    Label3: TLabel;
    ComboNature: TComboBox;
    Label4: TLabel;
    ComboDatum: TComboBox;
    GridParametre: TStringGrid;
    Label5: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ComboNatureChange(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure EditNomEnter(Sender: TObject);
    procedure EditNomExit(Sender: TObject);
  private
    { Private declarations }
    fCas_Multiple : boolean;
    Memoire : string;
  public
    { Public declarations }
    Resultat, Source : TAncetre;
    procedure Prepare(Projection : TAncetre; Proj_Multiple : boolean);
  end;

var
  DlgProjection: TDlgProjection;

implementation

{$R *.dfm}

uses
  createur, donnee, datum, outils;

procedure TDlgProjection.FormCreate(Sender: TObject);
begin
  GridParametre.Cells[0,0]:='Nom';
  GridParametre.Cells[1,0]:='Valeur';
end;

procedure TDlgProjection.ComboNatureChange(Sender: TObject);
var
  Taille, I : integer;
  Nom_Proj : string;
begin
  Nom_Proj:=ComboNature.Items[ComboNature.ItemIndex];
  Taille:=Nb_Param_Projection(Nom_Proj);
  GridParametre.Visible:=(Taille<>0);
  if Taille=0 then
    Label5.Caption:='Aucun paramètre'
  else
    Label5.Caption:='&Propriétés';

  GridParametre.RowCount:=Taille+1;
  for I:=0 to Taille-1 do
    GridParametre.Cells[0,I+1]:=Nom_Param_Projection(Nom_Proj,I);
  if GridParametre.RowCount>1 then
    GridParametre.FixedRows:=1;
end;

procedure TDlgProjection.Prepare(Projection : TAncetre; Proj_Multiple : boolean);
var
  I, Compteur : integer;
begin
  fCas_Multiple:=Proj_Multiple;
  Source:=Projection;

  ComboNature.Items.Clear;
  if fCas_Multiple then
    for I:=low(RelationMulti) to high(RelationMulti) do
      ComboNature.Items.Add(RelationMulti[I].Texte)
  else
    for I:=low(RelationProj) to high(RelationProj) do
      ComboNature.Items.Add(RelationProj[I].Texte);
  ComboNature.ItemIndex:=0;

  ComboDatum.Items.Clear;
  for I:=0 to Donnees.LesDatum.Count-1 do
  begin
    ComboDatum.Items.AddObject((Donnees.LesDatum.Items[I] as TDatum).Nom,
                               Donnees.LesDatum.Items[I]);
    if (Projection<>nil) and (Projection.Datum=Donnees.LesDatum.Items[I]) then
      ComboDatum.ItemIndex:=I;
  end;
  if Projection<>nil then
  begin
    EditNom.Text:=Projection.Nom;
    EditDescription.Text:=Projection.Description;
    for I:=0 to ComboNature.Items.Count-1 do if
      ComboNature.Items[I]=Projection.Nature then
        ComboNature.ItemIndex:=I;
    ComboNatureChange(self);
    Compteur:=0;
    for I:=0 to Projection.Nb_ParamReal-1 do
    begin
      inc(Compteur);
      GridParametre.Cells[1,Compteur]:=Format('%.12g',[Projection.ParamReal[I]]);
    end;
    for I:=0 to Projection.Nb_ParamInteger-1 do
    begin
      inc(Compteur);
      GridParametre.Cells[1,Compteur]:=Format('%d',[Projection.ParamInteger[I]]);
    end;
    for I:=0 to Projection.Nb_ParamAngle-1 do
    begin
      inc(Compteur);
      GridParametre.Cells[1,Compteur]:=Projection.ParamAngle[I];
    end;
    for I:=0 to Projection.Nb_ParamBoolean-1 do
    begin
      inc(Compteur);
      if Projection.ParamBoolean[I] then
        GridParametre.Cells[1,Compteur]:='OUI'
      else
        GridParametre.Cells[1,Compteur]:='NON';
    end;
  end
  else begin
    Compteur:=1;
    while not Donnees.Inconnu(Format('Projection%d',[Compteur])) do
      inc(Compteur);
    EditNom.Text:=Format('Projection%d',[Compteur]);

    EditDescription.Text:='';
    ComboDatum.ItemIndex:=0;
    ComboNatureChange(self);
  end;
end;

procedure TDlgProjection.OKBtnClick(Sender: TObject);
var
  Compteur, I : integer;
  S : string;
begin
  try
    if fCas_Multiple then
      Resultat:=CreateMultiple(ComboNature.Items[ComboNature.ItemIndex])
    else
      Resultat:=CreateProjection(ComboNature.Items[ComboNature.ItemIndex]);
    // vérifier que le nom n'existe pas avant
    Resultat.Nom:=EditNom.Text;
    Resultat.Description:=EditDescription.Text;
    Resultat.Datum:=TDatum(ComboDatum.Items.Objects[ComboDatum.ItemIndex]);

    // récupération des paramètres spécifiques à la projection
    Compteur:=0;
    for I:=0 to Resultat.Nb_ParamReal-1 do
    begin
      inc(Compteur);
      try
        Resultat.ParamReal[I]:=StrToFloat(GridParametre.Cells[1,Compteur]);
      except
        ShowMessageFmt('Le paramètre %s n''est pas correcte, exemple 6371235.36',[GridParametre.Cells[0,Compteur]]);
        GridParametre.SetFocus;
        GridParametre.Row:=Compteur;
        GridParametre.Col:=1;
        raise;
      end;
    end;
    for I:=0 to Resultat.Nb_ParamInteger-1 do
    begin
      inc(Compteur);
      try
        Resultat.ParamInteger[I]:=StrToInt(GridParametre.Cells[1,Compteur]);
      except
        ShowMessageFmt('Le paramètre %s doit être entier, exemple 7',[GridParametre.Cells[0,Compteur]]);
        GridParametre.SetFocus;
        GridParametre.Row:=Compteur;
        GridParametre.Col:=1;
        raise;
      end;
    end;
    for I:=0 to Resultat.Nb_ParamAngle-1 do
    begin
      inc(Compteur);
      try
        Resultat.ParamAngle[I]:=GridParametre.Cells[1,Compteur];
      except
        ShowMessageFmt('Le paramètre %s doit être un angle, exemple 2°15''03.235''''',[GridParametre.Cells[0,Compteur]]);
        GridParametre.SetFocus;
        GridParametre.Row:=Compteur;
        GridParametre.Col:=1;
        raise;
      end;
    end;
    for I:=0 to Resultat.Nb_ParamBoolean-1 do
    begin
      inc(Compteur);
      S:=UpperCase(GridParametre.Cells[1,Compteur]);
      if (S='OUI') or (S='1') or (S='TRUE') then
        Resultat.ParamBoolean[I]:=true
      else
        if (S='NON') or (S='0') or (S='FALSE') then
          Resultat.ParamBoolean[I]:=false
        else begin
          ShowMessageFmt('Le paramètre %s ne peut que OUI ou NON',[GridParametre.Cells[0,Compteur]]);
          GridParametre.SetFocus;
          GridParametre.Row:=Compteur;
          GridParametre.Col:=1;
          raise EConvertError.Create('Paramètre non booléen');
        end;
    end;
    Resultat.Valid:=true;
    ModalResult:=mrOk;
  except
    Resultat.Free;
    ModalResult:=mrNone;
  end;
end;

procedure TDlgProjection.EditNomEnter(Sender: TObject);
begin
  Memoire:=EditNom.Text;
end;

procedure TDlgProjection.EditNomExit(Sender: TObject);
var
  Lautre : TAncetre;
begin
  if not Donnees.Inconnu(EditNom.Text) then
  begin
    Lautre:=TAncetre(donnees.UneProjection(EditNom.Text));
    if (Lautre=nil) or (Lautre<>source) then
    begin
      ShowMessage('Nom déjà utilisé');
      EditNom.Text:=Memoire;
      EditNom.SetFocus;
    end;
  end;
end;

end.
