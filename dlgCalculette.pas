unit dlgCalculette;
{$INCLUDE CompilationParameters.inc}
interface
uses
  {$IFDEF FRENCH_MESSAGES}
     UnitMessages_fr,
  {$ENDIF}
  {$IFDEF ENGLISH_MESSAGES}
     UnitMessages_en,
  {$ENDIF}
  {$IFDEF SPANISH_MESSAGES}
     UnitMessages_sp,
  {$ENDIF}
  Common,
  StructuresDonnees,
  {$IFDEF MSWINDOWS}
    {$IFDEF USE_CONVERTISSEUR_EPSG}
      ConversionsCoordonneesEPSG, // API EPSG : Boguée, à ne pas utiliser.
    {$ENDIF}
    {$IFDEF USE_CONVERTISSEUR_ESIBERT}
      ConversionsCoordonneesESibert, // API E Sibert
    {$ENDIF}
    {$IFDEF USE_CONVERTISSEUR_JPC}
      ConvertisseurJPC, // API JPC
    {$ENDIF}
    UnitWrapperDeclimag,        // pour la déclinaison magnétique
  {$ENDIF}

  {$IFDEF LINUX}
    ConversionsCoordonneesEPSGLinux, // Convertisseur de coordonnées E.SIBERT
    UnitWrapperDeclimagLinux,
  {$ENDIF}
  Classes, SysUtils,
  Clipbrd,
  UPCalc, UEval,
  FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons, EditBtn,
  ComCtrls, Grids, curredit;

// pour l'export KML
type TLigneCoordonnees = record
  Etiquette      : string;
  XSource        : double;
  YSource        : double;
  XCible         : double;
  YCible         : double;
end;



type

  { TfrmCalculette }

  TfrmCalculette = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    btnCalcul: TButton;
    btnCalculDeclimag: TButton;
    Button1: TButton;
    btnConversionJPC: TButton;
    btnTableDeclimagAnnuelle: TButton;
    Button2: TButton;
    btnCopierTableau: TButton;
    btnExportCarto: TButton;
    btnConversionEnRafale: TButton;
    chkAutoIncrement: TCheckBox;
    chkPremLigneIsTitres: TCheckBox;
    cmbSystCibleJPC: TComboBox;
    cmbColXSource: TComboBox;
    cmbColYSource: TComboBox;
    cmbColXCible: TComboBox;
    cmbColYCible: TComboBox;
    cmbSystSourceJPC: TComboBox;
    cmbColLabels: TComboBox;
    editAnneeDepart: TCurrencyEdit;
    editAnneeArrivee: TCurrencyEdit;
    editAltitude: TCurrencyEdit;
    editDateDeclimag: TDateEdit;
    editDeclinaison: TCurrencyEdit;
    editExpression: TEdit;
    editLatitude: TCurrencyEdit;
    editLongitude: TCurrencyEdit;
    editX_Cible1JPC: TCurrencyEdit;
    editX_Source1JPC: TCurrencyEdit;
    editY_Cible1JPC: TCurrencyEdit;
    editY_Source1JPC: TCurrencyEdit;
    GroupBox1: TGroupBox;
    grbxConversionsIsolees: TGroupBox;
    grbxConversionsEnRafale: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lbSystCible: TLabel;
    lbSystSource: TLabel;
    lbValeursCalculees: TLabel;
    lbValeursEntree: TLabel;
    lsbListeFunctions: TListBox;
    Long: TLabel;
    Long1: TLabel;
    Long2: TLabel;
    Long3: TLabel;
    Long4: TLabel;
    PageControl1: TPageControl;
    grdDonnees: TStringGrid;
    grdDeclinaisons: TStringGrid;
    lbHintGrdConversions: TStaticText;
    tabShtDeclimag: TTabSheet;
    tabShtConvertisseur: TTabSheet;
    tabShtCalculatrice: TTabSheet;
    procedure btnCalculClick(Sender: TObject);
    procedure btnCalculDeclimagClick(Sender: TObject);
    procedure btnConversionJPCClick(Sender: TObject);
    procedure btnConversionSibertClick(Sender: TObject);
    procedure btnExportCartoClick(Sender: TObject);
    procedure btnTableDeclimagAnnuelleClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure btnCopierTableauClick(Sender: TObject);
    procedure btnConversionEnRafaleClick(Sender: TObject);
    procedure cmbColXCibleChange(Sender: TObject);
    procedure cmbColXSourceChange(Sender: TObject);
    procedure cmbColLabelsChange(Sender: TObject);
    procedure cmbColYCibleChange(Sender: TObject);
    procedure cmbColYSourceChange(Sender: TObject);
    procedure editExpressionEnter(Sender: TObject);
    procedure editExpressionKeyPress(Sender: TObject; var Key: char);
    procedure editX_SourceChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure grdDonneesClick(Sender: TObject);
    procedure Label3Click(Sender: TObject);
    procedure Label4Click(Sender: TObject);
    procedure lsbListeFunctionsDblClick(Sender: TObject);
  private
    { private declarations }
    FConversionUtils : TConversionSysteme;
    FConvertisseurActif: boolean;

    FCalculateurDeclimag: TCalculDeclinaisonMagnetique;
    FCalculateurDeclimagActif: boolean;
    // numéros de colonnes pour les conversions en rafale
    FColonneLabels : integer;
    FColonneXSource: integer;
    FColonneYSource: integer;
    FColonneXCible : integer;
    FColonneYCible : integer;

  public
    { public declarations }
    procedure SetDefaultExpression(const S: string);
    function  GetResultatCalcul: double;
    function  GetDeclimag: double;
    function  GetCoordonnees: TPoint2Df;
    procedure SetCoordonnees(const P: TPoint2Df);
    function InitialiseConvertisseur: boolean;
    function InitialiseCalculateurDeclimag: boolean;
    procedure ExportListePointsVersCarto(const FileName: string;
                                         const MC: TOutputFormatGIS;
                                         const FirstLineIsTitreCol: Boolean;
                                         const LabelsAutoIncrement: Boolean;
                                         const SystSource, SystCible: string;
                                         const NoColLabel: integer;
                                         const NoColXSource, NoColYSource: integer);

  end;

var
  frmCalculette: TfrmCalculette;

implementation


{$R *.lfm}

{ TfrmCalculette }


function TfrmCalculette.InitialiseConvertisseur: boolean;
begin
  Result := False;
  try
    FConversionUtils := TConversionSysteme.Create;
    FConvertisseurActif := FConversionUtils.Initialiser;
    Result := True;
  except
  end;
end;

function TfrmCalculette.InitialiseCalculateurDeclimag: boolean;
begin
  AfficherMessage('-- InitialiseCalculateurDeclimag');
  Result := False;

  FCalculateurDeclimagActif := false;
  FCalculateurDeclimag := TCalculDeclinaisonMagnetique.Create;
  try
    FCalculateurDeclimagActif := FCalculateurDeclimag.DemarrerDLL;
    FCalculateurDeclimagActif := FCalculateurDeclimag.Initialiser;
    Result := FCalculateurDeclimagActif;
  finally
  end;
  //*)

end;

procedure TfrmCalculette.ExportListePointsVersCarto(const FileName: string;
                                                    const MC: TOutputFormatGIS;
                                                    const FirstLineIsTitreCol: Boolean;
                                                    const LabelsAutoIncrement: Boolean;
                                                    const SystSource, SystCible: string;
                                                    const NoColLabel: integer;
                                                    const NoColXSource, NoColYSource: integer);
var
  EWE: TLigneCoordonnees;
  LigneDep: Integer;
  i, Nb: Integer;
  P1, P2: TProjUV;
  fp: TextFile;
  //****************************************************************************
  // en-tête de fichiers et déf des styles
  procedure WriteHeader(const M: TOutputFormatGIS);
  begin
    WriteLn(fp, FormatterVersionEncodageXML('1.0', 'ISO-8859-1')); //<?xml version="1.0" encoding="ISO-8859-1"?>');
    WriteLn(fp, Format('<gpx version="%.1f" creator="%s" xmlns:xsi="%s" xmlns="%s">',
                       [1.1, // version xml
                        rsGHTOPOEXENAME, // logiciel auteur
                        W3C_XML_SCHEMA_WEBSITE,
                        GPX_TOPOGRAPHIX_WEBSITE
                       ]));
  end;
  procedure WriteFooter(const M: TOutputFormatGIS);
  begin
    WriteLn(fp, ' </gpx>');
  end;
  procedure WritePoint(const M: TOutputFormatGIS; const PT : TLigneCoordonnees);
  begin
    WriteLn(fp, Format('     <wpt lat="%.15n" lon="%.15n">', [PT.XCible, PT.YCible]));
    WriteLn(fp, Format('       <name>%s</name>', [PT.Etiquette]));
    WriteLn(fp,        '     </wpt>');
  end;

  //****************************************************************************
begin
  AfficherMessage(Format('%s.ExportListePointsKML: %s: %s - %s: %d %d %d',
                         [ClassName,
                          FileName,
                          IIF(FirstLineIsTitreCol, 'Avec Titres', 'Sans Titres'),
                          IIF(LabelsAutoIncrement, 'Labels auto', 'Ya colonne labels'),
                          NoColLabel,
                          NoColXSource, NoColYSource
                         ]));
  AssignFile(fp, FileName);
  try
    Rewrite(fp);
    Nb := grdDonnees.RowCount;
    if (FirstLineIsTitreCol) then
    begin
      LigneDep := 1;
    end else
    begin
      LigneDep := 0;
    end;
    WriteHeader(MC); // entête
    for i := LigneDep to Nb - 1 do
    begin
      if (LabelsAutoIncrement) then EWE.Etiquette := Format('Label%d', [i])
                               else EWE.Etiquette := Trim(grdDonnees.Cells[NoColLabel, i]);
      //try
        // pas de strToFloatDef ici: on jette une exception direct
        P1.U := StrToFloat(grdDonnees.Cells[NoColXSource, i]);
        P1.V := StrToFloat(grdDonnees.Cells[NoColYSource, i]);
        P2 := FConversionUtils.ConversionSyst1ToSyst2(SystSource, SystCible, P1);

        EWE.XSource := P1.U;
        EWE.YSource := P1.V;
        EWE.XCible  := P2.U;
        EWE.YCible  := P2.V;

        // traitement à la volée
        AfficherMessage(Format('%d - %s - %.2f, %.2f > %.8f, %.8f', [i, EWE.Etiquette,
                                                                        EWE.XSource, EWE.YSource, EWE.XCible, EWE.YCible]));
        WritePoint(MC, EWE);
      //except
      //end;
    end;
    WriteFooter(MC);

  finally
    CloseFile(fp);
  end;
end;



procedure TfrmCalculette.FormShow(Sender: TObject);
  {$IFDEF USE_CONVERTISSEUR_EPSG}
  procedure RemplirCombos(const Cmb: TComboBox; const Idx: integer);
  var
    i: integer;
    EWE: TParametresEPSG;
  begin
    with Cmb do
    begin
      Clear;
      for i := 0 to FConversionUtils.GetNbreGrilles -1 do
      begin
        EWE := FConversionUtils.GetGeoGrid(i);
        Cmb.Items.Add(Format('EPSG:%d - %s', [EWE.CodeEPSG, EWE.Comments]));
      end;
      cmb.ItemIndex := Idx;
    end;
  end;


  {$ENDIF}
  {$IFDEF USE_CONVERTISSEUR_ESIBERT}
  procedure RemplirCombos(const Cmb: TComboBox; const Idx: integer);
  var
    i: integer;
    EWE: String;
  begin
    with Cmb do
    begin
      Clear;
      for i := 0 to FConversionUtils.GetNbSystemes - 1 do
      begin

        EWE := FConversionUtils.GetNomSysteme(i);
        AfficherMessage(EWE);
        Cmb.Items.Add(EWE);
      end;
      cmb.ItemIndex := Idx;
    end;
  end;
  {$ENDIF}
  {$IFDEF USE_CONVERTISSEUR_JPC}
  procedure RemplirCombos(const Cmb: TComboBox; const Idx: integer);
  var
    i: integer;
    EWE: String;
  begin
    with Cmb do
    begin
      Clear;
      for i := 0 to FConversionUtils.GetNbSystemes - 1 do
      begin

        EWE := FConversionUtils.GetNomSysteme(i);
        AfficherMessage(EWE);
        Cmb.Items.Add(EWE);
      end;
      cmb.ItemIndex := Idx;
    end;
  end;
  {$ENDIF}



  procedure RemplirListeFonctions;
    procedure QAdd(const S: string);
    begin
      lsbListeFunctions.Items.Add(S);
    end;
  begin
    lsbListeFunctions.Sorted := True;
    lsbListeFunctions.Clear;
    QAdd('FRAC'); //PFrac);
    QAdd('ENT'); //PINT);
    QAdd('ABS'); //PABS);
    QAdd('HASARD'); //PRANDOM);
    QAdd('ARRONDI'); //PROUND);
    QAdd('RACINE'); //PSQRT);
    QAdd('CARRE'); //PSQR);
    QAdd('SIN'); //PSIN);
    QAdd('COS'); //PCOS);
    QAdd('TAN'); //PTAN);
    QAdd('ARCSIN'); //PARCSIN);
    QAdd('ARCCOS'); //PARCCOS);
    QAdd('ARCTAN'); //PARCTAN);

    QAdd('ARGSH'); //PARGSH);
    QAdd('ARGCH'); //PARGCH);
    QAdd('ARGTH'); //PARGTH);
    QAdd('EXP'); //PEXP);
    QAdd('LN'); //PLN);
    QAdd('LOG'); //PLOG);
    QAdd('SINH'); //PSH);
    QAdd('COSH'); //PCH);
    QAdd('TANH'); //PTH);
  end;
  procedure ClearComboColonnes(const cmb: TComboBox; const Idx: integer);
  var
    q: Integer;
  begin
    cmb.Clear;
    for q := 0 to 4 do cmb.Items.add(format('Colonne_%d', [q]));
    cmb.ItemIndex := Idx;
  end;
var
  i: Integer;
begin
  self.Caption         := AnsiToUtf8(rsDLG_CALC_TITLE);
  Label1.Caption       := AnsiToUtf8(rsDLG_CALC_EXPR);
  //btnCalcul.Caption    := rsDLG_CALC_DOCALC;
  tabShtCalculatrice.Caption:= AnsiToUtf8(rsDLG_CALC_TAB_CAL);
  lbSystSource.Caption := AnsiToUtf8(rsDLG_CALC_LB_SYST_SOURCE);
  lbSystCible.Caption  := AnsiToUtf8(rsDLG_CALC_LB_SYST_CIBLE);
  tabShtConvertisseur.Caption := AnsiToUtf8(rsDLG_CALC_CDR_CONVERT);
  tabShtConvertisseur.Enabled := InitialiseConvertisseur();
  tabShtDeclimag.Caption      := AnsiToUtf8(rsDLG_CALC_CDR_DECLIMAG);
  tabShtDeclimag.Enabled      := InitialiseCalculateurDeclimag();
  RemplirCombos(cmbSystSourceJPC, 0);
  RemplirCombos(cmbSystCibleJPC, 0);
  RemplirListeFonctions();
  editDateDeclimag.Date := Now();
  PageControl1.ActivePageIndex := 0;

  // grille de conversion en rafale
  // la grille est éditable et on peut l'utiliser pour des conversions
  grdDonnees.Options := grdDonnees.Options + [goColSizing, goEditing];
  grdDonnees.ColCount :=  5;
  grdDonnees.RowCount := 20;
  grdDonnees.Cells[0, 0] := 'Etiquette';
  grdDonnees.Cells[1, 0] := 'X source';
  grdDonnees.Cells[2, 0] := 'Y source';
  grdDonnees.Cells[3, 0] := 'X cible';
  grdDonnees.Cells[4, 0] := 'Y cible';
  for i := 0 to grdDonnees.ColCount - 1 do grdDonnees.ColWidths[i] := 120;
  for i := 1 to grdDonnees.RowCount - 1 do grdDonnees.Cells[0,i] := IntToStr(i);

  FColonneLabels := 0;
  FColonneXSource:= 1;
  FColonneYSource:= 2;
  FColonneXCible := 3;
  FColonneYCible := 4;
  ClearComboColonnes(cmbColLabels, FColonneLabels);
  ClearComboColonnes(cmbColXSource, FColonneXSource);
  ClearComboColonnes(cmbColYSource, FColonneYSource);
  ClearComboColonnes(cmbColXCible , FColonneXCible);
  ClearComboColonnes(cmbColYCible , FColonneYCible);
   // aide pour la grille de conversion
  lbHintGrdConversions.Caption := AnsiToUtf8(rsDLG_CALC_HINT_GRD_CONVERSIONS);
end;

procedure TfrmCalculette.grdDonneesClick(Sender: TObject);
begin

end;

procedure TfrmCalculette.Label3Click(Sender: TObject);
begin

end;

procedure TfrmCalculette.Label4Click(Sender: TObject);
begin

end;

procedure TfrmCalculette.lsbListeFunctionsDblClick(Sender: TObject);
var
  NomFunc: String;
begin
  if (lsbListeFunctions.Count > 0) then
  begin
    NomFunc := Trim(lsbListeFunctions.Items[lsbListeFunctions.ItemIndex]);
    editExpression.Text := editExpression.Text + ' ' + NomFunc + '(';
    editExpression.SetFocus;
  end;
end;

procedure TfrmCalculette.SetDefaultExpression(const S: string);
begin
  editExpression.Text := Trim(S);
end;

function TfrmCalculette.GetResultatCalcul: double;
begin
  Result := StrToFloatDef(trim(editExpression.Text), 0.00);
end;

function TfrmCalculette.GetDeclimag: double;
begin
  Result := editDeclinaison.Value;
end;

function TfrmCalculette.GetCoordonnees: TPoint2Df;
begin
  Result.X := editX_Cible1JPC.Value;
  Result.Y := editY_Cible1JPC.Value;
end;

procedure TfrmCalculette.SetCoordonnees(const P: TPoint2Df);
begin
  editX_Source1JPC.Value := P.X;
  editY_Source1JPC.Value := P.Y;
end;

procedure TfrmCalculette.editExpressionEnter(Sender: TObject);
begin
  with editExpression do begin
    //SelStart:=0;
    //SelLength:=Length(Text);
  end;
end;

procedure TfrmCalculette.editExpressionKeyPress(Sender: TObject; var Key: char);
begin
  if (key = #13) then btnCalculClick(self);
end;

procedure TfrmCalculette.editX_SourceChange(Sender: TObject);
begin

end;

// Le calculateur de déclinaisons n'est pas prêt
procedure TfrmCalculette.FormDestroy(Sender: TObject);
begin

  try
    FConversionUtils.Finaliser;
    FCalculateurDeclimag.Finaliser;
    FCalculateurDeclimag.StopperDLL;
  finally
    FConversionUtils.Free;
    FCalculateurDeclimag.Free;
  end;
  Inherited;
end;


procedure TfrmCalculette.btnCalculClick(Sender: TObject);
var
  Err: Boolean;
  S  : string;
  R  : double;
begin
  Err:=False;
  S:=Trim(editExpression.Text);
  R:=0.00;
  R:=EvalExpr(S, Err);
  editExpression.Text := IIF(Err, 'ERR',
                             FloatToStr(R));
end;

procedure TfrmCalculette.btnCalculDeclimagClick(Sender: TObject);
begin

  if (FCalculateurDeclimagActif) then
    editDeclinaison.Value := FCalculateurDeclimag.CalculerDeclimag(editLongitude.Value,
                                                                   editLatitude.Value,
                                                                   editAltitude.Value,
                                                                   editDateDeclimag.Date);
  //*)
end;






procedure TfrmCalculette.Button1Click(Sender: TObject);
var
  LesLignes: TStringList;
  ClipBoard: TClipboard;
  EWE: String;
  P: SizeInt;
  Lin: String;
  i: Integer;
  ValeursColonnes: TStringArray;
  j: Integer;
  R: Integer;
  procedure RemplirComboColonnes(const Cmb: TComboBox; const Idx: integer);
  var
    qc: Integer;
    blaireau: String;
  begin
    Cmb.Clear;
    for qc := 0 to grdDonnees.ColCount - 1 do
    begin
      blaireau := grdDonnees.Cells[qc, 0];
      cmb.Items.Add(blaireau);
    end;
    cmb.ItemIndex := Idx;
  end;
begin
  if (Not GRDCollerDepuisClipBoard(grdDonnees, chkPremLigneIsTitres.Checked)) then Exit;
  // A partir d'ici, on n'a plus besoin du presse-papier ni de la liste provisoire
  // On travaille directement sur la grille
  // Complètement des noms de colonnes pour titres de rubriques vides
  for j := 1 to grdDonnees.ColCount - 1 do
  begin
    if (Trim(grdDonnees.Cells[j, 0]) = '') then
       grdDonnees.Cells[j, 0] := Format('Colonne%d', [j]);
  end;
  // garnir les combobox
  RemplirComboColonnes(cmbColLabels , FColonneLabels);
  RemplirComboColonnes(cmbColXSource, FColonneXSource);
  RemplirComboColonnes(cmbColYSource, FColonneYSource);
  RemplirComboColonnes(cmbColXCible , FColonneXCible);
  RemplirComboColonnes(cmbColYCible , FColonneYCible);
end;

procedure TfrmCalculette.Button2Click(Sender: TObject);
begin
  GRDCCopierUnTableau(grdDeclinaisons);
end;

procedure TfrmCalculette.btnCopierTableauClick(Sender: TObject);
begin
  GRDCCopierUnTableau(grdDonnees);
end;




procedure TfrmCalculette.btnConversionJPCClick(Sender: TObject);
var
  P1, P2: TProjUV;
  {$IFDEF USE_CONVERTISSEUR_EPSG}
    SystSource: String;
    SystCible: String;

  {$ENDIF}
  {$IFDEF USE_CONVERTISSEUR_ESIBERT}
    SystSource: String;
    SystCible: String;
  {$ENDIF}
  {$IFDEF USE_CONVERTISSEUR_JPC}
    SystSource: String;
    SystCible: String;
  {$ENDIF}
begin
    // si syst source = syst cible --> []
  if (cmbSystSourceJPC.ItemIndex = cmbSystCibleJPC.ItemIndex) then Exit;

  P1.U := editX_Source1JPC.Value;
  P1.V := editY_Source1JPC.Value;

  SystSource := cmbSystSourceJPC.Items[cmbSystSourceJPC.ItemIndex];
  SystCible  := cmbSystCibleJPC.Items[cmbSystCibleJPC.ItemIndex];

  P2 := FConversionUtils.ConversionSyst1ToSyst2(SystSource, SystCible, P1);
  editX_Cible1JPC.Value := P2.U;
  editY_Cible1JPC.Value := P2.V;
  // pour les cases Long et Lat du calcul de déclinaison

  {$IFDEF USE_CONVERTISSEUR_EPSG}
    P2 := FConversionUtils.ConversionSyst1ToSyst2(SystSource, '4326', P1);
  {$ENDIF}
  {$IFDEF USE_CONVERTISSEUR_ESIBERT}
    P2 := FConversionUtils.ConversionSyst1ToSyst2(SystSource, 'WGS84', P1);
  {$ENDIF}
  editLongitude.Value:= P2.U;
  editLatitude.Value := P2.V;
  //*)

end;

procedure TfrmCalculette.btnConversionSibertClick(Sender: TObject);
begin

end;

procedure TfrmCalculette.btnExportCartoClick(Sender: TObject);
begin
  if (cmbSystSourceJPC.ItemIndex = cmbSystCibleJPC.ItemIndex) then
  begin
    ShowMessage('Systèmes de coordonnées source et cible identiques');
    exit;
  end;
  with TSaveDialog.Create(Application) do
  try
    begin
      Options    := Options + [ofOverwritePrompt];
      InitialDir := ExtractFilePath(ParamStr(0));
      DefaultExt := '.gpx';
      Filter     := 'Fichier GPX (*.gpx)|*.gpx';
      if (Execute) then
      begin
        ExportListePointsVersCarto(FileName,
                                   gisGPX,
                                   chkPremLigneIsTitres.Checked,
                                   chkAutoIncrement.Checked,
                                   cmbSystSourceJPC.Items[cmbSystSourceJPC.ItemIndex],
                                   cmbSystCibleJPC.Items[cmbSystCibleJPC.ItemIndex],
                                   FColonneLabels,
                                   FColonneXSource, FColonneYSource)
      end;
    end;
  finally
    Free;
  end;
end;

procedure TfrmCalculette.btnTableDeclimagAnnuelleClick(Sender: TObject);
var
  NbValeurs: Integer;
  AnneeCourante: LongInt;
  i: Integer;
  EWE: TDateTime;
  Decl: Double;
begin
  if (not FCalculateurDeclimagActif) then Exit;
  try
    i := editAnneeArrivee.AsInteger - editAnneeDepart.AsInteger;
    NbValeurs := i + 1;
    AnneeCourante := editAnneeDepart.AsInteger;
    if (i < 0) then
    begin
       ShowMessage('L''année de fin doit être supérieure à l''année de début');
       Exit;
    end;
    with grdDeclinaisons do
    begin
      ColCount     := 2;
      RowCount     := 1 + NbValeurs;
      Cells[0, 0]  := 'Année';
      Cells[1, 0]  := 'Déclinaison';
      ColWidths[0] := 70;
      ColWidths[1] := 100;
      for i := 1 to NbValeurs do
      begin
        EWE := EncodeDate(AnneeCourante, 1, 1);
        Cells[0,i] := Format('%d', [AnneeCourante]);
        Decl := FCalculateurDeclimag.CalculerDeclimag(editLongitude.Value,
                                                     editLatitude.Value,
                                                     editAltitude.Value,
                                                     EWE);
        Cells[1, i] := Format('%.4f', [Decl]);
        Inc(AnneeCourante, 1);
      end;
    end;

  except

  end;
end;

procedure TfrmCalculette.btnConversionEnRafaleClick(Sender: TObject);
var
  qSystSRC: String;
  qSystCIB: String;
  P1, P2: TProjUV;
  LigneDep: Integer;
  i: Integer;
begin
  if (cmbSystSourceJPC.ItemIndex = cmbSystCibleJPC.ItemIndex) then
  begin
    ShowMessageFmt('Les systèmes de coordonnées source et cible sont identiques', [qSystSRC, qSystCIB]);
    Exit;
  end;
  qSystSRC := cmbSystSourceJPC.Items[cmbSystSourceJPC.ItemIndex];
  qSystCIB := cmbSystCibleJPC.Items[cmbSystCibleJPC.ItemIndex];

  // récupération des index de colonnes
  FColonneXSource := cmbColXSource.ItemIndex;
  FColonneYSource := cmbColYSource.ItemIndex;
  FColonneXCible  := cmbColXCible.ItemIndex;
  FColonneYCible  := cmbColYCible.ItemIndex;


  if ( (FColonneXSource = FColonneXCible) OR // colonne sources et destination identiques
       (FColonneYSource = FColonneYCible) OR // colonne sources et destination identiques
       (FColonneXSource = FColonneYCible) OR
       (FColonneYSource = FColonneXCible) OR
       (FColonneXSource = FColonneYSource) OR // colonne XY sources identiques
       (FColonneXCible  = FColonneYCible)     // colonne XY sources identiques
     ) then
  begin
    ShowMessageFmt('Les colonnes %d, %d, %d, %d se marchent dessus', [FColonneXSource, FColonneXCible,FColonneYSource,FColonneYCible]);
    Exit;
  end;
  if (chkPremLigneIsTitres.Checked) then LigneDep := 1 else LigneDep := 0;
  for i := LigneDep to grdDonnees.RowCount - 1 do
  begin
    try
      // pas de strToFloatDef ici: on jette une exception direct
      P1.U := StrToFloat(grdDonnees.Cells[FColonneXSource, i]);
      P1.V := StrToFloat(grdDonnees.Cells[FColonneYSource, i]);
      P2 := FConversionUtils.ConversionSyst1ToSyst2(qSystSRC, qSystCIB, P1);
      grdDonnees.Cells[FColonneXCible, i] := FloatToStr(P2.U);
      grdDonnees.Cells[FColonneYCible, i] := FloatToStr(P2.V);

    except
      grdDonnees.Cells[FColonneXCible, i] := 'Err. X';
      grdDonnees.Cells[FColonneYCible, i] := 'Err. Y';
    end;
  end;
end;

procedure TfrmCalculette.cmbColXCibleChange(Sender: TObject);
begin
  FColonneXCible := cmbColXCible.ItemIndex;
end;

procedure TfrmCalculette.cmbColXSourceChange(Sender: TObject);
begin
  FColonneXSource := cmbColXSource.ItemIndex;
end;

procedure TfrmCalculette.cmbColLabelsChange(Sender: TObject);
begin
  FColonneLabels := cmbColLabels.ItemIndex;
end;

procedure TfrmCalculette.cmbColYCibleChange(Sender: TObject);
begin
  FColonneYCible := cmbColYCible.ItemIndex;
end;

procedure TfrmCalculette.cmbColYSourceChange(Sender: TObject);
begin
  FColonneYSource := cmbColYSource.ItemIndex;
end;



end.

