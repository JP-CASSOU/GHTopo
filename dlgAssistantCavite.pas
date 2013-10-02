unit dlgAssistantCavite;
// Date: 19/04/2012
// Statut: Remplacé par un PageControl
//         Prêt à recevoir les cadres Expé, Code, Entrées
//         Cadre "Série" brêlé
// 07/09/2012: Opérationnel.
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
  Classes,
  StructuresDonnees,
  Common,
  ToporobotClasses2012,
  {$IFDEF MSWINDOWS}
    {$IFDEF USE_CONVERTISSEUR_EPSG}
      ConversionsCoordonneesEPSG, // API Conversapi
    {$ENDIF}
    {$IFDEF USE_CONVERTISSEUR_ESIBERT}
      ConversionsCoordonneesESibert, // API ESIBERT
    {$ENDIF}
    {$IFDEF USE_CONVERTISSEUR_JPC}
      ConvertisseurJPC, // API JPC
    {$ENDIF}
  {$ENDIF}
  {$IFDEF LINUX}
    ConversionsCoordonneesEPSGLinux,
  {$ENDIF}

  ObjetSerie,
  CadreSerieIndependant,CadreEntrance,
  CallDialogsStdVersion,
  CadreCode,
  CadreExpe,
  SysUtils, FileUtil,
  Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  ExtendedNotebook, curredit;

type

  { TdlgAssistantNouveau }

  TdlgAssistantNouveau = class(TForm)
    btnCancel: TButton;
    btnSaveAs: TButton;
    btnConvertisseur: TButton;
    CdrCode1: TCdrCode;
    CdrExpe1: TCdrExpe;
    CdrSerieIndependant1: TCdrSerieIndependant;
    cmbSystemesGeographiques: TComboBox;
    Edit1: TEdit;
    editCommentaireEntree: TEdit;
    editCommentaireEtude: TMemo;
    editCoordX: TCurrencyEdit;
    editCoordY: TCurrencyEdit;
    editCoordZ: TCurrencyEdit;
    editNomEntreePrincipale: TEdit;
    editNomEtude: TEdit;
    editRefSerie: TCurrencyEdit;
    editRefStation: TCurrencyEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    grbxCoordonnees: TGroupBox;
    grbxStationInitiale: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lbCommentaireEntree: TLabel;
    lbPoint: TLabel;
    lbSerie: TLabel;
    lbCommentaireEtude: TLabel;
    lbNomEntreePrincipale: TLabel;
    lbNomEtude: TLabel;
    lbSystemesGeographiques: TLabel;
    PageControl1: TPageControl;
    General: TTabSheet;
    TabShtSerie: TTabSheet;
    TabShtCode: TTabSheet;
    TabShtExpe: TTabSheet;
    procedure btnCancelClick(Sender: TObject);
    procedure btnSaveAsClick(Sender: TObject);
    procedure btnConvertisseurClick(Sender: TObject);
    procedure cmbSystemesGeographiquesChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FFichierTAB: string;
    FNewDoc: TToporobotStructure2012;
    FDocCreated: boolean;
    FConvertisseurEPSG: TConversionSysteme;
    FConvertisseurReady: boolean;

    function  CheckModifs: boolean;
    procedure ValidateModifs;


    { private declarations }
  public
    { public declarations }
    function InitNewDocument: boolean;
    function InitConversionEPSG: boolean;
    function DocCreated: Boolean;
    function GetXTBFileName: string;
  end;

var
  dlgAssistantNouveau: TdlgAssistantNouveau;

implementation

{$R *.lfm}

procedure TdlgAssistantNouveau.FormShow(Sender: TObject);
var
  QS: TObjSerie;
  ii: integer;
  WU: String;
begin
  FDocCreated := False;
  self.Caption         := AnsiToUtf8(rsASSIST_TITLE);
  TabShtSerie.Caption  := AnsiToUtf8(rsASSIST_BNSERIE);
  TabShtCode.Caption   := AnsiToUtf8(rsASSIST_BNCODES);
  TabShtExpe.Caption   := AnsiToUtf8(rsASSIST_BNEXPES);
  btnConvertisseur.Caption := AnsiToUtf8(rsASSIST_LBCONVERTISSEUR);

  lbNomEtude.Caption                := AnsiToUtf8(rsASSIST_LBNOMETUDE);
  lbCommentaireEtude.Caption        := AnsiToUtf8(rsASSIST_LBOBSETUDE);
  lbNomEntreePrincipale.Caption     := AnsiToUtf8(rsASSIST_LBNOMENTREE);
  grbxCoordonnees.Caption           := AnsiToUtf8(rsASSIST_LBOBSENTREE);
  grbxStationInitiale.Caption       := AnsiToUtf8(rsASSIST_LBREFSTATION);
  lbCommentaireEntree.Caption       := AnsiToUtf8(rsASSIST_LBCOMMENTAIRE);
  lbSerie.Caption                   := AnsiToUtf8(rsASSIST_LBREFSERIE);
  lbPoint.Caption                   := AnsiToUtf8(rsASSIST_LBREFPOINT);
  lbSystemesGeographiques.Caption   := AnsiToUtf8(rsASSIST_LBSYSTGEO);

  btnSaveAs.Caption    := AnsiToUtf8(rsASSIST_BNSAVE);
  btnCancel.Caption    := AnsiToUtf8(rsASSIST_BNCANCEL);

  // série
  QS := FNewDoc.GetSerie(1);
  CdrSerieIndependant1.Initialise(FNewDoc, QS);
  // entrée
  editNomEntreePrincipale.Text := 'Entree principale';
  editCoordX.Value             := 0.00;
  editCoordY.Value             := 0.00;
  editCoordZ.Value             := 0.00;
  editRefSerie.AsInteger       := 1;
  editRefStation.AsInteger     := 0;


  // code
  CdrCode1.SetCode(FNewDoc.GetCode(1), True);
  // expé
  CdrExpe1.SetExpe(FNewDoc.GetExpe(1), True);
  // convertisseur
  if (FConvertisseurReady) then
  begin
    // TODO: Revoir cette zone; ajouter une méthode à FConvertisseurEPSG

    for ii := 0 to FConvertisseurEPSG.GetNbSystemes - 1 do
    begin
      WU := FConvertisseurEPSG.GetNomSysteme(ii);
      cmbSystemesGeographiques.Items.Add(WU);
    end;
    ii := 0; //FConvertisseurEPSG.GetIndexByCodeEPSG(DEFAULT_CODE_EPSG);
    cmbSystemesGeographiques.ItemIndex := ii;
    //*)
  end
  else
  begin
    ShowMessage(AnsiToUtf8(rsMSG_PROJ4S_FAIL));
  end;

  //SetDialog(1);
end;

function TdlgAssistantNouveau.CheckModifs: boolean;
begin
  Result := False;
  try
    Result := True;

  except

  end;
end;


procedure TdlgAssistantNouveau.ValidateModifs;
var
  E: TEntrance;
  Serie: TObjSerie;
begin
   with FNewDoc do begin
    // général
    SetNomEtude(Utf8ToAnsi(Trim(editNomEtude.Text)));
    SetCommentairesEtude(Utf8ToAnsi(Trim(editCommentaireEtude.Text)));
    SetSystemeCoordonnees(cmbSystemesGeographiques.Items[cmbSystemesGeographiques.ItemIndex]);

    // entrée principale
    E.eNomEntree := Utf8ToAnsi(Trim(editNomEntreePrincipale.Text));
    E.eObserv    := Utf8ToAnsi(Trim(editCommentaireEntree.Text));
    E.eXEntree   := editCoordX.Value;
    E.eYEntree   := editCoordY.Value;
    E.eZEntree   := editCoordZ.Value;

    E.eRefSer    := editRefSerie.AsInteger;
    E.eRefSt     := editRefStation.AsInteger;

    SetDefaultCoords(E.eXEntree, E.eYEntree, E.eZEntree);
    SetRefSeriePoint(E.eRefSer, E.eRefSt);


    // ajouter l'entrée dans la liste
    AddEntree(E);

    //end;
    //===============================
    PutExpe(1, CdrExpe1.GetExpeFromForm);
    PutCode(1, CdrCode1.GetCodeFromForm);

    if (CdrSerieIndependant1.ImplementerModifs) then
    begin
      Serie := CdrSerieIndependant1.GetCurrentSerie;
      FNewDoc.PutSerie(1, Serie);
    end;

    //CdrSerie1.ImplementerModifs;
    //CdrSerie1.RefreshTableaux;
  end;
 // E:=
end;

procedure TdlgAssistantNouveau.FormCreate(Sender: TObject);
begin
  Inherited;
  FConvertisseurEPSG := TConversionSysteme.Create;
  try
    FConvertisseurReady := FConvertisseurEPSG.Initialiser;
  finally
  end;
end;

procedure TdlgAssistantNouveau.btnSaveAsClick(Sender: TObject);
begin
  with TSaveDialog.Create(Application) do
  begin
    try
      Options:= Options + [ofOverwritePrompt];
      InitialDir := ExtractFilePath(ParamStr(0));
      Filter     := rsGHTOPO_FILE_FILTER;
      FileName   := EnsureMakeFilename(Utf8ToAnsi(editNomEtude.Text));
      DefaultExt := '.xtb';
      if (Execute) then
      begin
        if (not CheckModifs) then Exit;
        ValidateModifs;
        FNewDoc.SaveToFile(FileName, mtabEXTENDEDTAB, tfWINDOWS);
        FDocCreated := True;
        self.Close;
      end;
    finally
      Free;
    end;
  end;
end;

procedure TdlgAssistantNouveau.btnConvertisseurClick(Sender: TObject);
var
  Pt: TPoint2Df;
  EWE: Double;
  ResultatCalcul: double;
  Declimag: double;
begin
  ResultatCalcul  := 0.00;
  PT.X := editCoordX.Value;
  PT.Y := editCoordY.Value;
  if (CallCalculette('', PT, ResultatCalcul, Declimag)) then
  begin
    editCoordX.Value := PT.X;
    editCoordY.Value := PT.Y;
  end;

end;

procedure TdlgAssistantNouveau.btnCancelClick(Sender: TObject);
begin
  self.Close;
end;

procedure TdlgAssistantNouveau.cmbSystemesGeographiquesChange(Sender: TObject);
begin

end;

procedure TdlgAssistantNouveau.FormDestroy(Sender: TObject);
begin
  try
    FConvertisseurEPSG.Finaliser;
  finally
    FConvertisseurEPSG.Free;
  end;
  //Inherited;
end;



function TdlgAssistantNouveau.InitNewDocument: boolean;
var
  Expe: TExpe;
  Code : TCode;
  Serie: TObjSerie;
  Visee: TUneVisee;
  J, M, A: word;
begin
  Result := False;
  try
    FNewDoc:=TToporobotStructure2012.Create;
    with FNewDoc do begin
      ReInitialiser(True);
      SetDatabaseName('Topographie1.Tab');
      SetNomEtude('Nouvelle étude');
      SetCommentairesEtude('');
      AfficherMessage(' -- 1');
      // Première Expé:
      with Expe do begin
        IDExpe := 1;
        SafeDecodeDate(Now, A,M,J);
        //if A > 1900+ANNEE_PIVOT then A:=A-1900 else A:=A-2000;
        JourExpe:=J;
        MoisExpe:=M;
        AnneeExpe:=A;

        Speleometre :='';
        Speleographe:='';
        ModeDecl    :=0;
        Declinaison :=0.00;
        Inclinaison :=0.00;
        Couleur     :=1;
        Commentaire :='';
      end;
      AddExpe(Expe);
      AfficherMessage(' -- 2');
      // Premier Code
      with Code do begin
        IDCode      :=1;
        GradAz      :=360.00;
        GradInc     :=360.00;
        PsiL        :=0.01;
        PsiAz       :=1.00;
        PsiP        :=1.00;
        FactLong    :=1.00;
        AngLimite   :=0.00;
        Commentaire :='';
      end;
      AddCode(Code);
      // Première Série
      AfficherMessage(' -- 3');
      Serie := TObjSerie.Create;
      with Serie do begin
        SetNoReseau(0);
        SetIndexSerie(1);  // Numéro réel de la série
        SetNomSerie('Serie principale');
        SetSeriePtExtremites(1, 0, 1, 1);
        SetChanceObstacle(0, 0);
        AddVisee(EmptyVisee('Station 1/0'));
      end;
      AddSerie(Serie);
      AfficherMessage(' -- 4');
    end; //with FNewDoc do begin
    Result:=True;
  except
    ShowMessage('Erreur d''initialisation');
    FNewDoc.Free;
  end;
end;

function TdlgAssistantNouveau.InitConversionEPSG: boolean;
begin
  Result := False;
end;

function TdlgAssistantNouveau.DocCreated: Boolean;
begin
  Result := False;
end;
function TdlgAssistantNouveau.GetXTBFileName: string;
begin
  Result := FFichierTAB;
end;


end.

