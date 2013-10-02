unit CdrSerie;

{$mode delphi}{$H+}

interface

uses
  Common,
  StructuresDonnees,
  ToporobotClasses,
  Dialogs,
  CallDialogs,
  Classes, SysUtils, FileUtil, LResources, Forms, StdCtrls, ComCtrls;
const MAX_ARRAY_NB_PTS = 500;
type

  { TCadreSerie }

  TCadreSerie = class(TFrame)
    btnAddStation: TButton;
    Button13: TButton;
    Button14: TButton;
    chkLocked: TCheckBox;
    cmbTypeVisee: TComboBox;
    cmbObstacle: TComboBox;
    cmbChance: TComboBox;
    editCode: TEdit;
    editExpe: TEdit;
    editSerieObserv: TEdit;
    editRaideur: TEdit;
    editIDReseau: TEdit;
    editSerDep: TEdit;
    editStDep: TEdit;
    editSerArr: TEdit;
    editStArr: TEdit;
    editNomSerie: TEdit;
    editIDSerie: TEdit;
    editAzimut: TEdit;
    editHN: TEdit;
    editHZ: TEdit;
    editIDStation: TEdit;
    editLD: TEdit;
    editLG: TEdit;
    editLongueur: TEdit;
    editStObservations: TEdit;
    editPente: TEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lbCurrentSerie: TStaticText;
    PageControl1: TPageControl;
    lbColorSerie: TStaticText;
    lbNbVisees: TStaticText;
    sclStation: TScrollBar;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;

    procedure btnAddStationClick(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure Button13Click(Sender: TObject);
    procedure Button14Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure editCodeDblClick(Sender: TObject);
    procedure editExpeDblClick(Sender: TObject);
    procedure editIDReseauDblClick(Sender: TObject);
    procedure editIDSerieDblClick(Sender: TObject);
    procedure editIDStationDblClick(Sender: TObject);
    procedure editLongueurDblClick(Sender: TObject);
    procedure editAzimutDblClick(Sender: TObject);
    procedure editPenteDblClick(Sender: TObject);

    procedure editLGDblClick(Sender: TObject);
    procedure editLDDblClick(Sender: TObject);
    procedure editHZDblClick(Sender: TObject);
    procedure editHNDblClick(Sender: TObject);
    procedure sclStationChange(Sender: TObject);

  private
    FDocuTopo: TToporobotStructure;

    FCurrentNumeroSerie: integer;
    FCurrentIdxStation: integer;
    FCurrentNbStations: integer;
    FSerie:  TUneSerie;
    // tableau des stations
    FArrayPts: array[0..MAX_ARRAY_NB_PTS] of TUneVisee;

    procedure AttribuerExtremiteLibre(const S,P: integer);

    procedure PutSerieInForm(const QSerie: TUneSerie; const QNoSerie: integer);
    // Sauvegarde modifs formulaire et tableau
    function  ImplementerModifs: boolean;

    procedure PutStationInForm(const Vis: TUneVisee; const Idx: integer);
    function  GetStationFromForm: TUneVisee;

    function  QGetStation(const Idx: integer): TUneVisee;
    function  QSetStation(const V: TUneVisee; const Idx: integer): boolean;
    function  QAddStation(const V: TUneVisee): boolean;
    procedure SkipStation(const Offset: integer);
  public
    { Déclarations publiques}
    procedure SetDocuTopo(const MyDocuTopo: TToporobotStructure);
    procedure SetSerie(const QSerie: TUneSerie; const QNoSerie: integer);
    procedure RefreshTableaux;
end;
implementation

// attraper une station du tableau des points
function  TCadreSerie.QGetStation(const Idx: integer): TUneVisee;
var
  wu: integer;
begin
  wu := Idx;
  if High(FArrayPts) = 0 then Exit;
  if wu < 0 then wu := 0;
  if wu > High(FArrayPts) then wu := High(FArrayPts);
  Result := FArrayPts[wu];
end;
// affecter une station
function TCadreSerie.QSetStation(const V: TUneVisee; const Idx: integer): boolean;
begin
  Result := False;
  try
    FArrayPts[Idx] := V;
    Result := True;
  except
  end;
end;
// ajouter une station
function TCadreSerie.QAddStation(const V: TUneVisee): boolean;
var
  wu: integer;
begin
  Result := False;

  try
    // agrandir le tableau
    FCurrentNbStations := FCurrentNbStations + 1;
    wu := FCurrentNbStations - 1;
    FArrayPts[wu] := V;
    sclStation.Max      := wu;
    sclStation.Position := wu;
    Result := True;
  finally
  end;
end;

procedure TCadreSerie.SkipStation(const Offset: integer);
var
  St: TUneVisee;
  function WU(const N: integer): integer;
  var
    EWE: integer;
  begin
    EWE := FCurrentNbStations - 1;
    if (N < -9999) then begin Result:=0; Exit; end;
    if (N >  9999) then begin Result:=EWE; Exit; end;

    Result := FCurrentIdxStation + Offset;

    if (Result > EWE) then begin Result := EWE; Exit; end;
    EWE := 0;
    if (Result < EWE) then begin Result := EWE; Exit; end;
  end;

begin
  // grandes valeurs => début ou fin
  FCurrentIdxStation := WU(Offset);
  ST := QGetStation(FCurrentIdxStation);
  PutStationInForm(St, FCurrentIdxStation);
end;

//***************
// calculer et attribuer extrémité libre
procedure TCadreSerie.AttribuerExtremiteLibre(const S,P: integer);
var
  b: boolean;
  i: integer;
  wu: integer;
begin
  wu := 0;
  if chkLocked.Checked then Exit;
  //if (S>0) or (P>0) then Exit;
  for i := 0 to High(FArrayPts) do begin
    wu := i;
    if (FArrayPts[i].Code = -1) and
       (FArrayPts[i].Expe = -1)
    then Break;
  end;

  editSerArr.Text  := Format('%d',[editIDSerie.Text]);
  editStArr.Text   := Format('%d',[wu-1]);
end;
procedure TCadreSerie.SetDocuTopo(const MyDocuTopo: TToporobotStructure);
begin
  FDocuTopo := MyDocuTopo;
  FCurrentNumeroSerie := 0;
  FCurrentIdxStation  := 0;
end;
procedure TCadreSerie.SetSerie(const QSerie: TUneSerie; const QNoSerie: integer);
begin

  FCurrentNumeroSerie := QNoSerie;

  FSerie       := QSerie;
  FCurrentNbStations  := QSerie.NbPoints;
  //showmessage('SetSerie');
  PutSerieInForm(FSerie, 0);
  // tableau des points topo -> récupération des points topos
  RefreshTableaux();

end;
procedure TCadreSerie.RefreshTableaux;
var
  V: TUneVisee;
  i: integer;
begin
  //showmessagefmt('RefreshTableaux: %d (%d pts)',[FCurrentNumeroSerie, FCurrentNbStations]);
  // nettoyage du tableau des points topos  => codes et expés mis à -1
  for i:= 0 to High(FArrayPts) do begin
    FArrayPts[i].Code := -1;
    FArrayPts[i].Expe := -1;
  end;
  if FCurrentNumeroSerie = 0 then Exit;
  if FCurrentNbStations  = 0 then Exit;
  PageControl1.ActivePageIndex := 0;
  sclStation.Max        := FCurrentNbStations - 1;
  sclStation.Position   := 0;

  // remplissage du tableau
  for i:= 0 to FCurrentNbStations - 1 do begin
    V := FDocuTopo.GetStation(FCurrentNumeroSerie, i);
    FArrayPts[i] := V;
    //AfficherMessage(Format('Tab %d : %d %d %.2f %.2f %.2f',[i, FArrayPts[i].Code, FArrayPts[i].Expe, FArrayPts[i].Longueur, FArrayPts[i].Azimut, FArrayPts[i].Pente]));
  end;
  FCurrentIdxStation := 0;
  V := QGetStation(FCurrentIdxStation);
  PutStationInForm(V, FCurrentIdxStation);
end;

procedure TCadreSerie.PutSerieInForm(const QSerie: TUneSerie; const QNoSerie: integer);
begin

  editIDSerie.Text   := Format('%d',[QSerie.IndexSerie]);
  editIDReseau.Text  := Format('%d',[QSerie.Reseau]);
  editNomSerie.Text  := QSerie.NomSerie;
  editSerDep.Text    := IntToStr(QSerie.SerieDep);
  editStDep.Text     := IntToStr(QSerie.PtDep);
  editSerArr.Text    := IntToStr(QSerie.SerieArr);
  editStArr.Text     := IntToStr(QSerie.PtArr);
  cmbObstacle.ItemIndex := QSerie.Obstacles;
  cmbChance.ItemIndex   := QSerie.Chances;
  editSerieObserv.Text  := QSerie.Commentaires;
  lbCurrentSerie.Caption := Format('%d',[QSerie.IndexSerie]);
  lbColorSerie.Color     := QSerie.Couleur;
  FCurrentIdxStation := 0;

  PutStationInForm(QGetStation(FCurrentIdxStation), FCurrentIdxStation);
end;
// Sauvegarde modifs formulaire et tableau
// Ceci travaille sur la série COURANTE.
// Pour ajouter une série, il est nécessaire de la créer puis de se positionner
// dessus pour l'éditer
function TCadreSerie.ImplementerModifs: boolean;
var
  i, Q: integer;
  S: TUneSerie;
  b: boolean;
  V: TUneVisee;
  wu1, wu2: integer;
  // vérification des stations
  function CheckAStation(const AStation: TUneVisee): integer;
  var
    AMin, AMax: Double;
    CC: TCode;
    EE: TExpe;

  begin
    Result:=0;
    // code et expé
    //if Not IsInRange(AStation.Code, 0, FDocTopo.NbCodes)  then  ShowMessageFmt('Code invalide en station %d',[i]);
    //if Not IsInRange(AStation.Expe, 0, FDocTopo.NbExpes) then   ShowMessageFmt('Séance invalide en station %d',[i]);
    // longueur, ne doit pas dépasser 160 m
    if Not IsInRange(AStation.Longueur,0.00, 160.00) then
       ShowMessageFmt('Longueur hors limites en station %d',[i]);
    CC:=FDocuTopo.GetCode(AStation.Code);
    // azimuts
    AMin:=0.00;
    case Trunc(CC.GradAz) of
      359,360: AMax:=360.00; // directes degrés
      399,400: AMax:=400.00; // directes grades
      349,350: AMax:=360.00; // inverses degrés
      389,390: AMax:=400.00; // inverses grades
    end;
    if Not IsInRange(AStation.Azimut, AMin, AMax) then
       ShowMessageFmt('Azimut incorrect en station %d',[i]);
    // pentes
    case Trunc(CC.GradInc) of
      360: begin AMin:=-90.00 ; AMax:=  90.00; end; // zéro horizontal degré, directe
      400: begin AMin:=-100.00; AMax:= 100.00; end; // zéro horizontal grade, directe
      361: begin AMin:=  0.00;  AMax:= 180.00; end; // zéro zénithal degrés
      401: begin AMin:=  0.00;  AMax:= 200.00; end; // zéro zénithal grades
      359: begin AMin:=  0.00;  AMax:= 180.00; end; // zéro nadiral degrés
      399: begin AMin:=  0.00;  AMax:= 200.00; end; // zéro nadiral grades
      370: begin AMin:=-580.00; AMax:= 580.00; end; // pourcentage; +/- 80° max
      380: begin AMin:=-160.00; AMax:= 160.00; end; // dénivelés; maxi=long max visée
      350: begin AMin:=-90.00 ; AMax:=  90.00; end; // zéro horizontal degré, inverse
      390: begin AMin:=100.00 ; AMax:= 100.00; end; // zéro horizontal grade, inverse
    else
      begin AMin:=-100.00; AMax:= 100.00; end;
    end;
    if Not IsInRange(AStation.Pente, AMin, AMax) then
      ShowMessageFmt('Pente incorrecte en station %d',[i]);
    // largeurs et hauteurs
    if (Not IsInRange(AStation.LG, 0.00, 200.00)) or
       (Not IsInRange(AStation.LD, 0.00, 200.00)) or
       (Not IsInRange(AStation.HZ, 0.00, 400.00)) or
       (Not IsInRange(AStation.HN, 0.00, 400.00)) then
      ShowMessageFmt('Largeur ou hauteur incorrecte en station %d',[i]);

  end;

begin
  {$DEFINE ACTIVETRY}
  {$UNDEF ACTIVETRY}
  AfficherMessage(Format('%s.ImplementerModifs',[ClassName]));
  result:=False;

  // TODO: Revoir cette portion de code
  // vérifier si l'ID de la série est libre
  (*
  Q := editIndexSerie.AsInteger;
  for i := 1 to FDocTopo.NbSeries-1 do begin
    FSerie := FDocTopo.GetSerie(i);
    if (FSerie.IndexSerie = Q) and
       (i <> FNumeroSerie)
    then begin
      ShowMessageFmt(rsIDSERIE_EXISTS,[Q, i]);
      //editIndexSerie.AsInteger := Q
      Exit;
    end;
  end;
  //*)
  // calcul éventuel estrémités de série
  wu1 := StrToIntDef(Trim(editSerArr.Text), 0);
  wu2 := StrToIntDef(Trim(editStArr.Text) , 0);

  AttribuerExtremiteLibre(wu1, wu2);

  {$IFDEF ACTIVETRY}

  try
  {$ENDIF}
    //FSerie:=GetSerieFromForm;
    //sauvegarde du formulaire:
    {// TODO: A revoir
    with FSerie do begin
      Numero       :=FCurrentNumeroSerie;
      IndexSerie   :=editIndexSerie.AsInteger;
      NomSerie     :=editNomSerie.Text;
      SerieDep     :=editSerDep.AsInteger;
      SerieArr     :=editSerArr.AsInteger;
      PtDep        :=editPtDep.AsInteger;
      PtArr        :=editPtArr.AsInteger;
      Reseau       :=editNoReseau.AsInteger;
      Chances      :=cmbChance.ItemIndex;
      Obstacles    :=cmbObstacle.ItemIndex;
      Commentaires :=editCommentaire.Text;
      Raideur      :=editRaideur.Value;
      // Purger la liste des points topos
      for i:=0 to PointsTopo.Count-1 do
        Dispose(PointsTopo.Items[i]);
      PointsTopo.Clear;
    end;
    // analyser le tableur:
    // critère de sortie: Colonne 1 à 5 vides ou nulles

    with grdStations do begin
      AfficherMessage('  Checking values');
      // si les colonnes Long, Az et P sont vides,
      // la fin de la table des stations est supposée atteinte
      for i:=0 to RowCount-1 do begin
        b:=(i>0) and
            (Cells[NUM_COL_L,i]='') and
            (Cells[NUM_COL_A,i]='') and
            (Cells[NUM_COL_P,i]='');
        if b then
          Break;
        // analyse ligne
        V.NoVisee:=i;
        V.IDTerrainStation:=UpperCase(Trim(Cells[NUM_COL_IDTERRAIN,i]));

        V.TypeGalerie:=SetTypeGalerie(StrToIntDef(Cells[NUM_COL_TYPEGAL,i],-1));
        V.Code:=StrToIntDef(Cells[NUM_COL_CODE,i],0);
        V.Expe:=StrToIntDef(Cells[NUM_COL_EXPE,i],0);
        V.Longueur:=StrToFloatDef(Cells[NUM_COL_L,i],0);
        V.Azimut  :=StrToFloatDef(Cells[NUM_COL_A,i],0);
        V.Pente   :=StrToFloatDef(Cells[NUM_COL_P,i],0);
        V.LG:=StrToFloatDef(Cells[NUM_COL_LG,i],0);
        V.LD:=StrToFloatDef(Cells[NUM_COL_LD,i],0);
        V.HZ:=StrToFloatDef(Cells[NUM_COL_HZ,i],0);
        V.HN:=StrToFloatDef(Cells[NUM_COL_HN,i],0);
        V.Commentaires:=Cells[NUM_COL_OBS,i];

        //FDocumentToporobot.AddStation(FNumeroSerie,V);

        // vérifier les données
        // type de galerie
        (* désactivé provisoirement
        if Not (V.TypeGalerie in [tgENTRANCE, tgFOSSILE, tgVADOSE, tgENNOYABLE, tgSIPHON,
                   tgFIXPOINT, tgSURFACE, tgTUNNEL, tgMINE]) then
          ShowMessageFmt('Type de galerie %d inconnu en ligne %d',
                         [Ord(V.TypeGalerie),i]);
        //*)

        AfficherMessage(Format('   St: %d,  %d  %d - %.2f %.2f %.2f - %.2f %.2f %.2f %.2f  "%s"',
                               [V.NoVisee,V.Code,V.Expe,
                                V.Longueur,V.Azimut,V.Pente,
                                V.LG,V.LD,V.HZ,V.HN,
                                V.Commentaires]));
        // vérifier les données
        // Q:= CheckAStation(V);

        //FSerie.PointsTopo.AddViseeTopo(V);
        FDocTopo.AddStation(FNumeroSerie, V);


      end;
    end;
    // si le petit cadenas est déverrouillé,
    // le numéro de point d'arrivée est le numéro de station max.
    //if Not(FLockedEndSerie) then
    //  FSerie.PtArr:=FSerie.PointsTopo.Count-1;


    // nombre de stations
    FSerie.NbPoints:=FSerie.PointsTopo.Count;
    FDocTopo.PutSerie(FNumeroSerie, FSerie);
    // Vérification des stations

    // Tout est OK: On met Result à TRUE;
    Result:=True;
    AfficherMessage(Format('%s.ImplementerModifs OK',[ClassName]));
  //}
  {$IFDEF ACTIVETRY}
  except
    AfficherMessage(Format('%s.ImplementerModifs KO',[ClassName]));
  end;
  {$ENDIF}
end;

procedure TCadreSerie.PutStationInForm(const Vis: TUneVisee; const Idx: integer);
begin
  lbNbVisees.Caption := Format('%d/%d', [FCurrentIdxStation, FCurrentNbStations]);
  editIDStation.Text := Trim(Vis.IDTerrainStation);//IntToStr(Idx);
  cmbTypeVisee.ItemIndex:= Vis.TypeGalerie;
  editCode.Text      := IntToStr(Vis.Code);
  editExpe.Text      := IntToStr(Vis.Expe);
  editLongueur.Text  := Format('%.2f',[Vis.Longueur]);
  editAzimut.Text    := Format('%.2f',[Vis.Azimut]);
  editPente.Text     := Format('%.2f',[Vis.Pente]);

  editLG.Text        := Format('%.2f',[Vis.LG]);
  editLD.Text        := Format('%.2f',[Vis.LD]);
  editHZ.Text        := Format('%.2f',[Vis.HZ]);
  editHN.Text        := Format('%.2f',[Vis.HZ]);

  editStObservations.Text := Vis.Commentaires;

end;

function TCadreSerie.GetStationFromForm: TUneVisee;
begin
  Result.IDTerrainStation := Trim(editIDStation.Text);
  Result.TypeGalerie      := cmbTypeVisee.ItemIndex;

  Result.Code             := StrToIntDef(Trim(editCode.Text), 1);
  Result.Expe             := StrToIntDef(Trim(editExpe.Text), 1);

  Result.Longueur         := StrToFloatDef(Trim(editLongueur.Text), 0.00);
  Result.Azimut           := StrToFloatDef(Trim(editAzimut.Text), 0.00);
  Result.Pente            := StrToFloatDef(Trim(editPente.Text), 0.00);


  Result.LG               := StrToFloatDef(Trim(editLG.Text), 0.00);
  Result.LD               := StrToFloatDef(Trim(editLD.Text), 0.00);
  Result.HZ               := StrToFloatDef(Trim(editHZ.Text), 0.00);
  Result.HN               := StrToFloatDef(Trim(editHN.Text), 0.00);

  Result.Commentaires     := Trim(editStObservations.Text);

end;
//*)

procedure TCadreSerie.editCodeDblClick(Sender: TObject);
begin
   DialogEntry(editCode, 'N° code', 1, '1', '9999');
end;

procedure TCadreSerie.editExpeDblClick(Sender: TObject);
begin
  DialogEntry(editExpe, 'N° expe', 1, '1', '9999');
end;

procedure TCadreSerie.editIDReseauDblClick(Sender: TObject);
var
  WU: integer;
begin
  WU := StrToIntDef(editIDReseau.Text, 0);
  editIDReseau.Text := IntToStr(ChooseReseau(FDocuTopo, WU));
end;

procedure TCadreSerie.editIDSerieDblClick(Sender: TObject);
begin
  DialogEntry(editIDSerie, 'N° serie', 1, '1', '9999');
end;

procedure TCadreSerie.editIDStationDblClick(Sender: TObject);
begin
  DialogEntry(editIDStation, 'ID terrain', 0, '', '');
end;

procedure TCadreSerie.editAzimutDblClick(Sender: TObject);
begin
  DialogEntry(editAzimut, 'Azimut', 2, '0.00', '400.00');
end;






procedure TCadreSerie.editHNDblClick(Sender: TObject);
begin
  DialogEntry(editHN, 'Bas', 2, '0.00', '100.00');
end;

procedure TCadreSerie.sclStationChange(Sender: TObject);
var
V: TUneVisee;
begin
  FCurrentIdxStation := sclStation.Position;
  V := QGetStation(FCurrentIdxStation);
  PutStationInForm(V, FCurrentIdxStation);
end;





procedure TCadreSerie.editHZDblClick(Sender: TObject);
begin
  DialogEntry(editHZ, 'Haut', 2, '0.00', '100.00');
end;





procedure TCadreSerie.editLDDblClick(Sender: TObject);
begin
  DialogEntry(editLD, 'Droite', 2, '0.00', '100.00');
end;


procedure TCadreSerie.editLGDblClick(Sender: TObject);
begin
  DialogEntry(editLG, 'Gauche', 2, '0.00', '100.00');
end;


procedure TCadreSerie.editLongueurDblClick(Sender: TObject);
begin
  DialogEntry(editLongueur, 'Longueur', 2, '0.00', '160.00');
end;

procedure TCadreSerie.editPenteDblClick(Sender: TObject);
begin
   DialogEntry(editPente, 'Pente', 2, '-100.00', '200.00');
end;


procedure TCadreSerie.Button13Click(Sender: TObject);
var
  WU: string;
  Sr, St: integer;
begin
  WU := '';
  if InputQuery('', 'Find station', WU) then
  begin
    if FDocuTopo.FindPtTopo(WU, Sr, St) then
    begin
      editSerDep.Text := InttoStr(Sr);
      editStDep.Text  := Inttostr(St);
    end;
  end;
end;

procedure TCadreSerie.Button12Click(Sender: TObject);
begin
  SkipStation(9999);
end;

procedure TCadreSerie.Button11Click(Sender: TObject);
begin
  SkipStation(10);
end;

procedure TCadreSerie.Button10Click(Sender: TObject);
begin
  SkipStation(1);
end;

procedure TCadreSerie.btnAddStationClick(Sender: TObject);
var
  V: TUneVisee;
begin
  V := GetStationFromForm;
  if QuestionOuiNon('Ajouter station') then begin
    //V.Commentaires:= 'Added';
    if not QAddStation(V) then ShowMessage('Impossible d''ajouter cette station');
  end;
end;

procedure TCadreSerie.Button14Click(Sender: TObject);
var
  WU: string;
  Sr, St: integer;
begin
  WU := '';
  if InputQuery('', 'Find station', WU) then
  begin
    if FDocuTopo.FindPtTopo(WU, Sr, St) then
    begin
      editSerArr.Text := InttoStr(Sr);
      editStArr.Text  := Inttostr(St);
    end;
  end;

end;

procedure TCadreSerie.Button7Click(Sender: TObject);
begin
  SkipStation(-9999);
end;

procedure TCadreSerie.Button8Click(Sender: TObject);
begin
  SkipStation(-10);
end;

procedure TCadreSerie.Button9Click(Sender: TObject);
begin
  SkipStation(-1);
end;


initialization
  {$I CdrSerie.lrs}

end.

