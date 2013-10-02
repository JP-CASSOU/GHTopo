unit dlgStatistiques;

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
  StructuresDonnees,
  Common, UnitEntites, Classes, SysUtils, FileUtil, Forms, Controls, Graphics,
  Dialogs, ComCtrls, ExtCtrls, StdCtrls, Grids, Buttons, CadreHistoAltitudes,
  CadreHistoDirections, types;



type

  { TdlgStats }

  TdlgStats = class(TForm)
    BitBtn1: TBitBtn;
    btnCopyTableau: TButton;
    CdrHistoAltitudes1: TCdrHistoAltitudes;
    CdrHistogrammes1: TCdrHistogrammes;
    cmbAffichageTableaux: TComboBox;
    lbNbItems: TLabel;
    memoSynthese: TMemo;
    PageControl1: TPageControl;
    grdSyntheseSpeleometrique: TStringGrid;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    procedure btnCopyTableauClick(Sender: TObject);
    procedure cmbAffichageTableauxChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private

    { private declarations }
    FTableEntites : TTableDesEntites;
    procedure RefreshTableau(const Mode: byte);
    procedure ListerSynthese;

  public
    { public declarations }
    function InitialiserDialogStats(const Filename: string): boolean;
    procedure FinaliserDialogStats;
  end; 

var
  dlgStats: TdlgStats;

implementation


{$R *.lfm}
function TdlgStats.InitialiserDialogStats(const Filename: string): boolean;
begin
  Result := False;
  FTableEntites := TTableDesEntites.Create;
  try
    if (FTableEntites.LoadEntites(FileName, True) = 0) then
    begin
      ShowMessage(AnsiToUtf8(rsMSG_TABLE_ENTITY_FAIL));
      Exit;
    end;
    // préparation tables temporaires
    with FTableEntites do
    begin
      // mini et maxi
      SetMinMax;
      // filtres
      MetaFiltre('');
      // spéléométrie
      CalculSpeleometrie;
    end;
    // cadre histogramme altitudes
    with CdrHistoAltitudes1 do
    begin
      Initialise(FTableEntites, '');
    end;
    // cadre histogramme des directions
    with CdrHistogrammes1 do
    begin
      Initialise(FTableEntites, '');
    end;
    Result := True;
    // tableaux
    RefreshTableau(0);
    // synthèse
    ListerSynthese;

  except
  end;
end;

procedure TdlgStats.FinaliserDialogStats;
begin
  try
    FTableEntites.Finalise;
  finally
    FTableEntites.Free;
  end;
end;

procedure TdlgStats.cmbAffichageTableauxChange(Sender: TObject);
begin
  RefreshTableau(cmbAffichageTableaux.ItemIndex);
end;

procedure TdlgStats.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin

end;



procedure TdlgStats.btnCopyTableauClick(Sender: TObject);
begin
  GRDCCopierUnTableau(grdSyntheseSpeleometrique);
end;


procedure TdlgStats.RefreshTableau(const Mode: byte);
var
  i, Nb: integer;
  procedure WriteHeader(const M: byte);
  const
    WU = 100;
  begin
    with grdSyntheseSpeleometrique do
    begin
      ColWidths[0] := 120;
      case M of
        0: Cells[0, 0] := 'Dates';
        1: Cells[0, 0] := 'Reseaux';
        2: Cells[0, 0] := 'Colors';
      end;
      Cells[1, 0] := 'Fossiles';         ColWidths[1] := WU;
      Cells[2, 0] := 'Vadoses';          ColWidths[2] := WU;
      Cells[3, 0] := 'Ennoyables';       ColWidths[3] := WU;
      Cells[4, 0] := 'Siphons';          ColWidths[4] := WU;
      Cells[5, 0] := 'Spéciaux';         ColWidths[5] := WU;
      Cells[6, 0] := 'Tunnels';          ColWidths[6] := WU;
      Cells[7, 0] := 'Filons';           ColWidths[7] := WU;
    end;
  end;
  procedure WriteLigne(const Mode: byte; const Idx: integer);
  var
    Q: integer;
    SR: TVentilationSpeleometrie;
    QC: TColorGaleries;
    QR: TReseauxTopo;
    QD: TDatesTopo;
  begin
    with grdSyntheseSpeleometrique do
    begin
      Q := 1 + Idx;
      case Mode of
        0: begin
             QD := FTableEntites.GetDateSeanceByIdx(Idx);
             Cells[0, Q] := DateToStr(QD.DateTopo);
             SR := FTableEntites.GetSpeleometrieParDateByIdx(Idx);
           end;
        1: begin
             QR := FTableEntites.GetReseauByIdx(Idx);
             Cells[0, Q] := Format('%X',[QR.Couleur]);
             SR := FTableEntites.GetSpeleometrieParReseauxByIdx(Idx);
           end;
        2: begin
             QC := FTableEntites.GetCouleurByIdx(Idx);
             Cells[0, Q] := Format('%X',[QC.Color]);
             SR := FTableEntites.GetSpeleometrieParCouleurByIdx(Idx);
           end;
      end;
      Cells[1, Q] := Format('%.2f', [SR.Fossiles]);
      Cells[2, Q] := Format('%.2f', [SR.Vadoses]);
      Cells[3, Q] := Format('%.2f', [SR.Ennoyables]);
      Cells[4, Q] := Format('%.2f', [SR.Siphons]);
      Cells[5, Q] := Format('%.2f', [SR.Speciaux]);
      Cells[6, Q] := Format('%.2f', [SR.Tunnels]);
      Cells[7, Q] := Format('%.2f', [SR.Filons]);
    end;
  end;

begin
  case Mode of
    0: Nb := FTableEntites.GetNbDates;
    1: Nb := FTableEntites.GetNbReseaux;
    2: Nb := FTableEntites.GetNbCouleurs;
  end;
  case Mode of
    0: lbNbItems.Caption := Format('%d dates',[Nb]);
    1: lbNbItems.Caption := Format('%d reseaux',[Nb]);
    2: lbNbItems.Caption := Format('%d couleurs',[Nb]);
  end;
  with grdSyntheseSpeleometrique do
  begin
    ColCount := 8;
    RowCount := 1+Nb;
    WriteHeader(Mode);
    for i := 0 to Nb -1 do WriteLigne(Mode, i);
  end;
end;

procedure TdlgStats.ListerSynthese;
const
  ZDAR = '%s: X = %.2f m [%s], Y = %.2f m [%s], Z = %.2f m [%s]';
var
  EWE: TPoint3Df;
begin
  memoSynthese.Lines.Clear;
  with FTableEntites do
  begin
    EWE := GetCoinBasGauche;
    memoSynthese.Lines.Add(Format(ZDAR, ['Mini', EWE.X, GetIDStationXMini, EWE.Y, GetIDStationYMini , EWE.Z, GetIDStationZMini]));
    EWE := GetCoinHautDroit;
    memoSynthese.Lines.Add(Format(ZDAR, ['Maxi', EWE.X, GetIDStationXMaxi, EWE.Y, GetIDStationYMaxi , EWE.Z, GetIDStationZMaxi]));

  end;
end;

end.

