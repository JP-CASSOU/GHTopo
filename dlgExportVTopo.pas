unit dlgExportVTopo;

{$mode delphi}{$H+}

interface

uses
  Common, StructuresDonnees,
  UnitClassPalette,
  ToporobotClasses2012, ObjetSerie,
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Buttons, StdCtrls, ComCtrls;

type

  { TDialogVTopo }

  TDialogVTopo = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    chkNotUseIDs: TCheckBox;
    editXCoord: TEdit;
    editYCoord: TEdit;
    editZCoord: TEdit;
    editMainEntrance: TEdit;
    editStPrefix: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    lbFileName: TStaticText;
    lbProcess: TStaticText;
    ProgressBar: TProgressBar;
    procedure BitBtn1Click(Sender: TObject);
  private
    { private declarations }
    FDocuTopo      : TToporobotStructure2012;
    FFichierTRO    : string;
    FPalette       : TPalette256;
    FDefaultColor  : boolean;
    FReporter      : string;
    FMainEntrance  : string;
    FPrefix        : string;
    FIDPtDepart    : string;
    FCoordX        : double;
    FCoordY        : double;
    FCoordZ        : double;
    FColorReseau   : TColor;


  public
    { public declarations }
    function Initialize(const MyDocuTopo: TToporobotStructure2012; const QFileName: string): boolean;
    // Export Visual Topo
    procedure ExportVTOPO(const NotUseIDLitteraux: boolean);

  end; 

var
  DialogVTopo: TDialogVTopo;

implementation

procedure TDialogVTopo.BitBtn1Click(Sender: TObject);
begin
  ExportVTOPO(chkNotUseIDs.Checked);
end;

function TDialogVTopo.Initialize(const MyDocuTopo: TToporobotStructure2012; const QFileName: string): boolean;
begin
  Result := False;
  try
    FDocuTopo    := MyDocuTopo;
    FFichierTRO  := QFileName;
    lbFileName.Caption := ExtractFileName(FFichierTRO);
    FDefaultColor      := False;
    FPalette           := TPalette256.Create;
    FReporter          := Application.ExeName;
    FMainEntrance      := FDocuTopo.GetEntree(0).eNomEntree;
    FPrefix            := '';
    FIDPtDepart        := '1.0';
    FCoordX            := FDocuTopo.GetDefaultCoordX/1000.0;
    FCoordY            := FDocuTopo.GetDefaultCoordY/1000.0;
    FCoordZ            := FDocuTopo.GetDefaultCoordZ;
    FColorReseau       := clBlue;
    // peupler les textbox
    editStPrefix.Text := FPrefix;
    editMainEntrance.Text := FMainEntrance;

    //********************
    Result := True;
  except
  end;
end;
// Export Visual Topo
procedure TDialogVTopo.ExportVTOPO(const NotUseIDLitteraux: boolean);
const
  VTOPO_VERSION ='Version 4.9';
  FMTSERSTVTOPO ='%d.%d';
var
  FromStationID,
  ToStationID  :string;
  Code   : TCode;
  Expe   : TExpe;
  //Serie  : T;
  CurrStation,
  PrevStation: TUneVisee;
  pTRO   : TextFile;
  NoSer  : Integer;
  NoSt   : integer;
  Decl   : double;
  ParamChanged: Boolean;
  Palette256: TPalette256;
  S666, S777, S888   : string;
  Serie: TObjSerie;
  procedure WriteLnParam(const C: TCode; const E: TExpe);
  var
    UB, UC, Clino: string;
    CC: TColor;
  begin
    writeLn(pTRO,'');
    //Param Deca Deg Clino Deg 1.0300 Dir,Dir,Dir Std
    case Trunc(C.GradAz) of
      360: UB:='Deg';
      400: UB:='Gra';
    else
      UB:='Deg';
    end;
    case Trunc(C.GradInc) of
      360: begin
             Clino:='Clino';
             UC   :='Deg';
           end;
      361: begin
             Clino:='Vulc';
             UC   :='Deg';
           end;
      400: begin
             Clino:='Clino';
             UC   :='Gra';
           end;
      401: begin
             Clino:='Vulc';
             UC   :='Gra';
           end;
      380: begin
             Clino:='Deniv'; // anciennement: Prof
             UC   :='';
           end;
    end;
    Decl:=(E.Declinaison/10) * 400.0 / C.GradAz;


    CC:=Palette256.GetColorByIndex(E.Couleur);
    //if FDefaultColor then CC:=lbColorReseau.Color;
    WriteLn(pTRO, Format('Param Deca %s %s %s %.4f Dir,Dir,Dir %d,%d,%d',
                         [UB, Clino, UC,
                          Decl,
                          GetRValue(CC),
                          GetGValue(CC),
                          GetBValue(CC)

                         ]));
    //*)
    writeln(pTRO,'');
  end;
begin
  AfficherMessage(Format('%s.ExportVTOPO(%s)',[ClassName, FFichierTRO]));
  // initialisations
  FDefaultColor:=False;
  FPrefix      :=Trim(editStPrefix.Text);
  FMainEntrance:=trim(editMainEntrance.Text);
  FCoordX      :=StrToFloatDef(Trim(editXCoord.Text), 0.00);
  FCoordY      :=StrToFloatDef(Trim(editYCoord.Text), 0.00);
  FCoordZ      :=StrToFloatDef(Trim(editZCoord.Text), 0.00);
  //FColorReseau :=lbColorReseau.Color;
  ParamChanged:=False;
  // palette
  Palette256:=TPalette256.Create;
  with Palette256 do begin
    GenerateTOPOROBOTPalette;
  end;
  AssignFile(pTRO, FFichierTRO);
  try
    ReWrite(pTRO);
    with FDocuTopo do begin
      // en tête
      lbProcess.Caption:='Writing header';
      Application.ProcessMessages;
      WriteLn(pTRO, VTOPO_VERSION);
      WriteLn(pTRO, '');

      // Trou CDS03,0.000,0.000,0,
      WriteLn(pTRO, Format('Trou %s,%.3f,%.3f,%f,',
                           [FMainEntrance, FCoordX,FCoordY,FCoordZ]));
      // Club SiliconCavings
      WriteLn(pTRO, Format('Club %s',[FReporter]));
      WriteLn(pTRO, Format('Entree %s',[FIDPtDepart]));
      WriteLn(pTRO, Format('Couleur %d,%d,%d',[
                           GetRValue(FColorReseau),
                           GetGValue(FColorReseau),
                           GetBValue(FColorReseau)]));

      // première ligne de données
      WriteLn(pTRO,'Param Deca Deg Clino Deg 0.0000 Dir,Dir,Dir Std');
      Serie:=GetSerie(1);

      WriteLn(pTRO, Format('%10s %10s %.1f   %.1f %.1f %.1f %.2f %.2f %.2f %s',
                               [Format(FMTSERSTVTOPO,[Serie.GetNoSerieDep, Serie.GetNoPointDep]),
                                Format(FMTSERSTVTOPO,[Serie.GetNoSerieDep, Serie.GetNoPointDep]),
                                0.00,
                                0.00,
                                0.00,
                                1.00,
                                1.00,
                                1.00,
                                1.00,
                                'Entrée 1.0']));

      // données
      //************
      ProgressBar.Min:=1;
      //ProgressBar.Max:=TableSeries.Count-1;
      ProgressBar.Max:= GetNbSeries;
      Application.ProcessMessages;
      PrevStation.Code := 0;
      PrevStation.Expe := 0;


      //for NoSer:=1 to TableSeries.Count-1 do begin
      for NoSer:=1 to GetNbSeries-1 do begin
        lbProcess.Caption:=Format('Process. Serie %d',[NoSer]);
        Serie:=GetSerie(NoSer);
        FromStationID:=Format(FMTSERSTVTOPO,[Serie.GetNoSerieDep, Serie.GetNoPointDep]);
        for NoSt:=1 to Serie.GetNbVisees - 1 do begin
          //AfficherMessage(Format('%d, %d: %s',[NoSer, NoSt, Serie.NomSerie]));

          CurrStation := Serie.GetVisee(NoSt);
          Code:=GetCodeByIndex(CurrStation.Code);
          Expe:=GetExpeByIndex(CurrStation.Expe);
          //FromStationID := ToStationID;

          if ((NoSer=1) and (NoSt=1)) then begin
            //FromStationID:=FIDPtDepart;
            WriteLnParam(Code, Expe);

          end;
          //*)

          if (CurrStation.Code <> PrevStation.Code) OR
             (CurrStation.Expe <> PrevStation.Expe) then
             WriteLnParam(Code, Expe);
          //*)
(*
1_7        1_8          91097.0   91687.0  286.0  -17.0   0.50   0.30   2.50      * N
1_8        1_8a                      2.00   80.0    5.0   0.30   0.30   0.80      * N
//*)


          ToStationID:=Format(FMTSERSTVTOPO,[Serie.GetIndexSerie, NoSt]);
          if (NoSt = Serie.GetNbVisees - 1) then
             ToStationID:=Format(FMTSERSTVTOPO,[Serie.GetNoSerieArr, Serie.GetNoPointArr]);

          if Trim(CurrStation.Commentaires)=''
          then S777:=''
          else S777:=Format('%s',[CurrStation.Commentaires]);
          if Trim(CurrStation.IDTerrainStation)=''
          then S666:=''
          else S666:=Format('[%s]',[CurrStation.IDTerrainStation]);
          if (Trim(CurrStation.Commentaires)='')    and
             (Trim(CurrStation.IDTerrainStation)='')
          then
            S777:=''
          else
            S777:=';'+S666+S777+';';



          WriteLn(pTRO, Format('%10s %10s %.1f   %.1f %.1f %.1f %.2f %.2f %.2f N %s',
                               [FromStationID,
                                ToStationID,
                                CurrStation.Longueur,
                                CurrStation.Azimut,
                                CurrStation.Pente,
                                CurrStation.LG,
                                CurrStation.LD,
                                CurrStation.HZ,
                                CurrStation.HN,
                                S777]));
          PrevStation    := CurrStation;
          FromStationID  := ToStationID;
          //*)
          ProgressBar.Position:=NoSer;
          Application.ProcessMessages;
          lbProcess.Caption:='Complete';
        end;
      end;

    end;
    // footpage
    WriteLn(pTRO,'');
    WriteLn(pTRO,'[Configuration 4.9]');
    WriteLn(pTRO,'');
    WriteLn(pTRO,'Visual Topo=2,3,-1,-1,-1,-1,1,1,801,610');
    WriteLn(pTRO,'Options=1,1');
    WriteLn(pTRO,'Calcul=0,1,-1,-1,-1,-1,22,22,844,449');
    WriteLn(pTRO,'Options=26,1,0,1,0');
    WriteLn(pTRO,'ExportDxf=0,100,391,1,6,7,4,4,3,3,2,7,9');
    WriteLn(pTRO,'Colonnes=8.00,8.00,8.00,8.00,8.00,8.00,8.00,8.00,8.00,'+
                 '8.00,8.00,2.00,4.00,0.38,8.00,8.00,8.00,8.00,8.00,'+
                 '0.38,0.00,0.00');
    //WriteLn(pTRO,'Rendu3D=2,3,-1,-1,-4,-23,44,44,866,471');
    WriteLn(pTRO,'Options=384,1.00,4,0,2,6914,1');
    AfficherMessage(Format('%s.ExportVTOPO(%s) OK',[ClassName, FFichierTRO]));
  finally
    CloseFile(pTRO);
    Palette256.Free;
  end;

end;

initialization
//{$I dlgExportVTopo.lrs}

end.

