unit CadreEntrance;
// Date: 16/05/2013
// Statut: OK

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
  Common, StructuresDonnees, CallDialogs, ToporobotClasses2012,
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, curredit;

type

  { TCdrEntreeCavites }

  TCdrEntreeCavites = class(TFrame)
    editCommentaires: TEdit;
    editNomEntree: TEdit;
    editNumEntree: TCurrencyEdit;
    editPoint: TCurrencyEdit;
    editSerie: TCurrencyEdit;
    editXEntree: TCurrencyEdit;
    editYEntree: TCurrencyEdit;
    editZEntree: TCurrencyEdit;
    lbStationOfEntrance: TLabel;
    lbEntranceCoordinates: TLabel;
    lbCaveName: TLabel;
    lbComments: TLabel;
    lbEntrance: TLabel;
    procedure FrameEnter(Sender: TObject);
  private
    FEntrance: TEntrance;
    FModifie: boolean;


    procedure PutEntranceInForm;

    { private declarations }
  public
    { public declarations }
    procedure SetEntrance(const E: TEntrance; const DoInitCaptions: boolean);
    procedure InitCaptions;
    function GetEntranceFromForm: TEntrance;
    function  IsModified: boolean;
    procedure SetModified(const m: boolean);
  end; 

implementation

{$R *.lfm}


procedure TCdrEntreeCavites.SetEntrance(const E: TEntrance; const DoInitCaptions: boolean);
begin
  FEntrance := E;
  if (DoInitCaptions) then InitCaptions;
  PutEntranceInForm;
end;

procedure TCdrEntreeCavites.InitCaptions;
begin
  lbEntrance.Caption            := AnsiToUtf8(rsCDR_ENTR_NOENTRANCE);
  lbCaveName.Caption            := AnsiToUtf8(rsCDR_ENTR_ENTRNAME);
  lbEntranceCoordinates.Caption := AnsiToUtf8(rsCDR_ENTR_COORDINATES);
  lbStationOfEntrance.Caption   := AnsiToUtf8(rsCDR_ENTR_STATOFENTR);
  lbComments.Caption            := AnsiToUtf8(rsLBL_COMMENTS);

;
end;

procedure TCdrEntreeCavites.FrameEnter(Sender: TObject);
begin
  FModifie := True;
  AfficherMessage('Element modifie');
end;

procedure TCdrEntreeCavites.PutEntranceInForm;
begin;
 FModifie:= False;
 with FEntrance do begin
  editNumEntree.AsInteger := eNumEntree;
  editNomEntree.Text := AnsiToUtf8(eNomEntree);
  editXEntree.Value  := eXEntree;
  editYEntree.Value  := eYEntree;
  editZEntree.Value  := eZEntree;
  editSerie.AsInteger:= eRefSer;
  editPoint.AsInteger:= eRefSt;
  editCommentaires.Text:=AnsiToUtf8(eObserv);
 end;
end;
function TCdrEntreeCavites.GetEntranceFromForm: TEntrance;
var
  E: TEntrance;
begin
  E.eNumEntree   := StrToIntDef(editNumEntree.Text,0);
  E.eNomEntree   := editNomEntree.Text;
  E.eXEntree     := editXEntree.Value;
  E.eYEntree     := editYEntree.Value;
  E.eZEntree     := editZEntree.Value;
  //E.eDeltaX      := 0.00;
  //E.eDeltaY      := 0.00;
  //E.eDeltaZ      := 0.00;

  E.eRefSer      := editSerie.AsInteger;
  E.eRefSt       := editPoint.AsInteger;
  E.eObserv      := editCommentaires.Text;

  Result:=E;
end;

function TCdrEntreeCavites.IsModified: boolean;
begin
  Result := FModifie;
end;

procedure TCdrEntreeCavites.SetModified(const m: boolean);
begin
  FModifie:=m;
end;

end.

