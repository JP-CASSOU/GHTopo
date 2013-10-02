unit CadreViseesAntenne;
// Visées en antenne
// Date: 19/08/2012
// Statut: En cours
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
  Common, CallDialogsStdVersion, StructuresDonnees,
  ToporobotClasses2012, Dialogs,
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, Grids,
  ActnList, Menus, Buttons;

type

  { TCdrAntennes }

  TCdrAntennes = class(TFrame)
    acDeleteRow: TAction;
    acSauvegrd: TAction;
    acAddLigne: TAction;
    CdrAntennesActionList: TActionList;
    grdViseesEnAntenne: TStringGrid;
    CdrAntennesImagesList: TImageList;
    lbCurrentCell: TLabel;
    CdrAntennesPopUp: TPopupMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    procedure acAddLigneExecute(Sender: TObject);
    procedure acDeleteRowExecute(Sender: TObject);
    procedure acSauvegrdExecute(Sender: TObject);
    procedure grdViseesEnAntenneClick(Sender: TObject);
    procedure grdViseesEnAntenneDblClick(Sender: TObject);

  private
    { private declarations }
    FMyDocToporobot: TToporobotStructure2012;
    // position de la cellule courante
    FCurrRow, FCurrCol: integer;
    procedure ViderTableAntennes;
    procedure ImplementerModifs;
    procedure RemplirTableau;
    procedure InitCaptions;
    procedure ViderLigne(const N: integer; const F: Boolean);
  public
    { public declarations }
     function Initialise(const DT: TToporobotStructure2012) : boolean;
  end; 

implementation
const
  COL_ID_TERRAIN = 1;
  COL_RESEAU     = 2;
  COL_SER_DEP    = 3;
  COL_PT_DEP     = 4;
  COL_CODE       = 5;
  COL_EXPE       = 6;
  COL_LONGUEUR   = 7;
  COL_AZIMUT     = 8;
  COL_PENTE      = 9;
  COL_OBSERV     = 10;

{$R *.lfm}

function TCdrAntennes.Initialise(const DT: TToporobotStructure2012) : boolean;
begin
  Result := False;
  try
    FMyDocToporobot := DT;
    InitCaptions;
    RemplirTableau;
    Exit;
  except

  end;
end;

procedure TCdrAntennes.grdViseesEnAntenneDblClick(Sender: TObject);
var
  PP :  TPoint ;
  QCol, QRow: integer ;
  Q: integer;
  S: string;
  UnCode: TCode;
  UneExpe: TExpe;
begin
  //******************************************************
  // DONE: Ce code fonctionne mais l'événement OnDblClick est intercepté
  //       par l'éditeur de texte incorporé
  // Source: http://www.developpez.net/forums/d299139/ \n
  //         environnements-developpement/delphi/selection-cellule-tstringgrid-double-click/

  PP:=Mouse.CursorPos;
  //GetCursorPos(P) ;
  PP:=grdViseesEnAntenne.ScreenToClient(PP);
  grdViseesEnAntenne.MouseToCell(PP.X, PP.Y, Qcol, QRow);
  //ShowMessage(Format('Cellule(%d, %d) = %s', [Qcol, QRow, grdViseesEnAntenne.Cells[Qcol, QRow]]));
  //case QCol of
  case FCurrCol of
    COL_CODE: begin // sélectionner un code
         q:=StrToIntDef(grdViseesEnAntenne.Cells[COL_CODE, grdViseesEnAntenne.Row],1);
         q := SelectionDansListe(FMyDocToporobot, mslCODE, q, False);
         if (q>0) then
           UnCode := FMyDocToporobot.GetCodeByIndex(q)
         else
           UnCode := FMyDocToporobot.GetCodeByIndex(1);
         grdViseesEnAntenne.Cells[COL_CODE, grdViseesEnAntenne.Row] := Format('%d', [UnCode.IDCode]);
       end;
    COL_EXPE: begin // sélectionner une expé
         q:=StrToIntDef(grdViseesEnAntenne.Cells[COL_EXPE, grdViseesEnAntenne.Row],1);
         q := SelectionDansListe(FMyDocToporobot, mslEXPE, q, False);
         if (q>0) then
           UneExpe := FMyDocToporobot.GetExpeByIndex(Q)
         else
           UneExpe := FMyDocToporobot.GetExpeByIndex(1);

         AfficherMessage(Format('%d - %d',[q, UneExpe.IDExpe]));
         grdViseesEnAntenne.Cells[COL_EXPE, grdViseesEnAntenne.Row] := Format('%d', [UneExpe.IDExpe]);
       end;
    COL_OBSERV:
       begin // commentaire station
         s := grdViseesEnAntenne.Cells[COL_OBSERV, grdViseesEnAntenne.Row];
         if InputQuery(rsINPUT_COMMENTAIRE_TITRE, rsINPUT_COMMENTAIRE_MSG, s) then
            grdViseesEnAntenne.Cells[COL_OBSERV, grdViseesEnAntenne.Row]:=s;
       end;

  end;

end;

procedure TCdrAntennes.ViderTableAntennes;
begin
  while (FMyDocToporobot.GetNbAntennes > 1) do
  begin
    FMyDocToporobot.RemoveViseeAntenne(0);
  end;
end;


procedure TCdrAntennes.grdViseesEnAntenneClick(Sender: TObject);
begin
  FCurrCol := grdViseesEnAntenne.Col;
  FCurrRow := grdViseesEnAntenne.Row;
  lbCurrentCell.Caption := Format('L%dC%d',[FCurrRow, FCurrCol]);
end;

procedure TCdrAntennes.acDeleteRowExecute(Sender: TObject);
begin
  if (QuestionOuiNon(Format(rsCDR_ANTENNES_DEL_LINE, [FCurrRow]))) then
  begin
    try
      grdViseesEnAntenne.DeleteRow(FCurrRow);
    except
    end;
  end;
end;

procedure TCdrAntennes.acSauvegrdExecute(Sender: TObject);
begin
  self.ImplementerModifs;
end;

procedure TCdrAntennes.acAddLigneExecute(Sender: TObject);
begin
  grdViseesEnAntenne.RowCount := grdViseesEnAntenne.RowCount + 1;
  ViderLigne(grdViseesEnAntenne.RowCount - 1, False);
end;

procedure TCdrAntennes.ViderLigne(const N: integer; const F: Boolean);
var
  i: integer;
begin
  with grdViseesEnAntenne do
  begin
    Cells[0, N] := Format('%d',[N]);
    for i:= 1 to grdViseesEnAntenne.ColCount - 1 do
    begin
      Cells[i, N] := '';
    end;
    if (F) then Cells[1, N] := '*';
  end;
end;
procedure TCdrAntennes.RemplirTableau;

//  COL_
var
  i, Nb : integer;
  VA: TViseeAntenne;
begin
  // en-tête du tableau

  with grdViseesEnAntenne do
  begin
    ColCount:= 11;
    Cells[0, 0] := 'No';
    // Cells[COL_ID_VISEE, 0]    := 'ID'; // Les ID sont renumérotés
    Cells[COL_ID_TERRAIN, 0]  := 'Label';
    Cells[COL_RESEAU, 0]      := 'Reseau';
    Cells[COL_SER_DEP, 0]     := 'Ser';
    Cells[COL_PT_DEP, 0]      := 'Pt';
    Cells[COL_CODE, 0]        := 'Code';
    Cells[COL_EXPE, 0]        := 'Expe';
    Cells[COL_LONGUEUR, 0]    := 'Long.';
    Cells[COL_AZIMUT, 0]      := 'Az.';
    Cells[COL_PENTE, 0]       := 'Inc.';
    Cells[COL_OBSERV, 0]      := 'Obs.';
    Options:=[goFixedVertLine,goFixedHorzLine,goVertLine,goHorzLine, goColSizing,
              goRangeSelect,goEditing,goTabs]; // ,goAlwaysShowEditor
  end;
  Nb := FMyDocToporobot.GetNbAntennes;
  //ShowMessage(inttostr(Nb));
  // peupler avec les visées en antenne
  if (Nb > 0) then
  begin
    with grdViseesEnAntenne do
    begin
      RowCount := Nb;
      for i := 1 to Nb - 1 do
      begin
        //try
          VA := FMyDocToporobot.GetViseeAntenne(i);
          ViderLigne(i, False);
          //Cells[COL_ID_VISEE, EWE]    := Format('%d',[VA.IDViseeAntenne]);
          Cells[COL_ID_TERRAIN, i]  := AnsiToUtf8(VA.IDTerrainStation);
          Cells[COL_RESEAU, i]      := Format('%d',[VA.Reseau]);
          Cells[COL_SER_DEP, i]     := Format('%d',[VA.SerieDepart]);
          Cells[COL_PT_DEP, i]      := Format('%d',[VA.PtDepart]);
          Cells[COL_CODE, i]        := Format('%d',[VA.Code]);
          Cells[COL_EXPE, i]        := Format('%d',[VA.Expe]);
          Cells[COL_LONGUEUR, i]    := Format('%.2f',[VA.Longueur]);
          Cells[COL_AZIMUT, i]      := Format('%.2f',[VA.Azimut]);
          Cells[COL_PENTE, i]       := Format('%.2f',[VA.Pente]);
          Cells[COL_OBSERV, i]      := AnsiToUtf8(VA.Commentaires);

        //except
        //end;
      end;
    end;

  end;
end;

procedure TCdrAntennes.InitCaptions;
begin
  acAddLigne.Caption := rsCDR_ANTENNES_AC_ADDLINE;
  acDeleteRow.Caption:= rsCDR_ANTENNES_AC_DELLINE;
  acSauvegrd.Caption := rsCDR_ANTENNES_AC_SAVEGRD;
end;

// Ecrase la liste des visées en antennes dans FDocToporobot
// puis la recrée depuis le tableau
procedure TCdrAntennes.ImplementerModifs;
var
  i, j: integer;
  VA: TViseeAntenne;
  EWE: String;
begin
  ViderTableAntennes;
  with grdViseesEnAntenne do
  begin
    for i:=1 to grdViseesEnAntenne.RowCount - 1 do
    begin
      // supprimer lignes blanches
      EWE := '';
      for j := 1 to grdViseesEnAntenne.ColCount - 1 do EWE := EWE + Trim(Cells[j, i]);
      if (EWE = '') then Continue;
      VA.IDViseeAntenne     := i;
      VA.IDTerrainStation   := Utf8ToAnsi(Trim(Cells[COL_ID_TERRAIN, i]));
      VA.Reseau             := StrToIntDef(Trim(Cells[COL_RESEAU, i]), 0);
      VA.SerieDepart        := StrToIntDef(Trim(Cells[COL_SER_DEP, i]), 0);
      VA.PtDepart           := StrToIntDef(Trim(Cells[COL_PT_DEP, i]), 0);
      VA.Code               := StrToIntDef(Trim(Cells[COL_CODE, i]), 0);
      VA.Expe               := StrToIntDef(Trim(Cells[COL_EXPE, i]), 0);
      VA.Longueur           := StrToFloatDef(Trim(Cells[COL_LONGUEUR, i]), 0.00);
      VA.Azimut             := StrToFloatDef(Trim(Cells[COL_AZIMUT, i]), 0.00);
      VA.Pente              := StrToFloatDef(Trim(Cells[COL_PENTE, i]), 0.00);
      VA.Commentaires       := Utf8ToAnsi(Trim(Cells[COL_OBSERV, i]));
      FMyDocToporobot.AddViseeAntenne(VA);

    end;
  end;
  RemplirTableau;
end;

end.

