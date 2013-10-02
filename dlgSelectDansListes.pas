unit dlgSelectDansListes;
// différent de la version Delphi. Retourne un nombre
// Date: 19/04/2012
// Statut: Fonctionnel
// 20/04/2012: DONE: Redessin de la liste après modification de largeur de colonne
// 04/05/2012: DONE: Retourne les index TOPOROBOT et non les index internes
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
  UnitClassPalette, Common,
  StructuresDonnees,
  ToporobotClasses2012, ObjetSerie,
  Classes,
  SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  types, LCLType, Buttons;

type

  { TdlgSelectElement }

  TdlgSelectElement = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    hcColsTitres: THeaderControl;
    lbItemIndex: TLabel;
    lbTitreListe: TLabel;
    ListeElements: TListBox;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure hcColsTitresResize(Sender: TObject);
    procedure hcColsTitresSectionResize(HeaderControl: TCustomHeaderControl;
      Section: THeaderSection);
    procedure ListeElementsClick(Sender: TObject);
    procedure ListeElementsDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
  private
    { private declarations }
    FDocToporobot: TToporobotStructure2012;
    FNumeroElement: integer;
    FModeSelection: TModeSelectionListe;
    FNbItems: LongInt;
    FMyPalette: TPalette256;
    // tableau d'index
    FTableauIndex: array of Integer;
    function FindInternalIdx(const Idx: integer): integer;
    function GetIndexByInternalIndex(const Idx: integer): integer;

    //procedure SetDocToporobot(const DT: TToporobotStructure2012);


  public
    { public declarations }


    function GetNumeroElement: integer;
    function InitialiseListeSelection(const DT: TToporobotStructure2012;
                                      const MS: TModeSelectionListe;
                                      const QIndex: integer;
                                      const UseInternalIndex: boolean): boolean;
    function GetIndexElement(const UseInternalIndex: boolean): integer;
  end; 

var
  dlgSelectElement: TdlgSelectElement;

implementation

{$R *.lfm}
function TdlgSelectElement.FindInternalIdx(const Idx: integer): integer;
var
 i: integer;
begin
  Result := -1;
  if SizeOf(FTableauIndex) = 0 then Exit;
  try
    for i:= 0 to High(FTableauIndex) do
    begin
      if (FTableauIndex[i] = Idx) then
      begin
        Result := i;
        Exit;
      end;
    end;

  except
  end;
end;
function TdlgSelectElement.GetIndexByInternalIndex(const Idx: integer): integer;
begin

end;

function TdlgSelectElement.InitialiseListeSelection(const DT: TToporobotStructure2012;
                                                    const MS: TModeSelectionListe;
                                                    const QIndex: integer;
                                                    const UseInternalIndex: boolean): boolean;
// TODO: Implémenter la sélection sur ID de l'élément et non sur l'index interne
var
  i: integer;
  miou: string;
  ht:  THeaderSection;
  //MyIndex: LongInt;
  CC: TCode;
  EE: TExpe;
  SSR: TObjSerie;

  //R: TReseau;
  procedure AjouterTitreColonne(const Titre: string; const LG: integer);
  begin
    ht := hcColsTitres.Sections.Add;
    ht.Text := Titre;
    ht.MinWidth := LG;

  end;
begin
  Result := False;
  //ShowMessage('InitialiseListeSelection: '+ inttostr(QIndex) + ' - ' + BoolToStr(UseInternalIndex, 'Index internes', 'Refs Toporobot'));
  FDocToporobot := DT;

  FModeSelection := MS;
  // purge des titres des headers
  hcColsTitres.Sections.Clear;
  // titres
  case FModeSelection of
    mslENTRANCES: begin
                  miou := 'entrées';
                  FNbItems          := FDocToporobot.GetNbEntrees;
                  AfficherMessage(Format('-- %d items',[FNbItems]));
                  self.Caption      := 'Sélection d''une entrée';
                  with hcColsTitres do begin
                    AjouterTitreColonne('ID', 40);
                    AjouterTitreColonne('Entrée', 300);
                    AjouterTitreColonne('Réf', 80);
                    AjouterTitreColonne('X', 100);
                    AjouterTitreColonne('Y', 100);
                    AjouterTitreColonne('Z', 100);
                  end;

                end;
    mslRESEAUX: begin
                  miou := 'réseaux';
                  FNbItems := FDocToporobot.GetNbReseaux;
                  AfficherMessage(Format('-- %d items',[FNbItems]));
                  self.Caption      := 'Sélection d''un réseau';
                  with hcColsTitres do begin
                    AjouterTitreColonne('ID', 40);
                    AjouterTitreColonne('Couleur', 60);
                    AjouterTitreColonne('Réseau', 300);
                  end;
                end;
    mslCODE:   begin
                  miou := 'codes';
                  FNbItems := FDocToporobot.GetNbCodes;
                  AfficherMessage(Format('-- %d items',[FNbItems]));
                  self.Caption      := 'Sélection d''un code';
                  with hcColsTitres do begin
                    AjouterTitreColonne('ID', 40);
                    AjouterTitreColonne('Azimuts', 80);
                    AjouterTitreColonne('Pentes', 80);
                    AjouterTitreColonne('Commentaires', 500);

                  end;
                end;
    mslEXPE:   begin
                  // préparation de la palette
                  FMyPalette := TPalette256.Create;
                  FMyPalette.GenerateTOPOROBOTPalette;
                  AfficherMessage(Format('-- %d items',[FNbItems]));
                  // -------------------------
                  miou := 'séances';
                  FNbItems := FDocToporobot.GetNbExpes;
                  self.Caption      := 'Sélection d''une séance';
                  with hcColsTitres do begin
                    AjouterTitreColonne('ID', 40);
                    AjouterTitreColonne('Couleur', 80);
                    AjouterTitreColonne('Date', 90);
                    AjouterTitreColonne('Spéléomètre', 140);
                    AjouterTitreColonne('Spéléographe', 140);
                    AjouterTitreColonne('Déclinaison', 70);
                    AjouterTitreColonne('Commentaires', 500);
                  end;


                end;
    mslSERIE:  begin
                  miou := 'séries';
                  FNbItems := FDocToporobot.GetNbSeries;
                  AfficherMessage(Format('-- %d items',[FNbItems]));
                  self.Caption      := 'Sélection d''une série';
                  with hcColsTitres do begin
                    AjouterTitreColonne('ID', 40);
                    AjouterTitreColonne('Départ', 70);
                    AjouterTitreColonne('Arrivée', 70);
                    AjouterTitreColonne('Nom', 400);
                    AjouterTitreColonne('Réseau', 300);
                    AjouterTitreColonne('Nb points', 80);
                 end;
                end;
  end;
  lbTitreListe.Caption := Format('Liste des %d %s',[FNbItems, miou]);

  Result:=False;
  // liste
  try
    with FDocToporobot do begin
      SetLength(FTableauIndex, 0);
      if FNbItems = 0 then Exit;
      ListeElements.Clear;
      SetLength(FTableauIndex, FNbItems);
      for i:=0 to FNbItems - 1 do begin
        //R:=GetReseau(i);
        //lsbReseaux.Items.Add(Format('%000d - %s',  [R.IdxReseau, R.NomReseau]));
        case FModeSelection of
          mslCODE :
            begin
              CC := FDocToporobot.GetCode(i);
              FTableauIndex[i] := CC.IDCode;
            end;
          mslEXPE :
            begin
              EE := FDocToporobot.GetExpe(i);
              FTableauIndex[i] := EE.IDExpe;
            end;
          mslSERIE :
            begin
              SSR := FDocToporobot.GetSerie(i);
              FTableauIndex[i] := SSR.GetIndexSerie;
            end;
          else
            FTableauIndex[i] := i;
        end;
        ListeElements.Items.Add('');
      end;
    end;
    AfficherMessage(' -- 001');
    if (UseInternalIndex) then FNumeroElement := QIndex
                          else FNumeroElement := FindInternalIdx(QIndex);

    ListeElements.ItemIndex := FNumeroElement;
    Result:=True;
  except
  end;
end;

function  TdlgSelectElement.GetNumeroElement: integer;
begin
  Result := FNumeroElement;
end;
function TdlgSelectElement.GetIndexElement(const UseInternalIndex: boolean): integer;
begin

  Result := -1;
  try
   // Result := FNumeroElement;//ListeElements.ItemIndex;
    if (UseInternalIndex) then Result := FNumeroElement
                          else Result := FTableauIndex[FNumeroElement];
  except
  end;
  //ShowMessage('GetIndexElement: ' + inttostr(Result));
end;



procedure TdlgSelectElement.ListeElementsDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
const
  Q4  = 4; // compensation du décalage entre le header et la liste
  mg  = 1;
  // lgr = 25;
var

  C: TColor;
  m: integer;
  r: TRect;
  rs: TReseau;
  es: TEntrance;
  cs: TCode;
  ss: TExpe;
  sr: TObjSerie;
  miou: string;
  procedure DessineFiletColonne(const TB: integer);
  begin
    with ListeElements do begin
      Canvas.MoveTo(TB, ARect.Top);
      Canvas.LineTo(TB, ARect.Bottom);
    end;
  end;
  // bg = couleur de fond; tc= couleur du texte
  procedure DessineItem(const bg,tc: TColor);
  VAR
    //TB: integer;
    HS: THeaderSection;
  begin
    with ListeElements do begin
      case FModeSelection of
        mslENTRANCES:
          begin
            with ListeElements do begin

              //Canvas.Brush.Color:=clWhite;
              Canvas.FillRect(ARect);

              (*
              canvas.Pen.Color:=clBlack;
              canvas.Brush.Color:=rs.ColorReseau;
              canvas.Rectangle(R);
              //Canvas.Brush.Color:=clWhite;
              //*)
              Canvas.Brush.Color:=bg;
              Canvas.Font.Color :=tc;
              Canvas.Pen.Color  :=clSilver; // pour les filets




              //TB := hcColsTitres.hea
              HS := hcColsTitres.Sections.Items[0];  // ID entrée
              canvas.TextOut(HS.Left + 4, ARect.Top+1,  Format('%d',[es.eNumEntree]));
              HS := hcColsTitres.Sections.Items[1];  // Nom entrée
              DessineFiletColonne(HS.Left - Q4);
              canvas.TextOut(HS.Left + 4, ARect.Top+1, AnsiToUtf8(es.eNomEntree));
              HS := hcColsTitres.Sections.Items[2];
              DessineFiletColonne(HS.Left - Q4);
              canvas.TextOut(HS.Left + 4, ARect.Top+1, Format('%d.%d',[es.eRefSer, es.eRefSt]));
              HS := hcColsTitres.Sections.Items[3];
              DessineFiletColonne(HS.Left - Q4);
              canvas.TextOut(HS.Left + 4, ARect.Top+1, FormatterNombreAvecSepMilliers(es.eXEntree));
              HS := hcColsTitres.Sections.Items[4];
              DessineFiletColonne(HS.Left - Q4);
              canvas.TextOut(HS.Left + 4, ARect.Top+1, FormatterNombreAvecSepMilliers(es.eYEntree));
              HS := hcColsTitres.Sections.Items[5];
              DessineFiletColonne(HS.Left - Q4);
              canvas.TextOut(HS.Left + 4, ARect.Top+1, FormatterNombreAvecSepMilliers(es.eZEntree));
              DessineFiletColonne(HS.Right);


            end;
          end;
        mslRESEAUX:
          begin
            // offset:=Rect.Left + 30 + lgr;
            //Canvas.Brush.Color:=clWhite;
            Canvas.FillRect(ARect);
            canvas.Pen.Color:=clSilver;
            Canvas.Font.Color :=tc;

            HS := hcColsTitres.Sections.Items[0];  // ID réseau
            canvas.TextOut(HS.Left + 4, ARect.Top+1,  Format('%d',[rs.IdxReseau]));
            HS := hcColsTitres.Sections.Items[1];  // Couleur réseau
            DessineFiletColonne(HS.Left - Q4);
            canvas.Pen.Color:=clBlack;
            canvas.Brush.Color:=rs.ColorReseau; // couleur du réseau
            r.Left :=HS.Left;
            r.Right:=HS.Right - 8;
            r.Top  :=ARect.Top + mg;
            r.Bottom:=ARect.Bottom - mg;
            canvas.Rectangle(R);
            Canvas.Brush.Color:=bg;
            canvas.Pen.Color:=clSilver;
            HS := hcColsTitres.Sections.Items[2];  // Nom réseau
            DessineFiletColonne(HS.Left - Q4);


            //Canvas.Font.Color :=clBlack;
            Canvas.Font.Color :=tc;
            canvas.TextOut(HS.Left + 4, ARect.Top+1, AnsiToUtf8(rs.NomReseau));
          end;
        mslCODE:
          begin
            Canvas.FillRect(ARect);
            HS := hcColsTitres.Sections.Items[0];  // Nom réseau

            Canvas.Brush.Color:=bg;
            Canvas.Font.Color :=tc;
            canvas.Pen.Color:=clSilver;

            canvas.TextOut(HS.Left + 4  , ARect.Top+1, Format('%d', [cs.IDCode]));

            HS := hcColsTitres.Sections.Items[1];  // az
            DessineFiletColonne(HS.Left - Q4);
            canvas.TextOut(HS.Left + 4  , ARect.Top+1, Format('%.0f', [cs.GradAz]));
            HS := hcColsTitres.Sections.Items[2];  // p
            DessineFiletColonne(HS.Left - Q4);
            canvas.TextOut(HS.Left + 4, ARect.Top+1, Format('%.0f', [cs.Gradinc]));
            HS := hcColsTitres.Sections.Items[3];  // Commentaires
            DessineFiletColonne(HS.Left - Q4);
            canvas.TextOut(HS.Left + 4, ARect.Top+1, AnsiToUtf8(cs.Commentaire));
          end;
        mslEXPE:
          begin
            try
              miou := DateToStr(SafeEncodeDate(ss.AnneeExpe,ss.MoisExpe,ss.JourExpe));
            except
              miou := '01/01/2000';
            end;

            Canvas.FillRect(ARect);
            canvas.Pen.Color  :=clSilver;
            Canvas.Font.Color :=tc;

            HS := hcColsTitres.Sections.Items[0];  // ID expé
            canvas.TextOut(HS.Left + 4, ARect.Top+1,  Format('%d',[ss.IDExpe]));
            HS := hcColsTitres.Sections.Items[1];  // Couleur expé
            DessineFiletColonne(HS.Left - Q4);
            // couleur de l'expé
            canvas.Pen.Color:=clBlack;
            canvas.Brush.Color:=FMyPalette.GetColorByIndex(ss.Couleur);

            r.Left :=HS.Left;
            r.Right:=HS.Left + 32;
            r.Top  :=ARect.Top + mg;
            r.Bottom:=ARect.Bottom - mg;
            canvas.Rectangle(R);

            Canvas.Brush.Color:=bg;
            Canvas.TextOut(HS.Left + 32 + 4, ARect.Top+1, Format('%d',[ss.Couleur]));

            //Canvas.Brush.Color:=bg;

            canvas.Pen.Color:=clSilver;

            HS := hcColsTitres.Sections.Items[2];  // date
            DessineFiletColonne(HS.Left - Q4);
            canvas.TextOut(HS.Left + 4, ARect.Top+1, miou);

            HS := hcColsTitres.Sections.Items[3];  // spéléomètre
            DessineFiletColonne(HS.Left - Q4);
            canvas.TextOut(HS.Left + 4, ARect.Top+1, AnsiToUtf8(ss.Speleometre));

            HS := hcColsTitres.Sections.Items[4];  // spéléographe
            DessineFiletColonne(HS.Left - Q4);
            canvas.TextOut(HS.Left + 4, ARect.Top+1, AnsiToUtf8(ss.Speleographe));

            HS := hcColsTitres.Sections.Items[5];  // déclinaison
            DessineFiletColonne(HS.Left - Q4);
            canvas.TextOut(HS.Left + 4, ARect.Top+1, Format('%.3f',[ss.Declinaison]));


            HS := hcColsTitres.Sections.Items[6];  // commentaires
            DessineFiletColonne(HS.Left - Q4);
            canvas.TextOut(HS.Left + 4, ARect.Top+1, AnsiToUtf8(ss.Commentaire));





          end;
        mslSERIE:
          begin
             (*                  AjouterTitreColonne('ID', 40);
                    AjouterTitreColonne('Départ', 80);
                    AjouterTitreColonne('Arrivée', 80);
                    AjouterTitreColonne('Nom', 400);
                    AjouterTitreColonne('Réseau', 300);
                    AjouterTitreColonne('Nb points', 80);
              //*)
            with ListeElements do begin
              Canvas.FillRect(ARect);
              canvas.Pen.Color:=clSilver;
              Canvas.Font.Color :=tc;

              HS := hcColsTitres.Sections.Items[0];  // ID série
              canvas.TextOut(HS.Left + 4, ARect.Top+1,  Format('%d',[sr.GetIndexSerie]));
              HS := hcColsTitres.Sections.Items[1];  // Départ
              DessineFiletColonne(HS.Left - Q4);
              canvas.TextOut(HS.Left + 4, ARect.Top+1,  Format('%d.%d',[sr.GetNoSerieDep, sr.GetNoPointDep]));
              HS := hcColsTitres.Sections.Items[2];  // Arrivée
              DessineFiletColonne(HS.Left - Q4);
              canvas.TextOut(HS.Left + 4, ARect.Top+1,  Format('%d.%d',[sr.GetNoSerieArr, sr.GetNoPointArr]));
              HS := hcColsTitres.Sections.Items[3];  // nom
              DessineFiletColonne(HS.Left - Q4);
              canvas.TextOut(HS.Left + 4, ARect.Top+1, AnsiToUtf8(sr.GetNomSerie));
              // réseau
              HS := hcColsTitres.Sections.Items[4];  // nom
              DessineFiletColonne(HS.Left - Q4);
              rs := FDocToporobot.GetReseau(sr.GetNoReseau);

              canvas.Pen.Color:=clBlack;
              canvas.Brush.Color:=rs.ColorReseau;
              r.Left :=HS.Left;
              r.Right:=HS.Left + 32;
              r.Top  :=ARect.Top + mg;
              r.Bottom:=ARect.Bottom - mg;
              canvas.Rectangle(R);
              Canvas.Brush.Color:= bg;
              Canvas.TextOut(HS.Left + 32 + 4, ARect.Top+1, AnsiToUtf8(rs.NomReseau));

              Canvas.Brush.Color:=bg;

              canvas.Pen.Color:=clSilver;

              HS := hcColsTitres.Sections.Items[5];  // Nb points
              DessineFiletColonne(HS.Left - Q4);
              canvas.TextOut(HS.Left + 4, ARect.Top+1,  Format('%d',[sr.GetNbVisees]));
            end;
          end;
      end; //case FModeSelection of


    end;
  end;
begin


  case FModeSelection of
    mslENTRANCES: es := FDocToporobot.GetEntree(Index);
    mslRESEAUX: rs := FDocToporobot.GetReseau(Index);
    mslCODE:    cs := FDocToporobot.GetCode(Index);
    mslEXPE:    ss := FDocToporobot.getExpe(Index);
    mslSERIE:   sr := FDocToporobot.GetSerie(Index);

  end; //case FModeSelection of
  //ShowMessageFmt('%d',[Index]);

  with ListeElements do begin

    canvas.brush.color:=clwhite;
    //canvas.fillrect(rect);
    //on affiche le texte
    DessineItem(clwhite, clBlack);
    //affichage lorsque la ligne est sélectionnée
    if (odSelected in state) then begin
      canvas.brush.color:=clBlue;
      //canvas.fillrect(rect);
      //canvas.font.color:=clblue;
      //canvas.font.style:=Listbox1.canvas.font.style +[fsbold]+[fsitalic];
      DessineItem(clBlue, clWhite);
      //canvas.textout(rect.left+30,rect.top+2,listbox1.items[index]);
    end;

  end;
end;

procedure TdlgSelectElement.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  ListeElements.Clear;
  // libération de la palette utilisées par le mode Expés
  if (FModeSelection = mslEXPE) then begin
    try
      ;
    finally
      FMyPalette.Free;
    end;
  end;
end;

procedure TdlgSelectElement.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
end;

procedure TdlgSelectElement.hcColsTitresResize(Sender: TObject);
begin
end;

procedure TdlgSelectElement.hcColsTitresSectionResize(
  HeaderControl: TCustomHeaderControl; Section: THeaderSection);
begin
    ListeElements.Invalidate;
end;

procedure TdlgSelectElement.ListeElementsClick(Sender: TObject);
begin
  FNumeroElement := ListeElements.ItemIndex;
end;

procedure TdlgSelectElement.BitBtn1Click(Sender: TObject);
begin
end;

procedure TdlgSelectElement.BitBtn2Click(Sender: TObject);
begin
  FNumeroElement := -1;
end;




end.

