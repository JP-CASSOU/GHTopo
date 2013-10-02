unit CadreVue3D;
// Date: 19/04/2012
// Statut: Fonctionnel
// TODO: Des détails à revoir
// 20/04/2012: DONE: Le type TModeDessin ne comporte que deux états -> remplacé par un booléen

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
  Common,
  UnitEntites,
  Dialogs,
  Graphics,
  ClasseMaillage,
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls;

type TModeDraw3D = (mc3D_COLOR, mc3D_GRAY);
type TFormatOutput  = (foPOSTSCRIPT, foDXF2D);
type TViseesTopo2D = record
  ColorEntite         : integer;                 // ID unique de l'entité
  Type_Entite       : TTypeDeVisee;             // Type d'entité
  DateLeve          : TDateTime;
  Sub_System        : integer;                 // Sous-réseau
  Une_Station_1_X   : double;                  // Extrémités des visées
  Une_Station_1_Y   : double;
  Une_Station_2_X   : double;                // Extrémités des visées
  Une_Station_2_Y   : double;
  CoordEcrSt1X      : integer;
  CoordEcrSt2X      : integer;
  CoordEcrSt2Y      : integer;
  CoordEcrSt1Y      : integer;
  Depth             : Double;                // Profondeur (distance à l'observateur);
end;
type TPoint2DDepth = record
  X    : double;
  Y    : double;
  Depth: double;
end;
type TQuad = record
   Visible           : Boolean;
   Drawn             : Boolean; // dessiné en fonction du MétaFiltre;
   VertexA           : TPoint3Df;
   VertexB           : TPoint3Df;
   VertexC           : TPoint3Df;
   VertexD           : TPoint3Df;
   Normale           : TPoint3Df;
   Depth             : Double;
   Vertex2D_A        : TPoint;
   Vertex2D_B        : TPoint;
   Vertex2D_C        : TPoint;
   Vertex2D_D        : TPoint;
   Couleur           : TColor;
   Type_Entite       : TTypeDeVisee;
end;


type

  { TCdrVue3D }

  TCdrVue3D = class(TFrame)
    Vue: TPaintBox;
    pnlVue: TPanel;
    procedure FrameResize(Sender: TObject);
    procedure VueMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure VueMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure VuePaint(Sender: TObject);
  private
    { private declarations }
    FNomFichierEntites: string;
    FTableEntites: TTableDesEntites;

    // filtres
    FFiltres     : string;
    // Peut-on dessiner ?
    FDoDraw: Boolean;
    // angles
    FTheta       : double;
    FPhi         : double;
    // magnification
    FCoefMagnification: Double;
    // zoom
    FZoom        : double;
    // Translation
    FOffset       : TPoint3Df;
    // vecteur observateur (direction d'observation)
    FLookAt       : TPoint3Df;
    // cube
    FCube         : array[1..8] of TPoint;
    FColorCube    : TColor;
    FBackGround   : TColor;
    // table des polygonales
    FPolygonale  : array of TViseesTopo2D;
    // table des facettes
    FTableQuads  : array of TQuad;
    // mode de dessin : Polygonale ou solide
    // TModeDessin ne comporte que deux états -> remplacé par un booléen
    //FModeDessin : TModeDessin;
    FDoDrawWalls: boolean;
    // paramètres de la matrice de transformation
    //FTrMat: array[0..4, 0..4] of Double;
    Aux: array[1..8] of Double;
    // paramètres internes
    FT: TPoint;
    FRapportME: Double;
    FDiagonal: Double;
    // souris
    FOldMousePos : TPoint;
    // modes de représentation
    FModeRepresentationGaleries: TModeRepresentationGaleries;

    procedure CalcProjPolygonale;
    function Get2DDepthCoordinates(const X, Y, Z: double): TPoint2DDepth;
    function GetScreenCoordinates(const X, Y: double): TPoint;

    procedure PrecalculVolumesCavite;
    procedure QSortDatasByDepth;



    procedure ResetParamTransformation;
    procedure SetModeDessin(const MD: boolean);
    // paramètres de trafsformation, angles en degrés
    procedure SetParamTransformation(const QTheta, QPhi, QZoom, QMagnification: double);
  public
    { public declarations }
    function Initialise(const FileName: string): boolean;
    procedure RegenererVue(const Theta, Phi, Zoom, Magn: double;
                           const Filtres: string; const AvecVolumes: boolean);
    procedure ApplyMetaFiltre(const F: string);
    procedure MakeVolumesCavite;
    procedure RedessinVue;

    function  GetPointerTableEntites: TTableDesEntites;

  end; 

implementation
const
  PI180 = PI/180;
  NBFACESBYVISEE = 6;
  LOWINDEX = 1;

{$R *.lfm}
procedure TCdrVue3D.SetModeDessin(const MD: Boolean);
begin
  FDoDrawWalls := MD;
end;

function TCdrVue3D.Initialise(const FileName: string): Boolean;
begin
  Result := False;

  AfficherMessage(Format('%s.Initialize: %s',[ClassName, FileName]));
  // Le double-tampon est désactivé'

  //AfficherMessage(' -- Double buffer: ' + BoolToStr(self.DoubleBuffered, 'Actif', 'Inactif'));
  //AfficherMessage(' -- Double buffer: ' + BoolToStr(pnlVue.DoubleBuffered, 'Actif', 'Inactif'));
  FDoDraw:= False;
  // tout dessiner
  FDoDrawWalls := true;
  // mode de dessin par défaut = séances
  FModeRepresentationGaleries := rgSEANCES;
  // arrière plan
  FBackGround := clWhite;
  // chargement entités
  FTableEntites := TTableDesEntites.Create;
  try
    if (FTableEntites.LoadEntites(FileName, True) = 0) then
    begin
      ShowMessage(AnsiToUtf8(rsMSG_VUE3D_FAIL));
      Exit;
    end;
    // préparation tables temporaires
    with FTableEntites do
    begin
      // mini et maxi
      SetMinMax;
      // filtres
      MetaFiltre('');
      // initialisation des tables d'affichage
      SetLength(FPolygonale,0);
      SetLength(FPolygonale, GetNbEntites + 1);
      SetLength(FTableQuads, 0);
      SetLength(FTableQuads, GetNbEntites * NBFACESBYVISEE);
    end;
    // mise en place des paramètres
    ResetParamTransformation;
    (*
    with cmbModeDessinConduits do begin
      Clear;
      Items.Add('Séances');
      Items.Add('Réseaux');
      Items.Add('Gris');
      Items.Add('Altitudes');
      ItemIndex := 0;
    end;
    //*)
    // construction des volumes et précalcul
    MakeVolumesCavite;
    PrecalculVolumesCavite;
     // trier la topo
    QSortDatasByDepth;
    // redessin
    RedessinVue;
    //Vue.Invalidate;
    // OK ?
    Result := True;


  except
  end;
end;


procedure TCdrVue3D.VuePaint(Sender: TObject);
begin
  RedessinVue;
end;

procedure TCdrVue3D.VueMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
const M1 = 2;
var
  dy: Integer;
  dx: Integer;
begin
  // Fonctionne très bien mais est lent sur Lazarus
  (*
  if Shift=[ssLeft] then  begin
    dx:=X - FOldMousePos.X;
    if dx<0 then dx:=0;
    if dx>360 then dx:=dx-360;
    dy:=Y - FOldMousePos.Y;
    if dy<0 then dy:=0;
    if dy>(90 * M1) then dy:=(90*M1);
    FTheta := dx;
    FPhi   := dy / M1;
    SetParamTransformation(FTheta, FPhi, 1.00, 1.00);
    CalcProjPolygonale;
    SetModeDessin(False);
    //ModeDessin:=mdPOLYGONALE;
    //RedessinVue;
    Vue.Invalidate;
  end;
  //*)
end;

procedure TCdrVue3D.FrameResize(Sender: TObject);
begin
  Vue.Invalidate;
end;

procedure TCdrVue3D.VueMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  // Fonctionne très bien mais est lent sur Lazarus
  (*
  // précalculer les volumes
  SetParamTransformation(FTheta, FPhi, 1.00, 1.00);
  CalcProjPolygonale;
  PrecalculVolumesCavite;
  // trier la topo
  QSortDatasByDepth;
  SetModeDessin(True);
  RedessinVue;
  //*)
end;

procedure TCdrVue3D.RegenererVue(const Theta, Phi, Zoom, Magn: double; const Filtres: string; const AvecVolumes: boolean);
begin

  SetParamTransformation(Theta, Phi, Zoom, Magn);
  // calcul polygonale
  CalcProjPolygonale;
  if (AvecVolumes) then
  begin
    // filtres
    ApplyMetaFiltre(Filtres);

    // précalculer les volumes
    MakeVolumesCavite;
    PrecalculVolumesCavite;
    // trier la topo
    QSortDatasByDepth;
  end;
  SetModeDessin(AvecVolumes);
  Vue.Invalidate;
  //RedessinVue;
end;

procedure TCdrVue3D.ApplyMetaFiltre(const F: string);
begin
  FTableEntites.MetaFiltre(F);
end;

// angles en degrés
procedure TCdrVue3D.SetParamTransformation(const QTheta, QPhi, QZoom, QMagnification: double);
const
  Marge = 5;
var
  c: TPoint2DDepth;
  cnMini, cnMaxi: TPoint3Df;
begin
  FZoom  := QZoom;
  FTheta := QTheta;
  FPhi   := QPhi;
  FCoefMagnification := QMagnification;

  AfficherMessage(Format('%s.setParamTransformation(Theta = %.0f; Phi = %.0f; Zoom: %.3f; Magn. = %.3f',
                        [ClassName, QTheta, QPhi, QZoom, QMagnification]));
  Aux[1] := Sin(FTheta * PI180);
  Aux[2] := Sin(FPhi   * PI180); // | -sin(A)           +cos(A)         0           0 | | x |   | X |
  Aux[3] := Cos(FTheta * PI180); // |                                                 | |   |   |   |
  Aux[4] := Cos(FPhi   * PI180); // | -cos(A).sin(P)    -sin(A).sin(P)  cos(A)      0 | | y |   | Y |
  Aux[5] := Aux[3] * Aux[2];      // |                                                 |*|   | = |   |
  Aux[6] := Aux[1] * Aux[2];      // | -cos(A).cos(P)    -sin(A).cos(P)  -sin(P)     R | | z |   | Z |
  Aux[7] := Aux[3] * Aux[4];      // |                                                 | |   |   |   |
  Aux[8] := Aux[1] * Aux[4];      // | 0                 0               0           1 | | 1 |   | 1 |

  FT.X := 0;
  FT.Y := 0;
  FT.X :=  Vue.Width  div 2;
  FT.Y :=  Vue.Height div 2;
  FRapportME:=(Vue.Width/FDiagonal ) * FZoom;
  //AfficherMessage(Format('FRapportME=%.5f FT=(%d, %d)',
  //                       [FRapportME, FT.X, FT.Y]));
  //SetTransformationMatrix(FTheta, FPhi, FZoom, FOffset);
  CalcProjPolygonale();
  // direction d'observation
  FLookAt.X:=Cos(FPhi * PI180)*Cos(FTheta * PI180);
  FLookAt.Y:=Cos(FPhi * PI180)*sin(FTheta * PI180);

  FLookAt.Z:=sin(FPhi * PI180);

  // cube
  cnMini := FTableEntites.GetCoinBasGauche;
  cnMaxi := FTableEntites.GetCoinHautDroit;
  with FTableEntites do begin
    c:=Get2DDepthCoordinates(cnMini.X, cnMini.Y, cnMini.Z);
    FCube[1]:=GetScreenCoordinates(c.X, C.Y);
    c:=Get2DDepthCoordinates(cnMaxi.X, cnMini.Y, cnMini.Z);
    FCube[2]:=GetScreenCoordinates(c.X, C.Y);
    c:=Get2DDepthCoordinates(cnMaxi.X, cnMaxi.Y, cnMini.Z);
    FCube[3]:=GetScreenCoordinates(c.X, C.Y);
    c:=Get2DDepthCoordinates(cnMini.X, cnMaxi.Y, cnMini.Z);
    FCube[4]:=GetScreenCoordinates(c.X, C.Y);
    c:=Get2DDepthCoordinates(cnMini.X, cnMini.Y, cnMaxi.Z);
    FCube[5]:=GetScreenCoordinates(c.X, C.Y);
    c:=Get2DDepthCoordinates(cnMaxi.X, cnMini.Y, cnMaxi.Z);
    FCube[6]:=GetScreenCoordinates(c.X, C.Y);
    c:=Get2DDepthCoordinates(cnMaxi.X, cnMaxi.Y, cnMaxi.Z);
    FCube[7]:=GetScreenCoordinates(c.X, C.Y);
    c:=Get2DDepthCoordinates(cnMini.X, cnMaxi.Y, cnMaxi.Z);
    FCube[8]:=GetScreenCoordinates(c.X, C.Y);
  end;
  // précalculer les volumes
  //PrecalculVolumesCavite;
  // trier la topo
  //QSortDatasByDepth;
  FDoDraw:=True;
  //*********
  //editTheta.Value:=FTheta;
  //editPhi.Value  :=FPhi;
end;
// remise à zéro des paramètres de transformation
procedure TCdrVue3D.ResetParamTransformation;
var
  cnMini, cnMaxi: TPoint3Df;
begin
  cnMini := FTableEntites.GetCoinBasGauche;
  cnMaxi := FTableEntites.GetCoinHautDroit;
  with FTableEntites do begin
    FDiagonal:=Hypot3D(cnMaxi.X-cnMini.X,
                       cnMaxi.Y-cnMini.Y,
                       cnMaxi.Z-cnMini.Z);
    AfficherMessage(Format('   Mini: %.2f - %.2f - %.2f',
                           [cnMini.X, cnMini.Y, cnMini.Z]));
    AfficherMessage(Format('   Maxi: %.2f - %.2f - %.2f',
                           [cnMaxi.X, cnMaxi.Y, cnMaxi.Z]));


    FOffset.X := -(cnMini.X+ 0.5*(cnMaxi.X - cnMini.X));
    FOffset.Y := -(cnMini.Y+ 0.5*(cnMaxi.Y - cnMini.Y));
    FOffset.Z := -(cnMini.Z+ 0.5*(cnMaxi.Z - cnMini.Z));

    AfficherMessage(Format('   Offset: %.2f - %.2f - %.2f',
                           [FOffset.X, FOffset.Y, FOffset.Z]));
  end;
  SetParamTransformation(0.00, 0.00, 1.00, 1.00);
end;
function TCdrVue3D.Get2DDepthCoordinates(const X,Y,Z: double): TPoint2DDepth;
begin
  Result.X := -(X+FOffset.X) * Aux[1] +
               (Y+FOffset.Y) * Aux[3];
  Result.Y := -(X+FOffset.X) * Aux[5]
              -(Y+FOffset.Y) * Aux[6]+
               (FCoefMagnification*(Z+FOffset.Z)) * Aux[4];
  Result.Depth:= (-(X+FOffset.Z) * Aux[7]
                  -(Y+FOffset.Z) * Aux[8]
                  -(Z+FOffset.Z) * Aux[2]);
end;
function TCdrVue3D.GetScreenCoordinates (const X,Y: double): TPoint;
begin
  Result.X := round(X * FRapportME) + FT.X;
  Result.Y := Vue.Height -
              (round(Y * FRapportME) + FT.Y);
end;

//*****************************
// tri en profondeur
procedure TCdrVue3D.QSortDatasByDepth;
  procedure QSortByDepth(var FPolygonale: array of TViseesTopo2D;
                         lidx, ridx: integer);
  var
    k, e, mid: integer;
    Buffer   : TViseesTopo2D;
    bidon1,
    bidon2   : double;
  begin
    with self do
    begin
      if lidx >= ridx then Exit;
      mid := (lidx + ridx) div 2;
      Buffer := FPolygonale[lidx];
      FPolygonale[lidx]:=FPolygonale[mid];
      FPolygonale[mid]:=Buffer;
      e:=lidx;
      for k:=lidx+1 to ridx do
      begin
        if  FPolygonale[k].Depth > FPolygonale[lidx].Depth  then
        begin
          Inc(e);
          Buffer := FPolygonale[e];
          FPolygonale[e]:=FPolygonale[k];
          FPolygonale[k]:=Buffer;
        end;
      end;
      Buffer := FPolygonale[lidx];
      FPolygonale[lidx]:=FPolygonale[e];
      FPolygonale[e]:=Buffer;
      QSortByDepth(FPolygonale,lidx, e-1);
      QSortByDepth(FPolygonale,e+1, ridx);
    end;
  end;
  // tri des facettes
  procedure QSortQuadsByDepth(var FTableQuads: array of TQuad;
                             lidx, ridx: integer);
  var
    k, e, mid: integer;
    Buffer   : TQuad;
    bidon1,
    bidon2   : single;
  begin
    with self do
    begin
      if lidx >= ridx then Exit;
      mid := (lidx + ridx) div 2;
      Buffer := FTableQuads[lidx];
      FTableQuads[lidx]:=FTableQuads[mid];
      FTableQuads[mid]:=Buffer;
      e:=lidx;
      for k:=lidx+1 to ridx do
      begin
        if  FTableQuads[k].Depth > FTableQuads[lidx].Depth  then
        begin
          Inc(e);
          Buffer := FTableQuads[e];
          FTableQuads[e]:=FTableQuads[k];
          FTableQuads[k]:=Buffer;
        end;
      end;
      Buffer := FTableQuads[lidx];
      FTableQuads[lidx]:=FTableQuads[e];
      FTableQuads[e]:=Buffer;
      QSortQuadsByDepth(FTableQuads,lidx, e-1);
      QSortQuadsByDepth(FTableQuads,e+1, ridx);
   end;
  end;
begin
  AfficherMessage(Format('%s.QSortByDepths',[ClassName]));

  // tri de la polygonale;
  QSortByDepth(FPolygonale, LOWINDEX, FTableEntites.GetNbEntites - 1);
  // tri des volumes
  //AfficherMessage(Format('%d, %d, %d',[ High(FPolygonale),High(FPolygonale), High(FTableQuads)]));
  QSortQuadsByDepth(FTableQuads, 0, (FTableEntites.GetNbEntites - 1) * NBFACESBYVISEE);

end;
// calcul de la polygonale projetée
procedure TCdrVue3D.CalcProjPolygonale;
var
  i:integer;
  V: TViseesTopo2D;
  E : TEntite;
  d1: double;
  PtOut: TPoint2DDepth;
  P: TPoint;
begin
  for i:=LOWINDEX to FTableEntites.GetNbEntites - 1 do
  begin
    E:=FTableEntites.GetEntite(i);
    case FModeRepresentationGaleries of
       rgSEANCES : V.ColorEntite  := E.ColorEntite;
       rgRESEAUX : V.ColorEntite  := E.ColorReseau;
       rgGRAY    : V.ColorEntite  := clSilver;
    else
      V.ColorEntite  := E.ColorEntite;
    end;

    //V.ColorEntite  := E.ColorEntite;
    V.Type_Entite  := E.Type_Entite;
    V.DateLeve     := E.DateLeve;
    PtOut :=  Get2DDepthCoordinates(E.Une_Station_1_X,
                                    E.Une_Station_1_Y,
                                    E.Une_Station_1_Z);
    V.Une_Station_1_X := PtOut.X;
    V.Une_Station_1_Y := PtOut.Y;
    P:=GetScreenCoordinates(PtOut.X, PtOut.Y);
    V.CoordEcrSt1X:=P.X;
    V.CoordEcrSt1Y:=P.Y;
    d1:=PtOut.Depth;
    PtOut :=  Get2DDepthCoordinates(E.Une_Station_2_X,
                                    E.Une_Station_2_Y,
                                    E.Une_Station_2_Z);
    V.Une_Station_2_X := PtOut.X;
    V.Une_Station_2_Y := PtOut.Y;
    V.Depth           := 0.50 * (d1+PtOut.Depth);
    P:=GetScreenCoordinates(PtOut.X, PtOut.Y);
    V.CoordEcrSt2X:=P.X;
    V.CoordEcrSt2Y:=P.Y;
    FPolygonale[i]:=V;
  end;
end;

// redessin de la vue
procedure TCdrVue3D.RedessinVue;
var
  TmpBuffer: TBitMap;
  R        : TRect;
  procedure DrawCube;
  var i: integer;
  begin
    AfficherMessage('  --> Cube');
    with TmpBuffer.Canvas do begin
      Pen.Color:=FColorCube;
      for i:=2 to 4 do begin
        MoveTo(FCube[i].X, FCube[i].Y);
        LineTo(FCube[i+4].X, FCube[i+4].Y);
      end;
      MoveTo(FCube[1].X, FCube[1].Y);
      LineTo(FCube[2].X, FCube[2].Y);
      LineTo(FCube[3].X, FCube[3].Y);
      LineTo(FCube[4].X, FCube[4].Y);
      LineTo(FCube[1].X, FCube[1].Y);
      LineTo(FCube[5].X, FCube[5].Y);
      LineTo(FCube[6].X, FCube[6].Y);
      LineTo(FCube[7].X, FCube[7].Y);
      LineTo(FCube[8].X, FCube[8].Y);
      LineTo(FCube[5].X, FCube[5].Y);
    end;
  end;
  procedure DrawEntrances;
  const R666 = 3;
  var
    i: integer;
    ETT: TEntite;
  begin
    CalcProjPolygonale();
    AfficherMessage('  --> Entrees');
    with TmpBuffer.Canvas do begin
      Pen.Color:=clRED;
      Brush.Color:=clFuchsia;
      for i:=1 to FTableEntites.GetNbEntites - 1 do begin
        ETT:=FTableEntites.GetEntite(i);
        if ETT.Type_Entite=tgENTRANCE then begin
          with FPolygonale[i] do begin
           Ellipse(CoordEcrSt2X-R666, CoordEcrSt2Y-R666,
                   CoordEcrSt2X+R666, CoordEcrSt2Y+R666);

          end;
        end;
      end;
    end;
    //*)
  end;

  procedure DrawPolygonales;
  const sz=4;
  var i: integer;
  begin
    AfficherMessage('  --> Polygonales');
    with TmpBuffer.Canvas do begin
      //for i:=Low(FPolygonale) to High(FPolygonale) do begin
      for i:=1 to FTableEntites.GetNbEntites - 1 do begin
        with FPolygonale[i] do begin
          if Type_Entite=tgEntrance then Continue;
          Pen.Color:=ColorEntite;
          MoveTo(CoordEcrSt1X, CoordEcrSt1Y);
          LineTo(CoordEcrSt2X, CoordEcrSt2Y);
        end;
      end;
    end;
  end;
  procedure DessinerQuad(const QD: TQuad);
  const
    foo = -50;
  var
    P: array[0..3] of TPoint;
    v: integer;
    r,g,b: byte;
  begin
    with TmpBuffer.Canvas do begin
      with QD do begin
        if Not(Drawn) then Exit;
        if Not(Visible) then Exit;
        P[0]:=Vertex2D_A;  P[1]:=Vertex2D_B;
        P[2]:=Vertex2D_C;  P[3]:=Vertex2D_D;

        Pen.Color:=clBlack;
        Brush.Color:=QD.Couleur;

        //Pen.Color:=clGray;

        MoveTo(P[0].X, P[0].Y);
        for v:=1 to 3 do
          LineTo(P[v].X,P[v].Y);
        Polygon(P);
      end; //  with QD do begin
    end; // with TmpBuffer.Canvas do begin
  end;
  procedure DrawCavite;
  var i: integer;
  begin       ;
    AfficherMessage('  --> Cavite');
    //Pen.Width:=0;
    for i:=2 to FTableEntites.GetNbEntites - 1 do begin
      DessinerQuad(FTableQuads[NBFACESBYVISEE *(i-1) + 1]);
      DessinerQuad(FTableQuads[NBFACESBYVISEE *(i-1) + 2]);
      DessinerQuad(FTableQuads[NBFACESBYVISEE *(i-1) + 3]);
      DessinerQuad(FTableQuads[NBFACESBYVISEE *(i-1) + 4]);
      DessinerQuad(FTableQuads[NBFACESBYVISEE *(i-1) + 5]);
      DessinerQuad(FTableQuads[NBFACESBYVISEE *(i-1) + 6]);
    end;
  end;
  // dessin du référentiel
  procedure DrawReferentiel;
  const
    rSz = 40;
    rXo = 5 + rSz;


  var
    rYo       : integer;
    X1, X2, X3: TPoint3Df;
    R1, R2, R3: TPoint2Df;
    Rp        : TPoint;
  begin
    // calculs préliminaires
    X1.X  := 1.0; X1.Y  := 0.0; X1.Z  := 0.0; //X1.T  := 1.0;
    X2.X  := 0.0; X2.Y  := 1.0; X2.Z  := 0.0; //X2.T  := 1.0;
    X3.X  := 0.0; X3.Y  := 0.0; X3.Z  := 1.0; //X3.T  := 1.0;
    // transformations
    R1.X := -X1.X * Aux[1] +
             X1.Y * Aux[3];
    R1.Y := -X1.X * Aux[5] +
            -X1.Y * Aux[6] +
             X1.Z * FCoefMagnification * Aux[4];
    R2.X := -X2.X * Aux[1] +
             X2.Y * Aux[3];
    R2.Y := -X2.X * Aux[5] +
            -X2.Y * Aux[6] +
             X2.Z * FCoefMagnification * Aux[4];
    R3.X := -X3.X * Aux[1] +
             X3.Y * Aux[3];
    R3.Y := -X3.X * Aux[5] +
            -X3.Y * Aux[6] +
             X3.Z * FCoefMagnification * Aux[4];
    with TmpBuffer.Canvas do begin
      rYo := Height - rSz shl 1;
      Brush.Color:=FBackGround;
      Font.Color:=clBlue;

      Pen.Color:=clRed;
      MoveTo(rXo, rYo);

      Rp.x := rXo + round(rSz * R1.X);
      Rp.y := rYo - round(rSz * R1.Y);
      LineTo(Rp.X, Rp.Y);
      TextOut(Rp.x+2, Rp.y+2,'X');


      MoveTo(rXo, rYo);
      Rp.x := rXo + round(rSz * R2.X);
      Rp.y := rYo - round(rSz * R2.Y);
      LineTo(Rp.X, Rp.Y);
      TextOut(Rp.x+2, Rp.y+2,'Y');

      MoveTo(rXo, rYo);
      Rp.x := rXo + round(rSz * R3.X);
      Rp.y := rYo - round(rSz * R3.Y);
      LineTo(Rp.X, Rp.Y);
      TextOut(Rp.x+2, Rp.y+2,'Z');

    end;
  end;
  (*
  procedure DrawAnnotations;
  var
    i : integer;
    AN: TAnnotation;
    P2D: TPoint2DDepth;
    PP: TPoint;
    E: TEntite;
    PosTexte: TPoint3Df;
  begin
    AfficherMessage('-->DrawTextes');
    //if FTableEntites.LesAnnotationsSontOK
    if FTableEntites.MyTableAnnotations.Count = 0 then Exit;
    for i := 0 to FTableEntites.MyTableAnnotations.NbAnnotations - 1 do begin
     try
      AN:=FTableEntites.MyTableAnnotations.GetAnnotation(i);
      if Not AN.Displayed then Continue;
      //AfficherMessage(Format(' --- %d - %s',[i, AN.Caption]));
      case AN.ModePosition of
        0: begin // positionnement absolu
             PosTexte.X := AN.X;
             PosTexte.Y := AN.Y;
             PosTexte.Z := AN.Z;



           end;
        1: begin // positionnement relatif à une station
             E := FTableEntites.GetEntiteFromSerSt(AN.BaseRefSer, AN.BaseRefSt);
             PosTexte.X := E.Une_Station_2_X + AN.DX;
             PosTexte.Y := E.Une_Station_2_Y + AN.DY;
             PosTexte.Z := E.Une_Station_2_Z + AN.DZ;
          end;
      end;
      //vérifier si on est ds le cube
      if ( IsInRange(PosTexte.X, FTableEntites.cnMini.X, FTableEntites.cnMaxi.X) and
           IsInRange(PosTexte.Y, FTableEntites.cnMini.Y, FTableEntites.cnMaxi.Y) and
           IsInRange(PosTexte.Z, FTableEntites.cnMini.Z, FTableEntites.cnMaxi.Z) )
      then begin
        P2D := Get2DDepthCoordinates(PosTexte.X, PosTexte.Y, PosTexte.Z);
        PP := GetScreenCoordinates(P2D.X, P2D.Y);
        // dessin du texte
        TmpBuffer.Canvas.TextOut(PP.X, PP.Y, LLANFAIR_PG(AN.Caption, AN.MaxLength,
                                 AN.Z, AN.BaseRefSer, AN.BaseRefSt));
      end;
     except
       AfficherMessage(Format(' ---> %d - %s',[i, AN.Caption]));
     end;
    end;
  end;
  //*)
begin
  if Not(FDoDraw) then Exit;
  try
    TmpBuffer:=TBitmap.Create;
    with TmpBuffer do begin
      Height:= Vue.Height ;
      Width := Vue.Width;
      R.Left:= Vue.Left;
      R.Top := Vue.Top;
      R.Bottom :=Vue.Top+Vue.Height;
      R.Right  :=Vue.Left+Vue.Width;

      Canvas.Pen.Mode:=pmCopy;
      Canvas.Brush.Color := FBackGround;
      Canvas.FillRect(R);

      DrawPolygonales; // polygonales
      if (FDoDrawWalls) then
      begin
        DrawCavite;      // dessin des volumes
        //DrawAnnotations; // dessin des textes
      end;
      DrawEntrances;   // entrées
      DrawCube;        // cube (en dernier)
      DrawReferentiel; // dessin du référentiel
    end;
    Vue.Canvas.CopyRect(R, TmpBuffer.Canvas, R);
  finally

    TmpBuffer.Free;
  end;
end;

function TCdrVue3D.GetPointerTableEntites: TTableDesEntites;
begin
  Result := FTableEntites;
end;

//----------------------------------------------------------
// fabrication des volumes de la scène
procedure TCdrVue3D.MakeVolumesCavite;
var
  i: integer;
  P1, P2, P3, P4: TPoint3Df;
  E: TEntite;
  QC : TColor;
  procedure MakeQuad(const NQ: integer; const C: TColor; const TE: TTypeDeVisee; const DR: Boolean);
  var
    Q: TQuad;
  begin
    with Q do
    begin
      Drawn := DR;
      VertexA:=P1;
      VertexB:=P2;
      VertexC:=P3;
      VertexD:=P4;
      Couleur:=C;
      Type_Entite:=TE;
      Visible:=True;
    end;
    FTableQuads[NQ]:=Q;
  end;
begin
  AfficherMessage(Format('%s.MakeVolumesCavite',[ClassName]));
  for i:=LOWINDEX to FTableEntites.GetNbEntites - 1 do begin
    E:=FTableEntites.GetEntite(i);
    //if E.Type_Entite=tgENTRANCE then E.
    with E do begin
      case FModeRepresentationGaleries of
        rgSEANCES : QC := ColorEntite;
        rgRESEAUX : QC := ColorReseau;
        rgGRAY    : QC := clSilver;
      else
        QC := ColorEntite;
      end;
      // paroi gauche Quad 1
      P1.X := X2PG;    P2.X := X1PG;    P3.X := X1PG;    P4.X := X2PG;
      P1.Y := Y2PG;    P2.Y := Y1PG;    P3.Y := Y1PG;    P4.Y := Y2PG;
      P1.Z := Z2PB;    P2.Z := Z1PB;    P3.Z := Z1PH;    P4.Z := Z2PH;
      MakeQuad(NbFacesByVisee*(i-1) + 1, QC, Type_Entite, E.Drawn);
      // paroi droite Quad 2
      P1.X := X1PD;    P2.X := X2PD;    P3.X := X2PD;    P4.X := X1PD;
      P1.Y := Y1PD;    P2.Y := Y2PD;    P3.Y := Y2PD;    P4.Y := Y1PD;
      P1.Z := Z1PB;    P2.Z := Z2PB;    P3.Z := Z2PH;    P4.Z := Z1PH;
      MakeQuad(NbFacesByVisee*(i-1) + 2, QC, Type_Entite, E.Drawn);
      // paroi de dessus Quad 3
      P1.X := X1PD;    P2.X := X2PD;    P3.X := X2PG;    P4.X := X1PG;
      P1.Y := Y1PD;    P2.Y := Y2PD;    P3.Y := Y2PG;    P4.Y := Y1PG;
      P1.Z := Z1PH;    P2.Z := Z2PH;    P3.Z := Z2PH;    P4.Z := Z1PH;
      MakeQuad(NbFacesByVisee*(i-1) + 3, QC, Type_Entite, E.Drawn);
      // paroi de dessous Quad 4
      P1.X := X1PG;    P2.X := X2PG;    P3.X := X2PD;    P4.X := X1PD;
      P1.Y := Y1PG;    P2.Y := Y2PG;    P3.Y := Y2PD;    P4.Y := Y1PD;
      P1.Z := Z1PB;    P2.Z := Z2PB;    P3.Z := Z2PB;    P4.Z := Z1PB;
      MakeQuad(NbFacesByVisee*(i-1) + 4, QC, Type_Entite, E.Drawn);
      // paroi de face
      P1.X := X1PG;    P2.X := X1PD;    P3.X := X1PD;    P4.X := X1PG;
      P1.Y := Y1PG;    P2.Y := Y1PD;    P3.Y := Y1PD;    P4.Y := Y1PG;
      P1.Z := Z1PB;    P2.Z := Z1PB;    P3.Z := Z1PH;    P4.Z := Z1PH;
      MakeQuad(NbFacesByVisee*(i-1) + 5, QC, Type_Entite, E.Drawn);
      // paroi de derrière
      P1.X := X2PD;    P2.X := X2PG;    P3.X := X2PG;    P4.X := X2PD;
      P1.Y := Y2PD;    P2.Y := Y2PG;    P3.Y := Y2PG;    P4.Y := Y2PD;
      P1.Z := Z2PB;    P2.Z := Z2PB;    P3.Z := Z2PH;    P4.Z := Z2PH;
      MakeQuad(NbFacesByVisee*(i-1) + 6, QC, Type_Entite, E.Drawn);
    end; //with E do begin
  end; //for i:=0 to FTableEntites do begin
end;
// précalcul des volumes
procedure TCdrVue3D.PrecalculVolumesCavite;
var
  Q: integer;
  PP: TPoint2DDepth;
  M,U,V,W1, W2 : TPoint3Df;
  NW, PS: Double;
  procedure Calc3DQuad(var QD: TQuad);
  begin
    with QD do begin
      PP:=Get2DDepthCoordinates(VertexA.X, VertexA.Y, VertexA.Z);
      Vertex2D_A := GetScreenCoordinates(PP.X, PP.Y);
      PP:=Get2DDepthCoordinates(VertexB.X, VertexB.Y, VertexB.Z);
      Vertex2D_B := GetScreenCoordinates(PP.X, PP.Y);
      PP:=Get2DDepthCoordinates(VertexC.X, VertexC.Y, VertexC.Z);
      Vertex2D_C := GetScreenCoordinates(PP.X, PP.Y);
      PP:=Get2DDepthCoordinates(VertexD.X, VertexD.Y, VertexD.Z);
      Vertex2D_D := GetScreenCoordinates(PP.X, PP.Y);
        //test de visibilité
        U.X:=VertexB.X-VertexA.X;  V.X:= VertexC.X-VertexA.X;
        U.Y:=VertexB.Y-VertexA.Y;  V.Y:= VertexC.Y-VertexA.Y;
        U.Z:=VertexB.Z-VertexA.Z;  V.Z:= VertexC.Z-VertexA.Z;
        W1:=ProduitVectoriel(U,V,False);
        U.X:=VertexC.X-VertexA.X;  V.X:= VertexD.X-VertexA.X;
        U.Y:=VertexC.Y-VertexA.Y;  V.Y:= VertexD.Y-VertexA.Y;
        U.Z:=VertexC.Z-VertexA.Z;  V.Z:= VertexD.Z-VertexA.Z;
        W2:=ProduitVectoriel(U,V,False);
        W1.X:=W1.X+W2.X;
        W1.Y:=W1.Y+W2.Y;
        W1.Z:=W1.Z+W2.Z;
        NW:=1e-12+Hypot3D(W1.X, W1.Y, W1.Z);
        PS:=W1.X*FLookAt.X+
            W1.Y*FLookAt.Y+
            W1.Z*FLookAt.Z;
        Visible:=((PS/NW)>0);
        // calcul de profondeur
        M.X:=0.25*(VertexA.X+VertexB.X+VertexC.X+VertexD.X);
        M.Y:=0.25*(VertexA.Y+VertexB.Y+VertexC.Y+VertexD.Y);
        M.Z:=0.25*(VertexA.Z+VertexB.Z+VertexC.Z+VertexD.Z);
        Depth := -Aux[7] * M.X
                 -Aux[8] * M.Y
                 -Aux[2] * M.Z;
      end;
    end; // with QD
begin
    for Q:=LOWINDEX to FTableEntites.GetNbEntites - 1 do begin
      Calc3DQuad(FTableQuads[NbFacesByVisee*(Q-1) + 1]);
      Calc3DQuad(FTableQuads[NbFacesByVisee*(Q-1) + 2]);
      Calc3DQuad(FTableQuads[NbFacesByVisee*(Q-1) + 3]);
      Calc3DQuad(FTableQuads[NbFacesByVisee*(Q-1) + 4]);
      Calc3DQuad(FTableQuads[NbFacesByVisee*(Q-1) + 5]);
      Calc3DQuad(FTableQuads[NbFacesByVisee*(Q-1) + 6]);
    end;
end; // precalcul


end.

