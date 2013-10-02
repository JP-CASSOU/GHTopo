unit CadreVisuOpenGL;
// Date : 15/05/2012
// Nécessite l'installation du composant LazarusOpenGL
// ouvrir le paquet $(LAZARUS)/components/opengl/lazopenglcontext.lpk
// et l'installer.
// Statut: Opérationnel (identique version Delphi)
// Mode GLU_PERSPECTIVE: OK
// TODO: Enrichir ce module

{$mode delphi} {$H+}
{$DEFINE GLU_PERSPECTIVE}
//{$UNDEF GLU_PERSPECTIVE}

interface

uses
  Common, StructuresDonnees, Graphics, UnitEntites, GL, GLu, LCLProc, Classes,
  SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls, ColorBox, Dialogs,
  OpenGLContext, curredit,
  CallDialogsStdVersion;

// couleurs OpenGL
type TGLColor = record
  R: GLFloat;
  G: GLFloat;
  B: GLFloat;
  A: GLFloat;
end;

type

  { TCdrVueOpenGL }

  TCdrVueOpenGL = class(TFrame)
    btnOK: TButton;
    btnMoreOptions: TButton;
    editMagnification: TCurrencyEdit;
    editFiltres: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    OpenGLControl1: TOpenGLControl;
    Panel1: TPanel;
    pnlMoreOptions: TPanel;
    sclTheta: TScrollBar;
    sclPhi: TScrollBar;
    sclFOV: TScrollBar;
    btnColorBackGrnd: TStaticText;
    btnColorCube: TStaticText;
    procedure btnColorBackGrndClick(Sender: TObject);
    procedure btnColorCubeClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnMoreOptionsClick(Sender: TObject);
    procedure editFiltresKeyPress(Sender: TObject; var Key: char);
    procedure OpenGLControl1Paint(Sender: TObject);
    procedure sclFOVChange(Sender: TObject);
    procedure sclPhiChange(Sender: TObject);
    procedure sclThetaChange(Sender: TObject);
    procedure sclUpXChange(Sender: TObject);
    procedure sclUpYChange(Sender: TObject);
    procedure sclUpZChange(Sender: TObject);
  private
    FTableDesEntites: TTableDesEntites;
    // limites du volume
    FCoin1, FCoin2: TPoint3Df;
    // limites du volume * MULTIPLICATEUR
    c1, c2: TPoint3Df;
    FExcentr: TPoint3Df;
    // paramètres de vue: angle et FOV
    FTheta, FPhi, FFov : double;
    // magnification sur Z
    FMagnification: Double;
    // caractéristiques de lumière
    Flight_ambient      : array[0..3] of GLfloat; // lumière ambiante
    // couleurs
    FColorBackGrnd: TGLColor;           // arrière plan
    FColorCube    : TGLColor;           // cube
    FColorReferentiel: TGLColor;        // couleur du référentiel
    FSizeReferentiel : double;          // taille du référentiel
     //listes d'affichage
    FglListCUBE        : TGLuint;       // cube
    FglListREFERENTIEL : TGLuint;       // référentiel
    FglListPOLYGONALS  : TGLuint;       // polygonales de la cavité
    FglListVOLUMES     : TGLuint;       // volumes de la cavité
    FDoDrawScene : boolean; // Peut-on dessiner
    function InitContextOpenGL: boolean;
    function ConstruireScene(const Filtres: string): boolean;
    procedure MakeCUBE;
    procedure MakePOLYGONALES(const M: double);
    procedure MakeVOLUMES(const M: double);
    procedure RebuildVue;
    { private declarations }
  public
    { public declarations }
    function Initialise(const Filename: string): boolean;
  end; 

implementation
{$R *.lfm}
{$IFDEF GLU_PERSPECTIVE}
   const MULTIPLICATEUR = 1/10000;// 1/10000
{$ELSE}
   const MULTIPLICATEUR = 1.00;// 1/10000
{$ENDIF}
// Retourne sous forme de couleur OpenGL la couleur passée en argument
function GetGLColor(const Coul: TColor): TGLColor;
const
  m = 1/256;
var
  c: TGLColor;
begin
  c.R:=GetRValue(Coul)* m;
  c.G:=GetGValue(Coul)* m;
  c.B:=GetBValue(Coul)* m;
  c.A:=1.00;
  Result:=c;
end;
(* TCdrVueOpenGL *)
function TCdrVueOpenGL.Initialise(const Filename: string): boolean;
begin
  Result := False;
  FTableDesEntites := TTableDesEntites.Create;
  if FTableDesEntites.LoadEntites(Filename, True) > 0 then
  begin
    FMagnification:= 1.00;
    FCoin1 := FTableDesEntites.GetCoinBasGauche;
    FCoin2 := FTableDesEntites.GetCoinHautDroit;
    //FTableDesEntites.MetaFiltre('');
    Result := InitContextOpenGL;
  end;
end;
// construction des éléments de la vue
procedure TCdrVueOpenGL.MakeCUBE;
var
  i: integer;
  t: array[0..1] of GLFloat;
  dx, dy, dz: double;
begin
  {$IFDEF GLU_PERSPECTIVE}
    c1.X:=FCoin1.X * MULTIPLICATEUR;
    c1.Y:=FCoin1.Y * MULTIPLICATEUR;
    c1.Z:=FCoin1.Z * MULTIPLICATEUR;
    c2.X:=FCoin2.X * MULTIPLICATEUR;
    c2.Y:=FCoin2.Y * MULTIPLICATEUR;
    c2.Z:=FCoin2.Z * MULTIPLICATEUR;
  {$ELSE}
    dx := (FCoin2.X - FCoin1.X) * MULTIPLICATEUR / 2;
    dy := (FCoin2.Y - FCoin1.Y) * MULTIPLICATEUR / 2;
    dz := (FCoin2.Z - FCoin1.Z) * MULTIPLICATEUR / 2;

    c1.X := - dx;      c2.X:= dx;
    c1.Y := - dy;      c2.Y:= dy;
    c1.Z := - dz;      c2.Z:= dz;

    c1.X:=FCoin1.X * MULTIPLICATEUR;
    c1.Y:=FCoin1.Y * MULTIPLICATEUR;
    c1.Z:=FCoin1.Z * MULTIPLICATEUR;
    c2.X:=FCoin2.X * MULTIPLICATEUR;
    c2.Y:=FCoin2.Y * MULTIPLICATEUR;
    c2.Z:=FCoin2.Z * MULTIPLICATEUR;
  {$ENDIF}
  t[0]:=c1.Z;  t[1]:=c2.Z;
  for i:=0 to 1 do begin
    glBegin(GL_LINE_LOOP);
     glVertex3d(c1.X, c1.Y, t[i]);
     glVertex3d(c2.X, c1.Y, t[i]);
     glVertex3d(c2.X, c2.Y, t[i]);
     glVertex3d(c1.X, c2.Y, t[i]);
    glEnd();
  end;
  glBegin(GL_LINES);
    glVertex3d(c1.X, c1.Y, t[0]);
    glVertex3d(c1.X, c1.Y, t[1]);
    glVertex3d(c2.X, c1.Y, t[0]);
    glVertex3d(c2.X, c1.Y, t[1]);
    glVertex3d(c2.X, c2.Y, t[0]);
    glVertex3d(c2.X, c2.Y, t[1]);
    glVertex3d(c1.X, c2.Y, t[0]);
    glVertex3d(c1.X, c2.Y, t[1]);
  glEnd();
end;
// Contrairement à la version Delphi,
// le dessin OpenGL est du ressort de TCdrVueOpenGL et non de la table des entités
procedure TCdrVueOpenGL.MakePOLYGONALES(const M: double);
var
  i, Nb: integer;
  E : TEntite;
  c: TGLColor;
begin
  Nb := FTableDesEntites.GetNbEntites;
  AfficherMessage(Format('%s.DrawGLPolygonales(%d entities)', [ClassName, Nb]));
   glBegin(GL_LINES);
   for i:=LOW_INDEX to Nb - 1 do begin
     E := FTableDesEntites.GetEntite(i);
     //AfficherMessage(Format('%f %f %f',[TableEntites[i].Une_Station_2_X, TableEntites[i].Une_Station_2_Y, TableEntites[i].Une_Station_2_Z]));
     if Not(E.Type_Entite in [tgDEFAULT, tgFOSSILE .. tgSIPHON])
       then Continue;
     c:=GetGLColor(E.ColorEntite);
     glColor3d(c.R, c.G, c.B);
     glVertex3d(E.Une_Station_1_X * M,
                E.Une_Station_1_Y * M,
                E.Une_Station_1_Z * M);
     glVertex3d(E.Une_Station_2_X * M,
                E.Une_Station_2_Y * M,
                E.Une_Station_2_Z * M);

   end;
   glEnd;
  ;
end;
// volumes des conduits
procedure TCdrVueOpenGL.MakeVOLUMES(const M: double);
const
  NB_FACETTES = 14;
var
  i: integer;
  Vertex  : array[1..NB_FACETTES] of TPoint3Df;
  Normales: array[1..NB_FACETTES] of TPoint3Df;
  function GetNormale(const dx, dy, dz: double):TPoint3Df;
  const EPSILON=1e-5;
  var
    l: double;
  begin
    l:=Hypot3D(dx+EPSILON,dy+EPSILON,dz+EPSILON);
    Result.X:=dx/l;
    Result.Y:=dy/l;
    Result.Z:=dz/l;
    //Result.T:=1.00;
  end;
  procedure PutVertex(const q: integer);
  begin
    glNormal3d(Normales[q].X, Normales[q].Y, Normales[q].Z);
    glVertex3d(Vertex[q].X, Vertex[q].Y, Vertex[q].Z);;
  end;
  procedure DrawTube(const EE: TEntite);
  var
    c: TGLColor;
    p,q,r: integer;
    z1, z2: Double;
    v1, v2: TPoint3Df;
  begin
    // vertex
    (*
                V4 ---------------------------- V10
               /  \
              /     \
             /        \
            V5          \ V3                V11           V9
             !            !
             !            !
             !            !
             !            !
            V6           V2                 V12           V8
             \          /
              \       /
               \    /
                \ /
                V1                               V7

    Triangles 'pairs'

    V1 V7 V2
    V2 V8 V3
    V3 V9 V4
    V4 V10 V5
    V5 V11 V6
    V6 V12 V1

    Triangles impairs

    V2 V7 V8
    V3 V8 V9
    V4 V9 V10
    V5 V10 V11
    V6 V11 V12
    V1 V12 V7



    //*)
    // calcul des sommets
    with EE do begin
      if (Not Drawn) then Exit;
      if (Not (Type_Entite in [tgDEFAULT, tgFOSSILE .. tgSIPHON])) then Exit;

      z1:=0.50 * (Z1PB+Une_Station_1_Z) * M;
      z2:=0.50 * (Z1PH+Une_Station_1_Z) * M;

      Vertex[1].X := Une_Station_1_X * M;
      Vertex[1].Y := Une_Station_1_Y * M;
      Vertex[1].Z := Z1PB * M;

      Vertex[2].X := X1PD * M;
      Vertex[2].Y := Y1PD * M;
      Vertex[2].Z := z1;

      Vertex[3].X := Vertex[2].X;
      Vertex[3].Y := Vertex[2].Y;
      Vertex[3].Z := z2;

      Vertex[4].X := Vertex[1].X;
      Vertex[4].Y := Vertex[1].Y;
      Vertex[4].Z := Z1PH * M;

      Vertex[5].X := X1PG * M;
      Vertex[5].Y := Y1PG * M;
      Vertex[5].Z := z2;

      Vertex[6].X := Vertex[5].X;
      Vertex[6].Y := Vertex[5].Y;
      Vertex[6].Z := z1;

      z1:=0.50 * (Z2PB+Une_Station_2_Z) * M;
      z2:=0.50 * (Z2PH+Une_Station_2_Z) * M;

      Vertex[7].X := Une_Station_2_X * M;
      Vertex[7].Y := Une_Station_2_Y * M;
      Vertex[7].Z := Z2PB * M;

      Vertex[8].X := X2PD * M;
      Vertex[8].Y := Y2PD * M;
      Vertex[8].Z := z1;

      Vertex[9].X := Vertex[8].X;
      Vertex[9].Y := Vertex[8].Y;
      Vertex[9].Z := z2;

      Vertex[10].X := Vertex[7].X;
      Vertex[10].Y := Vertex[7].Y;
      Vertex[10].Z := Z2PH * M;

      Vertex[11].X := X2PG * M;
      Vertex[11].Y := Y2PG * M;
      Vertex[11].Z := z2;

      Vertex[12].X := Vertex[11].X;
      Vertex[12].Y := Vertex[11].Y;
      Vertex[12].Z := z1;
      // calcul des normales
      Normales[1].X  := 0.0; Normales[1].Y  := 0.0; Normales[1].Z  := -1.0;
      Normales[2]:=GetNormale(X1PD - Une_Station_1_X,
                              Y1PD - Une_Station_1_Y,
                              Z1PB - Une_Station_1_Z);
      Normales[3]:=GetNormale(X1PD - Une_Station_1_X,
                              Y1PD - Une_Station_1_Y,
                              Z1PH - Une_Station_1_Z);
      Normales[4].X  := 0.0; Normales[4].Y  := 0.0; Normales[4].Z  := 1.0;
      Normales[5]:=GetNormale(X1PG - Une_Station_1_X,
                              Y1PG - Une_Station_1_Y,
                              Z1PH - Une_Station_1_Z);
      Normales[6]:=GetNormale(X1PG - Une_Station_1_X,
                              Y1PG - Une_Station_1_Y,
                              Z1PB - Une_Station_1_Z);
      Normales[7].X  := 0.0; Normales[7].Y  := 0.0; Normales[7].Z  := -1.0;
      Normales[8]:=GetNormale(X2PD - Une_Station_2_X,
                              Y2PD - Une_Station_2_Y,
                              Z2PB - Une_Station_2_Z);
      Normales[9]:=GetNormale(X2PD - Une_Station_2_X,
                              Y2PD - Une_Station_2_Y,
                              Z2PH - Une_Station_2_Z);
      Normales[10].X := 0.0; Normales[10].Y := 0.0; Normales[10].Z := 1.0;
      Normales[11]:=GetNormale(X2PG - Une_Station_2_X,
                               Y2PG - Une_Station_2_Y,
                               Z2PH - Une_Station_2_Z);
      Normales[12]:=GetNormale(X2PG - Une_Station_2_X,
                               Y2PG - Une_Station_2_Y,
                               Z2PB - Une_Station_2_Z);
      V1.X :=Vertex[2].X - Vertex[6].X;
      V1.Y :=Vertex[2].Y - Vertex[6].Y;
      V1.Z :=Vertex[2].Z - Vertex[6].Z;

      V2.X :=Vertex[5].X - Vertex[6].X;
      V2.Y :=Vertex[5].Y - Vertex[6].Y;
      V2.Z :=Vertex[5].Z - Vertex[6].Z;
      Normales[13]:=ProduitVectoriel(V1,V2,True);

      V1.X :=Vertex[8].X - Vertex[12].X;
      V1.Y :=Vertex[8].Y - Vertex[12].Y;
      V1.Z :=Vertex[8].Z - Vertex[12].Z;

      V2.X :=Vertex[9].X - Vertex[12].X;
      V2.Y :=Vertex[9].Y - Vertex[12].Y;
      V2.Z :=Vertex[9].Z - Vertex[12].Z;

      Normales[14]:=ProduitVectoriel(V1,V2,True);

    end;
    //********
    c:=GetGLColor(EE.ColorEntite);

    //glColor3d(c.R, c.G, c.B);
    //glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, addr(c));
    glMaterialfv(GL_FRONT, GL_AMBIENT, addr(c));
    glBegin(GL_TRIANGLES);
    // Triangles pairs
    for p := 1 to 6 do begin
      q := p + 6;
      //if (q > 12) then q := 7;
      r := p + 1;
      if (r > 6) then r := 1;
      PutVertex(p);
      PutVertex(q);
      PutVertex(r);

    end;
    // Triangles impairs
    for q := 7 to 12 do begin
      p := q - 5;
      if ( p > 6) then p := 1;
      r := p+6;
       PutVertex(p);
      PutVertex(q);
      PutVertex(r);
    end;
    glEnd;
    // abouts (OK)
    glBegin(GL_TRIANGLE_FAN);
     glNormal3d(Normales[13].X, Normales[13].Y, Normales[13].Z);
     //glVertex3d(Vertex[1].X, Vertex[1].Y, Vertex[1].Z);
     for p:=1 to 6 do glVertex3d(Vertex[p].X, Vertex[p].Y, Vertex[p].Z);
    glEnd;    //***** ok

    glBegin(GL_TRIANGLE_FAN);
     glNormal3d(Normales[14].X, Normales[14].Y, Normales[14].Z);
     //glVertex3d(Vertex[7].X, Vertex[7].Y, Vertex[7].Z);
     for p:=12 downto 7 do glVertex3d(Vertex[p].X, Vertex[p].Y, Vertex[p].Z);
    glEnd;
  end;
begin
   AfficherMessage(Format('%s.DrawGLConduits(%d entities)', [ClassName, FTableDesEntites.GetNbEntites]));

   for i:=LOW_INDEX to FTableDesEntites.GetNbEntites - 1 do begin
     DrawTube(FTableDesEntites.GetEntite(i));
   end;
end;

function TCdrVueOpenGL.ConstruireScene(const Filtres: string): boolean;
begin
  AfficherMessage(Format('%s.ConstruireScene(%s)',[ClassName, Filtres]));
  Result := False;
  try
    FTableDesEntites.MetaFiltre(Filtres);
    // cube enveloppe
    AfficherMessage('-- >Cube enveloppe');
    FglListCUBE := glGenLists(1);
    glNewList(FglListCUBE, GL_COMPILE);
      MakeCUBE;
    glEndList;
    // polygonales
    FglListPOLYGONALS:=glGenLists(1);
    glNewList(FglListPOLYGONALS, GL_COMPILE);
      MakePOLYGONALES(MULTIPLICATEUR);
    glEndList;
    // volumes des conduits
    FglListVOLUMES:=glGenLists(1);
    glNewList(FglListVOLUMES, GL_COMPILE);
      MakeVOLUMES(MULTIPLICATEUR);
    glEndList;


    Result := True;
  except
  end;
end;

procedure TCdrVueOpenGL.RebuildVue;
var
  ErrCode: TGLuint;
  QEchelle: Extended;
  Radius, RP, Ang: double;
  Diago: double;
  PointVue, Centre, AppareilPhotoUp: TPoint3Df;
  AngTheta: Extended;
  AngPhi: Extended;
begin
  AfficherMessage(Format('%s.RebuildVue()',[ClassName]));
  if (Not FDoDrawScene) then Exit;
  glEnable(GL_DEPTH_TEST);
  FColorBackGrnd := GetGLColor(btnColorBackGrnd.Color);
  FColorCube     := GetGLColor(btnColorCube.Color);

  glClearColor(FColorBackGrnd.R, FColorBackGrnd.G, FColorBackGrnd.B, FColorBackGrnd.A);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  glClearDepth(1.0);
  glDepthFunc(GL_LESS);
  glShadeModel(GL_SMOOTH);
  glEnable(GL_CULL_FACE);

  //-------------------

  {$IFDEF GLU_PERSPECTIVE}
    // calcul d'excentrement
    FExcentr.X := 0.50 * (c2.X - c1.X);
    FExcentr.X := c1.X + FExcentr.X;
    FExcentr.Y := 0.50 * (c2.Y - c1.Y);
    FExcentr.Y := c1.Y + FExcentr.Y;
    FExcentr.Z := 0.50 * (FMagnification*(c2.Z - c1.Z));
    FExcentr.Z := FMagnification*c1.Z + FExcentr.Z;
    AfficherMessage(Format('-- >Enveloppe: %.0f, %.0f, %.0f -> %.0f, %.0f, %.0f', [
                    FCoin1.X, FCoin1.Y, FCoin1.Z,  FCoin2.X, FCoin2.Y, FCoin2.Z
                   ]));
    AfficherMessage(Format('-- >(scaled: ): %.0f, %.0f, %.0f -> %.0f, %.0f, %.0f', [
                    c1.X, c1.Y, c1.Z,  c2.X, c2.Y, c2.Z
                   ]));
    // mise en place projection
    glViewport(0,0,OpenGLControl1.Width, OpenGLControl1.Height);
    glMatrixMode (GL_PROJECTION);
    glLoadIdentity();
    //gluOrtho2D:=;
    gluPerspective(FFov, OpenGLControl1.Width / OpenGLControl1.Height, 0.01, 1000.0);    // Fonctionne bien
  {$ELSE}
    // calcul d'excentrement
    FExcentr.X := 0.50 * (c2.X - c1.X);
    FExcentr.X := c1.X + FExcentr.X;
    FExcentr.Y := 0.50 * (c2.Y - c1.Y);
    FExcentr.Y := c1.Y + FExcentr.Y;
    FExcentr.Z := 0.50 * (FMagnification*(c2.Z - c1.Z));
    FExcentr.Z := FMagnification*c1.Z + FExcentr.Z;
    glViewport(0,0,OpenGLControl1.Width, OpenGLControl1.Height);
    glMatrixMode (GL_PROJECTION);
    glLoadIdentity();
    AfficherMessage(Format('-- > glFrustum: %.2f, %.2f, %.2f, %.2f, %.2f, %.2f',
                           [c1.X, c2.X, c1.Y, c2.Y, -c1.Z, -c2.Z]));
    //glFrustum (c1.X*2, c2.X * 2, c1.y * 2, c2.y * 2, -c2.z * 2, -c1.z * 2);
    glOrtho(c1.X*2, c2.X * 2, c1.y * 2, c2.y * 2, c1.z * 2, c2.z * 2);
  {$ENDIF}



  //FFov, RenderWindow.Width/RenderWindow.Height, 0.01,10000
  glMatrixMode (GL_MODELVIEW);
  glLoadIdentity();
  // transformations
  {$IFDEF GLU_PERSPECTIVE}

    //------------- transformations d'ensemble
    glTranslated(0,0, -(c2.Z - c1.Z) * FMagnification * 10); // FMagnification * 10
    glRotated(FPhi  , 1,0,0);
    glRotated(FTheta, 0,0,1);


    glTranslated(-FExcentr.X,
                 -FExcentr.Y,
                 -FExcentr.Z);
    QEchelle := 1.00;
    glScalef(QEchelle, QEchelle, QEchelle * FMagnification);
    AfficherMessage(Format('Transformations pour FOV = %.2f, Theta = %.0f, Phi: %.0f, Scale = %.0f',[FFov, FTheta, FPhi, QEchelle]));

  {$ELSE}

    Diago := Hypot3D(c2.X - c1.X, c2.y - c1.y, c2.z - c1.z);
    Radius:= 1.5* Diago;
    AngTheta := FTheta * PI/180;
    AngPhi   := FPhi * PI/180;
    RP       := Radius * cos(AngPhi);
    // centre
    Centre.X := (c2.X + c1.X) / 2;
    Centre.Y := (c2.Y + c1.Y) / 2;
    Centre.Z := (c2.Z + c1.Z) / 2;
    // oeil

    PointVue.X    := Centre.X + RP * cos(AngTheta);
    PointVue.Y    := Centre.Y + RP * sin(AngTheta);
    PointVue.Z    := Centre.Z + Radius * sin(AngPhi);

    // Up
    AppareilPhotoUp.X := sclUpX.Position/100;
    AppareilPhotoUp.Y := sclUpY.Position/100;
    AppareilPhotoUp.Z := sclUpZ.Position/100;


    AfficherMessage(Format('%s: X = %.2f, Y: %.2f, Z = %.2f',['Point de vue', PointVue.X, PointVue.Y, PointVue.Z]));
    AfficherMessage(Format('%s: X = %.2f, Y: %.2f, Z = %.2f',['Centre', Centre.X, Centre.Y, Centre.Z]));
    AfficherMessage(Format('%s: X = %.2f, Y: %.2f, Z = %.2f',['Up', AppareilPhotoUp.X, AppareilPhotoUp.Y, AppareilPhotoUp.Z]));

    gluLookAt(PointVue.X, PointVue.Y, PointVue.Z,
              Centre.X, Centre.Y, Centre.Z,
              AppareilPhotoUp.X, AppareilPhotoUp.Y, AppareilPhotoUp.Z);
  {$ENDIF}

  // le dessin ici
  //************************************
  // dessin d'objets volumiques
  glLightfv (GL_LIGHT0, GL_AMBIENT, @Flight_ambient); // lumière ambiante
  glEnable(GL_LIGHTING);   	                      // Active l'éclairage
  glEnable(GL_LIGHT0);
  // dessin des volumes cavité
  AfficherMessage('-- >Volumes');
  glCallList(FglListVOLUMES);

  // dessin d'objets filaires
  glDisable(GL_LIGHTING);
  glDisable(GL_LIGHT0);
  // le cube enveloppe
  glColor3d(FColorCube.R,
            FColorCube.G,
            FColorCube.B);
  AfficherMessage('-- >Cube enveloppe');
  glCallList(FglListCUBE);
  // dessin des polygonales
  AfficherMessage('-- >Polygonales');
  glCallList(FglListPOLYGONALS);
  //--------------------------------

  // on envoie
  glFlush;
  ErrCode:=glGetError;
  if ErrCode <> 0 then
    AfficherMessage(Format('OpenGL error: %d - %s',[ErrCode, glGetString(ErrCode)])); //rsOPENGLERROR);

  // échange buffer
  OpenGLControl1.SwapBuffers;

end;

procedure TCdrVueOpenGL.OpenGLControl1Paint(Sender: TObject);
begin
  RebuildVue;
end;

procedure TCdrVueOpenGL.btnOKClick(Sender: TObject);
begin
  ConstruireScene(Trim(editFiltres.Text));
  OpenGLControl1.Repaint;
end;

procedure TCdrVueOpenGL.btnColorBackGrndClick(Sender: TObject);
begin
  btnColorBackGrnd.Color := ChooseColor(btnColorBackGrnd.Color);
end;

procedure TCdrVueOpenGL.btnColorCubeClick(Sender: TObject);
begin
  btnColorCube.Color := ChooseColor(btnColorCube.Color);
end;

procedure TCdrVueOpenGL.btnMoreOptionsClick(Sender: TObject);
begin
  pnlMoreOptions.Visible := Not pnlMoreOptions.Visible;
  btnMoreOptions.Caption := IIF(pnlMoreOptions.Visible, '<<', '>>');
end;

procedure TCdrVueOpenGL.editFiltresKeyPress(Sender: TObject; var Key: char);
begin
  if (Key = #13) then
  begin
    ConstruireScene(Trim(editFiltres.Text));
    OpenGLControl1.Repaint;
  end;
end;

procedure TCdrVueOpenGL.sclFOVChange(Sender: TObject);
begin
  FFov := sclFOV.Position;
  OpenGLControl1.Repaint;
end;

procedure TCdrVueOpenGL.sclPhiChange(Sender: TObject);
begin
  FPhi := -sclPhi.Position;
  OpenGLControl1.Repaint;
end;

procedure TCdrVueOpenGL.sclThetaChange(Sender: TObject);
begin
  FTheta := sclTheta.Position;
  OpenGLControl1.Repaint;
end;

procedure TCdrVueOpenGL.sclUpXChange(Sender: TObject);
begin
  OpenGLControl1.Repaint;
end;

procedure TCdrVueOpenGL.sclUpYChange(Sender: TObject);
begin
  OpenGLControl1.Repaint;
end;

procedure TCdrVueOpenGL.sclUpZChange(Sender: TObject);
begin
  OpenGLControl1.Repaint;
end;

function TCdrVueOpenGL.InitContextOpenGL: boolean;
const B = 0.2;
begin
  AfficherMessage(Format('%s.InitContextOpenGL()',[ClassName]));
  FDoDrawScene := False;
  Result := False;
  try
    if (OpenGLControl1.MakeCurrent) then
    begin
      // couleur
      FColorBackGrnd    :=GetGLColor(clBlack);
      FColorCube        :=GetGLColor(clRED);
      FColorReferentiel :=GetGLColor(clBlue);
      // lumière ambiante
      Flight_ambient[0]:=B ;
      Flight_ambient[1]:=B ;
      Flight_ambient[2]:=B ;
      Flight_ambient[3]:=1.0;

      FTheta:=0.00;
      FPhi  :=0.00;
      FFov  :=60.00;

      FDoDrawScene := ConstruireScene(editFiltres.Text);
      Result := True;
      //glEnable(GL_DEPTH_TEST);
      //glClearColor(0, 0, 0, 1.0);
      //glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
      //glLoadIdentity;             { clear the matrix }
    end;
  except
  end;
end;


end.

