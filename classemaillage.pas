unit ClasseMaillage;
interface
uses
  StructuresDonnees,
  Common, math,
  GL, GLU, OpenGLContext,
  FastGeo,
  SysUtils,Classes, Graphics, Dialogs;
type TTypeMaillage = (tmUNKNOWN, tmREGULAR_GRID, tmTRIANGLES);
type TModeDessinMaillage = (M3D_NONE, M3D_WIRE_FRAME, M3D_MESH, M3D_BLEND, M3D_MIXTE);
type TOpenGLColor = array[0..3] of GLFloat;
type TVertex = record
  ID         : integer;
  X          : double;
  Y          : double;
  Z          : double;
  NormX      : double;
  NormY      : double;
  NormZ      : double;
  Norme      : double;
end;
type TTriangleABC = record
   Numero  :  integer;
   PointA  :  integer;
   PointB  :  integer;
   PointC  :  integer;
   Marked  :  boolean;
   //TriangleVoisinAB: integer;
   //TriangleVoisinBC: integer;
   //TriangleVoisinCA: integer;
end;

 // intersection droite/triangle
 type TIntersectPlanTriangle = record
   X1, Y1, Z1: double;
   X2, Y2, Z2: double;
 end;

type

{ TMaillage }

 TMaillage = class
   procedure Initialiser;
   procedure Finaliser;
   function ReDimMaillage: int64;
   function LireFichierMaillage(const FichierMaillage: string): Int64;
   procedure ConstruireMaillageTriangleWire;
   procedure ConstruireMaillageTriangleSolide;
   procedure ConstruireMaillageGridWire;

   procedure ConstruireMaillageGridSolide;
   procedure ConstruireMaillages;
   procedure CalculerGridNormales;
   procedure CalculerNormales;
   procedure CalculerTablesNormales;
   procedure AffecterCouleur(const z: double; const DoDegrade: boolean);
   private
     ZNormales  : array of array of TPoint3Df;
     ZMatrix    : array of array of double;
     Vertexes   : array of TVertex;
     Triangles  : array of TTriangleABC;

     FTypeMaillage: TTypeMaillage;
     FMaillageValide        : boolean;
     FModeDessinMaillage    : TModeDessinMaillage;

     FCoordsMini: TPoint3Df;
     FCoordsMaxi: TPoint3Df;

     FNbCarreauxX : integer;
     FNbCarreauxY : integer;
     FNomTypeMaillage: string;


     FPasX      : double;
     FPasY      : double;
     FFacteurMult   : double;
     FMagnification : double;

     FCaption     : string;

     // couleurs et transparence
     FColor,
     FColorMin,
     FColorMax    : TGLColor;
     FAlphaMaillage: GLFloat;


     FNbVertexes  : integer;
     FNbTriangles : integer;
     FDegrade     : boolean;

     // pointeur sur fonctions de conversion pour le canevas
     FProcGCSToSRC : TProcGCSToSRC;
     // triangles sélectionnés uniquement ?
     FDoDrawSelectedTrianglesOnly: boolean;
     function GetCoordosPlan(const PM: TPoint3Df): TPoint;
     function GetCoordosVertexToPlan(const PM: TVertex): TPoint;
     function GetNearTriangleIdx(const QX, QY, QZ: double): integer;

   public

     // setters et getters
     // mode de dessin
     function  GetModeDessinMaillage: TModeDessinMaillage;
     procedure SetModeDessinMaillage(const M: TModeDessinMaillage);

     // couleurs
     function  GetDefaultCouleur: TGLColor;
     procedure SetDefaultCouleur(const C: TGLColor);
     function  GetMinCouleur: TGLColor;
     procedure SetMinCouleur(const C: TGLColor);
     function  GetMaxCouleur: TGLColor;
     procedure SetMaxCouleur(const C: TGLColor);
     // membres à lecture seule
     // -- maillage valide
     function  IsValidMaillage: boolean;
     // vertex et triangles
     function GetVertex(const Idx: integer): TVertex;
     function GetTriangle(const Idx: integer): TTriangleABC;
     // transformation d'un triangle TTriangulation en TGeoTriangle;
     function GetGeoTriangle2D(const QT: TTriangleABC): TGeoTriangle2D;
     function GetGeoTriangle3D(const QT: TTriangleABC): TGeoTriangle3D;



     // -- type de maillage
     function  GetTypeMaillage: TTypeMaillage;
     // -- points extrêmes du maillage
     function  GetCoordsMini: TPoint3Df;
     function  GetCoordsMaxi: TPoint3Df;
     // facteur d'échelle et magnification du maillage
     function  GetFacteurMultiplication: double;
     procedure SetFacteurMultiplication(const F: double);

     function  GetMagnification: double;
     procedure SetMagnification(const M: double);



     // -- Maillages TIN
     function  GetNbVertex   : integer;
     function  GetNbTriangles: integer;
     // -- Maillages à pas régulier
     function GetNbCarreauxX: integer;
     function GetNbCarreauxY: integer;
     function GetNbPasX     : double;
     function GetNbPasY     : double;
     // transparence
     property AlphaMaillage: GLFloat read FAlphaMaillage write FAlphaMaillage;
     // triangles sélectionnés uniquement
     property DoDrawSelectedTrianglesOnly: boolean read FDoDrawSelectedTrianglesOnly write FDoDrawSelectedTrianglesOnly;
     // calcul de la distance au versant
     function CalcDistAzPAuVersant(const QX, QY, QZ: double; out QL, QAz, QP: double): boolean;
     function CalcDistanceAuVersant(const QX, QY, QZ: double): double; overload;
     function CalcDistanceAuVersant(const PT: TPoint3Df): double; overload;
     // tracé du maillage sur un canevas.
     procedure TracerMaillage(const Cnv: TCanvas; const QP: TProcGCSToSRC; const Isovaleur: double; const CouleurIsoValeur: TColor);

     // Paramètres:
     // Cnv: Un TCanvas
     // QP: Une fonction de conversion TPoint2Df vers TPoint
     // Isovaleur: Une altitude; -1 = ne pas calculer
     function GetIntersectPlanV_Triangle(const QT: TTriangleABC; const ExtrProfil1, ExtrProfil2: TPoint3Df; out QIntersect: TIntersectPlanTriangle): boolean;

end;


implementation
procedure TMaillage.CalculerTablesNormales;
begin
  case self.FTypeMaillage of
    tmUNKNOWN:;
    tmREGULAR_GRID: CalculerGridNormales;
    tmTRIANGLES   : CalculerNormales;
  end;
end;
procedure TMaillage.CalculerNormales;
var
  i: integer;
  V1, V2, W: TPoint3Df;
  Norm, NormW: double;

begin
  AfficherMessage(Format('%s.CalculerNormales: %d vertex, %d triangles',[ClassName, FNbVertexes, FNbTriangles]));
  for i:=0 to FNbVertexes-1 do
    begin
      Vertexes[i].NormX:=0.0;
      Vertexes[i].NormY:=0.0;
      Vertexes[i].NormZ:=0.0;
      Vertexes[i].Norme:=0.0;
    end;

  for i:=0 to FNbTriangles-1 do
    begin
      with Triangles[i] do
        begin
          // vecteurs v1 et v2
          V1.X:=Vertexes[PointB].X-Vertexes[PointA].X;
          V1.Y:=Vertexes[PointB].Y-Vertexes[PointA].Y;
          V1.Z:=Vertexes[PointB].Z-Vertexes[PointA].Z;
          V2.X:=Vertexes[PointC].X-Vertexes[PointA].X;
          V2.Y:=Vertexes[PointC].Y-Vertexes[PointA].Y;
          V2.Z:=Vertexes[PointC].Z-Vertexes[PointA].Z;
          // produit vectoriel: normale du triangle
          W.X:=V1.Y * V2.Z - V1.Z * V2.Y;
          W.Y:=V1.Z * V2.X - V1.X * V2.Z;
          W.Z:=V1.X * V2.Y - V1.Y * V2.X;
          // toutes les normales doivent etre dirigées vers le haut:
          // composante z positive
          (*
          if W.z <0 then
          begin
            W.X:=-W.X;
            W.Y:=-W.Y;
            W.Z:=-W.Z;

          end;
          //*)
          Norm:=sqrt(sqr(w.x)+sqr(w.y)+sqr(w.z));

          Vertexes[PointA].Norme:=Vertexes[PointA].Norme+Norm;

          Vertexes[PointA].NormX:=Vertexes[PointA].NormX+W.X;
          Vertexes[PointA].NormY:=Vertexes[PointA].NormY+W.Y;
          Vertexes[PointA].NormZ:=Vertexes[PointA].NormZ+W.Z;

          Vertexes[PointB].Norme:=Vertexes[PointB].Norme+Norm;

          Vertexes[PointB].NormX:=Vertexes[PointB].NormX+W.X;
          Vertexes[PointB].NormY:=Vertexes[PointB].NormY+W.Y;
          Vertexes[PointB].NormZ:=Vertexes[PointB].NormZ+W.Z;

          Vertexes[PointC].Norme:=Vertexes[PointC].Norme+Norm;

          Vertexes[PointC].NormX:=Vertexes[PointC].NormX+W.X;
          Vertexes[PointC].NormY:=Vertexes[PointC].NormY+W.Y;
          Vertexes[PointC].NormZ:=Vertexes[PointC].NormZ+W.Z;

        end;
    end;
  // normalisation
  for i:=1 to FNbVertexes do
    begin
      Vertexes[i].NormX:=Vertexes[i].NormX/(Vertexes[i].Norme+1e-12);
      Vertexes[i].NormY:=Vertexes[i].NormY/(Vertexes[i].Norme+1e-12);
      Vertexes[i].NormZ:=Vertexes[i].NormZ/(Vertexes[i].Norme+1e-12);
    end;
end;
procedure TMaillage.ConstruireMaillages;
begin
  AfficherMessage(Format('%s.ConstruireMaillages: %d',[ClassName, Ord(FTypeMaillage)]));

  case FTypeMaillage of
    tmREGULAR_GRID: begin
        case FModeDessinMaillage of
           M3D_WIRE_FRAME    : ConstruireMaillageGridWire;
           M3D_MESH,M3D_BLEND: ConstruireMaillageGridSolide;
        end;
      end;
    tmTRIANGLES: begin
        case FModeDessinMaillage of

           M3D_WIRE_FRAME    : ConstruireMaillageTriangleWire;
           M3D_MESH,M3D_BLEND: ConstruireMaillageTriangleSolide;
        end;
      end;
    else
     ;
  end;
end;

procedure TMaillage.Initialiser;
begin
  AfficherMessage(Format('%s.Initialiser',[ClassName]));
  SetLength(Vertexes,0);
  SetLength(Triangles,0);
  SetLength(ZMatrix,0);
  SetLength(ZNormales,0);

  FFacteurMult      := 1.00;
  FMagnification    := 1.00;
  FMaillageValide   := False;
  FDegrade          := False;
end;

procedure TMaillage.Finaliser;
begin
  AfficherMessage(Format('%s.Free',[ClassName]));
  SetLength(Vertexes,0);
  SetLength(Triangles,0);
  SetLength(ZMatrix,0);
  SetLength(ZNormales,0);

end;

function TMaillage.ReDimMaillage:Int64;
var
  cx, cy: integer;
begin
  cx := 1 + FNbCarreauxX ;
  cy := 1 + FNbCarreauxY;
  AfficherMessage(Format('%s.ReDimMaillage: %d, %d',[ClassName, cx, cy]));
  try
    Result:=-2;
    SetLength(ZMatrix,0,0);
    SetLength(ZMatrix,   cy, cx);
    Result:=-1;
    SetLength(ZNormales, 0, 0);
    SetLength(ZNormales, cy, cx);
    //ShowMessageFmt('%d %d', [FNbLignesX,FNbLignesY]);
    Result:= FNbCarreauxX * FNbCarreauxY;

  finally
  end;
end;

function TMaillage.LireFichierMaillage(const FichierMaillage: string): Int64;
var
  i, j         : integer;
  pFileMAI     : TextFile;
  TmpBuff      : string;
  postab       : integer;
  s            : string;

  V1, V2, W    : TPoint3Df;

  PrmsLn       : TStringArray;
begin
  AfficherMessage(Format('%s.LireFichierMaillage: %s',[ClassName, FichierMaillage]));
  // Maillage KO par défaut
  FMaillageValide := False;
  // opaque par défaut
  FAlphaMaillage := 1.00;
  Result:=-1;
  if Not (FileExists(FichierMaillage)) then Exit;
  AssignFile(PFileMAI,FichierMaillage);

  try
      // Lecture du fichier
      Reset(PFileMAI);
      // Titre du maillage
      Readln(PFileMAI,TmpBuff);
      FCaption:=TmpBuff;
      // Type de maillage
      Readln(PFileMAI,TmpBuff);

      if Trim(UpperCase(TmpBuff))='REGULAR_GRID'   then FTypeMaillage:=tmREGULAR_GRID
      else if Trim(UpperCase(TmpBuff))='TRIANGLES' then FTypeMaillage:=tmTRIANGLES
                                                   else FTypeMaillage:=tmUNKNOWN;
      case FTypeMaillage of
        tmREGULAR_GRID: begin
            AfficherMessage('--> Type de maillage: MNT à pas régulier');
            //Couleur
            Readln(PFileMAI,TmpBuff);
            FColor := GetGLColor(StrToInt(TmpBuff), FAlphaMaillage);
            FColorMax:=FColor;
            Readln(PFileMAI,TmpBuff);
            FColorMin:=GetGLColor(StrToInt(TmpBuff), FAlphaMaillage);
            // Nombre de lignes
            Readln(PFileMAI,TmpBuff);
            FNbCarreauxX:=StrToInt(TmpBuff);
            Readln(PFileMAI,TmpBuff);
            FNbCarreauxY:=StrToInt(TmpBuff);
            // Coordonnées mini
            Readln(PFileMAI,TmpBuff);
            FCoordsMini.X := StrToFloat(TmpBuff) * FFacteurMult;
            Readln(PFileMAI,TmpBuff);
            FCoordsMini.Y:=StrToFloat(TmpBuff) * FFacteurMult;
            // Coordonnées maxi
            Readln(PFileMAI,TmpBuff);
            FCoordsMaxi.X:=StrToFloat(TmpBuff) * FFacteurMult;
            Readln(PFileMAI,TmpBuff);
            FCoordsMaxi.Y:=StrToFloat(TmpBuff) * FFacteurMult;
            // redimensionner les tableaux
            SetLength(Vertexes,0);
            SetLength(Triangles,0);

            SetLength(ZMatrix,   1+FNbCarreauxY, 1+FNbCarreauxX);
            SetLength(ZNormales, 1+FNbCarreauxY, 1+FNbCarreauxX);
              // Calcul du pas
            FPasX:=(FCoordsMaxi.X-FCoordsMini.X)/(FNbCarreauxX-1);
            FPasY:=(FCoordsMaxi.Y-FCoordsMini.Y)/(FNbCarreauxY-1);
            // Affectation des abscisses et ordonnées
            for i:=1 to FNbCarreauxX do ZMatrix[0,i]:=FCoordsMini.X + (i-1)*FPasX;
            for j:=1 to FNbCarreauxY do ZMatrix[j,0]:=FCoordsMini.Y + (j-1)*FPasY;
            // lecture d'une donnée obsolète
            ReadLn(pFileMAI, TmpBuff);
            // chargement de la table
            FCoordsMaxi.Z:=1e+10;
            FCoordsMaxi.Z:=-1e+10;
            for i:=1 to FNbCarreauxY do
              for j:=1 to FNbCarreauxX do
                 begin
                   Readln(PFileMAI,TmpBuff);
                   ZMatrix[i,j]:=strtofloat(TmpBuff) * FFacteurMult * FMagnification;
                   if (ZMatrix[i,j] > FCoordsMaxi.Z) then FCoordsMaxi.Z:=ZMatrix[i,j];
                   if (ZMatrix[i,j] < FCoordsMaxi.Z) then FCoordsMaxi.Z:=ZMatrix[i,j];
                  end;
            Result  := FNbCarreauxX * FNbCarreauxY;
            FMaillageValide := True;
            FNomTypeMaillage:=Format('Grille: (%d * %d)', [FNbCarreauxX, FNbCarreauxY]);
        end;
      tmTRIANGLES: begin
            AfficherMessage('--> Type de maillage: Réseau irrégulier de triangles');
            //Couleur
            Readln(PFileMAI,TmpBuff);
            FColor:=GetGLColor(StrToInt(TmpBuff), FAlphaMaillage);
            FColorMax:=FColor;
            Readln(PFileMAI,TmpBuff);
            FColorMin:=GetGLColor(StrToInt(TmpBuff), FAlphaMaillage);
            // section Vertex
            Readln(PFileMAI,TmpBuff);
            // nombre de sommets
            Readln(PFileMAI,TmpBuff);
            //WriteLn(TmpBuff);
            FNbVertexes:=StrToInt(TmpBuff);
            AfficherMessage('--> Lecture section VERTEX');

            SetLength(Vertexes, 0);
            SetLength(Vertexes, FNbVertexes+1);
            for i:=0 to FNbVertexes-1 do
            begin
              Readln(PFileMAI,TmpBuff);
              PrmsLn := Split(TmpBuff, #9);
              Vertexes[i].ID:=StrToIntDef(PrmsLn[0],0);
              Vertexes[i].X:=StrToFloat(PrmsLn[1])* FFacteurMult * FMagnification;
              Vertexes[i].Y:=StrToFloat(PrmsLn[2])* FFacteurMult * FMagnification;
              Vertexes[i].Z:=StrToFloat(PrmsLn[3])* FFacteurMult * FMagnification;
              Vertexes[i].NormX:=0.0;
              Vertexes[i].NormY:=0.0;
              Vertexes[i].NormZ:=0.0;
              Vertexes[i].Norme:=0.0;
            end;
            AfficherMessage(Format('--> %d vertex',[FNbVertexes]));
            // mini et maxi
            FCoordsMini.X:=  1e+10;
            FCoordsMaxi.X:= -1e+10;
            FCoordsMini.Y:=  1e+10;
            FCoordsMaxi.Y:= -1e+10;
            FCoordsMini.Z:=  1e+10;
            FCoordsMaxi.Z:= -1e+10;

            for i:=0 to FNbVertexes-1 do
            begin
              if ((i mod 1000) = 0) then  AfficherMessage(Format('%d - %f %f %f',[i, Vertexes[i].X, Vertexes[i].Y, Vertexes[i].Z]));
              if (Vertexes[i].X < FCoordsMini.X) then FCoordsMini.X:=Vertexes[i].X;
              if (Vertexes[i].Y < FCoordsMini.Y) then FCoordsMini.Y:=Vertexes[i].Y;
              if (Vertexes[i].Z < FCoordsMini.Z) then FCoordsMini.Z:=Vertexes[i].Z;
              if (Vertexes[i].X > FCoordsMaxi.X) then FCoordsMaxi.X:=Vertexes[i].X;
              if (Vertexes[i].Y > FCoordsMaxi.Y) then FCoordsMaxi.Y:=Vertexes[i].Y;
              if (Vertexes[i].Z > FCoordsMaxi.Z) then FCoordsMaxi.Z:=Vertexes[i].Z;
            end;
            // lire le '-1'
            ReadLn(pFileMAI, TmpBuff);
            // lire le TRIANGLES
            ReadLn(pFileMAI, TmpBuff);
            //WriteLn('Fin-section ' + TmpBuff);
            // nombre de triangles
            AfficherMessage('--> Lecture section TRIANGLES');

            Readln(PFileMAI,TmpBuff);
            FNbTriangles:=StrToInt(TmpBuff);
            SetLength(Triangles,0);
            SetLength(Triangles,FNbTriangles);

            for i:=0 to FNbTriangles-1 do
            begin
              Readln(PFileMAI,TmpBuff);
              PrmsLn := Split(TmpBuff, #9);


              Triangles[i].Numero:=StrToIntDef(PrmsLn[0],0);
              Triangles[i].PointA:=StrToIntDef(PrmsLn[1],0);
              Triangles[i].PointB:=StrToIntDef(PrmsLn[2],0);
              Triangles[i].PointC:=StrToIntDef(PrmsLn[3],0);
              Triangles[i].Marked:=IIF(StrToIntDef(PrmsLn[4],0) = 0, False, True);


              if (Triangles[i].PointA < 0) or
                 (Triangles[i].PointB < 0) or
                 (Triangles[i].PointC < 0)
              then  AfficherMessage(Format('--->Triangle dégénéré: %d - %d %d %d',[i,Triangles[i].PointA, Triangles[i].PointB,Triangles[i].PointC]));
            end;
            AfficherMessage(Format('--> %d triangles',[FNbTriangles]));

            FNomTypeMaillage:=Format('Triangles: (%d)',[FNbTriangles]);
            // initialiser tableaux inutilisés
            SetLength(ZMatrix,   1,1);
            SetLength(ZNormales, 1,1);
            FMaillageValide := True;
            Result := FNbTriangles;

        end;
      tmUNKNOWN: begin
           // initialiser tableaux inutilisés
           SetLength(Triangles,1);
           SetLength(Vertexes,1);
           SetLength(ZMatrix,   1,1);
           SetLength(ZNormales, 1,1);
           FMaillageValide := False;
           Result:=-64;
        end;
      end;
    // contrôle
    AfficherMessage(Format('--> X: De %.0f a %.0f', [FCoordsMini.X, FCoordsMaxi.X]));
    AfficherMessage(Format('--> Y: De %.0f a %.0f', [FCoordsMini.Y, FCoordsMaxi.Y]));
    AfficherMessage(Format('--> Z: De %.0f a %.0f', [FCoordsMini.Z, FCoordsMaxi.Z]));

  finally
    CloseFile(pFileMAI);
  end;
end;



// MNT à pas régulier: Fil de fer
procedure TMaillage.ConstruireMaillageGridWire;
var
  Idx_X_Dep,
  Idx_Y_Dep,
  Idx_X_Arr,
  Idx_Y_Arr   :  integer;

  procedure DessinerUneGrilleMNT(var Idx_X_Dep, Idx_Y_Dep, Idx_X_Arr, Idx_Y_Arr: integer);
  //const colTerrain:Array[0..3] of GLFloat=(1.0, 0.0,0.0,0.0);
  var
    i,j: integer;
      procedure AffecterCouleur(const z: double);
      var
        c: TGLColor;
      begin
        if FDegrade then
        begin
          c:=GetGLColor(GetColorDegrade(z,
                                        FCoordsMini.Z, FCoordsMaxi.Z,
                                        GetPASColor(FColorMin),
                                        GetPASColor(FColorMax)), FAlphaMaillage);
          glColor3fv(@c);
        end;
      end;


  begin
    begin
      for j:=Idx_Y_Dep to Idx_Y_Arr do
        begin
          glBegin(GL_LINE_STRIP);
          AffecterCouleur(ZMatrix[j-1,2]);
          glVertex3f(ZMatrix[0  ,2],
                     ZMatrix[j-1,0],
                     ZMatrix[j-1,2]);
          for i:=Idx_X_Dep to Idx_X_Arr do
            begin
              AffecterCouleur(ZMatrix[j,i-1]);
              glVertex3f(ZMatrix[0,i-1],
                         ZMatrix[j,0],
                         ZMatrix[j,i-1]);
            end;
          glEnd;
        end;

       for i:=Idx_X_Dep to Idx_X_Arr do

        begin
          glBegin(GL_LINE_STRIP);
          AffecterCouleur(ZMatrix[2,i-1]);
          glVertex3f(ZMatrix[0,i-1],
                     ZMatrix[2,0],
                     ZMatrix[2,i-1]);
          for j:=Idx_Y_Dep to Idx_Y_Arr do
            begin
              AffecterCouleur(ZMatrix[j,i-1]);
              glVertex3f(ZMatrix[0,i-1],
                         ZMatrix[j,0],
                         ZMatrix[j,i-1]);
            end;
          glEnd;
        end;

      end;
  end;
begin

  if (FMaillageValide) then
  begin
    Idx_X_Dep:=3;
    Idx_Y_Dep:=3;
    Idx_X_Arr:=FNbCarreauxX;
    Idx_Y_Arr:=FNbCarreauxY;
    DessinerUneGrilleMNT(Idx_X_Dep, Idx_Y_Dep, Idx_X_Arr, Idx_Y_Arr);
  end;
end;

// MNT à pas régulier: Normales
procedure TMaillage.CalculerGridNormales;
var
  i,j: Integer;
  Idx_X_Min,
  Idx_X_Max,
  Idx_Y_Min,
  Idx_Y_Max   : integer;

  (*Idx_X_Dep,
  Idx_Y_Dep,
  Idx_X_Arr,
  Idx_Y_Arr   :  integer;
  //*)
  (*Normale1,
  Normale2    : Point3Df;

  P1, P2, P3, P4: Point3Df;    //*)
  PointP1, PointP2, PointP3, PointP4,
  RepP1, RepP2, RepP3, RepP4: TPoint3Df;
  Norme:   double;
  procedure CalculerNormalesFacette(var PointP1, PointP2, PointP3, PointP4: TPoint3Df;
                                    var RepP1, RepP2, RepP3, RepP4: TPoint3Df);
  // calcul des quatre normales à la facette P1 P2 P3 P4
  var
    V1, V2 : TPoint3Df;
    Normale: TPoint3Df;
    procedure VecteurNormal;
    begin
      Normale.X := V1.Y * V2.Z - V1.Z * V2.Y;
      Normale.Y := V1.Z * V2.X - V1.X * V2.Z;
      Normale.Z := V1.X * V2.Y - V1.Y * V2.X;
    end;
  begin
    // triangle 1
    V1.X := PointP2.X - PointP1.X;
    V1.Y := PointP2.Y - PointP1.Y;
    V1.Z := PointP2.Z - PointP1.Z;

    V2.X := PointP3.X - PointP1.X;
    V2.Y := PointP3.Y - PointP1.X;
    V2.Z := PointP3.Z - PointP1.X;
    VecteurNormal;
    RepP1.X := Normale.X;
    RepP1.Y := Normale.Y;
    RepP1.Z := Normale.Z;
    // triangle 2
    V1.X := V2.X;
    V1.Y := V2.Y;
    V1.Z := V2.Z;

    V2.X := PointP4.X - PointP1.X;
    V2.Y := PointP4.Y - PointP1.Y;
    V2.Z := PointP4.Z - PointP1.Z;
    VecteurNormal;
    RepP3.X := Normale.X;
    RepP3.Y := Normale.Y;
    RepP3.Z := Normale.Z;

    RepP2.X := RepP1.X + RepP3.X;
    RepP2.Y := RepP1.Y + RepP3.Y;
    RepP2.Z := RepP1.Z + RepP3.Z;

    RepP4.X := RepP2.X;
    RepP4.Y := RepP2.Y;
    RepP4.Z := RepP2.Z;

  end;
begin

  if Not(FMaillageValide) then Exit;
  Idx_X_Min := 3;
  Idx_Y_Min := 3;
  Idx_X_Max := FNbCarreauxX;
  Idx_Y_Max := FNbCarreauxY;
  FPasX := (FCoordsMaxi.X - FCoordsMini.X) / (FNbCarreauxX - 1);
  FPasY := (FCoordsMaxi.Y - FCoordsMini.Y) / (FNbCarreauxY - 1);
  for i:= Idx_X_Min to Idx_X_Max-2 do
    for j:=Idx_Y_Min to Idx_Y_Max-2 do
      begin
        (*
                |          |
           i-1  |    i     |   i+1
           j+1  |    j+1   |   j+1
                |          |
        ----------------------------
                | Facette  |
           i-1  |    i     |   i+1
           j    |    j     |   j
                | Normale  |
        --------o-------------------
                |          |
           j-1  |    j     |   j+1
           i-1  |    i-1   |   i+1
                |          |
        //*)
        //Normale[i,j], située en coin bas gauche de la facette[i,j]
        // traitement facette (,j-1, i-1)
        PointP4.X := 0;
        PointP4.Y := FPasY;
        PointP4.Z := ZMatrix[j-1,i]-
                     ZMatrix[j-1,i-1];
        PointP3.X := FPasX;
        PointP3.Y := FPasY;
        PointP3.Z := ZMatrix[j,i]-ZMatrix[j-1,i-1];

        PointP2.X := FPasX;
        PointP2.Y := 0;
        PointP2.Z := ZMatrix[j-1,i]-ZMatrix[j-1,i-1];

        PointP1.X := 0;
        PointP1.Y := 0;
        PointP1.Z := 0;
        CalculerNormalesFacette(PointP1, PointP2, PointP3, PointP4,
                                RepP1, RepP2, RepP3, RepP4);
        ZNormales[j,i].X:=RepP3.X;
        ZNormales[j,i].Y:=RepP3.Y;
        ZNormales[j,i].Z:=RepP3.Z;
        // traitement facette (j,i-1)
        PointP4.X := 0;                         PointP3.X := FPasX;
        PointP4.Y := FPasY;                     PointP3.Y := FPasY;
        PointP4.Z := ZMatrix[j,i]-ZMatrix[j,i-1]; PointP3.Z := ZMatrix[j+1,i]-ZMatrix[j,i-1];

        PointP1.X := 0;                    PointP2.X := FPasX;
        PointP1.Y := 0;                    PointP2.Y := 0;
        PointP1.Z := 0;                    PointP2.Z := ZMatrix[j+1,i-1]-ZMatrix[j,i-1];

        CalculerNormalesFacette(PointP1, PointP2, PointP3, PointP4,
                                RepP1, RepP2, RepP3, RepP4);
        ZNormales[j,i].X:=ZNormales[j,i].X+RepP4.X;
        ZNormales[j,i].Y:=ZNormales[j,i].Y+RepP4.Y;
        ZNormales[j,i].Z:=ZNormales[j,i].Z+RepP4.Z;


        PointP4.X := 0;                         PointP3.X := FPasX;
        PointP4.Y := FPasY;                     PointP3.Y := FPasY;
        PointP4.Z := ZMatrix[j,i+1]-ZMatrix[j,i]; PointP3.Z := ZMatrix[j+1,i+1]-ZMatrix[j,i];

        PointP1.X := 0;                    PointP2.X := FPasX;
        PointP1.Y := 0;                    PointP2.Y := 0;
        PointP1.Z := 0;                    PointP2.Z := ZMatrix[j+1,i]-ZMatrix[j,i];

        CalculerNormalesFacette(PointP1, PointP2, PointP3, PointP4,
                                RepP1, RepP2, RepP3, RepP4);
        ZNormales[j,i].X:=ZNormales[j,i].X+RepP1.X;
        ZNormales[j,i].Y:=ZNormales[j,i].Y+RepP1.Y;
        ZNormales[j,i].Z:=ZNormales[j,i].Z+RepP1.Z;
        // traitement facette (i-1,j)

        PointP4.X := 0;                              PointP3.X := FPasX;
        PointP4.Y := FPasY;                          PointP3.Y := FPasY;
        PointP4.Z := ZMatrix[j-1,i+1]-ZMatrix[j-1, i]; PointP3.Z := ZMatrix[j,i+1]-ZMatrix[j-1, i];

        PointP1.X := 0;                    PointP2.X := FPasX;
        PointP1.Y := 0;                    PointP2.Y := 0;
        PointP1.Z := 0;                    PointP2.Z := ZMatrix[j,i]-ZMatrix[j-1, i];

        CalculerNormalesFacette(PointP1, PointP2, PointP3, PointP4,
                                RepP1, RepP2, RepP3, RepP4);
        ZNormales[j,i].X:=ZNormales[j,i].X+RepP2.X;
        ZNormales[j,i].Y:=ZNormales[j,i].Y+RepP2.Y;
        ZNormales[j,i].Z:=ZNormales[j,i].Z+RepP2.Z;

        // normaliser
        Norme:=Sqrt(Sqr(ZNormales[j,i].X)+
                    Sqr(ZNormales[j,i].Y)+
                    Sqr(ZNormales[j,i].Z));
        ZNormales[j,i].X:=ZNormales[j,i].X/Norme;
        ZNormales[j,i].Y:=ZNormales[j,i].Y/Norme;
        ZNormales[j,i].Z:=ZNormales[j,i].Z/Norme;
      end;
end;

procedure TMaillage.AffecterCouleur(const z: double; const DoDegrade: boolean);
var
  c: TGLColor;
begin
  if (DoDegrade) then
  begin
    c:=GetGLColor(GetColorDegrade(z,
                                  FCoordsMini.Z, FCoordsMaxi.Z,
                                  GetPASColor(FColorMin),
                                  GetPASColor(FColorMax)), FAlphaMaillage);
  end else c:= FColorMax;
  glColor4fv(@c);
end;

function TMaillage.GetModeDessinMaillage: TModeDessinMaillage;
begin
  Result := FModeDessinMaillage;
end;

procedure TMaillage.SetModeDessinMaillage(const M: TModeDessinMaillage);
begin
  FModeDessinMaillage := M;
end;


function TMaillage.IsValidMaillage: boolean;
begin
  Result := FMaillageValide;
end;

function TMaillage.GetVertex(const Idx: integer): TVertex;
begin
  Result := Vertexes[Idx];
end;

function TMaillage.GetTriangle(const Idx: integer): TTriangleABC;
begin
  Result := Triangles[Idx];
end;

function TMaillage.GetTypeMaillage: TTypeMaillage;
begin
  Result := FTypeMaillage;
end;

function TMaillage.GetNbVertex: integer;
begin
  Result := FNbVertexes;
end;

function TMaillage.GetNbTriangles: integer;
begin
  Result := FNbTriangles;
end;

function TMaillage.GetNbCarreauxX: integer;
begin
  Result := FNbCarreauxX;
end;

function TMaillage.GetNbCarreauxY: integer;
begin
  Result := FNbCarreauxY;
end;

function TMaillage.GetNbPasX: double;
begin
  Result := FPasX;
end;

function TMaillage.GetNbPasY: double;
begin
  Result := FPasY;
end;
// distance au versant; retourne long, azimut et pente
function TMaillage.CalcDistAzPAuVersant(const QX, QY, QZ: double; out QL, QAz, QP: double): boolean;
var
  WU: TGeoFloat;
  i, j: Integer;
  S: TPoint3Df;
  _dx, _dy, _dz: double;
  Qdx, Qdy, Qdz: Double;
  EWE: TTriangleABC;
begin
  WU := 1e18;
  Result := False;
  if (not FMaillageValide) then Exit;
  case FTypeMaillage of
    tmUNKNOWN: exit;
    tmREGULAR_GRID:
      begin
        try
          for i := 0 to FNbCarreauxX - 1 do
            for j := 0 to FNbCarreauxY - 1 do
            begin
              S.X := FPasX * i;
              S.Y := FPasY * j;
              S.Z := ZMatrix[i,j ];
              _dx  := QX - S.X;
              _dy  := QY - S.Y;
              _dz  := QZ - S.Z;
              QL := Hypot3D(_dx, _dy, _dz);
              if (QL < WU) then
              begin
                WU := QL;
                Qdx := _dx;
                Qdy := _dy;
                Qdz := _dz;
              end;
            end;
          GetBearingInc(Qdx, Qdy, Qdz, QL, QAz, QP, 360.00, 360.00);
          Result := True;
        except
        end;
      end;
    tmTRIANGLES:
      begin


      end;
  end;
end;

function TMaillage.CalcDistanceAuVersant(const QX, QY, QZ: double): double;
begin

end;

function TMaillage.GetNearTriangleIdx(const QX, QY, QZ: double): integer;
var
  EWE: TTriangleABC;
  VA, VB, VC: TVertex;
  i: Integer;
begin
  Result := -1;
  for i := 0 to FNbTriangles - 1 do
  begin
    EWE := GetTriangle(i);
    VA  := GetVertex(EWE.PointA);
    VB  := GetVertex(EWE.PointB);
    VC  := GetVertex(EWE.PointC);
    //dx  := (VA.X
    //+ VB.X + VC.X) / 3;
    //d   := MinimumDistanceFromPointToTriangle(Px,Py,x1,y1,x2,y2,x3,y3:TGeoFloat):TGeoFloat;

  end;
end;

function TMaillage.CalcDistanceAuVersant(const PT: TPoint3Df): double;
begin
  Result := CalcDistanceAuVersant(PT.X, PT.Y, PT.Z);
end;

function TMaillage.GetDefaultCouleur: TGLColor;
begin
  Result := FColor;
end;

procedure TMaillage.SetDefaultCouleur(const C: TGLColor);
begin
  FColor := C;
end;

function TMaillage.GetMinCouleur: TGLColor;
begin
  Result := FColorMin;
end;

procedure TMaillage.SetMinCouleur(const C: TGLColor);
begin
  FColorMin := C;
end;

function TMaillage.GetMaxCouleur: TGLColor;
begin
  Result := FColorMax;
end;

procedure TMaillage.SetMaxCouleur(const C: TGLColor);
begin
  FColorMax := C;
end;

function TMaillage.GetCoordsMini: TPoint3Df;
begin
  Result := FCoordsMini;
end;

function TMaillage.GetCoordsMaxi: TPoint3Df;
begin
  Result := FCoordsMaxi;
end;

function TMaillage.GetFacteurMultiplication: double;
begin
  Result := FFacteurMult;
end;

procedure TMaillage.SetFacteurMultiplication(const F: double);
begin
  FFacteurMult := F;
end;

function TMaillage.GetMagnification: double;
begin
  Result := FMagnification;
end;

procedure TMaillage.SetMagnification(const M: double);
begin
  FMagnification := M;
end;

// MNT à pas régulier: Solide
procedure TMaillage.ConstruireMaillageGridSolide;
var
  i,j: integer;
  Idx_X_Max,
  Idx_Y_Max   : integer;

begin
  AfficherMessage(Format('%s.ConstruireMaillageGridSolide: %d x %d',[ClassName, FNbCarreauxX, FNbCarreauxY]));
  AfficherMessage(Format('-- > Alpha = %f',[FAlphaMaillage]));
  Idx_X_Max := FNbCarreauxX;
  Idx_Y_Max := FNbCarreauxY;
  //exit;
  glBegin(GL_TRIANGLES);
  for j:=3 to Idx_Y_Max-1 do
      for i:=3 to Idx_X_Max-1 do
          begin
              //recto:
              //triangle 1
              glNormal3f(ZNormales[j-1,i-1].X,
                         ZNormales[j-1,i-1].Y,
                         ZNormales[j-1,i-1].Z);
              AffecterCouleur(ZMatrix[j-1,i-1], FDegrade);
              glVertex3f(ZMatrix[0  ,i-1],
                         ZMatrix[j-1,0],
                         ZMatrix[j-1,i-1]);
              glNormal3f(ZNormales[j-1,i].X,
                         ZNormales[j-1,i].Y,
                         ZNormales[j-1,i].Z);
              AffecterCouleur(ZMatrix[j-1,i], FDegrade);
              glVertex3f(ZMatrix[0,i],
                         ZMatrix[j-1,0],
                         ZMatrix[j-1,i]);
              glNormal3f(ZNormales[j,i].X,
                         ZNormales[j,i].Y,
                         ZNormales[j,i].Z);
              AffecterCouleur(ZMatrix[j,i], FDegrade);
              glVertex3f(ZMatrix[0,i],
                         ZMatrix[j,0],
                         ZMatrix[j,i]);

              //recto:
              //triangle 2
              glNormal3f(ZNormales[j-1,i-1].X,
                         ZNormales[j-1,i-1].Y,
                         ZNormales[j-1,i-1].Z);
              AffecterCouleur(ZMatrix[j-1,i-1], FDegrade);
              glVertex3f(ZMatrix[0  ,i-1],
                         ZMatrix[j-1,0],
                         ZMatrix[j-1,i-1]);
              glNormal3f(ZNormales[j,i].X,
                         ZNormales[j,i].Y,
                         ZNormales[j,i].Z);
              AffecterCouleur(ZMatrix[j,i], FDegrade);
              glVertex3f(ZMatrix[0,i],
                         ZMatrix[j,0],
                         ZMatrix[j,i]);
              glNormal3f(ZNormales[j,i-1].X,
                         ZNormales[j,i-1].Y,
                         ZNormales[j,i-1].Z);
              AffecterCouleur(ZMatrix[j,i-1], FDegrade);
              glVertex3f(ZMatrix[0,i-1],
                         ZMatrix[j,0],
                         ZMatrix[j,i-1]);



              //verso:
              //triangle 1
              glNormal3f(-ZNormales[j-1,i-1].X,
                         -ZNormales[j-1,i-1].Y,
                         -ZNormales[j-1,i-1].Z);
              glVertex3f(ZMatrix[0  ,i-1],
                         ZMatrix[j-1,0],
                         ZMatrix[j-1,i-1]);
              glNormal3f(-ZNormales[j,i].X,
                         -ZNormales[j,i].Y,
                         -ZNormales[j,i].Z);
              glVertex3f(ZMatrix[0,i],
                         ZMatrix[j,0],
                         ZMatrix[j,i]);
              glNormal3f(-ZNormales[j-1,i].X,
                         -ZNormales[j-1,i].Y,
                         -ZNormales[j-1,i].Z);
              glVertex3f(ZMatrix[0,i],
                         ZMatrix[j-1,0],
                         ZMatrix[j-1,i]);
              //verso:
              //triangle 2
              glNormal3f(-ZNormales[j-1,i-1].X,
                         -ZNormales[j-1,i-1].Y,
                         -ZNormales[j-1,i-1].Z);
              glVertex3f(ZMatrix[0  ,i-1],
                         ZMatrix[j-1,0],
                         ZMatrix[j-1,i-1]);

              glNormal3f(-ZNormales[j,i-1].X,
                         -ZNormales[j,i-1].Y,
                         -ZNormales[j,i-1].Z);
              glVertex3f(ZMatrix[0,i-1],
                         ZMatrix[j,0],
                         ZMatrix[j,i-1]);
              glNormal3f(-ZNormales[j,i].X,
                         -ZNormales[j,i].Y,
                         -ZNormales[j,i].Z);
              glVertex3f(ZMatrix[0,i],
                         ZMatrix[j,0],
                         ZMatrix[j,i]);
          end;
      glEnd;
end;

// MNT à TIN (triangles)

procedure TMaillage.ConstruireMaillageTriangleWire;
var
  i: integer;

begin
  AfficherMessage(Format('%s.ConstruireMaillageTriangleWire: %d triangles, %d vertex',[ClassName, FNbTriangles, FNbVertexes]));
  AfficherMessage(Format('-- > Alpha = %f',[FAlphaMaillage]));

  for i:=0 to FNbTriangles-1 do
    begin
      if FDoDrawSelectedTrianglesOnly then begin
        if Not Triangles[i].Marked then Continue;
      end;

      glBegin(GL_LINE_LOOP);
      AffecterCouleur(Vertexes[Triangles[i].PointA].Z, FDegrade);
      glVertex3f(Vertexes[Triangles[i].PointA].X,
                   Vertexes[Triangles[i].PointA].Y,
                   Vertexes[Triangles[i].PointA].Z);
      AffecterCouleur(Vertexes[Triangles[i].PointB].Z, FDegrade);
      glVertex3f(Vertexes[Triangles[i].PointB].X,
                   Vertexes[Triangles[i].PointB].Y,
                   Vertexes[Triangles[i].PointB].Z);
      AffecterCouleur(Vertexes[Triangles[i].PointC].Z, FDegrade);
      glVertex3f(Vertexes[Triangles[i].PointC].X,
                   Vertexes[Triangles[i].PointC].Y,
                   Vertexes[Triangles[i].PointC].Z);
      AffecterCouleur(Vertexes[Triangles[i].PointA].Z, FDegrade);
      glVertex3f(Vertexes[Triangles[i].PointA].X,
                   Vertexes[Triangles[i].PointA].Y,
                   Vertexes[Triangles[i].PointA].Z);
      glEnd;
  end;
 
end;


// MNT à TIN (triangles)
procedure TMaillage.ConstruireMaillageTriangleSolide;
var
  i: integer;
  
begin
  AfficherMessage('ConstruireMaillageTriangleSolide:');
  AfficherMessage(Format('--> Couleur max: %f %f %f %f',[FColorMin.R, FColorMin.G, FColorMin.B, FAlphaMaillage]));
  AfficherMessage(Format('--> Couleur min: %f %f %f %f',[FColorMax.R, FColorMax.G, FColorMax.B, FAlphaMaillage]));
  glBegin(GL_TRIANGLES);
  // recto
  for i:=0 to FNbTriangles-1 do
    begin
      if FDoDrawSelectedTrianglesOnly then begin
        if Not Triangles[i].Marked then Continue;
      end;
      glNormal3F(Vertexes[Triangles[i].PointA].NormX,
                 Vertexes[Triangles[i].PointA].NormY,
                 Vertexes[Triangles[i].PointA].NormZ);
      AffecterCouleur(Vertexes[Triangles[i].PointA].Z, FDegrade);
      glVertex3f(Vertexes[Triangles[i].PointA].X,
                 Vertexes[Triangles[i].PointA].Y,
                 Vertexes[Triangles[i].PointA].Z);
      glNormal3F(Vertexes[Triangles[i].PointB].NormX,
                 Vertexes[Triangles[i].PointB].NormY,
                 Vertexes[Triangles[i].PointB].NormZ);
      //*)
      AffecterCouleur(Vertexes[Triangles[i].PointB].Z, FDegrade);
      glVertex3f(Vertexes[Triangles[i].PointB].X,
                 Vertexes[Triangles[i].PointB].Y,
                 Vertexes[Triangles[i].PointB].Z);
      glNormal3F(Vertexes[Triangles[i].PointC].NormX,
                 Vertexes[Triangles[i].PointC].NormY,
                 Vertexes[Triangles[i].PointC].NormZ);
      //*)
      AffecterCouleur(Vertexes[Triangles[i].PointC].Z, FDegrade);
      glVertex3f(Vertexes[Triangles[i].PointC].X,
                 Vertexes[Triangles[i].PointC].Y,
                 Vertexes[Triangles[i].PointC].Z);

    end;
  // verso
  for i:=0 to FNbTriangles-1 do
    begin
      if FDoDrawSelectedTrianglesOnly then begin
        if Not Triangles[i].Marked then Continue;
      end;
      glNormal3F(-Vertexes[Triangles[i].PointC].NormX,
                 -Vertexes[Triangles[i].PointC].NormY,
                 -Vertexes[Triangles[i].PointC].NormZ);
      //*)
      AffecterCouleur(Vertexes[Triangles[i].PointC].Z, FDegrade);
      glVertex3f(Vertexes[Triangles[i].PointC].X,
                 Vertexes[Triangles[i].PointC].Y,
                 Vertexes[Triangles[i].PointC].Z);

      glNormal3F(-Vertexes[Triangles[i].PointB].NormX,
                 -Vertexes[Triangles[i].PointB].NormY,
                 -Vertexes[Triangles[i].PointB].NormZ);
      //*)
      AffecterCouleur(Vertexes[Triangles[i].PointB].Z, FDegrade);
      glVertex3f(Vertexes[Triangles[i].PointB].X,
                 Vertexes[Triangles[i].PointB].Y,
                 Vertexes[Triangles[i].PointB].Z);

      glNormal3F(-Vertexes[Triangles[i].PointA].NormX,
                 -Vertexes[Triangles[i].PointA].NormY,
                 -Vertexes[Triangles[i].PointA].NormZ);
      AffecterCouleur(Vertexes[Triangles[i].PointA].Z, FDegrade);
      glVertex3f(Vertexes[Triangles[i].PointA].X,
                 Vertexes[Triangles[i].PointA].Y,
                 Vertexes[Triangles[i].PointA].Z);
    end;
  glEnd; // GL_TRIANGLES;
end;

function TMaillage.GetCoordosVertexToPlan(const PM: TVertex): TPoint; overload;
var
  EWE : TPoint2Df;
begin
  EWE.X  := PM.X;
  EWE.Y  := PM.Y;
  Result := FProcGCSToSRC(EWE);
end;
function TMaillage.GetCoordosPlan(const PM: TPoint3Df): TPoint; overload;
var
  EWE : TPoint2Df;
begin
  EWE.X  := PM.X;
  EWE.Y  := PM.Y;
  Result := FProcGCSToSRC(EWE);
end;


// extraction des coordonnées d'intersection d'un plan vertical et d'un triangle
// QT: Triangle traversé
// QX1, QX2, QY1, QY2: Coordonnées des extrémités de la trace horizontale du plan
function TMaillage.GetIntersectPlanV_Triangle(const QT: TTriangleABC; const ExtrProfil1, ExtrProfil2: TPoint3Df; out QIntersect: TIntersectPlanTriangle): boolean;
var
  P1, P2, P3: TPoint3Df;
  function CalcZ(const AP1, AP2: TPoint3Df; const QIx, QIy: double): double;
  var
    QR1, QR2, m: double;
    dx, dy, dz : double;
  begin

    dx := QIx - AP1.X;
    dy := QIy - AP1.y;

    QR1 := Hypot(AP2.X - AP1.X, AP2.Y - AP1.Y);
    QR2 := Hypot(dx, dy);
    dz  := AP2.z - AP1.z;

    m   := QR2 / QR1;
    Result := AP1.Z + m * dz;
  end;
  function CalcIntersect(const AP1, AP2, AP3: TPoint3Df) : boolean;
  var
    IX1, IY1: TGeoFloat;
  begin
    Result := False;
    // premier côté ?
    if Intersect(TGeoFloat(AP1.X), TGeoFloat(AP1.Y), TGeoFloat(AP2.X), TGeoFloat(AP2.Y),
                TGeoFloat(ExtrProfil1.X), TGeoFloat(ExtrProfil1.Y),
                TGeoFloat(ExtrProfil2.X), TGeoFloat(ExtrProfil2.Y),
                IX1, IY1) then
    begin
      QIntersect.X1 := IX1;
      QIntersect.Y1 := IY1;
      QIntersect.Z1 := CalcZ(AP1, AP2, IX1, IY1);


      // coté suivant ?
      if Intersect(TGeoFloat(AP2.X), TGeoFloat(AP2.Y), TGeoFloat(AP3.X), TGeoFloat(AP3.Y),
              TGeoFloat(ExtrProfil1.X), TGeoFloat(ExtrProfil1.Y),
              TGeoFloat(ExtrProfil2.X), TGeoFloat(ExtrProfil2.Y),
              IX1, IY1)
      then
      begin
        QIntersect.X2 := IX1;
        QIntersect.Y2 := IY1;
        QIntersect.Z2 := CalcZ(AP2, AP3, IX1, IY1);
      end else
      begin
        Intersect(TGeoFloat(AP3.X), TGeoFloat(AP3.Y), TGeoFloat(AP1.X), TGeoFloat(AP1.Y),
              TGeoFloat(ExtrProfil1.X), TGeoFloat(ExtrProfil1.Y),
              TGeoFloat(ExtrProfil2.X), TGeoFloat(ExtrProfil2.Y),
              IX1, IY1);
        QIntersect.X2 := IX1;
        QIntersect.Y2 := IY1;
        QIntersect.Z2 := CalcZ(AP3, AP1, IX1, IY1);
      end;
      Result := True;
    end;

  end;
begin
  Result := False;
  {*
  P1 := GetVertex(QT.VertexA);
  P2 := GetVertex(QT.VertexB);
  P3 := GetVertex(QT.VertexC);
  (*
  AfficherMessage(Format('P1 =(%.2f, %.2f, %.2f), P2 = (%.2f, %.2f, %.2f), P3 = (%.2f, %.2f, %.2f)',
                              [P1.X, P1.Y, P1.Z,
                               P2.X, P2.Y, P2.Z,
                               P3.X, P3.Y, P3.Z
                              ]));
  //*)
  if      CalcIntersect(P1, P2, P3) then begin Result := True; Exit; end
  else if CalcIntersect(P2, P3, P1) then begin Result := True; Exit; end
  else if CalcIntersect(P3, P1, P2) then begin Result := True; Exit; end
  //*}
end;

procedure TMaillage.TracerMaillage(const Cnv: TCanvas; const QP: TProcGCSToSRC; const Isovaleur: double; const CouleurIsoValeur: TColor);
var
  PP1, PP2, PP3: TPoint;
  PM: TPoint3Df;
  i: Integer;
  TR: TTriangleABC;
  VX1, VX2, VX3: TVertex;
  QT: TGeoTriangle3D;
  I1, I2: TGeoPoint3D;
  PP: TPoint;
begin
  AfficherMessage(Format('%s.TracerMaillage()', [ClassName]));
  FProcGCSToSRC := QP;
  try
    AfficherMessage('-- Essai de la fonction de conversion');
    PP1 := GetCoordosPlan(FCoordsMini);
    PP2 := GetCoordosPlan(FCoordsMaxi);
    AfficherMessage(Format('Coords mini: %.0f, %.0f, %.0f -> %d, %d', [FCoordsMini.X, FCoordsMini.Y, FCoordsMini.Z, PP1.X, PP1.Y]));
    AfficherMessage(Format('Coords mini: %.0f, %.0f, %.0f -> %d, %d', [FCoordsMaxi.X, FCoordsMaxi.Y, FCoordsMaxi.Z, PP2.X, PP2.Y]));
    // le tracé lui même
    Cnv.Pen.Color := clYellow;
    Cnv.Pen.Style := psSolid;
    Cnv.Pen.Width := 0;
    for i:= 0 to GetNbTriangles - 1 do
    begin
      TR := GetTriangle(i);
      VX1 := GetVertex(TR.PointA);
      VX2 := GetVertex(TR.PointB);
      VX3 := GetVertex(TR.PointC);
      PP1 := GetCoordosVertexToPlan(VX1);
      PP2 := GetCoordosVertexToPlan(VX2);
      PP3 := GetCoordosVertexToPlan(VX3);
      Cnv.MoveTo(PP1);
      Cnv.LineTo(PP2);
      Cnv.LineTo(PP3);
      Cnv.LineTo(PP1);
    end;
    // tracé des isovaleurs
    if (Isovaleur > 0.00) then
    begin
      Cnv.Pen.Color := CouleurIsoValeur;
      Cnv.Pen.Style := psSolid;
      Cnv.Pen.Width := 2;
      for i:= 0 to GetNbTriangles - 1 do
      begin
        TR := GetTriangle(i);
        QT := GetGeoTriangle3D(TR);
        if (IntersectPlanHorizontalTriangle(Isovaleur + 1e-03, QT, I1, I2)) then
        begin
          //AfficherMessage('Isovaleur');

          PM.X := I1.X; PM.Y := I1.Y; PM.Z := Isovaleur;
          PP1 := GetCoordosPlan(PM);
          PM.X := I2.X; PM.Y := I2.Y;

          PP2 := GetCoordosPlan(PM);
          Cnv.Line(PP1.X, PP1.Y, PP2.X, PP2.Y);
        end;
      end;
    end;
  except
  end;
end;

// transformation d'un triangle TTriangulation en TGeoTriangle;
function TMaillage.GetGeoTriangle2D(const QT: TTriangleABC): TGeoTriangle2D;
var
 V1, V2, V3: TVertex;
begin
 V1 := GetVertex(QT.PointA);
 V2 := GetVertex(QT.PointB);
 V3 := GetVertex(QT.PointC);

 Result[1].x := V1.X;
 Result[1].y := V1.Y;

 Result[2].x := V2.X;
 Result[2].y := V2.Y;

 Result[3].x := V3.X;
 Result[3].y := V3.Y;


 //function Intersect(const Line:TGeoLine2D;         const Triangle  : TGeoTriangle2D):Boolean;
end;
// transformation d'un triangle TTriangulation en TGeoTriangle;
function TMaillage.GetGeoTriangle3D(const QT: TTriangleABC): TGeoTriangle3D;
var
 V1, V2, V3: TVertex;
begin
 V1 := GetVertex(QT.PointA);
 V2 := GetVertex(QT.PointB);
 V3 := GetVertex(QT.PointC);

 Result[1].x := V1.X;
 Result[1].y := V1.Y;
 Result[1].z := V1.Z;


 Result[2].x := V2.X;
 Result[2].y := V2.Y;
 Result[2].z := V2.Z;


 Result[3].x := V3.X;
 Result[3].y := V3.Y;
 Result[3].z := V3.Z;
end;


end.
