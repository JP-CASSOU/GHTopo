unit UnitClasseMaillage;
interface
uses
  StructuresDonnees,
  Windows,
  Common,
  GL, GLu,
  SysUtils,Classes,
  Graphics, Controls, Forms, Dialogs,
  ExtCtrls;
type TMaillage = class
   procedure Free;
   function ReDimMaillage:integer;
   function LireFichierMaillage: integer;
   function ImporterMaillageGRD: integer;
   function ImporterMaillageSUR: integer;
   function SaveToFile(const Fichier: string): integer;
   procedure ConstruireMaillageTriangleWire;
   procedure ConstruireMaillageTriangleSolide;
   procedure ConstruireMaillageWire;
   procedure CalculerGridNormales;
   procedure CalculerNormalesTriangles;
   procedure ConstruireMaillageSolide;
   procedure ConstruireMaillages;
   procedure CalculerNormales;
   procedure CalculerTablesNormales;
   procedure Draw3D(const AngleRot: double;
                    const AngleInc: double;
                    const Zoom: double;
                    const Mode: Byte;
                    const DestDev: TPaintBox;
                    const Destination: TCanvas);
   procedure Draw2D(const Zoom: double;
                    const Mode: Byte;
                    const Equidistance: double;
                    const DestDev: TPaintBox;
                    const Destination: TCanvas);

   private
     FDefined   : boolean;
     FNbLignesX : integer;
     FNbLignesY : integer;
     FXMini     : double;
     FYMini     : double;
     FZMini     : double;
     FXMaxi     : double;
     FYMaxi     : double;
     FZMaxi     : double;
     FPasX      : double;
     FPasY      : double;
     FMult      : double;
     FMagnification : double;
     FBlendCoef : double;
     FFileMaillage: string;
     FCaption     : string;
     FModeDess    : TMaillageModeDessin;
     FColor,
     FColorMin,
     FColorMax    : TColor;
     FDisplayed   : boolean;
     FTypeMaillage: TTypeMaillage;
     FNbVertexes  : integer;
     FNbTriangles : integer;
     FNomTypeMaillage: string;
     FIsCorrect   : boolean;
     FDegrade     : boolean;
     FDiagonale   : double;
     FRayon       : double;
   public
     ZNormales  : array of array of TPoint3Df;
     ZMatrix    : TGridMNTArray;
     Vertexes   : array of TMaillageVertex;
     Triangles  : array of TTriangleABC;

end;


implementation
const PI180 = PI/180.0;

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

  for i:=1 to FNbVertexes do
    begin
      Vertexes[i].NormX:=0.0;
      Vertexes[i].NormY:=0.0;
      Vertexes[i].NormZ:=0.0;
      Vertexes[i].Norme:=0.0;

      //ShowMessageFmt('%f %f %f',[Vertexes[i].NormX, Vertexes[i].NormY, Vertexes[i].NormZ]);
    end;

  for i:=1 to FNbTriangles do
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
      //ShowMessageFmt('%f %f %f',[Vertexes[i].NormX, Vertexes[i].NormY, Vertexes[i].NormZ]);
    end;
  //*)
end;
function TMaillage.ImporterMaillageGRD: integer;
const
  FT1= '   %s: Mini = %.2f Maxi = %.2f - Nb = %d - Pas = %.2f';
var
  i,j, Cnt: integer;
  q: boolean;
  pGRD: TextFile;
  Ligne, Toto: string;
  function IsGRDTextFile(const f: string): boolean;
  var
    p   : File;
    Buff: array[0..3] of Char;
    t: integer;
    b: string;
  begin
    Result:=False;
    try
      try
        AssignFile(P, FFileMaillage);
        Reset(p,4);
        BlockRead(P, Buff, 1,t);
        b:=Buff;
        //showmessagefmt('%d de %s',[t,b]);
        Result:=(b='DSAA');
      except
      end;
    finally
      CloseFile(p);
    end;
  end;
begin

  Result:=-1;
  // si fichier GRD introuvable, quitter
  if Not FileExists(FFileMaillage) then Exit;
  // si fichier <> DSAA, quitter
  q:=IsGRDTextFile(FFileMaillage);
  AfficherMessage(Format('%s.ImporterMaillageGRD: %s',
                         [ClassName,
                          IIF(q, 'Valid ASCII Surfer Grid', 'Unknown Grid format')]));
  //exit;
  if Not q then Exit;
  // Type de maillage:
  FTypeMaillage:=tmREGULAR_GRID;
  // Couleurs par défaut
  FColor:=clGREEN;
  FColorMax:=FColor;
  FColorMin:=clMAROON;


  //***********************
  // lecture du fichier
  try
    AssignFile(pGRD, FFileMaillage);
    Reset(pGRD);
    ReadLn(pGRD, Ligne); // première ligne DSAA
    ReadLn(pGRD, Ligne); // nb de mailles
    FNbLignesX:=strtoint(
                        Trim(
                        copy(Trim(Ligne),
                             0,
                             pos(' ',Trim(Ligne))
                            )
                            )
                       );
    FNbLignesY:=strtoint(
                        Trim(
                        copy(ligne,
                             1+pos(' ',ligne),
                             length(ligne)-1+pos(' ',ligne)
                            )
                            )
                       );
    // produit m x n nul ==> erreur obligatoire
    if FNbLignesX*FNbLignesY = 0 then begin
      AfficherMessage(Format('Null product of %d * %d ==> erroneous parameters',
                             [FNbLignesX,FNbLignesY]));
      Result:=-1;
      Exit;
    end;

    // lire les valeurs extremes de X
    ReadLn(pGRD,Ligne);
    FXMini := strtofloat(
                        Trim(
                        copy(Trim(Ligne),
                             0,
                             pos(' ',Trim(Ligne))
                            )
                            )
                       );
    FXMaxi:=strtofloat(
                        Trim(
                        copy(Ligne,
                             1+pos(' ',Ligne),
                             length(Ligne)-1+pos(' ',Ligne)
                            )
                            )
                       );

    // lire les valeurs extremes de Y
    ReadLn(pGRD,Ligne);
    FYMini:=strtofloat(
                        Trim(
                        copy(Trim(Ligne),
                             0,
                             pos(' ',Trim(Ligne))
                            )
                            )
                       );
    FYMaxi:=strtofloat(
                        Trim(
                        copy(Ligne,
                             1+pos(' ',Ligne),
                             length(Ligne)-1+pos(' ',Ligne)
                            )
                            )
                       );
    // lire les valeurs extremes de Z
    ReadLn(pGRD,Ligne);
    FZMini:=strtofloat(
                        Trim(
                        copy(Trim(Ligne),
                             0,
                             pos(' ',Trim(Ligne))
                            )
                            )
                       );
    FZMaxi:=strtofloat(
                        Trim(
                        copy(Ligne,
                             1+pos(' ',Ligne),
                             length(Ligne)-1+pos(' ',Ligne)
                            )
                            )
                       );

    // calcul du pas
    FPasX := (FXMaxi-FXMini)/(FNbLignesX-1);
    FPasY := (FYMaxi-FYMini)/(FNbLignesY-1);
    // Affectation des abscisses et ordonnées
    // mettre le tableau des Z au carré
    ReDimMaillage;
    for i:=1 to FNbLignesX do
      ZMatrix[0,i]:=FXMini + (i-1)*FPasX;
    for j:=1 to FNbLignesY do
      ZMatrix[j,0]:=FYMini + (j-1)*FPasY;

    // affichage de contrôle
    AfficherMessage(Format(FT1,['X',FXMini, FXMaxi, FNbLignesX, FPasX]));
    AfficherMessage(Format(FT1,['Y',FYMini, FYMaxi, FNbLignesY, FPasY]));
    AfficherMessage('  ZMatrix OK');
    for j:=1 to FNbLignesY do begin
       Cnt:=0;
       while Cnt<=FNbLignesX do begin
           ReadLn(pGRD,ligne);
           ligne:=trim(ligne);
           while Pos(' ',ligne)<> 0 do begin
               Cnt:=Cnt+1;
               Toto:=copy(ligne,0,
                          pos(' ', ligne));
               ZMatrix[j,cnt]:=strtofloat(Toto);
               ligne:=trim(
                          copy(ligne,
                               pos(' ', ligne),
                               length(ligne)-1+pos(' ',ligne)
                              )
                          );
             end;
             Cnt:=1+Cnt;
             if Trim(ligne)<>'' then ZMatrix[j,Cnt]:=strtofloat(ligne);
         end;
     end;
     // ZMin et ZMax
     FZMini:=1e20;
     FZMaxi:=-1e20;
     for i:=1 to FNbLignesY do
       for j:=1 to FNbLignesX do begin
           //Readln(PFileMAI,TmpBuff);
           ZMatrix[i,j]:=ZMatrix[i,j] * FMult * FMagnification;
           if ZMatrix[i,j]>FZMaxi then FZMaxi:=ZMatrix[i,j];
           if ZMatrix[i,j]<FZMini then FZMini:=ZMatrix[i,j];
          end;
     // réglages finaux
     FDefined:=True;
     FNomTypeMaillage:=Format('Grille: (%d * %d)',
                              [FNbLignesX-1, FNbLignesY-1]);
     FRayon:=0.50 * Sqrt(Sqr(FXMaxi-FXMini)+
                         Sqr(FYMaxi-FYMini)+
                         Sqr(FZMaxi-FZMini));

     // réussi ici
     Result:=FNbLignesX*FNbLignesY;
  finally
    CloseFile(pGRD);
  end;
end;
procedure TMaillage.ConstruireMaillages;
begin

  case FTypeMaillage of
    tmREGULAR_GRID: begin


        case FModeDess of
           M3D_WIRE_FRAME    : ConstruireMaillageWire;
           M3D_MESH,M3D_BLEND: ConstruireMaillageSolide;
        end;
      end;
    tmTRIANGLES: begin
        case FModeDess of

           M3D_WIRE_FRAME    : ConstruireMaillageTriangleWire;
           M3D_MESH,M3D_BLEND: ConstruireMaillageTriangleSolide;
        end;
      end;
    else
     ;
  end;
end;
function TMaillage.ReDimMaillage:integer;
begin
  try
    Result:=-2;
//    SetLength(Z_Grid,M3D_nbLinesY+1,M3D_nbLinesX+1);
    SetLength(ZMatrix,   1+FNbLignesY, 1+FNbLignesX);
    Result:=-1;
    SetLength(ZNormales, 1+FNbLignesY, 1+FNbLignesX);
    //ShowMessageFmt('%d %d', [FNbLignesX,FNbLignesY]);

    Result:= FNbLignesX * FNbLignesY;

  finally
  end;
end;

function TMaillage.LireFichierMaillage: integer;
var
  i, j         : integer;
  pFileMAI     : TextFile;
  TmpBuff      : string;
  postab       : integer;
  s            : string;

  V1, V2, W    : TPoint3Df;
  Params       : TStringArray;
begin
  Result:=-1;
  try
    try
      if Not (FileExists(FFileMaillage)) then
        begin
          Result:=-1;
          exit;
        end;
      FDisplayed:=True;
      // Lecture du fichier
      AssignFile(PFileMAI,FFileMaillage);
      Reset(PFileMAI);
      // Titre du maillage
      Readln(PFileMAI,TmpBuff);
      FCaption:=TmpBuff;
      // Type de maillage
      Readln(PFileMAI,TmpBuff);

      if Trim(UpperCase(TmpBuff))='REGULAR_GRID' then
        begin
          FTypeMaillage:=tmREGULAR_GRID;

        end
      else if Trim(UpperCase(TmpBuff))='TRIANGLES' then
        begin
          FTypeMaillage:=tmTRIANGLES;

        end
      else
        begin
          FTypeMaillage:=tmUNKNOWN;
          FNomTypeMaillage:='Type inconnu';
        end;
      case FTypeMaillage of
        tmREGULAR_GRID: begin
            //Couleur
            Readln(PFileMAI,TmpBuff);
            FColor:=StrToInt(TmpBuff);
            FColorMax:=FColor;
            Readln(PFileMAI,TmpBuff);
            FColorMin:=StrToInt(TmpBuff);
            // Nombre de lignes
            Readln(PFileMAI,TmpBuff);
            FNbLignesX:=StrToInt(TmpBuff);
            Readln(PFileMAI,TmpBuff);
            FNbLignesY:=StrToInt(TmpBuff);
            // Coordonnées mini
            Readln(PFileMAI,TmpBuff);
            FXMini:=StrToFloat(TmpBuff) * FMult;
            Readln(PFileMAI,TmpBuff);
            FYMini:=StrToFloat(TmpBuff) * FMult;
            // Coordonnées maxi
            Readln(PFileMAI,TmpBuff);
            FXMaxi:=StrToFloat(TmpBuff) * FMult;
            Readln(PFileMAI,TmpBuff);
            FYMaxi:=StrToFloat(TmpBuff) * FMult;
            // redimensionner les tableaux
            SetLength(Vertexes,0);
            SetLength(Triangles,0);

            SetLength(ZMatrix,   1+FNbLignesY, 1+FNbLignesX);
            SetLength(ZNormales, 1+FNbLignesY, 1+FNbLignesX);
              // Calcul du pas
            FPasX:=(FXMaxi-FXMini)/(FNbLignesX-1);
            FPasY:=(FYMaxi-FYMini)/(FNbLignesY-1);
            // Affectation des abscisses et ordonnées
            for i:=1 to FNbLignesX do
              ZMatrix[0,i]:=FXMini + (i-1)*FPasX;
            for j:=1 to FNbLignesY do
              ZMatrix[j,0]:=FYMini + (j-1)*FPasY;
            // lecture d'une donnée obsolète
            ReadLn(pFileMAI, TmpBuff);
            // chargement de la table
            FZMini:=1e+10;
            FZMaxi:=-1e+10;
            for i:=1 to FNbLignesY do
              for j:=1 to FNbLignesX do
                 begin
                   Readln(PFileMAI,TmpBuff);
                   ZMatrix[i,j]:=strtofloat(TmpBuff) * FMult * FMagnification;
                   if ZMatrix[i,j]>FZMaxi then FZMaxi:=ZMatrix[i,j];
                   if ZMatrix[i,j]<FZMini then FZMini:=ZMatrix[i,j];
                  end;
            Result:=FNbLignesX * FNbLignesY;
            FDefined:=True;
            FNomTypeMaillage:=Format('Grille: (%d * %d)',
                                   [FNbLignesX-1, FNbLignesY-1]);
            FRayon:=0.50 * Sqrt(Sqr(FXMaxi-FXMini)+
                                Sqr(FYMaxi-FYMini)+
                                Sqr(FZMaxi-FZMini));

        end;
      tmTRIANGLES: begin

            //Couleur
            Readln(PFileMAI,TmpBuff);
            FColor:=StrToInt(TmpBuff);
            FColorMax:=FColor;
            Readln(PFileMAI,TmpBuff);
            FColorMin:=StrToInt(TmpBuff);
            // section Vertex
            Readln(PFileMAI,TmpBuff);
            // nombre de sommets
            Readln(PFileMAI,TmpBuff);
            FNbVertexes:=StrToInt(TmpBuff);
            SetLength(Vertexes,1+FNbVertexes);
            with Vertexes[0] do
            begin
              ID:=0;
              X:=0.0;
              Y:=0.0;
              Z:=0.0;
              NormX:=0.0;
              NormY:=0.0;
              NormZ:=0.0;
              Norme:=0;
            end;
            for i:=1 to FNbVertexes do
            begin
              Readln(PFileMAI,TmpBuff);

              Params := Split(TmpBuff, TAB);
              Vertexes[i].ID := StrToIntDef(Params[0], 0);
              Vertexes[i].X  := StrToFloat(Params[1]) * FMult * FMagnification;
              Vertexes[i].Y  := StrToFloat(Params[2]) * FMult * FMagnification;
              Vertexes[i].Z  := StrToFloat(Params[3]) * FMult * FMagnification;
              Vertexes[i].NormX:=0.0;
              Vertexes[i].NormY:=0.0;
              Vertexes[i].NormZ:=0.0;
              Vertexes[i].Norme:=0.0;


              //ShowMessageFmt('%d - %f %f %f',[i,Vertexes[i].X, Vertexes[i].Y, Vertexes[i].Z]);
            end;
            // minimax
            FXMini:=1e+10;
            FXMaxi:=-1e+10;
            FYMini:=1e+10;
            FYMaxi:=-1e+10;
            FZMini:=1e+10;
            FZMaxi:=-1e+10;
            for i:=1 to FNbVertexes do
            begin
              if Vertexes[i].Z<FXMini then FXmini:=Vertexes[i].Z;
              if Vertexes[i].Z<FYMini then FYmini:=Vertexes[i].Z;
              if Vertexes[i].Z<FZMini then FZmini:=Vertexes[i].Z;
              if Vertexes[i].Z>FXMaxi then FXmaxi:=Vertexes[i].Z;
              if Vertexes[i].Z>FYMaxi then FYmaxi:=Vertexes[i].Z;
              if Vertexes[i].Z>FZMaxi then FZmaxi:=Vertexes[i].Z;

            end;
            FRayon:=0.50 * Sqrt(Sqr(FXMaxi-FXMini)+
                                Sqr(FYMaxi-FYMini)+
                                Sqr(FZMaxi-FYMini));

            // lire le '-1'
            ReadLn(pFileMAI, TmpBuff);
            // lire le TRIANGLES
            ReadLn(pFileMAI, TmpBuff);
            // nombre de triangles
            Readln(PFileMAI,TmpBuff);
            FNbTriangles:=StrToInt(TmpBuff);
            SetLength(Triangles,1+FNbTriangles);

            for i:=1 to FNbTriangles do
            begin
              Readln(PFileMAI,TmpBuff);
              Params := Split(TmpBuff, TAB);

              Triangles[i].Numero := StrToIntDef(Params[0],0);
              Triangles[i].PointA := StrToIntDef(Params[1],0);

              Triangles[i].PointB := StrToIntDef(Params[2],0);
              Triangles[i].PointC := StrToIntDef(Params[3],0);
            end;
            FNomTypeMaillage:=Format('Triangles: (%d)',[FNbTriangles]);
            // initialiser tableaux inutilisés
            SetLength(ZMatrix,   1,1);
            SetLength(ZNormales, 1,1);
            FDefined:=True;
            Result:=FNbTriangles;

        end;
      tmUNKNOWN: begin
           // initialiser tableaux inutilisés
           SetLength(Triangles,1);
           SetLength(Vertexes,1);
           SetLength(ZMatrix,   1,1);
           SetLength(ZNormales, 1,1);
           Result:=-64;
        end;
      end;
      CloseFile(pFileMAI);
      //ShowMessageFmt('%f %f -  %f %f - %f %f',[FXMini, FYMini, FZMini,FXMaxi,FYMaxi,FZMaxi]);
    except
    end;
  finally

  end;
end;

procedure TMaillage.Free;
begin
  SetLength(Vertexes,0);
  SetLength(Triangles,0);
  SetLength(ZMatrix,0);
  SetLength(ZNormales,0);
  inherited Free;
end;


procedure TMaillage.ConstruireMaillageWire;
var
  Idx_X_Dep,
  Idx_Y_Dep,
  Idx_X_Arr,
  Idx_Y_Arr   :  integer;

  procedure DessinerUneGrilleMNT(const Idx_X_Dep, Idx_Y_Dep, Idx_X_Arr, Idx_Y_Arr: integer);
  //const colTerrain:Array[0..3] of GLFloat=(1.0, 0.0,0.0,0.0);
  var
    i,j: integer;
      procedure AffecterCouleur(const z: double);
      var
        c: TGLColor;
      begin
        if FDegrade then
        begin
          c:= PascalToGLColor(GetColorDegrade(z,
                                        FZMini,
                                        FZMaxi,
                                        FColorMin, FColorMax));
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

  if Not(FDefined) then Exit;

  Idx_X_Dep:=3;
  Idx_Y_Dep:=3;
  Idx_X_Arr:=FNbLignesX;
  Idx_Y_Arr:=FNbLignesY;
  WriteLn(Format('ConstruireMaillageWire(%d %d %d %d)',[Idx_X_Dep,Idx_Y_Dep,FNbLignesX,FNbLignesY]));
  DessinerUneGrilleMNT(Idx_X_Dep, Idx_Y_Dep, Idx_X_Arr, Idx_Y_Arr);
end;

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

  if Not(FDefined) then Exit;
  Idx_X_Min:=3;
  Idx_Y_Min:=3;
  Idx_X_Max:=FNbLignesX;
  Idx_Y_Max:=FNbLignesY;
  FPasX:=(FXMaxi-FXMini)/(FNbLignesX-1);
  FPasY:=(FYMaxi-FYMini)/(FNbLignesY-1);
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

procedure TMaillage.ConstruireMaillageSolide;
var
  i,j: integer;
  Idx_X_Max,
  Idx_Y_Max   : integer;
  procedure AffecterCouleur(const z: double);
  var
    c: TGLColor;
  begin
    if FDegrade then
    begin
      c:=PascalToGLColor(GetColorDegrade(z,
                                    FZMini,
                                    FZMaxi,
                                    FColorMin, FColorMax));
      glMaterialfv(GL_FRONT, GL_AMBIENT, @c);
    end;
  end;
begin
  Idx_X_Max:=FNbLignesX;
  Idx_Y_Max:=FNbLignesY;
  //exit;
  glBegin(GL_TRIANGLES);
  for j:=3 to Idx_Y_Max-1 do
      for i:=3 to Idx_X_Max-1 do
          begin
              //triangle 1
              glNormal3f(ZNormales[j-1,i-1].X,
                         ZNormales[j-1,i-1].Y,
                         ZNormales[j-1,i-1].Z);
              AffecterCouleur(ZMatrix[j-1,i-1]);
              glVertex3f(ZMatrix[0  ,i-1],
                         ZMatrix[j-1,0],
                         ZMatrix[j-1,i-1]);
              glNormal3f(ZNormales[j-1,i].X,
                         ZNormales[j-1,i].Y,
                         ZNormales[j-1,i].Z);
              AffecterCouleur(ZMatrix[j-1,i]);
              glVertex3f(ZMatrix[0,i],
                         ZMatrix[j-1,0],
                         ZMatrix[j-1,i]);
              glNormal3f(ZNormales[j,i].X,
                         ZNormales[j,i].Y,
                         ZNormales[j,i].Z);
              AffecterCouleur(ZMatrix[j,i]);
              glVertex3f(ZMatrix[0,i],
                         ZMatrix[j,0],
                         ZMatrix[j,i]);


              glNormal3f(ZNormales[j-1,i-1].X,
                         ZNormales[j-1,i-1].Y,
                         ZNormales[j-1,i-1].Z);
              AffecterCouleur(ZMatrix[j-1,i-1]);
              glVertex3f(ZMatrix[0  ,i-1],
                         ZMatrix[j-1,0],
                         ZMatrix[j-1,i-1]);
              glNormal3f(ZNormales[j,i].X,
                         ZNormales[j,i].Y,
                         ZNormales[j,i].Z);
              AffecterCouleur(ZMatrix[j,i]);
              glVertex3f(ZMatrix[0,i],
                         ZMatrix[j,0],
                         ZMatrix[j,i]);
              glNormal3f(ZNormales[j,i-1].X,
                         ZNormales[j,i-1].Y,
                         ZNormales[j,i-1].Z);
              AffecterCouleur(ZMatrix[j,i-1]);
              glVertex3f(ZMatrix[0,i-1],
                         ZMatrix[j,0],
                         ZMatrix[j,i-1]);



              //(*
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
procedure TMaillage.CalculerNormalesTriangles;
var i: integer;
begin
  exit;
  for i:=1 to FNbTriangles do
    begin

    end;
end;
procedure TMaillage.ConstruireMaillageTriangleWire;
var
  i: integer;
  procedure AffecterCouleur(const z: double);
  var
    c: TGLColor;
  begin
    if FDegrade then
    begin
      c:=PascalToGLColor(GetColorDegrade(z,
                                    FZMini,
                                    FZMaxi,
                                    FColorMin, FColorMax));
      glColor3fv(@c);
    end;
  end;

begin
  //showmessage('Maillage.ConstruireMaillageTriangleSolide');

  for i:=1 to FNbTriangles do
    begin
      glBegin(GL_LINE_LOOP);
      AffecterCouleur(Vertexes[Triangles[i].PointA].Z);
      glVertex3f(Vertexes[Triangles[i].PointA].X,
                   Vertexes[Triangles[i].PointA].Y,
                   Vertexes[Triangles[i].PointA].Z);
      AffecterCouleur(Vertexes[Triangles[i].PointB].Z);
      glVertex3f(Vertexes[Triangles[i].PointB].X,
                   Vertexes[Triangles[i].PointB].Y,
                   Vertexes[Triangles[i].PointB].Z);
      AffecterCouleur(Vertexes[Triangles[i].PointC].Z);
      glVertex3f(Vertexes[Triangles[i].PointC].X,
                   Vertexes[Triangles[i].PointC].Y,
                   Vertexes[Triangles[i].PointC].Z);
      AffecterCouleur(Vertexes[Triangles[i].PointA].Z);
      glVertex3f(Vertexes[Triangles[i].PointA].X,
                   Vertexes[Triangles[i].PointA].Y,
                   Vertexes[Triangles[i].PointA].Z);
      glEnd;
  end;
end;
procedure TMaillage.ConstruireMaillageTriangleSolide;
var
  i: integer;
  procedure AffecterCouleur(const z: double);
  var
    c: TGLColor;
  begin
    if FDegrade then
    begin
      c:=PascalToGLColor(GetColorDegrade(z,
                                    FZMini,
                                    FZMaxi,
                                    FColorMin, FColorMax));
      glMaterialfv(GL_FRONT, GL_AMBIENT, @c);
    end;
  end;

begin
  glBegin(GL_TRIANGLES);
  // recto
  for i:=1 to FNbTriangles do
    begin
      //ShowMessageFmt('%f %f %f',[Vertexes[Triangles[i].PointA].X,
      //           Vertexes[Triangles[i].PointA].Y,
      //           Vertexes[Triangles[i].PointA].Z]);
      glNormal3F(Vertexes[Triangles[i].PointA].NormX,
                 Vertexes[Triangles[i].PointA].NormY,
                 Vertexes[Triangles[i].PointA].NormZ);
      AffecterCouleur(Vertexes[Triangles[i].PointA].Z);
      glVertex3f(Vertexes[Triangles[i].PointA].X,
                 Vertexes[Triangles[i].PointA].Y,
                 Vertexes[Triangles[i].PointA].Z);
      glNormal3F(Vertexes[Triangles[i].PointB].NormX,
                 Vertexes[Triangles[i].PointB].NormY,
                 Vertexes[Triangles[i].PointB].NormZ);
      //*)
      AffecterCouleur(Vertexes[Triangles[i].PointB].Z);
      glVertex3f(Vertexes[Triangles[i].PointB].X,
                 Vertexes[Triangles[i].PointB].Y,
                 Vertexes[Triangles[i].PointB].Z);
      glNormal3F(Vertexes[Triangles[i].PointC].NormX,
                 Vertexes[Triangles[i].PointC].NormY,
                 Vertexes[Triangles[i].PointC].NormZ);
      //*)
      AffecterCouleur(Vertexes[Triangles[i].PointC].Z);
      glVertex3f(Vertexes[Triangles[i].PointC].X,
                 Vertexes[Triangles[i].PointC].Y,
                 Vertexes[Triangles[i].PointC].Z);

    end;
  // verso
  for i:=1 to FNbTriangles do
    begin
      glNormal3F(-Vertexes[Triangles[i].PointC].NormX,
                 -Vertexes[Triangles[i].PointC].NormY,
                 -Vertexes[Triangles[i].PointC].NormZ);
      //*)
      AffecterCouleur(Vertexes[Triangles[i].PointC].Z);
      glVertex3f(Vertexes[Triangles[i].PointC].X,
                 Vertexes[Triangles[i].PointC].Y,
                 Vertexes[Triangles[i].PointC].Z);

      glNormal3F(-Vertexes[Triangles[i].PointB].NormX,
                 -Vertexes[Triangles[i].PointB].NormY,
                 -Vertexes[Triangles[i].PointB].NormZ);
      //*)
      AffecterCouleur(Vertexes[Triangles[i].PointB].Z);
      glVertex3f(Vertexes[Triangles[i].PointB].X,
                 Vertexes[Triangles[i].PointB].Y,
                 Vertexes[Triangles[i].PointB].Z);

      glNormal3F(-Vertexes[Triangles[i].PointA].NormX,
                 -Vertexes[Triangles[i].PointA].NormY,
                 -Vertexes[Triangles[i].PointA].NormZ);
      AffecterCouleur(Vertexes[Triangles[i].PointA].Z);
      glVertex3f(Vertexes[Triangles[i].PointA].X,
                 Vertexes[Triangles[i].PointA].Y,
                 Vertexes[Triangles[i].PointA].Z);


    end;

  glEnd;
end;

procedure TMaillage.Draw3D(const AngleRot: double;
                           const AngleInc: double;
                           const Zoom: double;
                           const Mode: Byte;
                           const DestDev: TPaintBox;
                           const Destination: TCanvas);
const NbOmit=8;
var
  i,j,l         : integer;
  DoubleBuffer:  TBitmap;
  SourceRect,
  DestRect    : TRect;

  RapportME   : double;
  Radius      : integer;
  Offset3D    : TPoint3Df;
  Marge       : integer;
  PT          : TPoint;
  Aux         : array[1..8] of double;
  Cube2d      : array[1..8] of TPoint;
  Tx, Ty      : integer;
  st, ct,
  sp, cp      : double;

begin

  if Not(FDefined) then
    Exit;

  ct:=Cos(AngleRot*PI180); st:=Sin(AngleRot*PI180);
  cp:=Cos(AngleInc*PI180); sp:=Sin(AngleInc*PI180);


  Tx:=0;
  Ty:=0;

  Offset3D.X:=(-(FXMaxi-FXMini)/2)-FXMini;
  Offset3D.Y:=(-(FYMaxi-FYMini)/2)-FYMini;
  Offset3D.Z:=(-(FZMaxi-FZMini)/2)-FZMini;

  (*
  Offset3D.X:=(-(Coin2.X-Coin1.X)/2)-Topo_XMin;
  Offset3D.Y:=(-(Coin2.Y-Coin1.Y)/2)-Topo_YMin;
  Offset3D.Z:=(-(Coin2.Z-Coin1.Z)/2)-Topo_ZMin;
  //*)
  //Offset3D.X:=0;
  //Offset3D.Y:=0;
  //Offset3D.Z:=0;
  for i:=1 to 8 do
    begin
     Cube2D[1].X:=0;
     Cube2D[1].Y:=0;
    end;
  Aux[1] := st;
  Aux[2] := sp;       // | -sin(A)           +cos(A)         0           0 | | x |   | X |
  Aux[3] := ct;   // |                                                 | |   |   |   |
  Aux[4] := cp;       // | -cos(A).sin(P)    -sin(A).sin(P)  cos(A)      0 | | y |   | Y |
  Aux[5] := Aux[3] * Aux[2];    // |                                                 |*|   | = |   |
  Aux[6] := Aux[1] * Aux[2];    // | -cos(A).cos(P)    -sin(A).cos(P)  -sin(P)     R | | z |   | Z |
  Aux[7] := Aux[3] * Aux[4];    // |                                                 | |   |   |   |
  Aux[8] := Aux[1] * Aux[4];    // | 0                 0               0           1 | | 1 |   | 1 |
  //*)


  try

    DoubleBuffer:=TBitmap.Create;
    DoubleBuffer.Height:= DestDev.Height;
    DoubleBuffer.Width := DestDev.Width;
    SourceRect.Left:=0;
    SourceRect.Top:=0;
    SourceRect.Right:=DestDev.Width;
    SourceRect.Bottom:= DestDev.Height;
    //*)
    Marge:=5;
    //ShowMessageFmt('%f',[FRayon]);
    RapportME:=(0.5*(DestDev.Width-Marge)/FRayon) * Zoom;

    //Offset2D.X:=round(Offset3D.X*RapportME)+ ;
    //Offset2D.y:=DestDev.Height-(round(Offset3D.Y*RapportME)+ );
    with DoubleBuffer.Canvas do
      begin
        (* on dessine ici *)
        MoveTo(DestDev.Width div 2,
               DestDev.Height div 2);

        //Tx:= DestDev.Width div 2 + TranslateX;
        //Ty:= DestDev.Height div 2 + TranslateY;
        Tx:= DestDev.Width div 2;
        Ty:= DestDev.Height div 2;
        Radius:=Round(FRayon * RapportME);

        //****************************************

        Cube2D[1].X:=trunc((-(FXMini + Offset3D.X)* Aux[1]+
                             (FYMini + Offset3D.Y)* Aux[3])*RapportME)+
                           Tx;
        Cube2D[1].Y:= DestDev.Height-(
                    trunc((-(FXMini + Offset3D.X)* Aux[5]-
                            (FYMini + Offset3D.Y)* Aux[6]+
                            (FZMini + Offset3D.Z)* Aux[4])*RapportME)+
                           Ty);

        //--------
        Cube2D[2].X:=trunc((-(FXMaxi + Offset3D.X)* Aux[1]+
                             (FYMini + Offset3D.Y) * Aux[3])*RapportME)+
                           Tx;
        Cube2D[2].Y:= DestDev.Height-(
                    trunc((-(FXMaxi + Offset3D.X) * Aux[5]-
                            (FYMini + Offset3D.Y) * Aux[6]+
                            (FZMini + Offset3D.Z) * Aux[4])*RapportME)+
                           Ty);
        Cube2D[3].X:=trunc((-(FXMaxi + Offset3D.X) * Aux[1]+
                             (FYMaxi + Offset3D.Y)* Aux[3])*RapportME)+
                           Tx;
        Cube2D[3].Y:= DestDev.Height-(
                    trunc((-(FXMaxi + Offset3D.X) * Aux[5]-
                            (FYMaxi + Offset3D.Y) * Aux[6]+
                            (FZMini + Offset3D.Z) * Aux[4])*RapportME)+
                           Ty);
        Cube2D[4].X:=trunc((-(FXMini + Offset3D.X) * Aux[1]+
                             (FYMaxi + Offset3D.Y) * Aux[3])*RapportME)+
                           Tx;
        Cube2D[4].Y:= DestDev.Height-(
                    trunc((-(FXMini + Offset3D.X) * Aux[5]-
                            (FYMaxi + Offset3D.Y) * Aux[6]+
                            (FZMini + Offset3D.Z) * Aux[4])*RapportME)+
                           Ty);
        Cube2D[5].Y:= DestDev.Height-(
                    trunc((-(FXMini + Offset3D.X) * Aux[5]-
                            (FYMini + Offset3D.Y) * Aux[6]+
                            (FZMaxi + Offset3D.Z) * Aux[4])*RapportME)+
                           Ty);
        Cube2D[6].Y:= DestDev.Height-(
                    trunc((-(FXMaxi + Offset3D.X) * Aux[5]-
                            (FYMini + Offset3D.Y) * Aux[6]+
                            (FZMaxi + Offset3D.Z) * Aux[4])*RapportME)+
                           Ty);
        Cube2D[7].Y:= DestDev.Height-(
                    trunc((-(FXMaxi + Offset3D.X) * Aux[5]-
                            (FYMaxi + Offset3D.Y) * Aux[6]+
                            (FZMaxi + Offset3D.Z) * Aux[4])*RapportME)+
                           Ty);
        Cube2D[8].Y:= DestDev.Height-(
                    trunc((-(FXMini + Offset3D.X) * Aux[5]-
                            (FYMaxi + Offset3D.Y) * Aux[6]+
                            (FZMaxi + Offset3D.Z) * Aux[4])*RapportME)+
                           Ty);
        //****************************************


        for i:=1 to 4 do
          Cube2D[4+i].X:=Cube2D[i].X;


        // le cube ici
        Pen.Style:=psSolid;
        Pen.Color:=clSilver;
        MoveTo(Cube2D[1].x, Cube2D[1].y);
        LineTo(Cube2D[2].x, Cube2D[2].y);
        LineTo(Cube2D[3].x, Cube2D[3].y);
        LineTo(Cube2D[4].x, Cube2D[4].y);
        LineTo(Cube2D[1].x, Cube2D[1].y);
        LineTo(Cube2D[5].x, Cube2D[5].y);
        LineTo(Cube2D[6].x, Cube2D[6].y);
        LineTo(Cube2D[7].x, Cube2D[7].y);
        LineTo(Cube2D[8].x, Cube2D[8].y);
        LineTo(Cube2D[5].x, Cube2D[5].y);
        for i:=2 to 4 do
          begin
            MoveTo(Cube2D[i].x, Cube2D[i].y);
            LineTo(Cube2D[i+4].x, Cube2D[i+4].y);
          end;
        // dessin du maillage
        Pen.Color:=clBlue;
        for i:=3 to FNbLignesX do
        begin
          Pt.X:=trunc((-(  ZMatrix[0,i-1] + Offset3D.X) * Aux[1]+
                          (ZMatrix[2,0] + Offset3D.Y) * Aux[3])*RapportME)+
                           Tx;
          Pt.Y:= DestDev.Height-(
                    trunc((-(ZMatrix[0,i-1] + Offset3D.X) * Aux[5]-
                            (ZMatrix[2,0] + Offset3D.Y) * Aux[6]+
                            (ZMatrix[2,i-1] + Offset3D.Z) * Aux[4])*RapportME)+
                           Ty);
          case Mode of
                1: begin
                    Pen.Color:=GetColorDegrade(ZMatrix[2,i-1],
                                              FZMini,
                                              FZMaxi,
                                              FColorMin, FColorMax);
                    MoveTo(Pt.X, Pt.Y);
                   end;
                2: begin
                     if ((i Mod NbOmit)=0) or
                        (i=FNbLignesX) then
                     begin
                       Pen.Color:=GetColorDegrade(ZMatrix[2,i-1],
                          FZMini,
                          FZMaxi,
                          FColorMin, FColorMax);
                       MoveTo(Pt.X, Pt.Y);

                     end;
                   end;
          end;
          for j:=3 to FNbLignesY do
            begin

              Pt.X:=trunc((-(ZMatrix[0,i-1] + Offset3D.X) * Aux[1]+
                            (ZMatrix[j,0] + Offset3D.Y) * Aux[3])*RapportME)+
                               Tx;
              Pt.Y:= DestDev.Height-(
                    trunc((-(ZMatrix[0,i-1] + Offset3D.X) * Aux[5]-
                            (ZMatrix[j,0] + Offset3D.Y) * Aux[6]+
                            (ZMatrix[j,i-1] + Offset3D.Z) * Aux[4])*RapportME)+
                           Ty);
              case Mode of
                1: begin
                     Pen.Color:=GetColorDegrade(ZMatrix[j,i-1],
                                                FZMini,
                                                FZMaxi,
                                                FColorMin, FColorMax);
                     LineTo(pt.X, pt.Y);
                   end;
                2: begin

                     if ((i Mod NbOmit)=0) or
                        (i=FNbLignesX) then
                     begin
                       Pen.Color:=GetColorDegrade(ZMatrix[j,i-1],
                                                FZMini,
                                                FZMaxi,
                                                FColorMin, FColorMax);
                       LineTo(pt.X, pt.Y);
                     end;
                     //*)
                   end;
              end;
            end;

        end;


        for j:=3 to FNbLignesY do
        begin


          Pt.X:=trunc((-(ZMatrix[0,2] + Offset3D.X) * Aux[1]+
                        (ZMatrix[j-1,0] + Offset3D.Y) * Aux[3])*RapportME)+
                           Tx;
          Pt.Y:= DestDev.Height-(
                    trunc((-(ZMatrix[0,2] + Offset3D.X) * Aux[5]-
                            (ZMatrix[j-1,0] + Offset3D.Y) * Aux[6]+
                            (ZMatrix[j-1,2] + Offset3D.Z) * Aux[4])*RapportME)+
                           Ty);

          case Mode of
            1: begin
                 Pen.Color:=GetColorDegrade(ZMatrix[j-1,2],
                                    FZMini,
                                    FZMaxi,
                                    FColorMin, FColorMax);
                 MoveTo(Pt.X, Pt.Y);
               end;
            2: begin
                 if ((j Mod NbOmit)=0) or
                     (j=FNbLignesY) then
                 begin
                   Pen.Color:=GetColorDegrade(ZMatrix[j-1,2],
                                    FZMini,
                                    FZMaxi,
                                    FColorMin, FColorMax);
                   MoveTo(Pt.X, Pt.Y);
                 end;
               end;
          end;

          for i:=3 to FNbLignesX do
            begin

              Pt.X:=trunc((-(ZMatrix[0,i] + Offset3D.X) * Aux[1]+
                            (ZMatrix[j-1,0] + Offset3D.Y) * Aux[3])*RapportME)+
                               Tx;
              Pt.Y:= DestDev.Height-(
                    trunc((-(ZMatrix[0,i] + Offset3D.X) * Aux[5]-
                            (ZMatrix[j-1,0] + Offset3D.Y) * Aux[6]+
                            (ZMatrix[j-1,i] + Offset3D.Z) * Aux[4])*RapportME)+
                           Ty);
              case Mode of
                1: begin
                     Pen.Color:=GetColorDegrade(ZMatrix[j-1,i],
                                    FZMini,
                                    FZMaxi,
                                    FColorMin, FColorMax);
                      LineTo(pt.X, pt.Y);
                   end;
                2: begin
                     if ((j Mod NbOmit)=0) or
                        (j=FNbLignesY) then
                     begin
                       Pen.Color:=GetColorDegrade(ZMatrix[j-1,i],
                                    FZMini,
                                    FZMaxi,
                                    FColorMin, FColorMax);
                       LineTo(pt.X, pt.Y);
                     end;
                     //*)
                   end;
              end;

            end;

        end;
        //*)




      end; // with Canvas
   finally
     Destination.CopyRect(SourceRect, DoubleBuffer.Canvas, SourceRect);
     DoubleBuffer.Free;
   end;
   //}
end;

procedure TMaillage.Draw2D(const Zoom: double;
                 const Mode: Byte;
                 const Equidistance: double;
                 const DestDev: TPaintBox;
                 const Destination: TCanvas);
const Marge = 100.0;
var
  DoubleBuffer: TBitmap;
  SourceRect,
  DestRect    : TRect;
  Zo          : double;
  RappYX      : double;
  Fact        : double;
  pt: TPoint;
  NbIntervalles: integer;
  EtendueZ     : double;
  Isohypses    : array of TIsoHypse;
  function W2S(const Xw, Yw: double): TPoint;
  begin
    Result.x:=round((Xw-FXMini) * Fact);
    Result.y:=round(DestDev.Height -
                     ((Yw-FYMini)*Fact));
  end;
  function Process_IsoHypses:integer;
  var
    i,j:integer;

    ar_a: array[0..3] of double;
    ar_b: array[0..3] of double;
    ar_c: array[0..3] of double;

    ar_g: array[1..2] of double;
    ar_h: array[1..2] of double;

    TestMinZ: double;

    x1: double;
    x2: double;
    xi: double;
    xs: double;
    cx: Integer;
    grd_Pas_X: double;

    y1: double;
    y2: double;
    yi: double;
    ys: double;
    cy: Integer;
    grd_Pas_Y: double;


    Z1: double;
    Z2: double;
    Z3: double;
    Z4: double;


    Tmp_w: double;
    Tmp_z: double;


    i_Inf, j_Inf: Integer;
    i_Sup, j_Sup: Integer;
    procedure Triangle;
      var
        k, l, p: integer;
      begin
        For k := 0 To NbIntervalles do
         begin
          Tmp_z := Zo + k * Equidistance;
          p := 0;
          For l := 0 To 2 do
            begin
              If Tmp_z = ar_c[l] Then ar_c[l] := ar_c[l] + 0.0000001 * EtendueZ;
              If Tmp_z = ar_c[l + 1] Then ar_c[l + 1] := ar_c[l + 1] + 0.0000001 * EtendueZ;

              If (Tmp_z - ar_c[l]) *
                 (Tmp_z - ar_c[l + 1]) < 0 Then
                begin

                  Tmp_w := (Tmp_z - ar_c[l]) /
                           (ar_c[1 + l] - ar_c[l]);
                  p := p + 1;
                  ar_h[p] := ar_a[l] + Tmp_w * (ar_a[l + 1] - ar_a[l]);
                  ar_g[p] := ar_b[l] + Tmp_w * (ar_b[l + 1] - ar_b[l]);
                End;
              If p = 2 Then
                begin

                   with DoubleBuffer.Canvas do
                     begin
                       if (Round(Tmp_Z) mod (Round(5*Equidistance))) = 0 then
                         Pen.Color:=clRed
                       else
                         Pen.Color:=clGray;
                       pt:=W2S(ar_h[1] , ar_g[1]);
                       MoveTo(pt.x, pt.y);


                       pt:=W2S(ar_h[2] , ar_g[2]);
                       LineTo(pt.x, pt.y);

                     end;
                   //*)
                end;
            end;
         end;
      end;
  begin
  //  SurfHades.shPLotXY.Crayon.Color:=RGB(100,10,26);
    Process_IsoHypses:=0;
    cx:=trunc(self.FNbLignesX);
    cy:=trunc(self.FNbLignesY);
    xi := ZMatrix[0, 1];
    xs := ZMatrix[0, cx];
    grd_Pas_X := (xs - xi) / cx;

    yi := ZMatrix[1,0];
    ys := ZMatrix[cy,0];
    grd_Pas_Y := (ys - yi) / cy;

    grd_Pas_X:=Self.FPasX;
    grd_Pas_Y:=Self.FPasY;

    i_Inf := 2;
    j_Inf := 2;
    i_Sup := cx;
    j_Sup := cy;


  //dessin du maillage
  For i := i_Inf To i_Sup do
    For j := j_Inf To j_Sup do
      begin
        y1 := yi + (j - 1) * grd_Pas_Y;
        y2 := yi + j * grd_Pas_Y;
        x1 := xi + (i - 1) * grd_Pas_X;
        x2 := xi + i * grd_Pas_X;

        Z1 := ZMatrix[j - 1,i - 1];
        Z2 := ZMatrix[j - 1,i ];
        z3 := ZMatrix[j,i];
        z4 := ZMatrix[j,i-1];//*)

        (*
         calcul et tracé du triangle droit

                       *           * (x2, y2)
                       |        .  |
                       |      .    |             ^
                       |    .      |             |
                       |  .    O   |             | Sens du parcours
                       |.          |             |
              (x1, y1) *-----------* (x2, y1)    -------->
        *)
        (*
            x1  x2  x2
        T = y1  y1  y2
            z1  z2  z3
        *)
        ar_a[1] := x1; ar_b[1] := y1; ar_c[1] := Z1;
        ar_a[2] := x2; ar_b[2] := y1 ; ar_c[2] := Z2;
        ar_a[3] := x2; ar_b[3] := y2; ar_c[3] := z3;
        ar_a[0] := x2; ar_b[0] := y2; ar_c[0] := z3;
        Triangle;
        (*
         calcul et tracé du triangle gauche

              (x1, y2) *-----------* (x2, y2)
                       |        .  |
                       |  O   .    |            ^
                       |    .      |            |
                       |  .    X   |            | Sens du parcours
                       |.          |            |
              (x1, y1) *-----------* (x2, y1)   --------->
        *)
        ar_a[2] := x1; ar_b[2] := y2; ar_c[2] := z4;
        Triangle;
        Z1 := z4;
        Z2 := z3;
      end;


    Process_IsoHypses:=1;


  end;



begin
  if Not(FDefined) then
    Exit;
  try
    // calculs d'intervalles, etc ...
    Zo:=(round(FZMini/100))*100;
    //ShowMessageFmt('%f %f', [FZMini, Zo]);
    EtendueZ:=FZMaxi-FZMini;
    NbIntervalles:=round(EtendueZ/Equidistance);
    SetLength(Isohypses, NbIntervalles + 1);
    //*******************************

    DoubleBuffer:=TBitmap.Create;
    DoubleBuffer.Height:= DestDev.Height;
    DoubleBuffer.Width := DestDev.Width;
    SourceRect.Left:=0;
    SourceRect.Top:=0;
    SourceRect.Right:=DestDev.Width;
    SourceRect.Bottom:= DestDev.Height;
    //*)
    RappYX:=DestDev.Height/DestDev.Width;
    if (FXMaxi - FXMini)>(FYMaxi-FYMini) then
      Fact:=DestDev.Width / (FXMaxi - FXMini)
    else
      Fact:=DestDev.Height / (FYMaxi - FYMini) ;
    // dessin
    with DoubleBuffer.Canvas do
    begin
      Pen.Color:=clBlue;
      pt:=W2S(FXMini, FYMini);
      MoveTo(pt.x, pt.y);
      pt:=W2S(FXMaxi, FYMini);
      LineTo(pt.x, pt.y);
      pt:=W2S(FXMaxi, FYMaxi);
      LineTo(pt.x, pt.y);
      pt:=W2S(FXMini, FYMaxi);
      LineTo(pt.x, pt.y);
      pt:=W2S(FXMini, FYMini);
      LineTo(pt.x, pt.y);

      Process_IsoHypses;

    end;
    //

  finally
     Destination.CopyRect(SourceRect, DoubleBuffer.Canvas, SourceRect);
     DoubleBuffer.Free;
     SetLength(Isohypses, 0);
  end;
end;

function TMaillage.ImporterMaillageSUR: integer;
var
  i, j: integer;
  s1, s2: string;
  p: integer;
  LnTxt: string;
  pSUR : TextFile;
begin;
  AfficherMessage(Format('%s.ImporterMaillageSUR(%s)',[ClassName, FFileMaillage]));
  Result:=-1;
  try
    try
      if Not (FileExists(FFileMaillage)) then
        begin
          Result:=-1;
          exit;
        end;
      // Valeurs par défaut
      FColor:=$CC00;
      FColorMax:=clMaroon;
      FColorMin:=clGreen;
      FDisplayed:=True;
      FTypeMaillage:=tmREGULAR_GRID;
      // Lecture du fichier
      AssignFile(pSUR,FFileMaillage);
      Reset(pSUR);

      // première ligne
      //Surface 403.000 404.000 0.200 6 3089.000 3090.000 0.250 5
      ReadLn(pSUR, LnTxt);
      // Analyse de la ligne:
      // - Libellé surface
      LnTxt:=Trim(LnTxt);
      p:=Pos(#32, LnTxt);
      s1:=Trim(Copy(LnTxt,0, p-1));
      LnTxt:=Trim(Copy(LnTxt, p+1, Length(LnTxt)));
      FCaption:=s1;
      //X Mini
      p:=Pos(#32, LnTxt);
      s1:=Trim(Copy(LnTxt,0, p-1));
      LnTxt:=Trim(Copy(LnTxt, p+1, Length(LnTxt)));
      FXMini:=StrToFloat(s1) * FMult * 1000.0;
      //X Maxi
      p:=Pos(#32, LnTxt);
      s1:=Trim(Copy(LnTxt,0, p-1));
      LnTxt:=Trim(Copy(LnTxt, p+1, Length(LnTxt)));
      FXMaxi:=StrToFloat(s1) * FMult * 1000.0;
      // pas X
      p:=Pos(#32, LnTxt);
      s1:=Trim(Copy(LnTxt,0, p-1));
      LnTxt:=Trim(Copy(LnTxt, p+1, Length(LnTxt)));
      FPasX:=StrToFloat(s1);
      // Nb mailles X
      p:=Pos(#32, LnTxt);
      s1:=Trim(Copy(LnTxt,0, p-1));
      LnTxt:=Trim(Copy(LnTxt, p+1, Length(LnTxt)));
      FNbLignesX:=StrToInt(s1);
      //Y Mini
      p:=Pos(#32, LnTxt);
      s1:=Trim(Copy(LnTxt,0, p-1));
      LnTxt:=Trim(Copy(LnTxt, p+1, Length(LnTxt)));
      FYMini:=StrToFloat(s1) * FMult * 1000.0;
      //Y Maxi
      p:=Pos(#32, LnTxt);
      s1:=Trim(Copy(LnTxt,0, p-1));
      LnTxt:=Trim(Copy(LnTxt, p+1, Length(LnTxt)));
      FYMaxi:=StrToFloat(s1) * FMult * 1000.0;
      // pas Y
      p:=Pos(#32, LnTxt);
      s1:=Trim(Copy(LnTxt,0, p-1));
      LnTxt:=Trim(Copy(LnTxt, p+1, Length(LnTxt)));
      FPasY:=StrToFloat(s1);
      // Nb mailles Y
      p:=Pos(#32, LnTxt);
      s1:=Trim(LnTxt);
      FNbLignesY:=StrToInt(s1);
      // redim tableaux
      SetLength(Vertexes,0);
      SetLength(Triangles,0);

      SetLength(ZMatrix,   1+FNbLignesY, 1+FNbLignesX);
      SetLength(ZNormales, 1+FNbLignesY, 1+FNbLignesX);
      ShowMessage('');
      // lecture ligne vierge
      ReadLn(pSUR, LnTxt);
      FZMini:=1e+10;
      FZMaxi:=-1e+10;
      // lecture des données
              // Calcul du pas
      FPasX:=(FXMaxi-FXMini)/(FNbLignesX-1);
      FPasY:=(FYMaxi-FYMini)/(FNbLignesY-1);
      // Affectation des abscisses et ordonnées
      for i:=1 to FNbLignesX do
        ZMatrix[0,i]:=FXMini + (i-1)*FPasX ;
      for j:=1 to FNbLignesY do
        ZMatrix[j,0]:=FYMini + (j-1)*FPasY ;

      // chargement de la table
      for i:=1 to FNbLignesY do begin
        ReadLn(pSUR, LnTxt);
        LnTxt:=Trim(LnTxt)+#32;
        for j:=1 to FNbLignesX do begin
          p:=Pos(#32, LnTxt);
          s1:=Trim(Copy(LnTxt,0, p-1));
          LnTxt:=Trim(Copy(LnTxt, p+1, Length(LnTxt)))+#32;
          //ZMatrix[i,1+FNbLignesX - j]:=strtofloat(s1) * FMult * FMagnification;
          ZMatrix[1+FNbLignesY-i,j]:=strtofloat(s1) * FMult * FMagnification;
        end;
      end;
      for i:=1 to FNbLignesY do begin
       for j:=1 to FNbLignesX do begin
          if ZMatrix[i,j]>FZMaxi then FZMaxi:=ZMatrix[i,j];
          if ZMatrix[i,j]<FZMini then FZMini:=ZMatrix[i,j];
        end;
      end;
      //AfficherMessage('('%f %f -  %f %f - %f %f',[FXMini, FYMini, FZMini,FXMaxi,FYMaxi,FZMaxi]);

      FDefined:=True;
      FNomTypeMaillage:=Format('Grille: (%d * %d)',
                             [FNbLignesX-1, FNbLignesY-1]);
      FRayon:=0.50 * Sqrt(Sqr(FXMaxi-FXMini)+
                          Sqr(FYMaxi-FYMini)+
                          Sqr(FZMaxi-FZMini));

      Result:=FNbLignesX * FNbLignesY;
    except
    end;
(*

      //Couleur
            Readln(PFileMAI,TmpBuff);

            // Nombre de lignes
            Readln(PFileMAI,TmpBuff);
            FNbLignesX:=StrToInt(TmpBuff);
            Readln(PFileMAI,TmpBuff);
            FNbLignesY:=StrToInt(TmpBuff);
            // Coordonnées mini
            Readln(PFileMAI,TmpBuff);
            FXMini:=StrToFloat(TmpBuff) * FMult;
            Readln(PFileMAI,TmpBuff);
            FYMini:=StrToFloat(TmpBuff) * FMult;
            // Coordonnées maxi
            Readln(PFileMAI,TmpBuff);
            FXMaxi:=StrToFloat(TmpBuff) * FMult;
            Readln(PFileMAI,TmpBuff);
            FYMaxi:=StrToFloat(TmpBuff) * FMult;
            // redimensionner les tableaux
              // Calcul du pas
            FPasX:=(FXMaxi-FXMini)/(FNbLignesX-1);
            FPasY:=(FYMaxi-FYMini)/(FNbLignesY-1);
            // Affectation des abscisses et ordonnées
            for i:=1 to FNbLignesX do
              ZMatrix[0,i]:=FXMini + (i-1)*FPasX;
            for j:=1 to FNbLignesY do
              ZMatrix[j,0]:=FYMini + (j-1)*FPasY;
            // lecture d'une donnée obsolète
            ReadLn(pFileMAI, TmpBuff);
            // chargement de la table
            FZMini:=1e+10;
            FZMaxi:=-1e+10;
            for i:=1 to FNbLignesY do
              for j:=1 to FNbLignesX do
                 begin
                   Readln(PFileMAI,TmpBuff);
                   ZMatrix[i,j]:=strtofloat(TmpBuff) * FMult * FMagnification;
                   if ZMatrix[i,j]>FZMaxi then FZMaxi:=ZMatrix[i,j];
                   if ZMatrix[i,j]<FZMini then FZMini:=ZMatrix[i,j];
                  end;


        end;
      tmUNKNOWN: begin
           // initialiser tableaux inutilisés
           SetLength(Triangles,1);
           SetLength(Vertexes,1);
           SetLength(ZMatrix,   1,1);
           SetLength(ZNormales, 1,1);
           Result:=-64;
        end;
      end;
      CloseFile(pFileMAI);
    except
    end;
  //*)
  finally
    CloseFile(pSUR);
  end;
end;

function TMaillage.SaveToFile(const Fichier: string): integer;
var
  i,j: integer;
  pMAI: TextFile;
begin;
  if FileExists(Fichier) then begin
    if MessageDlg(Format('Ecraser %s existant',[Fichier]),
                  mtConfirmation, [mbYes, mbNo],0)=mrNo then
      Exit;
  end;

  try
    try
      AssignFile(pMAI, Fichier);
      ReWrite(pMAI);
      // Ecriture en tête
      WriteLn(pMAI, Format('%s',[FCaption]));
      // Type de maillage
      WriteLn(pMAI, 'REGULAR_GRID');
      // Couleur de base +couleurs de dégradé
      WriteLn(pMAI, Format('$%X',[FColorMax]));
      WriteLn(pMAI, Format('$%X',[FColorMin]));
      // Nombre de lignes et de colonnes
      WriteLn(pMAI, Format('%d',[FNbLignesX]));
      WriteLn(pMAI, Format('%d',[FNbLignesY]));
      // Coordonnées mini
      WriteLn(pMAI, Format('%.2f',[FXMini / FMult]));
      WriteLn(pMAI, Format('%.2f',[FYMini / FMult]));
      // Coordonnées mini
      WriteLn(pMAI, Format('%.2f',[FXMaxi / FMult]));
      WriteLn(pMAI, Format('%.2f',[FYMaxi / FMult]));
      // écriture ligne vierge
      WriteLn(pMAI, '0.00');
      // ecriture des données
      for i:=1 to FNbLignesY do
        for j:=1 to FNbLignesX do begin
           WriteLn(pMAI, Format('%.2f ',[ZMatrix[i,j] /
                                  (FMult * FMagnification)]));
        end;
    except
    end;
  finally
    CloseFile(pMAI);
  end;
end;
end.

