library LibConv;

{ Remarque importante concernant la gestion de mémoire de DLL : ShareMem doit
  être la première unité de la clause USES de votre bibliothèque ET de votre projet 
  (sélectionnez Projet-Voir source) si votre DLL exporte des procédures ou des
  fonctions qui passent des chaînes en tant que paramètres ou résultats de fonction.
  Cela s'applique à toutes les chaînes passées de et vers votre DLL --même celles
  qui sont imbriquées dans des enregistrements et classes. ShareMem est l'unité 
  d'interface pour le gestionnaire de mémoire partagée BORLNDMM.DLL, qui doit
  être déployé avec vos DLL. Pour éviter d'utiliser BORLNDMM.DLL, passez les 
  informations de chaînes avec des paramètres PChar ou ShortString. }

// Remarque de l'auteur :
// pour éviter les problèmes ci-dessus, toutes les chaînes sont échangées sous
// la forme de PChar

uses
  SysUtils,
  Classes,
  createur, donnee, ellipsoide, datum, ancetre, projection, multiple, outils;

{$R *.res}

function Nb_Projection : integer; stdcall;
begin
  result:=high(RelationProj)-low(RelationProj)+1;
end;

function Nom_Projection(Numero : integer; Resultat : PChar; SizeResultat : integer) : integer; stdcall;
begin
  if Numero in [low(RelationProj)..high(RelationProj)] then
  try
    StrPLCopy(Resultat, RelationProj[Numero].Texte, SizeResultat);
    result:=0;
  except
    result:=2;
  end
  else
    Result:=1;
end;

function Nb_Multiple : integer; stdcall;
begin
  result:=high(RelationMulti)-low(RelationMulti)+1;
end;

function Nom_Multiple(Numero : integer; Resultat : PChar; SizeResultat : integer) : integer; stdcall;
begin
  if Numero in [low(RelationMulti)..high(RelationMulti)] then
  try
    StrPLCopy(Resultat, RelationMulti[Numero].Texte, SizeResultat);
    result:=0;
  except
    result:=2;
  end
  else
    Result:=1;
end;

function Nb_Param_Proj(Numero : integer) : integer; stdcall;
begin
  if Numero in [low(RelationProj)..high(RelationProj)] then
  try
    result:=Nb_Param_Projection(RelationProj[Numero].Texte);
  except
    result:=-1;
  end
  else
    Result:=-1;
end;

function Nom_Param_Proj(Num_Proj, Num_Param : integer; Resultat : PChar; SizeResultat : integer) : integer; stdcall;
begin
  if Num_Proj in [low(RelationProj)..high(RelationProj)] then
  try
    StrPLCopy(Resultat, Nom_Param_Projection(RelationProj[Num_Proj].Texte, Num_Param),
              SizeResultat);
    result:=0;
  except
    result:=2;
  end
  else
    Result:=1;
end;

function Nb_Param_Multi(Numero : integer) : integer; stdcall;
begin
  if Numero in [low(RelationMulti)..high(RelationMulti)] then
  try
    result:=Nb_Param_Projection(RelationMulti[Numero].Texte);
  except
    result:=-1;
  end
  else
    Result:=-1;
end;

function Nom_Param_Multi(Num_Multi, Num_Param : integer; Resultat : PChar; SizeResultat : integer) : integer; stdcall;
begin
  if Num_Multi in [low(RelationMulti)..high(RelationMulti)] then
  try
    StrPLCopy(Resultat, Nom_Param_Projection(RelationMulti[Num_Multi].Texte, Num_Param),
              SizeResultat);
    result:=0;
  except
    result:=2;
  end
  else
    Result:=1;
end;

function LitFichier(Nom_Fichier : PChar; SizeNom : integer) : integer; stdcall;
var
  Flux : TFileStream;
  FileName : string;
begin
  result:=0;
  FileName:=StrPas(Nom_Fichier);
  if not FileExists(FileName) then
    result:=1
  else
    try
      Flux:=TFileStream.Create(FileName,fmOpenRead);
      Donnees.LoadFromStream(Flux);
      Flux.Free;
    except
      result:=2;
    end;
end;

function EcritFichier(Nom_Fichier : PChar; SizeNom : integer) : integer; stdcall;
var
  Flux : TFileStream;
  FileName : string;
begin
  result:=0;
  FileName:=StrPas(Nom_Fichier);
  try
    Flux:=TFileStream.Create(FileName,fmCreate);
    Donnees.SaveToStream(Flux);
    Flux.Free;
  except
    result:=1;
  end;
end;

function AjouterUnCas(Texte_Cas : PChar; SizeCas : integer) : integer; stdcall;
begin
  result:=0;
  try
    Donnees.AjoutLigne(StrPas(Texte_Cas));
  except
    result:=1;
  end;
end;

function Nb_Ellipsoide : integer; stdcall;
begin
  result:=Donnees.LesEllipsoide.Count;
end;

function Nom_Ellipsoide(Numero : integer; Resultat : PChar; SizeResultat : integer) : integer; stdcall;
begin
  if Numero in [0..Donnees.LesEllipsoide.Count-1] then
  try
    StrPLCopy(Resultat, TEllipsoide(Donnees.LesEllipsoide[Numero]).Nom, SizeResultat);
    result:=0;
  except
    result:=2;
  end
  else
    Result:=1;
end;

function Nb_Datum : integer; stdcall;
begin
  result:=Donnees.LesDatum.Count;
end;

function Nom_Datum(Numero : integer; Resultat : PChar; SizeResultat : integer) : integer; stdcall;
begin
  if Numero in [0..Donnees.LesDatum.Count-1] then
  try
    StrPLCopy(Resultat, TDatum(Donnees.LesDatum[Numero]).Nom, SizeResultat);
    result:=0;
  except
    result:=2;
  end
  else
    Result:=1;
end;

function Nb_Projection_Active : integer; stdcall;
begin
  result:=Donnees.LesProjection.Count;
end;

function Nom_Projection_Active(Numero : integer; Resultat : PChar; SizeResultat : integer) : integer; stdcall;
begin
  if Numero in [0..Donnees.LesProjection.Count-1] then
  try
    StrPLCopy(Resultat, TProjection(Donnees.LesProjection[Numero]).Nom, SizeResultat);
    result:=0;
  except
    result:=2;
  end
  else
    Result:=1;
end;

function Nb_Multiple_Active : integer; stdcall;
begin
  result:=Donnees.LesMultiple.Count;
end;

function Nom_Multiple_Active(Numero : integer; Resultat : PChar; SizeResultat : integer) : integer; stdcall;
begin
  if Numero in [0..Donnees.LesMultiple.Count-1] then
  try
    StrPLCopy(Resultat, TMultiple(Donnees.LesMultiple[Numero]).Nom, SizeResultat);
    result:=0;
  except
    result:=2;
  end
  else
    Result:=1;
end;

function datum2proj(PDepart : PChar; SizeDepart : integer; LatD, LongD : real;
                    PArrivee : PChar; SizeArrivee : integer; var XA, YA : real; var fuseauA : integer) : integer; stdcall;
var
  Depart : TDatum;
  Arrivee : TAncetre;
  Xcart,Ycart, Zcart : real;
begin
  try
    Depart:=TDatum(Donnees.UnDatum(StrPas(PDepart)));
    if Depart=nil then
      result:=1
    else begin
      Depart.VersCartesien(LatD, LongD, false, Xcart,Ycart, Zcart);

      Arrivee:=TAncetre(Donnees.UneProjection(StrPas(PArrivee)));
      if Arrivee=nil then
        result:=2
      else begin
        result:=0;
        Arrivee.Datum.VersLatLong(Xcart, Ycart, Zcart, false, LatD, LongD);
        if Arrivee is TProjection then
        begin
          (Arrivee as TProjection).VersGrille(LatD, LongD, XA, YA);
          fuseauA:=0;
        end
        else
          (Arrivee as TMultiple).VersGrille(LatD, LongD, XA, YA, fuseauA);
      end;
    end;
  except
    result:=3;
  end;
end;

function datum2datum(PDepart : PChar; SizeDepart : integer; LatD, LongD : real;
                     PArrivee : PChar; SizeArrivee : integer; var LatA, LongA : real) : integer; stdcall;
var
  Depart : TDatum;
  Arrivee : TDatum;
  Xcart,Ycart, Zcart : real;
begin
  try
    Depart:=TDatum(Donnees.UnDatum(StrPas(PDepart)));
    if Depart=nil then
      result:=1
    else begin
      Depart.VersCartesien(LatD, LongD, false, Xcart,Ycart, Zcart);

      Arrivee:=TDatum(Donnees.UnDatum(StrPas(PArrivee)));
      if Arrivee=nil then
        result:=2
      else
      begin
        result:=0;
        Arrivee.VersLatLong(Xcart, Ycart, Zcart, false, LatA, LongA);
      end;
    end;
  except
    result:=3;
  end;
end;

function proj2datum(PDepart : PChar; SizeDepart : integer; XD, YD : real; fuseauD : integer;
                    PArrivee : PChar; SizeArrivee : integer; var LatA, LongA : real) : integer; stdcall;
var
  Depart : TAncetre;
  Arrivee : TDatum;
  Xcart,Ycart, Zcart : real;
begin
  try
    Depart:=TAncetre(Donnees.UneProjection(StrPas(PDepart)));
    if Depart=nil then
      result:=1
    else begin
      if Depart is TProjection then
        (Depart as TProjection).VersLatLong(XD,YD,LatA, LongA)
      else
        (Depart as TMultiple).VersLatLong(XD, YD, fuseauD,LatA, LongA);
      Depart.Datum.VersCartesien(LatA,LongA,false, Xcart, Ycart, Zcart);

      Arrivee:=TDatum(Donnees.UnDatum(StrPas(PArrivee)));
      if Arrivee=nil then
        result:=2
      else
      begin
        result:=0;
        Arrivee.VersLatLong(Xcart, Ycart, Zcart, false, LatA, LongA);
      end;
    end;
  except
    result:=3;
  end;
end;

function proj2proj(PDepart : PChar; SizeDepart : integer; XD, YD : real; fuseauD : integer;
                    PArrivee : PChar; SizeArrivee : integer; var XA, YA : real; var fuseauA : integer) : integer; stdcall;
var
  Depart : TAncetre;
  Arrivee : TAncetre;
  Xcart,Ycart, Zcart : real;
  Lat, Long : real;
begin
  try
    Depart:=TAncetre(Donnees.UneProjection(StrPas(PDepart)));
    if Depart=nil then
      result:=1
    else begin
      if Depart is TProjection then
        (Depart as TProjection).VersLatLong(XD,YD,Lat, Long)
      else
        (Depart as TMultiple).VersLatLong(XD, YD, fuseauD, Lat, Long);
      Depart.Datum.VersCartesien(Lat,Long,false, Xcart, Ycart, Zcart);

      Arrivee:=TAncetre(Donnees.UneProjection(StrPas(PArrivee)));
      if Arrivee=nil then
        result:=2
      else begin
        result:=0;
        Arrivee.Datum.VersLatLong(Xcart, Ycart, Zcart, false, Lat, Long);
        if Arrivee is TProjection then
        begin
          (Arrivee as TProjection).VersGrille(Lat, Long, XA, YA);
          fuseauA:=0;
        end
        else
          (Arrivee as TMultiple).VersGrille(Lat, Long, XA, YA, fuseauA);
      end;
    end;
  except
    result:=3;
  end;
end;

function Conversion(NomSource : PChar; SizeNomSource : integer;
                    NomDest : PChar; SizeNomDest : integer;
                    XSource : PChar; SizeXSource : integer;
                    YSource : PChar; SizeYSource : integer;
                    IndiceSource : integer; var IndiceDest : integer;
                    XDest : PChar; SizeXDest : integer;
                    YDest : PChar; SizeYDest : integer;
                    UniteAngle : integer) : boolean; stdcall;
var
  ResultatX, ResultatY : string;
  UAngle : TUniteAngle;
begin
  try
    case UniteAngle of
      0 : UAngle:=uaDegre;
      1 : UAngle:=uaDMS;
      2 : UAngle:=uaGrade;
      3 : UAngle:=uaDM;
    else
      UAngle:=uaDegre;
    end;
    result:=Donnees.Conversion(NomSource,
                        NomDest,
                        XSource,
                        YSource,
                       IndiceSource,
                       IndiceDest,
                       ResultatX,
                       ResultatY,
                       UAngle );
    StrPLCopy(XDest, ResultatX, SizeXDest);
    StrPLCopy(YDest, ResultatY, SizeYDest);
  except
    result:=false;
  end;
end;

function Convergence(NomSource : PChar; SizeNomSource : integer;
                     NomDest : PChar; SizeNomDest : integer;
                     XSource : PChar; SizeXSource : integer;
                     YSource : PChar; SizeYSource : integer;
                     IndiceSource : integer;
                     ResultatConv : PChar; SizeResultatConv: integer;
                     UniteAngle : integer) : boolean; stdcall;
var
  Langle : string;
  UAngle : TUniteAngle;
begin
  try
    case UniteAngle of
      0 : UAngle:=uaDegre;
      1 : UAngle:=uaDMS;
      2 : UAngle:=uaGrade;
      3 : UAngle:=uaDM;
    else
      UAngle:=uaDegre;
    end;
    result:=Donnees.Convergence(NomSource,
                        NomDest,
                        XSource,
                        YSource,
                       IndiceSource,
                       Langle,
                       UAngle );
    StrPLCopy(ResultatConv, Langle, SizeResultatConv);
  except
    result:=false;
  end;
end;

function Alteration(NomSource : PChar; SizeNomSource : integer;
                    NomDest : PChar; SizeNomDest : integer;
                    XSource : PChar; SizeXSource : integer;
                    YSource : PChar; SizeYSource : integer;
                    IndiceSource : integer;
                    var ResultatAlteration : real) : boolean; stdcall;
begin
  try
    result:=Donnees.Alteration(NomSource,
                        NomDest,
                        XSource,
                        YSource,
                        IndiceSource,
                        ResultatAlteration);
  except
    result:=false;
  end;
end;

function Projection_GetA(NomSource : PChar; SizeNomSource : integer;
                         var Grand_Axe : real) : boolean; stdcall;
begin
  try
    Grand_Axe:=TProjection(Donnees.UneProjection(StrPas(NomSOurce))).Datum.Ellipsoide.A;
    result:=true;
  except
    result:=false;
  end;
end;

function Projection_GetInvF(NomSource : PChar; SizeNomSource : integer;
                            var Inv_Applatissement : real) : boolean; stdcall;
begin
  try
    Inv_Applatissement:=TProjection(Donnees.UneProjection(StrPas(NomSOurce))).Datum.Ellipsoide.InvF;
    result:=true;
  except
    result:=false;
  end;
end;

function Projection_GetDatum(NomSource : PChar; SizeNomSource : integer; Resultat : PChar; SizeResultat : integer) : boolean; stdcall;
begin
  try
    StrPLCopy(Resultat,TProjection(Donnees.UneProjection(StrPas(NomSOurce))).Datum.Nom,SizeResultat);
    result:=true;
  except
    result:=false;
  end;
end;


exports
  Nb_Projection,
  Nom_Projection,
  Nb_Multiple,
  Nom_Multiple,
  Nb_Multiple,
  Nom_Multiple,
  Nb_Param_Proj,
  Nom_Param_Proj,
  Nb_Param_Multi,
  Nom_Param_Multi,
  LitFichier,
  EcritFichier,
  AjouterUnCas,
  Nb_Ellipsoide,
  Nom_Ellipsoide,
  Nb_Datum,
  Nom_Datum,
  Nb_Projection_Active,
  Nom_Projection_Active,
  Nb_Multiple_Active,
  Nom_Multiple_Active,
  datum2proj,
  datum2datum,
  proj2datum,
  proj2proj,
  conversion,
  convergence,
  alteration,
  Projection_GetA,
  Projection_GetInvF,
  Projection_GetDatum
  ;

begin
  Donnees:=TDonnees.Create;


end.
