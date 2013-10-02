program convertisseur;

uses
  Forms,
  main in 'main.pas' {Form1},
  ellipsoide in 'ellipsoide.pas',
  datum in 'datum.pas',
  projection in 'projection.pas',
  lambert in 'lambert.pas',
  donnee in 'donnee.pas',
  outils in 'outils.pas',
  createur in 'createur.pas',
  ign in 'ign.pas',
  lambert_tan in 'lambert_tan.pas',
  lambert_sec in 'lambert_sec.pas',
  transmercator_brut in 'transmercator_brut.pas',
  transmercator in 'transmercator.pas',
  utm in 'utm.pas',
  multiple in 'multiple.pas',
  ancetre in 'ancetre.pas',
  rosenmund in 'rosenmund.pas',
  swisstopo in 'swisstopo.pas',
  laborde in 'laborde.pas',
  laborde_reel in 'laborde_reel.pas',
  laborde_courbure in 'laborde_courbure.pas',
  laborde_equatoriale in 'laborde_equatoriale.pas',
  laborde_bitan in 'laborde_bitan.pas',
  mtm in 'mtm.pas',
  stereographic in 'stereographic.pas',
  stereo_reel in 'stereo_reel.pas',
  stereo_courbure in 'stereo_courbure.pas',
  stereo_bitan in 'stereo_bitan.pas',
  stereo_equatoriale in 'stereo_equatoriale.pas',
  stereo_polaire in 'stereo_polaire.pas',
  newzealand in 'newzealand.pas',
  bonne in 'bonne.pas',
  listing in 'listing.pas' {DlgParametre},
  dial_ellipse in 'dial_ellipse.pas' {DlgEllipsoide},
  dial_datum in 'dial_datum.pas' {DlgDatum},
  dial_projection in 'dial_projection.pas' {DlgProjection},
  dial_import in 'dial_import.pas' {DlgImportation},
  fichier_ini in 'fichier_ini.pas',
  hotine in 'hotine.pas',
  ign_laborde in 'ign_laborde.pas',
  mercator_multi in 'mercator_multi.pas',
  laborde_oblic in 'laborde_oblic.pas',
  grille_ign in 'grille_ign.pas',
  cylindre_oblic in 'cylindre_oblic.pas',
  hotine_centre in 'hotine_centre.pas',
  lambert_oblic in 'lambert_oblic.pas',
  mercator_sphere in 'mercator_sphere.pas',
  complexes in 'complexes.pas',
  projlatlong in 'projlatlong.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Convertisseur';
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TDlgParametre, DlgParametre);
  Application.CreateForm(TDlgEllipsoide, DlgEllipsoide);
  Application.CreateForm(TDlgDatum, DlgDatum);
  Application.CreateForm(TDlgProjection, DlgProjection);
  Application.CreateForm(TDlgImportation, DlgImportation);
  Application.Run;
end.