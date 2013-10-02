// Mode DELPHI
{$mode DELPHI}{$H+}
{$PACKRECORDS 4}    // alignement sur 32 bits
// Débogage ?
{$DEFINE DEBUG}
// Mode TOPOROBOT étendu
{$DEFINE EXTENDED_TAB_FORMAT_FEATURES}
// Fonctionnalités de traceur CN: désactivées
{$DEFINE PLOTTER_CAPABILITIES}
{$UNDEF PLOTTER_CAPABILITIES}

// système d'exploitation
{$DEFINE MSWINDOWS}
//{$UNDEF MSWINDOWS}
{$DEFINE LINUX}
{$UNDEF LINUX}

// Le fichier GRIDS a été modifié ?
{$DEFINE GRIDS_SRC_MODIFIED}
{$UNDEF GRIDS_SRC_MODIFIED}


// Spécificités PocketGHTopo
{$DEFINE POCKET_GHTOPO}
{$UNDEF POCKET_GHTOPO}

// Choix du frontal de la BDD
{$DEFINE USE_FRONTAL_BDD_COMPACT} // frontal avec panneau 'magnétoscope'
{$UNDEF  USE_FRONTAL_BDD_COMPACT}

{$DEFINE USE_FRONTAL_BDD_WIDESCREEN} // frontal avec liste latérale permanente
//{$UNDEF  USE_FRONTAL_BDD_WIDESCREEN}

// Choix des utilitaires de conversion de coordonnees
// EPSG: Très complet mais nécessite une DLL et est bogué pour le LT93
{$DEFINE USE_CONVERTISSEUR_EPSG}
{$UNDEF USE_CONVERTISSEUR_EPSG}
// E. SIBERT: Opérationnel, autonome, avec sources, mais très mal codé et incompréhensible
{$DEFINE USE_CONVERTISSEUR_ESIBERT}
{$UNDEF USE_CONVERTISSEUR_ESIBERT}
// JP CASSOU: Opérationnel, autonome, avec sources, variante du convertisseur de Sibert
{$DEFINE USE_CONVERTISSEUR_JPC}
//{$UNDEF USE_CONVERTISSEUR_JPC}
//******************************************************************************
// Langues

{$DEFINE FRENCH_MESSAGES}
//{$UNDEF FRENCH_MESSAGES}

{$DEFINE ENGLISH_MESSAGES}
{$UNDEF ENGLISH_MESSAGES}

{$DEFINE SPANISH_MESSAGES}
{$UNDEF SPANISH_MESSAGES}
