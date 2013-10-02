unit UnitMessages_fr;
{Compatibilité OK avec la version Linux}

interface

const
  NATIONAL_GEODESIC_SYSTEM_IDX = 1; // pour la version française: Lambert93
  DEFAULT_CODE_EPSG            = 'LT93';

resourcestring
  
  // nom du logiciel
  rsGHTOPOEXENAME      = 'GHTopo';
  rsMAINMENUCAPTION    = '%s - GHTopo';
  rsGHTOPOVERSION      = 'Version 3.1415926 du %s';
  rsGHTOPOLICENSE      = 'Logiciel sous licence GPL';
  rsGHTOPOAUTHOR       = '(c) 1989..2012 Jean Pierre CASSOU';

  // infos sur la session
  rsGHTOPORESOL        = 'Résolution %d x %d';
  rsGHTOPOPlatFormLinux= 'Plateforme: Linux (Français)';
  rsGHTOPOPlatFormWin32= 'Plateforme: Microsoft Windows (Français)';

  // langue choisie
  rsCHOOSELANGUAGE     = 'Démarrage en langue française';
  // fin GHTopo
  rsENDOFHADES         = 'Fin GHTopo';

  rsSERIE_INITIALISATION = 'Série 0 non modifiable';

  // lecture des fichiers
  rsSEPARATOR_LINE     = '-------------------------------';
  rsWARNINGS_READFILE  = 'Avertissements de lecture de: ';
  rsCONV_TAB_MAC_UNIX  = '-> Conversion des fichiers TAB pouvant venir du Mac ou d''Unix';

  // messages dans TToporobotStructure.LoadFichierTab();
  rsRD_TAB_MSG_ERR     = 'Erreur lors du traitement de la ligne';
  rsRD_TAB_D_MULTI_OBS = 'Début de commentaires multilignes en ligne %d';
  rsRD_TAB_F_MULTI_OBS = 'Fin de commentaires multilignes en ligne %d';
  rsRD_TAB_STOP_9999   = '*** Line %d: Lecture arrêtée par le code -9999 ***';
  rsRD_TAB_LN_OBS      = '-- La ligne %d est un commentaire';
  rsRD_TAB_LASTSAVES   = 'Dernière sauvegarde: %s %s';
  rsRD_TAB_CLASSEURS   = '-7 [Classeurs] Cette fonctionnalité est ignorée';
  rsRD_TAB_ENTRANCE    = 'Entrée créée #%d';
  rsRD_TAB_ENTR_NOGEOREF = '[Avertissement] (Ligne %d): Entrée %d (%s) non géoréférencée';
  rsRD_TAB_ENTR_BADLINK  = '[Avertissement] (%d): Entrée %d (%s): Raccordement incorrect [Série: %d - Station: %d]';
  rsRD_TAB_IGNORED_SEC   = ' [Information] Section ignorée';
  rsRD_TAB_BAD_TRIP      = 'Séance incorrecte';
  rsRD_TAB_BAD_CODE      = 'Code incorrect';
  rsRD_TAB_BAD_DATE      = '[Avertissement] (%d) - Date incorrecte mise à la date actuelle';
  rsRD_TAB_BAD_IDXRES    = '[Avertissement] (%d) - Index de réseau incorrect (%d) pour la série %d - Mis à 0';
  rsRD_TAB_IDEXTR_MM     = '[Avertissement] (%d) - Les ID d''extrémité de la série %d sont identiques';
  rsRD_TAB_SELF_CLOSURE  = '[Avertissement] (%d) - Le terminus de la série %d se branche sur elle-même.';
  rsRD_TAB_NEG_LONG      = '[Avertissement] (%d) - Longueur négative (%.2f m), changée de signe';
  rsRD_ERROR_LN          = '*** Erreur en ligne #%d: %s';
  rsRD_CONTNT_LN         = '    Contenu de la ligne: %s';
  rsRD_TAB_FIELD_VALUES  = ' Valeurs des champs:';
  rsRD_TAB_NOFATAL_ERR   = 'Le fichier ne comporte pas d''erreurs fatales';
  rsRD_TAB_WTFATAL_ERR   = 'Le fichier comporte %d erreurs';

  //libellés communs
  rsDEFAULT            = 'Défaut';
  rsFORFIXEDPOINTS     = 'Item pour entrees et points fixes';
  rsWARNINGENTRYADDED  = 'Le nombre d''entrées dans -6 est différent de celui de -5; Corrigé.';
  rsCALCULNODES        = 'CALCUL DES COORDONNEES DES NOEUDS';
  rsLINEOFNB           = '---> Ligne %d / %d';
  rsBINDMATRIX         = 'Matrice d''assemblage';
  rsNB_NON_NUL_TERMES  = ' %d termes non nuls dans la matrice R';
  rsPR100_NON_NULS     = ' soit %.3f%% de (%d x %d) = %d termes';
  rsFINDING_BRANCHES   = 'Recensement des branches';
  rsADDENTRANCES       = 'Ajout des entrees';
  rsBUILDMATRICE       = 'CONSTRUCTION DE LA MATRICE DE COMPENSATION';
  rsFIND_SUM_LIMITS    = '-- Recherche des limites utiles de sommation';
  rsFACTORISEMATRICE   = 'FACTORISATION DE LA MATRICE DE COMPENSATION';
  rsAXIS               = 'Axe: ';
  rsDESCENTE           = '--> Descente: V.V* = A';
  rsREMONTEE           = '--> Remontée du système: inconnues recherchees';

  rsTRIANGULARISATION  = '--> Triangularisation';
  rs2NDMEMBER          = ' - Second membre';
  rsCOMPESMATRIX       = ' - Matrice de compensation';
  rsNODECOORD          = ' - Factorisation: Coordonnées des noeuds';
  rsCOORDONNEES_OK     = '--> Coordonnées des %d noeuds OK';

  rsDEL_TEMP_MATRIX    = '--> Destruction des matrices temporaires';

  rsWRITE_NODES_COORDS = 'Ecriture des coordonnées des noeuds dans: ';
  rsNODES_COORDS_FOR_DB= 'Coordonnées des noeuds pour: ';
  rsCALCULCONTOURS     = 'CALCUL CONTOURS GALERIES';
  rsCALCUL_ANTENNES    = 'Calcul des visees en antenne';

  // étapes du calcul
  rsRECENSEM_JONC      = '-- Recensement des jonctions';
  rsPURGE_TABLE_JONC   = '-- Purge table des jonctions';
  rsPURGE_TABLE_BRCH   = '-- Purge table des branches';
  rsNB_BRCHS           = '%d branches';

  rsSTEP_CALC_01       = '->Etape %d / %d: Matrice d''assemblage R';
  rsSTEP_CALC_02       = '->Etape %d / %d: Matrice de pondération W';
  rsSTEP_CALC_03       = '->Etape %d / %d: Matrice de compensation B = Rt.W*.R';
  rsSTEP_CALC_04       = '->Etape %d / %d: Coordonnées des noeuds';
  rsSTEP_CALC_05       = '->Etape %d / %d: Répartition des écarts';
  rsSTEP_CALC_06       = '->Etape %d / %d: Libérations mémoire et fin de traitement';
  rsFREE_TEMP_VARS     = 'LIBERATION DES VARIABLES TEMPORAIRES';

  rsREPARTIR_ECARTS    = 'REPARTITION DES ECARTS';
  rsSCAN_BRCHS         = '-- Balayage des branches';



  //********************************
  // Filtres de fichiers
  //rsTOPOROBOT_LOAD_FILE_FILTER = 'Documents Toporobot / GHTopo|*.tab;*.xtb';
  //rsTOPOROBOT_SAVE_FILE_FILTER = 'Documents GHTopo (*.xtb)|*.xtb|'+
  //                              'Documents TOPOROBOT|;
  rsFILEFILTER_ALL        = 'Tous (*.*)|*.*';
  rsGHTOPO_FILE_FILTER    = 'Fichier GHTopo (*.xtb)|*.xtb|'+
                            'Fichier Toporobot Tab (*.Tab)|*.Tab|'+
                            'Fichier GHTopo XML (*.gtx)|*.gtx|' +
                            'Tous (*.*)|*.*';
  rsKML_FILE_FILTER       = 'Fichier Google Earth (*.kml)|*.xtb|' +
                            'Tous (*.*)|*.*';
  rsGCD_FILE_FILTER       = 'Fichier GHCaveDraw (*.gcd)|*.gcd|' +
                            'Tous (*.*)|*.*';
  rsGPX_FILE_FILTER       = 'Fichier GPS (*.gpx)|*.gpx|' +
                            'Tous (*.*)|*.*';
  rsOSM_FILE_FILTER       = 'Fichier travail OpenStreetMap (*.osm)|*.osm|' +
                            'Tous (*.*)|*.*';
  rsCSV_FILE_FILTER       = 'Fichier texte tabulé (*.csv)|*.csv|' +
                            'Tous (*.*)|*.*';
  rsVTOPO_FILE_FILTER     = 'Fichier Visual Topo (*.tro)|*.tro|' +
                            'Tous (*.*)|*.*';
  rsTHERION_FILE_FILTER   = 'Fichier Therion (*.th)|*.th|' +
                            'Tous (*.*)|*.*';
  //*********************************************
  // resourcestring du menu principal
  rsMSGASSIGNCAPTIONS  = 'Mise en place de l''interface';
  rsCALCURAPIDE        = 'Calculette: ';
  rsMNU_FILE           = '&Fichier';
    rsNEW              = '&Nouveau';
    rsOPEN             = '&Ouvrir';
    rsSAVE             = '&Sauvegarder';
    rsSAVEAS           = '&Enregistrer sous';
    
    rsMNU_SAVELOG      = '&Enregistrer historique';
    rsCLOSE            = '&Fermer';
    rsRECENT_DOCS      = '&Documents récents';
    rsRELOAD           = 'Recharger le document';
    rsEDITTAB          = 'Editer fichier &Tab';
    rsPRINT            = 'Im&primer';
    rsVTOPO            = 'Export Visual Topo';
    rsTHERION          = 'Export vers Therion';
    rsEXPORT_GIS       = 'Export vers logiciels de cartographie';

    rsEXPORTGRAPHIQUE  = 'E&xport graphique (PS et SVG)';
    rsERRINFOXTB       = '&Rapport d''erreur de lecture';
    rsGHTOPO_QUIT      = '&Quitter GHTopo';
    rsQUIT_EDITOR     = '&Quitter l''éditeur de texte';
  rsMNU_EDITION        = '&Edition';
  rsMNU_TOPOGRAPHIE    = '&Topographie';
    rsCHECKBASE        = '&Vérifier les données';
    rsCOMPILE          = '&Calculer le réseau';
    rsVUEPLAN          = 'Vue en &Plan';
    rsVUE3D            = '&Vue 3D';
    rsRENDU3D          = 'Rendu &3D';
    rsSTATISTIQUES     = '&Statistiques';
    rsINFOCAVITE       = '&Info cavité';
    rsNODESCOORDINATES = 'Coordonnées des &Noeuds';
  rsMNU_WINDOW         = '&Fenêtre';
    rsMNU_STAY_ON_TOP  = 'Stay on &Top';
    rsWND_DATABASE     = 'Base de données';
    rsWND_PLAN         = '&Vue en plan';
  rsMNU_TOOLS          = '&Outils';
  rsMNU_HELP           = '&Aide';
    rsHLPINDEX         = '&Index';
    rsHLPNEWS          = 'Nouveautés';
    rsABOUT            = '&A propos de GHTopo';

   rsASSISTANT_SUCCESS = 'Document créé avec succès par l''assistant';
   rsASSISTANT_RELOAD  = 'Rechargement du document';
   rsASSISTANT_ECHEC   = 'Echec de l''assistant';


  //**************************************
  // resourcestring des titres de boites de dialogue
  //**************************************
  // resourcestring des libellés communs
  rsLBL_COMMENTS          = 'Commentaires';

  // resourcestring du visualisateur 2D
  rsVUE2D_FMT_INFOS_STATION_RAPIDE = 'Station: %s (%d.%d): X=%.2f Y=%.2f Z=%.2f';
  rsVUE2D_FMT_INFOS_ID_STATION     = '%d.%d [%s]';
  rsVUE2D_FMT_INFOS_MESURES        = 'L = %.2f, Az = %.2f, P =%.2f, G = %.2f, D = %.2f, H = %.2f, B = %.2f';
  rsVUE2D_FMT_INFOS_COORDONNEES    = 'X = %s, Y = %s, Z = %s';

  // resourcestring de l'Assistant
  rsASSIST_TITLE       = 'Assistant Nouvelle Cavité';
  rsASSIST_BNGENERAL   = 'Général';
  rsASSIST_BNENTRANCES = '1ère Entrée';
  rsASSIST_BNCODES     = '1er Code';
  rsASSIST_BNEXPES     = '1ère Séance';
  rsASSIST_BNSERIE     = '1ère Série';
  rsASSIST_BNSAVE      = 'Sauvegarder ...';
  rsASSIST_BNCANCEL    = 'Annuler';
  rsASSIST_LBNOMETUDE  = 'Nom de l''étude';
  rsASSIST_LBOBSETUDE  = 'Commentaires étude';
  rsASSIST_LBNOMENTREE = 'Entrée principale';

  rsASSIST_LBCOORDS    = 'Coordonnées';
  rsASSIST_LBOBSENTREE = 'Observations';
  rsASSIST_LBCONVERTISSEUR = 'Convertisseur ...';
  rsASSIST_LBREFSTATION    = 'Station initiale';
  rsASSIST_LBREFSERIE      = 'Série';
  rsASSIST_LBREFPOINT      = 'Point';
  rsASSIST_LBCOMMENTAIRE   = 'Commentaires';


  rsASSIST_LBSYSTGEO   = 'Systèmes de coordonnées géographiques';



  // resourcestring du cadre Entrées
  rsCDR_ENTR_NOENTRANCE   = 'Entrée';
  rsCDR_ENTR_ENTRNAME     = 'Nom de l''entrée';
  rsCDR_ENTR_COORDINATES  = 'Coordonnées';
  rsCDR_ENTR_STATOFENTR   = 'Station de référence:';

  // resourcestring du cadre Codes
  rsCDR_CODES_NUMERO      = 'Code:';
  rsCDR_CODES_TYPEGALERIE = 'Type de visée:';
  rsTITRE_SELECTEUR_VISEE = 'Choix du type de visée';
    rsCMBTYPE_D          = '0 - Défaut (conduit fossile)';
    rsCMBTYPE_E          = '1 - Entrée';
    rsCMBTYPE_B          = '2 - Galerie fossile';
    rsCMBTYPE_V          = '3 - Ecoulement libre';
    rsCMBTYPE_W          = '4 - Siphon';
    rsCMBTYPE_C          = '5 - Passage ennoyable';
    rsCMBTYPE_F          = '6 - Point fixe';
    rsCMBTYPE_S          = '7 - Topo de surface';
    rsCMBTYPE_A          = '8 - Tunnel artificiel';
    rsCMBTYPE_M          = '9 - Filon minier';
  rsCDR_CODES_VISEE       = 'Visée';
    rsCDR_CODES_VDIRECT   = 'Directe';
    rsCDR_CODES_VINVERSE  = 'Inverse';

  rsCDR_CODES_GRADCOMPAS  = 'Graduation du compas:';
  rsCDR_CODES_GRADCLINO   = 'Graduation du clinomètre:';
    rsCDR_CODES_CMBUNIT_0 = '400 - Grades';
    rsCDR_CODES_CMBUNIT_1 = '360 - Degrés';
    rsCDR_CODES_CMBUNIT_2 = '370 - Pourcentages';
    rsCDR_CODES_CMBUNIT_3 = '380 - Dénivellations';
  rsCDR_CODES_FACT        = 'Longueurs x';
  rsCDR_CODES_POSZERO     = 'Position du zéro:';
    rsCDR_CODES_CMBZERO_0 = 'Nadiral';
    rsCDR_CODES_CMBZERO_1 = 'Horizontal';
    rsCDR_CODES_CMBZERO_2 = 'Zénithal';
  rsCDR_CODES_ANGLIMIT    = 'Angle limite:';
  rsCDR_CODES_PRECISION   = 'Incertitude des mesures:';
  // resourcestring des onglets du Gestionnaire de Cavité
  rsTBS_GENERAL        = 'Général';
  rsTBS_ENTRANCE       = 'Entrées';
  rsTBS_CODES          = 'Codes';
  rsTBS_TRIPS          = 'Séances';
  rsTBS_SERIES         = 'Séries';
  rsTBS_RESEAUX        = 'Réseaux';
  rsTBS_ANTENNES       = 'Visées en antenne';
  rsTBS_MAINTENANCE    = 'Maintenance';



  rsLB_NOM_ETUDE          = 'Nom de l''étude';
  rsLB_COMMENTAIRE_ETUDE  = 'Commentaires';
  rsLB_CODE_EPSG          = 'Code EPSG du système de coordonnées';
  rsBTN_SELECT_EPSG       = 'Choisir ...';
  rsBTN_CALC_DECLIMAGS    = 'Calculer les déclinaisons magnétiques';

  //*******************************************************
  // resourcestrings du cadre Réseaux
  rsCDR_RESEAU_LBIDX   = 'Numero';
  rsCDR_RESEAU_NAME    = 'Nom';
  rsCDR_RESEAU_TYPE    = 'Type de réseau';
    rsCDR_RESEAU_CB0     = 'Cavité naturelle';
    rsCDR_RESEAU_CB1     = 'Cavité artificielle';
    rsCDR_RESEAU_CB2     = 'Topo de surface';
    rsCDR_RESEAU_CB3     = 'Thalweg';
    rsCDR_RESEAU_CB4     = 'Route ou piste';
    rsCDR_RESEAU_CB5     = 'Sentier';
    rsCDR_RESEAU_CB6     = 'Autre';





  
  //*******************************************************
  // resourcestrings du cadre Expés
  rsCDR_EXPE_SEANCE      = 'Séance topo:';
  rsCDR_EXPE_DATE        = 'Date';

  rsCDR_EXPE_DECLIMAG    = 'Déclinaison';
  rsCDR_EXPE_INCLIN      = 'Inclinaison';
  rsCOLOR                = 'Couleur';

  rsCDR_EXPE_SPELEOMETRE = 'Spéléomètre';
  rsCDR_EXPE_SPELEOGRAPHE= 'Spéléographe';
  //*********************************************
  // resourcestring du cadre Séries

  rsDLG_PJMNGR_MOVETOSERIE = 'Changer de série ?';
  rsDLG_PJMNGR_ADDSERIE  = 'Ajouter Série';
  rsCDR_SERIE_LB_RESEAU  = 'Réseau';
  rsCDR_SERIE_VALIDATE   = 'Valider';
  rsCDR_SERIE_ADDPHOTO   = 'Ajouter photo';
  rsCDR_SERIE_NUMERO     = 'Série:';
  rsCDR_SERIE_NAME       = 'Nom:';
  rsCDR_SERIE_DEPART     = 'Départ:';
  rsCDR_SERIE_ARRIVEE    = 'Arrivée:';
  rsCDR_SERIE_CHANCE     = 'Chance:'; // en espagnol: Suerte
    rsCDR_SERIE_CHANCE0  = 'Aucune';
    rsCDR_SERIE_CHANCE1  = 'Faible';
    rsCDR_SERIE_CHANCE2  = 'Bonne';
    rsCDR_SERIE_CHANCE3  = 'Excellente';
  rsCDR_SERIE_OBSTACLE   = 'Obstacle:';
    rsCDR_SERIE_OBSTACLE0= 'Aucun';
    rsCDR_SERIE_OBSTACLE1= 'Puits';
    rsCDR_SERIE_OBSTACLE2= 'Cheminée';
    rsCDR_SERIE_OBSTACLE3= 'Etroiture';
    rsCDR_SERIE_OBSTACLE4= 'Lac';
    rsCDR_SERIE_OBSTACLE5= 'Siphon';
    rsCDR_SERIE_OBSTACLE6= 'Effondrement';
    rsCDR_SERIE_OBSTACLE7= 'Concrétionnement';
    rsCDR_SERIE_OBSTACLE8= 'Sédiments';
    rsCDR_SERIE_OBSTACLE9= 'Autre';
    // spécifique à la France lol
    rsCDR_SERIE_OBSTACLE10 = 'Gaz toxiques';       // aurait dû être ajouté
    rsCDR_SERIE_OBSTACLE11 = 'Oies agressives';    // référence à une expé franco-chinoise
    rsCDR_SERIE_OBSTACLE12 = 'Animaux dangereux';  // référence à une expé de LH Fage
    rsCDR_SERIE_OBSTACLE13 = 'Baisodrome';         // pour la topographie de certains sentiers forestiers xD



  rsCDR_SERIE_LOCKED     = 'Verrouillé';
   rsCDR_SERIE_INSERTLINE = 'Insertion de lignes';
   rsCDR_SERIE_NBLINES    = 'Nombre de lignes';
   rsCDR_SERIE_DELETELINE    = 'Effacement de lignes';
   rsCDR_SERIE_UNDOCOPY      = 'Recopie vers le bas';
   rsCDR_SERIE_INC_UNDOCOPY  = 'Recopie vers le bas incrémentale';
  rsCDR_SERIE_IMPLEMENT      = 'Valider';
  rsINPUT_COMMENTAIRE_TITRE  = 'Commentaires station';
  rsINPUT_COMMENTAIRE_MSG    = 'Entrez un texte';
  rsCDR_SERIE_PASTE          = 'Importer le tableau depuis le presse-papiers';
  rsCDR_SERIE_BTN_GRD_COPY   = 'Copier la grille';

  rsCDR_SERIE_MSG_ERR_ENTREE_NOT_FOUND = 'Entrée introuvable';
  rsCDR_SERIE_MSG_ERR_RESEAU_NOT_FOUND = 'Réseau introuvable';
  rsCDR_SERIE_MSG_ERR_CODE_NOT_FOUND   = 'Code instruments introuvable';
  rsCDR_SERIE_MSG_ERR_EXPE_NOT_FOUND   = 'Session topo introuvable';
  rsCDR_SERIE_MSG_ERR_SERIE_NOT_FOUND  = 'Code instruments introuvable';



  rsCDR_SERIE_MSG_ERR_LONG   = 'La longueur doit être positive et inférieure à %.0f m';
  rsCDR_SERIE_MSG_ERR_LRUD   = 'La distance ne doit pas être négative';

  //*******************************************************
  // resoursestring du cadre Antennes
  rsCDR_ANTENNES_AC_ADDLINE = 'Ajouter une ligne';
  rsCDR_ANTENNES_AC_DELLINE = 'Supprimer une ligne';
  rsCDR_ANTENNES_AC_SAVEGRD = 'Valider les modifications';
  rsCDR_ANTENNES_DEL_LINE   = 'Supprimer la ligne %d';

  //*******************************************************
  // resourcestring du cadre CdrNavigateurDB
  rsCDR_NAVIG_DB_DO_SORT    = 'Trier';
  rsCDR_NAVIG_DB_DO_ADD     = 'Ajouter élément';
  rsCDR_NAVIG_DB_DO_DELETE  = 'Supprimer élément';



  // général
  rsSELECTALL          = 'Tout sélectionner';
  rsDESELECTALL        = 'Tout désélectionner';
  rsOBS                = 'Commentaires';
  rsFILTERS            = 'Filtres:';
  rsCHKSELECTALL       = 'Sél. tout';
  rsCHKDESELECTALL     = 'Désél. tout';
  rsCHKREVERSE         = 'Inv. sél.';
  rsMAIN_NETWORK       = 'Réseau principal';

  //*********************************
  // resourcestring du dialogue Vtopo
  rsVTOPO_EDPREFIX       = 'Préfixe des stations:';
  rsVTOPO_LBIDPTDEP      = 'ID point de départ:';
  rsVTOPO_LBREPORTER     = 'Opérateur du report:';
  rsLBFICHIER          = 'Fichier: ';
  rsLBMAINENTRANCE     = 'Entrée principale:';
  rsLBIDMAINST         = 'Station de départ:';
  rsLBSTPREFIX         = 'Préfixe des stations';
  rsLBREPORTER         = 'Report:';
  rsLBMAINENTCOORD     = 'Coordonnées de l''entrée principale';
  rsLBCOLORDEFAULT     = 'Couleur par défaut';

  //*******************************************************
  // resourcestring du visualisateur OpenGL
  rsOGLVIEWERTITLE     = 'Visualisateur OpenGL [%s]';
  rsLBLBACKCOLOR       = 'Arrière-plan';
  rsLBLCUBECOLOR       = 'Cube';
  rsLBLREFCOLOR        = 'Référentiel';
  rsLBLREFSIZE         = 'Taille';
  rsOPENGLERROR        = 'Erreur OpenGL';
  rsOGLVQUIT           = 'Quitter le visualisateur OpenGL';
  //*******************************************************
  // resourcestrings de l'outil d'exportation graphique
  rsPSDXF_TITLE        = 'Export graphique: [%s]';
  rsDODISPDESELECTEDPARTS = 'Afficher les parties refusées par le MétaFiltre - Couleur';
  rsTAB_LAYERS            = 'Couches';
  rsTAB_QUADR             = 'Quadrillage';
  rsTAB_DESSIN            = 'Dessin';
  rsTAB_TEXTES            = 'Textes';
  rsTYPEQDR          = 'Type de quadrillage';
  rsQDNONE           = 'Aucun';
  rsQDCROSS          = 'Grille';
  rsQDQUADRILLES     = 'Croix';

  rsGRAPHICS_PS        = 'PostScript PS';
  rsGRAPHICS_DXF       = 'AutoCAD DXF';
  rsGRAPHICS_SVG       = 'Scalable Graphics SVG';
  rsGRAPHICS_WMF       = 'Windows MetaFile WMF';

  rsDLGDXF_TITLE       = 'Export DXF: %s';
  rsDLGDXF_MSGENTETE   = 'Le fichier d''en-tête est invalide ou inexistant'+#13+
                         'Utiliser la commande ''_dxfin'' d''AutoCAD'+#13+
                         'Seule la section ENTITIES du fichier sera lue';

  // mots clefs du MétaFiltre
  rsHLPMETAFILTRE      = 'METAFILTRE';
  rsMETAFILTRE_NIL     = 'RIEN';          // 0
  rsMETAFILTRE_ALL     = 'TOUT';          // 1
  rsMETAFILTRE_ID      = 'ID';            // 2
  rsMETAFILTRE_LENGTH  = 'LONGUEUR';      // 3
  rsMETAFILTRE_AZIMUTH = 'GISEMENT';      // 4
  rsMETAFILTRE_PENTE   = 'PENTE';         // 5
  rsMETAFILTRE_DATE    = 'DATE';          // 6
  rsMETAFILTRE_COLOR   = 'COULEUR';       // 7
  rsMETAFILTRE_X       = 'COORD_X';       // 8
  rsMETAFILTRE_Y       = 'COORD_Y';       // 9
  rsMETAFILTRE_Z       = 'COORD_Z';       // 10
  rsMETAFILTRE_LARGEUR = 'LARGEUR';       // 11
  rsMETAFILTRE_HAUTEUR = 'HAUTEUR';       // 12
  rsMETAFILTRE_DATES   = 'DATES';         // 13
  rsMETAFILTRE_COLORS  = 'COULEURS';      // 14
  rsMETAFILTRE_SERIE   = 'SERIE';         // 15
  rsMETAFILTRE_RESEAU  = 'RESEAU';        // 16
  rsMETAFILTRE_CODE    = 'CODE';          // 17
  rsMETAFILTRE_EXPE    = 'SEANCE';        // 18
  rsMETAFILTRE_TYPEVISEE = 'TYPE_VISEE';  // 19

  rsMETAFILTRE_APPLY     = 'Appliquer';

  // recherche de stations
  rsMSG_FIND_STATION_TITRE  = 'Recherche de station';
  rsMSG_FIND_STATION_PROMPT = 'Entrer un ID littéral ou un couple série et station (séparateur: point décimal)';

  //*******************************************************
  // resourcestring du dialogue du MétaFiltre
  rsDLGMETAFILTRE_TBS1 = 'Dates';
  rsDLGMETAFILTRE_TBS2 = 'Couleurs';
  rsDLGMETAFILTRE_PERSO= 'Personnalisés';
  //*******************************************************
  // resourcestring du centre d'impression
  rsDLGIMP_TAB1        = 'Aperçu';
  rsDLGIMP_TAB2        = 'Imprimante';
  rsDLGIMP_TAB3        = 'Options de dessin';
  rsDLGIMP_TAB4        = 'Réseaux';
  rsQDRSPACING         = 'Espacement';
  rsECHELLE            = 'Echelle: 1 / ';
  rsLAYERS             = 'Couches de dessin';
  rsPREVIEW            = 'Previsualisation';
  rsSTARTPRINTING      = 'Lancer impression';
  rsREGLE              = 'Règle';

  //******************************************************
  // resourcestring du sélecteur de couleurs
  rsSELECTCOLORTITLE   = 'Selection d''une couleur';
  rsLBLUSEDCOLORS      = 'Dernières couleurs:';
  rsDLGCOULSAVEPAL     = 'Enregistrer palette';
  rsDLGCOULRESTPAL     = 'Restaurer palette';
  rsDLGCOULFILTERPAL   = 'Fichiers de palette (*.pal)|*.pal|Tous (*.*)|*.*';
  rsDLGCOUDELPAL       = 'Ecraser le fichier existant';
  rsPALETTENOTFOUNT    = 'Palette introuvable';


  

  //******************************************************
  // resourcestring de l'utilitaire d'export graphique
  rsDLGGRAPHIC_OUTPUTFMT = 'Format de sortie:';
  rsDLGGRAPHIC_LBFILENAME= 'Nom de fichier:';
  rsDLGGRAPHIC_LBOBS     = 'Commentaires:';
  rsDLG_GRAPHIC_TABTITLE = 'Export graphique';
  rsDLGGRAPHIC_GBCROIX   = 'Quadrillage';
  rsDLGGRAPHIC_GBCHEM    = 'Cheminements et sections';
  rsDLGGRAPHIC_GBWALLS   = 'Parois et couleurs';

  rsDLGGRAPHIC_SPACING   = 'Espacement';
  rsDLGGRAPHIC_TYPEGRID  = 'Type:';
  rsDLGGRAPHIC_CMBGRD2   = 'Croix';
  rdDLGGRAPHIC_LAYER     = 'Couche:';
  rdDLGGRAPHIC_WALLFILL  = 'Remplissage';
  rsDLGGRAPHIC_WFILL1    = 'Plein (1 couleur)';
  rsDLGGRAPHIC_WFILL2    = 'Types de galeries';
  rsDLGGRAPHIC_WFILL3    = 'Couleurs des visées';
  rsDLGGRAPHIC_WFILL4    = 'Par réseaux';
  rsDLGGRAPHIC_WFILL5    = 'Par dates';

  rsDLGGRAPHIC_CHKCHEM   = 'Exporter cheminements';
  rsDLGGRAPHIC_CHKSECT   = 'Exporter sections';

  // resourcestring du centre d'impression
  rsPRN_NOPRINTER        = 'Pas d''imprimante installée';
  rsPRN_TBPRINTER        = 'Imprimante';
  rsPRN_TBDRAW           = 'Dessin';
  rsPRN_TITLE            = 'Centre d''impression [%s]';

  rsPRN_CHKPOLY          = 'Polygonales';
  rsPRN_CHKFILL          = 'Remplissage';
  rsPRN_CHKWALLS         = 'Parois';
  rsPRN_CHKSECTS         = 'Sections';
  rsPRN_CHKSTATIONS      = 'Stations';
  rsPRN_CHKSTATIONS_LBL  = 'ID stations';
  rsPRN_CHKCOTATION      = 'Cotation';
  rsPRN_CHKQUADRILLAGE   = 'Quadrillage';
  rsPRN_CHKENTREES       = 'Entrees';
  rsPRN_CHKANNOTATIONS   = 'Annotations';

  rsPRM_LBANGLEROT       = 'Angle de rotation';
  rsPRN_TYPEQDR          = 'Type de quadrillage';
  rsPRN_QDCROSS          = 'Croix';
  rsPRN_QDQUADRILLES     = 'Grille';
  rsPRN_QDPOINTS         = 'Points';
  rs_PRN_SCALING         = 'Echelle';
  rsPRN_LBSPACING        = 'Espacement';
  rsLANDSCAPE            = 'Paysage';
  rsPORTRAIT             = 'Portrait';



  //-------------------------------------------------
  // resourcestring du dialogue HelpSystem
  rsHLP_BNEDIT         = 'Editer le fichier d''aide';
  rsHLP_DOCONTINUE     = 'Souhaitez-vous continuer ?';
  //-------------------------------------------------

  //-------------------------------------------------
  // resourcestring du dialogue Série/station
  rsDLG_SERST_TITLE    = 'Recherche de station';
  rsDLG_SERST_SERIE    = 'Série:';
  rsDLG_SERST_STATION  = 'Station';
  rsDLG_SERST_CLE      = 'Code terrain';
  rsDLG_SERST_LBSEARCH = 'Recherche';
  rsDLG_SERST_BYSERST  = 'Par couple série / station';
  rsDLG_SERST_BYREFTER = 'Par référence de terrain';

  //-------------------------------------------------
  // resourcestring du dialogue Editeur d'annotations
  rsDLG_ANN_TITLE      = 'Editeur d''annotations';
  rsDLG_ANN_LBTEXTE    = 'Annotation';
  rsDLG_ANN_LBMAX      = 'Longueur max';
  rsDLG_ANN_CHKTXTDISP = 'Dessiner cette annotation';
  rsDLG_ANN_GRBPOSTEXT = 'Positionnement du texte';
    rsDLG_ANN_GRBMETH0 = 'Coordonnées absolues';
    rsDLG_ANN_GRBMETH1 = 'Accroché à la station';
  rsDLG_ANN_LBPOSTEXTE   = 'Point de base';
  rsDLG_ANN_LBSTATION    = 'Station topo';
  rsDLG_ANN_LBOFFSET     = 'Décalage (m) X =';
  rsDLG_ANN_GRBATTRTXT   = 'Attributs de caractères';
  rsDLG_ANN_GRBBASEPT    = 'Point de base du texte';




  //-------------------------------------------------

  // recherche de station par ID littéral
  rsDLG_FIND_PT_BY_ID_TITLE  = 'Recherche par ID littéral';
  rsDLG_FIND_PT_BY_ID_PROMPT = 'Code terrain';

  // calculette intégrée
  rsDLG_CALC_TITLE    = 'Calculatrice et convertisseur de coordonnées';
  rsDLG_CALC_TAB_CAL  = 'Calculatrice';
  rsDLG_CALC_EXPR     = 'Entrer une expression';
  rsDLG_CALC_DOCALC   = 'Calculer';
  rsDLG_CALC_CDR_CONVERT  = 'Convertisseur de coordonnées';
  rsDLG_CALC_CDR_DECLIMAG = 'Calcul de déclinaison magnétique';
  rsDLG_CALC_BTN_CONVERT  = 'Convertir';
  rsDLG_CALC_LB_SYST_SOURCE = 'Système source';
  rsDLG_CALC_LB_SYST_CIBLE  = 'Système cible';
  rsDLG_CALC_HINT_GRD_CONVERSIONS = 'Saisir des valeurs dans la grille ou coller depuis le presse-papiers';
  // gestion du presse papiers pour l'import de données dans la grille Stations
  rsDLG_CLIPBRD_PTS_TITLE = 'Importation de points topo';

  (*-- Messages *)
  rsINSERTITEM         = 'Insérer élément ?';
  rsDELETEITEM         = 'Détruire élément ?';
  rsONCLOSEPRJMNGR     = 'Ceci fermera le document courant'+#13+
                         'Sauvegarder les modifications ?';
  rsMSG_SEEALSO        = 'Voir aussi ...';
  rsMSG_NDSNEEDED      = 'Fichier noeuds inexistant - Recalculer le réseau';
  rsMSG_SAVECHANGES    = 'Enregistrer les modifications';
  rsMSG_ERASESTATION   = 'Ecraser la station %d ?';
  rsMSG_FILENOTFOUND   = 'Fichier %s non trouvé';
  rsMSG_READY          = 'PRET';
  rsMSG_NOFILEOPENED   = 'Pas de fichier ouvert';
  rsDISPLAY_HELP_SYSTEM= 'Démarrage du système d''aide';
  rsHLPCENTER_TITLE    = 'Système d''aide de GHTopo';
  rsMATCHNOTFOUND      = 'Occurrence non trouvée';
  rsNOCANCLOSE         = 'Quitter en sauvegardant';
  rsWARN_FILEALREADYOPEN= 'Un document est déjà ouvert - Poursuivre ?';
  rsSAVECHANGES        = 'Enregistrer les modifications';
  rsNOCANCLOSEWND      = 'Fenêtre permanente de GHTopo, ne peut être fermée';
  rsERASEEXISTNAMEDFILE = 'Ecraser le fichier %s ?';
  rsSAVESLOST          = 'Les modifications seront perdues';
  rsCFGMTFLTR_UNABLE   = 'Erreur dans le fichier de filtres personnalisés';
  rsERRORSTARTPRNCTR   = 'Erreur de démarrage du Centre d''Impression';
  rsNOPRINTERINSTALLED = 'Aucune imprimante installée';
  rsERRORLOADINGTOP    = 'Erreur en chargement du fichier TOP';
  rsIDSERIE_EXISTS     = 'Index de série %d déjà attribué (série #%d)';

  rsMSG_VUE3D_FAIL     = 'Echec au démarrage de la vue 3D';
  rsMSG_VUE2D_FAIL     = 'Echec au démarrage de la vue en plan';
  rsMSG_STATS_DLG_FAIL = 'Echec au démarrage du dialogue de statistiques';
  rsMSG_PRINT_CENTER_FAIL  = 'Echec au démarrage du centre d''impression';
  rsMSG_PROJ_MANAGER_FAIL  = 'Echec au démarrage du frontal de la base de données';
  rsMSG_PROJ4S_FAIL        = 'Echec au démarrage du convertisseur de coordonnées Proj4s';
  rsMSG_DECLIMAG_FAIL      = 'Echec au démarrage du calculateur de déclinaison magnétique';

  rsMSG_TABLE_ENTITY_FAIL  = 'Echec au chargement de la table des entités';

  rsMSG_CODE_NOT_FOUND     = 'Code introuvable';
  rsMSG_EXPE_NOT_FOUND     = 'Expé introuvable';
  rsMSG_ANGLE_OUT_OF_RANGE = '%s doit être compris entre %.2f et %.2f';



  // pour le cadre visualisateur 2D

implementation

end.
