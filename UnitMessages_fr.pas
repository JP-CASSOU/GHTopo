unit UnitMessages_fr;
{Compatibilit� OK avec la version Linux}

interface

const
  NATIONAL_GEODESIC_SYSTEM_IDX = 1; // pour la version fran�aise: Lambert93
  DEFAULT_CODE_EPSG            = 'LT93';

resourcestring
  
  // nom du logiciel
  rsGHTOPOEXENAME      = 'GHTopo';
  rsMAINMENUCAPTION    = '%s - GHTopo';
  rsGHTOPOVERSION      = 'Version 3.1415926 du %s';
  rsGHTOPOLICENSE      = 'Logiciel sous licence GPL';
  rsGHTOPOAUTHOR       = '(c) 1989..2012 Jean Pierre CASSOU';

  // infos sur la session
  rsGHTOPORESOL        = 'R�solution %d x %d';
  rsGHTOPOPlatFormLinux= 'Plateforme: Linux (Fran�ais)';
  rsGHTOPOPlatFormWin32= 'Plateforme: Microsoft Windows (Fran�ais)';

  // langue choisie
  rsCHOOSELANGUAGE     = 'D�marrage en langue fran�aise';
  // fin GHTopo
  rsENDOFHADES         = 'Fin GHTopo';

  rsSERIE_INITIALISATION = 'S�rie 0 non modifiable';

  // lecture des fichiers
  rsSEPARATOR_LINE     = '-------------------------------';
  rsWARNINGS_READFILE  = 'Avertissements de lecture de: ';
  rsCONV_TAB_MAC_UNIX  = '-> Conversion des fichiers TAB pouvant venir du Mac ou d''Unix';

  // messages dans TToporobotStructure.LoadFichierTab();
  rsRD_TAB_MSG_ERR     = 'Erreur lors du traitement de la ligne';
  rsRD_TAB_D_MULTI_OBS = 'D�but de commentaires multilignes en ligne %d';
  rsRD_TAB_F_MULTI_OBS = 'Fin de commentaires multilignes en ligne %d';
  rsRD_TAB_STOP_9999   = '*** Line %d: Lecture arr�t�e par le code -9999 ***';
  rsRD_TAB_LN_OBS      = '-- La ligne %d est un commentaire';
  rsRD_TAB_LASTSAVES   = 'Derni�re sauvegarde: %s %s';
  rsRD_TAB_CLASSEURS   = '-7 [Classeurs] Cette fonctionnalit� est ignor�e';
  rsRD_TAB_ENTRANCE    = 'Entr�e cr��e #%d';
  rsRD_TAB_ENTR_NOGEOREF = '[Avertissement] (Ligne %d): Entr�e %d (%s) non g�or�f�renc�e';
  rsRD_TAB_ENTR_BADLINK  = '[Avertissement] (%d): Entr�e %d (%s): Raccordement incorrect [S�rie: %d - Station: %d]';
  rsRD_TAB_IGNORED_SEC   = ' [Information] Section ignor�e';
  rsRD_TAB_BAD_TRIP      = 'S�ance incorrecte';
  rsRD_TAB_BAD_CODE      = 'Code incorrect';
  rsRD_TAB_BAD_DATE      = '[Avertissement] (%d) - Date incorrecte mise � la date actuelle';
  rsRD_TAB_BAD_IDXRES    = '[Avertissement] (%d) - Index de r�seau incorrect (%d) pour la s�rie %d - Mis � 0';
  rsRD_TAB_IDEXTR_MM     = '[Avertissement] (%d) - Les ID d''extr�mit� de la s�rie %d sont identiques';
  rsRD_TAB_SELF_CLOSURE  = '[Avertissement] (%d) - Le terminus de la s�rie %d se branche sur elle-m�me.';
  rsRD_TAB_NEG_LONG      = '[Avertissement] (%d) - Longueur n�gative (%.2f m), chang�e de signe';
  rsRD_ERROR_LN          = '*** Erreur en ligne #%d: %s';
  rsRD_CONTNT_LN         = '    Contenu de la ligne: %s';
  rsRD_TAB_FIELD_VALUES  = ' Valeurs des champs:';
  rsRD_TAB_NOFATAL_ERR   = 'Le fichier ne comporte pas d''erreurs fatales';
  rsRD_TAB_WTFATAL_ERR   = 'Le fichier comporte %d erreurs';

  //libell�s communs
  rsDEFAULT            = 'D�faut';
  rsFORFIXEDPOINTS     = 'Item pour entrees et points fixes';
  rsWARNINGENTRYADDED  = 'Le nombre d''entr�es dans -6 est diff�rent de celui de -5; Corrig�.';
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
  rsREMONTEE           = '--> Remont�e du syst�me: inconnues recherchees';

  rsTRIANGULARISATION  = '--> Triangularisation';
  rs2NDMEMBER          = ' - Second membre';
  rsCOMPESMATRIX       = ' - Matrice de compensation';
  rsNODECOORD          = ' - Factorisation: Coordonn�es des noeuds';
  rsCOORDONNEES_OK     = '--> Coordonn�es des %d noeuds OK';

  rsDEL_TEMP_MATRIX    = '--> Destruction des matrices temporaires';

  rsWRITE_NODES_COORDS = 'Ecriture des coordonn�es des noeuds dans: ';
  rsNODES_COORDS_FOR_DB= 'Coordonn�es des noeuds pour: ';
  rsCALCULCONTOURS     = 'CALCUL CONTOURS GALERIES';
  rsCALCUL_ANTENNES    = 'Calcul des visees en antenne';

  // �tapes du calcul
  rsRECENSEM_JONC      = '-- Recensement des jonctions';
  rsPURGE_TABLE_JONC   = '-- Purge table des jonctions';
  rsPURGE_TABLE_BRCH   = '-- Purge table des branches';
  rsNB_BRCHS           = '%d branches';

  rsSTEP_CALC_01       = '->Etape %d / %d: Matrice d''assemblage R';
  rsSTEP_CALC_02       = '->Etape %d / %d: Matrice de pond�ration W';
  rsSTEP_CALC_03       = '->Etape %d / %d: Matrice de compensation B = Rt.W*.R';
  rsSTEP_CALC_04       = '->Etape %d / %d: Coordonn�es des noeuds';
  rsSTEP_CALC_05       = '->Etape %d / %d: R�partition des �carts';
  rsSTEP_CALC_06       = '->Etape %d / %d: Lib�rations m�moire et fin de traitement';
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
  rsCSV_FILE_FILTER       = 'Fichier texte tabul� (*.csv)|*.csv|' +
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
    rsRECENT_DOCS      = '&Documents r�cents';
    rsRELOAD           = 'Recharger le document';
    rsEDITTAB          = 'Editer fichier &Tab';
    rsPRINT            = 'Im&primer';
    rsVTOPO            = 'Export Visual Topo';
    rsTHERION          = 'Export vers Therion';
    rsEXPORT_GIS       = 'Export vers logiciels de cartographie';

    rsEXPORTGRAPHIQUE  = 'E&xport graphique (PS et SVG)';
    rsERRINFOXTB       = '&Rapport d''erreur de lecture';
    rsGHTOPO_QUIT      = '&Quitter GHTopo';
    rsQUIT_EDITOR     = '&Quitter l''�diteur de texte';
  rsMNU_EDITION        = '&Edition';
  rsMNU_TOPOGRAPHIE    = '&Topographie';
    rsCHECKBASE        = '&V�rifier les donn�es';
    rsCOMPILE          = '&Calculer le r�seau';
    rsVUEPLAN          = 'Vue en &Plan';
    rsVUE3D            = '&Vue 3D';
    rsRENDU3D          = 'Rendu &3D';
    rsSTATISTIQUES     = '&Statistiques';
    rsINFOCAVITE       = '&Info cavit�';
    rsNODESCOORDINATES = 'Coordonn�es des &Noeuds';
  rsMNU_WINDOW         = '&Fen�tre';
    rsMNU_STAY_ON_TOP  = 'Stay on &Top';
    rsWND_DATABASE     = 'Base de donn�es';
    rsWND_PLAN         = '&Vue en plan';
  rsMNU_TOOLS          = '&Outils';
  rsMNU_HELP           = '&Aide';
    rsHLPINDEX         = '&Index';
    rsHLPNEWS          = 'Nouveaut�s';
    rsABOUT            = '&A propos de GHTopo';

   rsASSISTANT_SUCCESS = 'Document cr�� avec succ�s par l''assistant';
   rsASSISTANT_RELOAD  = 'Rechargement du document';
   rsASSISTANT_ECHEC   = 'Echec de l''assistant';


  //**************************************
  // resourcestring des titres de boites de dialogue
  //**************************************
  // resourcestring des libell�s communs
  rsLBL_COMMENTS          = 'Commentaires';

  // resourcestring du visualisateur 2D
  rsVUE2D_FMT_INFOS_STATION_RAPIDE = 'Station: %s (%d.%d): X=%.2f Y=%.2f Z=%.2f';
  rsVUE2D_FMT_INFOS_ID_STATION     = '%d.%d [%s]';
  rsVUE2D_FMT_INFOS_MESURES        = 'L = %.2f, Az = %.2f, P =%.2f, G = %.2f, D = %.2f, H = %.2f, B = %.2f';
  rsVUE2D_FMT_INFOS_COORDONNEES    = 'X = %s, Y = %s, Z = %s';

  // resourcestring de l'Assistant
  rsASSIST_TITLE       = 'Assistant Nouvelle Cavit�';
  rsASSIST_BNGENERAL   = 'G�n�ral';
  rsASSIST_BNENTRANCES = '1�re Entr�e';
  rsASSIST_BNCODES     = '1er Code';
  rsASSIST_BNEXPES     = '1�re S�ance';
  rsASSIST_BNSERIE     = '1�re S�rie';
  rsASSIST_BNSAVE      = 'Sauvegarder ...';
  rsASSIST_BNCANCEL    = 'Annuler';
  rsASSIST_LBNOMETUDE  = 'Nom de l''�tude';
  rsASSIST_LBOBSETUDE  = 'Commentaires �tude';
  rsASSIST_LBNOMENTREE = 'Entr�e principale';

  rsASSIST_LBCOORDS    = 'Coordonn�es';
  rsASSIST_LBOBSENTREE = 'Observations';
  rsASSIST_LBCONVERTISSEUR = 'Convertisseur ...';
  rsASSIST_LBREFSTATION    = 'Station initiale';
  rsASSIST_LBREFSERIE      = 'S�rie';
  rsASSIST_LBREFPOINT      = 'Point';
  rsASSIST_LBCOMMENTAIRE   = 'Commentaires';


  rsASSIST_LBSYSTGEO   = 'Syst�mes de coordonn�es g�ographiques';



  // resourcestring du cadre Entr�es
  rsCDR_ENTR_NOENTRANCE   = 'Entr�e';
  rsCDR_ENTR_ENTRNAME     = 'Nom de l''entr�e';
  rsCDR_ENTR_COORDINATES  = 'Coordonn�es';
  rsCDR_ENTR_STATOFENTR   = 'Station de r�f�rence:';

  // resourcestring du cadre Codes
  rsCDR_CODES_NUMERO      = 'Code:';
  rsCDR_CODES_TYPEGALERIE = 'Type de vis�e:';
  rsTITRE_SELECTEUR_VISEE = 'Choix du type de vis�e';
    rsCMBTYPE_D          = '0 - D�faut (conduit fossile)';
    rsCMBTYPE_E          = '1 - Entr�e';
    rsCMBTYPE_B          = '2 - Galerie fossile';
    rsCMBTYPE_V          = '3 - Ecoulement libre';
    rsCMBTYPE_W          = '4 - Siphon';
    rsCMBTYPE_C          = '5 - Passage ennoyable';
    rsCMBTYPE_F          = '6 - Point fixe';
    rsCMBTYPE_S          = '7 - Topo de surface';
    rsCMBTYPE_A          = '8 - Tunnel artificiel';
    rsCMBTYPE_M          = '9 - Filon minier';
  rsCDR_CODES_VISEE       = 'Vis�e';
    rsCDR_CODES_VDIRECT   = 'Directe';
    rsCDR_CODES_VINVERSE  = 'Inverse';

  rsCDR_CODES_GRADCOMPAS  = 'Graduation du compas:';
  rsCDR_CODES_GRADCLINO   = 'Graduation du clinom�tre:';
    rsCDR_CODES_CMBUNIT_0 = '400 - Grades';
    rsCDR_CODES_CMBUNIT_1 = '360 - Degr�s';
    rsCDR_CODES_CMBUNIT_2 = '370 - Pourcentages';
    rsCDR_CODES_CMBUNIT_3 = '380 - D�nivellations';
  rsCDR_CODES_FACT        = 'Longueurs x';
  rsCDR_CODES_POSZERO     = 'Position du z�ro:';
    rsCDR_CODES_CMBZERO_0 = 'Nadiral';
    rsCDR_CODES_CMBZERO_1 = 'Horizontal';
    rsCDR_CODES_CMBZERO_2 = 'Z�nithal';
  rsCDR_CODES_ANGLIMIT    = 'Angle limite:';
  rsCDR_CODES_PRECISION   = 'Incertitude des mesures:';
  // resourcestring des onglets du Gestionnaire de Cavit�
  rsTBS_GENERAL        = 'G�n�ral';
  rsTBS_ENTRANCE       = 'Entr�es';
  rsTBS_CODES          = 'Codes';
  rsTBS_TRIPS          = 'S�ances';
  rsTBS_SERIES         = 'S�ries';
  rsTBS_RESEAUX        = 'R�seaux';
  rsTBS_ANTENNES       = 'Vis�es en antenne';
  rsTBS_MAINTENANCE    = 'Maintenance';



  rsLB_NOM_ETUDE          = 'Nom de l''�tude';
  rsLB_COMMENTAIRE_ETUDE  = 'Commentaires';
  rsLB_CODE_EPSG          = 'Code EPSG du syst�me de coordonn�es';
  rsBTN_SELECT_EPSG       = 'Choisir ...';
  rsBTN_CALC_DECLIMAGS    = 'Calculer les d�clinaisons magn�tiques';

  //*******************************************************
  // resourcestrings du cadre R�seaux
  rsCDR_RESEAU_LBIDX   = 'Numero';
  rsCDR_RESEAU_NAME    = 'Nom';
  rsCDR_RESEAU_TYPE    = 'Type de r�seau';
    rsCDR_RESEAU_CB0     = 'Cavit� naturelle';
    rsCDR_RESEAU_CB1     = 'Cavit� artificielle';
    rsCDR_RESEAU_CB2     = 'Topo de surface';
    rsCDR_RESEAU_CB3     = 'Thalweg';
    rsCDR_RESEAU_CB4     = 'Route ou piste';
    rsCDR_RESEAU_CB5     = 'Sentier';
    rsCDR_RESEAU_CB6     = 'Autre';





  
  //*******************************************************
  // resourcestrings du cadre Exp�s
  rsCDR_EXPE_SEANCE      = 'S�ance topo:';
  rsCDR_EXPE_DATE        = 'Date';

  rsCDR_EXPE_DECLIMAG    = 'D�clinaison';
  rsCDR_EXPE_INCLIN      = 'Inclinaison';
  rsCOLOR                = 'Couleur';

  rsCDR_EXPE_SPELEOMETRE = 'Sp�l�om�tre';
  rsCDR_EXPE_SPELEOGRAPHE= 'Sp�l�ographe';
  //*********************************************
  // resourcestring du cadre S�ries

  rsDLG_PJMNGR_MOVETOSERIE = 'Changer de s�rie ?';
  rsDLG_PJMNGR_ADDSERIE  = 'Ajouter S�rie';
  rsCDR_SERIE_LB_RESEAU  = 'R�seau';
  rsCDR_SERIE_VALIDATE   = 'Valider';
  rsCDR_SERIE_ADDPHOTO   = 'Ajouter photo';
  rsCDR_SERIE_NUMERO     = 'S�rie:';
  rsCDR_SERIE_NAME       = 'Nom:';
  rsCDR_SERIE_DEPART     = 'D�part:';
  rsCDR_SERIE_ARRIVEE    = 'Arriv�e:';
  rsCDR_SERIE_CHANCE     = 'Chance:'; // en espagnol: Suerte
    rsCDR_SERIE_CHANCE0  = 'Aucune';
    rsCDR_SERIE_CHANCE1  = 'Faible';
    rsCDR_SERIE_CHANCE2  = 'Bonne';
    rsCDR_SERIE_CHANCE3  = 'Excellente';
  rsCDR_SERIE_OBSTACLE   = 'Obstacle:';
    rsCDR_SERIE_OBSTACLE0= 'Aucun';
    rsCDR_SERIE_OBSTACLE1= 'Puits';
    rsCDR_SERIE_OBSTACLE2= 'Chemin�e';
    rsCDR_SERIE_OBSTACLE3= 'Etroiture';
    rsCDR_SERIE_OBSTACLE4= 'Lac';
    rsCDR_SERIE_OBSTACLE5= 'Siphon';
    rsCDR_SERIE_OBSTACLE6= 'Effondrement';
    rsCDR_SERIE_OBSTACLE7= 'Concr�tionnement';
    rsCDR_SERIE_OBSTACLE8= 'S�diments';
    rsCDR_SERIE_OBSTACLE9= 'Autre';
    // sp�cifique � la France lol
    rsCDR_SERIE_OBSTACLE10 = 'Gaz toxiques';       // aurait d� �tre ajout�
    rsCDR_SERIE_OBSTACLE11 = 'Oies agressives';    // r�f�rence � une exp� franco-chinoise
    rsCDR_SERIE_OBSTACLE12 = 'Animaux dangereux';  // r�f�rence � une exp� de LH Fage
    rsCDR_SERIE_OBSTACLE13 = 'Baisodrome';         // pour la topographie de certains sentiers forestiers xD



  rsCDR_SERIE_LOCKED     = 'Verrouill�';
   rsCDR_SERIE_INSERTLINE = 'Insertion de lignes';
   rsCDR_SERIE_NBLINES    = 'Nombre de lignes';
   rsCDR_SERIE_DELETELINE    = 'Effacement de lignes';
   rsCDR_SERIE_UNDOCOPY      = 'Recopie vers le bas';
   rsCDR_SERIE_INC_UNDOCOPY  = 'Recopie vers le bas incr�mentale';
  rsCDR_SERIE_IMPLEMENT      = 'Valider';
  rsINPUT_COMMENTAIRE_TITRE  = 'Commentaires station';
  rsINPUT_COMMENTAIRE_MSG    = 'Entrez un texte';
  rsCDR_SERIE_PASTE          = 'Importer le tableau depuis le presse-papiers';
  rsCDR_SERIE_BTN_GRD_COPY   = 'Copier la grille';

  rsCDR_SERIE_MSG_ERR_ENTREE_NOT_FOUND = 'Entr�e introuvable';
  rsCDR_SERIE_MSG_ERR_RESEAU_NOT_FOUND = 'R�seau introuvable';
  rsCDR_SERIE_MSG_ERR_CODE_NOT_FOUND   = 'Code instruments introuvable';
  rsCDR_SERIE_MSG_ERR_EXPE_NOT_FOUND   = 'Session topo introuvable';
  rsCDR_SERIE_MSG_ERR_SERIE_NOT_FOUND  = 'Code instruments introuvable';



  rsCDR_SERIE_MSG_ERR_LONG   = 'La longueur doit �tre positive et inf�rieure � %.0f m';
  rsCDR_SERIE_MSG_ERR_LRUD   = 'La distance ne doit pas �tre n�gative';

  //*******************************************************
  // resoursestring du cadre Antennes
  rsCDR_ANTENNES_AC_ADDLINE = 'Ajouter une ligne';
  rsCDR_ANTENNES_AC_DELLINE = 'Supprimer une ligne';
  rsCDR_ANTENNES_AC_SAVEGRD = 'Valider les modifications';
  rsCDR_ANTENNES_DEL_LINE   = 'Supprimer la ligne %d';

  //*******************************************************
  // resourcestring du cadre CdrNavigateurDB
  rsCDR_NAVIG_DB_DO_SORT    = 'Trier';
  rsCDR_NAVIG_DB_DO_ADD     = 'Ajouter �l�ment';
  rsCDR_NAVIG_DB_DO_DELETE  = 'Supprimer �l�ment';



  // g�n�ral
  rsSELECTALL          = 'Tout s�lectionner';
  rsDESELECTALL        = 'Tout d�s�lectionner';
  rsOBS                = 'Commentaires';
  rsFILTERS            = 'Filtres:';
  rsCHKSELECTALL       = 'S�l. tout';
  rsCHKDESELECTALL     = 'D�s�l. tout';
  rsCHKREVERSE         = 'Inv. s�l.';
  rsMAIN_NETWORK       = 'R�seau principal';

  //*********************************
  // resourcestring du dialogue Vtopo
  rsVTOPO_EDPREFIX       = 'Pr�fixe des stations:';
  rsVTOPO_LBIDPTDEP      = 'ID point de d�part:';
  rsVTOPO_LBREPORTER     = 'Op�rateur du report:';
  rsLBFICHIER          = 'Fichier: ';
  rsLBMAINENTRANCE     = 'Entr�e principale:';
  rsLBIDMAINST         = 'Station de d�part:';
  rsLBSTPREFIX         = 'Pr�fixe des stations';
  rsLBREPORTER         = 'Report:';
  rsLBMAINENTCOORD     = 'Coordonn�es de l''entr�e principale';
  rsLBCOLORDEFAULT     = 'Couleur par d�faut';

  //*******************************************************
  // resourcestring du visualisateur OpenGL
  rsOGLVIEWERTITLE     = 'Visualisateur OpenGL [%s]';
  rsLBLBACKCOLOR       = 'Arri�re-plan';
  rsLBLCUBECOLOR       = 'Cube';
  rsLBLREFCOLOR        = 'R�f�rentiel';
  rsLBLREFSIZE         = 'Taille';
  rsOPENGLERROR        = 'Erreur OpenGL';
  rsOGLVQUIT           = 'Quitter le visualisateur OpenGL';
  //*******************************************************
  // resourcestrings de l'outil d'exportation graphique
  rsPSDXF_TITLE        = 'Export graphique: [%s]';
  rsDODISPDESELECTEDPARTS = 'Afficher les parties refus�es par le M�taFiltre - Couleur';
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
  rsDLGDXF_MSGENTETE   = 'Le fichier d''en-t�te est invalide ou inexistant'+#13+
                         'Utiliser la commande ''_dxfin'' d''AutoCAD'+#13+
                         'Seule la section ENTITIES du fichier sera lue';

  // mots clefs du M�taFiltre
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
  rsMSG_FIND_STATION_PROMPT = 'Entrer un ID litt�ral ou un couple s�rie et station (s�parateur: point d�cimal)';

  //*******************************************************
  // resourcestring du dialogue du M�taFiltre
  rsDLGMETAFILTRE_TBS1 = 'Dates';
  rsDLGMETAFILTRE_TBS2 = 'Couleurs';
  rsDLGMETAFILTRE_PERSO= 'Personnalis�s';
  //*******************************************************
  // resourcestring du centre d'impression
  rsDLGIMP_TAB1        = 'Aper�u';
  rsDLGIMP_TAB2        = 'Imprimante';
  rsDLGIMP_TAB3        = 'Options de dessin';
  rsDLGIMP_TAB4        = 'R�seaux';
  rsQDRSPACING         = 'Espacement';
  rsECHELLE            = 'Echelle: 1 / ';
  rsLAYERS             = 'Couches de dessin';
  rsPREVIEW            = 'Previsualisation';
  rsSTARTPRINTING      = 'Lancer impression';
  rsREGLE              = 'R�gle';

  //******************************************************
  // resourcestring du s�lecteur de couleurs
  rsSELECTCOLORTITLE   = 'Selection d''une couleur';
  rsLBLUSEDCOLORS      = 'Derni�res couleurs:';
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
  rsDLGGRAPHIC_WFILL3    = 'Couleurs des vis�es';
  rsDLGGRAPHIC_WFILL4    = 'Par r�seaux';
  rsDLGGRAPHIC_WFILL5    = 'Par dates';

  rsDLGGRAPHIC_CHKCHEM   = 'Exporter cheminements';
  rsDLGGRAPHIC_CHKSECT   = 'Exporter sections';

  // resourcestring du centre d'impression
  rsPRN_NOPRINTER        = 'Pas d''imprimante install�e';
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
  // resourcestring du dialogue S�rie/station
  rsDLG_SERST_TITLE    = 'Recherche de station';
  rsDLG_SERST_SERIE    = 'S�rie:';
  rsDLG_SERST_STATION  = 'Station';
  rsDLG_SERST_CLE      = 'Code terrain';
  rsDLG_SERST_LBSEARCH = 'Recherche';
  rsDLG_SERST_BYSERST  = 'Par couple s�rie / station';
  rsDLG_SERST_BYREFTER = 'Par r�f�rence de terrain';

  //-------------------------------------------------
  // resourcestring du dialogue Editeur d'annotations
  rsDLG_ANN_TITLE      = 'Editeur d''annotations';
  rsDLG_ANN_LBTEXTE    = 'Annotation';
  rsDLG_ANN_LBMAX      = 'Longueur max';
  rsDLG_ANN_CHKTXTDISP = 'Dessiner cette annotation';
  rsDLG_ANN_GRBPOSTEXT = 'Positionnement du texte';
    rsDLG_ANN_GRBMETH0 = 'Coordonn�es absolues';
    rsDLG_ANN_GRBMETH1 = 'Accroch� � la station';
  rsDLG_ANN_LBPOSTEXTE   = 'Point de base';
  rsDLG_ANN_LBSTATION    = 'Station topo';
  rsDLG_ANN_LBOFFSET     = 'D�calage (m) X =';
  rsDLG_ANN_GRBATTRTXT   = 'Attributs de caract�res';
  rsDLG_ANN_GRBBASEPT    = 'Point de base du texte';




  //-------------------------------------------------

  // recherche de station par ID litt�ral
  rsDLG_FIND_PT_BY_ID_TITLE  = 'Recherche par ID litt�ral';
  rsDLG_FIND_PT_BY_ID_PROMPT = 'Code terrain';

  // calculette int�gr�e
  rsDLG_CALC_TITLE    = 'Calculatrice et convertisseur de coordonn�es';
  rsDLG_CALC_TAB_CAL  = 'Calculatrice';
  rsDLG_CALC_EXPR     = 'Entrer une expression';
  rsDLG_CALC_DOCALC   = 'Calculer';
  rsDLG_CALC_CDR_CONVERT  = 'Convertisseur de coordonn�es';
  rsDLG_CALC_CDR_DECLIMAG = 'Calcul de d�clinaison magn�tique';
  rsDLG_CALC_BTN_CONVERT  = 'Convertir';
  rsDLG_CALC_LB_SYST_SOURCE = 'Syst�me source';
  rsDLG_CALC_LB_SYST_CIBLE  = 'Syst�me cible';
  rsDLG_CALC_HINT_GRD_CONVERSIONS = 'Saisir des valeurs dans la grille ou coller depuis le presse-papiers';
  // gestion du presse papiers pour l'import de donn�es dans la grille Stations
  rsDLG_CLIPBRD_PTS_TITLE = 'Importation de points topo';

  (*-- Messages *)
  rsINSERTITEM         = 'Ins�rer �l�ment ?';
  rsDELETEITEM         = 'D�truire �l�ment ?';
  rsONCLOSEPRJMNGR     = 'Ceci fermera le document courant'+#13+
                         'Sauvegarder les modifications ?';
  rsMSG_SEEALSO        = 'Voir aussi ...';
  rsMSG_NDSNEEDED      = 'Fichier noeuds inexistant - Recalculer le r�seau';
  rsMSG_SAVECHANGES    = 'Enregistrer les modifications';
  rsMSG_ERASESTATION   = 'Ecraser la station %d ?';
  rsMSG_FILENOTFOUND   = 'Fichier %s non trouv�';
  rsMSG_READY          = 'PRET';
  rsMSG_NOFILEOPENED   = 'Pas de fichier ouvert';
  rsDISPLAY_HELP_SYSTEM= 'D�marrage du syst�me d''aide';
  rsHLPCENTER_TITLE    = 'Syst�me d''aide de GHTopo';
  rsMATCHNOTFOUND      = 'Occurrence non trouv�e';
  rsNOCANCLOSE         = 'Quitter en sauvegardant';
  rsWARN_FILEALREADYOPEN= 'Un document est d�j� ouvert - Poursuivre ?';
  rsSAVECHANGES        = 'Enregistrer les modifications';
  rsNOCANCLOSEWND      = 'Fen�tre permanente de GHTopo, ne peut �tre ferm�e';
  rsERASEEXISTNAMEDFILE = 'Ecraser le fichier %s ?';
  rsSAVESLOST          = 'Les modifications seront perdues';
  rsCFGMTFLTR_UNABLE   = 'Erreur dans le fichier de filtres personnalis�s';
  rsERRORSTARTPRNCTR   = 'Erreur de d�marrage du Centre d''Impression';
  rsNOPRINTERINSTALLED = 'Aucune imprimante install�e';
  rsERRORLOADINGTOP    = 'Erreur en chargement du fichier TOP';
  rsIDSERIE_EXISTS     = 'Index de s�rie %d d�j� attribu� (s�rie #%d)';

  rsMSG_VUE3D_FAIL     = 'Echec au d�marrage de la vue 3D';
  rsMSG_VUE2D_FAIL     = 'Echec au d�marrage de la vue en plan';
  rsMSG_STATS_DLG_FAIL = 'Echec au d�marrage du dialogue de statistiques';
  rsMSG_PRINT_CENTER_FAIL  = 'Echec au d�marrage du centre d''impression';
  rsMSG_PROJ_MANAGER_FAIL  = 'Echec au d�marrage du frontal de la base de donn�es';
  rsMSG_PROJ4S_FAIL        = 'Echec au d�marrage du convertisseur de coordonn�es Proj4s';
  rsMSG_DECLIMAG_FAIL      = 'Echec au d�marrage du calculateur de d�clinaison magn�tique';

  rsMSG_TABLE_ENTITY_FAIL  = 'Echec au chargement de la table des entit�s';

  rsMSG_CODE_NOT_FOUND     = 'Code introuvable';
  rsMSG_EXPE_NOT_FOUND     = 'Exp� introuvable';
  rsMSG_ANGLE_OUT_OF_RANGE = '%s doit �tre compris entre %.2f et %.2f';



  // pour le cadre visualisateur 2D

implementation

end.
