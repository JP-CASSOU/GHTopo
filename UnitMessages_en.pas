unit UnitMessages_en;

interface
const
  NATIONAL_GEODESIC_SYSTEM_IDX = 0; // pour la version française: Lambert93
  DEFAULT_CODE_EPSG            = 'LT93';

resourcestring

  // nom du logiciel
  rsGHTOPOEXENAME      = 'GHTopo';
  rsMAINMENUCAPTION    = '%s - GHTopo';
  rsGHTOPOVERSION      = 'Version 3.1415926 at %s';
  rsGHTOPOLICENSE      = 'Software under GPL''s license';
  rsGHTOPOAUTHOR       = '(c) 1989..2008 Jean Pierre CASSOU';
  rsGHTOPOHANDICAP     = 'Software developed by a handicapped person';

  // infos sur la session
  rsGHTOPORESOL        = 'Resolution %d x %d';
  rsGHTOPOPlatFormLinux= 'Platform: Linux (English)';
  rsGHTOPOPlatFormWin32= 'Platform: Microsoft Windows (English)';

  // langue choisie
  rsCHOOSELANGUAGE     = 'Starting in English language';
  // fin HADES
  rsENDOFHADES         = 'Ending GHTopo';

  rsSERIE_INITIALISATION = 'Cannot modify Serie #0';

   // lecture des fichiers
  rsSEPARATOR_LINE     = '-------------------------------';
  rsCONV_TAB_MAC_UNIX  = '-> Converting TAB files from Mac or Unix OS';
  rsWARNINGS_READFILE  = 'Warnings in reading file: ';

  // messages dans TToporobotStructure.LoadFichierTab();
  rsRD_TAB_MSG_ERR     = 'Processing line error';
  rsRD_TAB_D_MULTI_OBS = 'Starting multiline comments at line #%d';
  rsRD_TAB_F_MULTI_OBS = 'Ending multiline comments at line #d';
  rsRD_TAB_STOP_9999   = '*** Line %d: Reading aborted by -9999 code***';
  rsRD_TAB_LN_OBS      = 'Line %d is a comment';
  rsRD_TAB_LASTSAVES   = 'Last saves: %s %s';
  rsRD_TAB_CLASSEURS   = '-7 [Folders] Functionnality disabled';
  rsRD_TAB_ENTRANCE    = 'Created entrance #%d';
  rsRD_TAB_ENTR_NOGEOREF = '[Warning] (%d): Entrance #%d (%s): Ungeoreferenced';
  rsRD_TAB_ENTR_BADLINK  = '[Warning] (%d): Entrance #%d (%s): Uncorrect linkage [Serie: %d - Station: %d]';
  rsRD_TAB_IGNORED_SEC   = ' [Information] Ignored section';
  rsRD_TAB_BAD_TRIP      = 'Uncorrect trip';
  rsRD_TAB_BAD_CODE      = 'Uncorrect code';
  rsRD_TAB_BAD_DATE      = '[Warning] (%d) - Invalid date - System-date assigned';
  rsRD_TAB_BAD_IDXRES    = '[Warning] (%d) - Network index unknown (%d) for serie %d - Value 0 assigned';
  rsRD_TAB_IDEXTR_MM     = '[Warning] (%d) - Serie %d: Start and end with same ID';
  rsRD_TAB_SELF_CLOSURE  = '[Warning] (%d) - Le terminus de la série %d se branche sur elle-même.';
  rsRD_TAB_NEG_LONG      = '[Warning] (%d) - Negative length (%.2f m); muliply by -1';
  rsRD_ERROR_LN          = '*** Error in line #%d: %s';
  rsRD_CONTNT_LN         = '    Line contents: %s';
  rsRD_TAB_FIELD_VALUES  = ' Fields values:';
  rsRD_TAB_NOFATAL_ERR   = 'No fatal errors in this fils';
  rsRD_TAB_WTFATAL_ERR   = '%d errors found in this file';


  //libellés communs
  rsDEFAULT            = 'Default';
  rsFORFIXEDPOINTS     = 'Item for entrances and Fixed points';
  rsWARNINGENTRYADDED  = 'Number entrances in -6 and -5 sections are differents; Corrected';
  rsCALCULNODES        = 'CALCULATING NODES COORDINATES';
  rsLINEOFNB           = '---> Line %d / %d';
  rsBINDMATRIX         = 'Connectivity Matrix';
  rsBUILDMATRICE       = 'BUILDING COMPENSATION MATRIX';
  rsFACTORISEMATRICE   = 'FACTORIZING COMPENSATION MATRIX';
  rsFINDING_BRANCHES   = 'Finding branches';
  rsADDENTRANCES       = 'Adding cave entrances';
  rsFIND_SUM_LIMITS    = 'Finding not-zero limits';
  rsNB_NON_NUL_TERMES  = ' %d not null terms in R matrix';
  rsPR100_NON_NULS     = ' i.e: %.3f%% of (%d x %d) = %d terms';

  rsDESCENTE           = '--> Down system: V.V* = A';
  rsREMONTEE           = '--> Up system: Equation Unknowns found';

  rsTRIANGULARISATION  = '--> Triangularisation';
  rs2NDMEMBER          = ' - Second member';
  rsCOMPESMATRIX       = ' - Compensation matrix';
  rsNODECOORD          = ' - Factorization: Nodes coordinates';
  rsCOORDONNEES_OK     = 'Coordinates of the %d noeuds OK';

  rsDEL_TEMP_MATRIX    = '--> Deleting temporary matrices';

  rsWRITE_NODES_COORDS = 'Writing nodes coordinates in: ';
  rsNODES_COORDS_FOR_DB= 'Nodes coordinates for: ';
  rsCALCULCONTOURS     = 'CALCULATING VOLUMES';
  rsCALCUL_ANTENNES    = 'Calculating antenna shots';

  rsAXIS               = 'Axis: ';

  // étapes du calcul
  rsRECENSEM_JONC      = 'Finding Junctions';
  rsPURGE_TABLE_JONC   = 'Clear jonctions table';
  rsPURGE_TABLE_BRCH   = 'Clear branches tables';
  rsNB_BRCHS           = '%d branches';

  rsSTEP_CALC_01       = '->Step %d / %d: Connectivity matrix R';
  rsSTEP_CALC_02       = '->Step %d / %d: Weighting matrix W';
  rsSTEP_CALC_03       = '->Step %d / %d: Compensation matrix B = Rt.W*.R';
  rsSTEP_CALC_04       = '->Step %d / %d: Nodes coordinates';
  rsSTEP_CALC_05       = '->Step %d / %d: Adjust deviations';
  rsSTEP_CALC_06       = '->Step %d / %d: Memory free and ending processing';
  rsFREE_TEMP_VARS     = 'DELETING TEMPORARY VARIABLES';

  rsREPARTIR_ECARTS    = 'ADJUST DEVIATIONS';
  rsSCAN_BRCHS         = 'Scanning branches';


  //********************************
  // Filtres de fichiers
  rsTOPOROBOT_FILE_FILTER = 'Toporobot / GHTopo documents (*.xtb; *.tab)|*.xtb;*.tab';
  rsFILEFILTER_ALL        = 'All (*.*)|*.*';


  //*********************************************
  // resourcestring du menu principal
  rsMSGASSIGNCAPTIONS  = 'Building interface';
  rsMNU_FILE           = '&File';
    rsNEW              = '&New';
    rsOPEN             = '&Open';
    rsSAVE             = '&Save';
    rsSAVEAS           = 'Save &as';
    rsCLOSE            = '&Close';
    rsRECENT_DOCS      = 'Recent &Documents';
    rsRELOAD           = 'Reload this document';
    rsEDITTAB          = 'Edit &Tab file';
    rsPRINT            = '&Print';
    rsVTOPO            = '&Visual Topo export';
    rsEXPORTGRAPHIQUE  = 'Graphical E&xport (PS, DXF, SVG)';
    rsEXPORT_GIS       = 'Export to GIS software';
    rsERRINFOXTB       = 'Error &Report';
    rsGHTOPO_QUIT       = '&Quit GHTopo';
    rsQUIT_EDITOR     = '&Quitter text editor';
  rsMNU_EDITION        = '&Edit';
  rsMNU_TOPOGRAPHIE    = '&Survey';
    rsCHECKBASE        = 'Check &database';
    rsCOMPILE          = '&Calculate the network';
    rsVUEPLAN          = '&Plan view';
    rsVUE3D            = '3D &View';
    rsRENDU3D          = '&3D Rendering';
    rsSTATISTIQUES     = '&Statistics';
    rsINFOCAVITE       = 'Cave &Info';
    rsNODESCOORDINATES = '&Nodes coordinates';
  rsMNU_WINDOW         = '&Window';
    rsWND_DATABASE     = 'Database';
  rsMNU_TOOLS          = '&Tools';
  rsMNU_SAVELOG        = 'Save log';
  rsMNU_STAY_ON_TOP    = 'Window on top';
  rsMNU_HELP           = '&Help';
    rsHLPINDEX         = '&Index';
    rsHLPNEWS          = '&News';
    rsABOUT            = '&About GHTopo';

  //**************************************
  // resourcestring des titres de boites de dialogue
  //**************************************
  // resourcestring des libellés communs
  rsLBL_COMMENTS          = 'Comments';
  // resourcestring de l'Assistant
  rsASSIST_TITLE       = 'New Cave Assistant';
  rsASSIST_BNENTRANCES = '1st Entrance';
  rsASSIST_BNCODES     = '1st Code';
  rsASSIST_BNEXPES     = '1st Trip';
  rsASSIST_BNSERIE     = '1st Serie';
  rsASSIST_BNSAVE      = 'Save ...';
  rsASSIST_BNGENERAL   = 'General';

  rsASSIST_BNCANCEL    = 'Cancel';
  rsASSIST_LBNOMETUDE  = 'Survey name';
  rsASSIST_LBOBSETUDE  = 'Survey comments';
  rsASSIST_LBNOMENTREE = 'Main entrance';

  rsASSIST_LBCOORDS    = 'Coordinates';
  rsASSIST_LBOBSENTREE = 'Observations';
  rsASSIST_LBCONVERTISSEUR = 'Converter ...';
  rsASSIST_LBREFSTATION    = 'Main station';
  rsASSIST_LBREFSERIE      = 'Serie';
  rsASSIST_LBREFPOINT      = 'Station';
  rsASSIST_LBCOMMENTAIRE   = 'Comments';


  rsASSIST_LBSYSTGEO   = 'Geographic coordinates systems';

  rsASSISTANT_SUCCESS = 'Document created successfully by wizard';
  rsASSISTANT_RELOAD  = 'Reloading document';
  rsASSISTANT_ECHEC   = 'Wizard fault';

  // resourcestring du cadre Entrées
  rsCDR_ENTR_NOENTRANCE   = 'Entrance';
  rsCDR_ENTR_ENTRNAME     = 'Entrance Name';
  rsCDR_ENTR_COORDINATES  = 'Entrance coordinates';
  rsCDR_ENTR_STATOFENTR   = 'Station of entrance:';

  // resourcestring du cadre Codes
  rsCDR_CODES_NUMERO      = 'Code:';
  rsCDR_CODES_TYPEGALERIE = 'Conduit Type:';
  rsTITRE_SELECTEUR_VISEE = 'Choose shot type';
    rsCMBTYPE_D          = 'D - Default';
    rsCMBTYPE_E          = 'E - Entrance';
    rsCMBTYPE_B          = 'B - Fossile Gallery';
    rsCMBTYPE_V          = 'V - Free flow';
    rsCMBTYPE_W          = 'W - Sump';
    rsCMBTYPE_C          = 'C - Floodable conduit';
    rsCMBTYPE_F          = 'F - Fixed point';
    rsCMBTYPE_S          = 'S - Surface survey';
    rsCMBTYPE_A          = 'A - Artificial Tunnel';
    rsCMBTYPE_M          = 'M - Mine gallery';
   rsCDR_CODES_VISEE       = 'Shot';
    rsCDR_CODES_VDIRECT   = 'Direct';
    rsCDR_CODES_VINVERSE  = 'Invert';

  rsCDR_CODES_GRADCOMPAS  = 'Compass Graduation:';
  rsCDR_CODES_GRADCLINO   = 'Clinometer Graduation:';
    rsCDR_CODES_CMBUNIT_0 = '400 - Gon';
    rsCDR_CODES_CMBUNIT_1 = '360 - Degrees';
    rsCDR_CODES_CMBUNIT_2 = '370 - Percentages';
    rsCDR_CODES_CMBUNIT_3 = '380 - Slopes';
  rsCDR_CODES_FACT        = 'Lengths x';
  rsCDR_CODES_POSZERO     = 'Zero position:';
    rsCDR_CODES_CMBZERO_0 = 'Nadiral';
    rsCDR_CODES_CMBZERO_1 = 'Horizontal';
    rsCDR_CODES_CMBZERO_2 = 'Zenithal';
  rsCDR_CODES_ANGLIMIT    = 'Limit angle:';
  rsCDR_CODES_PRECISION   = 'Instruments uncertainty:';
  //****************************************************************************
  // resourcestring des onglets du Gestionnaire de Cavité
  rsTBS_GENERAL        = 'General';
  rsTBS_ENTRANCE       = 'Entrances';
  rsTBS_CODES          = 'Codes';
  rsTBS_TRIPS          = 'Trips';
  rsTBS_SERIES         = 'Series';
  rsTBS_RESEAUX        = 'Networks';
  rsTBS_ANTENNES       = 'Antenna shots';
  rsTBS_MAINTENANCE    = 'Maintenance';

  //*******************************************************
  // resourcestrings du cadre Réseaux
  rsCDR_RESEAU_LBIDX   = 'Numero';
  rsCDR_RESEAU_NAME    = 'Name';
  rsCDR_RESEAU_TYPE    = 'Network type';
    rsCDR_RESEAU_CB0     = 'Natural cave';
    rsCDR_RESEAU_CB1     = 'Anthropic cave';
    rsCDR_RESEAU_CB2     = 'Surface surveying';
    rsCDR_RESEAU_CB3     = 'Thalweg';
    rsCDR_RESEAU_CB4     = 'Road';
    rsCDR_RESEAU_CB5     = 'Track';
    rsCDR_RESEAU_CB6     = 'Other';


  //*******************************************************
  // resourcestrings du cadre Expés
  rsCDR_EXPE_SEANCE      = 'Survey session:';
  rsCDR_EXPE_DATE        = 'Date';

  rsCDR_EXPE_DECLIMAG    = 'Declination';
  rsCDR_EXPE_INCLIN      = 'Inclination';
  rsCOLOR                = 'Color';

  rsCDR_EXPE_SPELEOMETRE = 'Speleometer';
  rsCDR_EXPE_SPELEOGRAPHE= 'Speleographist';
  //*********************************************
  // resourcestring du cadre Séries
  rsDLG_PJMNGR_MOVETOSERIE = 'Move to other serie ?';
  rsDLG_PJMNGR_ADDSERIE  = 'Add Serie';
  rsCDR_SERIE_LB_RESEAU  = 'Network';
  rsCDR_SERIE_VALIDATE   = 'Validate';
  rsCDR_SERIE_ADDPHOTO   = 'Add photo';
  rsCDR_SERIE_NUMERO     = 'Serie:';
  rsCDR_SERIE_NAME       = 'Name:';
  rsCDR_SERIE_DEPART     = 'Start:';
  rsCDR_SERIE_ARRIVEE    = 'End:';
  rsCDR_SERIE_CHANCE     = 'Chance:'; // en espagnol: Suerte
    rsCDR_SERIE_CHANCE0  = 'None';
    rsCDR_SERIE_CHANCE1  = 'Low';
    rsCDR_SERIE_CHANCE2  = 'Good';
    rsCDR_SERIE_CHANCE3  = 'Excellent';
  rsCDR_SERIE_OBSTACLE   = 'Obstacle:';
    rsCDR_SERIE_OBSTACLE0= 'None';
    rsCDR_SERIE_OBSTACLE1= 'Pit';
    rsCDR_SERIE_OBSTACLE2= 'Chimney';
    rsCDR_SERIE_OBSTACLE3= 'Squeeze';
    rsCDR_SERIE_OBSTACLE4= 'Lake';
    rsCDR_SERIE_OBSTACLE5= 'Sump';
    rsCDR_SERIE_OBSTACLE6= 'Collapse';
    rsCDR_SERIE_OBSTACLE7= 'Speleothems';
    rsCDR_SERIE_OBSTACLE8= 'Sediments';
    rsCDR_SERIE_OBSTACLE9= 'Other';
    // spécifique à la France lol
    rsCDR_SERIE_OBSTACLE10 = 'Toxic gas';       // aurait dû être ajouté
    rsCDR_SERIE_OBSTACLE11 = 'Aggressives geese';  // référence à une expé franco-chinoise Oie: goose au singulier
    rsCDR_SERIE_OBSTACLE12 = 'Dangerous animals';  // référence à une expé de LH Fage
    rsCDR_SERIE_OBSTACLE13 = 'Gang-bang site';         // pour la topographie de certains sentiers forestiers xD

  rsCDR_SERIE_LOCKED     = 'Locked';
   rsCDR_SERIE_INSERTLINE = 'Insert lines';
   rsCDR_SERIE_NBLINES    = 'Number of lines';
   rsCDR_SERIE_DELETELINE    = 'Remove lines';
   rsCDR_SERIE_UNDOCOPY      = 'Undo copy';
   rsCDR_SERIE_INC_UNDOCOPY  = 'Incremental undocopy';
  rsCDR_SERIE_IMPLEMENT      = 'Validate';
  rsINPUT_COMMENTAIRE_TITRE  = 'Station comments';
  rsINPUT_COMMENTAIRE_MSG    = 'input a text';
  rsCDR_SERIE_PASTE          = 'Paste from clipboard';
  rsCDR_SERIE_BTN_GRD_COPY   = 'Copy grid';

    // resoursestring du cadre Antennes
  rsCDR_ANTENNES_AC_ADDLINE = 'Add line';
  rsCDR_ANTENNES_AC_DELLINE = 'Remove ligne';
  rsCDR_ANTENNES_AC_SAVEGRD = 'Apply modifications';
  rsCDR_ANTENNES_DEL_LINE   = 'Remove line %d';




  //*******************************************************
  // général
  rsSELECTALL          = 'Select All';
  rsDESELECTALL        = 'Deselect All';
  rsFILTERS            = 'Filters:';
  rsCHKSELECTALL       = 'Sel. all';
  rsCHKDESELECTALL     = 'Desel. all';
  rsCHKREVERSE         = 'Rev. sel.';
  rsMAIN_NETWORK       = 'Main network';

  //*********************************
  // resourcestring du dialogue Vtopo
  rsVTOPO_EDPREFIX       = 'Stations prefix:';
  rsVTOPO_LBIDPTDEP      = 'ID start point:';
  rsVTOPO_LBREPORTER     = 'Operator:';
  rsLBFICHIER          = 'File: ';
  rsLBMAINENTRANCE     = 'Main entrance:';
  rsLBIDMAINST         = 'Starting station:';
  rsLBSTPREFIX         = 'Stations prefix';
  rsLBREPORTER         = 'Report:';
  rsLBMAINENTCOORD     = 'Main entrance coordinates';
  rsLBCOLORDEFAULT     = 'Default color';
  rsVTOPOFILTERTRO     = 'Visual Topo files (*.tro)|*.tro|All (*.*)|*.*';
  //*******************************************************
  // resourcestring du visualisateur OpenGL
  rsVUE2D_FMT_INFOS_STATION_RAPIDE = 'Station: %s (%d.%d): X=%.2f Y=%.2f Z=%.2f';
  rsVUE2D_FMT_INFOS_ID_STATION     = '%d.%d [%s]';
  rsVUE2D_FMT_INFOS_MESURES        = 'L = %.2f, Az = %.2f, Inc =%.2f, L = %.2f, R = %.2f, U = %.2f, D = %.2f';
  rsVUE2D_FMT_INFOS_COORDONNEES    = 'X = %s, Y = %s, Z = %s';
  //*******************************************************
  // resourcestring du visualisateur OpenGL
  rsOGLVIEWERTITLE     = 'OpenGL Viewer [%s]';
  rsLBLBACKCOLOR       = 'Background color';
  rsLBLCUBECOLOR       = 'Cube color';
  rsLBLREFCOLOR        = 'Referential color';
  rsOPENGLERROR        = 'OpenGL error';
  rsOGLVQUIT           = 'Quit OpenGL viewer';
  //*******************************************************
  // resourcestrings de l'outil d'exportation graphique
  rsPSDXF_TITLE        = 'Graphical Export: [%s]';
  rsDODISPDESELECTEDPARTS = 'Display parts rejected by MetaFiltre - Color';
  rsTAB_LAYERS            = 'Layers';
  rsTAB_QUADR             = 'Criss-cross';
  rsTAB_DESSIN            = 'Drawing';
  rsTAB_TEXTES            = 'Texts';
  rsTYPEQDR          = 'Criss-cross type';
  rsQDNONE           = 'None';
  rsQDCROSS          = 'Grid';
  rsQDQUADRILLES     = 'Cross';
  rsGRAPHICS_PS        = 'PostScript PS';
  rsGRAPHICS_DXF       = 'AutoCAD DXF';
  rsGRAPHICS_SVG       = 'Scalable Graphics SVG';
  rsGRAPHICS_WMF       = 'Windows MetaFile WMF';

  rsDLGDXF_TITLE       = 'Export DXF: %s';
  rsDLGDXF_MSGENTETE   = 'Unable to use header file'+#13+
                         'Use ''_dxfin'' AutoCAD command'+#13+
                         'Only ENTITIES section will readed';
  // mots clefs du MétaFiltre
  rsHLPMETAFILTRE      = 'METAFILTRE';
  rsMETAFILTRE_NIL     = 'NIL';           // 0
  rsMETAFILTRE_ALL     = 'ALL';           // 1
  rsMETAFILTRE_ID      = 'ID';            // 2
  rsMETAFILTRE_LENGTH  = 'LENGTH';        // 3
  rsMETAFILTRE_AZIMUTH = 'AZIMUTH';       // 4
  rsMETAFILTRE_PENTE   = 'SLOPE';         // 5
  rsMETAFILTRE_DATE    = 'DATE';          // 6
  rsMETAFILTRE_COLOR   = 'COLOR';         // 7
  rsMETAFILTRE_X       = 'X_COORD';       // 8
  rsMETAFILTRE_Y       = 'Y_COORD';       // 9
  rsMETAFILTRE_Z       = 'Z_COORD';       // 10
  rsMETAFILTRE_LARGEUR = 'WIDTH';         // 11
  rsMETAFILTRE_HAUTEUR = 'HEIGHT';        // 12
  rsMETAFILTRE_DATES   = 'DATES';         // 13
  rsMETAFILTRE_COLORS  = 'COLORS';        // 14
  rsMETAFILTRE_SERIE   = 'SERIE';         // 15

  rsMETAFILTRE_RESEAU  = 'NETWORK';        // 16
  rsMETAFILTRE_CODE    = 'CODE';          // 17
  rsMETAFILTRE_EXPE    = 'TRIP';        // 18
  rsMETAFILTRE_TYPEVISEE = 'TYPE_SHOT';  // 19

  rsMETAFILTRE_APPLY     = 'Apply';

  //*******************************************************
  // resourcestring du dialogue du MétaFiltre
  rsDLGMETAFILTRE_TBS1 = 'Dates';
  rsDLGMETAFILTRE_TBS2 = 'Colors';
  rsDLGMETAFILTRE_PERSO= 'Custom';
  //*******************************************************
  // resourcestring du centre d'impression
  rsDLGIMP_TAB1        = 'Preview';
  rsDLGIMP_TAB2        = 'Printer';
  rsDLGIMP_TAB3        = 'Drawing Options';
  rsDLGIMP_TAB4        = 'Networks';

  rsQDRSPACING         = 'Spacing';
  rsECHELLE            = 'Scale: 1 / ';
  rsLAYERS             = 'Drawing layers';
  rsPREVIEW            = 'Preview';
  rsSTARTPRINTING      = 'Start printing';
  rsREGLE              = 'Rule';

  //******************************************************
  // resourcestring du sélecteur de couleurs
  //rsSELECTCOLORTITLE   = 'Selection d''une couleur';
  //rsLBLUSEDCOLORS      = 'Dernières couleurs:';
  //rsDLGCOULSAVEPAL     = 'Enregistrer palette';
  //rsDLGCOULRESTPAL     = 'Restaurer palette';
  //rsDLGCOULFILTERPAL   = 'Fichiers de palette (*.pal)|*.pal|Tous (*.*)|*.*';
  //rsDLGCOUDELPAL       = 'Ecraser le fichier existant';
  //rsPALETTENOTFOUNT    = 'Palette introuvable';


  // resourcestring des utilitaires d'export graphique


  //-----------------------------------
//  rsTABLE_MONTAGE_QUIT = 'Quitter la table de montage';
//  rsFAIL_LOAD_FILE     = 'Echec de chargement de %s';
//  rsDIST_FIRST_POINT   = 'Distance: Première station';
//  rsDIST_SECOND_POINT  = 'Distance: Deuxième station';
//  rsNEW_TRAV_FST_ND    = 'Nouvelle traverse: Premier noeud';
//  rsNEW_TRAV_SEC_ND    = 'Nouvelle traverse: Deuxième noeud';
//  rsDOUBLE_CLICK       = 'Double-Cliquez sur le plan';
//  rsINEXISTANT_LEG     = 'Cheminement inexistant';

//  rsHISTO_FIRST_POINT  = 'Histogramme: Premier coin';
//  rsPAN_FIRST_POINT    = 'Pan: Premier point';

//  rsPAN_SECOND_POINT   = 'Pan: Deuxième point';
// resourcestrings de l'éditeur de cheminements
//  rsDATE               = 'Date';
  (*-- En tête des colonnes du tableur --*)
//  rsCOL_LENGTH         = 'Longueur';
//  rsCOL_BEARING        = 'Azimut';
//  rsCOL_INC            = 'Pente';
//  rsCOL_LG             = 'Gauche';
//  rsCOL_LD             = 'Droite';
//  rsCOL_HZ             = 'Haut';
//  rsCOL_HN             = 'Bas';


  //******************************************************
  // resourcestring de l'utilitaire d'export graphique
  rsDLGGRAPHIC_OUTPUTFMT = 'Output format:';
  rsDLGGRAPHIC_LBFILENAME= 'File name:';
  rsDLG_GRAPHIC_TABTITLE = 'Graphic export';
  rsDLGGRAPHIC_GBCROIX   = 'Criss-cross';
  rsDLGGRAPHIC_GBCHEM    = 'Paths and sections';
  rsDLGGRAPHIC_GBWALLS   = 'Walls and colors';

  rsDLGGRAPHIC_SPACING   = 'Spacing';
  rsDLGGRAPHIC_TYPEGRID  = 'Type:';
  rsDLGGRAPHIC_CMBGRD2   = 'Cross';
  rdDLGGRAPHIC_LAYER     = 'Layer:';
  rdDLGGRAPHIC_WALLFILL  = 'Fill';
  rsDLGGRAPHIC_WFILL1    = 'Solid (1 color)';
  rsDLGGRAPHIC_WFILL2    = 'Conduits types';
  rsDLGGRAPHIC_WFILL3    = 'Shots color';
  rsDLGGRAPHIC_WFILL4    = 'By networks ';
  rsDLGGRAPHIC_WFILL5    = 'Par dates';

  rsDLGGRAPHIC_CHKCHEM   = 'Export paths';
  rsDLGGRAPHIC_CHKSECT   = 'Export sections';

  // resourcestring du centre d'impression
  rsPRN_NOPRINTER        = 'No printer available';
  rsPRN_TBPRINTER        = 'Printer';
  rsPRN_TBDRAW           = 'Drawing';
  rsPRN_TITLE            = 'Printing center [%s]';
  rsPRN_CHKPOLY          = 'Polygonals';
  rsPRN_CHKFILL          = 'Fill';
  rsPRN_CHKWALLS         = 'Walls';
  rsPRN_CHKSECTS         = 'Sections';
  rsPRN_STATIONS         = 'Stations';
  rsPRN_STATIONS_LBL     = 'ID stations';
  rsPRM_LBANGLEROT       = 'Rotation angle';
  rsPRN_TYPEQDR          = 'Criss-cross type';
  rsPRN_QDCROSS          = 'Grid';
  rsPRN_QDQUADRILLES     = 'Cross';
  rs_PRN_SCALING         = 'Scale';
  rsPRN_CHKSTATIONS      = 'Stations';
  rsPRN_CHKSTATIONS_LBL  = 'ID stations';
  rsPRN_CHKCOTATION      = 'Cotation';
  rsPRN_CHKQUADRILLAGE   = 'Quadrillage';
  rsPRN_CHKENTREES       = 'Entrees';
  rsPRN_CHKANNOTATIONS   = 'Annotations';

  rsPRN_LBSPACING        = 'Spacing';
  rsLANDSCAPE            = 'Landscape';
  rsPORTRAIT             = 'Portrait';


  // resourcestring du dialogue HelpSystem
  rsHLP_BNEDIT         = 'Edit help file';
  rsHLP_DOCONTINUE     = 'Want continue ?';

  //-------------------------------------------------
  // resourcestring du dialogue Editeur d'annotations
  rsDLG_ANN_TITLE      = 'Captions Editor';
  rsDLG_ANN_LBTEXTE    = 'Text';
  rsDLG_ANN_LBMAX      = 'Max length';
  rsDLG_ANN_CHKTXTDISP = 'Draw this text';
  rsDLG_ANN_GRBPOSTEXT = 'Text positioning';
    rsDLG_ANN_GRBMETH0 = 'Absolute Coordinates';
    rsDLG_ANN_GRBMETH1 = 'Hooked at station';
  rsDLG_ANN_LBPOSTEXTE   = 'Base point';
  rsDLG_ANN_LBSTATION    = 'Station';
  rsDLG_ANN_LBOFFSET     = 'Offset (m) X =';
  rsDLG_ANN_GRBATTRTXT   = 'Character attributes';
  rsDLG_ANN_GRBBASEPT    = 'Text base point';

  //-------------------------------------------------
  //-------------------------------------------------
  // resourcestring du dialogue Série/station
  rsDLG_SERST_TITLE    = 'Find station';
  rsDLG_SERST_SERIE    = 'Serie:';
  rsDLG_SERST_STATION  = 'Station';
  rsDLG_SERST_CLE      = 'Field code';
  rsDLG_SERST_LBSEARCH = 'Finding';
  rsDLG_SERST_BYSERST  = 'By serie and station';
  rsDLG_SERST_BYREFTER = 'By field reference';


  //-------------------------------------------------
  // recherche de station par ID littéral
  rsDLG_FIND_PT_BY_ID_TITLE  = 'Find by litteral ID';
  rsDLG_FIND_PT_BY_ID_PROMPT = 'Field code';

  // calculette intégrée


  // calculette intégrée
  rsDLG_CALC_TITLE    = 'Calculation utilities';
  rsDLG_CALC_TAB_CAL  = 'Calculator';
    rsDLG_CALC_EXPR     = 'Enter expression';
    rsDLG_CALC_DOCALC   = 'Calculate';

  rsDLG_CALC_CDR_CONVERT  = 'Convertisseur de coordonnées';
  rsDLG_CALC_CDR_DECLIMAG = 'Calcul de déclinaison magnétique';
  rsDLG_CALC_BTN_CONVERT  = 'Convertir';
  rsDLG_CALC_LB_SYST_SOURCE = 'Système source';
  rsDLG_CALC_LB_SYST_CIBLE  = 'Système cible';
  rsDLG_CALC_HINT_GRD_CONVERSIONS = 'Saisir des valeurs dans la grille ou coller depuis le presse-papiers';





  (*-- Messages *)

  rsINSERTITEM         = 'Insert item ?';
  rsDELETEITEM         = 'Delete item ?';


  rsMSG_SEEALSO        = 'Voir aussi ...';
  rsMSG_NDSNEEDED      = 'Nodes file not found - Recompile the network';
  rsMSG_ERASESTATION   = 'Erase station #%d ?';
  rsMSG_FILENOTFOUND   = 'File %s not found';
  rsMSG_READY          = 'READY';
  rsMSG_NOFILEOPENED   = 'No file opened'; // Pas de fichier ouvert
  rsDISPLAY_HELP_SYSTEM= 'Starting GHTopo help system';
  rsHLPCENTER_TITLE    = 'GHTopo help system';
  rsMATCHNOTFOUND      = 'Match not found';
  rsNOCANCLOSE         = 'Quit and save ?';
  rsWARN_FILEALREADYOPEN= 'Document already opened - Continue ?';
  rsSAVECHANGES        = 'Save changes';
  rsNOCANCLOSEWND      = 'Unable to close this window';
  rsERASEEXISTINGFILE  = 'Erase existing file';
  rsERASEEXISTNAMEDFILE = 'Erase the file %s ?';

  rsSAVESLOST          = 'Changes will be lost';
  rsCFGMTFLTR_UNABLE   = 'Error in file of personalized filters';
  rsERRORSTARTPRNCTR   = 'Error starting printing center';
  rsNOPRINTERINSTALLED = 'No printer installed';
  rsERRORLOADINGTOP    = 'Error loading TOP file';
  rsIDSERIE_EXISTS     = 'Serie index %d already attributed (serie #%d)';

  rsMSG_VUE3D_FAIL     = 'Failed to starting 3D viewer';
  rsMSG_VUE2D_FAIL     = 'Failed to starting 2D map viewer';
  rsMSG_STATS_DLG_FAIL = 'Failed to starting statistics dialog';
  rsMSG_PRINT_CENTER_FAIL  = 'Failed to starting printing center';
  rsMSG_PROJ_MANAGER_FAIL  = 'Failed to starting database frontal';

  rsMSG_PROJ4S_FAIL    = 'Failed to starting Proj4s coordinates converter';
  rsMSG_DECLIMAG_FAIL  = 'Failed to starting IGRF Magnetic Declinations calculator';

  rsMSG_TABLE_ENTITY_FAIL  = 'Error loading entities table';
  //-----
  rsDLGSTATS_RESEAUX = 'Networks';
  rsDLGSTATS_DATES   = 'Dates';
  rsDLGSTATS_COLORS  = 'Colours';

  rsTHERION          = 'Therion';
implementation

end.

