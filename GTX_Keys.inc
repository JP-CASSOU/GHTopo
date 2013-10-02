// Fichier de déclaration des clés pour le format GHTopo XML GTX
const
  GTX_KEY_SECTION_GENERAL     = 'General';
  GTX_KEY_SECTION_ENTRANCES    = 'Entrances';
    GTX_KEY_ENTRANCE             = 'Entrance';
      GTX_ATTR_ENTRANCE_IDX      = 'Numero';
      GTX_ATTR_ENTRANCE_NAME     = 'Name';
      GTX_ATTR_ENTRANCE_REFSERIE = 'RefSerie';
      GTX_ATTR_ENTRANCE_REFPT    = 'RefPoint';
      GTX_ATTR_ENTRANCE_X        = 'X';
      GTX_ATTR_ENTRANCE_Y        = 'Y';
      GTX_ATTR_ENTRANCE_Z        = 'Z';
      GTX_ATTR_ENTRANCE_OBS      = 'Comments';

  GTX_KEY_SECTION_RESEAUX     = 'Networks';
    GTX_KEY_RESEAU            = 'Network';
      GTX_ATTR_RESEAU_IDX     = 'Numero';
      GTX_ATTR_RESEAU_COLOR   = 'Color';
      GTX_ATTR_RESEAU_TYPE    = 'Type';
      GTX_ATTR_RESEAU_NAME    = 'Name';
      GTX_ATTR_RESEAU_OBS     = 'Comments';

  GTX_KEY_SECTION_CODES       = 'Codes';
    GTX_KEY_CODE              = 'Code';
      // <Code PsiL="0.000" PsiP="0.000" Type="0" PsiAz="0.000" Numero="0" Comments="Item pour entrees et points fixes" FactLong="1.000" ClinoUnit="400.00" AngleLimite="0.00" CompassUnit="400.00"/>
      GTX_ATTR_CODE_NUMERO    = 'Numero';
      GTX_ATTR_CODE_UCLINO    = 'ClinoUnit';
      GTX_ATTR_CODE_UCOMPASS  = 'CompassUnit';
      GTX_ATTR_CODE_FACT_LONG = 'FactLong';
      GTX_ATTR_CODE_TYPE      = 'Type';
      GTX_ATTR_CODE_ANGLIMITE = 'AngleLimite';
      GTX_ATTR_CODE_PSI_L     = 'PsiL';
      GTX_ATTR_CODE_PSI_A     = 'PsiAz';
      GTX_ATTR_CODE_PSI_P     = 'PsiP';
      GTX_ATTR_CODE_OBS       = 'Comments';

  GTX_KEY_SECTION_EXPES       = 'Seances';
    GTX_KEY_EXPE              = 'Trip';
      //<Trip Date="0000-01-01" Color="0" Numero="0" Comments="Item pour entrees et points fixes" Surveyor1="" Surveyor2="" Declination="0.0000" Inclination="0.0000" ModeDeclination="0"/>

      GTX_ATTR_EXPE_NUMERO    = 'Numero';
      GTX_ATTR_EXPE_DATE      = 'Date';
      GTX_ATTR_EXPE_IDXCOLOR  = 'Color';
      GTX_ATTR_EXPE_SURVEY1   = 'Surveyor1';
      GTX_ATTR_EXPE_SURVEY2   = 'Surveyor2';
      GTX_ATTR_EXPE_DECLINAT  = 'Declination';
      GTX_ATTR_EXPE_INCLINAT  = 'Inclination';
      GTX_ATTR_EXPE_MODEDECL  = 'ModeDeclination';
      GTX_ATTR_EXPE_OBS       = 'Comments';

  GTX_KEY_SECTION_SERIES      = 'Series';
    GTX_KEY_SERIE             = 'Serie';
      // <Serie ="Galerie  vers 1/9" Color="#00000000" ="9" ="7" ="3" Numero="4" ="1" ="1" ="0" ="0" ="">
      GTX_ATTR_SERIE_Numero   = 'Numero';
      GTX_ATTR_SERIE_NAME     = 'Name';
      GTX_ATTR_SERIE_SERDEP   = 'SerDep';
      GTX_ATTR_SERIE_PTDEP    = 'PtDep';
      GTX_ATTR_SERIE_SERARR   = 'SerArr';
      GTX_ATTR_SERIE_PTARR    = 'PtArr';
      GTX_ATTR_SERIE_RESEAU   = 'Network';
      GTX_ATTR_SERIE_OBSTACLE = 'Obstacle';
      GTX_ATTR_SERIE_CHANCE   = 'Chance';
      GTX_ATTR_SERIE_RAIDEUR  = 'Raideur';
      GTX_ATTR_SERIE_OBS      = 'Commments';
      GTX_ATTR_SERIE_COLOR    = 'Color';
      GTX_KEY_STATIONS        = 'Stations';
        // <Shot Az="398.00" ID="3.5" Up="2.00" Code="2" Down="1.00" Expe="1" Incl="1.00" Left="1.00" Label="" Right="1.00" Length="10.000" Comments="" TypeShot="0"/>
        GTX_KEY_VISEE         = 'Shot';
          GTX_ATTR_VISEE_ID   = 'ID';
          GTX_ATTR_VISEE_LBL  = 'Label';
          GTX_ATTR_VISEE_TYPE = 'TypeShot';
          GTX_ATTR_VISEE_CODE = 'Code';
          GTX_ATTR_VISEE_EXPE = GTX_KEY_EXPE;
          GTX_ATTR_VISEE_LONG = 'Length';
          GTX_ATTR_VISEE_AZ   = 'Az';
          GTX_ATTR_VISEE_P    = 'Incl';
          GTX_ATTR_VISEE_LG   = 'Left';
          GTX_ATTR_VISEE_LD   = 'Right';
          GTX_ATTR_VISEE_HZ   = 'Up';
          GTX_ATTR_VISEE_HN   = 'Down';
          GTX_ATTR_VISEE_OBS  = 'Comments';

  GTX_KEY_SECTION_ANTENNAS    = 'AntennaShots';
    GTX_KEY_ANTENNA_SHOT      = 'AntennaShot';
    GTX_KEY_ANTENNA_NUMERO    = 'Numero';
    GTX_KEY_ANTENNA_NETWORK   = 'Network';
    GTX_KEY_ANTENNA_LABEL     = 'Label';
    GTX_KEY_ANTENNA_SERIE     = 'SerDep';
    GTX_KEY_ANTENNA_POINT     = 'PtDep';
    GTX_KEY_ANTENNA_CODE      = 'Code';
    GTX_KEY_ANTENNA_TRIP      = 'Trip';
    GTX_KEY_ANTENNA_LONG      = 'Length';
    GTX_KEY_ANTENNA_AZIMUT    = 'Az';
    GTX_KEY_ANTENNA_PENTE     = 'Incl';
    GTX_KEY_ANTENNA_OBS       = 'Comments';
    //<AntennaShot ="0.00" Code="1" ="1" ="0.00" ="" ="0" ="0.001" ="0" ="1" ="0" =""/>







