object DlgParametre: TDlgParametre
  Left = 416
  Top = 242
  Width = 305
  Height = 328
  HorzScrollBar.Range = 289
  VertScrollBar.Range = 285
  ActiveControl = OKBtn
  BorderStyle = bsDialog
  Caption = 'Param'#232'tres'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = 11
  Font.Name = 'MS Sans Serif'
  Font.Pitch = fpVariable
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object OKBtn: TButton
    Left = 111
    Top = 260
    Width = 75
    Height = 25
    Caption = 'Fermer'
    Default = True
    ModalResult = 1
    TabOrder = 4
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 289
    Height = 193
    ActivePage = TabSheet1
    MultiLine = True
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Ellipso'#239'des'
      object ListEllips: TListBox
        Left = 16
        Top = 8
        Width = 121
        Height = 129
        ItemHeight = 13
        TabOrder = 0
        OnDblClick = ButtonAjoutEllipsClick
      end
      object ButtonAjoutEllips: TButton
        Left = 152
        Top = 16
        Width = 75
        Height = 25
        Caption = 'Ajouter ...'
        TabOrder = 1
        OnClick = ButtonAjoutEllipsClick
      end
      object Button1: TButton
        Left = 152
        Top = 48
        Width = 75
        Height = 25
        Caption = 'Modifier ...'
        TabOrder = 2
        OnClick = ButtonAjoutEllipsClick
      end
      object Button2: TButton
        Left = 152
        Top = 88
        Width = 75
        Height = 25
        Caption = 'Supprimer ...'
        TabOrder = 3
        OnClick = Button2Click
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Datums'
      ImageIndex = 1
      object ListDatum: TListBox
        Left = 16
        Top = 8
        Width = 121
        Height = 129
        ItemHeight = 13
        TabOrder = 0
        OnDblClick = ButtonAjoutDatumClick
      end
      object ButtonAjoutDatum: TButton
        Left = 152
        Top = 16
        Width = 75
        Height = 25
        Caption = 'Ajouter ...'
        TabOrder = 1
        OnClick = ButtonAjoutDatumClick
      end
      object Button4: TButton
        Left = 152
        Top = 48
        Width = 75
        Height = 25
        Caption = 'Modifier ...'
        TabOrder = 2
        OnClick = ButtonAjoutDatumClick
      end
      object Button5: TButton
        Left = 152
        Top = 88
        Width = 75
        Height = 25
        Caption = 'Supprimer ...'
        TabOrder = 3
        OnClick = Button5Click
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Projections simples'
      ImageIndex = 2
      object ListSimple: TListBox
        Left = 16
        Top = 8
        Width = 121
        Height = 129
        ItemHeight = 13
        TabOrder = 0
        OnDblClick = ButtonAjoutProjectionClick
      end
      object ButtonAjoutProjection: TButton
        Left = 152
        Top = 16
        Width = 75
        Height = 25
        Caption = 'Ajouter ...'
        TabOrder = 1
        OnClick = ButtonAjoutProjectionClick
      end
      object Button7: TButton
        Left = 152
        Top = 48
        Width = 75
        Height = 25
        Caption = 'Modifier ...'
        TabOrder = 2
        OnClick = ButtonAjoutProjectionClick
      end
      object Button8: TButton
        Left = 152
        Top = 88
        Width = 75
        Height = 25
        Caption = 'Supprimer ...'
        TabOrder = 3
        OnClick = Button8Click
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Projections multiples'
      ImageIndex = 3
      object ListMultiple: TListBox
        Left = 16
        Top = 8
        Width = 121
        Height = 129
        ItemHeight = 13
        TabOrder = 0
        OnDblClick = ButtonAjoutMultipleClick
      end
      object ButtonAjoutMultiple: TButton
        Left = 152
        Top = 16
        Width = 75
        Height = 25
        Caption = 'Ajouter ...'
        TabOrder = 1
        OnClick = ButtonAjoutMultipleClick
      end
      object Button10: TButton
        Left = 152
        Top = 48
        Width = 75
        Height = 25
        Caption = 'Modifier ...'
        TabOrder = 2
        OnClick = ButtonAjoutMultipleClick
      end
      object Button11: TButton
        Left = 152
        Top = 88
        Width = 75
        Height = 25
        Caption = 'Supprimer ...'
        TabOrder = 3
        OnClick = Button11Click
      end
    end
  end
  object CheckAutomatique: TCheckBox
    Left = 8
    Top = 228
    Width = 265
    Height = 30
    Caption = 'Chargement/sauvegarde automatiques'
    TabOrder = 3
    OnClick = CheckAutomatiqueClick
  end
  object ButtonOuvrir: TButton
    Left = 24
    Top = 200
    Width = 89
    Height = 25
    Caption = 'Ouvrir ...'
    TabOrder = 1
    OnClick = ButtonOuvrirClick
  end
  object ButtonEnregistrer: TButton
    Left = 128
    Top = 200
    Width = 89
    Height = 25
    Caption = 'Enregistrer ...'
    TabOrder = 2
    OnClick = ButtonEnregistrerClick
  end
  object OpenDialog: TOpenDialog
    DefaultExt = '*.txt'
    FileName = '*.txt'
    Filter = 'Fichiers texte (*.txt)'
    Title = 'Ouvrir'
    Left = 24
    Top = 256
  end
  object SaveDialog: TSaveDialog
    DefaultExt = '*.txt'
    FileName = '*.txt'
    Filter = 'Fichiers texte (*.txt)'
    Title = 'Enregistrer sous'
    Left = 64
    Top = 256
  end
end
