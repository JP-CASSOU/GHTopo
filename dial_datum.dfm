object DlgDatum: TDlgDatum
  Left = 352
  Top = 255
  Width = 334
  Height = 347
  HorzScrollBar.Range = 313
  VertScrollBar.Range = 301
  ActiveControl = OKBtn
  BorderStyle = bsDialog
  Caption = 'Param'#232'tres du datum'
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
  object Bevel1: TBevel
    Left = 8
    Top = 8
    Width = 305
    Height = 265
    Shape = bsFrame
  end
  object Label1: TLabel
    Left = 16
    Top = 20
    Width = 22
    Height = 13
    Caption = '&Nom'
    FocusControl = EditNom
  end
  object Label2: TLabel
    Left = 16
    Top = 52
    Width = 53
    Height = 13
    Caption = '&Description'
    FocusControl = EditDescription
  end
  object Label3: TLabel
    Left = 16
    Top = 84
    Width = 46
    Height = 13
    Caption = '&Ellipso'#239'de'
  end
  object Label4: TLabel
    Left = 16
    Top = 124
    Width = 69
    Height = 13
    Caption = 'Translation (m)'
  end
  object Label5: TLabel
    Left = 112
    Top = 104
    Width = 32
    Height = 13
    Alignment = taCenter
    Caption = 'Delta&X'
    FocusControl = EditDeltaX
  end
  object Label6: TLabel
    Left = 184
    Top = 104
    Width = 32
    Height = 13
    Alignment = taCenter
    Caption = 'Delta&Y'
    FocusControl = EditDeltaY
  end
  object Label7: TLabel
    Left = 256
    Top = 104
    Width = 32
    Height = 13
    Alignment = taCenter
    Caption = 'Delta&Z'
    FocusControl = EditDeltaZ
  end
  object Label8: TLabel
    Left = 14
    Top = 188
    Width = 81
    Height = 13
    Caption = 'Rotation(second)'
  end
  object Label9: TLabel
    Left = 112
    Top = 168
    Width = 13
    Height = 13
    Alignment = taCenter
    Caption = 'Rx'
    FocusControl = EditRotX
  end
  object Label10: TLabel
    Left = 184
    Top = 168
    Width = 13
    Height = 13
    Alignment = taCenter
    Caption = 'Ry'
    FocusControl = EditRotY
  end
  object Label11: TLabel
    Left = 256
    Top = 168
    Width = 13
    Height = 13
    Alignment = taCenter
    Caption = 'Rz'
    FocusControl = EditRotZ
  end
  object Label12: TLabel
    Left = 16
    Top = 216
    Width = 64
    Height = 13
    Caption = 'E&chelle (ppm)'
    FocusControl = EditEchelle
  end
  object Label13: TLabel
    Left = 16
    Top = 244
    Width = 23
    Height = 13
    Caption = '&Grille'
    FocusControl = EditGrille
  end
  object OKBtn: TButton
    Left = 79
    Top = 276
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 13
  end
  object CancelBtn: TButton
    Left = 159
    Top = 276
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Annuler'
    ModalResult = 2
    TabOrder = 14
  end
  object EditNom: TEdit
    Left = 88
    Top = 16
    Width = 100
    Height = 21
    TabOrder = 0
    OnEnter = EditNomEnter
    OnExit = EditNomExit
  end
  object EditDescription: TEdit
    Left = 88
    Top = 48
    Width = 217
    Height = 21
    TabOrder = 1
    OnExit = EditDescriptionExit
  end
  object ComboEllipsoide: TComboBox
    Left = 88
    Top = 80
    Width = 140
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 2
    OnChange = ComboEllipsoideChange
  end
  object EditDeltaX: TEdit
    Left = 104
    Top = 120
    Width = 57
    Height = 21
    TabOrder = 3
    Text = '0'
    OnEnter = EditDeltaXEnter
    OnExit = EditDeltaXExit
  end
  object EditDeltaY: TEdit
    Left = 176
    Top = 120
    Width = 57
    Height = 21
    TabOrder = 4
    Text = '0'
    OnEnter = EditDeltaYEnter
    OnExit = EditDeltaYExit
  end
  object EditDeltaZ: TEdit
    Left = 248
    Top = 120
    Width = 57
    Height = 21
    TabOrder = 5
    Text = '0'
    OnEnter = EditDeltaZEnter
    OnExit = EditDeltaZExit
  end
  object CheckSept: TCheckBox
    Left = 24
    Top = 144
    Width = 249
    Height = 25
    Caption = 'Transformation '#224' 7 param'#232'tres'
    TabOrder = 6
    OnClick = CheckSeptClick
  end
  object EditRotX: TEdit
    Left = 104
    Top = 184
    Width = 57
    Height = 21
    TabOrder = 8
    Text = '0'
    OnEnter = EditRotXEnter
    OnExit = EditRotXExit
  end
  object EditRotY: TEdit
    Left = 176
    Top = 184
    Width = 57
    Height = 21
    TabOrder = 9
    Text = '0'
    OnEnter = EditRotYEnter
    OnExit = EditRotYExit
  end
  object EditRotZ: TEdit
    Left = 248
    Top = 184
    Width = 57
    Height = 21
    TabOrder = 10
    Text = '0'
    OnEnter = EditRotZEnter
    OnExit = EditRotZExit
  end
  object EditEchelle: TEdit
    Left = 104
    Top = 212
    Width = 57
    Height = 21
    TabOrder = 7
    Text = '0'
    OnEnter = EditEchelleEnter
    OnExit = EditEchelleExit
  end
  object EditGrille: TEdit
    Left = 88
    Top = 240
    Width = 185
    Height = 21
    TabOrder = 11
    OnExit = EditGrilleExit
  end
  object ButtonGrille: TButton
    Left = 284
    Top = 240
    Width = 21
    Height = 21
    Caption = '...'
    TabOrder = 12
    OnClick = ButtonGrilleClick
  end
  object OpenDialogGrille: TOpenDialog
    DefaultExt = '*.txt'
    FileName = 'gr3d*.txt'
    Filter = 'Fichier texte (*.txt)|Fichier mnt (*.mnt)'
    Title = 'Enregistrer sous'
    Left = 256
    Top = 272
  end
end
