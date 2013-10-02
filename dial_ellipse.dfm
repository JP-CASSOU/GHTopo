object DlgEllipsoide: TDlgEllipsoide
  Left = 382
  Top = 272
  Width = 330
  Height = 285
  HorzScrollBar.Range = 313
  VertScrollBar.Range = 237
  ActiveControl = OKBtn
  BorderStyle = bsDialog
  Caption = 'Param'#232'tres de l'#39'ellipso'#239'de'
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
    Height = 201
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
    Width = 23
    Height = 13
    Caption = '&a (m)'
    FocusControl = EditA
  end
  object Label4: TLabel
    Left = 16
    Top = 116
    Width = 23
    Height = 13
    Caption = '&b (m)'
    FocusControl = EditB
  end
  object Label5: TLabel
    Left = 16
    Top = 148
    Width = 6
    Height = 13
    Caption = '&e'
    FocusControl = EditE
  end
  object Label6: TLabel
    Left = 16
    Top = 180
    Width = 14
    Height = 13
    Caption = '1/&f'
    FocusControl = EditF
  end
  object OKBtn: TButton
    Left = 79
    Top = 212
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 6
  end
  object CancelBtn: TButton
    Left = 159
    Top = 212
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Annuler'
    ModalResult = 2
    TabOrder = 7
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
  object EditA: TEdit
    Left = 88
    Top = 80
    Width = 100
    Height = 21
    TabOrder = 2
    OnEnter = EditAEnter
    OnExit = EditAExit
  end
  object EditB: TEdit
    Left = 88
    Top = 112
    Width = 100
    Height = 21
    TabOrder = 3
    OnEnter = EditBEnter
    OnExit = EditBExit
  end
  object EditE: TEdit
    Left = 88
    Top = 144
    Width = 100
    Height = 21
    TabOrder = 4
    OnEnter = EditEEnter
    OnExit = EditEExit
  end
  object EditF: TEdit
    Left = 88
    Top = 176
    Width = 100
    Height = 21
    TabOrder = 5
    OnEnter = EditFEnter
    OnExit = EditFExit
  end
end
