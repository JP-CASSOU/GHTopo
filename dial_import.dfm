object DlgImportation: TDlgImportation
  Left = 310
  Top = 220
  Width = 328
  Height = 261
  HorzScrollBar.Range = 313
  VertScrollBar.Range = 213
  ActiveControl = OKBtn
  BorderStyle = bsDialog
  Caption = 'Importation'
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
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 40
    Width = 305
    Height = 137
    Shape = bsFrame
  end
  object OKBtn: TButton
    Left = 79
    Top = 188
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 4
  end
  object CancelBtn: TButton
    Left = 159
    Top = 188
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Annuler'
    ModalResult = 2
    TabOrder = 5
  end
  object CheckTout: TCheckBox
    Left = 8
    Top = 8
    Width = 161
    Height = 25
    Caption = '&Tout importrer'
    Checked = True
    State = cbChecked
    TabOrder = 0
    OnClick = CheckToutClick
  end
  object CheckListBoxImport: TCheckListBox
    Left = 16
    Top = 48
    Width = 209
    Height = 121
    ItemHeight = 13
    TabOrder = 1
  end
  object ButtonTout: TButton
    Left = 232
    Top = 80
    Width = 75
    Height = 25
    Hint = 'Tout s'#233'lectionner'
    Caption = 'Tout'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    OnClick = ButtonToutClick
  end
  object ButtonAucun: TButton
    Left = 232
    Top = 112
    Width = 75
    Height = 25
    Hint = 'Tout d'#233's'#233'lectionner'
    Caption = 'Aucun'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
    OnClick = ButtonToutClick
  end
end
