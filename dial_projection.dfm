object DlgProjection: TDlgProjection
  Left = 301
  Top = 170
  Width = 328
  Height = 346
  HorzScrollBar.Range = 313
  VertScrollBar.Range = 301
  ActiveControl = OKBtn
  BorderStyle = bsDialog
  Caption = 'Param'#232'tres de la projection'
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
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 8
    Width = 305
    Height = 257
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
    Width = 32
    Height = 13
    Caption = 'Na&ture'
  end
  object Label4: TLabel
    Left = 16
    Top = 116
    Width = 31
    Height = 13
    Caption = 'Datu&m'
  end
  object Label5: TLabel
    Left = 16
    Top = 144
    Width = 47
    Height = 13
    Caption = '&Propri'#233't'#233's'
    FocusControl = GridParametre
  end
  object OKBtn: TButton
    Left = 79
    Top = 276
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 5
    OnClick = OKBtnClick
  end
  object CancelBtn: TButton
    Left = 159
    Top = 276
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Annuler'
    ModalResult = 2
    TabOrder = 6
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
  end
  object ComboNature: TComboBox
    Left = 88
    Top = 80
    Width = 217
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 2
    OnChange = ComboNatureChange
  end
  object ComboDatum: TComboBox
    Left = 88
    Top = 112
    Width = 140
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 3
  end
  object GridParametre: TStringGrid
    Left = 88
    Top = 144
    Width = 200
    Height = 108
    ColCount = 2
    DefaultColWidth = 87
    DefaultRowHeight = 20
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing]
    ScrollBars = ssVertical
    TabOrder = 4
  end
end
