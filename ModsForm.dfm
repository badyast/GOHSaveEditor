object FrmMods: TFrmMods
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Aktive Mods bearbeiten'
  ClientHeight = 320
  ClientWidth = 460
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poMainFormCenter
  TextHeight = 15
  object LblInfo: TLabel
    Left = 12
    Top = 10
    Width = 436
    Height = 28
    AutoSize = False
    Caption = 'Liste der aktiven Steam-Workshop-Mods. Format: mod_XXXXXXXX:0'
    WordWrap = True
  end
  object ListBoxMods: TListBox
    Left = 12
    Top = 44
    Width = 336
    Height = 224
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnClick = ListBoxModsClick
  end
  object BtnAdd: TButton
    Left = 360
    Top = 44
    Width = 88
    Height = 28
    Caption = 'Hinzuf'#252'gen...'
    TabOrder = 1
    OnClick = BtnAddClick
  end
  object BtnRemove: TButton
    Left = 360
    Top = 80
    Width = 88
    Height = 28
    Caption = 'Entfernen'
    Enabled = False
    TabOrder = 2
    OnClick = BtnRemoveClick
  end
  object PanelBottom: TPanel
    Left = 0
    Top = 272
    Width = 460
    Height = 48
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 3
    object BtnOK: TButton
      Left = 284
      Top = 10
      Width = 80
      Height = 28
      Caption = 'OK'
      Default = True
      TabOrder = 0
      OnClick = BtnOKClick
    end
    object BtnCancel: TButton
      Left = 372
      Top = 10
      Width = 80
      Height = 28
      Cancel = True
      Caption = 'Abbrechen'
      TabOrder = 1
      OnClick = BtnCancelClick
    end
  end
end
