object FrmPresetSave: TFrmPresetSave
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Vorlage speichern'
  ClientHeight = 275
  ClientWidth = 400
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poMainFormCenter
  OnCreate = FormCreate
  TextHeight = 15
  object LblName: TLabel
    Left = 16
    Top = 16
    Width = 80
    Height = 15
    Caption = 'Vorlagenname:'
  end
  object LblGroup: TLabel
    Left = 16
    Top = 70
    Width = 45
    Height = 15
    Caption = 'Gruppe:'
  end
  object LblInfo: TLabel
    Left = 16
    Top = 124
    Width = 368
    Height = 105
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGray
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object EdtName: TEdit
    Left = 16
    Top = 37
    Width = 368
    Height = 23
    TabOrder = 0
    OnChange = EdtNameChange
  end
  object CmbGroup: TComboBox
    Left = 16
    Top = 91
    Width = 260
    Height = 23
    Style = csDropDownList
    TabOrder = 1
    OnChange = CmbGroupChange
  end
  object BtnOK: TButton
    Left = 228
    Top = 240
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 2
    OnClick = BtnOKClick
  end
  object BtnCancel: TButton
    Left = 309
    Top = 240
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Abbrechen'
    TabOrder = 3
    OnClick = BtnCancelClick
  end
end
