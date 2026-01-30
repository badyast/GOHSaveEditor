object FrmPresetLoad: TFrmPresetLoad
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Vorlage laden'
  ClientHeight = 350
  ClientWidth = 500
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poMainFormCenter
  OnCreate = FormCreate
  TextHeight = 15
  object PanelLeft: TPanel
    Left = 0
    Top = 0
    Width = 180
    Height = 300
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    object ListBoxPresets: TListBox
      Left = 8
      Top = 8
      Width = 164
      Height = 284
      ItemHeight = 15
      TabOrder = 0
      OnClick = ListBoxPresetsClick
      OnDblClick = ListBoxPresetsDblClick
    end
  end
  object PanelRight: TPanel
    Left = 180
    Top = 0
    Width = 320
    Height = 300
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object LblPresetName: TLabel
      Left = 16
      Top = 8
      Width = 290
      Height = 20
      AutoSize = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object LblCreated: TLabel
      Left = 16
      Top = 32
      Width = 290
      Height = 15
      AutoSize = False
    end
    object LblGroupInfo: TLabel
      Left = 16
      Top = 50
      Width = 290
      Height = 15
      AutoSize = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object MemoPreview: TMemo
      Left = 16
      Top = 75
      Width = 290
      Height = 215
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 0
    end
  end
  object PanelBottom: TPanel
    Left = 0
    Top = 300
    Width = 500
    Height = 50
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object BtnLoad: TButton
      Left = 254
      Top = 12
      Width = 75
      Height = 25
      Caption = 'Laden'
      Default = True
      Enabled = False
      TabOrder = 0
      OnClick = BtnLoadClick
    end
    object BtnDelete: TButton
      Left = 16
      Top = 12
      Width = 90
      Height = 25
      Caption = 'L'#246'schen'
      Enabled = False
      TabOrder = 1
      OnClick = BtnDeleteClick
    end
    object BtnCancel: TButton
      Left = 409
      Top = 12
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Abbrechen'
      TabOrder = 2
      OnClick = BtnCancelClick
    end
  end
end
