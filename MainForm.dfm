object FrmMain: TFrmMain
  Left = 0
  Top = 0
  Caption = 'GOH Savegame Editor'
  ClientHeight = 420
  ClientWidth = 860
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object LblSave: TLabel
    Left = 152
    Top = 20
    Width = 51
    Height = 15
    Caption = '(kein File)'
  end
  object LblBaseInfo: TLabel
    Left = 16
    Top = 344
    Width = 124
    Height = 15
    Caption = 'Quelle: (keine Auswahl)'
  end
  object LblTargetInfo: TLabel
    Left = 444
    Top = 344
    Width = 109
    Height = 15
    Caption = 'Ziel: (keine Auswahl)'
  end
  object BtnOpen: TButton
    Left = 16
    Top = 16
    Width = 120
    Height = 25
    Caption = 'Save laden'#8230
    TabOrder = 0
    OnClick = BtnOpenClick
  end
  object TreeBase: TTreeView
    Left = 16
    Top = 56
    Width = 400
    Height = 280
    HideSelection = False
    Indent = 19
    ReadOnly = True
    RowSelect = True
    TabOrder = 1
    OnChange = TreeBaseChange
  end
  object TreeTarget: TTreeView
    Left = 452
    Top = 56
    Width = 400
    Height = 282
    HideSelection = False
    Indent = 19
    ReadOnly = True
    RowSelect = True
    TabOrder = 2
    OnChange = TreeTargetChange
  end
  object BtnTransfer: TButton
    Left = 16
    Top = 376
    Width = 140
    Height = 25
    Caption = '(Transfer deaktiviert)'
    Enabled = False
    TabOrder = 3
    Visible = False
    OnClick = BtnTransferClick
  end
  object BtnSwap: TButton
    Left = 168
    Top = 376
    Width = 140
    Height = 25
    Caption = 'Units tauschen '#8596
    TabOrder = 4
    OnClick = BtnSwapClick
  end
  object BtnSaveAs: TButton
    Left = 444
    Top = 376
    Width = 140
    Height = 25
    Caption = 'Speichern als'#8230
    TabOrder = 5
    OnClick = BtnSaveAsClick
  end
  object BtnExportCsv: TButton
    Left = 604
    Top = 376
    Width = 140
    Height = 25
    Caption = 'CSV exportieren'#8230
    TabOrder = 6
    OnClick = BtnExportCsvClick
  end
  object ChkOnlyHumans: TCheckBox
    Left = 452
    Top = 20
    Width = 200
    Height = 17
    Caption = 'Nur Menschen verschieben'
    Checked = True
    State = cbChecked
    TabOrder = 7
    OnClick = ChkOnlyHumansClick
  end
  object OpenDialog1: TOpenDialog
    Options = [ofFileMustExist, ofEnableSizing]
  end
  object SaveDialog1: TSaveDialog
    Options = [ofOverwritePrompt, ofEnableSizing]
  end
end
