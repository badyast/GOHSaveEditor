object FrmMain: TFrmMain
  Caption = 'GOH Savegame Editor'
  ClientHeight = 420
  ClientWidth = 860
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  object BtnOpen: TButton
    Left = 16
    Top = 16
    Width = 120
    Height = 25
    Caption = 'Save laden…'
    OnClick = BtnOpenClick
  end
  object LblSave: TLabel
    Left = 152
    Top = 20
    Width = 500
    Height = 17
    Caption = '(kein File)'
  end
  object TreeBase: TTreeView
    Left = 16
    Top = 56
    Width = 400
    Height = 280
    Indent = 19
    ReadOnly = True
    HideSelection = False
    RowSelect = True
    OnChange = TreeBaseChange
  end
  object TreeTarget: TTreeView
    Left = 444
    Top = 56
    Width = 400
    Height = 280
    Indent = 19
    ReadOnly = True
    HideSelection = False
    RowSelect = True
    OnChange = TreeTargetChange
  end
  object LblBaseInfo: TLabel
    Left = 16
    Top = 344
    Width = 400
    Height = 17
    Caption = 'Quelle: (keine Auswahl)'
  end
  object LblTargetInfo: TLabel
    Left = 444
    Top = 344
    Width = 400
    Height = 17
    Caption = 'Ziel: (keine Auswahl)'
  end
  object BtnTransfer: TButton
    Left = 16
    Top = 376
    Width = 140
    Height = 25
    Caption = 'Unit übertragen →'
    OnClick = BtnTransferClick
  end
  object BtnSwap: TButton
    Left = 168
    Top = 376
    Width = 140
    Height = 25
    Caption = 'Units tauschen ↔'
    OnClick = BtnSwapClick
  end
  object BtnSaveAs: TButton
    Left = 444
    Top = 376
    Width = 140
    Height = 25
    Caption = 'Speichern als…'
    OnClick = BtnSaveAsClick
  end
  object BtnExportCsv: TButton
    Left = 604
    Top = 376
    Width = 140
    Height = 25
    Caption = 'CSV exportieren…'
    OnClick = BtnExportCsvClick
  end
  object OpenDialog1: TOpenDialog
    Options = [ofFileMustExist, ofEnableSizing]
  end
  object SaveDialog1: TSaveDialog
    Options = [ofOverWritePrompt, ofEnableSizing]
  end
end
