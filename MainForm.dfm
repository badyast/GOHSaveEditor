object FrmMain: TFrmMain
  Left = 0
  Top = 0
  Caption = 'GOH Savegame Editor'
  ClientHeight = 640
  ClientWidth = 860
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Menu = MainMenu1
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
  object LblStatus: TLabel
    Left = 16
    Top = 410
    Width = 72
    Height = 15
    Caption = 'Lade Daten...'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
    Visible = False
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
  object ProgressBar1: TProgressBar
    Left = 344
    Top = 407
    Width = 400
    Height = 17
    TabOrder = 8
    Visible = False
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 448
    Width = 860
    Height = 192
    ActivePage = TabGeneral
    Align = alBottom
    TabOrder = 9
    object TabGeneral: TTabSheet
      Caption = 'Allgemein'
      object MemoInfo: TMemo
        Left = 0
        Top = 0
        Width = 852
        Height = 162
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Consolas'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
    object TabInventory: TTabSheet
      Caption = 'Inventar'
      ImageIndex = 1
      object ListViewInventory: TListView
        Left = 0
        Top = 0
        Width = 852
        Height = 162
        Align = alClient
        Columns = <
          item
            Caption = 'Item'
            Width = 250
          end
          item
            Caption = 'Typ'
            Width = 150
          end
          item
            Caption = 'Anzahl'
            Width = 80
          end
          item
            Caption = 'Position'
            Width = 100
          end
          item
            Caption = 'Anmerkung'
            Width = 150
          end>
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Segoe UI'
        Font.Style = []
        GridLines = True
        ReadOnly = True
        RowSelect = True
        ParentFont = False
        TabOrder = 0
        ViewStyle = vsReport
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Options = [ofFileMustExist, ofEnableSizing]
  end
  object SaveDialog1: TSaveDialog
    Options = [ofOverwritePrompt, ofEnableSizing]
  end
  object MainMenu1: TMainMenu
    Left = 816
    Top = 65520
    object Optionen1: TMenuItem
      Caption = 'Optionen'
      object Debuglevel: TMenuItem
        Caption = 'Debuglevel'
        object Off1: TMenuItem
          Caption = 'Off'
          Hint = 'Off'
          OnClick = DebugLevelClick
        end
        object Emergency1: TMenuItem
          Tag = 1
          Caption = 'Emergency'
          Hint = 'Emergency'
          OnClick = DebugLevelClick
        end
        object Alert1: TMenuItem
          Tag = 2
          Caption = 'Alert'
          Hint = 'Alert'
        end
        object Critical1: TMenuItem
          Tag = 3
          Caption = 'Critical'
          Hint = 'Critical'
          OnClick = DebugLevelClick
        end
        object Error1: TMenuItem
          Tag = 4
          Caption = 'Error'
          Hint = 'Error'
          OnClick = DebugLevelClick
        end
        object Warning1: TMenuItem
          Tag = 5
          Caption = 'Warning'
          Hint = 'Warning'
          OnClick = DebugLevelClick
        end
        object Notice1: TMenuItem
          Tag = 6
          Caption = 'Notice'
          Hint = 'Notice'
          OnClick = DebugLevelClick
        end
        object Info1: TMenuItem
          Tag = 7
          Caption = 'Info'
          Hint = 'Info'
          OnClick = DebugLevelClick
        end
        object Debug1: TMenuItem
          Tag = 8
          Caption = 'Debug'
          Checked = True
          Hint = 'Debug'
          OnClick = DebugLevelClick
        end
      end
    end
  end
end
