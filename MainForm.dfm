object FrmMain: TFrmMain
  Left = 0
  Top = 0
  Caption = 'GOH Savegame Editor'
  ClientHeight = 670
  ClientWidth = 860
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Menu = MainMenu1
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  TextHeight = 15
  object PanelTop: TPanel
    Left = 0
    Top = 0
    Width = 860
    Height = 45
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object LblSave: TLabel
      Left = 136
      Top = 15
      Width = 51
      Height = 15
      Caption = '<-----'
    end
    object BtnOpen: TButton
      Left = 8
      Top = 10
      Width = 120
      Height = 25
      Caption = 'Save laden'#8230
      TabOrder = 0
      OnClick = BtnOpenClick
    end
    object ChkOnlyHumans: TCheckBox
      Left = 440
      Top = 14
      Width = 200
      Height = 17
      Anchors = [akTop]
      Caption = 'Nur Menschen verschieben'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = ChkOnlyHumansClick
    end
  end
  object PanelBottom: TPanel
    Left = 0
    Top = 365
    Width = 860
    Height = 93
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object LblStatus: TLabel
      Left = 8
      Top = 67
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
    object LblBaseInfo: TLabel
      Left = 8
      Top = 8
      Width = 124
      Height = 15
      Caption = 'Quelle: (keine Auswahl)'
    end
    object LblTargetInfo: TLabel
      Left = 440
      Top = 8
      Width = 109
      Height = 15
      Anchors = [akTop]
      Caption = 'Ziel: (keine Auswahl)'
    end
    object BtnTransfer: TButton
      Left = 8
      Top = 32
      Width = 140
      Height = 25
      Caption = '(Transfer deaktiviert)'
      Enabled = False
      TabOrder = 0
      Visible = False
      OnClick = BtnTransferClick
    end
    object BtnSwap: TButton
      Left = 156
      Top = 32
      Width = 140
      Height = 25
      Caption = 'Units tauschen '#8596
      TabOrder = 1
      OnClick = BtnSwapClick
    end
    object BtnSaveAs: TButton
      Left = 440
      Top = 32
      Width = 140
      Height = 25
      Anchors = [akTop]
      Caption = 'Speichern als'#8230
      TabOrder = 2
      OnClick = BtnSaveAsClick
    end
    object BtnExportCsv: TButton
      Left = 588
      Top = 32
      Width = 140
      Height = 25
      Anchors = [akTop]
      Caption = 'CSV exportieren'#8230
      TabOrder = 3
      OnClick = BtnExportCsvClick
    end
    object ProgressBar1: TProgressBar
      Left = 300
      Top = 64
      Width = 400
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 4
      Visible = False
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 458
    Width = 860
    Height = 212
    ActivePage = TabGeneral
    Align = alBottom
    TabOrder = 2
    object TabGeneral: TTabSheet
      Caption = 'Allgemein'
      object MemoInfo: TMemo
        Left = 0
        Top = 0
        Width = 852
        Height = 182
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
        Height = 182
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
  object PanelClient: TPanel
    Left = 0
    Top = 45
    Width = 860
    Height = 320
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 3
    object Splitter1: TSplitter
      Left = 421
      Top = 0
      Width = 8
      Height = 320
      Beveled = True
    end
    object PanelLeft: TPanel
      Left = 0
      Top = 0
      Width = 421
      Height = 320
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      object BtnExpandBase: TSpeedButton
        Left = 370
        Top = 2
        Width = 23
        Height = 22
        Hint = 'Alles ausklappen'
        Anchors = [akTop, akRight]
        Caption = '+'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
        ShowHint = True
        OnClick = BtnExpandBaseClick
      end
      object BtnCollapseBase: TSpeedButton
        Left = 393
        Top = 2
        Width = 23
        Height = 22
        Hint = 'Alles einklappen'
        Anchors = [akTop, akRight]
        Caption = '-'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
        ShowHint = True
        OnClick = BtnCollapseBaseClick
      end
      object TreeBase: TTreeView
        Left = 8
        Top = 26
        Width = 408
        Height = 286
        Anchors = [akLeft, akTop, akRight, akBottom]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Segoe UI'
        Font.Style = []
        HideSelection = False
        Indent = 19
        ParentFont = False
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        OnChange = TreeBaseChange
      end
    end
    object PanelRight: TPanel
      Left = 429
      Top = 0
      Width = 431
      Height = 320
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object BtnExpandTarget: TSpeedButton
        Left = 380
        Top = 2
        Width = 23
        Height = 22
        Hint = 'Alles ausklappen'
        Anchors = [akTop, akRight]
        Caption = '+'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
        ShowHint = True
        OnClick = BtnExpandTargetClick
      end
      object BtnCollapseTarget: TSpeedButton
        Left = 403
        Top = 2
        Width = 23
        Height = 22
        Hint = 'Alles einklappen'
        Anchors = [akTop, akRight]
        Caption = '-'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
        ShowHint = True
        OnClick = BtnCollapseTargetClick
      end
      object TreeTarget: TTreeView
        Left = 8
        Top = 26
        Width = 418
        Height = 286
        Anchors = [akLeft, akTop, akRight, akBottom]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Segoe UI'
        Font.Style = []
        HideSelection = False
        Indent = 19
        ParentFont = False
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        OnChange = TreeTargetChange
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
      object BackupOptionen1: TMenuItem
        Caption = 'Backup-Optionen...'
        OnClick = BackupOptionen1Click
      end
      object Sprache1: TMenuItem
        Caption = 'Sprache'
        object Deutsch1: TMenuItem
          Caption = 'Deutsch'
          Hint = 'de'
          OnClick = LanguageClick
        end
        object English1: TMenuItem
          Caption = 'English'
          Hint = 'en'
          OnClick = LanguageClick
        end
      end
      object Design1: TMenuItem
        Caption = 'Design'
        object Hell1: TMenuItem
          Caption = 'Hell'
          Hint = 'light'
          OnClick = ThemeClick
        end
        object Dunkel1: TMenuItem
          Caption = 'Dunkel'
          Hint = 'dark'
          OnClick = ThemeClick
        end
      end
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
