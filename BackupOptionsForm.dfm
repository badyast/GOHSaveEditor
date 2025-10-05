object FrmBackupOptions: TFrmBackupOptions
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Backup-Optionen'
  ClientHeight = 250
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
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 500
    Height = 180
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object LblBackupFolder: TLabel
      Left = 16
      Top = 16
      Width = 80
      Height = 15
      Caption = 'Backup-Ordner:'
    end
    object LblMaxBackups: TLabel
      Left = 16
      Top = 80
      Width = 130
      Height = 15
      Caption = 'Maximale Anzahl Backups:'
    end
    object LblInfo: TLabel
      Left = 16
      Top = 140
      Width = 460
      Height = 30
      AutoSize = False
      Caption =
        'Backups werden automatisch beim '#220'berschreiben eines Saves erst' +
        'ellt. '#196'ltere Backups werden gel'#246'scht, wenn die maximale Anzahl ' +
        #252'berschritten wird.'
      WordWrap = True
    end
    object EdtBackupFolder: TEdit
      Left = 16
      Top = 37
      Width = 365
      Height = 23
      TabOrder = 0
    end
    object BtnBrowse: TButton
      Left = 387
      Top = 35
      Width = 95
      Height = 25
      Caption = 'Durchsuchen...'
      TabOrder = 1
      OnClick = BtnBrowseClick
    end
    object BtnOpenFolder: TButton
      Left = 387
      Top = 66
      Width = 95
      Height = 25
      Caption = 'Ordner '#246'ffnen'
      TabOrder = 4
      OnClick = BtnOpenFolderClick
    end
    object EdtMaxBackups: TEdit
      Left = 16
      Top = 101
      Width = 80
      Height = 23
      TabOrder = 2
      Text = '10'
    end
    object ChkUnlimited: TCheckBox
      Left = 112
      Top = 103
      Width = 180
      Height = 17
      Caption = 'Unbegrenzt (alle behalten)'
      TabOrder = 3
      OnClick = ChkUnlimitedClick
    end
  end
  object BtnOK: TButton
    Left = 327
    Top = 205
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = BtnOKClick
  end
  object BtnCancel: TButton
    Left = 408
    Top = 205
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Abbrechen'
    TabOrder = 2
    OnClick = BtnCancelClick
  end
end
