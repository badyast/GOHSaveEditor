object FrmAbout: TFrmAbout
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = #220'ber'
  ClientHeight = 300
  ClientWidth = 450
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poMainFormCenter
  OnCreate = FormCreate
  TextHeight = 15
  object PnlMain: TPanel
    Left = 0
    Top = 0
    Width = 450
    Height = 255
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object LblTitle: TLabel
      Left = 16
      Top = 16
      Width = 179
      Height = 21
      Caption = 'GOH Savegame Editor'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object LblVersion: TLabel
      Left = 16
      Top = 48
      Width = 96
      Height = 15
      Caption = 'Version: 0.9.0 Beta'
    end
    object LblAuthor: TLabel
      Left = 16
      Top = 80
      Width = 83
      Height = 15
      Caption = 'Autor: badyast'
    end
    object LblDevelopment: TLabel
      Left = 16
      Top = 208
      Width = 418
      Height = 35
      AutoSize = False
      Caption =
        'Entwicklung mit Unterst'#252'tzung von Claude (Anthropic) - einem KI' +
        '-Assistenten f'#252'r Code und Design.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
      Font.Height = -11
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      WordWrap = True
    end
    object LblGitHub: TLabel
      Left = 16
      Top = 112
      Width = 249
      Height = 15
      Cursor = crHandPoint
      Caption = 'GitHub: github.com/badyast/GOHSaveEditor'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      OnClick = LblGitHubClick
      OnMouseEnter = LblGitHubMouseEnter
      OnMouseLeave = LblGitHubMouseLeave
    end
    object LblCoffee: TLabel
      Left = 16
      Top = 144
      Width = 160
      Height = 15
      Cursor = crHandPoint
      Caption = #9749' Buy me a Coffee (freiwillig)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clMaroon
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      OnClick = LblCoffeeClick
      OnMouseEnter = LblCoffeeMouseEnter
      OnMouseLeave = LblCoffeeMouseLeave
    end
    object LblLicense: TLabel
      Left = 16
      Top = 176
      Width = 150
      Height = 15
      Caption = 'Lizenz: MIT - Hobby-Projekt'
    end
  end
  object BtnClose: TButton
    Left = 360
    Top = 265
    Width = 75
    Height = 25
    Caption = 'Schlie'#223'en'
    TabOrder = 1
    OnClick = BtnCloseClick
  end
end
