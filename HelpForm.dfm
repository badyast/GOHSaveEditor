object FrmHelp: TFrmHelp
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Hilfe'
  ClientHeight = 380
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
  object PnlMain: TPanel
    Left = 0
    Top = 0
    Width = 500
    Height = 335
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object LblTitle: TLabel
      Left = 16
      Top = 16
      Width = 27
      Height = 21
      Caption = 'Hilfe'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object LblStatus: TLabel
      Left = 16
      Top = 48
      Width = 456
      Height = 45
      AutoSize = False
      Caption =
        'Dieses Tool befindet sich in aktiver Entwicklung. Fehler oder I' +
        'nkompatibilit'#228'ten k'#246'nnen auftreten. Die Nutzung geschieht auf eig' +
        'ene Verantwortung.'
      WordWrap = True
    end
    object LblIssues: TLabel
      Left = 16
      Top = 104
      Width = 456
      Height = 30
      AutoSize = False
      Caption =
        'Fehler, Verbesserungsvorschl'#228'ge oder Fragen? Bitte erstelle ein' +
        ' Issue auf GitHub:'
      WordWrap = True
    end
    object LblIssuesLink: TLabel
      Left = 16
      Top = 136
      Width = 321
      Height = 15
      Cursor = crHandPoint
      Caption = 'GitHub Issues: github.com/badyast/GOHSaveEditor/issues'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      OnClick = LblIssuesLinkClick
      OnMouseEnter = LblIssuesLinkMouseEnter
      OnMouseLeave = LblIssuesLinkMouseLeave
    end
    object MemoDisclaimer: TMemo
      Left = 16
      Top = 168
      Width = 468
      Height = 153
      TabStop = False
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
      Font.Height = -11
      Font.Name = 'Segoe UI'
      Font.Style = []
      Lines.Strings = (

          'GOH Savegame Editor ist ein inoffizielles Fan-Projekt und steht' +
          ' in keiner '

          'Verbindung zu Digitalmindsoft/Barbedwire Studios. Alle Rechte a' +
          'n Gates of Hell: '
        'Ostfront liegen bei deren jeweiligen Rechteinhabern.')
      ParentFont = False
      ReadOnly = True
      TabOrder = 0
    end
  end
  object BtnClose: TButton
    Left = 409
    Top = 345
    Width = 75
    Height = 25
    Caption = 'Schlie'#223'en'
    TabOrder = 1
    OnClick = BtnCloseClick
  end
end
