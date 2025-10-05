unit HelpForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Winapi.ShellAPI, AppSettings, AppLanguage;

type
  TFrmHelp = class(TForm)
    PnlMain: TPanel;
    LblTitle: TLabel;
    LblStatus: TLabel;
    LblIssues: TLabel;
    LblIssuesLink: TLabel;
    BtnClose: TButton;
    MemoDisclaimer: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure LblIssuesLinkClick(Sender: TObject);
    procedure BtnCloseClick(Sender: TObject);
    procedure LblIssuesLinkMouseEnter(Sender: TObject);
    procedure LblIssuesLinkMouseLeave(Sender: TObject);
  private
    procedure ApplyLanguage;
  public
  end;

var
  FrmHelp: TFrmHelp;

implementation

{$R *.dfm}

procedure TFrmHelp.FormCreate(Sender: TObject);
begin
  ApplyLanguage;
end;

procedure TFrmHelp.ApplyLanguage;
var
  Lang: TLanguageStrings;
begin
  Lang := GetLanguageStrings(Settings.Language);

  Caption := Lang.HelpCaption;
  LblTitle.Caption := Lang.HelpTitle;
  LblStatus.Caption := Lang.HelpStatus;
  LblIssues.Caption := Lang.HelpIssues;
  LblIssuesLink.Caption := 'GitHub Issues: github.com/badyast/GOHSaveEditor/issues';
  MemoDisclaimer.Lines.Clear;
  MemoDisclaimer.Lines.Add(Lang.HelpDisclaimer);
  BtnClose.Caption := Lang.BtnClose;
end;

procedure TFrmHelp.LblIssuesLinkClick(Sender: TObject);
begin
  ShellExecute(0, 'open', 'https://github.com/badyast/GOHSaveEditor/issues', nil, nil, SW_SHOWNORMAL);
end;

procedure TFrmHelp.LblIssuesLinkMouseEnter(Sender: TObject);
begin
  LblIssuesLink.Font.Style := [fsUnderline];
  Screen.Cursor := crHandPoint;
end;

procedure TFrmHelp.LblIssuesLinkMouseLeave(Sender: TObject);
begin
  LblIssuesLink.Font.Style := [];
  Screen.Cursor := crDefault;
end;

procedure TFrmHelp.BtnCloseClick(Sender: TObject);
begin
  Close;
end;

end.
