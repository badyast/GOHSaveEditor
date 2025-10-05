unit AboutForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Winapi.ShellAPI, AppSettings, AppLanguage;

type
  TFrmAbout = class(TForm)
    PnlMain: TPanel;
    LblTitle: TLabel;
    LblVersion: TLabel;
    LblAuthor: TLabel;
    LblGitHub: TLabel;
    LblCoffee: TLabel;
    BtnClose: TButton;
    LblLicense: TLabel;
    LblDevelopment: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure LblGitHubClick(Sender: TObject);
    procedure LblCoffeeClick(Sender: TObject);
    procedure BtnCloseClick(Sender: TObject);
    procedure LblGitHubMouseEnter(Sender: TObject);
    procedure LblGitHubMouseLeave(Sender: TObject);
    procedure LblCoffeeMouseEnter(Sender: TObject);
    procedure LblCoffeeMouseLeave(Sender: TObject);
  private
    procedure ApplyLanguage;
  public
  end;

var
  FrmAbout: TFrmAbout;

implementation

{$R *.dfm}

procedure TFrmAbout.FormCreate(Sender: TObject);
begin
  ApplyLanguage;
end;

procedure TFrmAbout.ApplyLanguage;
var
  Lang: TLanguageStrings;
begin
  Lang := GetLanguageStrings(Settings.Language);

  Caption := Lang.AboutCaption;
  LblTitle.Caption := 'GOH Savegame Editor';
  LblVersion.Caption := Lang.AboutVersion + ' 0.9.0 Beta';
  LblAuthor.Caption := Lang.AboutAuthor + ' badyast';
  LblGitHub.Caption := 'GitHub: github.com/badyast/GOHSaveEditor';
  LblCoffee.Caption := Lang.AboutCoffee;
  LblLicense.Caption := Lang.AboutLicense;
  LblDevelopment.Caption := Lang.AboutDevelopment;
  BtnClose.Caption := Lang.BtnClose;
end;

procedure TFrmAbout.LblGitHubClick(Sender: TObject);
begin
  ShellExecute(0, 'open', 'https://github.com/badyast/GOHSaveEditor', nil, nil, SW_SHOWNORMAL);
end;

procedure TFrmAbout.LblCoffeeClick(Sender: TObject);
begin
  ShellExecute(0, 'open', 'https://www.buymeacoffee.com/badyast', nil, nil, SW_SHOWNORMAL);
end;

procedure TFrmAbout.LblGitHubMouseEnter(Sender: TObject);
begin
  LblGitHub.Font.Style := [fsUnderline];
  Screen.Cursor := crHandPoint;
end;

procedure TFrmAbout.LblGitHubMouseLeave(Sender: TObject);
begin
  LblGitHub.Font.Style := [];
  Screen.Cursor := crDefault;
end;

procedure TFrmAbout.LblCoffeeMouseEnter(Sender: TObject);
begin
  LblCoffee.Font.Style := [fsUnderline];
  Screen.Cursor := crHandPoint;
end;

procedure TFrmAbout.LblCoffeeMouseLeave(Sender: TObject);
begin
  LblCoffee.Font.Style := [];
  Screen.Cursor := crDefault;
end;

procedure TFrmAbout.BtnCloseClick(Sender: TObject);
begin
  Close;
end;

end.
