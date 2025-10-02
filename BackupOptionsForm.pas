unit BackupOptionsForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  AppSettings;

type
  TFrmBackupOptions = class(TForm)
    LblBackupFolder: TLabel;
    EdtBackupFolder: TEdit;
    BtnBrowse: TButton;
    LblMaxBackups: TLabel;
    EdtMaxBackups: TEdit;
    ChkUnlimited: TCheckBox;
    BtnOK: TButton;
    BtnCancel: TButton;
    Panel1: TPanel;
    LblInfo: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure BtnBrowseClick(Sender: TObject);
    procedure ChkUnlimitedClick(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
  private
    procedure LoadSettings;
    procedure SaveSettings;
  public
  end;

var
  FrmBackupOptions: TFrmBackupOptions;

implementation

uses
  Vcl.FileCtrl;

{$R *.dfm}

procedure TFrmBackupOptions.FormCreate(Sender: TObject);
begin
  LoadSettings;
end;

procedure TFrmBackupOptions.LoadSettings;
begin
  EdtBackupFolder.Text := Settings.BackupFolder;

  if Settings.MaxBackupCount <= 0 then
  begin
    ChkUnlimited.Checked := True;
    EdtMaxBackups.Text := '10';
    EdtMaxBackups.Enabled := False;
  end
  else
  begin
    ChkUnlimited.Checked := False;
    EdtMaxBackups.Text := IntToStr(Settings.MaxBackupCount);
    EdtMaxBackups.Enabled := True;
  end;
end;

procedure TFrmBackupOptions.SaveSettings;
var
  MaxCount: Integer;
begin
  Settings.BackupFolder := EdtBackupFolder.Text;

  if ChkUnlimited.Checked then
    Settings.MaxBackupCount := -1
  else
  begin
    if TryStrToInt(EdtMaxBackups.Text, MaxCount) and (MaxCount > 0) then
      Settings.MaxBackupCount := MaxCount
    else
    begin
      MessageDlg('Bitte geben Sie eine gültige Zahl größer als 0 ein.', mtError, [mbOK], 0);
      Exit;
    end;
  end;

  Settings.Save;
end;

procedure TFrmBackupOptions.BtnBrowseClick(Sender: TObject);
var
  Dir: string;
begin
  Dir := EdtBackupFolder.Text;
  if SelectDirectory('Backup-Ordner wählen', '', Dir, [sdNewUI, sdNewFolder]) then
    EdtBackupFolder.Text := Dir;
end;

procedure TFrmBackupOptions.ChkUnlimitedClick(Sender: TObject);
begin
  EdtMaxBackups.Enabled := not ChkUnlimited.Checked;
end;

procedure TFrmBackupOptions.BtnOKClick(Sender: TObject);
begin
  SaveSettings;
  ModalResult := mrOK;
end;

procedure TFrmBackupOptions.BtnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

end.
