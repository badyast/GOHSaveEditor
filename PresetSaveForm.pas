unit PresetSaveForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  AppSettings, AppLanguage;

type
  TGroupInfo = record
    StageFilter: string;
    DisplayName: string;
    SquadCount: Integer;
  end;

  TFrmPresetSave = class(TForm)
    LblName: TLabel;
    EdtName: TEdit;
    LblGroup: TLabel;
    CmbGroup: TComboBox;
    LblInfo: TLabel;
    BtnOK: TButton;
    BtnCancel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure EdtNameChange(Sender: TObject);
    procedure CmbGroupChange(Sender: TObject);
  private
    FAvailableGroups: TArray<TGroupInfo>;
    procedure ApplyLanguage;
    procedure UpdateInfo;
  public
    procedure SetAvailableGroups(const Groups: TArray<TGroupInfo>);
    function GetPresetName: string;
    function GetSelectedStageFilter: string;
  end;

var
  FrmPresetSave: TFrmPresetSave;

implementation

{$R *.dfm}

procedure TFrmPresetSave.FormCreate(Sender: TObject);
begin
  ApplyLanguage;
  BtnOK.Enabled := False;
end;

procedure TFrmPresetSave.ApplyLanguage;
var
  Lang: TLanguageStrings;
begin
  Lang := GetLanguageStrings(Settings.Language);
  Caption := Lang.PresetSaveCaption;
  LblName.Caption := Lang.PresetNameLabel;
  LblGroup.Caption := Lang.PresetGroupLabel;
  BtnOK.Caption := Lang.BtnOK;
  BtnCancel.Caption := Lang.BtnCancel;
end;

procedure TFrmPresetSave.SetAvailableGroups(const Groups: TArray<TGroupInfo>);
var
  I: Integer;
begin
  FAvailableGroups := Groups;
  CmbGroup.Items.Clear;
  for I := 0 to Length(Groups) - 1 do
    CmbGroup.Items.Add(Groups[I].DisplayName);
  if CmbGroup.Items.Count > 0 then
    CmbGroup.ItemIndex := 0;
  UpdateInfo;
end;

procedure TFrmPresetSave.UpdateInfo;
var
  Lang: TLanguageStrings;
begin
  Lang := GetLanguageStrings(Settings.Language);
  if (CmbGroup.ItemIndex >= 0) and (CmbGroup.ItemIndex < Length(FAvailableGroups)) then
    LblInfo.Caption := Format(Lang.PresetSaveInfo, [FAvailableGroups[CmbGroup.ItemIndex].SquadCount])
  else
    LblInfo.Caption := '';
end;

procedure TFrmPresetSave.CmbGroupChange(Sender: TObject);
begin
  UpdateInfo;
  EdtNameChange(Sender);
end;

procedure TFrmPresetSave.EdtNameChange(Sender: TObject);
begin
  BtnOK.Enabled := (Trim(EdtName.Text) <> '') and (CmbGroup.ItemIndex >= 0);
end;

procedure TFrmPresetSave.BtnOKClick(Sender: TObject);
begin
  if Trim(EdtName.Text) = '' then
  begin
    EdtName.SetFocus;
    Exit;
  end;
  ModalResult := mrOK;
end;

procedure TFrmPresetSave.BtnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

function TFrmPresetSave.GetPresetName: string;
begin
  Result := Trim(EdtName.Text);
end;

function TFrmPresetSave.GetSelectedStageFilter: string;
begin
  if (CmbGroup.ItemIndex >= 0) and (CmbGroup.ItemIndex < Length(FAvailableGroups)) then
    Result := FAvailableGroups[CmbGroup.ItemIndex].StageFilter
  else
    Result := '';
end;

end.
