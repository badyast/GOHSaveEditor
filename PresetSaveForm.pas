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
    InAllGroups: Boolean; // Teil der Vorlage "Alle Gruppen" (Gruppen 1-5)
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
    // True = alle Gruppen 1-5 speichern
    function AllGroupsSelected: Boolean;
    // Nur gueltig, wenn AllGroupsSelected = False
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
  Lang: TLanguageStrings;
begin
  Lang := GetLanguageStrings(Settings.Language);
  FAvailableGroups := Groups;
  CmbGroup.Items.Clear;
  // Index 0 = alle Gruppen, danach die einzelnen Gruppen
  CmbGroup.Items.Add(Lang.PresetAllGroups);
  for I := 0 to Length(Groups) - 1 do
    CmbGroup.Items.Add(Groups[I].DisplayName);
  CmbGroup.ItemIndex := 0;
  UpdateInfo;
end;

procedure TFrmPresetSave.UpdateInfo;
var
  I, Total: Integer;
  Lines: TStringList;
  Lang: TLanguageStrings;
begin
  Lang := GetLanguageStrings(Settings.Language);

  if AllGroupsSelected then
  begin
    Lines := TStringList.Create;
    try
      Total := 0;
      for I := 0 to Length(FAvailableGroups) - 1 do
        if FAvailableGroups[I].InAllGroups then
        begin
          Lines.Add(Format(Lang.PresetGroupLine,
            [FAvailableGroups[I].DisplayName, FAvailableGroups[I].SquadCount]));
          Inc(Total, FAvailableGroups[I].SquadCount);
        end;
      Lines.Add('');
      Lines.Add(Format(Lang.PresetSaveInfo, [Total]));
      LblInfo.Caption := TrimRight(Lines.Text);
    finally
      Lines.Free;
    end;
  end
  else if (CmbGroup.ItemIndex > 0) and (CmbGroup.ItemIndex <= Length(FAvailableGroups)) then
    LblInfo.Caption := Format(Lang.PresetSaveInfo,
      [FAvailableGroups[CmbGroup.ItemIndex - 1].SquadCount])
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

function TFrmPresetSave.AllGroupsSelected: Boolean;
begin
  Result := CmbGroup.ItemIndex = 0;
end;

function TFrmPresetSave.GetSelectedStageFilter: string;
begin
  if (CmbGroup.ItemIndex > 0) and (CmbGroup.ItemIndex <= Length(FAvailableGroups)) then
    Result := FAvailableGroups[CmbGroup.ItemIndex - 1].StageFilter
  else
    Result := '';
end;

end.
