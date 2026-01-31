unit PresetLoadForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.Menus, System.IOUtils, System.DateUtils,
  AppSettings, AppLanguage, SquadPresets;

type
  TFrmPresetLoad = class(TForm)
    PanelLeft: TPanel;
    ListBoxPresets: TListBox;
    PanelRight: TPanel;
    LblPresetName: TLabel;
    LblCreated: TLabel;
    LblGroupInfo: TLabel;
    MemoPreview: TMemo;
    PanelBottom: TPanel;
    BtnLoad: TButton;
    BtnDelete: TButton;
    BtnCancel: TButton;
    PopupMenuPreset: TPopupMenu;
    MenuItemRename: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure ListBoxPresetsClick(Sender: TObject);
    procedure ListBoxPresetsDblClick(Sender: TObject);
    procedure BtnLoadClick(Sender: TObject);
    procedure BtnDeleteClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure MenuItemRenameClick(Sender: TObject);
  private
    FPresetFiles: TArray<string>;
    FSelectedPreset: TSquadPreset;
    FHasValidSelection: Boolean;
    procedure ApplyLanguage;
    procedure LoadPresetList;
    procedure UpdatePreview;
    function StageFilterToDisplayName(const StageFilter: string): string;
  public
    function GetSelectedPreset: TSquadPreset;
    function HasValidSelection: Boolean;
  end;

var
  FrmPresetLoad: TFrmPresetLoad;

implementation

{$R *.dfm}

procedure TFrmPresetLoad.FormCreate(Sender: TObject);
begin
  FHasValidSelection := False;
  ApplyLanguage;
  LoadPresetList;
end;

procedure TFrmPresetLoad.ApplyLanguage;
var
  Lang: TLanguageStrings;
begin
  Lang := GetLanguageStrings(Settings.Language);
  Caption := Lang.PresetLoadCaption;
  BtnLoad.Caption := Lang.PresetLoadButton;
  BtnDelete.Caption := Lang.PresetDeleteButton;
  BtnCancel.Caption := Lang.BtnCancel;
  MenuItemRename.Caption := Lang.PresetRenameButton;
end;

function TFrmPresetLoad.StageFilterToDisplayName(const StageFilter: string): string;
var
  Lang: TLanguageStrings;
begin
  Lang := GetLanguageStrings(Settings.Language);
  if StageFilter = 'stage_1' then
    Result := Lang.GroupPrefix + ' 1'
  else if StageFilter = 'stage_2' then
    Result := Lang.GroupPrefix + ' 2'
  else if StageFilter = 'stage_3' then
    Result := Lang.GroupPrefix + ' 3'
  else if StageFilter = 'stage_4' then
    Result := Lang.GroupPrefix + ' 4'
  else if StageFilter = 'stage_5' then
    Result := Lang.GroupPrefix + ' 5'
  else if StageFilter = 'stage_special' then
    Result := Lang.GroupSpecial
  else
    Result := StageFilter;
end;

procedure TFrmPresetLoad.LoadPresetList;
var
  I: Integer;
  Preset: TSquadPreset;
  DisplayName: string;
begin
  ListBoxPresets.Items.Clear;
  FPresetFiles := GetPresetManager.GetPresetList;

  for I := 0 to Length(FPresetFiles) - 1 do
  begin
    try
      Preset := GetPresetManager.LoadPreset(FPresetFiles[I]);
      DisplayName := Preset.Name;
    except
      DisplayName := TPath.GetFileNameWithoutExtension(FPresetFiles[I]) + ' (!)';
    end;
    ListBoxPresets.Items.Add(DisplayName);
  end;

  BtnLoad.Enabled := False;
  BtnDelete.Enabled := False;
  MemoPreview.Clear;
  LblPresetName.Caption := '';
  LblCreated.Caption := '';
  LblGroupInfo.Caption := '';
end;

procedure TFrmPresetLoad.ListBoxPresetsClick(Sender: TObject);
begin
  UpdatePreview;
end;

procedure TFrmPresetLoad.ListBoxPresetsDblClick(Sender: TObject);
begin
  if FHasValidSelection then
    BtnLoadClick(Sender);
end;

procedure TFrmPresetLoad.UpdatePreview;
var
  Index: Integer;
  I: Integer;
  Lang: TLanguageStrings;
begin
  Lang := GetLanguageStrings(Settings.Language);
  Index := ListBoxPresets.ItemIndex;

  if (Index < 0) or (Index >= Length(FPresetFiles)) then
  begin
    BtnLoad.Enabled := False;
    BtnDelete.Enabled := False;
    MemoPreview.Clear;
    LblPresetName.Caption := '';
    LblCreated.Caption := '';
    LblGroupInfo.Caption := '';
    FHasValidSelection := False;
    Exit;
  end;

  BtnDelete.Enabled := True;

  try
    FSelectedPreset := GetPresetManager.LoadPreset(FPresetFiles[Index]);

    LblPresetName.Caption := FSelectedPreset.Name;
    LblCreated.Caption := Lang.PresetCreatedLabel + ' ' +
      FormatDateTime('dd.mm.yyyy hh:nn', FSelectedPreset.Created);
    LblGroupInfo.Caption := Format(Lang.PresetGroupInfo,
      [StageFilterToDisplayName(FSelectedPreset.StageFilter)]);

    MemoPreview.Clear;
    MemoPreview.Lines.Add(Format(Lang.PresetSquadCountLabel, [Length(FSelectedPreset.SquadNames)]));
    MemoPreview.Lines.Add('');
    for I := 0 to Length(FSelectedPreset.SquadNames) - 1 do
      MemoPreview.Lines.Add('- ' + FSelectedPreset.SquadNames[I]);

    BtnLoad.Enabled := True;
    FHasValidSelection := True;

  except
    on E: Exception do
    begin
      MemoPreview.Clear;
      MemoPreview.Lines.Add(Lang.PresetLoadError + ': ' + E.Message);
      LblPresetName.Caption := '';
      LblCreated.Caption := '';
      LblGroupInfo.Caption := '';
      BtnLoad.Enabled := False;
      FHasValidSelection := False;
    end;
  end;
end;

procedure TFrmPresetLoad.BtnLoadClick(Sender: TObject);
begin
  if FHasValidSelection then
    ModalResult := mrOK;
end;

procedure TFrmPresetLoad.BtnDeleteClick(Sender: TObject);
var
  Index: Integer;
  Lang: TLanguageStrings;
begin
  Lang := GetLanguageStrings(Settings.Language);
  Index := ListBoxPresets.ItemIndex;

  if (Index < 0) or (Index >= Length(FPresetFiles)) then
    Exit;

  if MessageDlg(Format(Lang.PresetDeleteConfirm, [ListBoxPresets.Items[Index]]),
    mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    GetPresetManager.DeletePreset(FPresetFiles[Index]);
    LoadPresetList;
  end;
end;

procedure TFrmPresetLoad.BtnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TFrmPresetLoad.MenuItemRenameClick(Sender: TObject);
var
  Index: Integer;
  Lang: TLanguageStrings;
  NewName: string;
begin
  Lang := GetLanguageStrings(Settings.Language);
  Index := ListBoxPresets.ItemIndex;

  if (Index < 0) or (Index >= Length(FPresetFiles)) then
    Exit;

  NewName := ListBoxPresets.Items[Index];
  if InputQuery(Lang.PresetRenameButton, Lang.PresetRenamePrompt, NewName) then
  begin
    if (NewName <> '') and (NewName <> ListBoxPresets.Items[Index]) then
    begin
      GetPresetManager.RenamePreset(FPresetFiles[Index], NewName);
      LoadPresetList;
    end;
  end;
end;

function TFrmPresetLoad.GetSelectedPreset: TSquadPreset;
begin
  Result := FSelectedPreset;
end;

function TFrmPresetLoad.HasValidSelection: Boolean;
begin
  Result := FHasValidSelection;
end;

end.
