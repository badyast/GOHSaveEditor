unit SquadPresets;

interface

uses
  System.SysUtils, System.Classes, System.IOUtils, System.JSON,
  System.Generics.Collections, System.DateUtils;

type
  TSquadPreset = record
    Name: string;
    Created: TDateTime;
    StageFilter: string;      // z.B. 'stage_1', 'stage_special'
    SquadNames: TArray<string>; // Namen der Squads in dieser Gruppe
  end;

  TSquadPresetManager = class
  private
    FPresetsFolder: string;
    function GetPresetsFolder: string;
    procedure EnsurePresetsFolderExists;
    function PresetToJSON(const Preset: TSquadPreset): TJSONObject;
    function JSONToPreset(const JSON: TJSONObject): TSquadPreset;
  public
    constructor Create;

    function SavePreset(const Preset: TSquadPreset): string;
    function LoadPreset(const FileName: string): TSquadPreset;
    function GetPresetList: TArray<string>;
    procedure DeletePreset(const FileName: string);
    procedure RenamePreset(const FileName: string; const NewName: string);

    property PresetsFolder: string read GetPresetsFolder;
  end;

function GetPresetManager: TSquadPresetManager;

implementation

var
  FPresetManager: TSquadPresetManager = nil;

function GetPresetManager: TSquadPresetManager;
begin
  if FPresetManager = nil then
    FPresetManager := TSquadPresetManager.Create;
  Result := FPresetManager;
end;

{ TSquadPresetManager }

constructor TSquadPresetManager.Create;
begin
  inherited Create;
  FPresetsFolder := '';
end;

function TSquadPresetManager.GetPresetsFolder: string;
begin
  if FPresetsFolder = '' then
    FPresetsFolder := TPath.Combine(ExtractFilePath(ParamStr(0)), 'Presets');
  Result := FPresetsFolder;
end;

procedure TSquadPresetManager.EnsurePresetsFolderExists;
begin
  if not TDirectory.Exists(GetPresetsFolder) then
    TDirectory.CreateDirectory(GetPresetsFolder);
end;

function TSquadPresetManager.PresetToJSON(const Preset: TSquadPreset): TJSONObject;
var
  SquadsArray: TJSONArray;
  I: Integer;
begin
  Result := TJSONObject.Create;
  try
    Result.AddPair('name', Preset.Name);
    Result.AddPair('created', DateToISO8601(Preset.Created, False));
    Result.AddPair('stageFilter', Preset.StageFilter);

    SquadsArray := TJSONArray.Create;
    for I := 0 to Length(Preset.SquadNames) - 1 do
      SquadsArray.Add(Preset.SquadNames[I]);
    Result.AddPair('squadNames', SquadsArray);
  except
    Result.Free;
    raise;
  end;
end;

function TSquadPresetManager.JSONToPreset(const JSON: TJSONObject): TSquadPreset;
var
  SquadsArray: TJSONArray;
  I: Integer;
  CreatedStr: string;
begin
  Result.Name := JSON.GetValue<string>('name');

  CreatedStr := JSON.GetValue<string>('created');
  Result.Created := ISO8601ToDate(CreatedStr, False);

  JSON.TryGetValue<string>('stageFilter', Result.StageFilter);

  SquadsArray := JSON.GetValue<TJSONArray>('squadNames');
  SetLength(Result.SquadNames, SquadsArray.Count);
  for I := 0 to SquadsArray.Count - 1 do
    Result.SquadNames[I] := SquadsArray.Items[I].Value;
end;

function TSquadPresetManager.SavePreset(const Preset: TSquadPreset): string;
var
  JSON: TJSONObject;
  FileName: string;
  SafeName: string;
  I: Integer;
begin
  EnsurePresetsFolderExists;

  SafeName := Preset.Name;
  for I := 1 to Length(SafeName) do
  begin
    if not CharInSet(SafeName[I], ['a'..'z', 'A'..'Z', '0'..'9', ' ', '-', '_']) then
      SafeName[I] := '_';
  end;

  FileName := TPath.Combine(GetPresetsFolder, SafeName + '.json');

  I := 1;
  while TFile.Exists(FileName) do
  begin
    Inc(I);
    FileName := TPath.Combine(GetPresetsFolder, SafeName + '_' + IntToStr(I) + '.json');
  end;

  JSON := PresetToJSON(Preset);
  try
    TFile.WriteAllText(FileName, JSON.Format(2), TEncoding.UTF8);
  finally
    JSON.Free;
  end;

  Result := FileName;
end;

function TSquadPresetManager.LoadPreset(const FileName: string): TSquadPreset;
var
  JSON: TJSONObject;
  Content: string;
begin
  Content := TFile.ReadAllText(FileName, TEncoding.UTF8);
  JSON := TJSONObject.ParseJSONValue(Content) as TJSONObject;
  try
    Result := JSONToPreset(JSON);
  finally
    JSON.Free;
  end;
end;

function TSquadPresetManager.GetPresetList: TArray<string>;
begin
  EnsurePresetsFolderExists;
  Result := TDirectory.GetFiles(GetPresetsFolder, '*.json');
end;

procedure TSquadPresetManager.DeletePreset(const FileName: string);
begin
  if TFile.Exists(FileName) then
    TFile.Delete(FileName);
end;

procedure TSquadPresetManager.RenamePreset(const FileName: string; const NewName: string);
var
  Preset: TSquadPreset;
  JSON: TJSONObject;
begin
  if not TFile.Exists(FileName) then
    Exit;

  // Load, update name, save back
  Preset := LoadPreset(FileName);
  Preset.Name := NewName;

  JSON := PresetToJSON(Preset);
  try
    TFile.WriteAllText(FileName, JSON.Format(2), TEncoding.UTF8);
  finally
    JSON.Free;
  end;
end;

initialization

finalization
  if FPresetManager <> nil then
    FPresetManager.Free;

end.
