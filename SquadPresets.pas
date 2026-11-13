unit SquadPresets;

interface

uses
  System.SysUtils, System.Classes, System.IOUtils, System.JSON,
  System.Generics.Collections, System.DateUtils;

const
  // Gruppen, die in einer Vorlage gespeichert werden (stage_special bleibt aussen vor)
  PRESET_STAGES: array[0..4] of string = ('stage_1', 'stage_2', 'stage_3', 'stage_4', 'stage_5');

type
  TPresetGroup = record
    StageFilter: string;        // z.B. 'stage_1'
    SquadNames: TArray<string>; // Namen der Squads in dieser Gruppe
  end;

  TSquadPreset = record
    Name: string;
    Created: TDateTime;
    Groups: TArray<TPresetGroup>;
    function TotalSquadCount: Integer;
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

{ TSquadPreset }

function TSquadPreset.TotalSquadCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Length(Groups) - 1 do
    Inc(Result, Length(Groups[I].SquadNames));
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
  GroupsArray, SquadsArray: TJSONArray;
  GroupObj: TJSONObject;
  I, J: Integer;
begin
  Result := TJSONObject.Create;
  try
    Result.AddPair('name', Preset.Name);
    Result.AddPair('created', DateToISO8601(Preset.Created, False));

    GroupsArray := TJSONArray.Create;
    Result.AddPair('groups', GroupsArray);
    for I := 0 to Length(Preset.Groups) - 1 do
    begin
      GroupObj := TJSONObject.Create;
      GroupsArray.AddElement(GroupObj);
      GroupObj.AddPair('stage', Preset.Groups[I].StageFilter);

      SquadsArray := TJSONArray.Create;
      GroupObj.AddPair('squadNames', SquadsArray);
      for J := 0 to Length(Preset.Groups[I].SquadNames) - 1 do
        SquadsArray.Add(Preset.Groups[I].SquadNames[J]);
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TSquadPresetManager.JSONToPreset(const JSON: TJSONObject): TSquadPreset;

  function ReadNames(const Arr: TJSONArray): TArray<string>;
  var
    K: Integer;
  begin
    SetLength(Result, Arr.Count);
    for K := 0 to Arr.Count - 1 do
      Result[K] := Arr.Items[K].Value;
  end;

var
  GroupsArray, SquadsArray: TJSONArray;
  GroupObj: TJSONObject;
  I: Integer;
begin
  Result.Name := JSON.GetValue<string>('name');
  Result.Created := ISO8601ToDate(JSON.GetValue<string>('created'), False);

  if JSON.TryGetValue<TJSONArray>('groups', GroupsArray) then
  begin
    // Neues Format: mehrere Gruppen
    SetLength(Result.Groups, GroupsArray.Count);
    for I := 0 to GroupsArray.Count - 1 do
    begin
      GroupObj := GroupsArray.Items[I] as TJSONObject;
      Result.Groups[I].StageFilter := GroupObj.GetValue<string>('stage');
      Result.Groups[I].SquadNames := ReadNames(GroupObj.GetValue<TJSONArray>('squadNames'));
    end;
  end
  else
  begin
    // Altes Format: eine einzelne Gruppe (stageFilter + squadNames)
    SetLength(Result.Groups, 1);
    JSON.TryGetValue<string>('stageFilter', Result.Groups[0].StageFilter);
    SquadsArray := JSON.GetValue<TJSONArray>('squadNames');
    Result.Groups[0].SquadNames := ReadNames(SquadsArray);
  end;
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
  if JSON = nil then
    raise Exception.CreateFmt('Ungueltige Vorlagendatei: %s', [TPath.GetFileName(FileName)]);
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
