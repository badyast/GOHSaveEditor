unit ConquestSave;
{
  Delphi 12 class that mirrors the core functionality of Editor.py for
  Gates of Hell Conquest save editing.

  Features:
  - Load a .sav (zip) file, extract campaign.scn + status
  - Parse CampaignSquads block to read squad names and unit IDs
  - Read unit info (Human/Entity, Unit name) by UnitId
  - Move a unit to another squad
  - Exchange two units between squads
  - Repack the .sav using Deflate compression

  Notes:
  * All squad indexes are 0-based (matching Delphi/combobox conventions).
  * Unit IDs are strings like '0xABCD'.
}

interface

uses
  System.StrUtils,
  System.Classes,
  System.SysUtils,
  System.RegularExpressions,
  System.Zip,
  System.IOUtils,
  System.Generics.Collections;

type
  EConquestSave = class(Exception);

  TUnitInfo = record
    Kind: string; // 'Human' or 'Entity'
    Name: string; // Unit name (unquoted)
  end;

  TConquestSave = class
  private
    FSaveFile: string; // Path to the original .sav file
    FWorkDir: string; // Temp working directory
    FCampaignPath: string; // <WorkDir>/campaign.scn
    FStatusPath: string; // <WorkDir>/status
    FCampaign: TStringList;

    function ReadAllText: string;
    function FindCampaignSquadsStart: Integer;
    // index of line with '{CampaignSquads'
    function IsSquadLine(const S: string): Boolean;
    function SquadLineIndices: TArray<Integer>;
    function GetSquadLine(Index: Integer): string;
    procedure SetSquadLine(Index: Integer; const NewLine: string);

    // helpers for editing a squad line
    function ExtractUnitIdsFromSquadLine(const Line: string): TArray<string>;
    function ReplaceUnitIdsInSquadLine(const Line: string;
      const NewUnitIds: TArray<string>): string;
    function AppendUnitIdToSquadLine(const Line, UnitId: string): string;
    function RemoveUnitIdFromSquadLine(const Line, UnitId: string): string;
  public
    constructor Create;
    destructor Destroy; override;

    // Loading & saving
    procedure LoadFromSave(const ASaveFile: string);
    procedure LoadFromFolder(const AFolder: string); // if already extracted
    procedure SaveToSave; // repack into FSaveFile (Deflate)
    procedure SaveToSaveAs(const DestFile: string);

    // Queries
    function GetSquadNames: TArray<string>;
    function GetSquadMembers(SquadIndex: Integer): TArray<string>;
    function GetUnitInfo(const UnitId: string): TUnitInfo;

    // Edits
    procedure MoveUnit(BaseSquadIndex: Integer; const UnitId: string;
      TargetSquadIndex: Integer);
    procedure ExchangeUnits(BaseSquadIndex: Integer; const BaseUnitId: string;
      TargetSquadIndex: Integer; const TargetUnitId: string);

    // Paths
    property SaveFile: string read FSaveFile;
    property WorkDir: string read FWorkDir;
    property CampaignPath: string read FCampaignPath;
    property StatusPath: string read FStatusPath;
  end;

implementation

{ TConquestSave }

constructor TConquestSave.Create;
begin
  inherited Create;
  FCampaign := TStringList.Create;
end;

destructor TConquestSave.Destroy;
begin
  FCampaign.Free;
  inherited;
end;

procedure TConquestSave.LoadFromFolder(const AFolder: string);
begin
  FWorkDir := AFolder;
  FCampaignPath := TPath.Combine(FWorkDir, 'campaign.scn');
  FStatusPath := TPath.Combine(FWorkDir, 'status');
  if not TFile.Exists(FCampaignPath) then
    raise EConquestSave.Create('campaign.scn not found in folder: ' + FWorkDir);
  FCampaign.LoadFromFile(FCampaignPath, TEncoding.UTF8);
end;

procedure TConquestSave.LoadFromSave(const ASaveFile: string);
var
  Zip: TZipFile;
  ExtractDir: string;
  G: TGUID;
begin
  if not TFile.Exists(ASaveFile) then
    raise EConquestSave.Create('Save file not found: ' + ASaveFile);

  FSaveFile := ASaveFile;

  // Create temp working directory
  CreateGUID(G);
  ExtractDir := TPath.Combine(TPath.GetTempPath,
    'ConquestSave_' + GUIDToString(G));
  TDirectory.CreateDirectory(ExtractDir);

  Zip := TZipFile.Create;
  try
    Zip.Open(FSaveFile, zmRead);
    Zip.ExtractAll(ExtractDir);
  finally
    Zip.Free;
  end;

  LoadFromFolder(ExtractDir);
end;

procedure TConquestSave.SaveToSave;
begin
  if FSaveFile = '' then
    raise EConquestSave.Create('No original .sav path set. Use SaveToSaveAs.');
  SaveToSaveAs(FSaveFile);
end;

procedure TConquestSave.SaveToSaveAs(const DestFile: string);
var
  Zip: TZipFile;
  Files: TArray<string>;
  FileName, RelPath, Base: string;
  Enc: TEncoding;

begin
  // Persist current campaign text (if we have it loaded)
  if (FCampaignPath <> '') and (FCampaign <> nil) then
  begin
    Enc := TUTF8Encoding.Create(False); // UTF-8 ohne BOM
    FCampaign.SaveToFile(FCampaignPath, Enc);
  end;

  if TFile.Exists(DestFile) then
    TFile.Delete(DestFile);

  // Collect ALL files under the working directory so we preserve any
  // new/unknown files the game added after recent patches.
  if (FWorkDir = '') or (not TDirectory.Exists(FWorkDir)) then
    raise EConquestSave.Create
      ('Working folder is invalid or not set. Load a save first.');

  Files := TDirectory.GetFiles(FWorkDir, '*', TSearchOption.soAllDirectories);

  Zip := TZipFile.Create;
  try
    Zip.Open(DestFile, zmWrite);

    Base := IncludeTrailingPathDelimiter(FWorkDir);

    for FileName in Files do
    begin
      // Compute archive-relative path and normalize to forward slashes
      RelPath := FileName;
      if RelPath.StartsWith(Base, True) then
        RelPath := RelPath.Substring(Base.Length)
      else
        RelPath := TPath.GetFileName(FileName); // fallback, should not happen

      RelPath := StringReplace(RelPath, '\', '/', [rfReplaceAll]);

      // Always use Deflate to avoid uncompressed .sav (observed bug elsewhere)
      Zip.Add(FileName, RelPath, zcDeflate);
    end;
  finally
    Zip.Close;
    Zip.Free;
  end;
end;

function TConquestSave.ReadAllText: string;
begin
  Result := FCampaign.Text;
end;

function TConquestSave.FindCampaignSquadsStart: Integer;
var
  I: Integer;
begin
  for I := 0 to FCampaign.Count - 1 do
    if FCampaign[I].Contains('{CampaignSquads') then
      Exit(I);
  Result := -1;
end;

function TConquestSave.IsSquadLine(const S: string): Boolean;
begin
  // Erlaubt führendes '{' und ein leeres zweites Anführungsfeld ("")
  Result := TRegEx.IsMatch(S, '"[^"]+"\s+"[^"]*"(?:\s+0x[0-9A-Fa-f]{4})*');
end;

function TConquestSave.SquadLineIndices: TArray<Integer>;
var
  StartIdx, I: Integer;
  L: TList<Integer>;
begin
  StartIdx := FindCampaignSquadsStart;
  if StartIdx < 0 then
    raise EConquestSave.Create('CampaignSquads block not found.');

  L := TList<Integer>.Create;
  try
    for I := StartIdx + 1 to FCampaign.Count - 1 do
    begin
      if not IsSquadLine(FCampaign[I]) then
        Break;
      L.Add(I);
    end;
    Result := L.ToArray;
  finally
    L.Free;
  end;
end;

function TConquestSave.GetSquadLine(Index: Integer): string;
var
  IdxArr: TArray<Integer>;
begin
  IdxArr := SquadLineIndices;
  if (Index < 0) or (Index >= Length(IdxArr)) then
    raise EConquestSave.CreateFmt('Squad index %d out of range.', [Index]);
  Result := FCampaign[IdxArr[Index]];
end;

procedure TConquestSave.SetSquadLine(Index: Integer; const NewLine: string);
var
  IdxArr: TArray<Integer>;
  LineIdx: Integer;
begin
  IdxArr := SquadLineIndices;
  if (Index < 0) or (Index >= Length(IdxArr)) then
    raise EConquestSave.CreateFmt('Squad index %d out of range.', [Index]);
  LineIdx := IdxArr[Index];
  FCampaign[LineIdx] := NewLine;
end;

function TConquestSave.ExtractUnitIdsFromSquadLine(const Line: string)
  : TArray<string>;
var
  M: TMatchCollection;
  I: Integer;
  L: TList<string>;
  CleanLine: string;
begin
  // Strip closing '}' (if present), then collect all 0xXXXX tokens
  CleanLine := Line;
  if CleanLine.Trim.EndsWith('}') then
    CleanLine := CleanLine.TrimRight([' ', #9])
      .Substring(0, CleanLine.TrimRight([' ', #9]).Length - 1);

  M := TRegEx.Matches(CleanLine, '0x[0-9A-Fa-f]{4}');
  L := TList<string>.Create;
  try
    for I := 0 to M.Count - 1 do
      L.Add(M.Item[I].Value);
    Result := L.ToArray;
  finally
    L.Free;
  end;
end;

function TConquestSave.ReplaceUnitIdsInSquadLine(const Line: string;
  const NewUnitIds: TArray<string>): string;
var
  M: TMatch;
  Prefix, Suffix, JoinUnits: string;
begin
  // Prefix = alles bis inkl. zweites Anführungsfeld
  // Suffix = optional schließende '}'
  M := TRegEx.Match(Line,
    '^(.*?"[^"]+"\s*"[^"]*")(?:\s+0x[0-9A-Fa-f]{4})*\s*(\}?)\s*$');
  if not M.Success then
    // Fallback ohne ^/$ (zusätzliche Robustheit)
    M := TRegEx.Match(Line,
      '(.*?"[^"]+"\s*"[^"]*")(?:\s+0x[0-9A-Fa-f]{4})*\s*(\}?)');
  if not M.Success then
    raise EConquestSave.Create('Invalid squad line format: ' + Line);

  Prefix := M.Groups[1].Value;
  Suffix := M.Groups[2].Value;

  if Length(NewUnitIds) > 0 then
    JoinUnits := ' ' + string.Join(' ', NewUnitIds)
  else
    JoinUnits := '';

  Result := Prefix + JoinUnits + Suffix;
end;

function TConquestSave.AppendUnitIdToSquadLine(const Line,
  UnitId: string): string;
var
  Units: TArray<string>;
  L: TList<string>;
begin
  Units := ExtractUnitIdsFromSquadLine(Line);
  L := TList<string>.Create;
  try
    L.AddRange(Units);
    if L.IndexOf(UnitId) < 0 then
      L.Add(UnitId);
    Result := ReplaceUnitIdsInSquadLine(Line, L.ToArray);
  finally
    L.Free;
  end;
end;

function TConquestSave.RemoveUnitIdFromSquadLine(const Line,
  UnitId: string): string;
var
  Units: TArray<string>;
  L: TList<string>;
begin
  Units := ExtractUnitIdsFromSquadLine(Line);
  L := TList<string>.Create;
  try
    L.AddRange(Units);
    L.Remove(UnitId);
    Result := ReplaceUnitIdsInSquadLine(Line, L.ToArray);
  finally
    L.Free;
  end;
end;

function TConquestSave.GetSquadNames: TArray<string>;
var
  IdxArr: TArray<Integer>;
  I: Integer;
  M: TMatch;
  Names: TList<string>;
  Line: string;
begin
  IdxArr := SquadLineIndices;
  Names := TList<string>.Create;
  try
    for I := 0 to Length(IdxArr) - 1 do
    begin
      Line := FCampaign[IdxArr[I]];
      // Unverankert, erlaubt leeres zweites Feld ("")
      M := TRegEx.Match(Line, '"(.*?)"\s+"(.*?)"');
      if not M.Success then
        raise EConquestSave.Create
          ('Unexpected squad line format while reading names: ' + Line);
      Names.Add(M.Groups[1].Value); // nur der Squadname
    end;
    Result := Names.ToArray;
  finally
    Names.Free;
  end;
end;

function TConquestSave.GetSquadMembers(SquadIndex: Integer): TArray<string>;
var
  Line: string;
begin
  Line := GetSquadLine(SquadIndex);
  Result := ExtractUnitIdsFromSquadLine(Line);
end;

function TConquestSave.GetUnitInfo(const UnitId: string): TUnitInfo;
var
  Text: string;
  M: TMatch;
begin
  // Regex like: (Human|Entity) "(.*?)" <space> UnitId
  Text := ReadAllText;
  M := TRegEx.Match(Text, '(Human|Entity)\s+"(.*?)"\s+' +
    TRegEx.Escape(UnitId));
  if not M.Success then
    raise EConquestSave.Create('Unit not found by id: ' + UnitId);
  Result.Kind := M.Groups[1].Value;
  Result.Name := M.Groups[2].Value;
end;

procedure TConquestSave.MoveUnit(BaseSquadIndex: Integer; const UnitId: string;
  TargetSquadIndex: Integer);
var
  BaseLine, TargetLine: string;
begin
  if BaseSquadIndex = TargetSquadIndex then
    Exit; // nothing to do

  BaseLine := GetSquadLine(BaseSquadIndex);
  TargetLine := GetSquadLine(TargetSquadIndex);

  BaseLine := RemoveUnitIdFromSquadLine(BaseLine, UnitId);
  TargetLine := AppendUnitIdToSquadLine(TargetLine, UnitId);

  SetSquadLine(BaseSquadIndex, BaseLine);
  SetSquadLine(TargetSquadIndex, TargetLine);
end;

procedure TConquestSave.ExchangeUnits(BaseSquadIndex: Integer;
  const BaseUnitId: string; TargetSquadIndex: Integer;
  const TargetUnitId: string);
var
  BaseLine, TargetLine: string;
begin
  if (BaseSquadIndex = TargetSquadIndex) and (BaseUnitId = TargetUnitId) then
    Exit;

  BaseLine := GetSquadLine(BaseSquadIndex);
  TargetLine := GetSquadLine(TargetSquadIndex);

  // Remove & add accordingly
  BaseLine := RemoveUnitIdFromSquadLine(BaseLine, BaseUnitId);
  BaseLine := AppendUnitIdToSquadLine(BaseLine, TargetUnitId);

  TargetLine := RemoveUnitIdFromSquadLine(TargetLine, TargetUnitId);
  TargetLine := AppendUnitIdToSquadLine(TargetLine, BaseUnitId);

  SetSquadLine(BaseSquadIndex, BaseLine);
  SetSquadLine(TargetSquadIndex, TargetLine);
end;

end.
