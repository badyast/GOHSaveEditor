unit ConquestSave;

interface

uses
  ujachLogAuto, ujachLogMgr,
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

  TInventoryItem = record
    ItemName: string;
    ItemType: string; // "ammo", "grenade", etc.
    Quantity: Integer;
    Cell: string; // z.B. "cell 0 0"
    IsUserItem: Boolean; // z.B. {user "head"}
  end;

  TUnitDetails = record
    UnitId: string;
    Kind: string; // 'Human' or 'Entity'
    UnitType: string; // z.B. "mp/fin/mid/officer"
    Name: string; // Unit name
    Position: string; // z.B. "-136.08 2685.79"
    Veterancy: Integer; // 0-3
    Score: Double;
    InfantryKills: Integer;
    MID: Integer;
    NameId1: Integer; // Erste NameId
    NameId2: Integer; // Zweite NameId
    LastItem: string;
    LastThrowItem: string;
    FsmState: string;
    Inventory: TArray<TInventoryItem>;
  end;

  TConquestSave = class
  private
    FSaveFile: string; // Path to the original .sav file
    FWorkDir: string; // Temp working directory
    FCampaignPath: string; // <WorkDir>/campaign.scn
    FStatusPath: string; // <WorkDir>/status
    FTempExtractDir: string;
    FCampaign: TStringList;

    function ReadAllText: string;
    function FindCampaignSquadsStart: Integer;
    // index of line with '{CampaignSquads'
    function IsSquadLine(const S: string): Boolean;
    function SquadLineIndices: TArray<Integer>;

    // helpers for editing a squad line
    function ExtractUnitIdsFromSquadLine(const Line: string): TArray<string>;
    function AppendUnitIdToSquadLine(const Line, UnitId: string): string;
    function RemoveUnitIdFromSquadLine(const Line, UnitId: string): string;

    // Unit detail parsing
    function ParseInventory(const UnitId: string): TArray<TInventoryItem>;
  public
    constructor Create;
    destructor Destroy; override;
    function GetSquadLine(Index: Integer): string;
    procedure SetSquadLine(Index: Integer; const NewLine: string);
    function ReplaceUnitIdsInSquadLine(const Line: string;
      const NewUnitIds: TArray<string>): string;

    // Loading & saving
    procedure LoadFromSave(const ASaveFile: string);
    procedure LoadFromFolder(const AFolder: string); // if already extracted
    procedure SaveToSave; // repack into FSaveFile (Deflate)
    procedure SaveToSaveAs(const DestFile: string);

    // Queries
    function GetSquadNames: TArray<string>;
    function GetSquadMembers(SquadIndex: Integer): TArray<string>;
    function GetUnitInfo(const UnitId: string): TUnitInfo;
    function GetUnitDetails(const UnitId: string): TUnitDetails;

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

    // Cleanup
    procedure CleanupTempDirectory;
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
  jachLog.LogDebug('TConquestSave.Destroy aufgerufen');

  // Temp-Verzeichnis aufräumen beim Zerstören des Objekts
  CleanupTempDirectory;

  // Andere Ressourcen freigeben
  if Assigned(FCampaign) then
  begin
    FCampaign.Free;
    jachLog.LogDebug('FCampaign freigegeben');
  end;

  inherited Destroy;
  jachLog.LogDebug('TConquestSave.Destroy abgeschlossen');
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
  FileSize: Int64;
  StartTime: TDateTime;
  ElapsedMs: Integer;
begin
  StartTime := Now;
  jachLog.LogInfo('ConquestSave.LoadFromSave gestartet für: %s', [ASaveFile]);

  if not TFile.Exists(ASaveFile) then
  begin
    jachLog.LogError('Savegame-Datei nicht gefunden: %s', [ASaveFile]);
    raise EConquestSave.Create('Save file not found: ' + ASaveFile);
  end;

  try
    FileSize := TFile.GetSize(ASaveFile);
    jachLog.LogInfo('Savegame-Dateigröße: %.2f MB (%d Bytes)',
      [FileSize / (1024 * 1024), FileSize]);
  except
    on E: Exception do
      jachLog.LogWarning('Konnte Savegame-Dateigröße nicht ermitteln: %s',
        [E.Message]);
  end;

  FSaveFile := ASaveFile;

  CreateGUID(G);
  ExtractDir := TPath.Combine(TPath.GetTempPath,
    'ConquestSave_' + GUIDToString(G));

  FTempExtractDir := ExtractDir;
  jachLog.LogDebug('Temp-Verzeichnis für späteres Cleanup gespeichert: %s',
    [FTempExtractDir]);

  try
    TDirectory.CreateDirectory(ExtractDir);
    jachLog.LogDebug('Temporäres Verzeichnis erstellt: %s', [ExtractDir]);
  except
    on E: Exception do
    begin
      jachLog.LogError
        ('Fehler beim Erstellen des temporären Verzeichnisses', E);
      raise EConquestSave.Create('Could not create temp directory: ' +
        E.Message);
    end;
  end;

  Zip := TZipFile.Create;
  try
    try
      jachLog.LogDebug('Öffne ZIP-Datei zum Lesen');
      Zip.Open(FSaveFile, zmRead);
      jachLog.LogInfo('ZIP-Datei geöffnet, %d Einträge gefunden',
        [Zip.FileCount]);

      jachLog.LogDebug('Extrahiere alle Dateien');
      Zip.ExtractAll(ExtractDir);
      jachLog.LogInfo('Alle ZIP-Inhalte extrahiert nach: %s', [ExtractDir]);

    except
      on E: Exception do
      begin
        jachLog.LogError('Fehler beim Öffnen/Extrahieren der ZIP-Datei', E);
        raise EConquestSave.Create('Could not open/extract save file: ' +
          E.Message);
      end;
    end;
  finally
    try
      Zip.Free;
      jachLog.LogDebug('ZIP-Objekt freigegeben');
    except
      on E: Exception do
        jachLog.LogWarning('Fehler beim Schließen der ZIP-Datei: %s',
          [E.Message]);
    end;
  end;

  try
    LoadFromFolder(ExtractDir);
    ElapsedMs := Round((Now - StartTime) * 24 * 60 * 60 * 1000);
    jachLog.LogInfo
      ('ConquestSave.LoadFromSave erfolgreich abgeschlossen in %d ms',
      [ElapsedMs]);
  except
    on E: Exception do
    begin
      jachLog.LogError('Fehler beim Laden aus extrahiertem Ordner', E);

      // Cleanup bei Fehler
      CleanupTempDirectory;

      raise;
    end;
  end;
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
  StartTime: TDateTime;
  ElapsedMs: Integer;
  FileCount: Integer;
begin
  StartTime := Now;
  jachLog.LogInfo('ConquestSave.SaveToSaveAs gestartet für: %s', [DestFile]);

  // Campaign-Datei persistent speichern
  if (FCampaignPath <> '') and (FCampaign <> nil) then
  begin
    jachLog.LogDebug('Speichere Campaign-Daten nach: %s', [FCampaignPath]);
    Enc := TUTF8Encoding.Create(False);
    try
      FCampaign.SaveToFile(FCampaignPath, Enc);
      jachLog.LogDebug('Campaign-Datei erfolgreich gespeichert');
    finally
      Enc.Free;
    end;
  end;

  if TFile.Exists(DestFile) then
  begin
    jachLog.LogInfo('Überschreibe existierende Zieldatei: %s', [DestFile]);
    TFile.Delete(DestFile);
  end;

  if (FWorkDir = '') or (not TDirectory.Exists(FWorkDir)) then
  begin
    jachLog.LogError('Arbeitsverzeichnis ungültig oder nicht gesetzt: %s',
      [FWorkDir]);
    raise EConquestSave.Create
      ('Working folder is invalid or not set. Load a save first.');
  end;

  jachLog.LogDebug('Sammle alle Dateien aus Arbeitsverzeichnis: %s',
    [FWorkDir]);
  Files := TDirectory.GetFiles(FWorkDir, '*', TSearchOption.soAllDirectories);
  FileCount := Length(Files);
  jachLog.LogInfo('Gefunden: %d Dateien zum Packen', [FileCount]);

  Zip := TZipFile.Create;
  try
    Zip.Open(DestFile, zmWrite);
    jachLog.LogDebug('ZIP-Datei zum Schreiben geöffnet');

    Base := IncludeTrailingPathDelimiter(FWorkDir);

    for FileName in Files do
    begin
      RelPath := FileName;
      if RelPath.StartsWith(Base, True) then
        RelPath := RelPath.Substring(Base.Length)
      else
        RelPath := TPath.GetFileName(FileName);

      RelPath := StringReplace(RelPath, '\', '/', [rfReplaceAll]);
      Zip.Add(FileName, RelPath, zcDeflate);
    end;

    jachLog.LogDebug('Alle %d Dateien zu ZIP hinzugefügt', [FileCount]);

  finally
    Zip.Close;
    Zip.Free;
    jachLog.LogDebug('ZIP-Datei geschlossen und freigegeben');
  end;

  ElapsedMs := Round((Now - StartTime) * 24 * 60 * 60 * 1000);

  try
    var
    FinalSize := TFile.GetSize(DestFile);
    jachLog.LogInfo('ConquestSave.SaveToSaveAs erfolgreich abgeschlossen:');
    jachLog.LogInfo('- Zieldatei: %s', [DestFile]);
    jachLog.LogInfo('- Größe: %.2f MB (%d Bytes)', [FinalSize / (1024 * 1024),
      FinalSize]);
    jachLog.LogInfo('- Gepackte Dateien: %d', [FileCount]);
    jachLog.LogInfo('- Dauer: %d ms', [ElapsedMs]);
  except
    jachLog.LogInfo
      ('ConquestSave.SaveToSaveAs erfolgreich abgeschlossen in %d ms',
      [ElapsedMs]);
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
  Result := TRegEx.IsMatch(S, '"[^"]+"\s+"[^"]*"(?:\s+0x[0-9A-Fa-f]{4,8})*');
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

  M := TRegEx.Matches(CleanLine, '0x[0-9A-Fa-f]{4,8}');
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
    '^(.*?"[^"]+"\s*"[^"]*")(?:\s+0x[0-9A-Fa-f]{4,8})*\s*(\}?)\s*$');
  if not M.Success then
    // Fallback ohne ^/$ (zusätzliche Robustheit)
    M := TRegEx.Match(Line,
      '(.*?"[^"]+"\s*"[^"]*")(?:\s+0x[0-9A-Fa-f]{4,8})*\s*(\}?)');
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

function TConquestSave.ParseInventory(const UnitId: string)
  : TArray<TInventoryItem>;
var
  Text: string;
  M, ItemMatch: TMatch;
  Items: TList<TInventoryItem>;
  Item: TInventoryItem;
  InvBlock: string;
  ItemRegex: string;
  StartPos, EndPos, BraceCount: Integer;
  Lines: TStringList;
  I: Integer;
  Line: string;
  InInventoryBlock, InBoxBlock: Boolean;
begin
  Items := TList<TInventoryItem>.Create;
  try
    Text := ReadAllText;
    Lines := TStringList.Create;
    try
      Lines.Text := Text;

      // Manuell den Inventory-Block für diese Unit finden
      InInventoryBlock := False;
      InBoxBlock := False;
      InvBlock := '';

      for I := 0 to Lines.Count - 1 do
      begin
        Line := Lines[I].Trim;

        // Start des Inventory-Blocks gefunden?
        if Line.Contains('{Inventory') and Line.Contains(UnitId) then
        begin
          InInventoryBlock := True;
          Continue;
        end;

        // Sind wir im Inventory-Block?
        if InInventoryBlock then
        begin
          // Start der Box gefunden?
          if Line.Contains('{box') then
          begin
            InBoxBlock := True;
            Continue;
          end;

          // Ende des Inventory-Blocks?
          if Line = '}' then
          begin
            Break; // Inventory-Block beendet
          end;

          // Sammle Zeilen innerhalb der Box
          if InBoxBlock then
          begin
            InvBlock := InvBlock + Line + #13#10;
          end;
        end;
      end;

    finally
      Lines.Free;
    end;

    // Jetzt die Items aus dem InvBlock parsen
    if InvBlock <> '' then
    begin
      // Flexiblerer Item-RegEx der verschiedene Formate versteht
      // Grundformat: {item "name" [weitere_parameter] {cell x y} [optional {user "part"}]}
      ItemRegex :=
        '\{item\s+"([^"]+)"([^}]*?)\{cell\s+(\d+)\s+(\d+)\}([^}]*?)\}';

      for ItemMatch in TRegEx.Matches(InvBlock, ItemRegex) do
      begin
        Item.ItemName := ItemMatch.Groups[1].Value;

        // Parse die Zusatzparameter zwischen Item-Name und Cell
        var
        ParamsPart := ItemMatch.Groups[2].Value.Trim;
        var
        CellX := ItemMatch.Groups[3].Value;
        var
        CellY := ItemMatch.Groups[4].Value;
        var
        AfterCellPart := ItemMatch.Groups[5].Value.Trim;

        // Item-Typ und Quantity aus den Parametern extrahieren
        Item.ItemType := '';
        Item.Quantity := 1;

        // Suche nach quoted strings für Typ
        var
        TypeMatch := TRegEx.Match(ParamsPart, '"([^"]+)"');
        if TypeMatch.Success then
          Item.ItemType := TypeMatch.Groups[1].Value;

        // Suche nach Zahlen für Quantity
        var
        QuantityMatch := TRegEx.Match(ParamsPart, '\b(\d+)\b');
        if QuantityMatch.Success then
          Item.Quantity := StrToIntDef(QuantityMatch.Groups[1].Value, 1);

        // Cell-Position
        Item.Cell := CellX + ',' + CellY;

        // User-Item prüfen
        Item.IsUserItem := AfterCellPart.Contains('{user');

        Items.Add(Item);
      end;
    end;

    Result := Items.ToArray;
  finally
    Items.Free;
  end;
end;

function TConquestSave.GetUnitDetails(const UnitId: string): TUnitDetails;
var
  Text: string;
  M: TMatch;
  Pattern: string;
begin
  Result := Default (TUnitDetails);
  Result.UnitId := UnitId;

  Text := ReadAllText;

  // Haupt-Pattern für Human/Entity Block
  Pattern := '\{(Human|Entity)\s+"([^"]+)"\s+' + TRegEx.Escape(UnitId) +
    '(.*?)\n\t\}';
  M := TRegEx.Match(Text, Pattern, [roSingleLine]);

  if M.Success then
  begin
    Result.Kind := M.Groups[1].Value;
    Result.UnitType := M.Groups[2].Value;

    // Extrahiere Details aus dem Block
    var
    Block := M.Groups[3].Value;

    // Position
    M := TRegEx.Match(Block, '\{Position\s+([-\d.]+)\s+([-\d.]+)\}');
    if M.Success then
      Result.Position := M.Groups[1].Value + ' ' + M.Groups[2].Value;

    // Veterancy
    M := TRegEx.Match(Block, '\{Veterancy\s+(\d+)\}');
    if M.Success then
      Result.Veterancy := StrToIntDef(M.Groups[1].Value, 0);

    // Score
    M := TRegEx.Match(Block, '\{Score\s+([\d.]+)\}');
    if M.Success then
      Result.Score := StrToFloatDef(M.Groups[1].Value, 0,
        FormatSettings.Invariant);

    // InfantryKills
    M := TRegEx.Match(Block, '\{InfantryKills\s+(\d+)\}');
    if M.Success then
      Result.InfantryKills := StrToIntDef(M.Groups[1].Value, 0);

    // MID
    M := TRegEx.Match(Block, '\{MID\s+(\d+)\}');
    if M.Success then
      Result.MID := StrToIntDef(M.Groups[1].Value, 0);

    // NameId
    M := TRegEx.Match(Block, '\{NameId\s+(\d+)\s+(\d+)\}');
    if M.Success then
    begin
      Result.NameId1 := StrToIntDef(M.Groups[1].Value, 0);
      Result.NameId2 := StrToIntDef(M.Groups[2].Value, 0);
    end;

    // LastItem
    M := TRegEx.Match(Block, '\{LastItem\s+"([^"]+)"\}');
    if M.Success then
      Result.LastItem := M.Groups[1].Value;

    // LastThrowItem
    M := TRegEx.Match(Block, '\{LastThrowItem\s+"([^"]+)"\}');
    if M.Success then
      Result.LastThrowItem := M.Groups[1].Value;

    // FsmState
    M := TRegEx.Match(Block, '\{FsmState\s+"([^"]+)"\}');
    if M.Success then
      Result.FsmState := M.Groups[1].Value;

    // Unit Name aus GetUnitInfo
    var
    Info := GetUnitInfo(UnitId);
    Result.Name := Info.Name;

    // Inventar
    Result.Inventory := ParseInventory(UnitId);
  end;
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

procedure TConquestSave.CleanupTempDirectory;
var
  StartTime: TDateTime;
  ElapsedMs: Integer;
  FileCount: Integer;
  DirSize: Int64;
begin
  if FTempExtractDir = '' then
  begin
    jachLog.LogDebug
      ('CleanupTempDirectory: Kein Temp-Verzeichnis zum Bereinigen vorhanden');
    Exit;
  end;

  if not TDirectory.Exists(FTempExtractDir) then
  begin
    jachLog.LogDebug
      ('CleanupTempDirectory: Temp-Verzeichnis existiert nicht mehr: %s',
      [FTempExtractDir]);
    FTempExtractDir := '';
    Exit;
  end;

  StartTime := Now;
  jachLog.LogInfo('=== CLEANUP-OPERATION gestartet ===');
  jachLog.LogInfo('Bereinige Temp-Verzeichnis: %s', [FTempExtractDir]);

  try
    // Statistiken sammeln vor dem Löschen
    try
      var
      Files := TDirectory.GetFiles(FTempExtractDir, '*',
        TSearchOption.soAllDirectories);
      FileCount := Length(Files);
      DirSize := 0;
      for var F in Files do
        Inc(DirSize, TFile.GetSize(F));

      jachLog.LogDebug('Temp-Verzeichnis enthält %d Dateien (%.2f MB)',
        [FileCount, DirSize / (1024 * 1024)]);
    except
      on E: Exception do
        jachLog.LogWarning('Konnte Statistiken nicht ermitteln: %s',
          [E.Message]);
    end;

    // Verzeichnis löschen
    TDirectory.Delete(FTempExtractDir, True);

    ElapsedMs := Round((Now - StartTime) * 24 * 60 * 60 * 1000);
    jachLog.LogInfo('CLEANUP-OPERATION erfolgreich abgeschlossen:');
    jachLog.LogInfo('- Verzeichnis: %s', [ExtractFileName(FTempExtractDir)]);
    jachLog.LogInfo('- Gelöschte Dateien: %d', [FileCount]);
    jachLog.LogInfo('- Freigegebener Speicher: %.2f MB',
      [DirSize / (1024 * 1024)]);
    jachLog.LogInfo('- Dauer: %d ms', [ElapsedMs]);

    FTempExtractDir := '';

  except
    on E: Exception do
    begin
      jachLog.LogError('Fehler beim Bereinigen des Temp-Verzeichnisses: %s',
        [FTempExtractDir], E);
      jachLog.LogWarning
        ('Temp-Verzeichnis konnte nicht gelöscht werden und bleibt zurück');
      // Trotzdem zurücksetzen, damit wir es nicht nochmal versuchen
      FTempExtractDir := '';
    end;
  end;
end;

end.
