unit ProjektArchive;

{
  ========================================================================
  PROJEKT-ARCHIV: Nicht mehr verwendete Methoden
  ========================================================================

  Diese Unit enthält Funktionen und Prozeduren, die früher im GOH Save
  Editor verwendet wurden, aber durch neuere/bessere Implementierungen
  ersetzt wurden.

  ZWECK:
  - Bewahrung von Code für mögliche zukünftige Referenz
  - Dokumentation der Entwicklungsgeschichte
  - Einfache Wiederherstellung falls benötigt

  WARNUNG:
  Diese Methoden sind NICHT getestet mit der aktuellen Codebase!
  Vor einer eventuellen Wiederverwendung müssen sie an die aktuelle
  Datenstruktur angepasst und getestet werden.

  ARCHIVIERUNGS-DATUM: 2025-09-27
  URSPRÜNGLICHE DATEIEN: MainForm.pas, ConquestSave.pas

  ========================================================================
}

interface

uses
  System.Classes, System.SysUtils, System.RegularExpressions, TypInfo,
  Vcl.ComCtrls, Vcl.Controls;

type
  {
    Archivierte Typen aus der ursprünglichen Implementierung
    Diese werden für die archivierten Methoden benötigt
  }
  TNodeKind = (nkSquad, nkUnit);

  TNodeInfo = class
  public
    Kind: TNodeKind;
    SquadIndex: Integer;
    UnitId: string;
  end;

  // Typen aus ConquestSave für Archiv-Methoden
  TUnitInfo = record
    Kind: string;
    Name: string;
  end;

  TInventoryItem = record
    ItemName: string;
    ItemType: string;
    Quantity: Integer;
    Cell: string;
    IsUserItem: Boolean;
  end;

  TUnitDetails = record
    UnitId: string;
    Kind: string;
    UnitType: string;
    Name: string;
    Position: string;
    Veterancy: Integer;
    Score: Double;
    InfantryKills: Integer;
    MID: Integer;
    NameId1: Integer;
    NameId2: Integer;
    LastItem: string;
    LastThrowItem: string;
    FsmState: string;
    Inventory: TArray<TInventoryItem>;
  end;

  {
    Archiv-Klasse für MainForm-Methoden
    Diese Methoden waren Teil der TFrmMain-Klasse
  }
  TMainFormArchive = class
  public
    {
      ARCHIVIERT aus MainForm.pas:103/738
      GRUND: Ersetzt durch UpdateTreeViewsFromSquads
      DATUM: 2025-09-27

      Diese Methode war für die Population einzelner TreeViews zuständig,
      wurde aber durch eine effizientere Lösung ersetzt, die beide
      TreeViews gleichzeitig aktualisiert.
    }
    procedure PopulateTree(ATree: TTreeView);

    {
      ARCHIVIERT aus MainForm.pas:120/974
      GRUND: Ersetzt durch UpdateSquadVeterancyFromCache
      DATUM: 2025-09-27

      Diese Methode berechnete Veteranenstufen durch direktes Parsen.
      Die neue Implementation verwendet gecachte Daten für bessere Performance.
    }
    procedure UpdateSquadVeterancy(ASquadIndex: Integer);

    {
      ARCHIVIERT aus MainForm.pas:111/1306
      GRUND: Ersetzt durch spezifischere RestoreFocusToUnit
      DATUM: 2025-09-27

      Focus-Wiederherstellung auf Squad-Ebene wurde durch eine granularere
      Unit-spezifische Fokussierung ersetzt.
    }
    procedure RestoreFocusToSquad(ATree: TTreeView; ASquadIndex: Integer);
  end;

  {
    Archiv-Klasse für ConquestSave-Methoden
    Diese Methoden waren Teil der TConquestSave-Klasse
  }
  TConquestSaveArchive = class
  public
    {
      ARCHIVIERT aus ConquestSave.pas:104/281
      GRUND: Nur SaveToSaveAs wird verwendet
      DATUM: 2025-09-27

      Diese Methode sollte die ursprüngliche Datei überschreiben.
      Das Projekt verwendet ausschließlich SaveToSaveAs für alle
      Speichervorgänge aus Sicherheitsgründen.
    }
    procedure SaveToSave;

    {
      ARCHIVIERT aus ConquestSave.pas:114/801
      GRUND: Durch Swap-Operationen ersetzt
      DATUM: 2025-09-27

      Unit-Bewegung zwischen Squads wurde durch ein robusteres
      Swap-System ersetzt, das konsistentere Ergebnisse liefert.
    }
    procedure MoveUnit(BaseSquadIndex: Integer; const UnitId: string;
      TargetSquadIndex: Integer);

    {
      ARCHIVIERT aus ConquestSave.pas:116/819
      GRUND: Durch Squad-Level Swaps ersetzt
      DATUM: 2025-09-27

      Direkter Unit-Austausch wurde durch Squad-basierte Operationen
      ersetzt, die die Datenintegrität besser gewährleisten.
    }
    procedure ExchangeUnits(BaseSquadIndex: Integer; const BaseUnitId: string;
      TargetSquadIndex: Integer; const TargetUnitId: string);

    {
      ARCHIVIERT aus ConquestSave.pas:131/1007
      GRUND: GetSquadNamesAndStages für Bulk-Operationen verwendet
      DATUM: 2025-09-27

      Einzelne Stage-Abfragen wurden durch effizientere Bulk-Operationen
      ersetzt, die mehrere Squads gleichzeitig verarbeiten.
    }
    function GetSquadStage(SquadIndex: Integer): string;
  end;

implementation

{ TMainFormArchive }

procedure TMainFormArchive.PopulateTree(ATree: TTreeView);
var
  I, J: Integer;
  SquadNode, UnitNode: TTreeNode;
  Units: TArray<string>;
  Info: TNodeInfo;
  UnitCaption, SquadCaption: string;
  IsEntity, IsEmpty: Boolean;
  SquadMaxVeterancy: Integer;
  SquadVeterancyText: string;
begin
  {
    ORIGINAL IMPLEMENTATION aus MainForm.pas:738-800

    WARNUNG: Diese Implementierung ist nicht mehr kompatibel!
    Dependencies: FAllSquadNames, FSave, ChkOnlyHumans, GetSquadMaxVeterancy,
                  GetVeterancyDisplay, IsEmptySlot, IsEntityUnit, GetUnitDisplayName

    ERSETZT DURCH: UpdateTreeViewsFromSquads (verwendet FSquads-Cache)
  }
  raise Exception.Create('TMainFormArchive.PopulateTree: ' +
    'Archivierte Methode - UpdateTreeViewsFromSquads verwenden! ' +
    'Siehe Kommentare für Dependencies.');

  { ORIGINAL CODE:
  for I := 0 to Length(FAllSquadNames) - 1 do
  begin
    Info := TNodeInfo.Create;
    Info.Kind := nkSquad;
    Info.SquadIndex := I;
    Info.UnitId := '';

    // Squad-Namen mit höchstem Veteranenstatus erstellen
    SquadCaption := FAllSquadNames[I];

    // Höchsten Veteranenstatus im Squad ermitteln
    SquadMaxVeterancy := GetSquadMaxVeterancy(I);
    SquadVeterancyText := GetVeterancyDisplay(SquadMaxVeterancy);
    SquadCaption := SquadCaption + SquadVeterancyText;

    SquadNode := ATree.Items.AddObject(nil, SquadCaption, Info);

    Units := FSave.GetSquadMembers(I);
    for J := 0 to Length(Units) - 1 do
    begin
      IsEmpty := IsEmptySlot(Units[J]);

      if IsEmpty then
      begin
        // Leere Slots als "Leer" anzeigen
        UnitCaption := Units[J] + ' – [Leer]';
        IsEntity := False;
      end
      else
      begin
        IsEntity := IsEntityUnit(Units[J]);

        // Entity-Units bei aktivem Filter überspringen
        if ChkOnlyHumans.Checked and IsEntity then
          Continue;

        // Vollständigen Anzeigename mit Veteranenstatus erstellen
        UnitCaption := GetUnitDisplayName(Units[J]);
      end;

      Info := TNodeInfo.Create;
      Info.Kind := nkUnit;
      Info.SquadIndex := I;
      Info.UnitId := Units[J];

      UnitNode := ATree.Items.AddChildObject(SquadNode, UnitCaption, Info);

      // Entity-Units visuell markieren
      if IsEntity then
        UnitNode.ImageIndex := 1; // Falls TreeView Images verwendet
    end;

    SquadNode.Expand(False);
  end;
  }
end;

procedure TMainFormArchive.UpdateSquadVeterancy(ASquadIndex: Integer);
var
  I: Integer;
  UnitDetails: TUnitDetails;
  UnitInfo: TUnitInfo;
  MaxVeterancy: Integer;
begin
  {
    ORIGINAL IMPLEMENTATION aus MainForm.pas:974-1002

    PERFORMANCE-PROBLEM: Direkte GetUnitInfo/GetUnitDetails-Aufrufe
    Dependencies: FSquads, IsEmptySlot, FSave.GetUnitInfo, FSave.GetUnitDetails

    ERSETZT DURCH: UpdateSquadVeterancyFromCache (verwendet gecachte Daten)
  }
  raise Exception.Create('TMainFormArchive.UpdateSquadVeterancy: ' +
    'Archivierte Methode - UpdateSquadVeterancyFromCache verwenden! ' +
    'Performance-Problem durch direkte Datei-Zugriffe.');

  { ORIGINAL CODE:
  MaxVeterancy := 0;

  for I := 0 to Length(FSquads[ASquadIndex].UnitIds) - 1 do
  begin
    if IsEmptySlot(FSquads[ASquadIndex].UnitIds[I]) then
      Continue;

    try
      UnitInfo := FSave.GetUnitInfo(FSquads[ASquadIndex].UnitIds[I]);
      if not SameText(UnitInfo.Kind, 'Human') then
        Continue;

      UnitDetails := FSave.GetUnitDetails(FSquads[ASquadIndex].UnitIds[I]);
      if UnitDetails.Veterancy > MaxVeterancy then
        MaxVeterancy := UnitDetails.Veterancy;
    except
      Continue;
    end;
  end;

  FSquads[ASquadIndex].MaxVeterancy := MaxVeterancy;
  }
end;

procedure TMainFormArchive.RestoreFocusToSquad(ATree: TTreeView; ASquadIndex: Integer);
var
  I: Integer;
  Node: TTreeNode;
  Info: TNodeInfo;
begin
  {
    ORIGINAL IMPLEMENTATION aus MainForm.pas:1306-1331

    Dependencies: FAllSquadNames
    ERSETZT DURCH: RestoreFocusToUnit für granularere Unit-spezifische Kontrolle
  }
  raise Exception.Create('TMainFormArchive.RestoreFocusToSquad: ' +
    'Archivierte Methode - RestoreFocusToUnit verwenden! ' +
    'Granularere Kontrolle auf Unit-Ebene verfügbar.');

  { ORIGINAL CODE:
  if (ASquadIndex < 0) or (ASquadIndex >= Length(FAllSquadNames)) then
    Exit;

  // Suche den Squad-Knoten mit dem passenden Index
  for I := 0 to ATree.Items.Count - 1 do
  begin
    Node := ATree.Items[I];
    if Assigned(Node.Data) then
    begin
      Info := TNodeInfo(Node.Data);
      if (Info.Kind = nkSquad) and (Info.SquadIndex = ASquadIndex) then
      begin
        // Fokus setzen und Squad expandieren
        ATree.Selected := Node;
        Node.Expand(False);
        Exit;
      end;
    end;
  end;
  }
end;

{ TConquestSaveArchive }

procedure TConquestSaveArchive.SaveToSave;
begin
  {
    ORIGINAL IMPLEMENTATION aus ConquestSave.pas:281-286

    SICHERHEITSHINWEIS: Diese Methode überschreibt die Original-Datei
    ohne Backup. Das aktuelle Design verwendet SaveToSaveAs für alle
    Speichervorgänge aus Sicherheitsgründen.

    Dependencies: FSaveFile
  }
  raise Exception.Create('TConquestSaveArchive.SaveToSave: ' +
    'Archivierte Methode - SaveToSaveAs für sichere Speicherung verwenden!');

  { ORIGINAL CODE:
  if FSaveFile = '' then
    raise EConquestSave.Create('No original .sav path set. Use SaveToSaveAs.');
  SaveToSaveAs(FSaveFile);
  }
end;

procedure TConquestSaveArchive.MoveUnit(BaseSquadIndex: Integer;
  const UnitId: string; TargetSquadIndex: Integer);
var
  BaseLine, TargetLine: string;
begin
  {
    ORIGINAL IMPLEMENTATION aus ConquestSave.pas:801-817

    HINWEIS: Diese Implementierung wurde durch ein robusteres
    Swap-System ersetzt, das bessere Datenintegrität gewährleistet.

    Dependencies: GetSquadLine, RemoveUnitIdFromSquadLine,
                  AppendUnitIdToSquadLine, SetSquadLine
  }
  raise Exception.Create('TConquestSaveArchive.MoveUnit: ' +
    'Archivierte Methode - Swap-Operationen verwenden!');

  { ORIGINAL CODE:
  if BaseSquadIndex = TargetSquadIndex then
    Exit; // nothing to do

  BaseLine := GetSquadLine(BaseSquadIndex);
  TargetLine := GetSquadLine(TargetSquadIndex);

  BaseLine := RemoveUnitIdFromSquadLine(BaseLine, UnitId);
  TargetLine := AppendUnitIdToSquadLine(TargetLine, UnitId);

  SetSquadLine(BaseSquadIndex, BaseLine);
  SetSquadLine(TargetSquadIndex, TargetLine);
  }
end;

procedure TConquestSaveArchive.ExchangeUnits(BaseSquadIndex: Integer;
  const BaseUnitId: string; TargetSquadIndex: Integer; const TargetUnitId: string);
var
  BaseLine, TargetLine: string;
begin
  {
    ORIGINAL IMPLEMENTATION aus ConquestSave.pas:819-840

    ERSETZT DURCH: Squad-Level Swap-Mechanismus für bessere Konsistenz

    Dependencies: GetSquadLine, RemoveUnitIdFromSquadLine,
                  AppendUnitIdToSquadLine, SetSquadLine
  }
  raise Exception.Create('TConquestSaveArchive.ExchangeUnits: ' +
    'Archivierte Methode - Squad-Level Swaps verwenden!');

  { ORIGINAL CODE:
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
  }
end;

function TConquestSaveArchive.GetSquadStage(SquadIndex: Integer): string;
var
  Line: string;
  M: TMatch;
begin
  {
    ORIGINAL IMPLEMENTATION aus ConquestSave.pas:1007-1018

    PERFORMANCE-HINWEIS: Diese Methode war für Einzelabfragen konzipiert.
    GetSquadNamesAndStages ist effizienter für Bulk-Operationen.

    Dependencies: GetSquadLine, TRegEx
  }
  raise Exception.Create('TConquestSaveArchive.GetSquadStage: ' +
    'Archivierte Methode - GetSquadNamesAndStages für Bulk-Ops verwenden!');

  { ORIGINAL CODE:
  Line := GetSquadLine(SquadIndex);
  M := TRegEx.Match(Line, '"[^"]*"\s+"([^"]*)"');
  if M.Success then
    Result := M.Groups[1].Value
  else
    Result := '';
  }

  Result := ''; // Compiler-Warnung vermeiden
end;

end.