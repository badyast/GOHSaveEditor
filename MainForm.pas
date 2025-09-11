{ TODO 1 -oDaniel -cVital : Problem mit Statusdatei und Name des Spielstands lösen }
unit MainForm;

interface

uses
  ujachLogAuto, ujachLogMgr,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ComCtrls,
  System.Generics.Collections, System.IOUtils, ConquestSave;

type
  TNodeKind = (nkSquad, nkUnit);

  TNodeInfo = class
  public
    Kind: TNodeKind;
    SquadIndex: Integer;
    UnitId: string; // nur bei nkUnit gefüllt
  end;

  TSquadData = record
    Name: string;
    UnitIds: TArray<string>;
    UnitNames: TArray<string>; // ← NEU: Gecachte Namen
    UnitVeterancies: TArray<Integer>; // ← NEU: Gecachte Veteranenstufen
    UnitKinds: TArray<string>;
    MaxVeterancy: Integer;
  end;

  TFrmMain = class(TForm)
    BtnOpen: TButton;
    LblSave: TLabel;
    TreeBase: TTreeView;
    TreeTarget: TTreeView;
    BtnTransfer: TButton;
    BtnSwap: TButton;
    BtnSaveAs: TButton;
    BtnExportCsv: TButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    LblBaseInfo: TLabel;
    LblTargetInfo: TLabel;
    ChkOnlyHumans: TCheckBox;
    PageControl1: TPageControl;
    TabGeneral: TTabSheet;
    TabInventory: TTabSheet;
    MemoInfo: TMemo;
    ListViewInventory: TListView;
    TabDebug: TTabSheet;
    MemoDebug: TMemo;
    // ← NEU: Komponenten für Ladefortschritt
    ProgressBar1: TProgressBar;
    LblStatus: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtnOpenClick(Sender: TObject);
    procedure BtnTransferClick(Sender: TObject);
    procedure BtnSwapClick(Sender: TObject);
    procedure BtnSaveAsClick(Sender: TObject);
    procedure BtnExportCsvClick(Sender: TObject);
    procedure TreeBaseChange(Sender: TObject; Node: TTreeNode);
    procedure TreeTargetChange(Sender: TObject; Node: TTreeNode);
    procedure ChkOnlyHumansClick(Sender: TObject);
    procedure TreeCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
      State: TCustomDrawState; var DefaultDraw: Boolean);
  private
    FSquads: TArray<TSquadData>; // Live-Datenstruktur
    FSquadsDirty: Boolean; // Änderungen pending?
    FSave: TConquestSave;
    FAllSquadNames: TArray<string>;
    procedure SetControlsEnabled(AEnabled: Boolean);
    procedure ClearTreeData(ATree: TTreeView);
    procedure PopulateTrees;
    procedure PopulateTree(ATree: TTreeView);
    function SelectedUnitInfo(ATree: TTreeView): TNodeInfo;
    function SelectedSquadIndex(ATree: TTreeView): Integer;
    procedure UpdateInfoLabels;
    procedure ShowStatus(const Msg: string);
    function IsEmptySlot(const UnitId: string): Boolean;
    function IsValidUnit(const UnitId: string): Boolean;
    function IsEntityUnit(const UnitId: string): Boolean;
    procedure RestoreFocusToSquad(ATree: TTreeView; ASquadIndex: Integer);
    procedure UpdateUnitInfoPanel(const UnitId: string);
    procedure ClearInfoPanel;
    function GetVeterancyDisplay(AVeterancy: Integer): string;
    function GetUnitDisplayName(const UnitId: string): string;
    function GetSquadMaxVeterancy(ASquadIndex: Integer): Integer;
    procedure CopyTreeStructure(ASourceTree, ATargetTree: TTreeView);
    procedure LoadSquadsFromSave;
    procedure UpdateTreeViewsFromSquads;
    procedure UpdateSquadVeterancy(ASquadIndex: Integer);
    procedure SwapUnitsInSquads(ASquadA, AUnitA: Integer;
      ASquadB, AUnitB: Integer; const AUnitIdA, AUnitIdB: string);
    procedure RebuildFCampaignFromSquads;
    procedure UpdateSquadVeterancyFromCache(ASquadIndex: Integer);
    procedure RestoreFocusToUnit(ATree: TTreeView; ASquadIndex: Integer;
      const AUnitId: string);

    // ← NEU: Fortschritts- und Status-Methoden
    procedure ShowProgress(const AMessage: string; AMaxValue: Integer = 0);
    procedure UpdateProgress(ACurrentValue: Integer;
      const AMessage: string = '');
    procedure HideProgress;
    procedure SetUILoadingState(ALoading: Boolean);
  public
  end;

var
  FrmMain: TFrmMain;

implementation

{$R *.dfm}

procedure TFrmMain.FormCreate(Sender: TObject);
begin
  jachLog.LogInfo('=== MainForm-Initialisierung gestartet ===');
  jachLog.LogDebug('FormCreate: Beginne UI-Setup');

  try
    // ConquestSave-Objekt erstellen
    jachLog.LogDebug('Erstelle ConquestSave-Instanz');
    FSave := TConquestSave.Create;
    jachLog.LogDebug('ConquestSave-Objekt erfolgreich erstellt');

    // UI-Initialisierung
    jachLog.LogDebug('Konfiguriere UI-Komponenten');
    SetControlsEnabled(False);
    jachLog.LogDebug('Controls initial deaktiviert');

    // Dialog-Filter setzen
    OpenDialog1.Filter := 'Gates of Hell Save (*.sav)|*.sav';
    SaveDialog1.Filter := 'Gates of Hell Save (*.sav)|*.sav|CSV-Datei (*.csv)|*.csv';
    jachLog.LogDebug('Dialog-Filter konfiguriert');

    // Labels initialisieren
    LblBaseInfo.Caption := 'Quelle: (keine Auswahl)';
    LblTargetInfo.Caption := 'Ziel: (keine Auswahl)';
    jachLog.LogDebug('Info-Labels initialisiert');

    // TreeView Event-Handler
    TreeBase.OnCustomDrawItem := TreeCustomDrawItem;
    TreeTarget.OnCustomDrawItem := TreeCustomDrawItem;
    jachLog.LogDebug('TreeView Event-Handler konfiguriert');

    // UI-Komponenten finalisieren
    ClearInfoPanel;
    PageControl1.ActivePageIndex := 0;
    HideProgress;
    jachLog.LogDebug('UI-Komponenten finalisiert');

    jachLog.LogInfo('MainForm-Initialisierung erfolgreich abgeschlossen');

  except
    on E: Exception do
    begin
      jachLog.LogError('FEHLER in FormCreate', E);
      jachLog.LogCritical('MainForm-Initialisierung fehlgeschlagen - Anwendung instabil');
      raise;
    end;
  end;
end;

// ← NEU: Fortschritts-Methoden
procedure TFrmMain.ShowProgress(const AMessage: string; AMaxValue: Integer = 0);
begin
  LblStatus.Caption := AMessage;
  LblStatus.Visible := True;

  if AMaxValue > 0 then
  begin
    ProgressBar1.Max := AMaxValue;
    ProgressBar1.Position := 0;
    ProgressBar1.Style := pbstNormal;
  end
  else
  begin
    // Unbestimmter Fortschritt (Marquee-Style)
    ProgressBar1.Style := pbstMarquee;
  end;

  ProgressBar1.Visible := True;

  // UI aktualisieren damit die Änderungen sofort sichtbar sind
  Application.ProcessMessages;
end;

procedure TFrmMain.UpdateProgress(ACurrentValue: Integer;
  const AMessage: string = '');
begin
  if AMessage <> '' then
    LblStatus.Caption := AMessage;

  if ProgressBar1.Style = pbstNormal then
    ProgressBar1.Position := ACurrentValue;

  // UI-Updates alle 10 Schritte oder bei wichtigen Meilensteinen
  if (ACurrentValue mod 10 = 0) or (AMessage <> '') then
    Application.ProcessMessages;
end;

procedure TFrmMain.HideProgress;
begin
  ProgressBar1.Visible := False;
  LblStatus.Visible := False;
  LblStatus.Caption := '';
end;

procedure TFrmMain.SetUILoadingState(ALoading: Boolean);
begin
  // Alle Hauptcontrols während des Ladens deaktivieren
  BtnOpen.Enabled := not ALoading;
  TreeBase.Enabled := not ALoading;
  TreeTarget.Enabled := not ALoading;
  ChkOnlyHumans.Enabled := not ALoading;
  BtnTransfer.Enabled := not ALoading;
  BtnSwap.Enabled := not ALoading;
  BtnSaveAs.Enabled := not ALoading;
  BtnExportCsv.Enabled := not ALoading;

  if ALoading then
  begin
    Screen.Cursor := crHourGlass;
    Caption := 'GOH Savegame Editor - Lade...';
  end
  else
  begin
    Screen.Cursor := crDefault;
    Caption := 'GOH Savegame Editor';
  end;
end;

procedure TFrmMain.FormDestroy(Sender: TObject);
begin
  jachLog.LogInfo('=== MainForm wird beendet ===');
  jachLog.LogDebug('FormDestroy: Beginne Cleanup');

  try
    jachLog.LogDebug('Bereinige TreeView-Daten');
    ClearTreeData(TreeBase);
    ClearTreeData(TreeTarget);
    jachLog.LogDebug('TreeView-Daten bereinigt');

    jachLog.LogDebug('Gebe ConquestSave-Objekt frei');
    FSave.Free;
    jachLog.LogDebug('ConquestSave-Objekt freigegeben');

    jachLog.LogInfo('MainForm-Cleanup erfolgreich abgeschlossen');

  except
    on E: Exception do
    begin
      jachLog.LogWarning('Fehler beim MainForm-Cleanup (nicht kritisch): %s', [E.Message]);
      // Nicht re-raisen, da wir beim Beenden sind
    end;
  end;
end;

procedure TFrmMain.SetControlsEnabled(AEnabled: Boolean);
begin
  TreeBase.Enabled := AEnabled;
  TreeTarget.Enabled := AEnabled;
  ChkOnlyHumans.Enabled := AEnabled;
  BtnTransfer.Enabled := AEnabled;
  BtnSwap.Enabled := AEnabled;
  BtnSaveAs.Enabled := AEnabled;
  BtnExportCsv.Enabled := AEnabled;
end;

procedure TFrmMain.ClearTreeData(ATree: TTreeView);
var
  I: Integer;
  N: TTreeNode;
begin
  // Alle TNodeInfo freigeben
  for I := 0 to ATree.Items.Count - 1 do
  begin
    N := ATree.Items[I];
    if Assigned(N.Data) then
    begin
      TObject(N.Data).Free;
      N.Data := nil;
    end;
  end;
  ATree.Items.Clear;
end;

function TFrmMain.IsEmptySlot(const UnitId: string): Boolean;
begin
  Result := SameText(UnitId, '0xffffffff');
end;

function TFrmMain.IsValidUnit(const UnitId: string): Boolean;
begin
  Result := not IsEmptySlot(UnitId);
end;

procedure TFrmMain.LoadSquadsFromSave;
var
  I, J: Integer;
  SquadNames: TArray<string>;
  UnitInfo: TUnitInfo;
  UnitDetails: TUnitDetails;
  MaxVet: Integer;
  TotalUnits, ProcessedUnits: Integer;
begin
  SquadNames := FSave.GetSquadNames;
  SetLength(FSquads, Length(SquadNames));

  // ← NEU: Gesamtanzahl der Units für Fortschrittsanzeige berechnen
  TotalUnits := 0;
  for I := 0 to Length(SquadNames) - 1 do
    TotalUnits := TotalUnits + Length(FSave.GetSquadMembers(I));

  ProcessedUnits := 0;
  ShowProgress('Lade Unit-Daten...', TotalUnits);

  for I := 0 to Length(SquadNames) - 1 do
  begin
    UpdateProgress(ProcessedUnits, Format('Verarbeite Squad %d/%d: %s',
      [I + 1, Length(SquadNames), SquadNames[I]]));

    FSquads[I].Name := SquadNames[I];
    FSquads[I].UnitIds := FSave.GetSquadMembers(I);

    // Arrays für gecachte Daten vorbereiten
    SetLength(FSquads[I].UnitNames, Length(FSquads[I].UnitIds));
    SetLength(FSquads[I].UnitVeterancies, Length(FSquads[I].UnitIds));
    SetLength(FSquads[I].UnitKinds, Length(FSquads[I].UnitIds));

    MaxVet := 0;

    // Alle Unit-Daten EINMAL laden und cachen
    for J := 0 to Length(FSquads[I].UnitIds) - 1 do
    begin
      Inc(ProcessedUnits);

      if IsEmptySlot(FSquads[I].UnitIds[J]) then
      begin
        FSquads[I].UnitNames[J] := '[Leer]';
        FSquads[I].UnitVeterancies[J] := 0;
        FSquads[I].UnitKinds[J] := 'Empty';
      end
      else
      begin
        try
          UnitInfo := FSave.GetUnitInfo(FSquads[I].UnitIds[J]);
          FSquads[I].UnitNames[J] := UnitInfo.Name;
          FSquads[I].UnitKinds[J] := UnitInfo.Kind;

          // Veterancy nur für menschliche Einheiten
          if SameText(UnitInfo.Kind, 'Human') then
          begin
            try
              UnitDetails := FSave.GetUnitDetails(FSquads[I].UnitIds[J]);
              FSquads[I].UnitVeterancies[J] := UnitDetails.Veterancy;
              if UnitDetails.Veterancy > MaxVet then
                MaxVet := UnitDetails.Veterancy;
            except
              FSquads[I].UnitVeterancies[J] := 0;
            end;
          end
          else
            FSquads[I].UnitVeterancies[J] := 0;

        except
          FSquads[I].UnitNames[J] := '(Fehler)';
          FSquads[I].UnitVeterancies[J] := 0;
          FSquads[I].UnitKinds[J] := 'Unknown';
        end;
      end;

      // ← NEU: Fortschritt alle 25 Units oder am Ende jedes Squads aktualisieren
      if (ProcessedUnits mod 25 = 0) or (J = Length(FSquads[I].UnitIds) - 1)
      then
        UpdateProgress(ProcessedUnits);
    end;

    FSquads[I].MaxVeterancy := MaxVet;
  end;

  FSquadsDirty := False;

  // ← NEU: Fortschritt verstecken
  HideProgress;
end;

function TFrmMain.IsEntityUnit(const UnitId: string): Boolean;
var
  UnitInfo: TUnitInfo;
begin
  Result := False;

  // Leere Slots sind keine Entities
  if IsEmptySlot(UnitId) then
    Exit(False);

  try
    UnitInfo := FSave.GetUnitInfo(UnitId);
    Result := SameText(UnitInfo.Kind, 'Entity');
  except
    // Bei Fehlern nehmen wir an, dass es kein Entity ist
    Result := False;
  end;
end;

function TFrmMain.GetVeterancyDisplay(AVeterancy: Integer): string;
begin
  /// <summary>
  /// Konvertiert Veteranenstufe in Anzeige-String.
  /// Gates of Hell verwendet Veteranenstufen 0-8.
  /// Niedrige Stufen (1-3) als Sterne, höhere Stufen (4-8) als römische Zahlen.
  /// </summary>
  case AVeterancy of
    0:
      Result := ''; // Rekrut/Normal - keine Anzeige
    1:
      Result := ' [★]'; // Erfahren
    2:
      Result := ' [★★]'; // Veteran
    3:
      Result := ' [★★★]'; // Elite
    4:
      Result := ' [★IV]'; // Hoch-Elite
    5:
      Result := ' [★V]'; // Meister
    6:
      Result := ' [★VI]'; // Großmeister
    7:
      Result := ' [★VII]'; // Legende
    8:
      Result := ' [★VIII]'; // Held/Maximum
  else
    // Fallback für unerwartete Werte
    if AVeterancy > 8 then
      Result := ' [★×' + IntToStr(AVeterancy) + ']'
    else
      Result := '';
  end;
end;

function TFrmMain.GetSquadMaxVeterancy(ASquadIndex: Integer): Integer;
var
  Units: TArray<string>;
  I: Integer;
  UnitDetails: TUnitDetails;
  UnitInfo: TUnitInfo;
  MaxVeterancy: Integer;
begin
  /// <summary>
  /// Ermittelt die höchste Veteranenstufe aller menschlichen Einheiten in einem Squad.
  /// Entities werden ignoriert, da sie keine Veteranenstufen haben.
  /// </summary>
  Result := 0;
  MaxVeterancy := 0;

  try
    Units := FSave.GetSquadMembers(ASquadIndex);

    for I := 0 to Length(Units) - 1 do
    begin
      // Leere Slots überspringen
      if IsEmptySlot(Units[I]) then
        Continue;

      try
        // Nur menschliche Einheiten prüfen
        UnitInfo := FSave.GetUnitInfo(Units[I]);
        if not SameText(UnitInfo.Kind, 'Human') then
          Continue;

        // Veteranenstatus abrufen
        UnitDetails := FSave.GetUnitDetails(Units[I]);
        if UnitDetails.Veterancy > MaxVeterancy then
          MaxVeterancy := UnitDetails.Veterancy;

      except
        // Bei Fehlern diese Unit überspringen
        Continue;
      end;
    end;

    Result := MaxVeterancy;
  except
    // Bei generellen Fehlern 0 zurückgeben
    Result := 0;
  end;
end;

function TFrmMain.GetUnitDisplayName(const UnitId: string): string;
var
  UnitInfo: TUnitInfo;
  UnitDetails: TUnitDetails;
  VeterancyText: string;
begin
  /// <summary>
  /// Erstellt den vollständigen Anzeigename für eine Unit
  /// inklusive Veteranenstatus für menschliche Einheiten.
  /// </summary>
  Result := UnitId;

  try
    // Basis-Informationen abrufen
    UnitInfo := FSave.GetUnitInfo(UnitId);

    // Unit-Name hinzufügen wenn vorhanden
    if UnitInfo.Name <> '' then
      Result := UnitId + ' – ' + UnitInfo.Name;

    // Veteranenstatus nur für menschliche Einheiten anzeigen
    if SameText(UnitInfo.Kind, 'Human') then
    begin
      try
        UnitDetails := FSave.GetUnitDetails(UnitId);
        VeterancyText := GetVeterancyDisplay(UnitDetails.Veterancy);
        Result := Result + VeterancyText;
      except
        // Falls GetUnitDetails fehlschlägt, ignorieren wir den Veteranenstatus
      end;
    end;

  except
    // Fallback: nur ID anzeigen bei Fehlern
    Result := UnitId;
  end;
end;

procedure TFrmMain.PopulateTrees;
begin
  UpdateTreeViewsFromSquads;
end;

procedure TFrmMain.CopyTreeStructure(ASourceTree, ATargetTree: TTreeView);

  function CopyNodeRecursive(ASourceNode: TTreeNode; ATargetParent: TTreeNode)
    : TTreeNode;
  var
    NewInfo: TNodeInfo;
    SourceInfo: TNodeInfo;
    I: Integer;
  begin
    // Neue TNodeInfo für den Zielknoten erstellen
    SourceInfo := TNodeInfo(ASourceNode.Data);
    NewInfo := TNodeInfo.Create;
    NewInfo.Kind := SourceInfo.Kind;
    NewInfo.SquadIndex := SourceInfo.SquadIndex;
    NewInfo.UnitId := SourceInfo.UnitId;

    // Knoten in Zieltree erstellen
    if Assigned(ATargetParent) then
      Result := ATargetTree.Items.AddChildObject(ATargetParent,
        ASourceNode.Text, NewInfo)
    else
      Result := ATargetTree.Items.AddObject(nil, ASourceNode.Text, NewInfo);

    // Rekursiv alle Kindknoten kopieren
    for I := 0 to ASourceNode.Count - 1 do
      CopyNodeRecursive(ASourceNode.Item[I], Result);
  end;

var
  I: Integer;
begin
  /// <summary>
  /// Kopiert die komplette TreeView-Struktur inklusive aller TNodeInfo-Objekte
  /// von einer Quell-TreeView in eine Ziel-TreeView. Dies ist deutlich schneller
  /// als ein erneutes Parsen aller Veteranenstatus-Informationen.
  /// </summary>

  // Ziel-TreeView leeren (ohne die Quelle zu beeinträchtigen)
  ClearTreeData(ATargetTree);

  // Alle Root-Knoten kopieren
  for I := 0 to ASourceTree.Items.Count - 1 do
  begin
    if ASourceTree.Items[I].Parent = nil then // Nur Root-Knoten
      CopyNodeRecursive(ASourceTree.Items[I], nil);
  end;
end;

procedure TFrmMain.PopulateTree(ATree: TTreeView);
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
    end;
  end;
end;

function TFrmMain.SelectedUnitInfo(ATree: TTreeView): TNodeInfo;
begin
  Result := nil;
  if Assigned(ATree.Selected) and Assigned(ATree.Selected.Data) then
    if TNodeInfo(ATree.Selected.Data).Kind = nkUnit then
      Result := TNodeInfo(ATree.Selected.Data);
end;

function TFrmMain.SelectedSquadIndex(ATree: TTreeView): Integer;
var
  N: TTreeNode;
  Info: TNodeInfo;
begin
  Result := -1;
  N := ATree.Selected;
  if not Assigned(N) then
    Exit;

  Info := TNodeInfo(N.Data);
  if not Assigned(Info) then
    Exit;

  // Egal ob Squad- oder Unit-Knoten: SquadIndex steckt drin
  Result := Info.SquadIndex;
end;

procedure TFrmMain.BtnOpenClick(Sender: TObject);
var
  StartTime: TDateTime;
  FileSize: Int64;
  ElapsedMs: Integer;
  FileName: string;
begin
  jachLog.LogDebug('Benutzer klickte "Save laden" Button');

  if not OpenDialog1.Execute then
  begin
    jachLog.LogDebug('Benutzer hat Dateiauswahl-Dialog abgebrochen');
    Exit;
  end;

  FileName := OpenDialog1.FileName;
  StartTime := Now;

  jachLog.LogInfo('LADE-OPERATION gestartet für: %s', [ExtractFileName(FileName)]);

  // Datei-Vorab-Informationen loggen
  try
    FileSize := TFile.GetSize(FileName);
    jachLog.LogInfo('Dateigröße: %.2f MB (%d Bytes)', [FileSize / (1024*1024), FileSize]);

    if FileSize > 100 * 1024 * 1024 then // > 100MB
      jachLog.LogWarning('Große Savegame-Datei (%.2f MB) - Laden könnte länger dauern', [FileSize / (1024*1024)]);

  except
    on E: Exception do
      jachLog.LogWarning('Konnte Dateigröße nicht ermitteln: %s', [E.Message]);
  end;

  jachLog.LogDebug('Setze UI in Lade-Zustand');
  SetUILoadingState(True);

  try
    ShowProgress('Lade Savegame...');
    jachLog.LogDebug('Fortschrittsanzeige aktiviert');

    jachLog.LogDebug('Rufe FSave.LoadFromSave auf');
    FSave.LoadFromSave(FileName);
    LblSave.Caption := ExtractFileName(FileName);
    jachLog.LogInfo('ZIP-Datei erfolgreich geladen und extrahiert');

    jachLog.LogDebug('Beginne Squad-Daten-Verarbeitung');
    LoadSquadsFromSave;
    jachLog.LogInfo('Squad-Daten erfolgreich verarbeitet');

    jachLog.LogDebug('Aktualisiere TreeView-Anzeige');
    UpdateTreeViewsFromSquads;
    jachLog.LogInfo('TreeView-Anzeige aktualisiert');

    SetControlsEnabled(True);
    ShowStatus('Save geladen: ' + ExtractFileName(FileName));
    ClearInfoPanel;
    jachLog.LogDebug('UI in Normal-Zustand zurückgesetzt');

    ElapsedMs := Round((Now - StartTime) * 24 * 60 * 60 * 1000);
    jachLog.LogInfo('LADE-OPERATION erfolgreich abgeschlossen in %d ms', [ElapsedMs]);

    if ElapsedMs > 10000 then // > 10 Sekunden
      jachLog.LogWarning('Langsame Lade-Performance: %d ms (> 10s)', [ElapsedMs]);

  except
    on E: Exception do
    begin
      jachLog.LogError('LADE-OPERATION fehlgeschlagen für: %s', [FileName], E);
      jachLog.LogError('Exception-Details: %s: %s', [E.ClassName, E.Message]);

      SetControlsEnabled(False);
      ShowStatus('Fehler beim Laden!');
      jachLog.LogDebug('UI auf Fehler-Zustand gesetzt');

      Application.MessageBox(PChar('Fehler beim Laden: ' + E.Message), 'Fehler', MB_ICONERROR);
    end;
  end;

  SetUILoadingState(False);
  HideProgress;
  jachLog.LogDebug('Lade-UI-Zustand beendet');
end;

procedure TFrmMain.UpdateInfoLabels;
var
  U: TNodeInfo;
  I: Integer;
  Info: TUnitInfo;
  S: string;
begin
  // Base
  S := 'Quelle: ';
  U := SelectedUnitInfo(TreeBase);
  if Assigned(U) then
  begin
    if IsEmptySlot(U.UnitId) then
      S := S + Format('Squad %d, [Leer]', [U.SquadIndex])
    else
    begin
      S := S + Format('Squad %d, Unit %s', [U.SquadIndex, U.UnitId]);
      try
        Info := FSave.GetUnitInfo(U.UnitId);
        if (Info.Kind <> '') or (Info.Name <> '') then
          S := S + Format(' (%s; %s)', [Info.Kind, Info.Name]);
      except
        // ignore
      end;
    end;
  end
  else
    S := S + '(keine Auswahl)';
  LblBaseInfo.Caption := S;

  // Target
  S := 'Ziel: ';
  U := SelectedUnitInfo(TreeTarget);
  if Assigned(U) then
  begin
    if IsEmptySlot(U.UnitId) then
      S := S + Format('Squad %d, [Leer]', [U.SquadIndex])
    else
    begin
      S := S + Format('Squad %d, Unit %s', [U.SquadIndex, U.UnitId]);
      try
        Info := FSave.GetUnitInfo(U.UnitId);
        if (Info.Kind <> '') or (Info.Name <> '') then
          S := S + Format(' (%s; %s)', [Info.Kind, Info.Name]);
      except
        // ignore
      end;
    end;
  end
  else
  begin
    I := SelectedSquadIndex(TreeTarget);
    if I >= 0 then
      S := S + Format('Squad %d', [I])
    else
      S := S + '(keine Auswahl)';
  end;
  LblTargetInfo.Caption := S;
end;

procedure TFrmMain.UpdateSquadVeterancy(ASquadIndex: Integer);
var
  I: Integer;
  UnitDetails: TUnitDetails;
  UnitInfo: TUnitInfo;
  MaxVeterancy: Integer;
begin
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
end;

procedure TFrmMain.UpdateTreeViewsFromSquads;
var
  I, J: Integer;
  SquadNode, UnitNode: TTreeNode;
  Info: TNodeInfo;
  UnitCaption, SquadCaption: string;
  IsEntity, IsEmpty: Boolean;
  SquadVeterancyText, UnitVeterancyText: string;
begin
  ClearTreeData(TreeBase);
  ClearTreeData(TreeTarget);

  for I := 0 to Length(FSquads) - 1 do
  begin
    // Squad-Knoten erstellen
    Info := TNodeInfo.Create;
    Info.Kind := nkSquad;
    Info.SquadIndex := I;
    Info.UnitId := '';

    SquadCaption := FSquads[I].Name;
    SquadVeterancyText := GetVeterancyDisplay(FSquads[I].MaxVeterancy);
    SquadCaption := SquadCaption + SquadVeterancyText;

    SquadNode := TreeBase.Items.AddObject(nil, SquadCaption, Info);

    // Unit-Knoten erstellen
    for J := 0 to Length(FSquads[I].UnitIds) - 1 do
    begin
      IsEmpty := IsEmptySlot(FSquads[I].UnitIds[J]);

      if IsEmpty then
      begin
        UnitCaption := FSquads[I].UnitIds[J] + ' – [Leer]';
        IsEntity := False;
      end
      else
      begin
        IsEntity := SameText(FSquads[I].UnitKinds[J], 'Entity'); // ← Aus Cache!
        if ChkOnlyHumans.Checked and IsEntity then
          Continue;

        // CACHED DATEN verwenden - KEIN PARSING!
        UnitCaption := FSquads[I].UnitIds[J];
        if FSquads[I].UnitNames[J] <> '' then
          UnitCaption := UnitCaption + ' – ' + FSquads[I].UnitNames[J];

        // Veterancy aus Cache
        UnitVeterancyText := GetVeterancyDisplay(FSquads[I].UnitVeterancies[J]);
        UnitCaption := UnitCaption + UnitVeterancyText;
      end;

      Info := TNodeInfo.Create;
      Info.Kind := nkUnit;
      Info.SquadIndex := I;
      Info.UnitId := FSquads[I].UnitIds[J];
      UnitNode := TreeBase.Items.AddChildObject(SquadNode, UnitCaption, Info);
    end;
  end;

  // TreeTarget durch Kopieren erstellen
  CopyTreeStructure(TreeBase, TreeTarget);

  if TreeBase.Items.Count > 0 then
    TreeBase.Items[0].Expand(True);
  if TreeTarget.Items.Count > 0 then
    TreeTarget.Items[0].Expand(True);
end;

procedure TFrmMain.ClearInfoPanel;
begin
  MemoInfo.Clear;
  MemoInfo.Lines.Add('Keine Unit ausgewählt');
  ListViewInventory.Items.Clear;
end;

procedure TFrmMain.UpdateUnitInfoPanel(const UnitId: string);
var
  Details: TUnitDetails;
  Item: TListItem;
  I: Integer;
  S: string;
begin
  if IsEmptySlot(UnitId) then
  begin
    MemoInfo.Clear;
    MemoInfo.Lines.Add('Leerer Slot');
    ListViewInventory.Items.Clear;
    Exit;
  end;

  try
    Details := FSave.GetUnitDetails(UnitId);

    // Allgemeine Informationen
    MemoInfo.Clear;
    MemoInfo.Lines.Add('=== UNIT INFORMATIONEN ===');
    MemoInfo.Lines.Add('');
    MemoInfo.Lines.Add(Format('Unit ID:        %s', [Details.UnitId]));
    MemoInfo.Lines.Add(Format('Typ:            %s', [Details.Kind]));
    MemoInfo.Lines.Add(Format('Unit-Klasse:    %s', [Details.UnitType]));
    MemoInfo.Lines.Add(Format('Name:           %s', [Details.Name]));
    MemoInfo.Lines.Add('');

    if Details.Position <> '' then
      MemoInfo.Lines.Add(Format('Position:       %s', [Details.Position]));

    if Details.Veterancy > 0 then
      MemoInfo.Lines.Add(Format('Veteranenstufe: %d%s', [Details.Veterancy,
        GetVeterancyDisplay(Details.Veterancy)]));

    if Details.Score > 0 then
      MemoInfo.Lines.Add(Format('Score:          %.2f', [Details.Score]));

    if Details.InfantryKills > 0 then
      MemoInfo.Lines.Add(Format('Infantry Kills: %d', [Details.InfantryKills]));

    MemoInfo.Lines.Add(Format('MID:            %d', [Details.MID]));

    if (Details.NameId1 > 0) or (Details.NameId2 > 0) then
      MemoInfo.Lines.Add(Format('Name-IDs:       %d, %d',
        [Details.NameId1, Details.NameId2]));

    if Details.LastItem <> '' then
      MemoInfo.Lines.Add(Format('Letzte Waffe:   %s', [Details.LastItem]));

    if Details.LastThrowItem <> '' then
      MemoInfo.Lines.Add(Format('Letzte Granate: %s', [Details.LastThrowItem]));

    if Details.FsmState <> '' then
      MemoInfo.Lines.Add(Format('FSM-Status:     %s', [Details.FsmState]));

    // Inventar
    ListViewInventory.Items.Clear;
    for I := 0 to High(Details.Inventory) do
    begin
      Item := ListViewInventory.Items.Add;
      Item.Caption := Details.Inventory[I].ItemName;

      // Typ
      if Details.Inventory[I].ItemType <> '' then
        Item.SubItems.Add(Details.Inventory[I].ItemType)
      else
        Item.SubItems.Add('-');

      // Anzahl
      if Details.Inventory[I].Quantity > 1 then
        Item.SubItems.Add(IntToStr(Details.Inventory[I].Quantity))
      else
        Item.SubItems.Add('1');

      // Position
      Item.SubItems.Add('Cell ' + Details.Inventory[I].Cell);

      // Anmerkung
      if Details.Inventory[I].IsUserItem then
        Item.SubItems.Add('Ausgerüstet')
      else
        Item.SubItems.Add('');
    end;

    // Wenn kein Inventar vorhanden
    if Length(Details.Inventory) = 0 then
    begin
      Item := ListViewInventory.Items.Add;
      Item.Caption := '(Kein Inventar gefunden)';
    end;

  except
    on E: Exception do
    begin
      MemoInfo.Clear;
      MemoInfo.Lines.Add('Fehler beim Laden der Unit-Details:');
      MemoInfo.Lines.Add(E.Message);
      ListViewInventory.Items.Clear;
    end;
  end;
end;

procedure TFrmMain.TreeBaseChange(Sender: TObject; Node: TTreeNode);
var
  U: TNodeInfo;
begin
  UpdateInfoLabels;

  // Update Info-Panel wenn eine Unit ausgewählt ist
  U := SelectedUnitInfo(TreeBase);
  if Assigned(U) then
    UpdateUnitInfoPanel(U.UnitId)
  else
    ClearInfoPanel;
end;

procedure TFrmMain.TreeTargetChange(Sender: TObject; Node: TTreeNode);
var
  U: TNodeInfo;
begin
  UpdateInfoLabels;

  // Update Info-Panel wenn eine Unit ausgewählt ist (Target hat Priorität)
  U := SelectedUnitInfo(TreeTarget);
  if Assigned(U) then
    UpdateUnitInfoPanel(U.UnitId);
end;

procedure TFrmMain.ChkOnlyHumansClick(Sender: TObject);
begin
  // Bei Änderung der Checkbox die TreeViews neu laden
  if Assigned(FSave) then
  begin
    PopulateTrees;
    ClearInfoPanel;
  end;
end;

procedure TFrmMain.TreeCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
  State: TCustomDrawState; var DefaultDraw: Boolean);
var
  Info: TNodeInfo;
  Canvas: TCanvas;
begin
  DefaultDraw := True;
  Canvas := Sender.Canvas;

  if Assigned(Node.Data) then
  begin
    Info := TNodeInfo(Node.Data);
    if Info.Kind = nkUnit then
    begin
      if IsEmptySlot(Info.UnitId) then
      begin
        // Leere Slots ausgegraut und kursiv
        Canvas.Font.Color := clGray;
        Canvas.Font.Style := [fsItalic];
      end
      else
      begin
        // Normale Units
        Canvas.Font.Color := clWindowText;
        Canvas.Font.Style := [];
      end;
    end
    else
    begin
      // Squad-Knoten normal darstellen
      Canvas.Font.Color := clWindowText;
      Canvas.Font.Style := [];
    end;
  end;
end;

procedure TFrmMain.RebuildFCampaignFromSquads;
var
  I: Integer;
  OldLine, NewLine: string;
begin
  // Für jeden Squad die entsprechende Zeile in FCampaign aktualisieren
  for I := 0 to Length(FSquads) - 1 do
  begin
    OldLine := FSave.GetSquadLine(I);
    NewLine := FSave.ReplaceUnitIdsInSquadLine(OldLine, FSquads[I].UnitIds);
    FSave.SetSquadLine(I, NewLine);
  end;

  FSquadsDirty := False;
end;

procedure TFrmMain.RestoreFocusToSquad(ATree: TTreeView; ASquadIndex: Integer);
var
  I: Integer;
  Node: TTreeNode;
  Info: TNodeInfo;
begin
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
end;

procedure TFrmMain.RestoreFocusToUnit(ATree: TTreeView; ASquadIndex: Integer;
  const AUnitId: string);
var
  I: Integer;
  Node, ChildNode: TTreeNode;
  Info, ChildInfo: TNodeInfo;
  DebugLog: TStringList;
begin
  DebugLog := TStringList.Create;
  try
    DebugLog.Add(Format('=== SUCHE Squad %d, Unit %s ===',
      [ASquadIndex, AUnitId]));

    // Erst den Squad finden und expandieren
    for I := 0 to ATree.Items.Count - 1 do
    begin
      Node := ATree.Items[I];
      if Assigned(Node.Data) then
      begin
        Info := TNodeInfo(Node.Data);
        DebugLog.Add(Format('Squad %d gefunden, Text: "%s"',
          [Info.SquadIndex, Node.Text]));

        if (Info.Kind = nkSquad) and (Info.SquadIndex = ASquadIndex) then
        begin
          Node.Expand(False);
          DebugLog.Add(Format('>>> ZIEL-SQUAD GEFUNDEN! Hat %d Kinder',
            [Node.Count]));

          // Jetzt die spezifische Unit in diesem Squad suchen
          ChildNode := Node.getFirstChild;
          while Assigned(ChildNode) do
          begin
            DebugLog.Add(Format('  Kind: "%s"', [ChildNode.Text]));

            if Assigned(ChildNode.Data) then
            begin
              ChildInfo := TNodeInfo(ChildNode.Data);
              DebugLog.Add
                (Format('    Kind-UnitId: "%s", Suche: "%s", Match: %s',
                [ChildInfo.UnitId, AUnitId, BoolToStr(SameText(ChildInfo.UnitId,
                AUnitId), True)]));

              if (ChildInfo.Kind = nkUnit) and
                SameText(ChildInfo.UnitId, AUnitId) then
              begin
                DebugLog.Add('*** GEFUNDEN! Fokussiere Unit ***');

                // Debug-Log ins Memo schreiben
                MemoDebug.Lines.Clear;
                MemoDebug.Lines.AddStrings(DebugLog);

                // ATree.Selected := ChildNode;   Erst mal so
                ATree.Selected := Node;
                Exit;
              end;
            end;

            ChildNode := ChildNode.getNextSibling;
          end;

          DebugLog.Add('>>> Unit nicht gefunden, fokussiere Squad');

          // Debug-Log ins Memo schreiben
          MemoDebug.Lines.Clear;
          MemoDebug.Lines.AddStrings(DebugLog);

          ATree.Selected := Node;
          Exit;
        end;
      end;
    end;

    DebugLog.Add('>>> Squad nicht gefunden!');

    // Debug-Log ins Memo schreiben
    MemoDebug.Lines.Clear;
    MemoDebug.Lines.AddStrings(DebugLog);

  finally
    DebugLog.Free;
  end;
end;

procedure TFrmMain.BtnTransferClick(Sender: TObject);
begin
  Application.MessageBox
    ('Transfer-Funktion ist deaktiviert. Verwenden Sie "Tauschen" um Units mit leeren Plätzen zu vertauschen.',
    'Hinweis', MB_ICONINFORMATION);
end;

procedure TFrmMain.BtnSwapClick(Sender: TObject);
var
  A, B: TNodeInfo;
  BaseSquadIndex, TargetSquadIndex: Integer;
  UnitIndexA, UnitIndexB: Integer;
  I: Integer;
begin
  A := SelectedUnitInfo(TreeBase);
  B := SelectedUnitInfo(TreeTarget);
  if (not Assigned(A)) or (not Assigned(B)) then
  begin
    Application.MessageBox
      ('Bitte links und rechts jeweils eine Unit auswählen.', 'Hinweis',
      MB_ICONINFORMATION);
    Exit;
  end;

  // Squad-Indizes für spätere Fokus-Wiederherstellung speichern
  BaseSquadIndex := A.SquadIndex;
  TargetSquadIndex := B.SquadIndex;

  // Prüfen ob beide Units verschoben werden dürfen (nur bei aktivem Filter)
  if ChkOnlyHumans.Checked and IsValidUnit(A.UnitId) and IsEntityUnit(A.UnitId)
  then
  begin
    Application.MessageBox
      ('Die linke Unit ist eine Entity und kann bei aktivem Filter nicht verschoben werden.',
      'Hinweis', MB_ICONINFORMATION);
    Exit;
  end;

  if ChkOnlyHumans.Checked and IsValidUnit(B.UnitId) and IsEntityUnit(B.UnitId)
  then
  begin
    Application.MessageBox
      ('Die rechte Unit ist eine Entity und kann bei aktivem Filter nicht verschoben werden.',
      'Hinweis', MB_ICONINFORMATION);
    Exit;
  end;

  if (A.SquadIndex = B.SquadIndex) and (SameText(A.UnitId, B.UnitId)) then
  begin
    Application.MessageBox
      ('Die gleiche Unit kann nicht mit sich selbst getauscht werden.',
      'Hinweis', MB_ICONINFORMATION);
    Exit;
  end;

  try
    // Unit-Indizes in FSquads finden
    UnitIndexA := -1;
    UnitIndexB := -1;

    for I := 0 to Length(FSquads[A.SquadIndex].UnitIds) - 1 do
      if SameText(FSquads[A.SquadIndex].UnitIds[I], A.UnitId) then
      begin
        UnitIndexA := I;
        Break;
      end;

    for I := 0 to Length(FSquads[B.SquadIndex].UnitIds) - 1 do
      if SameText(FSquads[B.SquadIndex].UnitIds[I], B.UnitId) then
      begin
        UnitIndexB := I;
        Break;
      end;

    if (UnitIndexA = -1) or (UnitIndexB = -1) then
      raise Exception.Create('Unit-Indizes nicht gefunden');

    // SCHNELLER SWAP: Nur FSquads ändern (kein FCampaign-Parsing!)
    SwapUnitsInSquads(A.SquadIndex, UnitIndexA, B.SquadIndex, UnitIndexB,
      A.UnitId, B.UnitId);

    // TreeViews schnell neu aufbauen (aus FSquads, nicht aus FCampaign!)
    UpdateTreeViewsFromSquads;

    // Fokus auf die getauschten Units wiederherstellen
    // Nach dem Swap: A.UnitId ist jetzt in TargetSquad, B.UnitId ist jetzt in BaseSquad
    RestoreFocusToUnit(TreeBase, BaseSquadIndex, A.UnitId);
    // A ist jetzt in TreeBase
    RestoreFocusToUnit(TreeTarget, TargetSquadIndex, B.UnitId);
    // B ist jetzt in TreeTarget

    // Info-Panel aktualisieren
    if Assigned(SelectedUnitInfo(TreeBase)) then
      UpdateUnitInfoPanel(SelectedUnitInfo(TreeBase).UnitId)
    else if Assigned(SelectedUnitInfo(TreeTarget)) then
      UpdateUnitInfoPanel(SelectedUnitInfo(TreeTarget).UnitId)
    else
      ClearInfoPanel;

    ShowStatus('Units getauscht.');
  except
    on E: Exception do
      Application.MessageBox(PChar('Fehler beim Tauschen: ' + E.Message),
        'Fehler', MB_ICONERROR);
  end;
end;

procedure TFrmMain.BtnSaveAsClick(Sender: TObject);
begin
  SaveDialog1.FilterIndex := 1; // Default auf .sav
  if not SaveDialog1.Execute then
    Exit;

  // ← NEU: Kurzes Feedback ohne Fortschrittsanzeige
  Screen.Cursor := crHourGlass;
  BtnSaveAs.Enabled := False;

  try
    if FSquadsDirty then
      RebuildFCampaignFromSquads;

    FSave.SaveToSaveAs(SaveDialog1.FileName);

    // ← NEU: Erfolgreiche Speicherung mit kurzem visuellen Feedback
    ShowStatus('✓ Erfolgreich gespeichert: ' +
      ExtractFileName(SaveDialog1.FileName));

    // Kurz grünes Feedback im Status-Label anzeigen
    LblStatus.Caption := '✓ Datei erfolgreich gespeichert!';
    LblStatus.Font.Color := clGreen;
    LblStatus.Visible := True;

    // Nach 2 Sekunden automatisch ausblenden
    TThread.CreateAnonymousThread(
      procedure
      begin
        Sleep(2000);
        TThread.Synchronize(nil,
          procedure
          begin
            if LblStatus.Visible and (LblStatus.Font.Color = clGreen) then
            begin
              LblStatus.Visible := False;
              LblStatus.Font.Color := clNavy; // Zurück zur Standard-Farbe
            end;
          end);
      end).Start;

  except
    on E: Exception do
    begin
      ShowStatus('Fehler beim Speichern!');

      // Rotes Fehler-Feedback
      LblStatus.Caption := '✗ Fehler beim Speichern!';
      LblStatus.Font.Color := clRed;
      LblStatus.Visible := True;

      Application.MessageBox(PChar('Fehler beim Speichern: ' + E.Message),
        'Fehler', MB_ICONERROR);

      // Fehler-Label nach 3 Sekunden ausblenden
      TThread.CreateAnonymousThread(
        procedure
        begin
          Sleep(3000);
          TThread.Synchronize(nil,
            procedure
            begin
              if LblStatus.Visible and (LblStatus.Font.Color = clRed) then
              begin
                LblStatus.Visible := False;
                LblStatus.Font.Color := clNavy;
              end;
            end);
        end).Start;
    end;
  end;

  Screen.Cursor := crDefault;
  BtnSaveAs.Enabled := True;
end;

procedure TFrmMain.BtnExportCsvClick(Sender: TObject);
var
  Csv: TStringList;
  I, J: Integer;
  Units: TArray<string>;
  Info: TUnitInfo;
  UnitDetails: TUnitDetails;
  Line: string;
  VeterancyText: string;
begin
  SaveDialog1.FilterIndex := 2; // CSV auswählen
  if not SaveDialog1.Execute then
    Exit;

  // ← NEU: UI während Export blockieren
  SetUILoadingState(True);
  ShowProgress('Exportiere CSV-Datei...');

  Csv := TStringList.Create;
  try
    Csv.Add('Squad;UnitId;Type;Name;Veterancy');
    FAllSquadNames := FSave.GetSquadNames;

    ShowProgress('Exportiere CSV-Datei...', Length(FAllSquadNames));

    for I := 0 to Length(FAllSquadNames) - 1 do
    begin
      UpdateProgress(I, Format('Verarbeite Squad %d/%d...',
        [I + 1, Length(FAllSquadNames)]));

      Units := FSave.GetSquadMembers(I);
      for J := 0 to Length(Units) - 1 do
      begin
        try
          if IsEmptySlot(Units[J]) then
            Line := Format('%s;%s;%s;%s;%s', [FAllSquadNames[I], Units[J],
              'Empty', '[Leer]', ''])
          else
          begin
            Info := FSave.GetUnitInfo(Units[J]);

            // Veteranenstatus für menschliche Einheiten abrufen
            VeterancyText := '';
            if SameText(Info.Kind, 'Human') then
            begin
              try
                UnitDetails := FSave.GetUnitDetails(Units[J]);
                if UnitDetails.Veterancy > 0 then
                  VeterancyText := IntToStr(UnitDetails.Veterancy);
              except
                // Bei Fehlern leeren Veteranenstatus verwenden
              end;
            end;

            Line := Format('%s;%s;%s;%s;%s', [FAllSquadNames[I], Units[J],
              Info.Kind, Info.Name, VeterancyText]);
          end;
        except
          on E: Exception do
            Line := Format('%s;%s;%s;%s;%s', [FAllSquadNames[I], Units[J],
              '?', '', '']);
        end;
        Csv.Add(Line);
      end;
    end;

    UpdateProgress(Length(FAllSquadNames), 'Schreibe CSV-Datei...');
    Csv.SaveToFile(SaveDialog1.FileName, TEncoding.UTF8);
    ShowStatus('CSV exportiert: ' + ExtractFileName(SaveDialog1.FileName));
  finally
    Csv.Free;
    SetUILoadingState(False);
    HideProgress;
  end;
end;

procedure TFrmMain.ShowStatus(const Msg: string);
begin
  Caption := 'GOH Savegame Editor – ' + Msg;
end;

procedure TFrmMain.SwapUnitsInSquads(ASquadA, AUnitA: Integer;
ASquadB, AUnitB: Integer; const AUnitIdA, AUnitIdB: string);
begin
  // Units in FSquads tauschen
  FSquads[ASquadA].UnitIds[AUnitA] := AUnitIdB;
  FSquads[ASquadB].UnitIds[AUnitB] := AUnitIdA;

  // AUCH die gecachten Namen, Veteranenstufen und Kinds tauschen!
  var
  TempName := FSquads[ASquadA].UnitNames[AUnitA];
  var
  TempVet := FSquads[ASquadA].UnitVeterancies[AUnitA];
  var
  TempKind := FSquads[ASquadA].UnitKinds[AUnitA];

  FSquads[ASquadA].UnitNames[AUnitA] := FSquads[ASquadB].UnitNames[AUnitB];
  FSquads[ASquadA].UnitVeterancies[AUnitA] := FSquads[ASquadB]
    .UnitVeterancies[AUnitB];
  FSquads[ASquadA].UnitKinds[AUnitA] := FSquads[ASquadB].UnitKinds[AUnitB];

  FSquads[ASquadB].UnitNames[AUnitB] := TempName;
  FSquads[ASquadB].UnitVeterancies[AUnitB] := TempVet;
  FSquads[ASquadB].UnitKinds[AUnitB] := TempKind;

  // Squad-Veterancy neu berechnen (nur aus Cache!)
  UpdateSquadVeterancyFromCache(ASquadA);
  if ASquadB <> ASquadA then
    UpdateSquadVeterancyFromCache(ASquadB);

  FSquadsDirty := True;
end;

procedure TFrmMain.UpdateSquadVeterancyFromCache(ASquadIndex: Integer);
var
  I, MaxVet: Integer;
begin
  MaxVet := 0;
  for I := 0 to Length(FSquads[ASquadIndex].UnitVeterancies) - 1 do
    if FSquads[ASquadIndex].UnitVeterancies[I] > MaxVet then
      MaxVet := FSquads[ASquadIndex].UnitVeterancies[I];
  FSquads[ASquadIndex].MaxVeterancy := MaxVet;
end;

end.
