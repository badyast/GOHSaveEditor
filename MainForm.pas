{ DONE: -oDaniel -cVital : Problem mit Statusdatei und Name des Spielstands lösen }
{ TODO -cFeature :
  Backup der Save-Dateien mit Zeitstempel
  Fokus on Unit Fixen
  DONE: Möglichkeit Units neu zu sortieren und anzuordnen }
{ DONE: Temporäres Verzeichnis löschen }
{ TODO: fehlende conquest_* Einheiten zu den bekannten Entitys hinzufügen}
unit MainForm;

interface

uses
  ujachLogAuto, ujachLogMgr,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, System.Generics.Defaults, TypInfo, System.Math,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.Themes,
  System.Generics.Collections, System.IOUtils, System.StrUtils, System.DateUtils,
  ConquestSave, Vcl.Menus, Entitys, AppSettings, AppLanguage;

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
    Stage: string;
    UnitIds: TArray<string>;
    UnitNames: TArray<string>; // ← NEU: Gecachte Namen
    UnitVeterancies: TArray<Integer>; // ← NEU: Gecachte Veteranenstufen
    UnitKinds: TArray<string>;
    MaxVeterancy: Integer;
  end;

type
  TSortMenuInfo = record
    Criteria: TSquadSortCriteria;
    Direction: TSquadSortDirection;
    Caption: string;
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
    ProgressBar1: TProgressBar;
    LblStatus: TLabel;
    MainMenu1: TMainMenu;
    Optionen1: TMenuItem;
    Debuglevel: TMenuItem;
    Off1: TMenuItem;
    Emergency1: TMenuItem;
    Critical1: TMenuItem;
    Error1: TMenuItem;
    Warning1: TMenuItem;
    Notice1: TMenuItem;
    Info1: TMenuItem;
    Debug1: TMenuItem;
    Alert1: TMenuItem;
    BackupOptionen1: TMenuItem;
    Sprache1: TMenuItem;
    Deutsch1: TMenuItem;
    English1: TMenuItem;
    Design1: TMenuItem;
    Hell1: TMenuItem;
    Dunkel1: TMenuItem;
    PageControl1: TPageControl;
    TabGeneral: TTabSheet;
    MemoInfo: TMemo;
    TabInventory: TTabSheet;
    ListViewInventory: TListView;
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
    procedure DebugLevelClick(Sender: TObject);
    procedure BackupOptionen1Click(Sender: TObject);
    procedure LanguageClick(Sender: TObject);
    procedure ThemeClick(Sender: TObject);

  private
    FSquads: TArray<TSquadData>; // Live-Datenstruktur
    FSquadsDirty: Boolean; // Änderungen pending?
    FSave: TConquestSave;
    FAllSquadNames: TArray<string>;
    FSortMenuInfos: TArray<TSortMenuInfo>; // Array für Sortier-Informationen
    FSortMenu: TMenuItem; // Referenz auf Sortier-Menü
    procedure SetControlsEnabled(AEnabled: Boolean);
    procedure BlinkForm(AColor: TColor; ACount: Integer);
    procedure CreateBackup(const FilePath: string);
    procedure CleanupOldBackups(const SaveName: string);
    procedure ClearTreeData(ATree: TTreeView);
    procedure PopulateTrees;
    function SelectedUnitInfo(ATree: TTreeView): TNodeInfo;
    function SelectedSquadIndex(ATree: TTreeView): Integer;
    procedure UpdateInfoLabels;
    procedure ShowStatus(const Msg: string);
    function IsEmptySlot(const UnitId: string): Boolean;
    function IsValidUnit(const UnitId: string): Boolean;
    function IsEntityUnit(const UnitId: string): Boolean; overload;
    function IsEntityUnit(const UnitId: string; ASquadIndex: Integer; AUnitIndex: Integer): Boolean; overload;
    function FindSquadWithSameNameAndNonEmptyFirstSlot(const SquadName: string; ExcludeIndex: Integer): Integer;
    procedure UpdateUnitInfoPanel(const UnitId: string);
    procedure ClearInfoPanel;
    function GetVeterancyDisplay(AVeterancy: Integer): string;
    function GetUnitDisplayName(const UnitId: string): string;
    function GetSquadMaxVeterancy(ASquadIndex: Integer): Integer;
    procedure CopyTreeStructure(ASourceTree, ATargetTree: TTreeView);
    procedure LoadSquadsFromSave;
    procedure UpdateTreeViewsFromSquads;
    procedure SwapUnitsInSquads(ASquadA, AUnitA: Integer;
      ASquadB, AUnitB: Integer; const AUnitIdA, AUnitIdB: string);
    procedure RebuildFCampaignFromSquads;
    procedure UpdateSquadVeterancyFromCache(ASquadIndex: Integer);
    procedure RestoreFocusToUnit(ATree: TTreeView; ASquadIndex: Integer;
      const AUnitId: string);

    // Fortschritts- und Status-Methoden
    procedure ShowProgress(const AMessage: string; AMaxValue: Integer = 0);
    procedure UpdateProgress(ACurrentValue: Integer;
      const AMessage: string = '');
    procedure HideProgress;
    procedure SetUILoadingState(ALoading: Boolean);
    procedure UncheckMenu(AMenuItem: TMenuItem);
    procedure SortMenuItemClick(Sender: TObject);
    procedure AddSortMenuItems;
    procedure RebuildSortMenu;
    function GetSortCriteriaDisplayName(Criteria: TSquadSortCriteria): string;
    function GetStageGroupDisplay(const Stage: string): string;
    procedure ApplyLanguage;
    procedure ApplyThemeColors;

  public

    procedure SortSquads(Criteria: TSquadSortCriteria;
      Direction: TSquadSortDirection);
  end;

var
  FrmMain: TFrmMain;

implementation

uses
  BackupOptionsForm;

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
    SaveDialog1.Filter :=
      'Gates of Hell Save (*.sav)|*.sav|CSV-Datei (*.csv)|*.csv';
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

    // Sprache initialisieren
    ApplyLanguage;
    if Settings.Language = 'de' then
      Deutsch1.Checked := True
    else
      English1.Checked := True;

    // Theme initialisieren
    if Settings.Theme = 'dark' then
      Dunkel1.Checked := True
    else
      Hell1.Checked := True;

    // Theme-Farben anwenden
    ApplyThemeColors;

  except
    on E: Exception do
    begin
      jachLog.LogError('FEHLER in FormCreate', E);
      jachLog.LogCritical
        ('MainForm-Initialisierung fehlgeschlagen - Anwendung instabil');
      raise;
    end;
  end;
  // Squad-Sortierung Menü hinzufügen
  AddSortMenuItems;

  // Sortier-Menü initial deaktivieren (kein Save geladen)
  if Assigned(FSortMenu) then
    FSortMenu.Enabled := False;
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
    jachLog.LogDebug('Fortschrittsanzeige gestartet: "%s" (Max: %d)',
      [AMessage, AMaxValue]);
  end
  else
  begin
    ProgressBar1.Style := pbstMarquee;
    jachLog.LogDebug('Fortschrittsanzeige gestartet (unbestimmt): "%s"',
      [AMessage]);
  end;

  ProgressBar1.Visible := True;
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
  jachLog.LogDebug('Fortschrittsanzeige beendet');
end;

procedure TFrmMain.SetUILoadingState(ALoading: Boolean);
begin
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
    Caption := 'GOH Savegame Editor - Verarbeitung läuft...';
    jachLog.LogDebug('UI in Lade-Zustand versetzt');
  end
  else
  begin
    Screen.Cursor := crDefault;
    Caption := 'GOH Savegame Editor';
    jachLog.LogDebug('UI in Normal-Zustand zurückgesetzt');
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
      jachLog.LogWarning('Fehler beim MainForm-Cleanup (nicht kritisch): %s',
        [E.Message]);
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

  // Sortier-Menü aktivieren/deaktivieren
  if Assigned(FSortMenu) then
    FSortMenu.Enabled := AEnabled;
end;

procedure TFrmMain.BlinkForm(AColor: TColor; ACount: Integer);
begin
  TThread.CreateAnonymousThread(
    procedure
    var
      J: Integer;
      LocalPanel: TPanel;
    begin
      for J := 1 to ACount do
      begin
        // Overlay erstellen und anzeigen
        TThread.Synchronize(nil,
          procedure
          begin
            LocalPanel := TPanel.Create(Self);
            LocalPanel.Parent := Self;
            LocalPanel.Align := alClient;
            LocalPanel.BevelOuter := bvNone;
            LocalPanel.Color := AColor;
            LocalPanel.ParentBackground := False;
            LocalPanel.StyleElements := []; // VCL-Styles deaktivieren für echte Farbe
            LocalPanel.BringToFront;
          end);
        Sleep(150);

        // Overlay entfernen
        TThread.Synchronize(nil,
          procedure
          begin
            if Assigned(LocalPanel) then
            begin
              LocalPanel.Free;
              LocalPanel := nil;
            end;
          end);
        Sleep(150);
      end;
    end).Start;
end;

procedure TFrmMain.CreateBackup(const FilePath: string);
var
  SaveName, TimeStamp, BackupDir, BackupFile: string;
begin
  if not TFile.Exists(FilePath) then
    Exit; // Keine Datei zum Backup vorhanden

  SaveName := TPath.GetFileNameWithoutExtension(FilePath);
  TimeStamp := FormatDateTime('yyyymmdd_hhnnss', Now);
  BackupDir := TPath.Combine(Settings.BackupFolder, SaveName + '_' + TimeStamp);
  BackupFile := TPath.Combine(BackupDir, TPath.GetFileName(FilePath));

  jachLog.LogInfo('=== BACKUP-OPERATION gestartet ===');
  jachLog.LogInfo('Erstelle Backup von: %s', [FilePath]);
  jachLog.LogInfo('Backup-Verzeichnis: %s', [BackupDir]);

  try
    // Backup-Verzeichnis erstellen
    TDirectory.CreateDirectory(BackupDir);
    jachLog.LogDebug('Backup-Verzeichnis erstellt');

    // Datei kopieren
    TFile.Copy(FilePath, BackupFile, True);
    jachLog.LogInfo('Backup erfolgreich erstellt: %s', [BackupFile]);

    // Alte Backups aufräumen falls konfiguriert
    if Settings.MaxBackupCount > 0 then
      CleanupOldBackups(SaveName);

  except
    on E: Exception do
    begin
      jachLog.LogError('Fehler beim Erstellen des Backups', E);
      // Fehler nicht nach außen werfen, Speichern soll trotzdem funktionieren
    end;
  end;
end;

procedure TFrmMain.CleanupOldBackups(const SaveName: string);
var
  BackupDirs: TArray<string>;
  DirList: TList<TPair<string, TDateTime>>;
  Dir: string;
  DirTime: TDateTime;
  I: Integer;
begin
  if Settings.MaxBackupCount <= 0 then
    Exit; // Unbegrenzte Backups

  jachLog.LogDebug('Prüfe alte Backups für: %s (Max: %d)', [SaveName, Settings.MaxBackupCount]);

  BackupDirs := TDirectory.GetDirectories(Settings.BackupFolder, SaveName + '_*');

  if Length(BackupDirs) <= Settings.MaxBackupCount then
  begin
    jachLog.LogDebug('Anzahl Backups (%d) unter Maximum (%d), kein Cleanup nötig',
      [Length(BackupDirs), Settings.MaxBackupCount]);
    Exit;
  end;

  // Verzeichnisse mit Zeitstempel in Liste sammeln
  DirList := TList<TPair<string, TDateTime>>.Create;
  try
    for Dir in BackupDirs do
    begin
      DirTime := TDirectory.GetCreationTime(Dir);
      DirList.Add(TPair<string, TDateTime>.Create(Dir, DirTime));
    end;

    // Nach Datum sortieren (älteste zuerst)
    DirList.Sort(TComparer<TPair<string, TDateTime>>.Construct(
      function(const A, B: TPair<string, TDateTime>): Integer
      begin
        Result := CompareDateTime(A.Value, B.Value);
      end));

    // Älteste Backups löschen bis Max erreicht
    I := 0;
    while DirList.Count > Settings.MaxBackupCount do
    begin
      jachLog.LogInfo('Lösche altes Backup: %s', [DirList[0].Key]);
      try
        TDirectory.Delete(DirList[0].Key, True);
        DirList.Delete(0);
        Inc(I);
      except
        on E: Exception do
        begin
          jachLog.LogWarning('Konnte Backup nicht löschen: %s - %s', [DirList[0].Key, E.Message]);
          DirList.Delete(0); // Trotzdem aus Liste entfernen
        end;
      end;
    end;

    if I > 0 then
      jachLog.LogInfo('%d alte Backups gelöscht', [I]);

  finally
    DirList.Free;
  end;
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
  SquadStages: TArray<string>;
  SquadCount: Integer;
  UnitInfo: TUnitInfo;
  UnitDetails: TUnitDetails;
  MaxVet: Integer;
  TotalUnits, ProcessedUnits: Integer;
  ErrorCount, WarningCount: Integer;
  StartTime: TDateTime;
  ElapsedMs: Integer;
  EmptySlotCount, EntityCount, HumanCount: Integer;
begin
  StartTime := Now;
  jachLog.LogInfo('=== Squad-Daten-Verarbeitung gestartet ===');

  ErrorCount := 0;
  WarningCount := 0;
  EmptySlotCount := 0;
  EntityCount := 0;
  HumanCount := 0;

  try
    jachLog.LogDebug('Lade Squad-Namen aus Savegame');
SquadCount := FSave.GetSquadNamesAndStages(SquadNames, SquadStages);
SetLength(FSquads, SquadCount);

    jachLog.LogInfo('Gefunden: %d Squads zur Verarbeitung',
      [Length(SquadNames)]);

    // Gesamtanzahl Units berechnen
    TotalUnits := 0;
    for I := 0 to Length(SquadNames) - 1 do
      TotalUnits := TotalUnits + Length(FSave.GetSquadMembers(I));

    jachLog.LogInfo('Gesamtanzahl Units in allen Squads: %d', [TotalUnits]);

    ProcessedUnits := 0;
    ShowProgress(GetLanguageStrings(Settings.Language).StatusLoadingUnitData, TotalUnits);

    // Squad-für-Squad-Verarbeitung
    for I := 0 to Length(SquadNames) - 1 do
    begin
      UpdateProgress(ProcessedUnits, Format('Verarbeite Squad %d/%d: %s',
        [I + 1, Length(SquadNames), SquadNames[I]]));

      // Progress-Logging (alle 10 Squads oder am Ende)
      if (I mod 10 = 0) or (I = Length(SquadNames) - 1) then
        jachLog.LogDebug('Squad-Progress: %d/%d - "%s"',
          [I + 1, Length(SquadNames), SquadNames[I]]);

      jachLog.LogDebug('Verarbeite Squad "%s" (Index %d)', [SquadNames[I], I]);

      FSquads[I].Name := SquadNames[I];
      FSquads[I].Stage := SquadStages[I];
      FSquads[I].UnitIds := FSave.GetSquadMembers(I);

      jachLog.LogDebug('Squad "%s" hat %d Units',
        [SquadNames[I], Length(FSquads[I].UnitIds)]);

      // Arrays für gecachte Daten vorbereiten
      SetLength(FSquads[I].UnitNames, Length(FSquads[I].UnitIds));
      SetLength(FSquads[I].UnitVeterancies, Length(FSquads[I].UnitIds));
      SetLength(FSquads[I].UnitKinds, Length(FSquads[I].UnitIds));

      MaxVet := 0;

      // Unit-für-Unit-Verarbeitung
      for J := 0 to Length(FSquads[I].UnitIds) - 1 do
      begin
        Inc(ProcessedUnits);

        if IsEmptySlot(FSquads[I].UnitIds[J]) then
        begin
          // Leere Slots
          FSquads[I].UnitNames[J] := GetLanguageStrings(Settings.Language).UnitEmpty;
          FSquads[I].UnitVeterancies[J] := 0;
          FSquads[I].UnitKinds[J] := 'Empty';
          Inc(EmptySlotCount);

          jachLog.LogDebug('Squad "%s" Slot %d: Leer (%s)',
            [SquadNames[I], J, FSquads[I].UnitIds[J]]);
        end
        else
        begin
          try
            // Unit-Basis-Informationen laden
            UnitInfo := FSave.GetUnitInfo(FSquads[I].UnitIds[J]);
            FSquads[I].UnitNames[J] := UnitInfo.Name;
            FSquads[I].UnitKinds[J] := UnitInfo.Kind;

            jachLog.LogDebug('Squad "%s" Slot %d: %s "%s" (%s)',
              [SquadNames[I], J, UnitInfo.Kind, UnitInfo.Name,
              FSquads[I].UnitIds[J]]);

            // Statistik-Zählung
            if SameText(UnitInfo.Kind, 'Human') then
            begin
              Inc(HumanCount);

              // Veterancy nur für Menschen laden
              try
                UnitDetails := FSave.GetUnitDetails(FSquads[I].UnitIds[J]);
                FSquads[I].UnitVeterancies[J] := UnitDetails.Veterancy;

                if UnitDetails.Veterancy > MaxVet then
                  MaxVet := UnitDetails.Veterancy;

                if UnitDetails.Veterancy > 0 then
                  jachLog.LogDebug('Unit %s: Veterancy %d',
                    [FSquads[I].UnitIds[J], UnitDetails.Veterancy]);

              except
                on E: Exception do
                begin
                  jachLog.LogWarning
                    ('Veterancy-Fehler für Unit %s in Squad "%s": %s',
                    [FSquads[I].UnitIds[J], SquadNames[I], E.Message]);
                  FSquads[I].UnitVeterancies[J] := 0;
                  Inc(WarningCount);
                end;
              end;
            end
            else
            begin
              Inc(EntityCount);
              FSquads[I].UnitVeterancies[J] := 0;
            end;

          except
            on E: Exception do
            begin
              jachLog.LogError
                ('Schwerwiegender Unit-Fehler für %s in Squad "%s": %s',
                [FSquads[I].UnitIds[J], SquadNames[I], E.Message]);
              FSquads[I].UnitNames[J] := GetLanguageStrings(Settings.Language).UnitError;
              FSquads[I].UnitVeterancies[J] := 0;
              FSquads[I].UnitKinds[J] := 'Unknown';
              Inc(ErrorCount);
            end;
          end;
        end;

        // Progress-Update (alle 50 Units)
        if (ProcessedUnits mod 50 = 0) or (J = Length(FSquads[I].UnitIds) - 1)
        then
          UpdateProgress(ProcessedUnits);
      end;

      FSquads[I].MaxVeterancy := MaxVet;

      if MaxVet > 0 then
        jachLog.LogDebug('Squad "%s" maximale Veterancy: %d',
          [SquadNames[I], MaxVet]);
    end;

    FSquadsDirty := False;

    ElapsedMs := Round((Now - StartTime) * 24 * 60 * 60 * 1000);

    // Ausführliche Abschluss-Statistiken
    jachLog.LogInfo('=== Squad-Verarbeitung abgeschlossen ===');
    jachLog.LogInfo('Verarbeitungszeit: %d ms', [ElapsedMs]);
    jachLog.LogInfo('Verarbeitete Squads: %d', [Length(SquadNames)]);
    jachLog.LogInfo('Verarbeitete Units gesamt: %d', [ProcessedUnits]);
    jachLog.LogInfo('- Menschen: %d', [HumanCount]);
    jachLog.LogInfo('- Entities: %d', [EntityCount]);
    jachLog.LogInfo('- Leere Slots: %d', [EmptySlotCount]);
    jachLog.LogInfo('Fehler: %d', [ErrorCount]);
    jachLog.LogInfo('Warnungen: %d', [WarningCount]);

    if ElapsedMs > 5000 then
      jachLog.LogWarning('Langsame Squad-Verarbeitung: %d ms (> 5s)',
        [ElapsedMs]);

    if ErrorCount > 0 then
      jachLog.LogWarning('Squad-Verarbeitung mit %d Fehlern abgeschlossen',
        [ErrorCount])
    else if WarningCount > 0 then
      jachLog.LogInfo('Squad-Verarbeitung mit %d Warnungen abgeschlossen',
        [WarningCount])
    else
      jachLog.LogInfo
        ('Squad-Verarbeitung vollständig erfolgreich abgeschlossen');

  except
    on E: Exception do
    begin
      jachLog.LogCritical('KRITISCHER FEHLER in LoadSquadsFromSave', E);
      jachLog.LogCritical('Squad-Verarbeitung abgebrochen nach %d ms',
        [Round((Now - StartTime) * 24 * 60 * 60 * 1000)]);
      raise;
    end;
  end;

  HideProgress;
  jachLog.LogDebug('Squad-Verarbeitung UI-Cleanup abgeschlossen');
end;

function TFrmMain.IsEntityUnit(const UnitId: string): Boolean;
begin
  // Rufe die überladene Version mit Standardwerten auf
  Result := IsEntityUnit(UnitId, -1, -1);
end;

function TFrmMain.IsEntityUnit(const UnitId: string; ASquadIndex: Integer; AUnitIndex: Integer): Boolean;
var
  UnitInfo: TUnitInfo;
  UnitTypeName: string;
  I: Integer;
  OtherUnitId: string;
begin
  Result := False;

  // Spezialfall: Erste Unit eines Squads ist leer
  if IsEmptySlot(UnitId) and (ASquadIndex >= 0) and (AUnitIndex = 0) then
  begin
    jachLog.LogDebug('IsEntityUnit: Erste Unit ist leer - Squad %d "%s"',
      [ASquadIndex, FSquads[ASquadIndex].Name]);

    // Schritt 1: Squad-Name in KNOWN_ENTITY_NAMES suchen
    Result := IsKnownEntityName(FSquads[ASquadIndex].Name);
    if Result then
    begin
      jachLog.LogDebug('IsEntityUnit: Squad-Name "%s" in KNOWN_ENTITY_NAMES gefunden -> Entity',
        [FSquads[ASquadIndex].Name]);
      Exit;
    end;
    jachLog.LogDebug('IsEntityUnit: Squad-Name "%s" nicht in KNOWN_ENTITY_NAMES',
      [FSquads[ASquadIndex].Name]);

    // Schritt 2: Suche Squad mit gleichem Namen und nicht-leerem ersten Slot
    I := FindSquadWithSameNameAndNonEmptyFirstSlot(FSquads[ASquadIndex].Name, ASquadIndex);
    if I >= 0 then
    begin
      // Verwende den ersten Slot dieses anderen Squads
      Result := SameText(FSquads[I].UnitKinds[0], 'Entity');
      jachLog.LogDebug('IsEntityUnit: Referenz-Squad %d gefunden -> %s',
        [I, IfThen(Result, 'Entity', 'Human')]);
      Exit;
    end;

    // Schritt 3: Default - Human annehmen
    jachLog.LogDebug('IsEntityUnit: Keine Referenz gefunden -> Human angenommen');
    Result := False;
    Exit;
  end;

  // Normale Logik für nicht-leere Units
  if IsEmptySlot(UnitId) then
    Exit(False);

  try
    UnitInfo := FSave.GetUnitInfo(UnitId);
    Result := SameText(UnitInfo.Kind, 'Entity');
  except
    // Bei Fehlern versuche Fallback auf Entity-Liste
    try
      UnitTypeName := FSave.ExtractUnitTypeName(UnitId);
      if UnitTypeName <> '' then
        Result := IsKnownEntityName(UnitTypeName);
    except
      // Bei beiden Fehlern nehmen wir an, dass es kein Entity ist
      Result := False;
    end;
  end;
end;

function TFrmMain.FindSquadWithSameNameAndNonEmptyFirstSlot(const SquadName: string; ExcludeIndex: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;

  // Durchsuche alle Squads nach gleichem Namen
  for I := 0 to Length(FSquads) - 1 do
  begin
    // Skip den aktuellen Squad
    if I = ExcludeIndex then
      Continue;

    // Prüfe ob Name gleich ist und erster Slot nicht leer
    if SameText(FSquads[I].Name, SquadName) and
       (Length(FSquads[I].UnitIds) > 0) and
       not IsEmptySlot(FSquads[I].UnitIds[0]) then
    begin
      Result := I;
      Exit;
    end;
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

  jachLog.LogInfo('LADE-OPERATION gestartet für: %s',
    [ExtractFileName(FileName)]);

  // Datei-Vorab-Informationen loggen
  try
    FileSize := TFile.GetSize(FileName);
    jachLog.LogInfo('Dateigröße: %.2f MB (%d Bytes)',
      [FileSize / (1024 * 1024), FileSize]);

    if FileSize > 100 * 1024 * 1024 then // > 100MB
      jachLog.LogWarning
        ('Große Savegame-Datei (%.2f MB) - Laden könnte länger dauern',
        [FileSize / (1024 * 1024)]);

  except
    on E: Exception do
      jachLog.LogWarning('Konnte Dateigröße nicht ermitteln: %s', [E.Message]);
  end;

  jachLog.LogDebug('Setze UI in Lade-Zustand');
  SetUILoadingState(True);

  try
    ShowProgress(GetLanguageStrings(Settings.Language).StatusLoadingSave);
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
    ShowStatus(Format(GetLanguageStrings(Settings.Language).StatusSaveLoaded, [ExtractFileName(FileName)]));
    ClearInfoPanel;
    jachLog.LogDebug('UI in Normal-Zustand zurückgesetzt');

    ElapsedMs := Round((Now - StartTime) * 24 * 60 * 60 * 1000);
    jachLog.LogInfo('LADE-OPERATION erfolgreich abgeschlossen in %d ms',
      [ElapsedMs]);

    if ElapsedMs > 10000 then // > 10 Sekunden
      jachLog.LogWarning('Langsame Lade-Performance: %d ms (> 10s)',
        [ElapsedMs]);

  except
    on E: Exception do
    begin
      jachLog.LogError('LADE-OPERATION fehlgeschlagen für: %s', [FileName], E);
      jachLog.LogError('Exception-Details: %s: %s', [E.ClassName, E.Message]);

      SetControlsEnabled(False);
      ShowStatus(GetLanguageStrings(Settings.Language).StatusErrorLoading);
      jachLog.LogDebug('UI auf Fehler-Zustand gesetzt');

      Application.MessageBox(
        PChar(Format(GetLanguageStrings(Settings.Language).ErrorLoading, [E.Message])),
        PChar(GetLanguageStrings(Settings.Language).ErrorLoadingTitle),
        MB_ICONERROR);
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

procedure TFrmMain.UpdateTreeViewsFromSquads;
var
  I, J: Integer;
  SquadNode, UnitNode: TTreeNode;
  Info: TNodeInfo;
  UnitCaption, SquadCaption: string;
  IsEntity, IsEmpty: Boolean;
  SquadVeterancyText, UnitVeterancyText: string;
  StageDisplay: string;
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

StageDisplay := GetStageGroupDisplay(FSquads[I].Stage);
SquadCaption := SquadCaption + StageDisplay;

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
        // Speziallogik für erste leere Unit eines Squads
        if J = 0 then
          IsEntity := IsEntityUnit(FSquads[I].UnitIds[J], I, J)
        else
          IsEntity := False;

        // Filter auch für leere Slots anwenden
        if ChkOnlyHumans.Checked and IsEntity then
          Continue;
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
var
  Lang: TLanguageStrings;
begin
  Lang := GetLanguageStrings(Settings.Language);
  jachLog.LogDebug('Info-Panel geleert');
  MemoInfo.Clear;
  MemoInfo.Lines.Add(Lang.InfoNoUnitSelected);
  ListViewInventory.Items.Clear;
end;

procedure TFrmMain.UpdateUnitInfoPanel(const UnitId: string);
var
  Details: TUnitDetails;
  Item: TListItem;
  I: Integer;
  StartTime: TDateTime;
  ElapsedMs: Integer;
  Lang: TLanguageStrings;
begin
  Lang := GetLanguageStrings(Settings.Language);
  StartTime := Now;
  jachLog.LogDebug('Aktualisiere Unit-Info-Panel für: %s', [UnitId]);

  if IsEmptySlot(UnitId) then
  begin
    MemoInfo.Clear;
    MemoInfo.Lines.Add(Lang.InfoEmptySlot);
    ListViewInventory.Items.Clear;
    jachLog.LogDebug('Unit-Info-Panel: Leerer Slot angezeigt');
    Exit;
  end;

  try
    Details := FSave.GetUnitDetails(UnitId);
    jachLog.LogDebug('Unit-Details geladen für %s: %s "%s"',
      [UnitId, Details.Kind, Details.Name]);

    // Allgemeine Informationen
    MemoInfo.Clear;
    MemoInfo.Lines.Add(Lang.InfoUnitInformation);
    MemoInfo.Lines.Add('');
    MemoInfo.Lines.Add(Format('%-16s%s', [Lang.InfoUnitId, Details.UnitId]));
    MemoInfo.Lines.Add(Format('%-16s%s', [Lang.InfoType, Details.Kind]));
    MemoInfo.Lines.Add(Format('%-16s%s', [Lang.InfoUnitClass, Details.UnitType]));
    MemoInfo.Lines.Add(Format('%-16s%s', [Lang.InfoName, Details.Name]));
    MemoInfo.Lines.Add('');

    if Details.Position <> '' then
      MemoInfo.Lines.Add(Format('%-16s%s', [Lang.InfoPosition, Details.Position]));

    if Details.Veterancy > 0 then
      MemoInfo.Lines.Add(Format('%-16s%d%s', [Lang.InfoVeterancy, Details.Veterancy,
        GetVeterancyDisplay(Details.Veterancy)]));

    if Details.Score > 0 then
      MemoInfo.Lines.Add(Format('%-16s%.2f', [Lang.InfoScore, Details.Score]));

    if Details.InfantryKills > 0 then
      MemoInfo.Lines.Add(Format('%-16s%d', [Lang.InfoInfantryKills, Details.InfantryKills]));

    MemoInfo.Lines.Add(Format('MID:            %d', [Details.MID]));

    if (Details.NameId1 > 0) or (Details.NameId2 > 0) then
      MemoInfo.Lines.Add(Format('Name-IDs:       %d, %d',
        [Details.NameId1, Details.NameId2]));

    if Details.LastItem <> '' then
      MemoInfo.Lines.Add(Format('%-16s%s', [Lang.InfoLastWeapon, Details.LastItem]));

    if Details.LastThrowItem <> '' then
      MemoInfo.Lines.Add(Format('%-16s%s', [Lang.InfoLastGrenade, Details.LastThrowItem]));

    if Details.FsmState <> '' then
      MemoInfo.Lines.Add(Format('%-16s%s', [Lang.InfoFsmState, Details.FsmState]));

    // Inventar
    ListViewInventory.Items.Clear;
    jachLog.LogDebug('Lade Inventar für Unit %s: %d Items',
      [UnitId, Length(Details.Inventory)]);

    for I := 0 to High(Details.Inventory) do
    begin
      Item := ListViewInventory.Items.Add;
      Item.Caption := Details.Inventory[I].ItemName;

      if Details.Inventory[I].ItemType <> '' then
        Item.SubItems.Add(Details.Inventory[I].ItemType)
      else
        Item.SubItems.Add('-');

      if Details.Inventory[I].Quantity > 1 then
        Item.SubItems.Add(IntToStr(Details.Inventory[I].Quantity))
      else
        Item.SubItems.Add('1');

      Item.SubItems.Add('Cell ' + Details.Inventory[I].Cell);

      if Details.Inventory[I].IsUserItem then
        Item.SubItems.Add(Lang.InfoEquipped)
      else
        Item.SubItems.Add('');
    end;

    // Wenn kein Inventar vorhanden
    if Length(Details.Inventory) = 0 then
    begin
      Item := ListViewInventory.Items.Add;
      Item.Caption := '(Kein Inventar gefunden)';
      jachLog.LogDebug('Unit %s: Kein Inventar vorhanden', [UnitId]);
    end;

    ElapsedMs := Round((Now - StartTime) * 24 * 60 * 60 * 1000);
    jachLog.LogDebug('Unit-Info-Panel aktualisiert in %d ms', [ElapsedMs]);

  except
    on E: Exception do
    begin
      jachLog.LogError('Fehler beim Laden der Unit-Details für %s',
        [UnitId], E);
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
  jachLog.LogDebug('TreeBase Selektion geändert');

  UpdateInfoLabels;

  // Update Info-Panel wenn eine Unit ausgewählt ist
  U := SelectedUnitInfo(TreeBase);
  if Assigned(U) then
  begin
    jachLog.LogDebug('TreeBase: Unit %s ausgewählt', [U.UnitId]);
    UpdateUnitInfoPanel(U.UnitId);
  end
  else
  begin
    jachLog.LogDebug('TreeBase: Keine Unit ausgewählt');
    ClearInfoPanel;
  end;
end;

procedure TFrmMain.TreeTargetChange(Sender: TObject; Node: TTreeNode);
var
  U: TNodeInfo;
begin
  jachLog.LogDebug('TreeTarget Selektion geändert');

  UpdateInfoLabels;

  // Update Info-Panel wenn eine Unit ausgewählt ist (Target hat Priorität)
  U := SelectedUnitInfo(TreeTarget);
  if Assigned(U) then
  begin
    jachLog.LogDebug('TreeTarget: Unit %s ausgewählt', [U.UnitId]);
    UpdateUnitInfoPanel(U.UnitId);
  end;
end;

procedure TFrmMain.ChkOnlyHumansClick(Sender: TObject);
begin
  jachLog.LogInfo('Human-Filter geändert auf: %s',
    [IfThen(ChkOnlyHumans.Checked, 'Nur Menschen', 'Alle Units')]);

  // Bei Änderung der Checkbox die TreeViews neu laden
  if Assigned(FSave) then
  begin
    jachLog.LogDebug('Lade TreeViews nach Filter-Änderung neu');
    PopulateTrees;
    ClearInfoPanel;
    jachLog.LogDebug('TreeViews nach Filter-Änderung aktualisiert');
  end;
end;

procedure TFrmMain.TreeCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
  State: TCustomDrawState; var DefaultDraw: Boolean);
var
  Info: TNodeInfo;
  Canvas: TCanvas;
  IsEntity: Boolean;
  Stage: string;
  StageColor: TColor;
  R: TRect;
begin
  Canvas := Sender.Canvas;
  DefaultDraw := True;

  if Assigned(Node.Data) then
  begin
    Info := TNodeInfo(Node.Data);

    if Info.Kind = nkSquad then
    begin
      // Squad-Knoten: Hintergrundfarbe basierend auf Stage
      Stage := FSquads[Info.SquadIndex].Stage;

      // Stage-basierte Hintergrundfarben (angepasst an Theme)
      if Settings.Theme = 'dark' then
      begin
        // Dark Theme: Dunkle Farben
        if Stage = '' then
          StageColor := RGB(45, 45, 48)
        else if Pos('stage_1', Stage) > 0 then
          StageColor := RGB(30, 50, 70) // Dunkelblau
        else if Pos('stage_2', Stage) > 0 then
          StageColor := RGB(40, 60, 40) // Dunkelgrün
        else if Pos('stage_3', Stage) > 0 then
          StageColor := RGB(70, 50, 30) // Dunkelorange
        else if Pos('stage_4', Stage) > 0 then
          StageColor := RGB(60, 30, 50) // Dunkelrosa
        else if Pos('stage_5', Stage) > 0 then
          StageColor := RGB(50, 40, 70) // Dunkellila
        else if Pos('stage_special', Stage) > 0 then
          StageColor := RGB(70, 50, 50) // Dunkelrot/Braun
        else
          StageColor := RGB(50, 50, 50); // Dunkelgrau für andere Stages
      end
      else
      begin
        // Light Theme: Helle Farben
        if Stage = '' then
          StageColor := clWindow
        else if Pos('stage_1', Stage) > 0 then
          StageColor := RGB(230, 240, 255) // Hellblau
        else if Pos('stage_2', Stage) > 0 then
          StageColor := RGB(240, 255, 230) // Hellgrün
        else if Pos('stage_3', Stage) > 0 then
          StageColor := RGB(255, 245, 230) // Hellorange
        else if Pos('stage_4', Stage) > 0 then
          StageColor := RGB(255, 230, 240) // Hellrosa
        else if Pos('stage_5', Stage) > 0 then
          StageColor := RGB(240, 230, 255) // Helllila
        else if Pos('stage_special', Stage) > 0 then
          StageColor := RGB(255, 240, 240) // Hellrot
        else
          StageColor := RGB(245, 245, 245); // Hellgrau für andere Stages
      end;

      // Nur Hintergrundfarbe setzen
      Canvas.Brush.Color := StageColor;

      // Hintergrund für komplette Zeile zeichnen
      R := Node.DisplayRect(False);
      R.Left := 0;
      R.Right := Sender.ClientWidth;
      Canvas.FillRect(R);

      // Entity-Squads in Blau, Human-Squads in Schwarz/Weiß (je nach Theme)
      if Length(FSquads[Info.SquadIndex].UnitIds) > 0 then
      begin
        if IsEmptySlot(FSquads[Info.SquadIndex].UnitIds[0]) then
          IsEntity := IsEntityUnit(FSquads[Info.SquadIndex].UnitIds[0], Info.SquadIndex, 0)
        else
          IsEntity := SameText(FSquads[Info.SquadIndex].UnitKinds[0], 'Entity');

        if IsEntity then
        begin
          if Settings.Theme = 'dark' then
            Canvas.Font.Color := RGB(100, 180, 255) // Hellblau für Entity-Squads (Dark Mode)
          else
            Canvas.Font.Color := RGB(0, 80, 160); // Dunkelblau für Entity-Squads (Light Mode)
        end
        else
        begin
          if Settings.Theme = 'dark' then
            Canvas.Font.Color := RGB(220, 220, 220) // Hellgrau für Human-Squads (Dark Mode)
          else
            Canvas.Font.Color := clWindowText; // Schwarz für Human-Squads (Light Mode)
        end;
      end
      else
      begin
        if Settings.Theme = 'dark' then
          Canvas.Font.Color := RGB(220, 220, 220)
        else
          Canvas.Font.Color := clWindowText;
      end;

      Canvas.Font.Style := [fsBold];
    end
    else if Info.Kind = nkUnit then
    begin
      // Unit-Knoten
      if IsEmptySlot(Info.UnitId) then
      begin
        // Leere Slots ausgegraut und kursiv
        Canvas.Font.Color := clGray;
        Canvas.Font.Style := [fsItalic];
      end
      else
      begin
        // Normale Units
        if Settings.Theme = 'dark' then
          Canvas.Font.Color := RGB(220, 220, 220)
        else
          Canvas.Font.Color := clWindowText;
        Canvas.Font.Style := [];
      end;
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

procedure TFrmMain.RestoreFocusToUnit(ATree: TTreeView; ASquadIndex: Integer;
  const AUnitId: string);
var
  I: Integer;
  Node, ChildNode: TTreeNode;
  Info, ChildInfo: TNodeInfo;
begin
  try
    // Erst den Squad finden und expandieren
    for I := 0 to ATree.Items.Count - 1 do
    begin
      Node := ATree.Items[I];
      if Assigned(Node.Data) then
      begin
        Info := TNodeInfo(Node.Data);

        if (Info.Kind = nkSquad) and (Info.SquadIndex = ASquadIndex) then
        begin
          Node.Expand(False);

          // Jetzt die spezifische Unit in diesem Squad suchen
          ChildNode := Node.getFirstChild;
          while Assigned(ChildNode) do
          begin
            if Assigned(ChildNode.Data) then
            begin
              ChildInfo := TNodeInfo(ChildNode.Data);

              if (ChildInfo.Kind = nkUnit) and
                SameText(ChildInfo.UnitId, AUnitId) then
              begin
                ATree.Selected := Node;
                // Notlösung da richtige Unit nicht gefunden wird und Restoreto Sqad nicht geht.
                Exit;
              end;
            end;

            ChildNode := ChildNode.getNextSibling;
          end;
          ATree.Selected := Node;
          Exit;
        end;
      end;
    end;
  finally
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
  StartTime: TDateTime;
  ElapsedMs: Integer;
begin
  StartTime := Now;
  jachLog.LogInfo('=== SWAP-OPERATION gestartet ===');
  jachLog.LogDebug('Benutzer klickte "Units tauschen" Button');

  A := SelectedUnitInfo(TreeBase);
  B := SelectedUnitInfo(TreeTarget);

  if (not Assigned(A)) or (not Assigned(B)) then
  begin
    jachLog.LogInfo('Swap abgebrochen: Nicht beide Units ausgewählt');
    jachLog.LogDebug('TreeBase selection: %s, TreeTarget selection: %s',
      [IfThen(Assigned(A), 'vorhanden', 'null'), IfThen(Assigned(B),
      'vorhanden', 'null')]);
    Application.MessageBox
      ('Bitte links und rechts jeweils eine Unit auswählen.', 'Hinweis',
      MB_ICONINFORMATION);
    Exit;
  end;

  jachLog.LogInfo('SWAP-KANDIDATEN identifiziert:');
  jachLog.LogInfo('- Quelle: Unit %s in Squad %d', [A.UnitId, A.SquadIndex]);
  jachLog.LogInfo('- Ziel: Unit %s in Squad %d', [B.UnitId, B.SquadIndex]);

  BaseSquadIndex := A.SquadIndex;
  TargetSquadIndex := B.SquadIndex;

  // Validierungen mit detailliertem Logging
  if ChkOnlyHumans.Checked and IsValidUnit(A.UnitId) and IsEntityUnit(A.UnitId)
  then
  begin
    jachLog.LogInfo
      ('Swap abgebrochen: Quelle-Unit %s ist Entity bei aktivem Human-Filter',
      [A.UnitId]);
    Application.MessageBox
      ('Die linke Unit ist eine Entity und kann bei aktivem Filter nicht verschoben werden.',
      'Hinweis', MB_ICONINFORMATION);
    Exit;
  end;

  if ChkOnlyHumans.Checked and IsValidUnit(B.UnitId) and IsEntityUnit(B.UnitId)
  then
  begin
    jachLog.LogInfo
      ('Swap abgebrochen: Ziel-Unit %s ist Entity bei aktivem Human-Filter',
      [B.UnitId]);
    Application.MessageBox
      ('Die rechte Unit ist eine Entity und kann bei aktivem Filter nicht verschoben werden.',
      'Hinweis', MB_ICONINFORMATION);
    Exit;
  end;

  if (A.SquadIndex = B.SquadIndex) and (SameText(A.UnitId, B.UnitId)) then
  begin
    jachLog.LogInfo('Swap abgebrochen: Identische Unit ausgewählt (%s)',
      [A.UnitId]);
    Application.MessageBox
      ('Die gleiche Unit kann nicht mit sich selbst getauscht werden.',
      'Hinweis', MB_ICONINFORMATION);
    Exit;
  end;

  jachLog.LogDebug('Validierungen erfolgreich, führe Swap durch');

  try
    // Unit-Indizes in Arrays finden
    UnitIndexA := -1;
    UnitIndexB := -1;

    jachLog.LogDebug('Suche Index für Quelle-Unit %s in Squad %d',
      [A.UnitId, A.SquadIndex]);
    for I := 0 to Length(FSquads[A.SquadIndex].UnitIds) - 1 do
      if SameText(FSquads[A.SquadIndex].UnitIds[I], A.UnitId) then
      begin
        UnitIndexA := I;
        Break;
      end;

    jachLog.LogDebug('Suche Index für Ziel-Unit %s in Squad %d',
      [B.UnitId, B.SquadIndex]);
    for I := 0 to Length(FSquads[B.SquadIndex].UnitIds) - 1 do
      if SameText(FSquads[B.SquadIndex].UnitIds[I], B.UnitId) then
      begin
        UnitIndexB := I;
        Break;
      end;

    if (UnitIndexA = -1) or (UnitIndexB = -1) then
    begin
      jachLog.LogError
        ('Unit-Indizes nicht gefunden: UnitA-Index=%d, UnitB-Index=%d',
        [UnitIndexA, UnitIndexB]);
      raise Exception.Create('Unit-Indizes nicht gefunden');
    end;

    jachLog.LogDebug('Unit-Indizes gefunden: UnitA-Index=%d, UnitB-Index=%d',
      [UnitIndexA, UnitIndexB]);
    jachLog.LogInfo('Führe Swap durch: %s (Squad %d[%d]) <-> %s (Squad %d[%d])',
      [A.UnitId, A.SquadIndex, UnitIndexA, B.UnitId, B.SquadIndex, UnitIndexB]);

    SwapUnitsInSquads(A.SquadIndex, UnitIndexA, B.SquadIndex, UnitIndexB,
      A.UnitId, B.UnitId);
    jachLog.LogDebug('SwapUnitsInSquads erfolgreich ausgeführt');

    jachLog.LogDebug('Aktualisiere TreeView-Anzeige');
    UpdateTreeViewsFromSquads;

    jachLog.LogDebug('Stelle UI-Fokus wieder her');
    RestoreFocusToUnit(TreeBase, BaseSquadIndex, A.UnitId);
    RestoreFocusToUnit(TreeTarget, TargetSquadIndex, B.UnitId);

    // Info-Panel aktualisieren
    if Assigned(SelectedUnitInfo(TreeBase)) then
      UpdateUnitInfoPanel(SelectedUnitInfo(TreeBase).UnitId)
    else if Assigned(SelectedUnitInfo(TreeTarget)) then
      UpdateUnitInfoPanel(SelectedUnitInfo(TreeTarget).UnitId)
    else
      ClearInfoPanel;

    ElapsedMs := Round((Now - StartTime) * 24 * 60 * 60 * 1000);
    jachLog.LogInfo('SWAP-OPERATION erfolgreich abgeschlossen in %d ms',
      [ElapsedMs]);
    ShowStatus(GetLanguageStrings(Settings.Language).StatusUnitsSwapped);

  except
    on E: Exception do
    begin
      ElapsedMs := Round((Now - StartTime) * 24 * 60 * 60 * 1000);
      jachLog.LogError('SWAP-OPERATION fehlgeschlagen nach %d ms',
        [ElapsedMs], E);
      Application.MessageBox(
        PChar(Format(GetLanguageStrings(Settings.Language).ErrorSwapping, [E.Message])),
        PChar(GetLanguageStrings(Settings.Language).ErrorSwappingTitle),
        MB_ICONERROR);
    end;
  end;
end;

procedure TFrmMain.BtnSaveAsClick(Sender: TObject);
var
  FileSizeKB: Int64;
  StartTime: TDateTime;
  ElapsedMs: Integer;
  FileName: string;
begin
  jachLog.LogDebug('Benutzer klickte "Speichern als" Button');

  SaveDialog1.FilterIndex := 1;
  if not SaveDialog1.Execute then
  begin
    jachLog.LogDebug('Benutzer hat Speichern-Dialog abgebrochen');
    Exit;
  end;

  FileName := SaveDialog1.FileName;
  StartTime := Now;

  jachLog.LogInfo('=== SPEICHER-OPERATION gestartet ===');
  jachLog.LogInfo('Zieldatei: %s', [FileName]);

  if TFile.Exists(FileName) then
  begin
    jachLog.LogInfo('Überschreibe existierende Datei: %s', [FileName]);
    // Backup erstellen vor dem Überschreiben
    CreateBackup(FileName);
  end
  else
    jachLog.LogInfo('Erstelle neue Datei: %s', [FileName]);

  Screen.Cursor := crHourGlass;
  BtnSaveAs.Enabled := False;
  jachLog.LogDebug('UI in Speicher-Zustand versetzt');

  try
    if FSquadsDirty then
    begin
      jachLog.LogInfo
        ('Squad-Daten wurden geändert, aktualisiere Campaign-Daten');
      RebuildFCampaignFromSquads;
      jachLog.LogDebug('Campaign-Daten erfolgreich aktualisiert');
    end
    else
    begin
      jachLog.LogDebug
        ('Squad-Daten unverändert, keine Campaign-Aktualisierung erforderlich');
    end;

    jachLog.LogDebug('Rufe FSave.SaveToSaveAs auf');
    FSave.SaveToSaveAs(FileName);
    jachLog.LogInfo('Savegame erfolgreich geschrieben');

    // Erfolgsstatistiken
    try
      FileSizeKB := TFile.GetSize(FileName) div 1024;
      ElapsedMs := Round((Now - StartTime) * 24 * 60 * 60 * 1000);
      jachLog.LogInfo('SPEICHER-OPERATION erfolgreich abgeschlossen:');
      jachLog.LogInfo('- Datei: %s', [ExtractFileName(FileName)]);
      jachLog.LogInfo('- Größe: %d KB (%.2f MB)',
        [FileSizeKB, FileSizeKB / 1024]);
      jachLog.LogInfo('- Dauer: %d ms', [ElapsedMs]);

      if ElapsedMs > 5000 then
        jachLog.LogWarning('Langsame Speicher-Performance: %d ms (> 5s)',
          [ElapsedMs]);

    except
      on E: Exception do
      begin
        jachLog.LogWarning
          ('Konnte Statistiken nach Speichern nicht ermitteln: %s',
          [E.Message]);
        jachLog.LogInfo('SPEICHER-OPERATION erfolgreich abgeschlossen: %s',
          [ExtractFileName(FileName)]);
      end;
    end;

    ShowStatus(Format(GetLanguageStrings(Settings.Language).StatusSavedSuccessfully, [ExtractFileName(FileName)]));
    jachLog.LogDebug('Status-Anzeige aktualisiert');

    // UI-Feedback
    LblStatus.Caption := '✓ Datei erfolgreich gespeichert!';
    LblStatus.Font.Color := clGreen;
    LblStatus.Visible := True;
    jachLog.LogDebug('Erfolgs-UI-Feedback aktiviert');

    // Fenster 2x grün blinken lassen
    BlinkForm(RGB(144, 238, 144), 2); // Hellgrün
    jachLog.LogDebug('Erfolgs-Blink gestartet');

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
              LblStatus.Font.Color := clNavy;
              jachLog.LogDebug('Erfolgs-UI-Feedback automatisch deaktiviert');
            end;
          end);
      end).Start;

  except
    on E: Exception do
    begin
      ElapsedMs := Round((Now - StartTime) * 24 * 60 * 60 * 1000);
      jachLog.LogError('SPEICHER-OPERATION fehlgeschlagen nach %d ms',
        [ElapsedMs], E);
      jachLog.LogError('Zieldatei war: %s', [FileName]);

      ShowStatus(GetLanguageStrings(Settings.Language).StatusErrorSaving);
      jachLog.LogDebug('Status auf Fehler gesetzt');

      // Fehler-UI-Feedback
      LblStatus.Caption := '✗ Fehler beim Speichern!';
      LblStatus.Font.Color := clRed;
      LblStatus.Visible := True;
      jachLog.LogDebug('Fehler-UI-Feedback aktiviert');

      // Fenster 3x rot blinken lassen
      BlinkForm(RGB(255, 100, 100), 3); // Hellrot
      jachLog.LogDebug('Fehler-Blink gestartet');

      Application.MessageBox(
        PChar(Format(GetLanguageStrings(Settings.Language).ErrorSaving, [E.Message])),
        PChar(GetLanguageStrings(Settings.Language).ErrorSavingTitle),
        MB_ICONERROR);

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
                jachLog.LogDebug('Fehler-UI-Feedback automatisch deaktiviert');
              end;
            end);
        end).Start;
    end;
  end;

  Screen.Cursor := crDefault;
  BtnSaveAs.Enabled := True;
  jachLog.LogDebug('UI aus Speicher-Zustand zurückgesetzt');
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
  StartTime: TDateTime;
  ElapsedMs: Integer;
  LineCount: Integer;
  ErrorCount: Integer;
begin
  jachLog.LogInfo('=== CSV-EXPORT gestartet ===');
  jachLog.LogDebug('Benutzer klickte "CSV exportieren" Button');

  SaveDialog1.FilterIndex := 2; // CSV auswählen
  if not SaveDialog1.Execute then
  begin
    jachLog.LogDebug('Benutzer hat CSV-Export-Dialog abgebrochen');
    Exit;
  end;

  StartTime := Now;
  jachLog.LogInfo('CSV-Export nach: %s', [SaveDialog1.FileName]);

  SetUILoadingState(True);
  ShowProgress(GetLanguageStrings(Settings.Language).StatusExportingCsv);
  jachLog.LogDebug('UI in Export-Zustand versetzt');

  Csv := TStringList.Create;
  LineCount := 0;
  ErrorCount := 0;

  try
    // CSV-Header
    Csv.Add('Squad;UnitId;Type;Name;Veterancy');
    Inc(LineCount);
    jachLog.LogDebug('CSV-Header geschrieben');

    FAllSquadNames := FSave.GetSquadNames;
    jachLog.LogInfo('Exportiere %d Squads', [Length(FAllSquadNames)]);

    ShowProgress(GetLanguageStrings(Settings.Language).StatusExportingCsv, Length(FAllSquadNames));

    for I := 0 to Length(FAllSquadNames) - 1 do
    begin
      UpdateProgress(I, Format('Verarbeite Squad %d/%d für CSV-Export',
        [I + 1, Length(FAllSquadNames)]));

      jachLog.LogDebug('Exportiere Squad %d: "%s"', [I + 1, FAllSquadNames[I]]);

      Units := FSave.GetSquadMembers(I);
      jachLog.LogDebug('Squad "%s" hat %d Units',
        [FAllSquadNames[I], Length(Units)]);

      for J := 0 to Length(Units) - 1 do
      begin
        try
          if IsEmptySlot(Units[J]) then
          begin
            Line := Format('%s;%s;%s;%s;%s', [FAllSquadNames[I], Units[J],
              'Empty', '[Leer]', '']);
          end
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
                on E: Exception do
                begin
                  jachLog.LogWarning
                    ('CSV-Export: Veterancy-Fehler für Unit %s: %s',
                    [Units[J], E.Message]);
                  Inc(ErrorCount);
                end;
              end;
            end;

            Line := Format('%s;%s;%s;%s;%s', [FAllSquadNames[I], Units[J],
              Info.Kind, Info.Name, VeterancyText]);
          end;

        except
          on E: Exception do
          begin
            jachLog.LogError('CSV-Export: Unit-Fehler für %s in Squad "%s": %s',
              [Units[J], FAllSquadNames[I], E.Message]);
            Line := Format('%s;%s;%s;%s;%s', [FAllSquadNames[I], Units[J],
              'ERROR', 'Fehler beim Laden', '']);
            Inc(ErrorCount);
          end;
        end;

        Csv.Add(Line);
        Inc(LineCount);
      end;
    end;

    UpdateProgress(Length(FAllSquadNames), GetLanguageStrings(Settings.Language).CsvWriting);
    jachLog.LogDebug('Schreibe CSV-Datei mit %d Zeilen', [LineCount]);

    Csv.SaveToFile(SaveDialog1.FileName, TEncoding.UTF8);

    ElapsedMs := Round((Now - StartTime) * 24 * 60 * 60 * 1000);
    jachLog.LogInfo('CSV-EXPORT erfolgreich abgeschlossen:');
    jachLog.LogInfo('- Datei: %s', [ExtractFileName(SaveDialog1.FileName)]);
    jachLog.LogInfo('- Zeilen: %d (inkl. Header)', [LineCount]);
    jachLog.LogInfo('- Squads: %d', [Length(FAllSquadNames)]);
    jachLog.LogInfo('- Dauer: %d ms', [ElapsedMs]);

    if ErrorCount > 0 then
      jachLog.LogWarning('CSV-Export mit %d Fehlern abgeschlossen',
        [ErrorCount])
    else
      jachLog.LogInfo('CSV-Export vollständig erfolgreich abgeschlossen');

    ShowStatus(Format(GetLanguageStrings(Settings.Language).StatusCsvExported, [ExtractFileName(SaveDialog1.FileName)]));

  except
    on E: Exception do
    begin
      jachLog.LogError('CSV-EXPORT fehlgeschlagen', E);
      ShowStatus(GetLanguageStrings(Settings.Language).StatusErrorCsvExport);
      Application.MessageBox(
        PChar(Format(GetLanguageStrings(Settings.Language).ErrorCsvExport, [E.Message])),
        PChar(GetLanguageStrings(Settings.Language).ErrorCsvExportTitle),
        MB_ICONERROR);
    end;
  end;

  try
    Csv.Free;
    jachLog.LogDebug('CSV-StringList freigegeben');
  except
    on E: Exception do
      jachLog.LogWarning('Fehler beim Freigeben der CSV-StringList: %s',
        [E.Message]);
  end;

  SetUILoadingState(False);
  HideProgress;
  jachLog.LogDebug('Export-UI-Zustand beendet');
end;

procedure TFrmMain.ShowStatus(const Msg: string);
begin
  Caption := 'GOH Savegame Editor – ' + Msg;
  jachLog.LogDebug('Status-Caption gesetzt: "%s"', [Msg]);
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

procedure TFrmMain.UncheckMenu(AMenuItem: TMenuItem);
var
  I: Integer;
begin
  for I := 0 to AMenuItem.Count - 1 do
    AMenuItem.Items[I].Checked := False;
end;

procedure TFrmMain.DebugLevelClick(Sender: TObject);
var
  MenuItem: TMenuItem;
begin
  if Sender is TMenuItem then
  begin
    MenuItem := TMenuItem(Sender);
    jachLog.LogLevel[jachLog.DefaultTopic] := TLogLevel(MenuItem.Tag);
    UncheckMenu(Debuglevel);
    MenuItem.Checked := True;
    jachLog.LogEmergency('Benutzer hat den Debuglevel auf ' + MenuItem.Hint +
      ' gesetzt');
  end;
end;

procedure TFrmMain.BackupOptionen1Click(Sender: TObject);
var
  BackupForm: TFrmBackupOptions;
begin
  BackupForm := TFrmBackupOptions.Create(Self);
  try
    BackupForm.ShowModal;
  finally
    BackupForm.Free;
  end;
end;

procedure TFrmMain.LanguageClick(Sender: TObject);
var
  MenuItem: TMenuItem;
begin
  if Sender is TMenuItem then
  begin
    MenuItem := TMenuItem(Sender);
    Settings.Language := MenuItem.Hint; // 'de' oder 'en'
    Settings.Save;
    UncheckMenu(Sprache1);
    MenuItem.Checked := True;
    ApplyLanguage;
  end;
end;

procedure TFrmMain.ThemeClick(Sender: TObject);
var
  MenuItem: TMenuItem;
  StyleName: string;
begin
  if Sender is TMenuItem then
  begin
    MenuItem := TMenuItem(Sender);
    Settings.Theme := MenuItem.Hint; // 'light' oder 'dark'
    Settings.Save;
    UncheckMenu(Design1);
    MenuItem.Checked := True;

    // VCL Style anwenden
    if Settings.Theme = 'dark' then
      StyleName := 'Carbon'
    else
      StyleName := 'Amethyst Kamri';

    if TStyleManager.TrySetStyle(StyleName) then
    begin
      jachLog.LogInfo('Style gewechselt zu: %s', [StyleName]);
      ApplyThemeColors;
    end
    else
      jachLog.LogWarning('Style konnte nicht gewechselt werden: %s', [StyleName]);
  end;
end;

procedure TFrmMain.ApplyThemeColors;
begin
  if Settings.Theme = 'dark' then
  begin
    // Dark Theme Farben
    TreeBase.Color := RGB(45, 45, 48);
    TreeBase.Font.Color := clWhite;
    TreeTarget.Color := RGB(45, 45, 48);
    TreeTarget.Font.Color := clWhite;
    MemoInfo.Color := RGB(30, 30, 30);
    MemoInfo.Font.Color := clWhite;
    ListViewInventory.Color := RGB(45, 45, 48);
    ListViewInventory.Font.Color := clWhite;
  end
  else
  begin
    // Light Theme Farben (Standard)
    TreeBase.Color := clWindow;
    TreeBase.Font.Color := clWindowText;
    TreeTarget.Color := clWindow;
    TreeTarget.Font.Color := clWindowText;
    MemoInfo.Color := clWindow;
    MemoInfo.Font.Color := clWindowText;
    ListViewInventory.Color := clWindow;
    ListViewInventory.Font.Color := clWindowText;
  end;
end;

procedure TFrmMain.ApplyLanguage;
var
  Lang: TLanguageStrings;
begin
  Lang := GetLanguageStrings(Settings.Language);

  // MainForm
  Caption := Lang.FormCaption;
  if not Assigned(FSave) then
    LblSave.Caption := Lang.NoFile;
  LblBaseInfo.Caption := Lang.SourceNoSelection;
  LblTargetInfo.Caption := Lang.TargetNoSelection;
  LblStatus.Caption := Lang.LoadingData;
  BtnOpen.Caption := Lang.BtnLoadSave;
  BtnTransfer.Caption := Lang.BtnTransferDisabled;
  BtnSwap.Caption := Lang.BtnSwapUnits;
  BtnSaveAs.Caption := Lang.BtnSaveAs;
  BtnExportCsv.Caption := Lang.BtnExportCsv;
  ChkOnlyHumans.Caption := Lang.ChkOnlyHumans;
  TabGeneral.Caption := Lang.TabGeneral;
  TabInventory.Caption := Lang.TabInventory;

  // Inventory columns
  ListViewInventory.Columns[0].Caption := Lang.ColItem;
  ListViewInventory.Columns[1].Caption := Lang.ColType;
  ListViewInventory.Columns[2].Caption := Lang.ColCount;
  ListViewInventory.Columns[3].Caption := Lang.ColPosition;
  ListViewInventory.Columns[4].Caption := Lang.ColNote;

  // Menu
  Optionen1.Caption := Lang.MenuOptions;
  BackupOptionen1.Caption := Lang.MenuBackupOptions;
  Debuglevel.Caption := Lang.MenuDebugLevel;
  Sprache1.Caption := Lang.MenuLanguage;
  Design1.Caption := Lang.MenuTheme;
  Hell1.Caption := Lang.ThemeLight;
  Dunkel1.Caption := Lang.ThemeDark;

  // Sortier-Menü neu aufbauen
  RebuildSortMenu;

  // Info-Panel aktualisieren
  if Assigned(TreeBase.Selected) and Assigned(TreeBase.Selected.Data) then
    UpdateUnitInfoPanel(TNodeInfo(TreeBase.Selected.Data).UnitId)
  else if Assigned(TreeTarget.Selected) and Assigned(TreeTarget.Selected.Data) then
    UpdateUnitInfoPanel(TNodeInfo(TreeTarget.Selected.Data).UnitId)
  else
    ClearInfoPanel;

  jachLog.LogInfo('Sprache gewechselt zu: %s', [Settings.Language]);
end;

procedure TFrmMain.SortSquads(Criteria: TSquadSortCriteria;
Direction: TSquadSortDirection);
var
  StartTime: TDateTime;
  ElapsedMs: Integer;
begin
  StartTime := Now;
  jachLog.LogInfo('=== Squad-Sortierung gestartet ===');
  jachLog.LogInfo('Kriterium: %s, Richtung: %s',
    [GetEnumName(TypeInfo(TSquadSortCriteria), Ord(Criteria)),
    GetEnumName(TypeInfo(TSquadSortDirection), Ord(Direction))]);

  // Prüfung auf ungespeicherte Änderungen
  if FSquadsDirty then
  begin
    case Application.MessageBox
      (PChar(GetLanguageStrings(Settings.Language).MsgUnsavedChanges),
      PChar(GetLanguageStrings(Settings.Language).MsgUnsavedChangesTitle),
      MB_YESNOCANCEL + MB_ICONQUESTION) of

      IDYES:
        begin
          jachLog.LogInfo
            ('Benutzer wählte: Änderungen speichern und sortieren');
          RebuildFCampaignFromSquads;
          FSquadsDirty := False;
          jachLog.LogDebug
            ('Ungespeicherte Änderungen in campaign.scn übernommen');
        end;

      IDNO:
        begin
          jachLog.LogInfo
            ('Benutzer wählte: Änderungen verwerfen und sortieren');
          LoadSquadsFromSave;
          FSquadsDirty := False;
          jachLog.LogDebug
            ('Cache von campaign.scn neu geladen, Änderungen verworfen');
        end;

      IDCANCEL:
        begin
          jachLog.LogInfo('Benutzer wählte: Sortierung abbrechen');
          Exit;
        end;
    end;
  end;

  SetUILoadingState(True);
  ShowProgress(GetLanguageStrings(Settings.Language).StatusSortingSquads);

  try
    // Direkte Sortierung in campaign.scn
    FSave.SortSquadLines(Criteria, Direction);

    // Cache und UI komplett neu laden
    LoadSquadsFromSave;
    UpdateTreeViewsFromSquads;
    ClearInfoPanel;

    ElapsedMs := Round((Now - StartTime) * 24 * 60 * 60 * 1000);
    jachLog.LogInfo('Squad-Sortierung erfolgreich abgeschlossen in %d ms',
      [ElapsedMs]);
    ShowStatus(Format(GetLanguageStrings(Settings.Language).StatusSquadsSorted,
      [GetSortCriteriaDisplayName(Criteria)]));

  except
    on E: Exception do
    begin
      jachLog.LogError('Fehler bei Squad-Sortierung', E);
      ShowStatus(GetLanguageStrings(Settings.Language).StatusErrorSorting);
      Application.MessageBox(PChar(Format(GetLanguageStrings(Settings.Language).MsgErrorSorting, [E.Message])),
        PChar(GetLanguageStrings(Settings.Language).MsgErrorSortingTitle), MB_ICONERROR);
    end;
  end;

  SetUILoadingState(False);
  HideProgress;
end;

function TFrmMain.GetSortCriteriaDisplayName
  (Criteria: TSquadSortCriteria): string;
var
  Lang: TLanguageStrings;
begin
  Lang := GetLanguageStrings(Settings.Language);
  case Criteria of
    scName:
      Result := Lang.SortCriteriaName;
    scStage:
      Result := Lang.SortCriteriaStage;
    scAverageVet:
      Result := Lang.SortCriteriaAvgVet;
    scMaxVet:
      Result := Lang.SortCriteriaMaxVet;
  else
    Result := 'Unknown';
  end;
end;

// Kompakte Event-Handler-Lösung für Sortierung
procedure TFrmMain.SortMenuItemClick(Sender: TObject);
var
  MenuItem: TMenuItem;
  SortInfo: TSortMenuInfo;
begin
  if Sender is TMenuItem then
  begin
    MenuItem := TMenuItem(Sender);
    // Tag enthält Index ins FSortMenuInfos-Array
    if (MenuItem.Tag >= 0) and (MenuItem.Tag < Length(FSortMenuInfos)) then
    begin
      SortInfo := FSortMenuInfos[MenuItem.Tag];
      SortSquads(SortInfo.Criteria, SortInfo.Direction);
    end;
  end;
end;

procedure TFrmMain.AddSortMenuItems;
const
  SORT_MENU_INFOS: array [0 .. 7] of TSortMenuInfo = (
    (Criteria: scName; Direction: sdAscending; Caption: 'Nach Name (A-Z)'),
    (Criteria: scName; Direction: sdDescending; Caption: 'Nach Name (Z-A)'),
    (Criteria: scStage; Direction: sdAscending; Caption: 'Nach Gruppe (aufsteigend)'),
    (Criteria: scStage; Direction: sdDescending; Caption: 'Nach Gruppe (absteigend)'),
    (Criteria: scAverageVet; Direction: sdAscending; Caption: 'Nach durchschn. Veteranenstufe (aufsteigend)'),
    (Criteria: scAverageVet; Direction: sdDescending; Caption: 'Nach durchschn. Veteranenstufe (absteigend)'),
    (Criteria: scMaxVet; Direction: sdAscending; Caption: 'Nach max. Veteranenstufe (aufsteigend)'),
    (Criteria: scMaxVet; Direction: sdDescending; Caption: 'Nach max. Veteranenstufe (absteigend)')
  );

  function NeedsSeparatorAfter(Index: Integer): Boolean;
  begin
    Result := (Index = 1) or (Index = 3) or (Index = 5);
  end;

var
  SortMenu: TMenuItem;
  MenuItem, SepItem: TMenuItem;
  I: Integer;
begin
  // Array für Event-Handler speichern
  SetLength(FSortMenuInfos, Length(SORT_MENU_INFOS));
  for I := 0 to High(SORT_MENU_INFOS) do
    FSortMenuInfos[I] := SORT_MENU_INFOS[I];

  FSortMenu := TMenuItem.Create(Self);
  FSortMenu.Caption := GetLanguageStrings(Settings.Language).MenuSort;

  // Menü-Items in Schleife erstellen
  for I := 0 to High(SORT_MENU_INFOS) do
  begin
    MenuItem := TMenuItem.Create(Self);
    MenuItem.Caption := GetSortMenuCaption(Ord(SORT_MENU_INFOS[I].Criteria),
                                            Ord(SORT_MENU_INFOS[I].Direction),
                                            Settings.Language);
    MenuItem.Tag := I; // Index für Event-Handler
    MenuItem.OnClick := SortMenuItemClick;
    FSortMenu.Add(MenuItem);

    // Trennstrich nach bestimmten Items
    if NeedsSeparatorAfter(I) then
    begin
      SepItem := TMenuItem.Create(Self);
      SepItem.Caption := '-';
      FSortMenu.Add(SepItem);
    end;
  end;

  MainMenu1.Items.Add(FSortMenu);
end;

procedure TFrmMain.RebuildSortMenu;
var
  I: Integer;
begin
  if not Assigned(FSortMenu) then
    Exit;

  // Sortier-Menü Titel aktualisieren
  FSortMenu.Caption := GetLanguageStrings(Settings.Language).MenuSort;

  // Alle Menüeinträge durchgehen und Captions aktualisieren (außer Trennstriche)
  for I := 0 to FSortMenu.Count - 1 do
  begin
    if FSortMenu.Items[I].Caption <> '-' then
    begin
      // Tag enthält den Index in FSortMenuInfos
      if FSortMenu.Items[I].Tag < Length(FSortMenuInfos) then
      begin
        FSortMenu.Items[I].Caption := GetSortMenuCaption(
          Ord(FSortMenuInfos[FSortMenu.Items[I].Tag].Criteria),
          Ord(FSortMenuInfos[FSortMenu.Items[I].Tag].Direction),
          Settings.Language);
      end;
    end;
  end;
end;

function TFrmMain.GetStageGroupDisplay(const Stage: string): string;
begin
  if Stage = '' then
    Result := GetLanguageStrings(Settings.Language).StageNoGroup
  else
    Result := ' [' + Stage + ']';
end;

end.
