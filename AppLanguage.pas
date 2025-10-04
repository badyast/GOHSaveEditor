unit AppLanguage;

interface

uses
  System.SysUtils;

type
  TLanguageStrings = record
    // MainForm
    FormCaption: string;
    NoFile: string;
    SourceNoSelection: string;
    TargetNoSelection: string;
    LoadingData: string;
    BtnLoadSave: string;
    BtnTransferDisabled: string;
    BtnSwapUnits: string;
    BtnSaveAs: string;
    BtnExportCsv: string;
    BtnExpandAll: string;
    BtnCollapseAll: string;
    ChkOnlyHumans: string;
    TabGeneral: string;
    TabInventory: string;
    // Inventory columns
    ColItem: string;
    ColType: string;
    ColCount: string;
    ColPosition: string;
    ColNote: string;
    // Menu
    MenuOptions: string;
    MenuBackupOptions: string;
    MenuDebugLevel: string;
    MenuLanguage: string;
    MenuTheme: string;
    ThemeLight: string;
    ThemeDark: string;
    // BackupOptionsForm
    BackupOptionsCaption: string;
    LblBackupFolder: string;
    LblMaxBackups: string;
    BtnBrowse: string;
    ChkUnlimited: string;
    BackupInfo: string;
    BtnOK: string;
    BtnCancel: string;
    // Messages
    MsgInvalidNumber: string;
    // Info Panel
    InfoNoUnitSelected: string;
    InfoEmptySlot: string;
    InfoUnitInformation: string;
    InfoUnitId: string;
    InfoType: string;
    InfoUnitClass: string;
    InfoName: string;
    InfoPosition: string;
    InfoVeterancy: string;
    InfoScore: string;
    InfoInfantryKills: string;
    InfoLastWeapon: string;
    InfoLastGrenade: string;
    InfoFsmState: string;
    InfoEquipped: string;
    // Status messages
    StatusLoadingUnitData: string;
    StatusLoadingSave: string;
    StatusSaveLoaded: string;
    StatusErrorLoading: string;
    StatusUnitsSwapped: string;
    StatusSavedSuccessfully: string;
    StatusErrorSaving: string;
    StatusExportingCsv: string;
    StatusCsvExported: string;
    StatusErrorCsvExport: string;
    StatusSortingSquads: string;
    StatusSquadsSorted: string;
    StatusErrorSorting: string;
    // Messages
    MsgUnsavedChanges: string;
    MsgUnsavedChangesTitle: string;
    MsgErrorSorting: string;
    MsgErrorSortingTitle: string;
    MenuSort: string;
    // Sort menu items
    SortByNameAsc: string;
    SortByNameDesc: string;
    SortByStageAsc: string;
    SortByStageDesc: string;
    SortByAvgVetAsc: string;
    SortByAvgVetDesc: string;
    SortByMaxVetAsc: string;
    SortByMaxVetDesc: string;
    // Additional UI strings
    UnitEmpty: string;
    UnitError: string;
    CsvWriting: string;
    StageNoGroup: string;
    GroupPrefix: string;
    GroupSpecial: string;
    SortCriteriaName: string;
    SortCriteriaStage: string;
    SortCriteriaAvgVet: string;
    SortCriteriaMaxVet: string;
    SelectBackupFolder: string;
    // Error messages
    ErrorLoadingTitle: string;
    ErrorLoading: string;
    ErrorSwappingTitle: string;
    ErrorSwapping: string;
    ErrorSavingTitle: string;
    ErrorSaving: string;
    ErrorCsvExportTitle: string;
    ErrorCsvExport: string;
    MsgNoSaveLoadedTitle: string;
    MsgNoSaveLoaded: string;
  end;

function GetLanguageStrings(const LanguageCode: string): TLanguageStrings;
function GetGermanStrings: TLanguageStrings;
function GetEnglishStrings: TLanguageStrings;
function GetSortMenuCaption(Criteria: Integer; Direction: Integer; const LanguageCode: string): string;

implementation

uses
  ConquestSave;

function GetSortMenuCaption(Criteria: Integer; Direction: Integer; const LanguageCode: string): string;
var
  Lang: TLanguageStrings;
begin
  Lang := GetLanguageStrings(LanguageCode);

  // scName = 0, scStage = 1, scAverageVet = 2, scMaxVet = 3
  // sdAscending = 0, sdDescending = 1

  if (Criteria = 0) and (Direction = 0) then
    Result := Lang.SortByNameAsc
  else if (Criteria = 0) and (Direction = 1) then
    Result := Lang.SortByNameDesc
  else if (Criteria = 1) and (Direction = 0) then
    Result := Lang.SortByStageAsc
  else if (Criteria = 1) and (Direction = 1) then
    Result := Lang.SortByStageDesc
  else if (Criteria = 2) and (Direction = 0) then
    Result := Lang.SortByAvgVetAsc
  else if (Criteria = 2) and (Direction = 1) then
    Result := Lang.SortByAvgVetDesc
  else if (Criteria = 3) and (Direction = 0) then
    Result := Lang.SortByMaxVetAsc
  else if (Criteria = 3) and (Direction = 1) then
    Result := Lang.SortByMaxVetDesc
  else
    Result := '';
end;

function GetGermanStrings: TLanguageStrings;
begin
  Result.FormCaption := 'GOH Savegame Editor';
  Result.NoFile := '(kein File)';
  Result.SourceNoSelection := 'Quelle: (keine Auswahl)';
  Result.TargetNoSelection := 'Ziel: (keine Auswahl)';
  Result.LoadingData := 'Lade Daten...';
  Result.BtnLoadSave := 'Save laden'#8230;
  Result.BtnTransferDisabled := '(Transfer deaktiviert)';
  Result.BtnSwapUnits := 'Units tauschen '#8596;
  Result.BtnSaveAs := 'Speichern als'#8230;
  Result.BtnExportCsv := 'CSV exportieren'#8230;
  Result.BtnExpandAll := 'Alles ausklappen';
  Result.BtnCollapseAll := 'Alles einklappen';
  Result.ChkOnlyHumans := 'Nur Menschen verschieben';
  Result.TabGeneral := 'Allgemein';
  Result.TabInventory := 'Inventar';
  Result.ColItem := 'Item';
  Result.ColType := 'Typ';
  Result.ColCount := 'Anzahl';
  Result.ColPosition := 'Position';
  Result.ColNote := 'Anmerkung';
  Result.MenuOptions := 'Optionen';
  Result.MenuBackupOptions := 'Backup-Optionen...';
  Result.MenuDebugLevel := 'Debuglevel';
  Result.MenuLanguage := 'Sprache';
  Result.MenuTheme := 'Design';
  Result.ThemeLight := 'Hell';
  Result.ThemeDark := 'Dunkel';
  Result.BackupOptionsCaption := 'Backup-Optionen';
  Result.LblBackupFolder := 'Backup-Ordner:';
  Result.LblMaxBackups := 'Maximale Anzahl Backups:';
  Result.BtnBrowse := 'Durchsuchen...';
  Result.ChkUnlimited := 'Unbegrenzt (alle behalten)';
  Result.BackupInfo := 'Backups werden automatisch beim Überschreiben eines Saves erstellt. '+
    'Ältere Backups werden gelöscht, wenn die maximale Anzahl überschritten wird.';
  Result.BtnOK := 'OK';
  Result.BtnCancel := 'Abbrechen';
  Result.MsgInvalidNumber := 'Bitte geben Sie eine gültige Zahl größer als 0 ein.';
  Result.InfoNoUnitSelected := 'Keine Unit ausgewählt';
  Result.InfoEmptySlot := 'Leerer Slot';
  Result.InfoUnitInformation := '=== UNIT INFORMATIONEN ===';
  Result.InfoUnitId := 'Unit ID:';
  Result.InfoType := 'Typ:';
  Result.InfoUnitClass := 'Unit-Klasse:';
  Result.InfoName := 'Name:';
  Result.InfoPosition := 'Position:';
  Result.InfoVeterancy := 'Veteranenstufe:';
  Result.InfoScore := 'Score:';
  Result.InfoInfantryKills := 'Infantry Kills:';
  Result.InfoLastWeapon := 'Letzte Waffe:';
  Result.InfoLastGrenade := 'Letzte Granate:';
  Result.InfoFsmState := 'FSM-Status:';
  Result.InfoEquipped := 'Ausgerüstet';
  Result.StatusLoadingUnitData := 'Lade Unit-Daten...';
  Result.StatusLoadingSave := 'Lade Savegame...';
  Result.StatusSaveLoaded := 'Save geladen: %s';
  Result.StatusErrorLoading := 'Fehler beim Laden!';
  Result.StatusUnitsSwapped := 'Units getauscht.';
  Result.StatusSavedSuccessfully := '✓ Erfolgreich gespeichert: %s';
  Result.StatusErrorSaving := 'Fehler beim Speichern!';
  Result.StatusExportingCsv := 'Exportiere CSV-Datei...';
  Result.StatusCsvExported := 'CSV exportiert: %s';
  Result.StatusErrorCsvExport := 'Fehler beim CSV-Export!';
  Result.StatusSortingSquads := 'Sortiere Squads...';
  Result.StatusSquadsSorted := 'Squads sortiert nach %s';
  Result.StatusErrorSorting := 'Fehler beim Sortieren!';
  Result.MsgUnsavedChanges := 'Sie haben ungespeicherte Änderungen an den Squad-Zusammenstellungen. '+
    'Diese müssen vor der Sortierung gespeichert werden.'#13#13+
    'Möchten Sie die Änderungen jetzt speichern und dann sortieren?';
  Result.MsgUnsavedChangesTitle := 'Ungespeicherte Änderungen';
  Result.MsgErrorSorting := 'Fehler beim Sortieren: %s';
  Result.MsgErrorSortingTitle := 'Fehler';
  Result.MenuSort := '&Sortieren';
  Result.SortByNameAsc := 'Nach Name (A-Z)';
  Result.SortByNameDesc := 'Nach Name (Z-A)';
  Result.SortByStageAsc := 'Nach Gruppe (aufsteigend)';
  Result.SortByStageDesc := 'Nach Gruppe (absteigend)';
  Result.SortByAvgVetAsc := 'Nach durchschn. Veteranenstufe (aufsteigend)';
  Result.SortByAvgVetDesc := 'Nach durchschn. Veteranenstufe (absteigend)';
  Result.SortByMaxVetAsc := 'Nach max. Veteranenstufe (aufsteigend)';
  Result.SortByMaxVetDesc := 'Nach max. Veteranenstufe (absteigend)';
  Result.UnitEmpty := '[Leer]';
  Result.UnitError := '(Fehler)';
  Result.CsvWriting := 'Schreibe CSV-Datei...';
  Result.StageNoGroup := ' [Keine Gruppe]';
  Result.GroupPrefix := 'Gruppe';
  Result.GroupSpecial := 'Spezial';
  Result.SortCriteriaName := 'Name';
  Result.SortCriteriaStage := 'Gruppe';
  Result.SortCriteriaAvgVet := 'Durchschn. Veteranenstufe';
  Result.SortCriteriaMaxVet := 'Max. Veteranenstufe';
  Result.SelectBackupFolder := 'Backup-Ordner wählen';
  Result.ErrorLoadingTitle := 'Fehler';
  Result.ErrorLoading := 'Fehler beim Laden: %s';
  Result.ErrorSwappingTitle := 'Fehler';
  Result.ErrorSwapping := 'Fehler beim Tauschen: %s';
  Result.ErrorSavingTitle := 'Fehler';
  Result.ErrorSaving := 'Fehler beim Speichern: %s';
  Result.ErrorCsvExportTitle := 'Fehler';
  Result.ErrorCsvExport := 'Fehler beim CSV-Export: %s';
  Result.MsgNoSaveLoadedTitle := 'Kein Save geladen';
  Result.MsgNoSaveLoaded := 'Bitte zuerst einen Save laden!';
end;

function GetEnglishStrings: TLanguageStrings;
begin
  Result.FormCaption := 'GOH Savegame Editor';
  Result.NoFile := '(no file)';
  Result.SourceNoSelection := 'Source: (no selection)';
  Result.TargetNoSelection := 'Target: (no selection)';
  Result.LoadingData := 'Loading data...';
  Result.BtnLoadSave := 'Load save'#8230;
  Result.BtnTransferDisabled := '(Transfer disabled)';
  Result.BtnSwapUnits := 'Swap units '#8596;
  Result.BtnSaveAs := 'Save as'#8230;
  Result.BtnExportCsv := 'Export CSV'#8230;
  Result.BtnExpandAll := 'Expand all';
  Result.BtnCollapseAll := 'Collapse all';
  Result.ChkOnlyHumans := 'Move humans only';
  Result.TabGeneral := 'General';
  Result.TabInventory := 'Inventory';
  Result.ColItem := 'Item';
  Result.ColType := 'Type';
  Result.ColCount := 'Count';
  Result.ColPosition := 'Position';
  Result.ColNote := 'Note';
  Result.MenuOptions := 'Options';
  Result.MenuBackupOptions := 'Backup Options...';
  Result.MenuDebugLevel := 'Debug Level';
  Result.MenuLanguage := 'Language';
  Result.MenuTheme := 'Theme';
  Result.ThemeLight := 'Light';
  Result.ThemeDark := 'Dark';
  Result.BackupOptionsCaption := 'Backup Options';
  Result.LblBackupFolder := 'Backup folder:';
  Result.LblMaxBackups := 'Maximum backup count:';
  Result.BtnBrowse := 'Browse...';
  Result.ChkUnlimited := 'Unlimited (keep all)';
  Result.BackupInfo := 'Backups are automatically created when overwriting a save file. '+
    'Older backups will be deleted when the maximum count is exceeded.';
  Result.BtnOK := 'OK';
  Result.BtnCancel := 'Cancel';
  Result.MsgInvalidNumber := 'Please enter a valid number greater than 0.';
  Result.InfoNoUnitSelected := 'No unit selected';
  Result.InfoEmptySlot := 'Empty slot';
  Result.InfoUnitInformation := '=== UNIT INFORMATION ===';
  Result.InfoUnitId := 'Unit ID:';
  Result.InfoType := 'Type:';
  Result.InfoUnitClass := 'Unit Class:';
  Result.InfoName := 'Name:';
  Result.InfoPosition := 'Position:';
  Result.InfoVeterancy := 'Veterancy:';
  Result.InfoScore := 'Score:';
  Result.InfoInfantryKills := 'Infantry Kills:';
  Result.InfoLastWeapon := 'Last Weapon:';
  Result.InfoLastGrenade := 'Last Grenade:';
  Result.InfoFsmState := 'FSM State:';
  Result.InfoEquipped := 'Equipped';
  Result.StatusLoadingUnitData := 'Loading unit data...';
  Result.StatusLoadingSave := 'Loading savegame...';
  Result.StatusSaveLoaded := 'Save loaded: %s';
  Result.StatusErrorLoading := 'Error loading!';
  Result.StatusUnitsSwapped := 'Units swapped.';
  Result.StatusSavedSuccessfully := '✓ Successfully saved: %s';
  Result.StatusErrorSaving := 'Error saving!';
  Result.StatusExportingCsv := 'Exporting CSV file...';
  Result.StatusCsvExported := 'CSV exported: %s';
  Result.StatusErrorCsvExport := 'Error exporting CSV!';
  Result.StatusSortingSquads := 'Sorting squads...';
  Result.StatusSquadsSorted := 'Squads sorted by %s';
  Result.StatusErrorSorting := 'Error sorting!';
  Result.MsgUnsavedChanges := 'You have unsaved changes to the squad compositions. '+
    'These must be saved before sorting.'#13#13+
    'Do you want to save the changes now and then sort?';
  Result.MsgUnsavedChangesTitle := 'Unsaved Changes';
  Result.MsgErrorSorting := 'Error sorting: %s';
  Result.MsgErrorSortingTitle := 'Error';
  Result.MenuSort := '&Sort';
  Result.SortByNameAsc := 'By Name (A-Z)';
  Result.SortByNameDesc := 'By Name (Z-A)';
  Result.SortByStageAsc := 'By Group (ascending)';
  Result.SortByStageDesc := 'By Group (descending)';
  Result.SortByAvgVetAsc := 'By Avg. Veterancy (ascending)';
  Result.SortByAvgVetDesc := 'By Avg. Veterancy (descending)';
  Result.SortByMaxVetAsc := 'By Max. Veterancy (ascending)';
  Result.SortByMaxVetDesc := 'By Max. Veterancy (descending)';
  Result.UnitEmpty := '[Empty]';
  Result.UnitError := '(Error)';
  Result.CsvWriting := 'Writing CSV file...';
  Result.StageNoGroup := ' [No Group]';
  Result.GroupPrefix := 'Group';
  Result.GroupSpecial := 'Special';
  Result.SortCriteriaName := 'Name';
  Result.SortCriteriaStage := 'Group';
  Result.SortCriteriaAvgVet := 'Avg. Veterancy';
  Result.SortCriteriaMaxVet := 'Max. Veterancy';
  Result.SelectBackupFolder := 'Select Backup Folder';
  Result.ErrorLoadingTitle := 'Error';
  Result.ErrorLoading := 'Error loading: %s';
  Result.ErrorSwappingTitle := 'Error';
  Result.ErrorSwapping := 'Error swapping: %s';
  Result.ErrorSavingTitle := 'Error';
  Result.ErrorSaving := 'Error saving: %s';
  Result.ErrorCsvExportTitle := 'Error';
  Result.ErrorCsvExport := 'Error exporting CSV: %s';
  Result.MsgNoSaveLoadedTitle := 'No Save Loaded';
  Result.MsgNoSaveLoaded := 'Please load a save first!';
end;

function GetLanguageStrings(const LanguageCode: string): TLanguageStrings;
begin
  if SameText(LanguageCode, 'en') then
    Result := GetEnglishStrings
  else
    Result := GetGermanStrings; // Default
end;

end.
