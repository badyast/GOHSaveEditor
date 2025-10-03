# GOHSaveEditor - Projekt-Kontext für Claude Code

## Projekt-Übersicht
GOHSaveEditor ist ein Save-Editor für das Spiel "Gates of Hell: Ostfront" (GoH).
- **Entwicklungsumgebung**: Embarcadero Delphi 12.1 Community Edition
- **Framework**: VCL (Visual Component Library)
- **Plattform**: Windows (Win32/Win64)
- **Sprache**: Object Pascal

## Build-Prozess
- **WICHTIG**: Verwende IMMER das PowerShell-Script für Builds:
  ```powershell
  powershell.exe -ExecutionPolicy Bypass -File "C:\scripts\build-delphi.ps1" -ProjectFile "C:\Users\badya\Documents\GitHub\GOHSaveEditor\GOHSaveEditor.dproj" -Config Release -Platform Win64
  ```
- **NIEMALS** `msbuild` direkt verwenden - Delphi Community Edition unterstützt keine MSBuild-Kommandozeile
- Siehe `Building.md` für Details

## Architektur-Entscheidungen

### Mehrsprachigkeit (Deutsch/English)
- **System**: Custom TLanguageStrings Record in `AppLanguage.pas`
- **Keine externen Bibliotheken**: Alle Strings in Code definiert
- **Sprach-Erkennung**: Automatisch beim ersten Start via `GetUserDefaultUILanguage()`
- **Persistierung**: In `settings.ini` unter `[General] Language=de|en`
- **Anwendung**: `GetLanguageStrings(Settings.Language)` für alle UI-Texte

### Theme-System (Dark/Light Mode)
- **VCL Styles**:
  - Dark Mode: `Carbon.vsf`
  - Light Mode: `Amethyst Kamri.vsf`
- **Windows-Integration**: Automatische Erkennung via Registry `HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Themes\Personalize\AppsUseLightTheme`
- **Custom Farben**: TreeView, ListView, Memo werden manuell in `ApplyThemeColors()` angepasst
- **Stage-Gruppen-Farben**: Unterschiedliche Farb-Paletten für Dark/Light Theme in `TreeCustomDrawItem`
- **Persistierung**: In `settings.ini` unter `[General] Theme=dark|light`

### Einstellungs-Verwaltung
- **Klasse**: `TAppSettings` in `AppSettings.pas`
- **Datei**: `settings.ini` im Programmverzeichnis
- **Properties**:
  - `Language`: Sprache (de/en)
  - `Theme`: Theme (dark/light)
  - `BackupFolder`: Backup-Verzeichnis
  - `MaxBackupCount`: Maximale Anzahl Backups (-1 = unbegrenzt)
- **Auto-Erkennung**: Sprache und Theme werden beim ersten Start automatisch erkannt

### Logging
- **Bibliothek**: jachLog (lokale Dependency in `CatalogRepository\jachLog`)
- **Verwendung**: Für Debug-Ausgaben und Fehler-Tracking
- **Log-Level**: Über Menü einstellbar

## Code-Konventionen

### Delphi-Spezifika
- **String-Literale**: Verwende einfache Quotes `'Text'`
- **Kommentare**: `//` für einzeilig, `{ }` oder `(* *)` für mehrzeilig
- **Naming**:
  - Klassen: `TClassName`
  - Felder: `FFieldName`
  - Properties: `PropertyName`
  - Methoden: `MethodName`
- **Einrückung**: 2 Spaces (nicht Tabs)

### UI-Texte
- **NIEMALS** hartcodierte deutsche Strings in UI-Code
- **IMMER** `GetLanguageStrings(Settings.Language).StringName` verwenden
- **Neue Strings**: Zuerst in `AppLanguage.pas` für beide Sprachen definieren

### Theme-Anpassungen
- Bei neuen UI-Komponenten mit Farben: Theme-Unterstützung in `ApplyThemeColors()` hinzufügen
- Bei CustomDraw-Elementen: `if Settings.Theme = 'dark' then` Abfragen einbauen

## Wichtige Dateien

### Core-Module
- `GOHSaveEditor.dpr`: Hauptprogramm, Initialisierung
- `MainForm.pas/.dfm`: Hauptformular mit allen UI-Elementen
- `ConquestSave.pas`: Save-Datei Parser/Writer
- `AppSettings.pas`: Einstellungs-Verwaltung
- `AppLanguage.pas`: Mehrsprachigkeits-System
- `BackupOptionsForm.pas/.dfm`: Backup-Einstellungen Dialog

### Daten-Strukturen
- `Entitys.pas`: Entity-Definitionen

### Ignorierte Dateien (Git)
- `settings.ini`: Benutzerspezifische Einstellungen
- `Backups/`: Backup-Verzeichnis
- `status`, `campaign.scn`: Benutzerspezifische Save-Dateien
- `Win32/`, `Win64/`, `__history/`, `*.local`, `*.identcache`, `*.~*`

## Bekannte Compiler-Hinweise (unkritisch)
- Unbenutzte Variablen in `ConquestSave.pas` (M, StartPos, EndPos, BraceCount)
- Möglicherweise nicht initialisierte Variablen (DirSize, FileCount) - werden in try-block behandelt
- Unbenutzte private Methoden - für zukünftige Features vorgehalten

## Testing
- **Manuell**: Über Delphi IDE (F9)
- **Wichtig**: Nach jeder Änderung Build durchführen, um Fehler frühzeitig zu erkennen
