# Changelog

Alle wichtigen Änderungen an diesem Projekt werden in dieser Datei dokumentiert.

Das Format basiert auf [Keep a Changelog](https://keepachangelog.com/de/1.0.0/).

---

## [0.9.0-beta] - 2025-01-XX

### 🎉 Erstes öffentliches Release

**GOH Savegame Editor** - Ein Tool zum Bearbeiten von Spielständen für *Gates of Hell: Ostfront*.

### ✨ Features

#### Kern-Funktionalität
- **Savegame laden und speichern**: Unterstützung für Dynamische Kampagnen
- **Unit-Transfer**: Einheiten zwischen Squads verschieben
  - Nur-Menschen-Filter für gezieltes Verschieben
  - Swap-Funktion zum Tauschen von Units
  - Intelligente Validierung (keine Duplikate, keine ungültigen IDs)
- **CSV-Export**: Exportiere alle Squads mit Einheiten-Details
- **Inventar-Ansicht**: Zeigt alle Items im Savegame an

#### Squad-Management
- **Squad-Sortierung** mit verschiedenen Kriterien:
  - Nach Name (A-Z / Z-A)
  - Nach Gruppe (aufsteigend / absteigend)
  - Nach durchschnittlicher Veteranenstufe
  - Nach maximaler Veteranenstufe
- **TreeView-Navigation**:
  - Gruppierung nach Stage (Gruppe 1-5, Spezial)
  - Expand/Collapse Buttons für schnelle Übersicht
  - Anzeige von Veteranenstufen und Unit-Namen

#### Backup-System
- **Automatische Backups** beim Überschreiben von Saves
- **Konfigurierbar**:
  - Backup-Ordner frei wählbar
  - Maximale Anzahl oder unbegrenzte Backups
  - Älteste Backups werden automatisch gelöscht
- **Backup-Ordner öffnen**: Direkter Zugriff via Explorer

#### Benutzeroberfläche
- **Mehrsprachigkeit**: Deutsch und Englisch
- **Theme-System**: Dark Mode und Light Mode
- **Hilfe-System**:
  - About-Dialog mit Projektinformationen und Links
  - Hilfe-Dialog mit GitHub Issues-Link
  - Entwicklungsstatus-Warnung
- **Benutzerfreundlich**:
  - Leere Slots werden klar als "Unbesetzt" angezeigt
  - Gruppen-Namen statt technischer IDs (z.B. "Gruppe 1" statt "stage_1")
  - Symmetrisches UI-Layout

#### Technisches
- **Settings-Datei** (settings.ini):
  - Speichert Sprache, Theme, Backup-Einstellungen
  - Automatische Systemsprache- und Theme-Erkennung
- **Debug-Level**: Konfigurierbare Logging-Stufen
- **Entity-System**: Umfangreiche Datenbank bekannter Unit-Typen

### 🛠️ Technische Details
- **Plattform**: Windows (Win64)
- **Entwickelt mit**: Embarcadero Delphi 12.1 Community Edition
- **Lizenz**: MIT License
- **Entwicklung**: Mit Unterstützung von Claude (Anthropic)

### ⚠️ Bekannte Einschränkungen
- Nur für Dynamische Kampagnen getestet
- Einige Unit-Typen könnten noch nicht in der Entity-Datenbank sein
- Community Edition Build-Prozess erfordert IDE

### 🐛 Bekannte Probleme
- Keine bekannten kritischen Bugs

---

## Legende
- ✨ **Features**: Neue Funktionen
- 🐛 **Bugfixes**: Behobene Fehler
- 🛠️ **Technisches**: Technische Änderungen, Refactoring
- ⚠️ **Breaking Changes**: Änderungen, die Inkompatibilitäten verursachen
- 📝 **Dokumentation**: Änderungen an der Dokumentation
