# GOH Savegame Editor

[![License](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
![Status](https://img.shields.io/badge/status-active-success.svg)
![Platform](https://img.shields.io/badge/platform-Windows-lightgrey.svg)
[![Buy Me a Coffee](https://img.shields.io/badge/☕-Buy%20me%20a%20coffee-orange.svg)](https://www.buymeacoffee.com/badyast)

**🇩🇪 Deutsche Version** | **[🇬🇧 English Version](README.en.md)**

---

Ein inoffizieller Editor für Spielstände von **Gates of Hell: Ostfront**.  
Mit diesem Tool kannst du deine gespeicherten Kampagnen bequem laden, ändern und wieder speichern.  

⚠️ **Hinweis:** Dieses Tool befindet sich in aktiver Entwicklung. Fehler oder Inkompatibilitäten können auftreten.  
Die Nutzung geschieht auf eigene Verantwortung.

---

## ✨ Funktionen

### 🎮 Squad-Management
- **Einheiten verschieben**: Verschiebe Units zwischen Squads per Drag & Drop-ähnlicher Auswahl
- **Nur-Menschen-Filter**: Filtere gezielt menschliche Einheiten beim Transfer
- **Unit-Swap**: Tausche zwei Einheiten mit einem Klick
- **Squad-Sortierung**: Sortiere deine Squads nach Namen, Gruppe oder Veteranenstufe
- **Übersichtliche Darstellung**: TreeView mit Gruppierung nach Stages (Gruppe 1-5, Spezial)

### 💾 Backup & Sicherheit
- **Automatische Backups**: Jeder überschriebene Save wird automatisch gesichert
- **Konfigurierbar**: Wähle Backup-Ordner und maximale Anzahl der Backups
- **Schneller Zugriff**: Öffne deinen Backup-Ordner direkt aus der App

### 📊 Export & Analyse
- **CSV-Export**: Exportiere alle Squads mit Einheiten-Details
- **Inventar-Ansicht**: Zeige alle Items im Savegame an
- **Unit-Informationen**: Detaillierte Anzeige von Veteranenstufe, Kills, Ausrüstung

### 🎨 Benutzeroberfläche
- **Mehrsprachigkeit**: Deutsch und Englisch
- **Dark/Light Mode**: Automatische System-Theme-Erkennung oder manuelle Auswahl
- **Expand/Collapse**: Schnelles Ein- und Ausklappen aller TreeView-Knoten
- **Hilfe-System**: Integrierte Hilfe mit Links zu GitHub Issues

### 🛠️ Weitere Features
- **Debug-Level**: Konfigurierbare Logging-Stufen für Fehlersuche
- **Settings-Verwaltung**: Alle Einstellungen werden persistent gespeichert
- **Entity-Datenbank**: Umfangreiche Datenbank bekannter Unit-Typen

---

## 🛠️ Installation
1. Lade dir die neueste Version unter [Releases](../../releases) herunter.  
2. Entpacke die Dateien in einen beliebigen Ordner.  
3. Starte `GOHSaveEditor.exe`.  

---

## 🚀 Verwendung

### Grundlegende Schritte
1. **Save laden**: Klicke auf "Save laden..." und wähle dein Savegame aus
2. **Einheiten bearbeiten**:
   - Wähle eine Unit in der Quell-TreeView (links)
   - Wähle einen Ziel-Squad in der Ziel-TreeView (rechts)
   - Klicke "Units tauschen ↔" zum Verschieben
3. **Speichern**: Klicke "Speichern als..." um deine Änderungen zu sichern
4. **Im Spiel laden**: Lade das bearbeitete Savegame in Gates of Hell

### Tipps
- Nutze **"Sortieren"** im Menü, um deine Squads zu organisieren
- Aktiviere **"Nur Menschen verschieben"** um NPCs zu ignorieren
- Verwende **"Expand/Collapse" Buttons** (+ / -) für bessere Übersicht
- Öffne **Optionen → Backup-Optionen** um dein Backup-System anzupassen
- Wechsle zu **Dark Mode** über Optionen → Design → Dunkel

---

## ☕ Unterstützung
Dieses Projekt ist ein reines **Hobby-Projekt** und wird kostenlos zur Verfügung gestellt.  
Wenn dir der Editor gefällt und du die Entwicklung unterstützen möchtest, kannst du mir freiwillig einen Kaffee spendieren:

👉 [Buy Me a Coffee](https://www.buymeacoffee.com/badyast)  

Danke für jede Unterstützung! ❤️

---

## ⚖️ Disclaimer / Haftungsausschluss
**GOH Savegame Editor** ist ein inoffizielles Fan-Projekt.  
Es wurde unabhängig entwickelt und steht in **keiner Verbindung** zu [Digitalmindsoft / Barbedwire Studios] oder dem Publisher von *Gates of Hell: Ostfront*.  

- Dieses Tool dient ausschließlich der **Bearbeitung eigener Spielstände**.  
- Es enthält **keine Originaldaten, Assets oder Code** aus dem Spiel.  
- Alle Rechte an *Gates of Hell: Ostfront* sowie an den zugehörigen Marken, Assets und Inhalten liegen ausschließlich bei deren jeweiligen Rechteinhabern.  
- Der Einsatz des Programms geschieht auf **eigene Verantwortung**. Es gibt **keine Garantie** für Funktion oder Kompatibilität.  
- Dieses Projekt verfolgt **keine kommerziellen Zwecke**.  
- Spenden sind **freiwillig** und dienen ausschließlich der Unterstützung der Entwicklung.  

---

## 📝 Changelog

Siehe [CHANGELOG.md](CHANGELOG.md) für eine detaillierte Liste aller Änderungen.

**Aktuelle Version**: 0.9.0 Beta (Erstes öffentliches Release)

---

## 🔧 Für Entwickler

Möchtest du am Projekt mitarbeiten oder es selbst kompilieren?
Siehe [BUILDING.md](BUILDING.md) für eine Anleitung zum Build-Prozess.

---

## 📜 Lizenz
Dieses Projekt steht unter der [MIT-Lizenz](LICENSE).
Das bedeutet: Du darfst den Code frei nutzen, verändern und weitergeben – solange ein Hinweis auf den Urheber enthalten bleibt.
