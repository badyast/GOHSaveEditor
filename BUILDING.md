# Build-Anleitung für GOHSaveEditor

## Voraussetzungen
- Embarcadero Delphi 12.1 Community Edition (oder höher)
- Windows (getestet auf Win64)

## Build-Prozess

### Automatischer Build über PowerShell-Script
Da die Community Edition von Delphi 12.1 eingeschränkte Command-Line-Tools hat, verwenden wir ein Wrapper-Script:

```powershell
C:\scripts\build-delphi.ps1 GOHSaveEditor.dpr
```

**Parameter:**
- `-ProjectFile`: Pfad zur .dpr Datei (erforderlich)
- `-Config`: Debug oder Release (Standard: Debug)
- `-Platform`: Win32 oder Win64 (Standard: Win32)

**Beispiele:**
```powershell
# Debug-Build (Standard)
C:\scripts\build-delphi.ps1 GOHSaveEditor.dpr

# Release-Build für Win64
C:\scripts\build-delphi.ps1 GOHSaveEditor.dpr -Config Release -Platform Win64
```

### Manueller Build in der IDE
1. Öffne `GOHSaveEditor.dpr` in Delphi
2. Wähle die gewünschte Build-Konfiguration (Debug/Release)
3. Drücke F9 zum Kompilieren und Ausführen oder Strg+F9 nur zum Kompilieren

## Build-Script Details
Das `build-delphi.ps1` Script:
- Startet die Delphi IDE (`bds.exe`) im Headless Build-Modus
- Parameter: `-pDelphi -b -ns` (Build, No Splash)
- Zeigt automatisch das Error-Log an, falls Fehler auftreten
- Wartet auf Completion des Builds

## Automatische Builds nach Code-Änderungen
Bei Verwendung von Claude Code wird nach jeder Code-Änderung automatisch ein Build durchgeführt, um Kompilierfehler frühzeitig zu erkennen.

## Bekannte Compiler-Hinweise
Folgende Hinweise/Warnungen können erscheinen und sind unkritisch:
- Unbenutzte Variablen in `ConquestSave.pas` (M, StartPos, EndPos, BraceCount)
- Möglicherweise nicht initialisierte Variablen (DirSize, FileCount) - werden in try-block behandelt
- Unbenutzte private Methoden (werden für zukünftige Features vorgehalten)

## Ausgabe
Die kompilierte .exe befindet sich nach erfolgreichem Build in:
- Debug: `Win64\Debug\GOHSaveEditor.exe`
- Release: `Win64\Release\GOHSaveEditor.exe`