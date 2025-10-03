program GOHSaveEditor;

uses
  ujachLogAuto,
  ujachLogMgr,
  Vcl.Forms,
  System.SysUtils,
  MainForm in 'MainForm.pas' {FrmMain},
  ConquestSave in 'ConquestSave.pas',
  Entitys in 'Entitys.pas',
  AppSettings in 'AppSettings.pas',
  AppLanguage in 'AppLanguage.pas',
  BackupOptionsForm in 'BackupOptionsForm.pas' {FrmBackupOptions},
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  // Startup-Logging
  jachLog.LogLevel[jachLog.DefaultTopic] := TLogLevel.llAll;
  jachLog.LogInfo('=== GOH Save Editor gestartet ===');
  jachLog.LogInfo('Version: 1.0.0');
  //jachLog.LogInfo('Build-Datum: %s', [{$I %DATE%}]);
  jachLog.LogInfo('Plattform: %s',
    [{$IFDEF WIN64}'Win64'{$ELSE}'Win32'{$ENDIF}]);
  jachLog.LogInfo('Build-Modus: %s',
    [{$IFDEF DEBUG}'Debug'{$ELSE}'Release'{$ENDIF}]);

  // System-Basisinfos
  jachLog.LogInfo('OS: %s', [TOSVersion.ToString]);
  jachLog.LogInfo('Anwendungspfad: %s', [Application.ExeName]);
  jachLog.LogInfo('Arbeitsverzeichnis: %s', [GetCurrentDir]);

  try
    jachLog.LogDebug('Initialisiere Delphi Application Framework');
    Application.Initialize;
    Application.MainFormOnTaskbar := True;

    // Theme basierend auf Settings laden
    jachLog.LogDebug('Lade Theme-Einstellungen');
    if Settings.Theme = 'dark' then
    begin
      if TStyleManager.TrySetStyle('Carbon') then
        jachLog.LogInfo('Dark Theme (Carbon) geladen')
      else
        jachLog.LogWarning('Dark Theme konnte nicht geladen werden');
    end
    else
    begin
      if TStyleManager.TrySetStyle('Amethyst Kamri') then
        jachLog.LogInfo('Light Theme (Amethyst Kamri) geladen')
      else
        jachLog.LogWarning('Light Theme konnte nicht geladen werden');
    end;

    jachLog.LogDebug('Erstelle Hauptformular');
    Application.CreateForm(TFrmMain, FrmMain);
    jachLog.LogInfo('Starte Anwendungs-Message-Loop');
    Application.Run;

  except
    on E: Exception do
    begin
      jachLog.LogCritical('KRITISCHER STARTUP-FEHLER: %s', [E.Message]);
      jachLog.LogError('Startup Exception Details', E);
      raise;
    end;
  end;

  jachLog.LogInfo('=== GOH Save Editor beendet ===');

end.
