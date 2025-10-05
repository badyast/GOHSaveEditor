unit AppSettings;

interface

uses
  System.SysUtils, System.IniFiles, System.IOUtils, Winapi.Windows, System.Win.Registry;

type
  TAppSettings = class
  private
    FIniFile: TIniFile;
    FBackupFolder: string;
    FMaxBackupCount: Integer;
    FLanguage: string;
    FTheme: string;
    function GetSettingsFilePath: string;
    function DetectSystemLanguage: string;
    function DetectSystemTheme: string;
    procedure LoadSettings;
    procedure SaveSettings;
  public
    constructor Create;
    destructor Destroy; override;

    property BackupFolder: string read FBackupFolder write FBackupFolder;
    property MaxBackupCount: Integer read FMaxBackupCount write FMaxBackupCount;
    property Language: string read FLanguage write FLanguage;
    property Theme: string read FTheme write FTheme;

    procedure Save;
  end;

var
  Settings: TAppSettings;

implementation

{ TAppSettings }

constructor TAppSettings.Create;
begin
  inherited Create;
  FIniFile := TIniFile.Create(GetSettingsFilePath);
  LoadSettings;
end;

destructor TAppSettings.Destroy;
begin
  FIniFile.Free;
  inherited;
end;

function TAppSettings.GetSettingsFilePath: string;
begin
  Result := TPath.Combine(ExtractFilePath(ParamStr(0)), 'settings.ini');
end;

function TAppSettings.DetectSystemLanguage: string;
var
  LangID: WORD;
  PrimaryLang: WORD;
begin
  // Windows-Systemsprache abrufen
  LangID := GetUserDefaultLangID;
  PrimaryLang := LangID and $3FF; // Primäre Sprach-ID extrahieren

  // LANG_GERMAN = $07
  if PrimaryLang = $07 then
    Result := 'de'
  else
    Result := 'en'; // Standard: Englisch für alle anderen Sprachen
end;

function TAppSettings.DetectSystemTheme: string;
var
  Reg: TRegistry;
  UseLightTheme: Integer;
begin
  Result := 'light'; // Fallback: Helles Theme

  Reg := TRegistry.Create(KEY_READ);
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKeyReadOnly('Software\Microsoft\Windows\CurrentVersion\Themes\Personalize') then
    begin
      try
        // AppsUseLightTheme: 1 = Hell, 0 = Dunkel
        if Reg.ValueExists('AppsUseLightTheme') then
        begin
          UseLightTheme := Reg.ReadInteger('AppsUseLightTheme');
          if UseLightTheme = 0 then
            Result := 'dark'
          else
            Result := 'light';
        end;
      finally
        Reg.CloseKey;
      end;
    end;
  finally
    Reg.Free;
  end;
end;

procedure TAppSettings.LoadSettings;
var
  SettingsFileExists: Boolean;
begin
  SettingsFileExists := TFile.Exists(GetSettingsFilePath);

  // Standard-Werte setzen
  FBackupFolder := TPath.Combine(ExtractFilePath(ParamStr(0)), 'Backups');
  FMaxBackupCount := -1; // -1 = unbegrenzt
  FLanguage := DetectSystemLanguage;
  FTheme := DetectSystemTheme;

  // Aus INI laden (überschreibt Defaults wenn vorhanden)
  if SettingsFileExists then
  begin
    FBackupFolder := FIniFile.ReadString('Backup', 'Folder', FBackupFolder);
    FMaxBackupCount := FIniFile.ReadInteger('Backup', 'MaxCount', FMaxBackupCount);
    FLanguage := FIniFile.ReadString('General', 'Language', FLanguage);
    FTheme := FIniFile.ReadString('General', 'Theme', FTheme);
  end;
end;

procedure TAppSettings.SaveSettings;
begin
  FIniFile.WriteString('Backup', 'Folder', FBackupFolder);
  FIniFile.WriteInteger('Backup', 'MaxCount', FMaxBackupCount);
  FIniFile.WriteString('General', 'Language', FLanguage);
  FIniFile.WriteString('General', 'Theme', FTheme);
  FIniFile.UpdateFile;
end;

procedure TAppSettings.Save;
begin
  SaveSettings;
end;

initialization
  Settings := TAppSettings.Create;

finalization
  Settings.Free;

end.
