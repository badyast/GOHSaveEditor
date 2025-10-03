unit AppSettings;

interface

uses
  System.SysUtils, System.IniFiles, System.IOUtils, Winapi.Windows;

type
  TAppSettings = class
  private
    FIniFile: TIniFile;
    FBackupFolder: string;
    FMaxBackupCount: Integer;
    FLanguage: string;
    function GetSettingsFilePath: string;
    function DetectSystemLanguage: string;
    procedure LoadSettings;
    procedure SaveSettings;
  public
    constructor Create;
    destructor Destroy; override;

    property BackupFolder: string read FBackupFolder write FBackupFolder;
    property MaxBackupCount: Integer read FMaxBackupCount write FMaxBackupCount;
    property Language: string read FLanguage write FLanguage;

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

procedure TAppSettings.LoadSettings;
var
  SettingsFileExists: Boolean;
begin
  SettingsFileExists := TFile.Exists(GetSettingsFilePath);

  // Standard-Werte
  FBackupFolder := TPath.Combine(ExtractFilePath(ParamStr(0)), 'Backups');
  FMaxBackupCount := -1; // -1 = unbegrenzt

  // Beim ersten Start: Systemsprache erkennen
  if not SettingsFileExists then
    FLanguage := DetectSystemLanguage
  else
    FLanguage := 'de'; // Fallback für vorhandene settings.ini ohne Language-Eintrag

  // Aus INI laden
  if SettingsFileExists then
  begin
    FBackupFolder := FIniFile.ReadString('Backup', 'Folder', FBackupFolder);
    FMaxBackupCount := FIniFile.ReadInteger('Backup', 'MaxCount', FMaxBackupCount);
    FLanguage := FIniFile.ReadString('General', 'Language', FLanguage);
  end;
end;

procedure TAppSettings.SaveSettings;
begin
  FIniFile.WriteString('Backup', 'Folder', FBackupFolder);
  FIniFile.WriteInteger('Backup', 'MaxCount', FMaxBackupCount);
  FIniFile.WriteString('General', 'Language', FLanguage);
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
