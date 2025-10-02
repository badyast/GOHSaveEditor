unit AppSettings;

interface

uses
  System.SysUtils, System.IniFiles, System.IOUtils;

type
  TAppSettings = class
  private
    FIniFile: TIniFile;
    FBackupFolder: string;
    FMaxBackupCount: Integer;
    function GetSettingsFilePath: string;
    procedure LoadSettings;
    procedure SaveSettings;
  public
    constructor Create;
    destructor Destroy; override;

    property BackupFolder: string read FBackupFolder write FBackupFolder;
    property MaxBackupCount: Integer read FMaxBackupCount write FMaxBackupCount;

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

procedure TAppSettings.LoadSettings;
begin
  // Standard-Werte
  FBackupFolder := TPath.Combine(ExtractFilePath(ParamStr(0)), 'Backups');
  FMaxBackupCount := -1; // -1 = unbegrenzt

  // Aus INI laden
  if TFile.Exists(GetSettingsFilePath) then
  begin
    FBackupFolder := FIniFile.ReadString('Backup', 'Folder', FBackupFolder);
    FMaxBackupCount := FIniFile.ReadInteger('Backup', 'MaxCount', FMaxBackupCount);
  end;
end;

procedure TAppSettings.SaveSettings;
begin
  FIniFile.WriteString('Backup', 'Folder', FBackupFolder);
  FIniFile.WriteInteger('Backup', 'MaxCount', FMaxBackupCount);
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
