program GOHSaveEditor;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {FrmMain},
  ConquestSave in 'ConquestSave.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;
end.
