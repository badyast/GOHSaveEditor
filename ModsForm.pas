unit ModsForm;

interface

uses
  System.SysUtils, System.Math, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  AppLanguage;

type
  TFrmMods = class(TForm)
    LblInfo: TLabel;
    ListBoxMods: TListBox;
    BtnAdd: TButton;
    BtnRemove: TButton;
    PanelBottom: TPanel;
    BtnOK: TButton;
    BtnCancel: TButton;
    procedure BtnAddClick(Sender: TObject);
    procedure BtnRemoveClick(Sender: TObject);
    procedure ListBoxModsClick(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
  private
    FLang: TLanguageStrings;
  public
    procedure LoadLanguage(const Lang: TLanguageStrings);
    function GetMods: TArray<string>;
    procedure SetMods(const Mods: TArray<string>);
  end;

var
  FrmMods: TFrmMods;

implementation

{$R *.dfm}

procedure TFrmMods.LoadLanguage(const Lang: TLanguageStrings);
begin
  FLang := Lang;
  Caption := Lang.ModsCaption;
  LblInfo.Caption := Lang.ModsInfo;
  BtnAdd.Caption := Lang.ModsAdd;
  BtnRemove.Caption := Lang.ModsRemove;
  BtnOK.Caption := Lang.BtnOK;
  BtnCancel.Caption := Lang.BtnCancel;
end;

procedure TFrmMods.BtnOKClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TFrmMods.BtnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TFrmMods.BtnAddClick(Sender: TObject);
var
  WorkshopId: string;
  ModStr: string;
  ColonPos: Integer;
begin
  WorkshopId := '';
  if not InputQuery(FLang.ModsAddPromptTitle, FLang.ModsAddPromptText, WorkshopId) then
    Exit;

  WorkshopId := Trim(WorkshopId);
  if WorkshopId = '' then
    Exit;

  // Accept full format (mod_XXXXX:0) or just the numeric ID
  if WorkshopId.StartsWith('mod_') then
    ModStr := WorkshopId
  else
  begin
    ColonPos := WorkshopId.IndexOf(':');
    if ColonPos >= 0 then
      WorkshopId := WorkshopId.Substring(0, ColonPos);
    ModStr := 'mod_' + WorkshopId + ':0';
  end;

  ListBoxMods.Items.Add(ModStr);
  ListBoxMods.ItemIndex := ListBoxMods.Items.Count - 1;
  BtnRemove.Enabled := True;
end;

procedure TFrmMods.BtnRemoveClick(Sender: TObject);
var
  Idx: Integer;
begin
  Idx := ListBoxMods.ItemIndex;
  if Idx < 0 then
    Exit;
  ListBoxMods.Items.Delete(Idx);
  if ListBoxMods.Items.Count > 0 then
    ListBoxMods.ItemIndex := Min(Idx, ListBoxMods.Items.Count - 1);
  BtnRemove.Enabled := ListBoxMods.ItemIndex >= 0;
end;

procedure TFrmMods.ListBoxModsClick(Sender: TObject);
begin
  BtnRemove.Enabled := ListBoxMods.ItemIndex >= 0;
end;

function TFrmMods.GetMods: TArray<string>;
var
  I: Integer;
begin
  SetLength(Result, ListBoxMods.Items.Count);
  for I := 0 to ListBoxMods.Items.Count - 1 do
    Result[I] := ListBoxMods.Items[I];
end;

procedure TFrmMods.SetMods(const Mods: TArray<string>);
var
  ModEntry: string;
begin
  ListBoxMods.Items.Clear;
  for ModEntry in Mods do
    ListBoxMods.Items.Add(ModEntry);
  BtnRemove.Enabled := False;
end;

end.
