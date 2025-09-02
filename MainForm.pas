unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
  System.Generics.Collections, ConquestSave;

type
  TNodeKind = (nkSquad, nkUnit);

  TNodeInfo = class
  public
    Kind: TNodeKind;
    SquadIndex: Integer;
    UnitId: string; // nur bei nkUnit gefüllt
  end;

  TFrmMain = class(TForm)
    BtnOpen: TButton;
    LblSave: TLabel;
    TreeBase: TTreeView;
    TreeTarget: TTreeView;
    BtnTransfer: TButton;
    BtnSwap: TButton;
    BtnSaveAs: TButton;
    BtnExportCsv: TButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    LblBaseInfo: TLabel;
    LblTargetInfo: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtnOpenClick(Sender: TObject);
    procedure BtnTransferClick(Sender: TObject);
    procedure BtnSwapClick(Sender: TObject);
    procedure BtnSaveAsClick(Sender: TObject);
    procedure BtnExportCsvClick(Sender: TObject);
    procedure TreeBaseChange(Sender: TObject; Node: TTreeNode);
    procedure TreeTargetChange(Sender: TObject; Node: TTreeNode);
  private
    FSave: TConquestSave;
    FAllSquadNames: TArray<string>;
    procedure SetControlsEnabled(AEnabled: Boolean);
    procedure ClearTreeData(ATree: TTreeView);
    procedure PopulateTrees;
    procedure PopulateTree(ATree: TTreeView);
    function SelectedUnitInfo(ATree: TTreeView): TNodeInfo;
    function SelectedSquadIndex(ATree: TTreeView): Integer;
    procedure UpdateInfoLabels;
    procedure ShowStatus(const Msg: string);
  public
  end;

var
  FrmMain: TFrmMain;

implementation

{$R *.dfm}

procedure TFrmMain.FormCreate(Sender: TObject);
begin
  FSave := TConquestSave.Create;
  SetControlsEnabled(False);

  OpenDialog1.Filter := 'Gates of Hell Save (*.sav)|*.sav';
  SaveDialog1.Filter := 'Gates of Hell Save (*.sav)|*.sav';

  LblBaseInfo.Caption := 'Quelle: (keine Auswahl)';
  LblTargetInfo.Caption := 'Ziel: (keine Auswahl)';
end;

procedure TFrmMain.FormDestroy(Sender: TObject);
begin
  ClearTreeData(TreeBase);
  ClearTreeData(TreeTarget);
  FSave.Free;
end;

procedure TFrmMain.SetControlsEnabled(AEnabled: Boolean);
begin
  TreeBase.Enabled := AEnabled;
  TreeTarget.Enabled := AEnabled;
  BtnTransfer.Enabled := AEnabled;
  BtnSwap.Enabled := AEnabled;
  BtnSaveAs.Enabled := AEnabled;
  BtnExportCsv.Enabled := AEnabled;
end;

procedure TFrmMain.ClearTreeData(ATree: TTreeView);
var
  I: Integer;
  N: TTreeNode;
begin
  // Alle TNodeInfo freigeben
  for I := 0 to ATree.Items.Count - 1 do
  begin
    N := ATree.Items[I];
    if Assigned(N.Data) then
    begin
      TObject(N.Data).Free;
      N.Data := nil;
    end;
  end;
  ATree.Items.Clear;
end;

procedure TFrmMain.PopulateTrees;
begin
  ClearTreeData(TreeBase);
  ClearTreeData(TreeTarget);
  FAllSquadNames := FSave.GetSquadNames;
  PopulateTree(TreeBase);
  PopulateTree(TreeTarget);
  if TreeBase.Items.Count > 0 then
    TreeBase.Items[0].Expand(True);
  if TreeTarget.Items.Count > 0 then
    TreeTarget.Items[0].Expand(True);
end;

procedure TFrmMain.PopulateTree(ATree: TTreeView);
var
  I, J: Integer;
  SquadNode: TTreeNode;
  Units: TArray<string>;
  Info: TNodeInfo;
  UnitInfo: TUnitInfo;
  UnitCaption: string;
begin
  for I := 0 to Length(FAllSquadNames) - 1 do
  begin
    Info := TNodeInfo.Create;
    Info.Kind := nkSquad;
    Info.SquadIndex := I;
    Info.UnitId := '';
    SquadNode := ATree.Items.AddObject(nil, FAllSquadNames[I], Info);

    Units := FSave.GetSquadMembers(I);
    for J := 0 to Length(Units) - 1 do
    begin
      UnitCaption := Units[J];
      try
        UnitInfo := FSave.GetUnitInfo(Units[J]);
        if UnitInfo.Name <> '' then
          UnitCaption := Units[J] + ' – ' + UnitInfo.Name;
      except
        // Fallback: nur ID anzeigen
      end;

      Info := TNodeInfo.Create;
      Info.Kind := nkUnit;
      Info.SquadIndex := I;
      Info.UnitId := Units[J];
      ATree.Items.AddChildObject(SquadNode, UnitCaption, Info);
    end;
  end;
end;

function TFrmMain.SelectedUnitInfo(ATree: TTreeView): TNodeInfo;
begin
  Result := nil;
  if Assigned(ATree.Selected) and Assigned(ATree.Selected.Data) then
    if TNodeInfo(ATree.Selected.Data).Kind = nkUnit then
      Result := TNodeInfo(ATree.Selected.Data);
end;

function TFrmMain.SelectedSquadIndex(ATree: TTreeView): Integer;
var
  N: TTreeNode;
  Info: TNodeInfo;
begin
  Result := -1;
  N := ATree.Selected;
  if not Assigned(N) then Exit;

  Info := TNodeInfo(N.Data);
  if not Assigned(Info) then Exit;

  // Egal ob Squad- oder Unit-Knoten: SquadIndex steckt drin
  Result := Info.SquadIndex;
end;

procedure TFrmMain.BtnOpenClick(Sender: TObject);
begin
  if not OpenDialog1.Execute then
    Exit;
  try
    FSave.LoadFromSave(OpenDialog1.FileName);
    LblSave.Caption := ExtractFileName(OpenDialog1.FileName);
    PopulateTrees;
    SetControlsEnabled(True);
    ShowStatus('Save geladen.');
  except
    on E: Exception do
    begin
      SetControlsEnabled(False);
      Application.MessageBox(PChar('Fehler beim Laden: ' + E.Message), 'Fehler', MB_ICONERROR);
    end;
  end;
end;

procedure TFrmMain.UpdateInfoLabels;
var
  U: TNodeInfo;
  I: Integer;
  Info: TUnitInfo;
  S: string;
begin
  // Base
  S := 'Quelle: ';
  U := SelectedUnitInfo(TreeBase);
  if Assigned(U) then
  begin
    S := S + Format('Squad %d, Unit %s',[U.SquadIndex, U.UnitId]);
    try
      Info := FSave.GetUnitInfo(U.UnitId);
      if (Info.Kind <> '') or (Info.Name <> '') then
        S := S + Format(' (%s; %s)', [Info.Kind, Info.Name]);
    except
      // ignore
    end;
  end
  else
    S := S + '(keine Auswahl)';
  LblBaseInfo.Caption := S;

  // Target
  S := 'Ziel: ';
  U := SelectedUnitInfo(TreeTarget);
  if Assigned(U) then
  begin
    S := S + Format('Squad %d, Unit %s',[U.SquadIndex, U.UnitId]);
    try
      Info := FSave.GetUnitInfo(U.UnitId);
      if (Info.Kind <> '') or (Info.Name <> '') then
        S := S + Format(' (%s; %s)', [Info.Kind, Info.Name]);
    except
      // ignore
    end;
  end
  else
  begin
    I := SelectedSquadIndex(TreeTarget);
    if I >= 0 then
      S := S + Format('Squad %d', [I])
    else
      S := S + '(keine Auswahl)';
  end;
  LblTargetInfo.Caption := S;
end;

procedure TFrmMain.TreeBaseChange(Sender: TObject; Node: TTreeNode);
begin
  UpdateInfoLabels;
end;

procedure TFrmMain.TreeTargetChange(Sender: TObject; Node: TTreeNode);
begin
  UpdateInfoLabels;
end;

procedure TFrmMain.BtnTransferClick(Sender: TObject);
var
  BaseU: TNodeInfo;
  TargetSquad: Integer;
begin
  BaseU := SelectedUnitInfo(TreeBase);
  if not Assigned(BaseU) then
  begin
    Application.MessageBox('Bitte links eine Unit auswählen.', 'Hinweis', MB_ICONINFORMATION);
    Exit;
  end;

  TargetSquad := SelectedSquadIndex(TreeTarget);
  if TargetSquad < 0 then
  begin
    // Wenn rechts eine Unit ausgewählt ist, deren Squad nehmen
    if Assigned(SelectedUnitInfo(TreeTarget)) then
      TargetSquad := SelectedUnitInfo(TreeTarget).SquadIndex;
  end;

  if TargetSquad < 0 then
  begin
    Application.MessageBox('Bitte rechts ein Ziel-Squad (oder eine Unit desselben) auswählen.', 'Hinweis', MB_ICONINFORMATION);
    Exit;
  end;

  if TargetSquad = BaseU.SquadIndex then
  begin
    Application.MessageBox('Quelle und Ziel sind identisch.', 'Hinweis', MB_ICONINFORMATION);
    Exit;
  end;

  try
    FSave.MoveUnit(BaseU.SquadIndex, BaseU.UnitId, TargetSquad);
    PopulateTrees;
    ShowStatus('Unit übertragen.');
  except
    on E: Exception do
      Application.MessageBox(PChar('Fehler beim Übertragen: ' + E.Message), 'Fehler', MB_ICONERROR);
  end;
end;

procedure TFrmMain.BtnSwapClick(Sender: TObject);
var
  A, B: TNodeInfo;
begin
  A := SelectedUnitInfo(TreeBase);
  B := SelectedUnitInfo(TreeTarget);
  if (not Assigned(A)) or (not Assigned(B)) then
  begin
    Application.MessageBox('Bitte links und rechts jeweils eine Unit auswählen.', 'Hinweis', MB_ICONINFORMATION);
    Exit;
  end;

  if (A.SquadIndex = B.SquadIndex) and (SameText(A.UnitId, B.UnitId)) then
  begin
    Application.MessageBox('Die gleiche Unit kann nicht mit sich selbst getauscht werden.', 'Hinweis', MB_ICONINFORMATION);
    Exit;
  end;

  try
    FSave.ExchangeUnits(A.SquadIndex, A.UnitId, B.SquadIndex, B.UnitId);
    PopulateTrees;
    ShowStatus('Units getauscht.');
  except
    on E: Exception do
      Application.MessageBox(PChar('Fehler beim Tauschen: ' + E.Message), 'Fehler', MB_ICONERROR);
  end;
end;

procedure TFrmMain.BtnSaveAsClick(Sender: TObject);
begin
  if not SaveDialog1.Execute then Exit;
  try
    FSave.SaveToSaveAs(SaveDialog1.FileName);
    ShowStatus('Gespeichert: ' + ExtractFileName(SaveDialog1.FileName));
  except
    on E: Exception do
      Application.MessageBox(PChar('Fehler beim Speichern: ' + E.Message), 'Fehler', MB_ICONERROR);
  end;
end;

procedure TFrmMain.BtnExportCsvClick(Sender: TObject);
var
  Csv: TStringList;
  I, J: Integer;
  Units: TArray<string>;
  Info: TUnitInfo;
  Line: string;
begin
  if not SaveDialog1.Execute then Exit;
  Csv := TStringList.Create;
  try
    Csv.Add('Squad;UnitId;Type;Name');
    FAllSquadNames := FSave.GetSquadNames;
    for I := 0 to Length(FAllSquadNames) - 1 do
    begin
      Units := FSave.GetSquadMembers(I);
      for J := 0 to Length(Units) - 1 do
      begin
        try
          Info := FSave.GetUnitInfo(Units[J]);
          Line := Format('%s;%s;%s;%s',[FAllSquadNames[I], Units[J], Info.Kind, Info.Name]);
        except
          on E: Exception do
            Line := Format('%s;%s;%s;%s',[FAllSquadNames[I], Units[J], '?', '']);
        end;
        Csv.Add(Line);
      end;
    end;
    Csv.SaveToFile(SaveDialog1.FileName, TEncoding.UTF8);
    ShowStatus('CSV exportiert: ' + ExtractFileName(SaveDialog1.FileName));
  finally
    Csv.Free;
  end;
end;

procedure TFrmMain.ShowStatus(const Msg: string);
begin
  Caption := 'GOH Savegame Editor – ' + Msg;
end;

end.
