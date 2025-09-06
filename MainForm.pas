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
    ChkOnlyHumans: TCheckBox;
    PageControl1: TPageControl;
    TabGeneral: TTabSheet;
    TabInventory: TTabSheet;
    MemoInfo: TMemo;
    ListViewInventory: TListView;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtnOpenClick(Sender: TObject);
    procedure BtnTransferClick(Sender: TObject);
    procedure BtnSwapClick(Sender: TObject);
    procedure BtnSaveAsClick(Sender: TObject);
    procedure BtnExportCsvClick(Sender: TObject);
    procedure TreeBaseChange(Sender: TObject; Node: TTreeNode);
    procedure TreeTargetChange(Sender: TObject; Node: TTreeNode);
    procedure ChkOnlyHumansClick(Sender: TObject);
    procedure TreeCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
      State: TCustomDrawState; var DefaultDraw: Boolean);
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
    function IsEmptySlot(const UnitId: string): Boolean;
    function IsValidUnit(const UnitId: string): Boolean;
    function IsEntityUnit(const UnitId: string): Boolean;
    procedure RestoreFocusToSquad(ATree: TTreeView; ASquadIndex: Integer);
    procedure UpdateUnitInfoPanel(const UnitId: string);
    procedure ClearInfoPanel;
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
  SaveDialog1.Filter := 'Gates of Hell Save (*.sav)|*.sav|CSV-Datei (*.csv)|*.csv';

  LblBaseInfo.Caption := 'Quelle: (keine Auswahl)';
  LblTargetInfo.Caption := 'Ziel: (keine Auswahl)';

  // CustomDraw Event Handler zuweisen
  TreeBase.OnCustomDrawItem := TreeCustomDrawItem;
  TreeTarget.OnCustomDrawItem := TreeCustomDrawItem;

  // Info-Panel initial leeren
  ClearInfoPanel;

  // Ersten Tab aktivieren
  PageControl1.ActivePageIndex := 0;
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
  ChkOnlyHumans.Enabled := AEnabled;
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

function TFrmMain.IsEmptySlot(const UnitId: string): Boolean;
begin
  Result := SameText(UnitId, '0xffffffff');
end;

function TFrmMain.IsValidUnit(const UnitId: string): Boolean;
begin
  Result := not IsEmptySlot(UnitId);
end;

function TFrmMain.IsEntityUnit(const UnitId: string): Boolean;
var
  UnitInfo: TUnitInfo;
begin
  Result := False;

  // Leere Slots sind keine Entities
  if IsEmptySlot(UnitId) then
    Exit(False);

  try
    UnitInfo := FSave.GetUnitInfo(UnitId);
    Result := SameText(UnitInfo.Kind, 'Entity');
  except
    // Bei Fehlern nehmen wir an, dass es kein Entity ist
    Result := False;
  end;
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
  SquadNode, UnitNode: TTreeNode;
  Units: TArray<string>;
  Info: TNodeInfo;
  UnitInfo: TUnitInfo;
  UnitCaption: string;
  IsEntity, IsEmpty: Boolean;
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
      IsEmpty := IsEmptySlot(Units[J]);

      if IsEmpty then
      begin
        // Leere Slots als "Leer" anzeigen
        UnitCaption := Units[J] + ' – [Leer]';
        IsEntity := False;
      end
      else
      begin
        IsEntity := IsEntityUnit(Units[J]);

        // Entity-Units bei aktivem Filter überspringen
        if ChkOnlyHumans.Checked and IsEntity then
          Continue;

        UnitCaption := Units[J];

        try
          UnitInfo := FSave.GetUnitInfo(Units[J]);
          if UnitInfo.Name <> '' then
            UnitCaption := Units[J] + ' – ' + UnitInfo.Name;
        except
          // Fallback: nur ID anzeigen
        end;
      end;

      Info := TNodeInfo.Create;
      Info.Kind := nkUnit;
      Info.SquadIndex := I;
      Info.UnitId := Units[J];
      UnitNode := ATree.Items.AddChildObject(SquadNode, UnitCaption, Info);
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
    ClearInfoPanel;
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
    if IsEmptySlot(U.UnitId) then
      S := S + Format('Squad %d, [Leer]',[U.SquadIndex])
    else
    begin
      S := S + Format('Squad %d, Unit %s',[U.SquadIndex, U.UnitId]);
      try
        Info := FSave.GetUnitInfo(U.UnitId);
        if (Info.Kind <> '') or (Info.Name <> '') then
          S := S + Format(' (%s; %s)', [Info.Kind, Info.Name]);
      except
        // ignore
      end;
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
    if IsEmptySlot(U.UnitId) then
      S := S + Format('Squad %d, [Leer]',[U.SquadIndex])
    else
    begin
      S := S + Format('Squad %d, Unit %s',[U.SquadIndex, U.UnitId]);
      try
        Info := FSave.GetUnitInfo(U.UnitId);
        if (Info.Kind <> '') or (Info.Name <> '') then
          S := S + Format(' (%s; %s)', [Info.Kind, Info.Name]);
      except
        // ignore
      end;
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

procedure TFrmMain.ClearInfoPanel;
begin
  MemoInfo.Clear;
  MemoInfo.Lines.Add('Keine Unit ausgewählt');
  ListViewInventory.Items.Clear;
end;

procedure TFrmMain.UpdateUnitInfoPanel(const UnitId: string);
var
  Details: TUnitDetails;
  Item: TListItem;
  I: Integer;
  S: string;
begin
  if IsEmptySlot(UnitId) then
  begin
    MemoInfo.Clear;
    MemoInfo.Lines.Add('Leerer Slot');
    ListViewInventory.Items.Clear;
    Exit;
  end;

  try
    Details := FSave.GetUnitDetails(UnitId);

    // Allgemeine Informationen
    MemoInfo.Clear;
    MemoInfo.Lines.Add('=== UNIT INFORMATIONEN ===');
    MemoInfo.Lines.Add('');
    MemoInfo.Lines.Add(Format('Unit ID:        %s', [Details.UnitId]));
    MemoInfo.Lines.Add(Format('Typ:            %s', [Details.Kind]));
    MemoInfo.Lines.Add(Format('Unit-Klasse:    %s', [Details.UnitType]));
    MemoInfo.Lines.Add(Format('Name:           %s', [Details.Name]));
    MemoInfo.Lines.Add('');

    if Details.Position <> '' then
      MemoInfo.Lines.Add(Format('Position:       %s', [Details.Position]));

    if Details.Veterancy > 0 then
      MemoInfo.Lines.Add(Format('Veteranenstufe: %d', [Details.Veterancy]));

    if Details.Score > 0 then
      MemoInfo.Lines.Add(Format('Score:          %.2f', [Details.Score]));

    if Details.InfantryKills > 0 then
      MemoInfo.Lines.Add(Format('Infantry Kills: %d', [Details.InfantryKills]));

    MemoInfo.Lines.Add(Format('MID:            %d', [Details.MID]));

    if (Details.NameId1 > 0) or (Details.NameId2 > 0) then
      MemoInfo.Lines.Add(Format('Name-IDs:       %d, %d', [Details.NameId1, Details.NameId2]));

    if Details.LastItem <> '' then
      MemoInfo.Lines.Add(Format('Letzte Waffe:   %s', [Details.LastItem]));

    if Details.LastThrowItem <> '' then
      MemoInfo.Lines.Add(Format('Letzte Granate: %s', [Details.LastThrowItem]));

    if Details.FsmState <> '' then
      MemoInfo.Lines.Add(Format('FSM-Status:     %s', [Details.FsmState]));

    // Inventar
    ListViewInventory.Items.Clear;
    for I := 0 to High(Details.Inventory) do
    begin
      Item := ListViewInventory.Items.Add;
      Item.Caption := Details.Inventory[I].ItemName;

      // Typ
      if Details.Inventory[I].ItemType <> '' then
        Item.SubItems.Add(Details.Inventory[I].ItemType)
      else
        Item.SubItems.Add('-');

      // Anzahl
      if Details.Inventory[I].Quantity > 1 then
        Item.SubItems.Add(IntToStr(Details.Inventory[I].Quantity))
      else
        Item.SubItems.Add('1');

      // Position
      Item.SubItems.Add('Cell ' + Details.Inventory[I].Cell);

      // Anmerkung
      if Details.Inventory[I].IsUserItem then
        Item.SubItems.Add('Ausgerüstet')
      else
        Item.SubItems.Add('');
    end;

    // Wenn kein Inventar vorhanden
    if Length(Details.Inventory) = 0 then
    begin
      Item := ListViewInventory.Items.Add;
      Item.Caption := '(Kein Inventar gefunden)';
    end;

  except
    on E: Exception do
    begin
      MemoInfo.Clear;
      MemoInfo.Lines.Add('Fehler beim Laden der Unit-Details:');
      MemoInfo.Lines.Add(E.Message);
      ListViewInventory.Items.Clear;
    end;
  end;
end;

procedure TFrmMain.TreeBaseChange(Sender: TObject; Node: TTreeNode);
var
  U: TNodeInfo;
begin
  UpdateInfoLabels;

  // Update Info-Panel wenn eine Unit ausgewählt ist
  U := SelectedUnitInfo(TreeBase);
  if Assigned(U) then
    UpdateUnitInfoPanel(U.UnitId)
  else
    ClearInfoPanel;
end;

procedure TFrmMain.TreeTargetChange(Sender: TObject; Node: TTreeNode);
var
  U: TNodeInfo;
begin
  UpdateInfoLabels;

  // Update Info-Panel wenn eine Unit ausgewählt ist (Target hat Priorität)
  U := SelectedUnitInfo(TreeTarget);
  if Assigned(U) then
    UpdateUnitInfoPanel(U.UnitId);
end;

procedure TFrmMain.ChkOnlyHumansClick(Sender: TObject);
begin
  // Bei Änderung der Checkbox die TreeViews neu laden
  if Assigned(FSave) then
  begin
    PopulateTrees;
    ClearInfoPanel;
  end;
end;

procedure TFrmMain.TreeCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
  State: TCustomDrawState; var DefaultDraw: Boolean);
var
  Info: TNodeInfo;
  Canvas: TCanvas;
begin
  DefaultDraw := True;
  Canvas := Sender.Canvas;

  if Assigned(Node.Data) then
  begin
    Info := TNodeInfo(Node.Data);
    if Info.Kind = nkUnit then
    begin
      if IsEmptySlot(Info.UnitId) then
      begin
        // Leere Slots ausgegraut und kursiv
        Canvas.Font.Color := clGray;
        Canvas.Font.Style := [fsItalic];
      end
      else
      begin
        // Normale Units
        Canvas.Font.Color := clWindowText;
        Canvas.Font.Style := [];
      end;
    end
    else
    begin
      // Squad-Knoten normal darstellen
      Canvas.Font.Color := clWindowText;
      Canvas.Font.Style := [];
    end;
  end;
end;

procedure TFrmMain.RestoreFocusToSquad(ATree: TTreeView; ASquadIndex: Integer);
var
  I: Integer;
  Node: TTreeNode;
  Info: TNodeInfo;
begin
  if (ASquadIndex < 0) or (ASquadIndex >= Length(FAllSquadNames)) then
    Exit;

  // Suche den Squad-Knoten mit dem passenden Index
  for I := 0 to ATree.Items.Count - 1 do
  begin
    Node := ATree.Items[I];
    if Assigned(Node.Data) then
    begin
      Info := TNodeInfo(Node.Data);
      if (Info.Kind = nkSquad) and (Info.SquadIndex = ASquadIndex) then
      begin
        // Fokus setzen und Squad expandieren
        ATree.Selected := Node;
        Node.Expand(False);
        Exit;
      end;
    end;
  end;
end;

procedure TFrmMain.BtnTransferClick(Sender: TObject);
begin
  Application.MessageBox('Transfer-Funktion ist deaktiviert. Verwenden Sie "Tauschen" um Units mit leeren Plätzen zu vertauschen.', 'Hinweis', MB_ICONINFORMATION);
end;

procedure TFrmMain.BtnSwapClick(Sender: TObject);
var
  A, B: TNodeInfo;
  BaseSquadIndex, TargetSquadIndex: Integer;
begin
  A := SelectedUnitInfo(TreeBase);
  B := SelectedUnitInfo(TreeTarget);
  if (not Assigned(A)) or (not Assigned(B)) then
  begin
    Application.MessageBox('Bitte links und rechts jeweils eine Unit auswählen.', 'Hinweis', MB_ICONINFORMATION);
    Exit;
  end;

  // Squad-Indizes für spätere Fokus-Wiederherstellung speichern
  BaseSquadIndex := A.SquadIndex;
  TargetSquadIndex := B.SquadIndex;

  // Prüfen ob beide Units verschoben werden dürfen (nur bei aktivem Filter)
  if ChkOnlyHumans.Checked and IsValidUnit(A.UnitId) and IsEntityUnit(A.UnitId) then
  begin
    Application.MessageBox('Die linke Unit ist eine Entity und kann bei aktivem Filter nicht verschoben werden.', 'Hinweis', MB_ICONINFORMATION);
    Exit;
  end;

  if ChkOnlyHumans.Checked and IsValidUnit(B.UnitId) and IsEntityUnit(B.UnitId) then
  begin
    Application.MessageBox('Die rechte Unit ist eine Entity und kann bei aktivem Filter nicht verschoben werden.', 'Hinweis', MB_ICONINFORMATION);
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

    // Fokus auf die vorherigen Squads wiederherstellen
    RestoreFocusToSquad(TreeBase, BaseSquadIndex);
    RestoreFocusToSquad(TreeTarget, TargetSquadIndex);

    // Info-Panel aktualisieren (falls eine Unit noch selektiert ist)
    if Assigned(SelectedUnitInfo(TreeBase)) then
      UpdateUnitInfoPanel(SelectedUnitInfo(TreeBase).UnitId)
    else if Assigned(SelectedUnitInfo(TreeTarget)) then
      UpdateUnitInfoPanel(SelectedUnitInfo(TreeTarget).UnitId)
    else
      ClearInfoPanel;

    ShowStatus('Units getauscht.');
  except
    on E: Exception do
      Application.MessageBox(PChar('Fehler beim Tauschen: ' + E.Message), 'Fehler', MB_ICONERROR);
  end;
end;

procedure TFrmMain.BtnSaveAsClick(Sender: TObject);
begin
  SaveDialog1.FilterIndex := 1; // Default auf .sav
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
  SaveDialog1.FilterIndex := 2; // CSV auswählen
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
          if IsEmptySlot(Units[J]) then
            Line := Format('%s;%s;%s;%s',[FAllSquadNames[I], Units[J], 'Empty', '[Leer]'])
          else
          begin
            Info := FSave.GetUnitInfo(Units[J]);
            Line := Format('%s;%s;%s;%s',[FAllSquadNames[I], Units[J], Info.Kind, Info.Name]);
          end;
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
