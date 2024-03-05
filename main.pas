unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, Generics.Collections, SysUtils, Forms, Controls, Graphics,
  Dialogs, ComCtrls, ExtCtrls, StdCtrls, Menus,
  Registry, fpjsondataset, DB, SQLDB, Clipbrd, LCLType
  {$IfDef I18N},DefaultTranslator{$EndIf}
  {$ifdef I18N},shfolder{$EndIf};

type

  { TMyForm }

  TMyForm = class(TForm)
    btnClearFilter: TButton;
    btnExport: TButton;
    edtFilterText: TEdit;
    ListView1: TListView;
    ListView2: TListView;
    pupCopyPublisher: TMenuItem;
    pupCopyRegKey: TMenuItem;
    pupCopyDisplayName: TMenuItem;
    pupCopyUninstallString: TMenuItem;
    PageControl1: TPageControl;
    Panel1: TPanel;
    ListViewPopupMenu: TPopupMenu;
    dlgSaveExport: TSaveDialog;
    StatusBar: TStatusBar;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    tglUseFilter: TToggleBox;
    StatusbarInfoRefreshTimer: TTimer;
    procedure btnClearFilterClick(Sender: TObject);
    procedure btnExportClick(Sender: TObject);
    procedure edtFilterTextKeyPress(Sender: TObject; var Key: char);
    procedure ListViewClick(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure PageControl1Exit(Sender: TObject);
    procedure tglUseFilterChange(Sender: TObject);
    procedure edtFilterTextChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ListViewContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: boolean);
    procedure pupCopyPublisherClick(Sender: TObject);
    procedure pupCopyRegKeyClick(Sender: TObject);
    procedure pupCopyDisplayNameClick(Sender: TObject);
    procedure pupCopyUninstallStringClick(Sender: TObject);
    procedure StatusbarInfoRefreshTimerTimer(Sender: TObject);
  private
    procedure RefreshStatusBar(cmdMsg: string = '');
  public

  end;

var
  MyForm: TMyForm;

implementation

resourcestring
  ListViewColumnsCap_1 = '显示名';
  ListViewColumnsCap_2 = '发布者';
  ListViewColumnsCap_3 = '安装日期';
  ListViewColumnsCap_4 = '卸载命令';
  ListViewColumnsCap_5 = '子键名';
  SaveExportFileName = '软件安装列表';

type
  {$ScopedEnums on}
  TRegistryRecordInfoType = (Normal, Remark);
  {$ScopedEnums off}

  TRegistryRecordEnum = (DisplayName, Publisher, InstallDate, UninstallString, RegKey);

  TRegistryRecord = record
    DisplayName: string;
    Publisher: string;
    InstallDate: string;
    UninstallString: string;
    RegRootKey: string;
    RegSubKey: string;
    InfoType: TRegistryRecordInfoType;
  end;

var
  // 将 RegistryRecords 提到全局，用来保存已提取到的注册表数据
  RegistryRecords: specialize TList<TRegistryRecord>;

procedure ListViewLoadRegKeyRecards(var CurListView: TListView;
  var RegRecords: specialize TList<TRegistryRecord>; CurRegRoot: string);
var
  CurListItem: TListItem;
  regrecord: TRegistryRecord;
begin

  CurListView.Items.BeginUpdate;  //开始批量更新

  for regrecord in RegRecords do
  begin
    if CurRegRoot <> regrecord.RegRootKey then Continue;

    case regrecord.InfoType of
      TRegistryRecordInfoType.Normal: begin
        // 新建 listview
        CurListItem := CurListView.Items.Add;
        // 然后填入每行数据
        CurListItem.Caption := regrecord.DisplayName;
        CurListItem.SubItems.Add(regrecord.Publisher);
        CurListItem.SubItems.Add(regrecord.InstallDate);
        CurListItem.SubItems.Add(regrecord.UninstallString);
        CurListItem.SubItems.Add(regrecord.RegSubKey);
      end;
      TRegistryRecordInfoType.Remark: begin
        CurListItem := CurListView.Items.Add;
        CurListItem.Caption := regrecord.DisplayName;
      end;
      else
    end;

  end;

  CurListView.Items.EndUpdate;  //结束批量更新

end;

function FilterRegKeyRecards(FilterString: string;
  var RegRecords: specialize TList<TRegistryRecord>): specialize TList<TRegistryRecord>;
var
  regrecord: TRegistryRecord;
  newRegRecords: specialize TList<TRegistryRecord>;
begin
  if FilterString = '' then exit(RegRecords);

  newRegRecords := specialize TList<TRegistryRecord>.Create;

  for regrecord in RegRecords do
  begin
    if LowerCase(regrecord.DisplayName).Contains(LowerCase(FilterString)) or
      LowerCase(regrecord.Publisher).Contains(LowerCase(FilterString)) then
      newRegRecords.Add(regrecord);
  end;

  Result := newRegRecords;

end;

procedure RefreshListViewOfPageControl(PageControl: TPageControl;
  FilteredRegKeyRecards: specialize TList<TRegistryRecord>);
var
  i: integer;
  curListView: TListView;
  curPageTabCaption: TCaption;
begin
  for i := 0 to PageControl.PageCount - 1 do
  begin
    curListView := (PageControl.Pages[i].Controls[0] as TListView);
    curPageTabCaption := PageControl.Pages[i].Caption;
    curListView.Clear;
    ListViewLoadRegKeyRecards(curListView, FilteredRegKeyRecards, curPageTabCaption);
  end;
  MyForm.RefreshStatusBar;
end;

{ Page 初始化 }
procedure PageLoadRegInList(PageControl: TPageControl);
var
  Reg: TRegistry;
  RegKeyNames: TStringList;
  RegKeyName, CurKey: string;
  RegScans: array of string;
  isSuccess: boolean;  //Note -c重要: 后边有一个验证没做，可能会引起Bug
  isPageInit: array of boolean;

  i, j, tabidx: integer;
  CurListView: TListView;
  CurRegRoot, CurRegPath: string;

  RegistryRecord: TRegistryRecord;
begin

  for i := 0 to PageControl.PageCount - 1 do
  begin
    CurListView := PageControl.Page[i].Controls[0] as TListView;
    //添加列
    CurListView.Columns.Add.Caption := ListViewColumnsCap_1;
    CurListView.Columns.Add.Caption := ListViewColumnsCap_2;
    CurListView.Columns.Add.Caption := ListViewColumnsCap_3;
    CurListView.Columns.Add.Caption := ListViewColumnsCap_4;
    CurListView.Columns.Add.Caption := ListViewColumnsCap_5;
    for j := 0 to (PageControl.Page[i].Controls[0] as TListView).Columns.Count - 1 do
    begin
      with CurListView.Columns[j] do
      begin
        Width := 200;
      end;
    end;
    CurListView.Columns[j].Width := 450;
  end;

  CurListView := nil;

  RegScans := ['HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\',
    'HKLM:\SOFTWARE\WOW6432Node\Microsoft\Windows\CurrentVersion\Uninstall\',
    'HKCU:\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\'{,
    'HKCU:\SOFTWARE\WOW6432Node\Microsoft\Windows\CurrentVersion\Uninstall\'}];

  SetLength(isPageInit, Length(RegScans));
  { Pointer(isPageInit)^ 是因为这是一个动态数组，无法确认大小及数组首元素
    也可以使用 isPageInit[0] }
  FillChar(Pointer(isPageInit)^, Length(RegScans) * SizeOf(False), False);
  //for i := 0 to Length(isPageInit) - 1 do isPageInit[i] := False;


  for i := 0 to Length(RegScans) - 1 do
  begin
    Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ);
    //Reg.Access:=KEY_READ or KEY_WOW64_64KEY;

    RegKeyNames := TStringList.Create;

    CurRegRoot := RegScans[i].Split(':')[0];
    CurRegPath := RegScans[i].Split(':')[1].Substring(1);

    { 设置根 }
    case CurRegRoot of
      'HKLM': begin
        Reg.RootKey := HKEY_LOCAL_MACHINE;
        tabidx := 0;
      end;
      'HKCU': begin
        Reg.RootKey := HKEY_CURRENT_USER;
        tabidx := 1;
      end;
      else // 什么也不做
    end;

    CurListView := (PageControl.Pages[tabidx].Controls[0] as TListView);

    { 添加列，设置列宽度 }
    if not isPageInit[tabidx] then
    begin
      PageControl.Pages[tabidx].Caption := CurRegRoot;

      isPageInit[tabidx] := True;
    end;

    try
      { 打开指定的注册表键 }
      if Reg.OpenKeyReadOnly(CurRegPath) then
      begin

        Reg.GetKeyNames(RegKeyNames); //获取注册表键的子项名称
        Reg.CloseKey;

        for RegKeyName in RegKeyNames do
        begin
          CurKey := CurRegPath + RegKeyName;
          isSuccess := Reg.OpenKeyReadOnly(CurKey);

          if isSuccess or True then
          begin
            if Reg.ValueExists('DisplayName') then
            begin

              with RegistryRecord do
              begin
                DisplayName := Reg.ReadString('DisplayName');
                Publisher := Reg.ReadString('Publisher');
                InstallDate := Reg.ReadString('InstallDate');
                UninstallString := Reg.ReadString('UninstallString');
                RegRootKey := CurRegRoot; //pagetab_caption
                RegSubKey := CurKey;
                InfoType := TRegistryRecordInfoType.Normal;
              end;

              RegistryRecords.Add(RegistryRecord);
            end;

            Reg.CloseKey; // 关闭注册表键
          end;

        end;
      end
      else
      begin
        CurListView.Columns[0].Caption :=
          CurListView.Columns[0].Caption + #9'- hasErrorTip';
        //CurListView.Columns[0].AutoSize := True;

        with RegistryRecord do
        begin
          DisplayName := Format('无法打开[%s]注册表键', [CurRegRoot]);
          Publisher := '';
          InstallDate := '';
          UninstallString := '';
          RegRootKey := ''; //pagetab_caption
          RegSubKey := CurRegPath;
          InfoType := TRegistryRecordInfoType.Remark;
        end;

        RegistryRecords.Add(RegistryRecord);
        //ShowMessage(Format('无法打开[%s]注册表键，已跳过', [CurRegRoot]));
      end;

    finally
      Reg.Free;
      RegKeyNames.Free;
      //RegistryRecords.Free;
    end;
  end;

  RefreshListViewOfPageControl(PageControl, RegistryRecords);

end;

procedure CopyColInfoWithListViewToClipBoard(CurMenuItem: TMenuItem;
  SelColType: TRegistryRecordEnum; isShowMsgBoxAfterCopy: boolean = False);
var
  PlainText: string = string.Empty;
  CurListView: TListView;
  SelListItem: TListItem;
  CurTabSheet: TTabSheet;
begin
  CurListView := ((CurMenuItem.GetParentMenu as TPopupMenu).PopupComponent as
    TListView);
  CurTabSheet := (CurListView.GetParentComponent as TTabSheet);
  SelListItem := CurListView.Selected;

  case SelColType of
    TRegistryRecordEnum.DisplayName: PlainText := SelListItem.Caption;
    TRegistryRecordEnum.Publisher: PlainText := SelListItem.SubItems[0];
    TRegistryRecordEnum.UninstallString: PlainText := SelListItem.SubItems[2];
    TRegistryRecordEnum.RegKey: PlainText :=
        Format('%s\%s', [CurTabSheet.Caption, SelListItem.SubItems[3]]);
    else;
  end;

  //Html := '<b>Formatted</b> text';
  //ClipBoard.SetAsHtml(Html, PlainText);
  if not PlainText.IsEmpty then
    ClipBoard.AsText := PlainText;

  if isShowMsgBoxAfterCopy then
    ShowMessage(PlainText);
end;

function StdCSVFormat(aCSVItem: string): string;
begin
  Result := aCSVItem;

  if Pos('"', aCSVItem) > 0 then Result := Format('"%s"', [aCSVItem.Replace('"', '""')])
  else if Pos(',', aCSVItem) > 0 then Result := Format('"%s"', [aCSVItem]);
end;

procedure WriteCSVFile(FileName: string);
var
  fs: TFileStream;
  keyrec: TRegistryRecord;
  slbuf: TStringList;
  i: integer;
begin

  slbuf := TStringList.Create;
  slbuf.Add(Format('%s,%s,%s,%s,%s', [ListViewColumnsCap_1,
    ListViewColumnsCap_2, ListViewColumnsCap_3, ListViewColumnsCap_4,
    ListViewColumnsCap_5]));

  for keyrec in RegistryRecords do
  begin
    slbuf.Add(Format('%s,%s,%s,%s,%s', [StdCSVFormat(keyrec.DisplayName),
      StdCSVFormat(keyrec.Publisher), StdCSVFormat(keyrec.InstallDate),
      StdCSVFormat(keyrec.UninstallString), StdCSVFormat(keyrec.RegRootKey +
      '\' + keyrec.RegSubKey)]));
  end;

  fs := TFileStream.Create(FileName, fmCreate or fmShareDenyWrite);
  try
    slbuf.SaveToStream(fs, TEncoding.GetEncoding('GBK'));
  finally
    fs.Free;
  end;

  if FileExists(FileName) then
    ShowMessage('已保存')
  else
    ShowMessage('保存未成功，请重试');
end;

{$R *.lfm}

{ TMyForm }

procedure TMyForm.RefreshStatusBar(cmdMsg: string);
type
  TCmdStruct = record
    cmd: string;
    bool: boolean;
  end;
var
  totalItemsCount, curPageItemsCount, curPageIdx: integer;
  curKeyRootShot: string;
  curSelListItem: string = string.Empty;
  curSelListItemCount: integer = 0;
  cmdArray: array of string;
  cmd: string;
  cmdStruct: TCmdStruct;
begin

  curPageIdx := PageControl1.ActivePage.PageIndex;
  if (curPageIdx = 0) and (ListView1.SelCount > 0) then
  begin
    curSelListItem := string.Join(',', ListView1.Selected.SubItems.ToStringArray);
    curSelListItemCount := ListView1.SelCount;
  end
  else if (curPageIdx = 1) and (ListView2.SelCount > 0) then
  begin
    curSelListItem := string.Join(',', ListView2.Selected.SubItems.ToStringArray);
    curSelListItemCount := ListView2.SelCount;
  end;

  cmdArray := cmdMsg.Split([';']);
  with cmdStruct do
  begin
    cmd := string.Empty;
    bool := True;
  end;

  for cmd in cmdArray do
  begin
    if cmd.StartsWith('!') then
    begin
      cmdStruct.cmd := cmd.Substring(1);
      cmdStruct.bool := False;
    end
    else
      cmdStruct.cmd := cmd;

    if cmdStruct.cmd.StartsWith('clear') then
    begin
      curSelListItem := string.Empty;
    end;
    if cmdStruct.cmd.StartsWith('setinfo') then
      curSelListItem := cmdStruct.cmd.Split('|')[1];

  end;


  curKeyRootShot := PageControl1.ActivePage.Caption;
  totalItemsCount := ListView1.Items.Count + ListView2.Items.Count;
  case curKeyRootShot of
    'HKLM': curPageItemsCount := ListView1.Items.Count;
    'HKCU': curPageItemsCount := ListView2.Items.Count;
  end;

  with StatusBar do
  begin
    Panels[0].Text :=
      Format('[%s: %d/Total: %d]', [curKeyRootShot, curPageItemsCount, TotalItemsCount]);
    Panels[1].Text := Format('Selected: %d', [curSelListItemCount]);
    Panels[2].Text := Format('%s', [curSelListItem]);
  end;

end;

procedure TMyForm.FormCreate(Sender: TObject);
begin

  RegistryRecords := specialize TList<TRegistryRecord>.Create();

  PageLoadRegInList(PageControl1);

end;

procedure TMyForm.ListViewContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: boolean);
var
  this: TListView;
begin

  this := (Sender as TListView);
  //PopupMenuContextSender := Sender;
  this.PopupMenu.PopupComponent := Sender as TListView;

  if this.SelCount = 0 then
    this.PopupMenu.AutoPopup := False
  else
    this.PopupMenu.AutoPopup := True;

end;

procedure TMyForm.pupCopyPublisherClick(Sender: TObject);
begin
  CopyColInfoWithListViewToClipBoard(Sender as TMenuItem,
    TRegistryRecordEnum.Publisher, True);
end;

procedure TMyForm.pupCopyRegKeyClick(Sender: TObject);
begin

  CopyColInfoWithListViewToClipBoard(Sender as TMenuItem,
    TRegistryRecordEnum.RegKey, True);

end;

procedure TMyForm.pupCopyDisplayNameClick(Sender: TObject);
begin

  CopyColInfoWithListViewToClipBoard(Sender as TMenuItem,
    TRegistryRecordEnum.DisplayName, True);

end;

procedure TMyForm.pupCopyUninstallStringClick(Sender: TObject);
begin

  CopyColInfoWithListViewToClipBoard(Sender as TMenuItem,
    TRegistryRecordEnum.UninstallString, True);

end;

procedure TMyForm.StatusbarInfoRefreshTimerTimer(Sender: TObject);
begin
  RefreshStatusBar('clear');
  StatusbarInfoRefreshTimer.Enabled := False;
end;

procedure TMyForm.edtFilterTextChange(Sender: TObject);
var
  useFilter: boolean;
  filteredRegKeyRecards: specialize TList<TRegistryRecord>;
begin

  filteredRegKeyRecards := FilterRegKeyRecards(Trim((Sender as TEdit).Text),
    RegistryRecords);
  useFilter := tglUseFilter.Checked;
  if not useFilter then exit;

  FilterRegKeyRecards((Sender as TEdit).Text, RegistryRecords);
  RefreshListViewOfPageControl(PageControl1, filteredRegKeyRecards);

end;

procedure TMyForm.tglUseFilterChange(Sender: TObject);
var
  filteredRegKeyRecards: specialize TList<TRegistryRecord>;
  tglChecked: boolean;
begin
  tglChecked := (Sender as TToggleBox).Checked;

  edtFilterText.Enabled := tglChecked;
  btnClearFilter.Enabled := tglChecked;

  if string(Trim(edtFilterText.Text)).IsEmpty then exit;

  if not tglChecked then
  begin
    edtFilterText.Clear;
    RefreshListViewOfPageControl(PageControl1, RegistryRecords);
  end
  else
  begin
    edtFilterText.SetFocus;
    filteredRegKeyRecards := FilterRegKeyRecards(Trim(edtFilterText.Text),
      RegistryRecords);
    RefreshListViewOfPageControl(PageControl1, filteredRegKeyRecards);
  end;
end;

procedure TMyForm.btnClearFilterClick(Sender: TObject);
var
  i: integer;
  curListView: TListView;
  curPageTabCaption: string;
begin

  if string(edtFilterText.Text).IsEmpty then exit;

  edtFilterText.Clear;

  RefreshListViewOfPageControl(PageControl1, RegistryRecords);

  edtFilterText.SetFocus;
end;

procedure TMyForm.btnExportClick(Sender: TObject);
var
  DesktopPath: array[0..MAX_PATH] of widechar;
  {$IfNDef I18N}
  reg: TRegistry;
  {$endif}
begin
  {$ifdef I18N}
  SHGetFolderPathW(0, CSIDL_DESKTOPDIRECTORY, 0, 0, DesktopPath);
  {$else}
  reg := TRegistry.Create;
  reg.RootKey := HKEY_CURRENT_USER;
  reg.OpenKeyReadOnly(
    'Software\Microsoft\Windows\CurrentVersion\Explorer\User Shell Folders');
  DesktopPath := reg.ReadString('Desktop');
  {$endif}
  dlgSaveExport.InitialDir := DesktopPath;
  dlgSaveExport.FileName := SaveExportFileName;
  if dlgSaveExport.Execute then
  begin
    WriteCSVFile(dlgSaveExport.FileName);
  end;
  RefreshStatusBar(Format('setinfo|save_to %s', [dlgSaveExport.FileName]));
  StatusbarInfoRefreshTimer.Enabled := True;
end;

procedure TMyForm.edtFilterTextKeyPress(Sender: TObject; var Key: char);
begin
  case Key of
    chr(VK_ESCAPE): (Sender as TEdit).Clear;
    chr(VK_RETURN): if (Sender as TEdit).SelLength > 0 then (Sender as TEdit).Clear;
    else;
  end;
end;


procedure TMyForm.ListViewClick(Sender: TObject);
begin
  RefreshStatusBar;
end;

procedure TMyForm.PageControl1Change(Sender: TObject);
begin

  RefreshStatusBar('clear');

end;

procedure TMyForm.PageControl1Exit(Sender: TObject);
begin
  StatusbarInfoRefreshTimer.Enabled := True;
end;


end.
