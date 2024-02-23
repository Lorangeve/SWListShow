unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, Generics.Collections, SysUtils, Forms, Controls, Graphics, Dialogs,
  {StdCtrls,}{ExtCtrls,} ComCtrls, ExtCtrls, StdCtrls, Menus,
  Registry, Clipbrd;

type

  { TMyForm }

  TMyForm = class(TForm)
    ClearFilterBtn: TButton;
    FilterTextEdit: TEdit;
    ListView1: TListView;
    ListView2: TListView;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    PageControl1: TPageControl;
    Panel1: TPanel;
    PopupMenu1: TPopupMenu;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    ToggleBox1: TToggleBox;
    procedure ClearFilterBtnClick(Sender: TObject);
    procedure FilterTextEditEditingDone(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ListView1ContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: boolean);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure ToggleBox1Change(Sender: TObject);
  private

  public

  end;

var
  MyForm: TMyForm;

implementation


type
  {$ScopedEnums on}
  TRegistryRecordInfoType = (Normal, Remark);
  {$ScopedEnums off}

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
  PopupMenuContextSender: TObject;


procedure ListViewLoadRegKeyRecards(var CurListView: TListView;
  var RegRecords: specialize TList<TRegistryRecord>; CurRegRoot: string);
var
  CurListItem: TListItem;
  regrecord: TRegistryRecord;
begin

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
    end;

  end;

end;

procedure SortRegKeyRecards(SortField: string;
  var RegRecords: specialize TList<TRegistryRecord>);
begin

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
end;

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
    CurListView.Columns.Add.Caption := 'DisplayName（显示名）';
    CurListView.Columns.Add.Caption := 'Publisher（发布者）';
    CurListView.Columns.Add.Caption := 'InstallDate（安装日期）';
    CurListView.Columns.Add.Caption := 'UninstallString（卸载命令）';
    CurListView.Columns.Add.Caption := 'Reg.KeyName（子键名）';
    for j := 0 to (PageControl.Page[i].Controls[0] as TListView).Columns.Count - 1 do
      CurListView.Columns[j].Width := 200;
    CurListView.Columns[j].Width := 400;
  end;
  CurListView := nil;

  RegScans := ['HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\',
    'HKLM:\SOFTWARE\WOW6432Node\Microsoft\Windows\CurrentVersion\Uninstall\',
    'HKCU:\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\',
    'HKCU:\SOFTWARE\WOW6432Node\Microsoft\Windows\CurrentVersion\Uninstall\'];

  SetLength(isPageInit, Length(RegScans));
  { Pointer(isPageInit)^ 是因为这是一个动态数组，无法确认大小及数组首元素
    也可以使用 isPageInit[0] }
  FillChar(Pointer(isPageInit)^, Length(RegScans) * SizeOf(False), False);
  //for i := 0 to Length(isPageInit) - 1 do isPageInit[i] := False;

  for i := 0 to Length(RegScans) - 1 do
  begin
    Reg := TRegistry.Create(KEY_READ);
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
        for RegKeyName in RegKeyNames do
        begin
          CurKey := CurRegPath + RegKeyName;
          isSuccess := Reg.OpenKeyReadOnly(CurKey);

          if True then
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
        Reg.CloseKey; // 关闭注册表键
      end
      else
      begin
        CurListView.Columns[0].Caption :=
          CurListView.Columns[0].Caption + #9'- hasErrorTip';
        //CurListView.Columns[0].AutoSize := True;

        RegistryRecord.InfoType := TRegistryRecordInfoType.Remark;
        RegistryRecord.RegRootKey := CurRegRoot;
        RegistryRecord.DisplayName :=
          Format('无法打开[%s]注册表键，已跳过[%s]',
          [CurRegRoot, CurRegPath]);
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
  ColIdx: integer; isShowMsgBoxAfterCopy: boolean = False);
var
  PlainText: string;
  CurListView: TListView;
  SelListItem: TListItem;
  CurTabSheet: TTabSheet;
begin
  CurListView := ((CurMenuItem.GetParentMenu as TPopupMenu).PopupComponent as
    TListView);
  CurTabSheet := (CurListView.GetParentComponent as TTabSheet);
  SelListItem := CurListView.Selected;

  if ColIdx > SelListItem.SubItems.Count - 1 then exit;

  case ColIdx of
    3: begin
      PlainText := Format('%s:\%s', [CurTabSheet.Caption,
        SelListItem.SubItems[ColIdx]]);
    end;
    else
      PlainText := SelListItem.SubItems[ColIdx]
  end;

  // 注册表项

  //Html := '<b>Formatted</b> text';
  //ClipBoard.SetAsHtml(Html, PlainText);
  ClipBoard.AsText := PlainText;

  if isShowMsgBoxAfterCopy then
    ShowMessage(PlainText);
end;

{$R *.lfm}

{ TMyForm }

procedure TMyForm.FormCreate(Sender: TObject);
begin

  RegistryRecords := specialize TList<TRegistryRecord>.Create();

  PageLoadRegInList(PageControl1);

end;

procedure TMyForm.ListView1ContextPopup(Sender: TObject; MousePos: TPoint;
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

procedure TMyForm.MenuItem1Click(Sender: TObject);
begin

  CopyColInfoWithListViewToClipBoard(Sender as TMenuItem, 3, True);

end;

procedure TMyForm.MenuItem2Click(Sender: TObject);
begin

  CopyColInfoWithListViewToClipBoard(Sender as TMenuItem, 0, True);

end;

procedure TMyForm.MenuItem3Click(Sender: TObject);
begin

  CopyColInfoWithListViewToClipBoard(Sender as TMenuItem, 2, True);

end;

procedure TMyForm.ToggleBox1Change(Sender: TObject);
var
  filteredRegKeyRecards: specialize TList<TRegistryRecord>;
begin
  FilterTextEdit.Enabled := (Sender as TToggleBox).Checked;

  if Trim(FilterTextEdit.Text) = '' then exit;

  if not (Sender as TToggleBox).Checked then
  begin
    FilterTextEdit.Clear;
    RefreshListViewOfPageControl(PageControl1, RegistryRecords);
  end
  else
  begin
    filteredRegKeyRecards := FilterRegKeyRecards(Trim(FilterTextEdit.Text),
      RegistryRecords);
    RefreshListViewOfPageControl(PageControl1, filteredRegKeyRecards);
  end;

end;

procedure TMyForm.ClearFilterBtnClick(Sender: TObject);
var
  i: integer;
  curListView: TListView;
  curPageTabCaption: string;
begin
  FilterTextEdit.Clear;

  for i := 0 to PageControl1.PageCount - 1 do
  begin
    curListView := (PageControl1.Pages[i].Controls[0] as TListView);
    curPageTabCaption := PageControl1.Pages[i].Caption;
    curListView.Clear;
    ListViewLoadRegKeyRecards(curListView, RegistryRecords, curPageTabCaption);
  end;

end;

procedure TMyForm.FilterTextEditEditingDone(Sender: TObject);
var
  useFilter: boolean;
  filteredRegKeyRecards: specialize TList<TRegistryRecord>;
begin
  filteredRegKeyRecards := FilterRegKeyRecards(Trim((Sender as TEdit).Text),
    RegistryRecords);
  useFilter := ToggleBox1.Checked;
  if not useFilter then exit;

  FilterRegKeyRecards((Sender as TEdit).Text, RegistryRecords);
  RefreshListViewOfPageControl(PageControl1, filteredRegKeyRecards);

end;


end.
