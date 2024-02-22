unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, Generics.Collections, SysUtils, Forms, Controls, Graphics, Dialogs,
  {StdCtrls,}{ExtCtrls,} ComCtrls, ExtCtrls, StdCtrls,
  Registry;

type

  { TMyForm }

  TMyForm = class(TForm)
    ClearFilterBtn: TButton;
    FilterTextEdit: TEdit;
    ListView1: TListView;
    ListView2: TListView;
    PageControl1: TPageControl;
    Panel1: TPanel;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    ToggleBox1: TToggleBox;
    procedure ClearFilterBtnClick(Sender: TObject);
    procedure FilterTextEditEditingDone(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ToggleBox1Change(Sender: TObject);
  private

  public

  end;

var
  MyForm: TMyForm;

implementation


type
  TRegistryRecord = record
    DisplayName: string;
    Publisher: string;
    InstallDate: string;
    UninstallString: string;
    RegRootKey: string;
    RegSubKey: string;
  end;

var
  // 将 RegistryRecords 提到全局，用来保存已提取到的注册表数据
  RegistryRecords: specialize TList<TRegistryRecord>;

// TODO: 排序
procedure SortRegKeyRecard(SortField: string;
  var RegRecords: specialize TList<TRegistryRecord>);
begin

end;

function FilterRegKeyRecard(FilterString: string;
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

procedure ListViewLoadRegKeyRecards(var CurListView: TListView;
  var RegRecords: specialize TList<TRegistryRecord>; CurRegRoot: string);
var
  CurListItem: TListItem;
  regrecord: TRegistryRecord;
begin

  for regrecord in RegRecords do
  begin
    if CurRegRoot = regrecord.RegRootKey then
    begin
      // 新建 listview
      CurListItem := CurListView.Items.Add;
      // 然后填入每行数据
      CurListItem.Caption := regrecord.DisplayName;
      CurListItem.SubItems.Add(regrecord.Publisher);
      CurListItem.SubItems.Add(regrecord.InstallDate);
      CurListItem.SubItems.Add(regrecord.UninstallString);
      CurListItem.SubItems.Add(regrecord.RegSubKey);
    end;
  end;

end;

procedure PageLoadRegInList(PageControl: TPageControl);
var
  Reg: TRegistry;
  RegKeyNames: TStringList;
  RegKeyName, CurKey: string;
  RegScans: array of string;
  isSuccess: boolean;  //后边有一个验证没做，可能会引起Bug
  isPageInit: array of boolean;

  i, j, tabidx: integer;
  CurListView: TListView;
  CurRegRoot, CurRegPath: string;

  RegistryRecord: TRegistryRecord;
begin
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
    Reg := TRegistry.Create;
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

    { 添加列，设置列宽度 }
    if not isPageInit[tabidx] then
    begin
      PageControl.Pages[tabidx].Caption := CurRegRoot;
      CurListView := (PageControl.Pages[tabidx].Controls[0] as TListView);
      isPageInit[tabidx] := True;

      //添加列
      CurListView.Columns.Add.Caption := 'DisplayName（显示名）';
      CurListView.Columns.Add.Caption := 'Publisher（发布者）';
      CurListView.Columns.Add.Caption := 'InstallDate（安装日期）';
      CurListView.Columns.Add.Caption := 'UninstallString（卸载命令）';
      CurListView.Columns.Add.Caption := 'Reg.KeyName（子键名）';
      for j := 0 to CurListView.Columns.Count - 1 do
        CurListView.Columns[j].Width := 200;
      CurListView.Columns[j].Width := 400;
    end;

    try

      { 打开指定的注册表键 }
      if True then
      begin
        Reg.GetKeyNames(RegKeyNames); // 获取注册表键的子项名称
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
              end;

              RegistryRecords.Add(RegistryRecord);
            end;

            Reg.CloseKey; // 关闭注册表键
          end;

        end;
        Reg.CloseKey; // 关闭注册表键
      end
      else
        ShowMessage('无法打开注册表键');
    finally
      Reg.Free; // 释放 TRegistry 对象
      RegKeyNames.Free; // 释放 TStringList 对象
      //RegistryRecords.Free;
    end;

    ListViewLoadRegKeyRecards(CurListView, RegistryRecords, CurRegRoot);

  end;
end;

{$R *.lfm}

{ TMyForm }

procedure TMyForm.FormCreate(Sender: TObject);
begin

  RegistryRecords := specialize TList<TRegistryRecord>.Create();

  PageLoadRegInList(PageControl1);

end;

procedure TMyForm.ToggleBox1Change(Sender: TObject);
var
  i: integer;
  curListView: TListView;
  curPageTabCaption: string;
begin
  FilterTextEdit.Clear;
  if (Sender as TToggleBox).Checked then exit;

  for i := 0 to PageControl1.PageCount - 1 do
  begin
    curListView := (PageControl1.Pages[i].Controls[0] as TListView);
    curPageTabCaption := PageControl1.Pages[i].Caption;
    curListView.Clear;
    ListViewLoadRegKeyRecards(curListView, RegistryRecords, curPageTabCaption);
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
  filteredRegKeyRecards: specialize TList<TRegistryRecord>;
  i: integer;
  curListView: TListView;
  curPageTabCaption: TCaption;
  useFilter: boolean;
begin
  filteredRegKeyRecards := FilterRegKeyRecard(Trim((Sender as TEdit).Text),
    RegistryRecords);
  useFilter := ToggleBox1.Checked;
  if not useFilter then exit;

  for i := 0 to PageControl1.PageCount - 1 do
  begin
    curListView := (PageControl1.Pages[i].Controls[0] as TListView);
    curPageTabCaption := PageControl1.Pages[i].Caption;
    curListView.Clear;
    ListViewLoadRegKeyRecards(curListView, filteredRegKeyRecards, curPageTabCaption);
  end;

end;


end.
