unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, Generics.Collections, SysUtils, Forms, Controls, Graphics, Dialogs,
  {StdCtrls,}{ExtCtrls,} ComCtrls,
  Registry;

type

  { TMyForm }

  TMyForm = class(TForm)
    ListView1: TListView;
    ListView2: TListView;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure FormCreate(Sender: TObject);
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

procedure SortRegKeyRecard(regrecard: TRegistryRecord);
begin

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

    RegistryRecords := specialize TList<TRegistryRecord>.Create();
    try

      { 打开指定的注册表键 }
      if Reg.OpenKeyReadOnly(CurRegPath) then
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

  PageLoadRegInList(PageControl1);

end;


end.
