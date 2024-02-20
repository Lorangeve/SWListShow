unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, Registry;

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

procedure PageLoadRegInList(PageControl: TPageControl);
var
  Reg: TRegistry;
  RegKeyNames: TStringList;
  RegKeyName, CurKey: string;
  RegScans: array of string;
  isSuccess: boolean;
  isPageInit: array of boolean;
  i, j, tabidx: integer;
  CurListView: TListView;
  CurListItem: TListItem;
  CurRegRoot, CurRegPath: string;
begin
  RegScans := ['HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\',
    'HKLM:\SOFTWARE\WOW6432Node\Microsoft\Windows\CurrentVersion\Uninstall\',
    'HKCU:\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\'];

  SetLength(isPageInit, Length(RegScans));
  for i := 0 to Length(isPageInit) - 1 do isPageInit[i] := False;

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
    end;

    if not isPageInit[tabidx] then
    begin
      PageControl.Pages[tabidx].Caption := CurRegRoot;
      CurListView := (PageControl.Pages[tabidx].Controls[0] as TListView);
      isPageInit[tabidx] := True;

      // 添加列
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
              CurListItem := CurListView.Items.Add;
              CurListItem.Caption := Reg.ReadString('DisplayName');
              CurListItem.SubItems.Add(Reg.ReadString('Publisher'));
              CurListItem.SubItems.Add(Reg.ReadString('InstallDate'));
              CurListItem.SubItems.Add(
                Reg.ReadString('UninstallString'));
              CurListItem.SubItems.Add(CurKey);
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
    end;
  end;
end;

{$R *.lfm}

{ TMyForm }

procedure TMyForm.FormCreate(Sender: TObject);
begin

  PageLoadRegInList(PageControl1);

end;


end.
