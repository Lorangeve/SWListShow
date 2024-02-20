unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  Registry;

type

  { TMyForm }

  TMyForm = class(TForm)
    ListView1: TListView;
    procedure FormCreate(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
  private

  public

  end;

var
  MyForm: TMyForm;

implementation

{$R *.lfm}

{ TMyForm }

procedure TMyForm.FormCreate(Sender: TObject);
var
  Reg: TRegistry;
  RegKeys: TStringList;
  RegKey, CurKey: string;
  i: integer;
  isSuccess: boolean;
  CurListItem: TListItem;
begin

  // 添加列
  ListView1.Columns.Add.Caption := 'DisplayName（显示名）';
  ListView1.Columns.Add.Caption := 'Publisher（发布者）';
  ListView1.Columns.Add.Caption := 'UninstallString（卸载命令）';
  ListView1.Columns.Add.Caption := 'InstallDate（安装日期）';
  ListView1.Columns.Add.Caption := 'Reg.KeyName（子键名）';
  ListView1.Columns[0].Width := 200;
  //ListView1.Columns[1].Width := 600;
  for i := 1 to ListView1.Columns.Count - 1 do
    ListView1.Columns[i].Width := 200;

  Reg := TRegistry.Create;
  RegKeys := TStringList.Create;
  try
    { 设置根 }
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    { 打开指定的注册表键 }
    if Reg.OpenKeyReadOnly('SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\') then
    begin
      Reg.GetKeyNames(RegKeys); // 获取注册表键的子项名称
      for RegKey in RegKeys do
      begin

        CurKey := 'SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\' + RegKey;
        isSuccess := Reg.OpenKeyReadOnly(CurKey);

        if True then
        begin
          if Reg.ValueExists('DisplayName') then
          begin
            CurListItem := ListView1.Items.Add;
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
    RegKeys.Free; // 释放 TStringList 对象
  end;
end;

procedure TMyForm.ListBox1Click(Sender: TObject);
begin

end;

end.
