unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Generics.Collections,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  DelphiRegistry,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.ListBox
  ;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Panel1: TPanel;
    Button1: TButton;
    ComboBox1: TComboBox;
    Button2: TButton;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    P4DFolder: String;
    DelphiPrettyName: TStringList;
    DelphiList: TStringList;
    SelectedDelphiVersion: TDelphiVersion;
    procedure RemovePackagePaths(const PackagePath: String);
    procedure RemovePath(var SomePaths: String; const PathToRemove: String; const Exact: Boolean = False);
    procedure AddPackagePaths(const PackagePath: String);
    procedure AddPath(var SomePaths: String; const PathToAdd: String);
    procedure MakeDelphiPrettyNames;
    procedure GetDelphiVersions;
    procedure ShowSelectedDelphi(const Clear: Boolean = True);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  System.IOUtils;

const
  P4DLW = 'Lightweight-Python-Wrappers';
  P4DDS = 'P4D-Data-Sciences';
  P4DPE = 'PythonEnviroments';
  P4DPP = 'PythonPackages4Delphi';
  P4DPD = 'python4delphi';

{$R *.fmx}

procedure TForm1.RemovePath(var SomePaths: String; const PathToRemove: String; const Exact: Boolean = False);
var
  Dirs: TStringList;
  I: Integer;
begin
  Dirs := TStringList.Create;
  SplitPaths(SomePaths, Dirs);

  for I := Dirs.Count - 1 downto 0 do
    begin
      if Exact then
        begin
          if Dirs[I] = PathToRemove then
            Dirs.Delete(I);
        end
      else
        begin
          if Dirs[I].Contains(PathToRemove) then
            Dirs.Delete(I);
        end
    end;

  SomePaths := JoinPaths(Dirs);
  Dirs.Free;
end;

procedure TForm1.AddPath(var SomePaths: String; const PathToAdd: String);
var
  Dirs: TStringList;
begin
  Dirs := TStringList.Create;
  SplitPaths(SomePaths, Dirs);

  Dirs.Add(PathToAdd);

  SomePaths := JoinPaths(Dirs);
  Dirs.Free;
end;

procedure TForm1.RemovePackagePaths(const PackagePath: String);
begin
  if PackagePath = P4DPE then
    begin
      RemovePath(SelectedDelphiVersion.Win32.SearchPath, PackagePath + '\src\AddOn');
      RemovePath(SelectedDelphiVersion.Win32.DebugDCUPath, PackagePath + '\src\AddOn');
      RemovePath(SelectedDelphiVersion.Win64.SearchPath, PackagePath + '\src\AddOn');
      RemovePath(SelectedDelphiVersion.Win64.DebugDCUPath, PackagePath + '\src\AddOn');
      RemovePath(SelectedDelphiVersion.Linux64.SearchPath, PackagePath + '\src\AddOn');
      RemovePath(SelectedDelphiVersion.Linux64.DebugDCUPath, PackagePath + '\src\AddOn');
      RemovePath(SelectedDelphiVersion.Android32.SearchPath, PackagePath + '\src\AddOn');
      RemovePath(SelectedDelphiVersion.Android32.DebugDCUPath, PackagePath + '\src\AddOn');
      RemovePath(SelectedDelphiVersion.Android64.SearchPath, PackagePath + '\src\AddOn');
      RemovePath(SelectedDelphiVersion.Android64.DebugDCUPath, PackagePath + '\src\AddOn');
      RemovePath(SelectedDelphiVersion.OSXARM64.SearchPath, PackagePath + '\src\AddOn');
      RemovePath(SelectedDelphiVersion.OSXARM64.DebugDCUPath, PackagePath + '\src\AddOn');
      RemovePath(SelectedDelphiVersion.OSX64.SearchPath, PackagePath + '\src\AddOn');
      RemovePath(SelectedDelphiVersion.OSX64.DebugDCUPath, PackagePath + '\src\AddOn');
    end;

  RemovePath(SelectedDelphiVersion.Win32.SearchPath, PackagePath + '\lib\Win32\Release');
  RemovePath(SelectedDelphiVersion.Win32.DebugDCUPath, PackagePath + '\lib\Win32\Debug');
  RemovePath(SelectedDelphiVersion.Win64.SearchPath, PackagePath + '\lib\Win64\Release');
  RemovePath(SelectedDelphiVersion.Win64.DebugDCUPath, PackagePath + '\lib\Win64\Debug');
  RemovePath(SelectedDelphiVersion.Linux64.SearchPath, PackagePath + '\lib\Linux64\Release');
  RemovePath(SelectedDelphiVersion.Linux64.DebugDCUPath, PackagePath + '\lib\Linux64\Debug');
  RemovePath(SelectedDelphiVersion.Android32.SearchPath, PackagePath + '\lib\Android32\Release');
  RemovePath(SelectedDelphiVersion.Android32.DebugDCUPath, PackagePath + '\lib\Android32\Debug');
  RemovePath(SelectedDelphiVersion.Android64.SearchPath, PackagePath + '\lib\Android64\Release');
  RemovePath(SelectedDelphiVersion.Android64.DebugDCUPath, PackagePath + '\lib\Android64\Debug');
  RemovePath(SelectedDelphiVersion.OSXARM64.SearchPath, PackagePath + '\lib\OSXARM64\Release');
  RemovePath(SelectedDelphiVersion.OSXARM64.DebugDCUPath, PackagePath + '\lib\OSXARM64\Debug');
  RemovePath(SelectedDelphiVersion.OSX64.SearchPath, PackagePath + '\lib\OSX64\Release');
  RemovePath(SelectedDelphiVersion.OSX64.DebugDCUPath, PackagePath + '\lib\OSX64\Debug');
end;

procedure TForm1.AddPackagePaths(const PackagePath: String);
begin
  if PackagePath = P4DPE then
    begin
      AddPath(SelectedDelphiVersion.Win32.SearchPath, P4DFolder + '\' + PackagePath + '\src\AddOn');
      AddPath(SelectedDelphiVersion.Win32.DebugDCUPath, P4DFolder + '\' + PackagePath + '\src\AddOn');
      AddPath(SelectedDelphiVersion.Win64.SearchPath, P4DFolder + '\' + PackagePath + '\src\AddOn');
      AddPath(SelectedDelphiVersion.Win64.DebugDCUPath, P4DFolder + '\' + PackagePath + '\src\AddOn');
      AddPath(SelectedDelphiVersion.Linux64.SearchPath, P4DFolder + '\' + PackagePath + '\src\AddOn');
      AddPath(SelectedDelphiVersion.Linux64.DebugDCUPath, P4DFolder + '\' + PackagePath + '\src\AddOn');
      AddPath(SelectedDelphiVersion.Android32.SearchPath, P4DFolder + '\' + PackagePath + '\src\AddOn');
      AddPath(SelectedDelphiVersion.Android32.DebugDCUPath, P4DFolder + '\' + PackagePath + '\src\AddOn');
      AddPath(SelectedDelphiVersion.Android64.SearchPath, P4DFolder + '\' + PackagePath + '\src\AddOn');
      AddPath(SelectedDelphiVersion.Android64.DebugDCUPath, P4DFolder + '\' + PackagePath + '\src\AddOn');
      AddPath(SelectedDelphiVersion.OSXARM64.SearchPath, P4DFolder + '\' + PackagePath + '\src\AddOn');
      AddPath(SelectedDelphiVersion.OSXARM64.DebugDCUPath, P4DFolder + '\' + PackagePath + '\src\AddOn');
      AddPath(SelectedDelphiVersion.OSX64.SearchPath, P4DFolder + '\' + PackagePath + '\src\AddOn');
      AddPath(SelectedDelphiVersion.OSX64.DebugDCUPath, P4DFolder + '\' + PackagePath + '\src\AddOn');
    end;
  AddPath(SelectedDelphiVersion.Win32.SearchPath, P4DFolder + '\' + PackagePath + '\lib\Win32\Release');
  AddPath(SelectedDelphiVersion.Win32.DebugDCUPath, P4DFolder + '\' + PackagePath + '\lib\Win32\Debug');
  AddPath(SelectedDelphiVersion.Win64.SearchPath, P4DFolder + '\' + PackagePath + '\lib\Win64\Release');
  AddPath(SelectedDelphiVersion.Win64.DebugDCUPath, P4DFolder + '\' + PackagePath + '\lib\Win64\Debug');
  AddPath(SelectedDelphiVersion.Linux64.SearchPath, P4DFolder + '\' + PackagePath + '\lib\Linux64\Release');
  AddPath(SelectedDelphiVersion.Linux64.DebugDCUPath, P4DFolder + '\' + PackagePath + '\lib\Linux64\Debug');
  AddPath(SelectedDelphiVersion.Android32.SearchPath, P4DFolder + '\' + PackagePath + '\lib\Android32\Release');
  AddPath(SelectedDelphiVersion.Android32.DebugDCUPath, P4DFolder + '\' + PackagePath + '\lib\Android32\Debug');
  AddPath(SelectedDelphiVersion.Android64.SearchPath, P4DFolder + '\' + PackagePath + '\lib\Android64\Release');
  AddPath(SelectedDelphiVersion.Android64.DebugDCUPath, P4DFolder + '\' + PackagePath + '\lib\Android64\Debug');
  AddPath(SelectedDelphiVersion.OSXARM64.SearchPath, P4DFolder + '\' + PackagePath + '\lib\OSXARM64\Release');
  AddPath(SelectedDelphiVersion.OSXARM64.DebugDCUPath, P4DFolder + '\' + PackagePath + '\lib\OSXARM64\Debug');
  AddPath(SelectedDelphiVersion.OSX64.SearchPath, P4DFolder + '\' + PackagePath + '\lib\OSX64\Release');
  AddPath(SelectedDelphiVersion.OSX64.DebugDCUPath, P4DFolder + '\' + PackagePath + '\lib\OSX64\Debug');
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if P4DFolder <> String.Empty then
    begin
      RemovePackagePaths(P4DLW);
      RemovePackagePaths(P4DDS);
      RemovePackagePaths(P4DPE);
      RemovePackagePaths(P4DPP);
      RemovePackagePaths(P4DPD);

      AddPackagePaths(P4DLW);
      AddPackagePaths(P4DDS);
      AddPackagePaths(P4DPE);
      AddPackagePaths(P4DPP);
      AddPackagePaths(P4DPD);

      WriteDelphiInfo(SelectedDelphiVersion, SelectedDelphiVersion.BDSVer);

      ShowSelectedDelphi(False);
    end;
end;

procedure TForm1.ShowSelectedDelphi(const Clear: Boolean = True);
var
  Dirs: TStringList;
  Joined: String;
begin
  if Assigned(SelectedDelphiVersion) then
    begin
      Dirs := TStringList.Create;
      if Clear then
        Memo1.Lines.Clear;
      Memo1.Lines.Add('BDS Ver : ' + SelectedDelphiVersion.BDSVer);
      Memo1.Lines.Add('App Loc : ' + SelectedDelphiVersion.AppLoc);
      Memo1.Lines.Add('App Ver : ' + SelectedDelphiVersion.AppVer);
      Memo1.Lines.Add('ProdVer : ' + SelectedDelphiVersion.ProdVer);
      Memo1.Lines.Add('BDS Dir : ' + SelectedDelphiVersion.BDSDir);
      Memo1.Lines.Add('---');
      Memo1.Lines.Add('Search Path : ' + sLineBreak + SelectedDelphiVersion.Win64.SearchPath);
      SplitPaths(SelectedDelphiVersion.Win64.SearchPath, Dirs);
      Memo1.Lines.AddStrings(Dirs);
      Memo1.Lines.Add(JoinPaths(Dirs));
      Memo1.Lines.Add('---');
      Memo1.Lines.Add('Debug DCU Path : ' + sLineBreak + SelectedDelphiVersion.Win64.DebugDCUPath);
      SplitPaths(SelectedDelphiVersion.Win64.DebugDCUPath, Dirs);
      Memo1.Lines.AddStrings(Dirs);
      Memo1.Lines.Add(JoinPaths(Dirs));
      Dirs.Free;
    end;
end;

procedure TForm1.GetDelphiVersions;
var
  I: Integer;
  IDX: Integer;
begin
  DelphiList := TStringList.Create;
  GetDelphiList(DelphiList);

  if DelphiList.Count > 0 then
    begin
      ComboBox1.Enabled := True;
      for I := 0 to DelphiList.Count - 1 do
        begin
          if Assigned(DelphiList.Objects[I]) then
            begin
              IDX := DelphiPrettyName.IndexOfName(DelphiList.Strings[I]);
              if IDX >= 0 then
                begin
                  ComboBox1.Items.AddObject(DelphiPrettyName.ValueFromIndex[IDX], DelphiList.Objects[I]);
                end;
            end;
        end;
    end;
  if ComboBox1.Items.Count > 0 then
    begin
     ComboBox1.ItemIndex := 0;
     ShowSelectedDelphi;
    end
  else
    ComboBox1.Visible := False;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  InitialDir: String;
begin
  if P4DFolder = String.Empty then
    InitialDir := TPath.GetSharedDocumentsPath
  else
    InitialDir := P4DFolder;
{ Reminder
  P4DLW = 'Lightweight-Python-Wrappers';
  P4DDS = 'P4D-Data-Sciences';
  P4DPE = 'PythonEnviroments';
  P4DPP = 'PythonPackages4Delphi';
  P4DPD = 'python4delphi';
}
  if SelectDirectory('Export Component Package as a Folder', InitialDir, P4DFolder) then
    begin
      if DirectoryExists(P4DFolder + '\' + P4DLW) and
         DirectoryExists(P4DFolder + '\' + P4DDS) and
         DirectoryExists(P4DFolder + '\' + P4DPE) and
         DirectoryExists(P4DFolder + '\' + P4DPP) and
         DirectoryExists(P4DFolder + '\' + P4DPD) then
        Label1.Text := 'P4D Suite Folder : ' + P4DFolder
      else
        begin
          Label1.Text := 'P4D Suite Folder : ';
          P4DFolder := String.Empty;
        end;
    end;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  SelectedDelphiVersion := TDelphiVersion(ComboBox1.Items.Objects[ComboBox1.ItemIndex]);
  ShowSelectedDelphi;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  P4DFolder := String.Empty;
  SelectedDelphiVersion := Nil;
  ComboBox1.Enabled := False;
  MakeDelphiPrettyNames;
  GetDelphiVersions;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  DelphiList.Free;
  DelphiPrettyName.Free;
end;

procedure TForm1.MakeDelphiPrettyNames;
begin
  DelphiPrettyName := TStringList.Create;
  DelphiPrettyName.NameValueSeparator := '|';

  DelphiPrettyName.Add('22.0|Delphi 11 Alerandria');
  DelphiPrettyName.Add('21.0|Delphi 10.4 Sydney');
  DelphiPrettyName.Add('20.0|Delphi 10.3 Rio');
  DelphiPrettyName.Add('19.0|Delphi 10.2 Tokyo');
end;

end.
