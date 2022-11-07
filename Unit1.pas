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
    procedure MakeDelphiPrettyNames;
    procedure GetDelphiVersions;
    procedure ShowSelectedDelphi;
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

procedure TForm1.Button1Click(Sender: TObject);
begin
//  ShowSelectedDelphi;
end;

procedure TForm1.ShowSelectedDelphi;
var
  Dirs: TStringList;
  Joined: String;
begin
  if Assigned(SelectedDelphiVersion) then
    begin
      Dirs := TStringList.Create;
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

  if DelphiList.Count > 1 then
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

  if SelectDirectory('Export Component Package as a Folder', InitialDir, P4DFolder) then
    begin
      Label1.Text := 'P4D Suite Folder : ' + P4DFolder;
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
