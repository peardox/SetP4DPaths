unit DelphiRegistry;

// {$DEFINE FULL_PLATFORM_DETAILS}
// {$DEFINE INCLUDE_ENVIRONMENT}
// {$DEFINE INCLUDE_IOS}

interface

uses
   System.SysUtils, System.Types, System.UITypes, System.Classes,
   {$IFDEF INCLUDE_ENVIRONMENT}
   DelphiEnvironment,
   {$ENDIF INCLUDE_ENVIRONMENT}
   System.Rtti, System.Generics.Collections;

type
  TDelphiPlatform = Class
  public
    SearchPath: String; // Search Path
    DebugDCUPath: String; // Debug DCU Path
    {$IFDEF FULL_PLATFORM_DETAILS}
    BrowsingPath: String; // Browsing Path
    HPPOutputDirectory: String; // HPP Output Directory
    NamespaceSearchPath: String; // Namespace Search Path
    PackageDCPOutput: String; // Package DCP Output
    PackageDPLOutput: String; // Package DPL Output
    PackageSearchPath: String; // Package Search Path
    TranslatedDebugLibraryPath: String; // Translated Debug Library Path
    TranslatedLibraryPath: String; // Translated Library Path
    TranslatedResourcePath: String; // Translated Resource Path
    {$ENDIF FULL_PLATFORM_DETAILS}
    constructor Create;
    destructor Destroy; override;
  end;

  TDelphiVersion = Class
  public
    BDSVer: String;
    AppLoc: String;
    AppVer: String;
    ProdVer: String;
    BDSDir: String;
    Android32: TDelphiPlatform;
    Android64: TDelphiPlatform;
    Linux64: TDelphiPlatform;
    OSX64: TDelphiPlatform;
    OSXARM64: TDelphiPlatform;
    Win32: TDelphiPlatform;
    Win64: TDelphiPlatform;
    {$IFDEF INCLUDE_IOS}
    IOSDevice64: TDelphiPlatform;
    IOSSimARM64: TDelphiPlatform;
    IOSSimulator: TDelphiPlatform;
    {$ENDIF INCLUDE_IOS}
    {$IFDEF INCLUDE_ENVIRONMENT}
    Environment: TDictionary<String, String>;
    {$ENDIF INCLUDE_ENVIRONMENT}
    constructor Create;
    destructor Destroy; override;
  end;

function JoinPaths(ListOfStrings: TStrings): String;
procedure SplitPaths(Str: string; ListOfStrings: TStrings);
function ReadDelphiPlatformInfo(var DelphiPlatform: TDelphiPlatform; const DelphiBDSVer: String; const APlatform: String): Boolean;
function ReadDelphiInfo(var ADelphiVersion: TDelphiVersion; const DelphiBDSVer: String): Boolean;
function GetComponentList(var ComponentList: TStringList; const DelphiVersion: String): Boolean;
function GetDelphiList(var ADelphiList: TStringList): Boolean;
function FindAllDescendantsOf(basetype: TClass): TList<TClass>;
function FileVersion(const FileName: TFileName): String;
function WriteDelphiPlatformInfo(var DelphiPlatform: TDelphiPlatform; const DelphiBDSVer: String; const APlatform: String): Boolean;
function WriteDelphiInfo(const ADelphiVersion: TDelphiVersion; const DelphiBDSVer: String): Boolean;

implementation

uses
  WinApi.Windows,
  System.Win.Registry;

const
  DelphiRegKey = '\Software\Embarcadero\BDS';
  ComponentRegKey = '\ToolForm\Mapping';
  LookingForClassPrefix = 'TComponentBased.System.Classes.TPersistent.';

constructor TDelphiVersion.Create;
begin
  Inherited;
  BDSVer := String.Empty;
  AppLoc := String.Empty;
  AppVer := String.Empty;
  ProdVer := String.Empty;
  BDSDir := String.Empty;
  Android32 := TDelphiPlatform.Create;
  Android64 := TDelphiPlatform.Create;
  Linux64 := TDelphiPlatform.Create;
  OSX64 := TDelphiPlatform.Create;
  OSXARM64 := TDelphiPlatform.Create;
  Win32 := TDelphiPlatform.Create;
  Win64 := TDelphiPlatform.Create;
  {$IFDEF INCLUDE_IOS}
  IOSDevice64 := TDelphiPlatform.Create;
  IOSSimARM64 := TDelphiPlatform.Create;
  IOSSimulator := TDelphiPlatform.Create;
  {$ENDIF INCLUDE_IOS}
  {$IFDEF INCLUDE_ENVIRONMENT}
  Environment := TDictionary<String, String>.Create;
  {$ENDIF INCLUDE_ENVIRONMENT}
end;

destructor TDelphiVersion.Destroy;
begin
  FreeAndNil(Android32);
  FreeAndNil(Android64);
  FreeAndNil(Linux64);
  FreeAndNil(OSX64);
  FreeAndNil(OSXARM64);
  FreeAndNil(Win32);
  FreeAndNil(Win64);
  {$IFDEF INCLUDE_IOS}
  FreeAndNil(IOSDevice64);
  FreeAndNil(IOSSimARM64);
  FreeAndNil(IOSSimulator);
  {$ENDIF INCLUDE_IOS}
  {$IFDEF INCLUDE_ENVIRONMENT}
  Environment.Free;
  {$ENDIF INCLUDE_ENVIRONMENT}
  Inherited;
end;

constructor TDelphiPlatform.Create;
begin
  Inherited;
  SearchPath := String.Empty; // Search Path
  DebugDCUPath := String.Empty; // Debug DCU Path
  {$IFDEF FULL_PLATFORM_DETAILS}
  BrowsingPath := String.Empty; // Browsing Path
  HPPOutputDirectory := String.Empty; // HPP Output Directory
  NamespaceSearchPath := String.Empty; // Namespace Search Path
  PackageDCPOutput := String.Empty; // Package DCP Output
  PackageDPLOutput := String.Empty; // Package DPL Output
  PackageSearchPath := String.Empty; // Package Search Path
  TranslatedDebugLibraryPath := String.Empty; // Translated Debug Library Path
  TranslatedLibraryPath := String.Empty; // Translated Library Path
  TranslatedResourcePath := String.Empty; // Translated Resource Path
  {$ENDIF FULL_PLATFORM_DETAILS}
end;

destructor TDelphiPlatform.Destroy;
begin
  Inherited;
end;

// https://stackoverflow.com/questions/64380302/collect-all-descendants-of-a-class-using-rtti-in-delphi-10-3-3
function FindAllDescendantsOf(basetype: TClass): TList<TClass>;
var
  ctx: TRttiContext;
  lType: TRttiType;
begin
  result := TList<TClass>.Create;
  ctx := TRttiContext.Create;
  for lType in ctx.GetTypes do
    if (lType is TRttiInstanceType) and
       (TRttiInstanceType(lType).MetaclassType.InheritsFrom(basetype)) then
      result.add(TRttiInstanceType(lType).MetaclassType);
end;

// https://stackoverflow.com/questions/2625707/split-a-string-into-an-array-of-strings-based-on-a-delimiter
procedure Split(Delimiter: Char; Str: string; ListOfStrings: TStrings) ;
begin
   ListOfStrings.Clear;
   ListOfStrings.Delimiter       := Delimiter;
   ListOfStrings.StrictDelimiter := True; // Requires D2006 or newer.
   ListOfStrings.DelimitedText   := Str;
end;

procedure SplitPaths(Str: string; ListOfStrings: TStrings);
begin
  Split(';', Str, ListOfStrings);
end;

function JoinPaths(ListOfStrings: TStrings): String;
var
  I: Integer;
begin
  for I := 0 to ListOfStrings.Count - 1 do
    begin
      if I = 0 then
        Result := ListOfStrings[I]
      else
        Result := Result + ';' + ListOfStrings[I];
    end;
end;

// https://stackoverflow.com/questions/17279394/getfileversioninfosize-and-getfileversioninfo-return-nothing/17286050#17286050
function FileVersion(const FileName: TFileName): String;
var
  VerInfoSize: Cardinal;
  VerValueSize: Cardinal;
  Dummy: Cardinal;
  PVerInfo: Pointer;
  PVerValue: PVSFixedFileInfo;
  iLastError: DWord;
begin
  Result := String.Empty;
  VerInfoSize := GetFileVersionInfoSize(PChar(FileName), Dummy);
  if VerInfoSize > 0 then
  begin
    GetMem(PVerInfo, VerInfoSize);
    try
      if GetFileVersionInfo(PChar(FileName), 0, VerInfoSize, PVerInfo) then
      begin
        if VerQueryValue(PVerInfo, '\', Pointer(PVerValue), VerValueSize) then
          with PVerValue^ do
            Result := Format('%d.%d.%d.%d', [
              HiWord(dwFileVersionMS), //Major
              LoWord(dwFileVersionMS), //Minor
              HiWord(dwFileVersionLS), //Release
              LoWord(dwFileVersionLS)]); //Build
      end
      else
      begin
        iLastError := GetLastError;
        Raise Exception.CreateFmt('GetFileVersionInfo failed: (%d) %s',
                      [iLastError, SysErrorMessage(iLastError)]);
      end;
    finally
      FreeMem(PVerInfo, VerInfoSize);
    end;
  end
  else
  begin
    iLastError := GetLastError;
    Raise Exception.CreateFmt('GetFileVersionInfo failed: (%d) %s',
                     [iLastError, SysErrorMessage(iLastError)]);
  end;
end;

function GetComponentList(var ComponentList: TStringList; const DelphiVersion: String): Boolean;
var
  reg: TRegistry;
  KeyStrings: TStringList;
  SplitList: TStringList;
  KeyName: String;
  KeyValue: String;
  KeyIndex: Integer;
  SplitValue: String;
  SplitIndex: Integer;
begin
  Result := False;
  reg := Nil;
  SplitList := TStringList.Create;
  KeyStrings := TStringList.Create;
  ComponentList.Clear;
  ComponentList.Duplicates := dupIgnore;
  ComponentList.CaseSensitive := False;
  ComponentList.Sorted := True;

  try
    try
      reg := TRegistry.Create(KEY_READ);
      reg.RootKey := HKEY_CURRENT_USER;
      KeyName := DelphiRegKey + '\' + DelphiVersion;

      try
        if reg.KeyExists(KeyName) then
          begin
            KeyName := KeyName + ComponentRegKey;
            if reg.KeyExists(KeyName) then
              begin
                if reg.OpenKey(KeyName, False) then
                  begin
                    try
                      reg.GetValueNames(KeyStrings);
                      for KeyIndex := 0 to KeyStrings.Count - 1 do
                        begin
                          try
                            KeyValue := reg.ReadString(KeyStrings[KeyIndex]);
                            if KeyValue <> String.Empty then
                              begin
                                Result := True;
                                Split(',', KeyValue, SplitList);
                                for SplitIndex := 0 to SplitList.Count - 1 do
                                  begin
                                    SplitValue := SplitList[SplitIndex];
                                    if SplitValue.StartsWith(LookingForClassPrefix) then
                                      begin
                                        SplitValue := SplitValue.Replace(LookingForClassPrefix, '', [rfReplaceAll]);
                                        ComponentList.Add(SplitValue);
                                      end;
                                  end;
                              end;
                          except
                            // Nothing
                          end;
                        end;
                    finally
                      reg.CloseKey;
                    end;
                  end;
              end;
          end;
      finally
        KeyStrings.Free;
        SplitList.Free;
      end;
    except
      // Couldn't open registry

    end;
  finally
    reg.Free;
  end;
end;

function GetDelphiList(var ADelphiList: TStringList): Boolean;
var
  reg: TRegistry;
  TrialDelphi: TStringList;
  DelphiVersion: TDelphiVersion;
  I: Integer;
begin
  Result := False;
  reg := Nil;
  ADelphiList.Clear;
  ADelphiList.OwnsObjects := True;
  TrialDelphi := TStringList.Create;
  TrialDelphi.Duplicates := dupIgnore;
  TrialDelphi.CaseSensitive := False;
  TrialDelphi.Sorted := True;

  try
    try
      reg := TRegistry.Create(KEY_READ);
      reg.RootKey := HKEY_CURRENT_USER;

      if reg.KeyExists(DelphiRegKey) then
        begin
          if reg.OpenKey(DelphiRegKey, False) then
            begin
              try
                reg.GetKeyNames(TrialDelphi);
                // Reverse the sort
                for I := TrialDelphi.Count - 1 downto 0 do
                  begin
                    DelphiVersion := TDelphiVersion.Create;
                    // Delphi inserts junk so skip those
                    if ReadDelphiInfo(DelphiVersion, TrialDelphi[I]) then
                      ADelphiList.AddObject(TrialDelphi[I], DelphiVersion)
                    else
                      DelphiVersion.Free;
                  end;
                  Result := True;
              finally
                reg.CloseKey;
              end;
            end;
        end;
    except
      // Couldn't open registry
    end;
  finally
    reg.Free;
    TrialDelphi.Free;
  end;
end;

function ReadDelphiInfo(var ADelphiVersion: TDelphiVersion; const DelphiBDSVer: String): Boolean;
var
  reg: TRegistry;
begin
  Result := False;
  reg := Nil;

  try
    try
      reg := TRegistry.Create(KEY_READ);
      reg.RootKey := HKEY_CURRENT_USER;

      if reg.KeyExists(DelphiRegKey + '\' + DelphiBDSVer) then
        begin
          if reg.OpenKey(DelphiRegKey + '\' + DelphiBDSVer, False) then
            begin
              ADelphiVersion.AppLoc := reg.ReadString('App');
              if FileExists(ADelphiVersion.AppLoc) then
                begin
                  try
                    ADelphiVersion.AppVer := FileVersion(ADelphiVersion.AppLoc);
                  except
                    // Bad version
                    on E : Exception do
                      Raise Exception.Create('Couldn''t read delphi version');
                  end;
                  if ADelphiVersion.AppVer <> String.Empty then
                    begin
                      ADelphiVersion.ProdVer := reg.ReadString('ProductVersion');
                      ADelphiVersion.BDSDir := ExcludeTrailingPathDelimiter(reg.ReadString('RootDir'));
                      if Length(ADelphiVersion.BDSDir) > 0 then
                        begin
                          ReadDelphiPlatformInfo(ADelphiVersion.Android32, DelphiBDSVer, 'Android32');
                          ReadDelphiPlatformInfo(ADelphiVersion.Android64, DelphiBDSVer, 'Android64');
                          ReadDelphiPlatformInfo(ADelphiVersion.Linux64, DelphiBDSVer, 'Linux64');
                          ReadDelphiPlatformInfo(ADelphiVersion.OSX64, DelphiBDSVer, 'OSX64');
                          ReadDelphiPlatformInfo(ADelphiVersion.OSXARM64, DelphiBDSVer, 'OSXARM64');
                          ReadDelphiPlatformInfo(ADelphiVersion.Win32, DelphiBDSVer, 'Win32');
                          ReadDelphiPlatformInfo(ADelphiVersion.Win64, DelphiBDSVer, 'Win64');
                          {$IFDEF INCLUDE_IOS}
                          ReadDelphiPlatformInfo(ADelphiVersion.IOSDevice64, DelphiBDSVer, 'IOSDevice64');
                          ReadDelphiPlatformInfo(ADelphiVersion.IOSSimARM64, DelphiBDSVer, 'IOSSimARM64');
                          ReadDelphiPlatformInfo(ADelphiVersion.IOSSimulator, DelphiBDSVer, 'IOSSimulator');
                          {$ENDIF INCLUDE_IOS}
                          {$IFDEF INCLUDE_ENVIRONMENT}
                          GetDelphiEnvironment(DelphiBDSVer, ADelphiVersion.Environment);
                          {$ENDIF INCLUDE_ENVIRONMENT}
                          ADelphiVersion.BDSVer := DelphiBDSVer;
                          Result := True;
                        end;
                    end;
                end;
              reg.CloseKey;
            end;
        end;
    except
      // Couldn't open registry
      on E : Exception do
        Raise Exception.Create('Couldn''t open registry');
    end;
  finally
    reg.Free;
  end;
end;

function ReadDelphiPlatformInfo(var DelphiPlatform: TDelphiPlatform; const DelphiBDSVer: String; const APlatform: String): Boolean;
var
  reg: TRegistry;
  PlatformKey: String;
begin
// Computer\HKEY_CURRENT_USER\Software\Embarcadero\BDS\22.0\Library\Android32

  Result := False;
  reg := Nil;

  try
    try
      reg := TRegistry.Create(KEY_READ);
      reg.RootKey := HKEY_CURRENT_USER;
      PlatformKey := DelphiRegKey + '\' + DelphiBDSVer + '\Library\' + APlatform;
      if reg.KeyExists(PlatformKey) then
        begin
          if reg.OpenKey(PlatformKey, False) then
            begin
              DelphiPlatform.SearchPath := reg.ReadString('Search Path');
              DelphiPlatform.DebugDCUPath := reg.ReadString('Debug DCU Path');
              {$IFDEF FULL_PLATFORM_DETAILS}
              DelphiPlatform.BrowsingPath := reg.ReadString('Browsing Path');
              DelphiPlatform.HPPOutputDirectory := reg.ReadString('HPP Output Directory');
              DelphiPlatform.NamespaceSearchPath := reg.ReadString('Namespace Search Path');
              DelphiPlatform.PackageDCPOutput := reg.ReadString('Package DCP Output');
              DelphiPlatform.PackageDPLOutput := reg.ReadString('Package DPL Output');
              DelphiPlatform.PackageSearchPath := reg.ReadString('Package Search Path');
              DelphiPlatform.TranslatedDebugLibraryPath := reg.ReadString('Translated Debug Library Path');
              DelphiPlatform.TranslatedLibraryPath := reg.ReadString('Translated Library Path');
              DelphiPlatform.TranslatedResourcePath := reg.ReadString('Translated Resource Path');
              {$ENDIF FULL_PLATFORM_DETAILS}

              Result := True;
              reg.CloseKey;
            end;
        end;
    except
      // Couldn't open registry
      on E : Exception do
        Raise Exception.Create('Couldn''t open registry');
    end;
  finally
    reg.Free;
  end;
end;

function WriteDelphiInfo(const ADelphiVersion: TDelphiVersion; const DelphiBDSVer: String): Boolean;
var
  reg: TRegistry;
begin
  Result := False;
  reg := Nil;

  try
    try
      reg := TRegistry.Create(KEY_READ);
      reg.RootKey := HKEY_CURRENT_USER;

      if reg.KeyExists(DelphiRegKey + '\' + DelphiBDSVer) then
        begin
          if reg.OpenKey(DelphiRegKey + '\' + DelphiBDSVer, False) then
            begin
              WriteDelphiPlatformInfo(ADelphiVersion.Android32, DelphiBDSVer, 'Android32');
              WriteDelphiPlatformInfo(ADelphiVersion.Android64, DelphiBDSVer, 'Android64');
              WriteDelphiPlatformInfo(ADelphiVersion.Linux64, DelphiBDSVer, 'Linux64');
              WriteDelphiPlatformInfo(ADelphiVersion.OSX64, DelphiBDSVer, 'OSX64');
              if DelphiBDSVer > '22.0' then
                WriteDelphiPlatformInfo(ADelphiVersion.OSXARM64, DelphiBDSVer, 'OSXARM64');
              WriteDelphiPlatformInfo(ADelphiVersion.Win32, DelphiBDSVer, 'Win32');
              WriteDelphiPlatformInfo(ADelphiVersion.Win64, DelphiBDSVer, 'Win64');
              {$IFDEF INCLUDE_IOS}
              ReadDelphiPlatformInfo(ADelphiVersion.IOSDevice64, DelphiBDSVer, 'IOSDevice64');
              ReadDelphiPlatformInfo(ADelphiVersion.IOSSimARM64, DelphiBDSVer, 'IOSSimARM64');
              ReadDelphiPlatformInfo(ADelphiVersion.IOSSimulator, DelphiBDSVer, 'IOSSimulator');
              {$ENDIF INCLUDE_IOS}
              Result := True;
              reg.CloseKey;
            end;
        end;
    except
      // Couldn't open registry
      on E : Exception do
        Raise Exception.Create('Couldn''t open registry');
    end;
  finally
    reg.Free;
  end;
end;

function WriteDelphiPlatformInfo(var DelphiPlatform: TDelphiPlatform; const DelphiBDSVer: String; const APlatform: String): Boolean;
var
  reg: TRegistry;
  PlatformKey: String;
begin
// Computer\HKEY_CURRENT_USER\Software\Embarcadero\BDS\22.0\Library\Android32

  Result := False;
  reg := Nil;

  try
    try
      reg := TRegistry.Create(KEY_SET_VALUE);
      reg.RootKey := HKEY_CURRENT_USER;
      PlatformKey := DelphiRegKey + '\' + DelphiBDSVer + '\Library\' + APlatform;
      if reg.KeyExists(PlatformKey) then
        begin
          if reg.OpenKey(PlatformKey, False) then
            begin
              reg.WriteString('Search Path', DelphiPlatform.SearchPath);
              reg.WriteString('Debug DCU Path', DelphiPlatform.DebugDCUPath);
              {$IFDEF FULL_PLATFORM_DETAILS}
              reg.WriteString('Browsing Path', DelphiPlatform.BrowsingPath);
              reg.WriteString('HPP Output Directory', DelphiPlatform.HPPOutputDirectory);
              reg.WriteString('Namespace Search Path', DelphiPlatform.NamespaceSearchPath);
              reg.WriteString('Package DCP Output', DelphiPlatform.PackageDCPOutput);
              reg.WriteString('Package DPL Output', DelphiPlatform.PackageDPLOutput);
              reg.WriteString('Package Search Path', DelphiPlatform.PackageSearchPath);
              reg.WriteString('Translated Debug Library Path', DelphiPlatform.TranslatedDebugLibraryPath);
              reg.WriteString('Translated Library Path', DelphiPlatform.TranslatedLibraryPath);
              reg.WriteString('Translated Resource Path', DelphiPlatform.TranslatedResourcePath);
              {$ENDIF FULL_PLATFORM_DETAILS}

              Result := True;
              reg.CloseKey;
            end;
        end;
    except
      // Couldn't open registry
      on E : Exception do
        Raise Exception.Create('Couldn''t open registry');
    end;
  finally
    reg.Free;
  end;
end;


end.
