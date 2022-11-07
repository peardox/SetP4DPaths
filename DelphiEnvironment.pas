unit DelphiEnvironment;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Generics.Collections;

function GetDelphiEnvironment(const ADelphiVersion: String; var EnvList: TDictionary<String, String>): Boolean; overload;

implementation

uses
  System.IOUtils,
  Xml.omnixmldom,
  Xml.xmldom,
  XmlIntf,
  XmlDoc;

function GetDelphiEnvironment(const ADelphiVersion: String; var EnvList: TDictionary<String, String>): Boolean;
var
  DelphiEnvXML: String;
  Doc: IXMLDocument;
  List: IDOMNodeList;
  Props: IDOMNodeList;
  Item: IDOMNodeList;
  lSelectNode: IdomNodeSelect;
  AEnvironmentFile: String;
  I: Integer;
begin
  Result := False;
  DefaultDOMVendor := sOmniXmlVendor;
  AEnvironmentFile := IncludeTrailingPathDelimiter(TPath.GetHomePath) +
    'Embarcadero\BDS\' + ADelphiVersion + '\environment.proj';
  try

    DelphiEnvXML := TFile.ReadAllText(AEnvironmentFile);
    Doc := LoadXMLData(DelphiEnvXML);

    if supports(Doc.DOMDocument, IDomNodeSelect, lSelectNode) then
    begin
      List := lSelectNode.selectNodes('/Project/PropertyGroup');
      if List.length = 1 then
        begin
          if List.item[0].hasChildNodes then
            begin
              Props := List.item[0].childNodes;
              if Props.length > 0 then
                begin
                  Result := True;
                  for I := 0 to Props.length - 1 do
                    begin
                      if Props.item[I].hasChildNodes then
                        begin
                          Item := Props.item[I].childNodes;
                          if Item.length = 1 then
                            EnvList.Add(Props.item[I].nodeName, Item.item[0].nodeValue);
                        end;
                    end;
                end;
            end;
        end;
    end;

  except
    on E: Exception do
        Raise Exception.CreateFmt('%s : %s', [E.ClassName, E.Message]);
  end;
end;

end.
