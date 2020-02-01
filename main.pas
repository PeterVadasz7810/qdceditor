(*

qDCEditor can create the dublin_core.xml file in a given folder for DSpace upload

qDCEditor létrehozza a dublin_core.xml fájlt adott mappába a DSpace-be való felöltéshez

Copyright (C) 2020  Vadász Péter

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program. If not, see <http://www.gnu.org/licenses/>.


Ez a program szabad szoftver; terjeszthető illetve módosítható a Free Software
Foundation által kiadott GNU General Public License dokumentumában leírtak;
akár a licenc 3-as, akár (tetszőleges) későbbi változata szerint.

Ez a program abban a reményben kerül közreadásra, hogy hasznos lesz, de minden
egyéb GARANCIA NÉLKÜL, az ELADHATÓSÁGRA vagy VALAMELY CÉLRA VALÓ
ALKALMAZHATÓSÁGRA való származtatott garanciát is beleértve. További
részleteket a GNU General Public License tartalmaz.

A felhasználónak a programmal együtt meg kell kapnia a GNU General Public
License egy példányát; ha mégsem kapta meg, akkor tekintse meg a
<http://www.gnu.org/licenses/> oldalon.

*)

unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons, SynEdit, SynHighlighterXML, DOM, XMLRead;

type

  { TfrmEditor }

  TfrmEditor = class(TForm)
    bbtnSave: TBitBtn;
    bbtnExit: TBitBtn;
    Bevel1: TBevel;
    btnOpenWorkingDir: TBitBtn;
    cbDCElement: TComboBox;
    cbDCQualifier: TComboBox;
    cbDCContent: TComboBox;
    eWorkingDir: TEdit;
    eDCContent: TEdit;
    gbWorkingDir: TGroupBox;
    gbDublinCore: TGroupBox;
    ilIcons: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    lDCElement: TLabel;
    lDCQualifier: TLabel;
    lDCContent: TLabel;
    mDCXML: TSynEdit;
    pMenu: TPanel;
    pAddDCElements: TPanel;
    sddOpenWorkingDir: TSelectDirectoryDialog;
    sbtnAddDCElement: TSpeedButton;
    SynXMLSyn1: TSynXMLSyn;
    procedure bbtnExitClick(Sender: TObject);
    procedure bbtnSaveClick(Sender: TObject);
    procedure btnOpenWorkingDirClick(Sender: TObject);
    procedure cbDCElementSelect(Sender: TObject);
    procedure cbDCQualifierChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Label1Click(Sender: TObject);
    procedure sbtnAddDCElementClick(Sender: TObject);
  private
    fDCXML: TXMLDocument;
    fDCElement: TDOMNode;
    fFileTypes: TStringList;
    fDocTypes: TStringList;
    fXMLCore: TStringList;

    procedure ReadDCElements(); overload;
    procedure ReadDCElements(ANodeName: DOMString); overload;
    procedure GetDCValue(ANodeName: DOMString; AAttrName: String);
    procedure HideEDCContent();
    procedure ShowEDCContent();
    procedure HideCbDCContent();
    procedure ShowCbDCContent(aValue: String);
    procedure ClearAllFields();

    function CheckValue(var aMessage: String):boolean;
  public

  end;

var
  frmEditor: TfrmEditor;

implementation

const
     cDCXMLFile='dcvalues.xml';
     cFileTypes='filetypes';
     cDocTypes='doctypes';
     cDCXMLOutFile='dublin_core.xml';
     cDCXMLCore='xmlcore';

{$R *.lfm}

{ TfrmEditor }

procedure TfrmEditor.btnOpenWorkingDirClick(Sender: TObject);
begin
  if sddOpenWorkingDir.Execute then
     eWorkingDir.Text:=sddOpenWorkingDir.FileName;
end;

procedure TfrmEditor.cbDCElementSelect(Sender: TObject);
begin
  ReadDCElements(cbDCElement.Text);
end;

procedure TfrmEditor.cbDCQualifierChange(Sender: TObject);
begin
  if ((cbDCElement.Text='format') or (cbDCElement.Text='type')) and (cbDCQualifier.Text='none') then
     ShowCbDCContent(cbDCElement.Text)
  else if not eDCContent.Visible then ShowEDCContent();

  GetDCValue(cbDCElement.Text,cbDCQualifier.Text);
end;


procedure TfrmEditor.FormCreate(Sender: TObject);
begin
  ilIcons.GetBitmap(0,sbtnAddDCElement.Glyph);
  ilIcons.GetBitmap(1,bbtnExit.Glyph);
  ilIcons.GetBitmap(2,bbtnSave.Glyph);
  ilIcons.GetBitmap(3,btnOpenWorkingDir.Glyph);

  fFileTypes:=TStringList.Create;
  fDocTypes:=TStringList.Create;
  fXMLCore:=TStringList.Create;


  fFileTypes.LoadFromFile(cFileTypes);
  fDocTypes.LoadFromFile(cDocTypes);
  fXMLCore.LoadFromFile(cDCXMLCore);

  ReadXMLFile(fDCXML, cDCXMLFile);

  ReadDCElements();
end;

procedure TfrmEditor.Label1Click(Sender: TObject);
begin
  ShowMessage('Köszönöm, hogy ezt a programot választotta!');
end;

procedure TfrmEditor.sbtnAddDCElementClick(Sender: TObject);
var
  memoindex: Integer;
  memoline: String;
  message: String;
begin
  memoindex:=0;
  message:='';
  memoindex:=mDCXML.Lines.Count;
  memoline:='';
  if CheckValue(message) then
  begin
       memoline:='<dcvalue element="'+cbDCElement.Text+'" qualifier="'+cbDCQualifier.Text+'">';
       if eDCContent.Visible then
          memoline:=memoline+eDCContent.Text
       else
           memoline:=memoline+cbDCContent.Text;
       memoline:=memoline+'</dcvalue>';
       mDCXML.Lines.Insert(memoindex-1,memoline);
  end else Showmessage(message);
end;

procedure TfrmEditor.ReadDCElements();
begin
  try
    cbDCElement.Items.Clear;
    fDCElement:=fDCXML.DocumentElement.FirstChild;
    while Assigned(fDCElement) do
    begin
      cbDCElement.Items.Add(fDCElement.NodeName);
      fDCElement:=fDCElement.NextSibling;
    end;
  finally
    fDCElement.Free;
  end;
end;

procedure TfrmEditor.ReadDCElements(ANodeName: DOMString);
var
  i: integer;
begin
  cbDCQualifier.Clear;
  fDCElement:=fDCXML.DocumentElement.FindNode(aNodeName);
  with fDCElement.ChildNodes do
  try
     for i:=0 to (Count-1) do
     begin
         cbDCQualifier.Items.Add(Item[i].Attributes.Item[0].NodeValue);

     end;
  finally
     Free;
     cbDCQualifier.Text:='';
  end;
end;

procedure TfrmEditor.GetDCValue(ANodeName: DOMString; AAttrName: String);
var
  i: integer;
begin
  fDCElement:=fDCXML.DocumentElement.FindNode(ANodeName);
  with fDCElement.ChildNodes do
  try
     for i:=0 to (Count-1) do
     begin
         if Item[i].Attributes.Item[0].NodeValue=AAttrName then
            if Assigned(Item[i].FirstChild) then
                     eDCContent.Text:=Item[i].FirstChild.NodeValue
            else eDCContent.Text:='';
     end;
  finally
     Free;
  end;
end;

procedure TfrmEditor.HideEDCContent();
begin
  with eDCContent do
  begin
       Clear;
       Enabled:=False;
       Visible:=False;
  end;
end;

procedure TfrmEditor.ShowEDCContent();
begin
  HideCbDCContent();
  with eDCContent do
  begin
       Enabled:=True;
       Visible:=True;
  end;
end;

procedure TfrmEditor.HideCbDCContent();
begin
  with cbDCContent do
  begin
       Enabled:=False;
       Visible:=False;
       Items.Clear;
  end;
end;

procedure TfrmEditor.ShowCbDCContent(aValue: String);
begin
  HideEDCContent();
  with cbDCContent do
  begin
       Items.Clear;
       Enabled:=True;
       Visible:=True;
       if aValue='format' then
          Items.Text:=fFileTypes.Text;
       if aValue='type' then
          Items.Text:=fDocTypes.Text;
  end;
end;

procedure TfrmEditor.ClearAllFields();
var i: integer;
begin
  //ShowMessage('Mezők ürítése...');
  (*
    Kiürítjük a mezőket
  *)
  for i:=0 to ComponentCount-1 do
  begin
       if (Components[i] is TEdit) then
          (Components[i] as TEdit).Text:='';
       if (Components[i] is TComboBox) then
          if ((Components[i] as TComboBox).Tag=1) then
             (Components[i] as TComboBox).Text:=''
          else (Components[i] as TComboBox).Items.Clear;
       if (Components[i] is TSynEdit) then
       begin
          (Components[i] as TSynEdit).Lines.Text:='';
          (Components[i] as TSynEdit).Lines.Text:=fXMLCore.Text;
       end;
  end;
end;

function TfrmEditor.CheckValue(var aMessage: String): boolean;
begin
  aMessage:='';
  Result:=True;

  if cbDCElement.Text='' then
  begin
    Result:=False;
    aMessage:=aMessage+'Kérem válassza ki az Elemet!'+LineEnding;
  end;

  if cbDCQualifier.Text='' then
  begin
    Result:=False;
    aMessage:=aMessage+'Kérem válassza ki a Minősítőt!'+LineEnding;
  end;

  if eDCContent.Visible then
  begin
    if trim(eDCContent.Text)='' then
    begin
      Result:=False;
      aMessage:=aMessage+'Kérem töltse ki az Érték mezőt!'+LineEnding;
    end;
  end else
          if cbDCContent.Text='' then
          begin
            Result:=False;
            aMessage:=aMessage+'Kérem válassza ki a megfelelő Értéket!'+LineEnding;
          end;
end;

procedure TfrmEditor.bbtnSaveClick(Sender: TObject);
var
  OutPath: String;
begin
  if (trim(eWorkingDir.Text)<>'') and DirectoryExists(eWorkingDir.Text) then
  begin
     OutPath:=eWorkingDir.Text+DirectorySeparator+cDCXMLOutFile;
     try
       mDCXML.Lines.SaveToFile(OutPath);
     except
       ShowMessage('A mentés nem sikerült!');
     end;
     ShowMessage('A fájl mentésre került!');
     ClearAllFields();
  end
  else
      ShowMessage('Kérem válassza ki a Munkamappát!')
end;

procedure TfrmEditor.bbtnExitClick(Sender: TObject);
begin
  fDCXML.Free;

  fDocTypes.Clear;
  FreeAndNil(fDocTypes);

  fFileTypes.Clear;
  FreeAndNil(fFileTypes);

  fXMLCore.Clear;
  FreeAndNil(fXMLCore);

  Close;
end;

end.

