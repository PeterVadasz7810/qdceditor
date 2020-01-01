unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons, DOM, XMLRead;

type

  { TfrmEditor }

  TfrmEditor = class(TForm)
    bbtnSave: TBitBtn;
    bbtnExit: TBitBtn;
    btnOpenWorkingDir: TButton;
    cdDCElement: TComboBox;
    cbDCQualifier: TComboBox;
    cbDCContent: TComboBox;
    eWorkingDir: TEdit;
    eDCContent: TEdit;
    gbWorkingDir: TGroupBox;
    gbDublinCore: TGroupBox;
    lDCElement: TLabel;
    lDCQualifier: TLabel;
    lDCContent: TLabel;
    mDCXML: TMemo;
    pMenu: TPanel;
    pAddDCElements: TPanel;
    sddOpenWorkingDir: TSelectDirectoryDialog;
    sbtnAddDCElement: TSpeedButton;
    procedure bbtnExitClick(Sender: TObject);
    procedure bbtnSaveClick(Sender: TObject);
    procedure btnOpenWorkingDirClick(Sender: TObject);
    procedure cdDCElementSelect(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure sbtnAddDCElementClick(Sender: TObject);
  private
    fDCXML: TXMLDocument;
    fDCElement: TDOMNode;
    fFileTypes: TStringList;
    fDocTypes: TStringList;

    procedure ReadDCElements(); overload;
    procedure ReadDCElements(ANodeName: DOMString); overload;
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
     cDCXMLCore='<?xml version="1.0" encoding="utf-8" standalone="no"?>'+LineEnding+'<dublin_core schema="dc">'+LineEnding+'</dublin_core>';

{$R *.lfm}

{ TfrmEditor }

procedure TfrmEditor.btnOpenWorkingDirClick(Sender: TObject);
begin
  if sddOpenWorkingDir.Execute then
     eWorkingDir.Text:=sddOpenWorkingDir.FileName;
end;

procedure TfrmEditor.cdDCElementSelect(Sender: TObject);
begin
  ReadDCElements(cdDCElement.Text);

  if (cdDCElement.Text='format') or (cdDCElement.Text='type') then
     ShowCbDCContent(cdDCElement.Text)
  else if not eDCContent.Visible then ShowEDCContent();

end;

procedure TfrmEditor.FormCreate(Sender: TObject);
begin
  fFileTypes:=TStringList.Create;
  fDocTypes:=TStringList.Create;

  fFileTypes.LoadFromFile(cFileTypes);
  fDocTypes.LoadFromFile(cDocTypes);

  ReadXMLFile(fDCXML, cDCXMLFile);

  ReadDCElements();
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
       memoline:='<dcvalue element="'+cdDCElement.Text+'" qualifier="'+cbDCQualifier.Text+'">';
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
    cdDCElement.Items.Clear;
    fDCElement:=fDCXML.DocumentElement.FirstChild;
    while Assigned(fDCElement) do
    begin
      cdDCElement.Items.Add(fDCElement.NodeName);
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
         cbDCQualifier.Items.Add(Item[i].FirstChild.NodeValue);
  finally
     Free;
     cbDCQualifier.Text:='';
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
  ShowMessage('Mezők ürítése...');
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
       if (Components[i] is TMemo) then
       begin
          (Components[i] as TMemo).Lines.Text:='';
          (Components[i] as TMemo).Lines.Add(cDCXMLCore);
       end;
  end;
end;

function TfrmEditor.CheckValue(var aMessage: String): boolean;
begin
  aMessage:='';
  Result:=True;

  if cdDCElement.Text='' then
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
  Close;
end;

end.

