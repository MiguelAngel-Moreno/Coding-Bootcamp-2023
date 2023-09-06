{--------------------------------------------------------------------------------}
{                                                                                }
{ MIT License                                                                    }
{                                                                                }
{ Copyright (c) 2023 Miguel Angel Moreno                                         }
{                                                                                }
{ Permission is hereby granted, free of charge, to any person obtaining a copy   }
{ of this software and associated documentation files (the "Software"), to deal  }
{ in the Software without restriction, including without limitation the rights   }
{ to use, copy, modify, merge, publish, distribute, sublicense, and/or sell      }
{ copies of the Software, and to permit persons to whom the Software is          }
{ furnished to do so, subject to the following conditions:                       }
{                                                                                }
{ The above copyright notice and this permission notice shall be included in all }
{ copies or substantial portions of the Software.                                }
{                                                                                }
{ THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR     }
{ IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,       }
{ FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE    }
{ AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER         }
{ LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,  }
{ OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE  }
{ SOFTWARE.                                                                      }
{                                                                                }
{--------------------------------------------------------------------------------}

unit XmlDocumentModel.FormMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Edit,
  Xml.xmldom, Xml.XMLIntf, Xml.XMLDoc, Xml.omnixmldom;

type
  TFormMain = class(TForm)
    Header: TToolBar;
    Footer: TToolBar;
    HeaderLabel: TLabel;
    EditOrderHeaderSource: TEdit;
    Label1: TLabel;
    ButtonLoad: TButton;
    XMLDocument1: TXMLDocument;
    Label2: TLabel;
    ButtonLoadTest: TButton;
    ButtonViewDocument: TButton;
    Label3: TLabel;
    Label4: TLabel;
    EditOrderHeaderVersionDate: TEdit;
    EditOrderHeaderReferenceBorn: TEdit;
    ButtonSave: TButton;
    procedure ButtonLoadClick(Sender: TObject);
    procedure ButtonLoadTestClick(Sender: TObject);
    procedure ButtonViewDocumentClick(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
  private
    FDataDirectory: String;
    procedure LoadXmlData;
    procedure LoadTestXmlData;
    procedure DoNothing;
  public
    property DataDirectory: String read FDataDirectory write FDataDirectory;
  end;

var
  FormMain: TFormMain;


implementation

{$R *.fmx}

uses
  XmlSampleDocuments,
  XmlTreeViewForms;


procedure TFormMain.ButtonLoadClick(Sender: TObject);
begin
  LoadXmlData;
end;

procedure TFormMain.ButtonLoadTestClick(Sender: TObject);
begin
  LoadTestXmlData;
end;

procedure TFormMain.ButtonSaveClick(Sender: TObject);
var
  LXmlRoot: IXMLNode;
begin

  DoNothing;

  LXmlRoot := XMLDocument1.DocumentElement;


  { Get from form editor - /order/message_header/@source }
  LXmlRoot.ChildNodes[ 'message_header' ]
            .Attributes[ 'source' ] := EditOrderHeaderSource.Text;


  { Get from form editor - /order/message_header/@document_version_date }
  LXmlRoot.ChildNodes[ 'message_header' ]
            .Attributes[ 'document_version_date' ] := EditOrderHeaderVersionDate.Text;


  { Get from form editor - /order/order_header/order_reference/BORN }
  LXmlRoot.ChildNodes[ 'order_header' ]
            .ChildNodes[ 'order_reference' ]
              .ChildNodes[ 'BORN' ].Text := EditOrderHeaderReferenceBorn.Text;

end;

procedure TFormMain.ButtonViewDocumentClick(Sender: TObject);
var
  S: String;
begin
  XMLDocument1.SaveToXML( S );
  XmlTreeViewForm.LoadFromXML( S );
  XmlTreeViewForm.ViewerTitle := 'purchase_order_sample.xml';
  XmlTreeViewForm.Show;
end;

procedure TFormMain.DoNothing;
begin
  { nothing }
end;



procedure TFormMain.LoadTestXmlData;
var
  LXmlNode1: IXMLNode;
  LXmlNode2: IXMLNode;
  LXmlNode3: IXMLNode;
  N1, N2, N3: String;
  V1, V2, V3, V4: String;
  B1, B2, B3: Boolean;
  T1, T2, T3: String;
  E: Exception;
begin


  DoNothing;    { <-- Set a breakpoint here just before assignment }


  { Level 1.a: Get the root 'element' node from inactive XML document (not 'open') }
  try
    LXmlNode1 := XMLDocument1.DocumentElement;                     { <-- Error: no active document }
    N1 := LXmlNode1.NodeName;
    V1 := LXmlNode1.NodeValue;
    B1 := LXmlNode1.IsTextElement;
  except
    on E: Exception do
      ShowMessage( 'Exception at Level 1.a: ' + E.Message );
  end;


  DoNothing;    { <-- Set a breakpoint here just before assignment }


  { Level 1.b: Get the root 'element' node from active XML document, no XML data loaded }
  try
    XMLDocument1.Active := True;                                    { Activate (open) XML document }

    LXmlNode1 := XMLDocument1.DocumentElement;
    N1 := LXmlNode1.NodeName;                        { <-- Error: Access violation - node is 'nil' }
    V1 := LXmlNode1.NodeValue;
    B1 := LXmlNode1.IsTextElement;
  except
    on E: Exception do
      ShowMessage( 'Exception at Level 1.b: ' + E.Message );
  end;


  DoNothing;    { <-- Set a breakpoint here just before assignment }


  { Level 1.c: Get the root 'element' node from XML document loaded with text data (no XML) }
  try
    XMLDocument1.LoadFromXML( 'Random text content, not XML' );     { Load text data from a string }
                                                                { The 'Load' activates the XML doc }
    LXmlNode1 := XMLDocument1.DocumentElement;
    N1 := LXmlNode1.NodeName;                        { <-- Error: Access violation - node is 'nil' }
    V1 := LXmlNode1.NodeValue;
    B1 := LXmlNode1.IsTextElement;
  except
    on E: Exception do
      ShowMessage( 'Exception at Level 1.c: ' + E.Message );
  end;


  DoNothing;    { <-- Set a breakpoint here just before assignment }


  { Level 1.d: Get the root 'element' node from XML document loaded with empty XML (only header) }
  try
    XMLDocument1.LoadFromXML( '<?xml version="1.0" encoding="UTF-8" ?>' );
                                                               { Load empty XML data from a string }
                                                               { The 'Load' activates the XML doc  }
    LXmlNode1 := XMLDocument1.DocumentElement;
    N1 := LXmlNode1.NodeName;                        { <-- Error: Access violation - node is 'nil' }
    V1 := LXmlNode1.NodeValue;
    B1 := LXmlNode1.IsTextElement;
  except
    on E: Exception do
      ShowMessage( 'Exception at Level 1.d: ' + E.Message );
  end;


  DoNothing;    { <-- Set a breakpoint here just before assignment }


  { Level 1.e: Get the root 'element' node from a document loaded with valid XML data }
  try
    XMLDocument1.LoadFromXML( SXmlSamplePurchaseOrder );        { Load XML data from a string }
  //XMLDocument1.LoadFromFile( XmlFileName);                    { Load XML data from a file }
  //XMLDocument1.LoadFromStream( XmlStream );                   { Load XML data from a stream }
                                                                { The 'Load' activates the XML doc }

    LXmlNode1 := XMLDocument1.DocumentElement;
    if Assigned( LXmlNode1 ) then                             { <-- OK, check if node is not 'nil' }
    begin
      N1 := LXmlNode1.NodeName;
      V1 := LXmlNode1.NodeValue;         { <-- Error: 'element' (pure) nodes do not have text data }
      B1 := LXmlNode1.IsTextElement;
      if B1 then
      begin
        T1 := LXmlNode1.Text;
        ShowMessage( '1.e: IsTextElement = True, Text = "' + T1 + '"' );
      end;
    end;
  except
    on E: Exception do
      ShowMessage( 'Exception at Level 1.e: ' + E.Message );
  end;






  DoNothing;    { <-- Set a breakpoint here just before assignment }


  { Level 2.a: Get an invalid child 'element' node with 'FindNode' }
  try
    LXmlNode2 := LXmlNode1.ChildNodes.FindNode( 'invalid_node_name1' );
    if Assigned( LXmlNode2 ) then                      { <-- OK, check node for 'nil' if not found }
    begin
      N2 := LXmlNode2.NodeName;
      V2 := LXmlNode2.NodeValue;
      B2 := LXmlNode2.IsTextElement;
    end;
  except
    on E: Exception do
      ShowMessage( 'Exception at Level 2.a: ' + E.Message );
  end;


  DoNothing;    { <-- Set a breakpoint here just before assignment }


  { Level 2.b: Get an invalid child 'element' node with 'Nodes' property (#1) }
  try
    XMLDocument1.Options := XMLDocument1.Options + [ doNodeAutoCreate ];      { <-- Default option }

    LXmlNode2 := LXmlNode1.ChildNodes.Nodes[ 'invalid_node_name2' ];
                                      { ^-- Error: 'Nodes' property creates a new node with the }
                                      {     invalid name because XML doc option 'doNodeAutoCreate' }
                                      {     is enabled by default }
    N2 := LXmlNode2.NodeName;
    //V2 := LXmlNode2.NodeValue;                            { Let's try this in the comming blocks }
    B2 := LXmlNode2.IsTextElement;
  except
    on E: Exception do
      ShowMessage( 'Exception at Level 2.b: ' + E.Message );
  end;


  DoNothing;    { <-- Set a breakpoint here just before assignment }


  { Level 2.c: Get an invalid child 'element' node with 'Nodes' property (#2) }
  try
    XMLDocument1.Options := XMLDocument1.Options - [ doNodeAutoCreate ];  { <-- Disable the option }

    LXmlNode2 := LXmlNode1.ChildNodes.Nodes[ 'invalid_node_name3' ];   { <-- Error: Node not found }

    N2 := LXmlNode2.NodeName;
    V2 := LXmlNode2.NodeValue;
    B2 := LXmlNode2.IsTextElement;
  except
    on E: Exception do
      ShowMessage( 'Exception at Level 2.c: ' + E.Message );
  end;


  DoNothing;    { <-- Set a breakpoint here just before assignment }


  { Level 2.d: Get child 'element' node 'message_header' with direct 'ChildNodes' property }
  try
    NullStrictConvert := True;                                                 { <-- Default value }

    LXmlNode2 := LXmlNode1.ChildNodes[ 'message_header' ];       { <-- 'Nodes' is default property }
  //LXmlNode2 := LXmlNode1.ChildNodes.Nodes[ 'message_header' ]; { <-- Same as this statement      }

    N2 := LXmlNode2.NodeName;
    V2 := LXmlNode2.NodeValue;                                           { <-- Error: 'Null' value }
    B2 := LXmlNode2.IsTextElement;
  except
    on E: Exception do
      ShowMessage( 'Exception at Level 2.d: ' + E.Message );
  end;


  DoNothing;    { <-- Set a breakpoint here just before assignment }


  { Level 2.e: Get child 'element' node 'message_header' with direct 'ChildNodes' property }
  try
    NullStrictConvert := False;                                         { <-- Change default value }

    LXmlNode2 := LXmlNode1.ChildNodes[ 'message_header' ];       { <-- 'Nodes' is default property }
  //LXmlNode2 := LXmlNode1.ChildNodes.Nodes[ 'message_header' ]; { <-- Same as this statement      }

    N2 := LXmlNode2.NodeName;
    V2 := LXmlNode2.NodeValue;                          { <-- OK: 'Null' converted to empty string }
    B2 := LXmlNode2.IsTextElement;
    if B2 then
    begin
      T2 := LXmlNode2.Text;
      ShowMessage( '2.e: IsTextElement = True, Text = "' + T2 + '"' );
    end;
  except
    on E: Exception do
      ShowMessage( 'Exception at Level 2.e: ' + E.Message );
  end;






  DoNothing;    { <-- Set a breakpoint here just before assignment }


  { Level 3.a: Get an invalid 'attribute' node with 'FindNode' }
  try
    LXmlNode3 := LXmlNode1.AttributeNodes.FindNode( 'invalid_attribute_name1' );
    if Assigned( LXmlNode3 ) then                 { <-- OK, check attribute for 'nil' if not found }
    begin
      N3 := LXmlNode3.NodeName;
      V3 := LXmlNode3.NodeValue;
      B3 := LXmlNode3.IsTextElement;
    end;
  except
    on E: Exception do
      ShowMessage( 'Exception at Level 3.a: ' + E.Message );
  end;


  DoNothing;    { <-- Set a breakpoint here just before assignment }


  { Level 3.b: Get an invalid 'attribute' node with 'ChildNodes' instead of 'AttributeNodes' }
  try
    LXmlNode3 := LXmlNode2.ChildNodes[ 'source' ];         { <-- Error: Attribute not found due to }
                                                           {     wrong propperty used (ChildNodes) }
    N3 := LXmlNode3.NodeName;
    V3 := LXmlNode3.NodeValue;
    B3 := LXmlNode3.IsTextElement;
  except
    on E: Exception do
      ShowMessage( 'Exception at Level 3.b: ' + E.Message );
  end;


  DoNothing;    { <-- Set a breakpoint here just before assignment }


  { Level 3.c: Get an invalid 'attribute' node with 'AttributeNodes' }
  try
    LXmlNode3 := LXmlNode2.AttributeNodes.Nodes[ 'invalid_attribute_name2' ];
                                  { ^-- OK: Correct property used (AttributeNodes) }
    N3 := LXmlNode3.NodeName;
    V3 := LXmlNode3.NodeValue;
    B3 := LXmlNode3.IsTextElement;
  except
    on E: Exception do
      ShowMessage( 'Exception at Level 3.c: ' + E.Message );
  end;


  DoNothing;    { <-- Set a breakpoint here just before assignment }


  { Level 3.d: Get an 'attribute' node with _empty value_ using 'AttributeNodes' (#1) }
  try
    NullStrictConvert := True;                                                 { <-- Default value }

    LXmlNode3 := LXmlNode2.AttributeNodes[ 'accesscode' ];       { <-- 'Nodes' is default property }
  //LXmlNode3 := LXmlNode2.AttributeNodes.Nodes[ 'accesscode' ]; { <-- Same as this expression     }
    N3 := LXmlNode3.NodeName;
    V3 := LXmlNode3.NodeValue;
    B3 := LXmlNode3.IsTextElement;
  except
    on E: Exception do
      ShowMessage( 'Exception at Level 3.d: ' + E.Message );
  end;


  DoNothing;    { <-- Set a breakpoint here just before assignment }


  { Level 3.e: Get an 'attribute' node with _empty value_ using 'AttributeNodes' (#2) }
  try
    NullStrictConvert := False;                                         { <-- Change default value }

    LXmlNode3 := LXmlNode2.AttributeNodes[ 'accesscode' ];       { <-- 'Nodes' is default property }
  //LXmlNode3 := LXmlNode2.AttributeNodes.Nodes[ 'accesscode' ]; { <-- Same as this expression     }
    N3 := LXmlNode3.NodeName;
    V3 := LXmlNode3.NodeValue;
    B3 := LXmlNode3.IsTextElement;
  except
    on E: Exception do
      ShowMessage( 'Exception at Level 3.e: ' + E.Message );
  end;


  DoNothing;    { <-- Set a breakpoint here just before assignment }


  { Level 3.f: Get the 'attribute' node named 'source' using 'AttributeNodes' }
  try

    LXmlNode3 := LXmlNode2.AttributeNodes[ 'source' ];           { <-- 'Nodes' is default property }
  //LXmlNode3 := LXmlNode2.AttributeNodes.Nodes[ 'source' ];     { <-- Same as this expression     }
    N3 := LXmlNode3.NodeName;
    V3 := LXmlNode3.NodeValue;
    B3 := LXmlNode3.IsTextElement;
    if B3 then
    begin
      T3 := LXmlNode3.Text;
      ShowMessage( '3.f: IsTextElement = True, Text = "' + T3 + '"' );
    end;
  except
    on E: Exception do
      ShowMessage( 'Exception at Level 3.f: ' + E.Message );
  end;






  DoNothing;    { <-- Set a breakpoint here just before assignment }


  { Level 4.a: Get an invalid 'attribute' value with 'Attributes' property (#1) }
  try
    XMLDocument1.Options := XMLDocument1.Options + [ doAttrNull ];      { <-- Default option }
    NullStrictConvert := True;                                          { <-- Default value }

    V4 := LXmlNode2.Attributes[ 'invalid_attribute_name3' ];    { <-- Get attribute value directly }
                                                                {     No node retrieved here, the  }
                                                                {     value is directly returned   }
  except
    on E: Exception do
      ShowMessage( 'Exception at Level 4.a: ' + E.Message );
  end;


  DoNothing;    { <-- Set a breakpoint here just before assignment }


  { Level 4.b: Get an invalid 'attribute' value with 'Attributes' property (#2) }
  try
    XMLDocument1.Options := XMLDocument1.Options + [ doAttrNull ];      { <-- Default option }
    NullStrictConvert := False;                                         { <-- Change default value }

    V4 := LXmlNode2.Attributes[ 'invalid_attribute_name3' ];    { <-- Get attribute value directly }
                                                                {     No node retrieved here, the  }
                                                                {     value is directly returned   }
  except
    on E: Exception do
      ShowMessage( 'Exception at Level 4.b: ' + E.Message );
  end;


  DoNothing;    { <-- Set a breakpoint here just before assignment }


  { Level 4.c: Get an invalid 'attribute' value with 'Attributes' property (#3) }
  try
    XMLDocument1.Options := XMLDocument1.Options - [ doAttrNull ];     { <-- Change default option }
    NullStrictConvert := True;                                         { <-- Default value }

    V4 := LXmlNode2.Attributes[ 'invalid_attribute_name3' ];    { <-- Get attribute value directly }
                                                                {     No node retrieved here, the  }
                                                                {     value is directly returned   }
  except
    on E: Exception do
      ShowMessage( 'Exception at Level 4.c: ' + E.Message );
  end;


  DoNothing;    { <-- Set a breakpoint here just before assignment }


  { Level 4.d: Get an empty 'attribute' value with 'Attributes' property (#1) }
  try
    XMLDocument1.Options := XMLDocument1.Options + [ doAttrNull ];      { <-- Default option }
    NullStrictConvert := True;                                          { <-- Default value }

    V4 := LXmlNode2.Attributes[ 'accesscode' ];                 { <-- Get attribute value directly }
                                                                {     No node retrieved here, the  }
                                                                {     value is directly returned   }
  except
    on E: Exception do
      ShowMessage( 'Exception at Level 4.d: ' + E.Message );
  end;


  DoNothing;    { <-- Set a breakpoint here just before assignment }


  { Level 4.e: Get an empty 'attribute' value with 'Attributes' property (#2) }
  try
    XMLDocument1.Options := XMLDocument1.Options + [ doAttrNull ];      { <-- Default option }
    NullStrictConvert := False;                                         { <-- Change default value }

    V4 := LXmlNode2.Attributes[ 'accesscode' ];                 { <-- Get attribute value directly }
                                                                {     No node retrieved here, the  }
                                                                {     value is directly returned   }
  except
    on E: Exception do
      ShowMessage( 'Exception at Level 4.e: ' + E.Message );
  end;


  DoNothing;    { <-- Set a breakpoint here just before assignment }


  { Level 4.f: Get an empty 'attribute' value with 'Attributes' property (#3) }
  try
    XMLDocument1.Options := XMLDocument1.Options - [ doAttrNull ];     { <-- Change default option }
    NullStrictConvert := True;                                         { <-- Default value }

    V4 := LXmlNode2.Attributes[ 'accesscode' ];                 { <-- Get attribute value directly }
                                                                {     No node retrieved here, the  }
                                                                {     value is directly returned   }
  except
    on E: Exception do
      ShowMessage( 'Exception at Level 4.f: ' + E.Message );
  end;






  { Update editor in form for '/order/message_header/@source' }
  EditOrderHeaderSource.Text := V3;

end;



procedure TFormMain.LoadXmlData;
var
  LXmlRoot: IXMLNode;
begin

  DoNothing;

  { Load the XML data from a string }
  XMLDocument1.LoadFromXML( SXmlSamplePurchaseOrder );


  { Set form editor - /order/message_header/@source }
  EditOrderHeaderSource.Text :=
          XMLDocument1.DocumentElement                    { <-- Use XMLDocument1.DocumentElement  }
                        .ChildNodes[ 'message_header' ]   {     directly                          }
                          .Attributes[ 'source' ];


  { Set form editor - /order/message_header/@document_version_date }
  LXmlRoot := XMLDocument1.DocumentElement;               { <-- Use a local var as a shortcut for }
                                                          {     XMLDocument1.DocumentElement      }

  EditOrderHeaderVersionDate.Text :=
          LXmlRoot.ChildNodes[ 'message_header' ]         { <-- Use LXmlRoot                      }
            .Attributes[ 'document_version_date' ];


  { Set form editor - /order/order_header/order_reference/BORN }
  EditOrderHeaderReferenceBorn.Text :=
          LXmlRoot.ChildNodes[ 'order_header' ]
            .ChildNodes[ 'order_reference' ]
              .ChildNodes[ 'BORN' ].Text;

end;

end.

