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

unit XmlTreeViewForms;

interface

uses
  System.SysUtils, System.Types, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts, FMX.TreeView,
  Xml.xmldom, Xml.omnixmldom, Xml.XMLDoc, Xml.XMLIntf, FMX.StdCtrls, FMX.Controls.Presentation,
  FMX.TabControl, System.Actions, FMX.ActnList, FMX.ScrollBox, FMX.Memo, FMX.Memo.Types;


  {--------------------}
  {  TXmlTreeViewForm  }
  {--------------------}

type
  TXmlTreeViewForm = class( TForm )
    TreeView1: TTreeView;
    XMLDocument1: TXMLDocument;
    TreeViewItem1: TTreeViewItem;
    TreeViewItem2: TTreeViewItem;
    TreeViewItem3: TTreeViewItem;
    TreeViewItem4: TTreeViewItem;
    TreeViewItem5: TTreeViewItem;
    TreeViewItem6: TTreeViewItem;
    TreeViewItem7: TTreeViewItem;
    TabControlMain: TTabControl;
    TabItemTreeView: TTabItem;
    TabItemXmlView: TTabItem;
    MemoXmlView: TMemo;
    ActionList1: TActionList;
    NextTabAction: TNextTabAction;
    PreviousTabAction: TPreviousTabAction;
    ToolBarTop: TToolBar;
    LabelViewerTitle: TLabel;
    XmlTextButton: TSpeedButton;
    TreeViewButton: TSpeedButton;
    procedure FormCreate( Sender: TObject );
    procedure TabControlMainChange( Sender: TObject );
    procedure FormShow( Sender: TObject );
  private
    FFormTitle: String;
    FViewerTitle: String;
    FFontSize: Single;
    FFontFamily: String;
    procedure BuildTreeView;
    procedure SetViewerTitle(const Value: String);
    procedure SetFontSize(const Value: Single);
    procedure SetFontFamily(const Value: String);
  public
    procedure LoadFromFile( AFile: String );
    procedure LoadFromStream( AStream: TStream );
    procedure LoadFromXML( AXml: String );
    property ViewerTitle: String read FViewerTitle write SetViewerTitle;
    property FontFamily: String read FFontFamily write SetFontFamily;
    property FontSize: Single read FFontSize write SetFontSize;
  end;

var
  XmlTreeViewForm: TXmlTreeViewForm;


implementation

uses
  System.UITypes;

{$r *.fmx}


  {--------------------}
  {  TXmlTreeViewForm  }
  {--------------------}

const
  DarkBlue   = $FF423A8E;
  LightBlue  = $FF5671C6;
  DarkGreen  = $FF378D3D;
  DarkYellow = $FF9E9258;
  DarkRed    = $FFBE5F5F;
  DarkBrown  = $FFCD7415;


procedure TXmlTreeViewForm.BuildTreeView;
var
  LRemoveStyleSettings:  TStyledSettings;

  procedure AddNodes( AXmlNode: IXMLNode; AParentTreeNode: TTreeViewItem );
  var
    LTreeItem: TTreeViewItem;
    I: Integer;
  begin
    LTreeItem := TTreeViewItem.Create( TreeView1 );
    if ( AParentTreeNode = nil ) then
      LTreeItem.Parent := TreeView1
    else
      LTreeItem.Parent := AParentTreeNode;

    LRemoveStyleSettings := [ TStyledSetting.FontColor, TStyledSetting.Size ];
    case AXmlNode.NodeType of

      ntReserved,
      ntElement,
      ntEntityRef,
      ntEntity,
      ntProcessingInstr,
      ntDocument,
      ntDocType,
      ntDocFragment,
      ntNotation:
        begin
          LTreeItem.Text := AXmlNode.NodeName;
          LTreeItem.StyledSettings := LTreeItem.StyledSettings - LRemoveStyleSettings;
          LTreeItem.TextSettings.Font.Size := FontSize;
          LTreeItem.TextSettings.Font.Style := [ TFontStyle.fsBold ];
          LTreeItem.TextSettings.FontColor := DarkBlue;
        end;

      ntAttribute:
        begin
          LTreeItem.Text := AXmlNode.NodeName + ' = ' + AXmlNode.Text;
          LTreeItem.StyledSettings := LTreeItem.StyledSettings - LRemoveStyleSettings;
          LTreeItem.TextSettings.Font.Size := FontSize;
          LTreeItem.TextSettings.Font.Style := [ ];
          LTreeItem.TextSettings.FontColor := DarkRed;
        end;

      ntText:
        begin
          LTreeItem.Text := AXmlNode.Text;
          LTreeItem.StyledSettings := LTreeItem.StyledSettings - LRemoveStyleSettings;
          LTreeItem.TextSettings.Font.Size := FontSize;
          LTreeItem.TextSettings.Font.Style := [ TFontStyle.fsBold ];
          LTreeItem.TextSettings.FontColor := DarkYellow;
        end;

      ntCData:
        begin
          LTreeItem.Text := '[cdata] ' + AXmlNode.Text;
          LTreeItem.StyledSettings := LTreeItem.StyledSettings - LRemoveStyleSettings;
          LTreeItem.TextSettings.Font.Size := FontSize;
          LTreeItem.TextSettings.Font.Style := [ TFontStyle.fsBold ];
          LTreeItem.TextSettings.FontColor := DarkBrown;
        end;

      ntComment:
        begin
          LTreeItem.Text := '[comment] ' + AXmlNode.Text;
          LTreeItem.StyledSettings := LTreeItem.StyledSettings - LRemoveStyleSettings;
          LTreeItem.TextSettings.Font.Size := FontSize;
          LTreeItem.TextSettings.Font.Style := [ TFontStyle.fsBold ];
          LTreeItem.TextSettings.FontColor := DarkGreen;
        end;
    end;

    for I := 0 to AXmlNode.AttributeNodes.Count - 1 do
      AddNodes( AXmlNode.AttributeNodes.Nodes[ I ], LTreeItem );

    for I := 0 to AXmlNode.ChildNodes.Count - 1 do
      AddNodes( AXmlNode.ChildNodes.Nodes[ I ], LTreeItem );

    LTreeItem.IsExpanded := True;
  end;

begin
  TreeView1.BeginUpdate;
  TreeView1.Clear;
  AddNodes( XMLDocument1.DocumentElement, {ParentTreeNode} nil );
  TreeView1.Items[ 0 ].Select;
  TreeView1.EndUpdate;
  MemoXmlView.Font.Family := FontFamily;
  MemoXmlView.FontColor := DarkBlue;
  XMLDocument1.Active := False;
end;

procedure TXmlTreeViewForm.FormCreate( Sender: TObject );
begin
  TabControlMain.TabPosition := TTabPosition.None;
{$if Defined(MSWINDOWS)}
  FontFamily := 'Consolas';
  FontSize := 14;
{$elseif Defined(MACOS)}
  FontFamily := 'Menlo';
  FontSize := 14;
{$elseif Defined(ANDROID)}
  FontFamily := 'monospace';
  FontSize := 12;
{$endif}
end;

procedure TXmlTreeViewForm.FormShow( Sender: TObject );
begin
  TabControlMain.ActiveTab := TabItemTreeView;
  TabControlMainChange( nil );
end;

procedure TXmlTreeViewForm.LoadFromFile( AFile: String );
begin
  XMLDocument1.LoadFromFile( AFile );
  MemoXmlView.Lines.Text := FormatXMLData( XMLDocument1.XML.Text );
  BuildTreeView;
end;

procedure TXmlTreeViewForm.LoadFromStream( AStream: TStream );
begin
  XMLDocument1.LoadFromStream( AStream );
  MemoXmlView.Lines.Text := FormatXMLData( XMLDocument1.XML.Text );
  BuildTreeView;
end;

procedure TXmlTreeViewForm.LoadFromXML( AXml: String );
begin
  XMLDocument1.LoadFromXML( AXml );
  MemoXmlView.Lines.Text := FormatXMLData( XMLDocument1.XML.Text );
  BuildTreeView;
end;

procedure TXmlTreeViewForm.SetFontFamily(const Value: String);
begin
  FFontFamily := Value;
  MemoXmlView.TextSettings.Font.Family := FFontFamily;
end;

procedure TXmlTreeViewForm.SetFontSize( const Value: Single );
begin
  FFontSize := Value;
  MemoXmlView.TextSettings.Font.Size := FFontSize;
end;

procedure TXmlTreeViewForm.SetViewerTitle( const Value: String );
begin
  FViewerTitle := Value;
  LabelViewerTitle.Text := Format( 'XML Viewer - [ %s ]', [ Value ]);
end;

procedure TXmlTreeViewForm.TabControlMainChange( Sender: TObject );
begin
  if ( TabControlMain.ActiveTab = TabItemTreeView ) then
  begin
    TreeViewButton.Visible := False;
    XmlTextButton.Visible := True;
  end
  else
  begin
    TreeViewButton.Visible := True;
    XmlTextButton.Visible := False;
  end;
end;

end.
