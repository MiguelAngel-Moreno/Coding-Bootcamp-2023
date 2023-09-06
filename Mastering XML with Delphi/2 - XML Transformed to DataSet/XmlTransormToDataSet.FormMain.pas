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

unit XmlTransormToDataSet.FormMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB, Xml.xmldom, Vcl.DBCtrls, Xml.XmlTransform,
  Vcl.BaseImageCollection, Vcl.ImageCollection, System.ImageList, Vcl.ImgList, Vcl.VirtualImageList,
  System.Actions, Vcl.ActnList, Datasnap.DBClient, Vcl.Grids, Vcl.DBGrids, Vcl.ComCtrls,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Buttons;


const
  DataDirectory                   = '..\..\..\Data';
  TransformationFileToDataPacket  = 'purchase_order_to_dp.xtr';
  TransformationFileToXml         = 'purchase_order_to_xml.xtr';


type
  TFormMain = class(TForm)
    Panel1: TPanel;
    ButtonOpenInvoiceFile: TSpeedButton;
    ButtonSaveDataSet: TSpeedButton;
    ButtonCloseDataSet: TSpeedButton;
    ButtonShowTransformErrors: TSpeedButton;
    RadioGroupStopOnErrors: TRadioGroup;
    StatusBar1: TStatusBar;
    Panel2: TPanel;
    DBGrid1: TDBGrid;
    ClientDataSet1: TClientDataSet;
    DataSource1: TDataSource;
    ActionList1: TActionList;
    ActionOpenOrderFile: TAction;
    ActionSaveDataSet: TAction;
    ActionCloseDataSet: TAction;
    ActionShowTransformErrors: TAction;
    VirtualImageList1: TVirtualImageList;
    ImageCollection1: TImageCollection;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    XMLTransform1: TXMLTransform;
    DBNavigator1: TDBNavigator;
    procedure ActionOpenOrderFileExecute(Sender: TObject);
    procedure ActionOpenOrderFileUpdate(Sender: TObject);
    procedure ActionSaveDataSetExecute(Sender: TObject);
    procedure ActionSaveDataSetUpdate(Sender: TObject);
    procedure ActionCloseDataSetExecute(Sender: TObject);
    procedure ActionCloseDataSetUpdate(Sender: TObject);
    procedure ActionShowTransformErrorsExecute(Sender: TObject);
    procedure ActionShowTransformErrorsUpdate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FBaseDir: String;
    FInvoiceFileName: String;
    FDataSetFileName: String;
    procedure UpdateStatusBar;
  public
    property BaseDir: String read FBaseDir;
    property InvoiceFileName: String read FInvoiceFileName;
    property DataSetFileName: String read FDataSetFileName;
  end;

var
  FormMain: TFormMain;


implementation

{$R *.dfm}


procedure TFormMain.FormCreate(Sender: TObject);
begin
  FBaseDir := ExpandFileName( ExtractFilePath( Application.ExeName ) + PathDelim + DataDirectory );
  OpenDialog1.InitialDir := FBaseDir;
  SaveDialog1.InitialDir := FBaseDir;
  ActiveControl := DBGrid1;
  RadioGroupStopOnErrors.Visible := False;
  UpdateStatusBar;
end;

procedure TFormMain.ActionCloseDataSetExecute(Sender: TObject);
begin
  ClientDataSet1.Close;
  UpdateStatusBar;
end;

procedure TFormMain.ActionCloseDataSetUpdate(Sender: TObject);
begin
  ActionCloseDataSet.Enabled := ClientDataSet1.Active;
end;

procedure TFormMain.ActionOpenOrderFileExecute(Sender: TObject);
begin
  if not OpenDialog1.Execute then
    exit;

  FInvoiceFileName := OpenDialog1.FileName;

  XMLTransform1.SourceXmlFile := FInvoiceFileName;
  XMLTransform1.TransformationFile := FBaseDir + PathDelim + TransformationFileToDataPacket;

  //XMLTransform1.StopOnError := TXMLTransformStopOnError( RadioGroupStopOnErrors.ItemIndex );

  ClientDataSet1.Close;
  ClientDataSet1.XMLData := XMLTransform1.Data;
  ClientDataSet1.Open;

  UpdateStatusBar;
end;

procedure TFormMain.ActionOpenOrderFileUpdate(Sender: TObject);
begin
  ActionOpenOrderFile.Enabled := True;
end;

procedure TFormMain.ActionSaveDataSetExecute(Sender: TObject);
var
  SL: TStringList;
begin
  if not SaveDialog1.Execute then
    Exit;

  FDataSetFileName := SaveDialog1.FileName;
  ClientDataSet1.SaveToFile( FDataSetFileName, dfXML );
  UpdateStatusBar;
end;

procedure TFormMain.ActionSaveDataSetUpdate(Sender: TObject);
begin
  ActionSaveDataSet.Enabled := ClientDataSet1.Active;
end;

procedure TFormMain.ActionShowTransformErrorsExecute(Sender: TObject);
const
  ErrorMessageFmt = 'XML Errors'#13'Critical: %d, Warning: %d';
begin
  //ShowMessage( XMLTransform1.TransformErrorsString );
end;

procedure TFormMain.ActionShowTransformErrorsUpdate(Sender: TObject);
begin
  ActionShowTransformErrors.Enabled := False;
  //ActionShowTransformErrors.Enabled := ( Length( XMLTransform1.TransformErrors ) > 0 );
end;

procedure TFormMain.UpdateStatusBar;
const
  StatusBarFmt = ' Critical Errors: %s  |  Warnings: %s  |  File: %s';
begin
  var S := '';
  //if ClientDataSet1.Active then
  //  S := Format( StatusBarFmt, [ IntToStr( XMLTransform1.TransformErrorsCount[ TXMLTransformErrorType.Critical ]),
  //                               IntToStr( XMLTransform1.TransformErrorsCount[ TXMLTransformErrorType.Warning ]),
  //                               InvoiceFileName ])     // ExtractFileName( InvoiceFileName )
  //else
  //  S := Format( StatusBarFmt, [ '--', '--', '(none)' ]);

  S := Format( StatusBarFmt, [ '--', '--', '(none)' ]);
  StatusBar1.SimpleText := S;
  StatusBar1.Refresh;
end;


end.
