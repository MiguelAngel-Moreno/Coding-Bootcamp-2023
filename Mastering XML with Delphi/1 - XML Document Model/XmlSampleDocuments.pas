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

unit XmlSampleDocuments;

interface

uses
  Xml.xmldom;

const
  SPlainTextNoXml: DOMString =
    'Random text content, not XML';

  SXmlSampleEmptyDoc: DOMString =
    '<?xml version="1.0" encoding="UTF-8" ?>'#13#10;

  SXmlSamplePurchaseOrder: DOMString =
    '<?xml version="1.0" encoding="UTF-8" ?>'#13#10 +
    '<order>'#13#10 +
    '    <!-- This is a commentary node in XML -->'#13#10 +
    '    <message_header source="Ihre Firma"'#13#10 +
    '                    destination="20"'#13#10 +
    '                    accesscode=""'#13#10 +
    '                    test_flag="0"'#13#10 +
    '                    message_type="order"'#13#10 +
    '                    document_version_date="03.05.2005"'#13#10 +
    '                    document_version="2.30"'#13#10 +
    '                    generation_date="24.07.2018"'#13#10 +
    '                    generation_time="09:23:58"'#13#10 +
    '                    eMail_confirmation="info@muster.ch"/>'#13#10 +
    '    <order_header>'#13#10 +
    '        <order_reference>'#13#10 +
    '            <BORN>987654321</BORN>'#13#10 +
    '            <BORD>24.07.2018</BORD>'#13#10 +
    '        </order_reference>'#13#10 +
    '        <order_party>'#13#10 +
    '            <buyer_party>'#13#10 +
    '                <party_id>'#13#10 +
    '                    <LPAN>X123456</LPAN>'#13#10 +
    '                </party_id>'#13#10 +
    '            </buyer_party>'#13#10 +
    '            <ship_to_party>'#13#10 +
    '                <party_id>'#13#10 +
    '                    <LPAN>EndcustNr</LPAN>'#13#10 +
    '                </party_id>'#13#10 +
    '                <name_address>'#13#10 +
    '                    <PNA1>Hans Muster</PNA1>'#13#10 +
    '                    <PSTR>Auf dem Holzweg 1</PSTR>'#13#10 +
    '                    <PCTY>Musterhausen</PCTY>'#13#10 +
    '                    <PPOC>1234</PPOC>'#13#10 +
    '                    <PCTR>CH</PCTR>'#13#10 +
    '                </name_address>'#13#10 +
    '            </ship_to_party>'#13#10 +
    '        </order_party>'#13#10 +
    '        <order_information>'#13#10 +
    '            <additional_information>'#13#10 +
    '                <CCOM>yourCommission</CCOM>'#13#10 +
    '                <CREF>CustomerReference</CREF>'#13#10 +
    '                <ECUN/>'#13#10 +
    '                <LNGC>G</LNGC>'#13#10 +
    '                <SHCF>Y</SHCF>'#13#10 +
    '                <SFDF>N</SFDF>'#13#10 +
    '                <RDDT>24.07.2018</RDDT>'#13#10 +
    '                <CARC>0</CARC>'#13#10 +
    '            </additional_information>'#13#10 +
    '        </order_information>'#13#10 +
    '    </order_header>'#13#10 +
    '    <order_body>'#13#10 +
    '        <line_item_number>'#13#10 +
    '            <BLIN>1</BLIN>'#13#10 +
    '            <part_number>'#13#10 +
    '                <LITM>61797</LITM>'#13#10 +
    '            </part_number>'#13#10 +
    '            <quantity>'#13#10 +
    '                <quantity_value>'#13#10 +
    '                    <OQTY>1</OQTY>'#13#10 +
    '                </quantity_value>'#13#10 +
    '            </quantity>'#13#10 +
    '            <price>'#13#10 +
    '                <UPRC>59.85</UPRC>'#13#10 +
    '            </price>'#13#10 +
    '        </line_item_number>'#13#10 +
    '        <line_item_number>'#13#10 +
    '            <BLIN>2</BLIN>'#13#10 +
    '            <part_number>'#13#10 +
    '                <LITM>365512</LITM>'#13#10 +
    '                <MITM>yourCode2</MITM>'#13#10 +
    '            </part_number>'#13#10 +
    '            <additional_line_information>'#13#10 +
    '                <PTXT>TRUST GXT 545</PTXT>'#13#10 +
    '                <SBNR/>'#13#10 +
    '                <EUEA/>'#13#10 +
    '                <EUCN/>'#13#10 +
    '            </additional_line_information>'#13#10 +
    '            <quantity>'#13#10 +
    '                <quantity_value>'#13#10 +
    '                    <OQTY>2</OQTY>'#13#10 +
    '                </quantity_value>'#13#10 +
    '            </quantity>'#13#10 +
    '            <price>'#13#10 +
    '                <UPRC>53.06</UPRC>'#13#10 +
    '            </price>'#13#10 +
    '        </line_item_number>'#13#10 +
    '        <line_item_number>'#13#10 +
    '            <BLIN>3</BLIN>'#13#10 +
    '            <part_number>'#13#10 +
    '                <LITM>630122</LITM>'#13#10 +
    '                <MITM>yourCode3</MITM>'#13#10 +
    '            </part_number>'#13#10 +
    '            <additional_line_information>'#13#10 +
    '                <PTXT>NESCAFE Dolce Gusto Chococino</PTXT>'#13#10 +
    '                <SBNR/>'#13#10 +
    '                <EUEA/>'#13#10 +
    '                <EUCN/>'#13#10 +
    '            </additional_line_information>'#13#10 +
    '            <quantity>'#13#10 +
    '                <quantity_value>'#13#10 +
    '                    <OQTY>5</OQTY>'#13#10 +
    '                </quantity_value>'#13#10 +
    '            </quantity>'#13#10 +
    '            <price>'#13#10 +
    '                <UPRC>19.47</UPRC>'#13#10 +
    '            </price>'#13#10 +
    '        </line_item_number>'#13#10 +
    '    </order_body>'#13#10 +
    '    <addendum>'#13#10 +
    '      <![CDATA[' +
    'This is a free style character data node (CDATA) in XML.'#13#10 +
    'There is no predefined size limit of its content, nor predefined format.' +
    '      ]]>'#13#10 +
    '    </addendum>'#13#10 +
    '</order>'#13#10;


implementation

end.
