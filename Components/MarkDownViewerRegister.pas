{******************************************************************************}
{                                                                              }
{       Viewer Components Registration                                         }
{                                                                              }
{       Copyright (c) 2023 (Ethea S.r.l.)                                      }
{       Author: Carlo Barazzetta                                               }
{                                                                              }
{       https://github.com/EtheaDev/MarkdownHelpViewer                         }
{                                                                              }
{******************************************************************************}
{  Those Components requires Two Libraries:                                    }
{                                                                              }
{   Delphi Markdown                                                            }
{   https://github.com/grahamegrieve/delphi-markdown                           }
{   Copyright (c) 2011+, Health Intersections Pty Ltd All rights reserved.     }
{                                                                              }
{   HTMLViewer - https://github.com/BerndGabriel/HtmlViewer                    }
{   Copyright (c) 1995 - 2008 by L. David Baldwin                              }
{   opyright (c) 1995 - 2023 by Anders Melander (DitherUnit.pas)               }
{   Copyright (c) 1995 - 2023 by Ron Collins (HtmlGif1.pas)                    }
{   Copyright (c) 2008 - 2009 by Sebastian Zierer (Delphi 2009 Port)           }
{   opyright (c) 2008 - 2010 by Arvid Winkelsdorf (Fixes)                      }
{   Copyright (c) 2009 - 2023 by HtmlViewer Team                               }
{                                                                              }
{******************************************************************************}
{                                                                              }
{  Licensed under the Apache License, Version 2.0 (the "License");             }
{  you may not use this file except in compliance with the License.            }
{  You may obtain a copy of the License at                                     }
{                                                                              }
{      http://www.apache.org/licenses/LICENSE-2.0                              }
{                                                                              }
{  Unless required by applicable law or agreed to in writing, software         }
{  distributed under the License is distributed on an "AS IS" BASIS,           }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.    }
{  See the License for the specific language governing permissions and         }
{  limitations under the License.                                              }
{                                                                              }
{******************************************************************************}
unit MarkDownViewerRegister;

interface

uses
  Classes
  , DesignIntf
  , Designer
  , DesignEditors
  , VCLEditors
  , MarkDownViewerComponents
  ;

type
  TMarkDownViewerComponentEditor = class (TComponentEditor)
  private
    function GetViewer: TMarkDownViewer;
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
    procedure Edit; override;
  end;

procedure Register;

implementation

uses
  System.SysUtils
  , Winapi.ShellAPI
  , Winapi.Windows
  , Vcl.Dialogs
  ;

var
  AMarkDownFileExt: TArray<String>;
  AHTMLFileExt: TArray<String>;

{ TMarkDownViewerComponentEditor }

function TMarkDownViewerComponentEditor.GetViewer: TMarkDownViewer;
var
  LComponent: TPersistent;
begin
  Result := nil;
  LComponent := GetComponent;
  if LComponent is TMarkDownViewer then
    Result := TMarkDownViewer(LComponent);
end;

procedure TMarkDownViewerComponentEditor.Edit;
begin
  inherited;
end;

procedure TMarkDownViewerComponentEditor.ExecuteVerb(Index: Integer);
var
  LOpenDialog: TOpenDialog;
  LMarkDownMasks, LHTMLMasks: string;

  function GetFileMasks(const AFileExt: array of string;
    const ASeparator: Char = ';'): string;
  var
    I: Integer;
    LExt: string;
  begin
    for I := Low(AFileExt) to High(AFileExt) do
    begin
      LExt := AFileExt[I];
      if I > 0 then
        Result := Result + ASeparator;
      Result := Result + '*'+LExt;
    end;
  end;

begin
  inherited;
  if Index = 0 then
  begin
    LOpenDialog := TOpenDialog.Create(nil);
    try
      LMarkDownMasks := GetFileMasks(AMarkDownFileExt);
      LHTMLMasks := GetFileMasks(AHTMLFileExt);
      LOpenDialog.Filter :=
        Format('%s (%s)|%s', [MARKDOWN_FILES, LMarkDownMasks, LMarkDownMasks])+'|'+
        Format('%s (%s)|%s', [HTML_FILES, LHTMLMasks, LHTMLMasks]);

      if LOpenDialog.Execute then
      begin
        GetViewer.LoadFromFile(LOpenDialog.FileName);
        Designer.Modified;
      end;
    finally
      LOpenDialog.Free;
    end;
  end
  else if Index = 1 then
  ShellExecute(0, 'open',
    PChar('https://github.com/EtheaDev/MarkdownHelpViewer'), nil, nil, SW_SHOWNORMAL);
end;

function TMarkDownViewerComponentEditor.GetVerb(Index: Integer): string;
begin
  if Index = 0 then
    Result := 'Load from file...';
  if Index = 1 then
    Result := 'Project page on GitHub...';
end;

function TMarkDownViewerComponentEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

procedure Register;
begin
  RegisterComponents('MarkDown',
    [TMarkDownViewer]);

  RegisterComponentEditor(TMarkDownViewer, TMarkDownViewerComponentEditor);
end;

initialization
  SetLength(AMarkDownFileExt, 9);
  AMarkDownFileExt[0] := '.md';
  AMarkDownFileExt[1] := '.mkd';
  AMarkDownFileExt[2] := '.mdwn';
  AMarkDownFileExt[3] := '.mdown';
  AMarkDownFileExt[4] := '.mdtxt';
  AMarkDownFileExt[5] := '.mdtext';
  AMarkDownFileExt[6] := '.markdown';
  AMarkDownFileExt[7] := '.txt';
  AMarkDownFileExt[8] := '.text';

  SetLength(AHTMLFileExt, 2);
  AHTMLFileExt[0] := '.html';
  AHTMLFileExt[1] := '.htm';

end.
