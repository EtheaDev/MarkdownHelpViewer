{******************************************************************************}
{                                                                              }
{       Viewer Components: design-time editors                                 }
{       (common to TMarkdownViewer and TEdgeMarkdownViewer)                    }
{                                                                              }
{       Copyright (c) 2023-2026 (Ethea S.r.l.)                                 }
{       Author: Carlo Barazzetta                                               }
{                                                                              }
{       https://github.com/EtheaDev/MarkdownHelpViewer                         }
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
unit MarkDownViewerDesign;

{$WARN UNIT_PLATFORM OFF}

interface

uses
  Classes
  , DesignIntf
  , DesignEditors
  , MarkDownViewerCommon
  ;

type
  //Editor of the TFolderName properties (ServerRoot, ScriptsFolder)
  TFolderNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  //Verbs of the viewers: "Load from file..." and the web help
  TMarkdownViewerEditorBase = class(TComponentEditor)
  protected
    procedure LoadFile(const AFileName: string); virtual; abstract;
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

procedure RegisterFolderNameEditor;

implementation

uses
  System.SysUtils
  , Vcl.FileCtrl
  , Winapi.ShellAPI
  , Winapi.Windows
  , Vcl.Dialogs
  ;

const
  HELP_URL = 'https://ethea.it/docs/markdowntools/';
  //NB: keep aligned with the FileVersion of the .dproj files
  PROJECT_VER = '3.0.0';
  MARKDOWN_FILES_DESC = 'Markdown text files';
  HTML_FILES_DESC = 'HTML text files';

procedure RegisterFolderNameEditor;
begin
  //TFolderName is a distinct type: the editor applies only to the folder properties
  RegisterPropertyEditor(TypeInfo(TFolderName), nil, '', TFolderNameProperty);
end;

{ TFolderNameProperty }

function TFolderNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

procedure TFolderNameProperty.Edit;
var
  LRoot: WideString;
  LFolder: string;
begin
  LRoot := GetValue;
  if SelectDirectory('Select a directory', LRoot, LFolder, [sdNewUI]) then
  begin
    SetValue(LFolder);
    Designer.Modified;
  end;
end;

{ TMarkdownViewerEditorBase }

procedure TMarkdownViewerEditorBase.ExecuteVerb(Index: Integer);
var
  LOpenDialog: TOpenDialog;
  LMarkdownMasks, LHTMLMasks: string;

  function GetFileMasks(const AFileExt: array of string;
    const ASeparator: Char = ';'): string;
  var
    I: Integer;
  begin
    Result := '';
    for I := Low(AFileExt) to High(AFileExt) do
    begin
      if I > 0 then
        Result := Result + ASeparator;
      Result := Result + '*' + AFileExt[I];
    end;
  end;

begin
  inherited;
  if Index = 0 then
  begin
    LOpenDialog := TOpenDialog.Create(nil);
    try
      LMarkdownMasks := GetFileMasks(AMarkdownFileExt);
      LHTMLMasks := GetFileMasks(AHTMLFileExt);
      LOpenDialog.Filter :=
        Format('%s (%s)|%s', [MARKDOWN_FILES_DESC, LMarkdownMasks, LMarkdownMasks]) + '|' +
        Format('%s (%s)|%s', [HTML_FILES_DESC, LHTMLMasks, LHTMLMasks]);
      if LOpenDialog.Execute then
      begin
        LoadFile(LOpenDialog.FileName);
        Designer.Modified;
      end;
    finally
      LOpenDialog.Free;
    end;
  end
  else if Index = 1 then
    ShellExecute(0, 'open', PChar(HELP_URL), nil, nil, SW_SHOWNORMAL);
end;

function TMarkdownViewerEditorBase.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Load from file...';
    1: Result := Format('Ver. %s - '#$00A9' Ethea S.r.l. - Open Web Help...', [PROJECT_VER]);
  else
    Result := '';
  end;
end;

function TMarkdownViewerEditorBase.GetVerbCount: Integer;
begin
  Result := 2;
end;

end.
