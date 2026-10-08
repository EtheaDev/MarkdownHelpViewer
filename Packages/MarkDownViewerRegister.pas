{******************************************************************************}
{                                                                              }
{       Viewer Components Registration                                         }
{                                                                              }
{       Copyright (c) 2023-2026 (Ethea S.r.l.)                                 }
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

{ Registration of TMarkdownViewer (HTMLViewer): package dclMarkDownViewer up to
  Delphi 11, optional package dclMarkDownViewerHTML from Delphi 12. }

interface

uses
  Classes
  , DesignIntf
  , MarkDownViewerDesign
  , MarkDownViewerComponents
  ;

type
  TMarkdownViewerComponentEditor = class(TMarkdownViewerEditorBase)
  protected
    procedure LoadFile(const AFileName: string); override;
  end;

procedure Register;

implementation

{ TMarkdownViewerComponentEditor }

procedure TMarkdownViewerComponentEditor.LoadFile(const AFileName: string);
begin
  if GetComponent is TMarkdownViewer then
    TMarkdownViewer(GetComponent).LoadFromFile(AFileName);
end;

procedure Register;
begin
  RegisterComponents('Markdown', [TMarkdownViewer]);
  RegisterFolderNameEditor;
  RegisterComponentEditor(TMarkdownViewer, TMarkdownViewerComponentEditor);
end;

end.
