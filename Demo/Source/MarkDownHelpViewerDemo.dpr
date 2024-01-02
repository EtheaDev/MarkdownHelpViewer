{******************************************************************************}
{                                                                              }
{       Markdown Help Viewer: Demo                                             }
{       (Help Viewer and Help Interfaces for Markdown files)                   }
{                                                                              }
{       Copyright (c) 2023-2024 (Ethea S.r.l.)                                 }
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
program MarkdownHelpViewerDemo;

uses
  Forms,
  MidasLib,
  SysUtils,
  MarkdownHelpViewer in '..\..\Source\AppInterface\MarkDownHelpViewer.pas',
  MainForm in 'MainForm.pas' {fmMain},
  DemoAbout in 'DemoAbout.pas' {FrmAbout},
  MarkDownViewerComponents in '..\..\Source\Components\MarkDownViewerComponents.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Markdown Help Viewer Demo App';
  Application.HelpFile := ExtractFilePath(Application.ExeName)+'..\Help\Home.md';
(* No need to add those lines if you have installed the Markdown HelpViewer
{$IFDEF WIN32}
  RegisterMDViewerLocation(ExtractFilePath(Application.ExeName)+
    '..\..\Bin32\MDHelpViewer.exe');
{$ELSE}
  RegisterMDViewerLocation(ExtractFilePath(Application.ExeName)+
    '..\..\Bin64\MDHelpViewer.exe');
{$ENDIF}
*)
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
