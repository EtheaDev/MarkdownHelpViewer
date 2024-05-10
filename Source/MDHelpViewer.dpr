{******************************************************************************}
{                                                                              }
{       Markdown Help Viewer                                                   }
{       (Help Viewer and Help Interfaces for Markdown files)                   }
{                                                                              }
{       Copyright (c) 2023-2024 (Ethea S.r.l.)                                 }
{       Author: Carlo Barazzetta                                               }
{       Contributors: Nicolò Boccignone, Emanuele Biglia                       }
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
program MDHelpViewer;

uses
  MidasLib,
  Vcl.Forms,
  Vcl.Themes,
  Vcl.Styles,
  CBMultiLanguage,
  MDHelpView.FormsHookTrx in 'MDHelpView.FormsHookTrx.pas',
  MDHelpView.Main in 'MDHelpView.Main.pas' {MainForm},
  MDHelpView.Resources in 'MDHelpView.Resources.pas' {dmResources: TDataModule},
  MDHelpView.Settings in 'MDHelpView.Settings.pas',
  MDHelpView.SettingsForm in 'MDHelpView.SettingsForm.pas' {MDSettingsForm},
  MDHelpView.Registry in 'MDHelpView.Registry.pas',
  MDHelpView.Misc in 'MDHelpView.Misc.pas',
  MDHelpView.About in 'MDHelpView.About.pas' {FrmAbout},
  vmHtmlToPdf in 'vmHtmlToPdf.pas',
  MarkDownHelpViewer in 'AppInterface\MarkDownHelpViewer.pas',
  MarkDownViewerComponents in 'Components\MarkDownViewerComponents.pas',
  {$IFDEF STYLEDCOMPONENTS}
    {$IFDEF SKIA}
    Skia.Vcl.StyledTaskDialogAnimatedUnit in '..\Ext\StyledComponents\source\Skia.Vcl.StyledTaskDialogAnimatedUnit.pas' {StyledTaskDialogAnimated},
    {$ELSE}
    Vcl.StyledTaskDialogFormUnit in '..\Ext\StyledComponents\source\Vcl.StyledTaskDialogFormUnit.pas' {StyledTaskDialogForm},
    {$ENDIF}
  {$ENDIF}
  MDHelpView.Messages in 'MDHelpView.Messages.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.ActionUpdateDelay := 50;
  Application.Title := 'Markdown Help Viewer';
  //Uses System Style for border / shadow of Forms
  TStyleManager.FormBorderStyle := TStyleManager.TFormBorderStyle.fbsSystemStyle;
  Application.CreateForm(TdmResources, dmResources);
  Application.CreateForm(TMainForm, MainForm);
  Application.OnException := MainForm.ManageExceptions;
  Application.Run;
end.
