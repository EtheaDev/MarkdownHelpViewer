{******************************************************************************}
{                                                                              }
{       Markdown Help Viewer: Splash Form                                      }
{       (Help Viewer and Help Interfaces for Markdown files)                   }
{                                                                              }
{       Copyright (c) 2023-2026 (Ethea S.r.l.)                                 }
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
unit MDHelpView.Splash;

interface

uses
  System.SysUtils, WinApi.Windows, WinApi.Messages, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Imaging.pngimage, Vcl.StdCtrls, Vcl.ExtCtrls,
  SVGIconImage;

type
  TSplashForm = class(TForm)
    Image1: TImage;
    SVGIconImage: TSVGIconImage;
    lbVersion: TLabel;
    lbLoad: TLabel;
    procedure FormShow(Sender: TObject);
  end;

var
  SplashForm: TSplashForm;

implementation

{$R *.dfm}

uses
  MDHelpView.About;

procedure TSplashForm.FormShow(Sender: TObject);
begin
  lbVersion.Caption := GetCurrentVersion(Application.ExeName);
end;

end.
