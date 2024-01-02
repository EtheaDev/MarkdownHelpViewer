{******************************************************************************}
{                                                                              }
{       Markdown Help Viewer: Hooks for TToolbar and TToolButton               }
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
unit MDHelpView.ComCtrls;

interface

uses
  Vcl.StyledButton
  , Vcl.StyledToolbar
  , System.Classes;

type
  //Hooking for TToolbar
  TToolBar = class(TStyledToolbar)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  //Hooking for TToolButton
  TToolButton = class(TStyledToolButton)
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{ TToolBar }

constructor TToolBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

{ TToolButton }

constructor TToolButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

end.