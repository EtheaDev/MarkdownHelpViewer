{******************************************************************************}
{                                                                              }
{       Markdown Help Viewer: Form Hook for translations                       }
{       (Help Viewer and Help Interfaces for Markdown files)                   }
{                                                                              }
{       Copyright (c) 2023-2025 (Ethea S.r.l.)                                 }
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
unit MDHelpView.FormsHookTrx;

interface

uses
  Vcl.Forms
  , System.Classes
  , CBMultiLanguage
  , CBMultiLanguageVCL
  ;

Type
  TFormHook = class;

  TFormHookClass = class of TFormHook;

  TFormHook = class(Vcl.Forms.TForm)
  private
    //Support for automatic translation of dfm
    FFormTranslator : TFormTranslatorVCL;
    function GetFormTranslator: TFormTranslatorVCL;
  protected
    procedure InitForm; virtual;

    procedure ReadState(Reader: TReader); override;
  public
    constructor Create(AOwner: TComponent); override;
    //Support for automatic translation of dfm
    property FormTranslator : TFormTranslatorVCL read GetFormTranslator;
  end;

implementation

uses
  Vcl.Controls
  ;

{ TFormHook }

constructor TFormHook.Create(AOwner: TComponent);
begin
  inherited;
  InitForm;
  FormTranslator.DoFormTranslation;
end;

function TFormHook.GetFormTranslator: TFormTranslatorVCL;
begin
  if not assigned(FFormTranslator) then
    FFormTranslator := TFormTranslatorVCL.Create(self);
  Result := FFormTranslator;
end;

procedure TFormHook.InitForm;
begin
  inherited;
  ;
end;

procedure TFormHook.ReadState(Reader: TReader);
begin
  //This method is launched once every "level" of form inheritance.
  //the name of the form indicates the file that must be used
  //for translations
  inherited;
  FormTranslator.AddForm(Self, Name);
end;

initialization
  RegisterClass(TFormHook);

end.
