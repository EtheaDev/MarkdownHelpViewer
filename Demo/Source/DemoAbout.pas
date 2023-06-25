{******************************************************************************}
{                                                                              }
{       MarkDown Help Viewer: Demo  About Form                                 }
{       (Help Viewer and Help Interfaces for Markdown files)                   }
{                                                                              }
{       Copyright (c) 2023 (Ethea S.r.l.)                                      }
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
unit DemoAbout;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, ExtCtrls;

resourcestring
  Title_MDHViewer = 'Markdown Help Viewer';

const
  HELP_URL = 'https://github.com/EtheaDev/MarkdownHelpViewer';
type
  TFrmAbout = class(TForm)
    Panel1: TPanel;
    btnOK: TButton;
    TitleLabel: TLabel;
    MemoCopyRights: TMemo;
    LinkLabel1: TLinkLabel;
    IconImage: TImage;
    IconImage2: TImage;
    ButtonHrlp: TButton;
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure LinkLabel1LinkClick(Sender: TObject; const Link: string;
      LinkType: TSysLinkType);
    procedure ButtonHrlpClick(Sender: TObject);
  private
    FTitle: string;
    procedure SetTitle(const Value: string);
  public
    property Title: string read FTitle write SetTitle;
  end;

procedure ShowAboutForm(const ATitle: string);
procedure HideAboutForm;

implementation

uses
  ShellApi, MainForm;

{$R *.dfm}

function GetAboutForm: TFrmAbout;
var
  I: integer;
begin
  Result := Nil;
  for I := 0 to Screen.FormCount - 1 do
    if Screen.Forms[I].ClassType = TFrmAbout then
    begin
      Result := Screen.Forms[I] as TFrmAbout;
      exit;
    end;
end;

procedure HideAboutForm;
var
  LFrm: TFrmAbout;
begin
  LFrm := GetAboutForm;
  if LFrm <> nil then
    LFrm.Close;
end;

procedure ShowAboutForm(const ATitle: string);
var
  LFrm: TFrmAbout;
begin
  LFrm := GetAboutForm;
  if LFrm <> nil then
  begin
    LFrm.BringToFront;
    exit;
  end;

  LFrm := TFrmAbout.Create(nil);
  try
    LFrm.Title := ATitle;
    LFrm.ShowModal;
  finally
    LFrm.Free;
  end;
end;

procedure TFrmAbout.btnOKClick(Sender: TObject);
begin
  Close();
end;

procedure TFrmAbout.ButtonHrlpClick(Sender: TObject);
begin
  Application.HelpContext(1000);
end;

procedure TFrmAbout.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TFrmAbout.FormCreate(Sender: TObject);
begin
  TitleLabel.Font.Height := Round(TitleLabel.Font.Height * 1.6);
end;

procedure TFrmAbout.LinkLabel1LinkClick(Sender: TObject; const Link: string;
  LinkType: TSysLinkType);
begin
  ShellExecute(Handle, 'open', PChar(Link), nil, nil, SW_SHOW);
end;

procedure TFrmAbout.SetTitle(const Value: string);
begin
  FTitle := Value;
  Caption := FTitle;
  TitleLabel.Caption := Value;
end;

end.
