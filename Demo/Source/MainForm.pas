{******************************************************************************}
{                                                                              }
{       Markdown Help Viewer: Demo Main Form                                   }
{       (Help Viewer and Help Interfaces for Markdown files)                   }
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
unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, Data.DB, Vcl.DBCtrls,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Grids, Vcl.DBGrids, Datasnap.DBClient,
  Vcl.ComCtrls, Vcl.Mask, Vcl.Buttons,
  {$IF CompilerVersion >= 36}
  MarkDownEdgeViewerComponents;
  {$ELSE}
  MarkDownViewerComponents;
  {$IFEND}

type
  //The viewer of the embedded help: WebView2 from Delphi 12, else HTMLViewer
  {$IF CompilerVersion >= 36}
  TDemoMarkdownViewer = TEdgeMarkdownViewer;
  {$ELSE}
  TDemoMarkdownViewer = TMarkdownViewer;
  {$IFEND}

  TfmMain = class(TForm)
    MainMenu: TMainMenu;
    FileMenu: TMenuItem;
    NewMenuitem: TMenuItem;
    OpenMenuItem: TMenuItem;
    SaveMenuItem: TMenuItem;
    SaveAsMenuItem: TMenuItem;
    PrintMenuItem: TMenuItem;
    PrintSetupMenuItem: TMenuItem;
    ExitMenuItem: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    EditMenu: TMenuItem;
    UndoMenuItem: TMenuItem;
    CutMenuItem: TMenuItem;
    CopyMenuItem: TMenuItem;
    PasteMenuItem: TMenuItem;
    FindMenuItem: TMenuItem;
    ReplaceMenuItem: TMenuItem;
    N4: TMenuItem;
    N5: TMenuItem;
    AboutMenu: TMenuItem;
    PageControl: TPageControl;
    BrowseTab: TTabSheet;
    EditTab: TTabSheet;
    ClientDataSet: TClientDataSet;
    DataSource: TDataSource;
    ClientDataSetSpeciesNo: TFloatField;
    ClientDataSetCategory: TStringField;
    ClientDataSetCommon_Name: TStringField;
    ClientDataSetSpeciesName: TStringField;
    ClientDataSetLengthcm: TFloatField;
    ClientDataSetLength_In: TFloatField;
    ClientDataSetNotes: TMemoField;
    ClientDataSetGraphic: TGraphicField;
    HelpMenuItem: TMenuItem;
    DBGrid1: TDBGrid;
    BottomPanel: TPanel;
    DBMemo: TDBMemo;
    DBImage: TDBImage;
    Label1: TLabel;
    DBEdit1: TDBEdit;
    Label2: TLabel;
    DBEdit2: TDBEdit;
    Label3: TLabel;
    DBEdit3: TDBEdit;
    Label4: TLabel;
    DBEdit4: TDBEdit;
    Label5: TLabel;
    DBEdit5: TDBEdit;
    Label6: TLabel;
    DBEdit6: TDBEdit;
    Label7: TLabel;
    DBMemo1: TDBMemo;
    Label8: TLabel;
    DBImage1: TDBImage;
    DBImage2: TDBImage;
    Label9: TLabel;
    TopPanel: TPanel;
    DBNavigator1: TDBNavigator;
    TitleLabel: TLabel;
    AboutMenuItem: TMenuItem;
    ShowHelpEmbedded: TMenuItem;
    EmbeddedHelpPanel: TPanel;
    RightSplitter: TSplitter;
    HelpTitleLabel: TLabel;
    procedure MenuItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HelpMenuItemClick(Sender: TObject);
    procedure DataSourceStateChange(Sender: TObject);
    procedure AboutMenuItemClick(Sender: TObject);
    procedure ExitMenuItemClick(Sender: TObject);
    procedure ShowHelpEmbeddedClick(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
  private
    FMarkdownViewer: TDemoMarkdownViewer;
    procedure CreateMarkdownViewer;
    procedure ShowEmbeddedHelp;
  protected
  public
    { Public declarations }
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

uses
  UITypes
  {$IF CompilerVersion >= 36}
  , Winapi.EdgeUtils
  {$IFEND}
  , MarkdownHelpViewer
  , MarkDownViewerCommon
  , DemoAbout;

procedure TfmMain.AboutMenuItemClick(Sender: TObject);
begin
  ShowAboutForm(Application.Title);
end;

procedure TfmMain.DataSourceStateChange(Sender: TObject);
begin
  if DataSource.DataSet.State in [dsEdit, dsInsert] then
    PageControl.ActivePage := EditTab;
end;

procedure TfmMain.ExitMenuItemClick(Sender: TObject);
begin
  Close;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  ClientDataSet.LoadFromFile(ExtractFilePath(Application.ExeName)+
    '..\Data\Biolife.xml');
  PageControl.ActivePageIndex := 0;

  //Register "ServerRoot" folder for any MarkdownViewer
  RegisterMDViewerServerRoot(ExtractFilePath(Application.ExeName)+'..\Help');
  //The viewer of the embedded help is created at runtime
  CreateMarkdownViewer;

  HelpTitleLabel.Font.Style := [fsBold];

  Caption := Application.Title + ' - Copyright © 2023-2026 Ethea S.r.l.';
  TitleLabel.Font.Height := Round(TitleLabel.Font.Height * 1.5);
  ClientDataSet.Open;
  DBImage.DataField := 'Graphic';
  DBImage2.DataField := 'Graphic';
  ShowEmbeddedHelp;
end;

procedure TfmMain.HelpMenuItemClick(Sender: TObject);
begin
  Application.HelpKeyword('home');
end;

procedure TfmMain.MenuItemClick(Sender: TObject);
var
  LMenuItem: TMenuItem;
begin
  LMenuItem := Sender as TMenuItem;
  MessageDlg(Format('Menu Item Clicked: %s',[LMenuItem.Name]),
    TMsgDlgType.mtInformation, [mbOK, mbHelp], LMenuItem.HelpContext);
end;

procedure TfmMain.PageControlChange(Sender: TObject);
begin
  ShowEmbeddedHelp;
end;

procedure TfmMain.CreateMarkdownViewer;
begin
  FMarkdownViewer := TDemoMarkdownViewer.Create(Self);
  FMarkdownViewer.AlignWithMargins := True;
  FMarkdownViewer.Align := alClient;
  {$IF CompilerVersion >= 36}
  //WebView2 needs WebView2Loader.dll and the WebView2 runtime: the 32 and 64 bit
  //executables of the demo share the Bin folder, so the loader of the platform
  //is in Bin\Win32 or Bin\Win64
  SetWebView2Path(ExtractFilePath(Application.ExeName)+
    {$IFDEF WIN64}'Win64'{$ELSE}'Win32'{$ENDIF}+'\WebView2Loader.dll');
  if not TEdgeMarkdownViewer.EdgeAvailable then
    HelpTitleLabel.Caption := 'Instant Help: WebView2 not available'+sLineBreak+
      '(WebView2Loader.dll not found or WebView2 runtime not installed)';
  {$ELSE}
  FMarkdownViewer.RescalingImage := True;
  {$IFEND}
  FMarkdownViewer.Parent := EmbeddedHelpPanel;
end;

procedure TfmMain.ShowEmbeddedHelp;
begin
  FMarkdownViewer.HelpKeyword := PageControl.ActivePage.HelpKeyword;
end;

procedure TfmMain.ShowHelpEmbeddedClick(Sender: TObject);
begin
  if not EmbeddedHelpPanel.Visible then
    Width := Width + EmbeddedHelpPanel.Width
  else
    Width := Width - EmbeddedHelpPanel.Width;

  EmbeddedHelpPanel.Visible := ShowHelpEmbedded.checked;
  RightSplitter.Visible := ShowHelpEmbedded.checked;
  RightSplitter.Left := EmbeddedHelpPanel.Left - 10;
end;

initialization
{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}

end.
