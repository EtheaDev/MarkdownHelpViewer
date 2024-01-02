{******************************************************************************}
{                                                                              }
{       Markdown Help Viewer: Settings Class                                   }
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
unit MDHelpView.Settings;

interface

uses
  System.SysUtils
  , System.Classes
  , VCL.Graphics
  , System.Generics.Collections
  , System.UITypes
  , MarkdownProcessor
  , MarkdownUtils
  , IniFiles
  , CBMultiLanguage
  ;

const
  MaxfontSize = 30;
  MinfontSize = 8;

  Settings_Folder = 'MarkdownHelpViewer';
  MAIN_WINDOW = 'MainWindow';
  HTML_VIEWER = 'HtmlViever';
  VCL_STYLE = 'VCLStyle';
  PDF_SETTINGS = 'PDFPageSettins';

type
  TThemeSelection = (tsAsWindows, tsDarkTheme, tsLightTheme);
  TThemeType = (ttLight, ttDark);

  //Class to register Theme attributes (like dark or light)
  TThemeAttribute = class
    StyleName: String;
    ThemeType: TThemeType;

  //function to get Theme Attributes
  class function GetStyleAttributes(const AStyleName: string;
    out AThemeAttribute: TThemeAttribute): Boolean;
  private
  end;

  TPDFPageSettings = record
    PrintOrientation: TPrinterOrientation;
    PaperSize: Integer;
    MarginTop: Integer;
    MarginBottom: Integer;
    MarginLeft: Integer;
    MarginRight: Integer;
  end;

  TViewerSettings = class
  private
    FPageControlSize: Integer;
    FPageControlVisible: Boolean;
    FVCLStyleName: string;

    FActivePageIndex: Integer;
    FThemeSelection: TThemeSelection;
    FGUILanguage: TAppLanguage;
    FHTMLFontSize: Integer;
    FHTMLFontName: string;
    FRescalingImage: Boolean;
    FProcessorDialect: TMarkdownProcessorDialect;
    FWindowState: TWindowState;
    FWindowTop: Integer;
    FWindowHeight: Integer;
    FWindowLeft: Integer;
    FWindowWidth: Integer;
    FShowToolbarCaptions: Boolean;
    FUseColoredIcons: Boolean;
    FDownloadFromWEB: Boolean;
    procedure SetDownloadFromWEB(const AValue: Boolean);
    function GetUseDarkStyle: Boolean;
    function GetButtonTextColor: TColor;
    class function GetSettingsFileName: string; static;
    procedure SetRescalingImage(const AValue: Boolean);
    procedure SetProcessorDialect(const AValue: TMarkdownProcessorDialect);
    procedure SetWindowState(const AValue: TWindowState);
    procedure SetWindowHeight(const AValue: Integer);
    procedure SetWindowLeft(const AValue: Integer);
    procedure SetWindowTop(const AValue: Integer);
    procedure SetWindowWidth(const AValue: Integer);
    procedure SetShowToolbarCaptions(const AValue: Boolean);
    procedure SetUseColoredIcons(const AValue: Boolean);
  protected
    FIniFile: TIniFile;
  public
    PDFPageSettings: TPDFPageSettings;
    CurrentFileName: TFileName;
    CurrentIndexFileName: TFileName;
    constructor CreateSettings;
    destructor Destroy; override;
    property DownloadFromWEB: Boolean read FDownloadFromWEB write SetDownloadFromWEB;

    class var FSettingsFileName: string;
    class var FSettingsPath: string;
    class property SettingsFileName: string read GetSettingsFileName;

    procedure UpdateSettings(const AHTMLFontName: string;
      AHTMLFontSize: Integer; AEditorVisible: Boolean);
    procedure ReadSettings; virtual;
    procedure WriteSettings; virtual;

    property UseDarkStyle: Boolean read GetUseDarkStyle;
    property ButtonTextColor: TColor read GetButtonTextColor;

    property HTMLFontSize: Integer read FHTMLFontSize write FHTMLFontSize;
    property HTMLFontName: string read FHTMLFontName write FHTMLFontName;
    property VCLStyleName: string read FVCLStyleName write FVCLStyleName;
    property PageControlVisible: Boolean read FPageControlVisible write FPageControlVisible;
    property PageControlSize: integer read FPageControlSize write FPageControlSize;
    property RescalingImage: Boolean read FRescalingImage write SetRescalingImage;
    property ActivePageIndex: Integer read FActivePageIndex write FActivePageIndex;
    property ThemeSelection: TThemeSelection read FThemeSelection write FThemeSelection;
    property GUILanguage: TAppLanguage read FGUILanguage write FGUILanguage;
    property ProcessorDialect: TMarkdownProcessorDialect read FProcessorDialect write SetProcessorDialect;

    property ShowToolbarCaptions: Boolean read FShowToolbarCaptions write SetShowToolbarCaptions;
    property UseColoredIcons: Boolean read FUseColoredIcons write SetUseColoredIcons;

    property WindowState: TWindowState read FWindowState write SetWindowState;
    property WindowWidth: Integer read FWindowWidth write SetWindowWidth;
    property WindowTop: Integer read FWindowTop write SetWindowTop;
    property WindowLeft: Integer read FWindowLeft write SetWindowLeft;
    property WindowHeight: Integer read FWindowHeight write SetWindowHeight;
  end;

implementation

uses
  Vcl.Controls
  , System.Types
  , System.TypInfo
  , System.Rtti
  , System.StrUtils
  , System.IOUtils
  , Winapi.ShlObj
  , Winapi.Windows
{$IFNDEF DISABLE_STYLES}
  , Vcl.Themes
{$ENDIF}
//  , uLogExcept
  , MDHelpView.Registry
  , MDHelpView.Misc
//  , SynEdit
  , Winapi.Messages
  ;

const
  LAST_OPENED_SECTION = 'LastOpened';
  FILES_OPENED_SECTION = 'FilesOpened';
  EDITOPTION_GUTTER = 'EditorOptions_Gutter';
  EDITOPTION_RIGHTEDGE = 'EditorOptions_RightEdge';
  EDITOPTION_LINESPACING = 'EditorOptions_LineSpacing';
  EDITOPTION_BOOKMARK = 'EditorOptions_Bookmark';
  EDITOPTION_OPTIONS = 'EditorOptions_Options';

var
  ThemeAttributes: TList<TThemeAttribute>;

procedure InitDefaultThemesAttributes;

  procedure RegisterThemeAttributes(
    const AVCLStyleName: string;
    const AThemeType: TThemeType);
  var
    LThemeAttribute: TThemeAttribute;

    procedure UpdateThemeAttributes;
    begin
      LThemeAttribute.StyleName := AVCLStyleName;
      LThemeAttribute.ThemeType := AThemeType;
    end;

  begin
    for LThemeAttribute in ThemeAttributes do
    begin
      if SameText(LThemeAttribute.StyleName, AVCLStyleName) then
      begin
        UpdateThemeAttributes;
        Exit; //Found: exit
      end;
    end;
    //not found
    LThemeAttribute := TThemeAttribute.Create;
    ThemeAttributes.Add(LThemeAttribute);
    UpdateThemeAttributes;
  end;

begin
  ThemeAttributes := TList<TThemeAttribute>.Create;

{$IFNDEF DISABLE_STYLES}
  if StyleServices.Enabled then
  begin
    //High-DPI Themes (Delphi 11.0)
    RegisterThemeAttributes('Windows'               ,ttLight );
    RegisterThemeAttributes('Aqua Light Slate'      ,ttLight );
    RegisterThemeAttributes('Copper'                ,ttLight );
    RegisterThemeAttributes('CopperDark'            ,ttDark  );
    RegisterThemeAttributes('Coral'                 ,ttLight );
    RegisterThemeAttributes('Diamond'               ,ttLight );
    RegisterThemeAttributes('Emerald'               ,ttLight );
    RegisterThemeAttributes('Flat UI Light'         ,ttLight );
    RegisterThemeAttributes('Glow'                  ,ttDark  );
    RegisterThemeAttributes('Iceberg Classico'      ,ttLight );
    RegisterThemeAttributes('Lavender Classico'     ,ttLight );
    RegisterThemeAttributes('Sky'                   ,ttLight );
    RegisterThemeAttributes('Slate Classico'        ,ttLight );
    RegisterThemeAttributes('Sterling'              ,ttLight );
    RegisterThemeAttributes('Tablet Dark'           ,ttDark  );
    RegisterThemeAttributes('Tablet Light'          ,ttLight );
    RegisterThemeAttributes('Windows10'             ,ttLight );
    RegisterThemeAttributes('Windows10 Blue'        ,ttDark  );
    RegisterThemeAttributes('Windows10 Dark'        ,ttDark  );
    RegisterThemeAttributes('Windows10 Green'       ,ttDark  );
    RegisterThemeAttributes('Windows10 Purple'      ,ttDark  );
    RegisterThemeAttributes('Windows10 SlateGray'   ,ttDark  );
    RegisterThemeAttributes('Glossy'                ,ttDark  );
    RegisterThemeAttributes('Windows10 BlackPearl'  ,ttDark  );
    RegisterThemeAttributes('Windows10 Blue Whale'  ,ttDark  );
    RegisterThemeAttributes('Windows10 Clear Day'   ,ttLight );
    RegisterThemeAttributes('Windows10 Malibu'      ,ttLight );
    RegisterThemeAttributes('Windows11 Modern Dark' ,ttDark  );
    RegisterThemeAttributes('Windows11 Modern Light',ttLight );
  end;
{$ELSE}
    RegisterThemeAttributes('Windows'            ,ttLight );
{$ENDIF}
end;

{ TViewerSettings }

constructor TViewerSettings.CreateSettings;
var
  LSettingsFileName: TFileName;
begin
  inherited Create;
  LSettingsFileName := IncludeTrailingPathDelimiter(
      GetSpecialFolder(CSIDL_APPDATA)) +Settings_Folder+'\ViewerSettings.ini';
  FIniFile := TIniFile.Create(LSettingsFileName);
  FSettingsFileName := LSettingsFileName;
  FSettingsPath := ExtractFilePath(LSettingsFileName);
  System.SysUtils.ForceDirectories(FSettingsPath);

  ReadSettings;
end;

destructor TViewerSettings.Destroy;
begin
  FIniFile.UpdateFile;
  FreeAndNil(FIniFile);
  inherited;
end;

function TViewerSettings.GetButtonTextColor: TColor;
{$IFNDEF DISABLE_STYLES}
var
  LStyleServices: TCustomStyleServices;
{$ENDIF}
begin
{$IFNDEF DISABLE_STYLES}
  LStyleServices := TStyleManager.Style[Self.VCLStyleName];
  if Assigned(LStyleServices) then
    Result := LStyleServices.GetStyleFontColor(sfButtonTextNormal)
  else
    Result := clBtnText;
{$ELSE}
  Result := clBtnText;
{$ENDIF}
end;

class function TViewerSettings.GetSettingsFileName: string;
begin
  Result := FSettingsFileName;
end;

function TViewerSettings.GetUseDarkStyle: Boolean;
begin
  //Select Style by default on Actual Windows Theme
  if FThemeSelection = tsAsWindows then
  begin
    Result := not IsWindowsAppThemeLight;
  end
  else
    Result := FThemeSelection = tsDarkTheme;
end;

procedure TViewerSettings.ReadSettings;
var
  LLanguage: string;
  LDefaultLanguage: TAppLanguage;
begin
  PageControlVisible := FIniFile.ReadBool(MAIN_WINDOW, 'PageControlVisible', True);
  PageControlSize := FIniFile.ReadInteger(MAIN_WINDOW, 'PageControlSize', 300);
  ActivePageIndex := FIniFile.ReadInteger(MAIN_WINDOW, 'ActivePageIndex', 0);
  WindowLeft := FIniFile.ReadInteger(MAIN_WINDOW, 'WindowLeft', 200);
  WindowTop := FIniFile.ReadInteger(MAIN_WINDOW, 'WindowTop', 200);
  WindowWidth := FIniFile.ReadInteger(MAIN_WINDOW, 'WindowWidth', 800);
  WindowHeight := FIniFile.ReadInteger(MAIN_WINDOW, 'WindowHeight', 550);
  WindowState := TWindowState(FIniFile.ReadInteger(MAIN_WINDOW, 'WindowState',
    Ord(TWindowState.wsNormal)));
  ShowToolbarCaptions := FIniFile.ReadBool(MAIN_WINDOW, 'ShowToolbarCaptions', true);
  UseColoredIcons := FIniFile.ReadBool(MAIN_WINDOW, 'UseColoredIcons', false);

  LLanguage := PreferredUILanguages;
  LDefaultLanguage := GetEnumFromIsoLanguage(Copy(LLanguage,1,2));
  GUILanguage := TAppLanguage(FIniFile.ReadInteger(MAIN_WINDOW, 'GUILanguage',
    Ord(LDefaultLanguage)));

  VCLStyleName := FIniFile.ReadString(VCL_STYLE, 'VCLStyleName', DefaultStyleName);
  ThemeSelection := TThemeSelection(FIniFile.ReadInteger(VCL_STYLE, 'ThemeSelection', 0));

  HTMLFontSize := FIniFile.ReadInteger(HTML_VIEWER, 'HTMLFontSize', 10);
  HTMLFontName := FIniFile.ReadString(HTML_VIEWER, 'HTMLFontName', 'Arial');
  RescalingImage := FIniFile.ReadBool(HTML_VIEWER, 'RescalingImage', True);
  ProcessorDialect := TMarkdownProcessorDialect(
    FIniFile.ReadInteger(HTML_VIEWER, 'ProcessorDialect', ord(mdCommonMark)));
  DownloadFromWEB := FIniFile.ReadBool(HTML_VIEWER, 'DownloadFromWEB', True);
  CurrentFileName := FIniFile.ReadString(HTML_VIEWER, 'CurrentFileName', '');
  CurrentIndexFileName := FIniFile.ReadString(HTML_VIEWER, 'CurrentIndexFileName', '');

  PDFPageSettings.PrintOrientation := TPrinterOrientation(FIniFile.ReadInteger(PDF_SETTINGS, 'PrintOrientation', Ord(TPrinterOrientation.poPortrait)));
  PDFPageSettings.PaperSize := FIniFile.ReadInteger(PDF_SETTINGS, 'PaperSize', 0);
  PDFPageSettings.MarginTop := FIniFile.ReadInteger(PDF_SETTINGS, 'MarginTop', 100);
  PDFPageSettings.MarginBottom := FIniFile.ReadInteger(PDF_SETTINGS, 'MarginBottom', 100);
  PDFPageSettings.MarginLeft := FIniFile.ReadInteger(PDF_SETTINGS, 'MarginLeft', 100);
  PDFPageSettings.MarginRight := FIniFile.ReadInteger(PDF_SETTINGS, 'MarginRight', 100);
end;

procedure TViewerSettings.SetProcessorDialect(const AValue: TMarkdownProcessorDialect);
begin
  FProcessorDialect := AValue;
end;

procedure TViewerSettings.SetRescalingImage(const AValue: Boolean);
begin
  FRescalingImage := AValue;
end;

procedure TViewerSettings.SetShowToolbarCaptions(const AValue: Boolean);
begin
  FShowToolbarCaptions := AValue;
end;

procedure TViewerSettings.SetUseColoredIcons(const AValue: Boolean);
begin
  FUseColoredIcons := AValue;
end;

procedure TViewerSettings.SetWindowState(const AValue: TWindowState);
begin
  if AValue <> TWindowState.wsMinimized then
    FWindowState := AValue;
end;

procedure TViewerSettings.SetWindowHeight(const AValue: Integer);
begin
  FWindowHeight := AValue;
end;

procedure TViewerSettings.SetWindowWidth(const AValue: Integer);
begin
  FWindowWidth := AValue;
end;

procedure TViewerSettings.SetWindowLeft(const AValue: Integer);
begin
  if AValue > 0 then
    FWindowLeft := AValue;
end;

procedure TViewerSettings.SetWindowTop(const AValue: Integer);
begin
  if AValue > 0 then
    FWindowTop := AValue;
end;

procedure TViewerSettings.UpdateSettings(const AHTMLFontName: string;
  AHTMLFontSize: Integer; AEditorVisible: Boolean);
begin
  HTMLFontSize := AHTMLFontSize;
  HTMLFontName := AHTMLFontName;
end;

procedure TViewerSettings.WriteSettings;
begin
  FIniFile.WriteBool(MAIN_WINDOW, 'PageControlVisible', FPageControlVisible);
  FIniFile.WriteInteger(MAIN_WINDOW, 'PageControlSize', Round(FPageControlSize));
  FIniFile.WriteInteger(MAIN_WINDOW, 'ActivePageIndex', FActivePageIndex);
  FIniFile.WriteInteger(MAIN_WINDOW, 'WindowState', Ord(FWindowState));
  FIniFile.WriteInteger(MAIN_WINDOW, 'WindowWidth', FWindowWidth);
  FIniFile.WriteInteger(MAIN_WINDOW, 'WindowHeight', FWindowHeight);
  FIniFile.WriteInteger(MAIN_WINDOW, 'WindowTop', FWindowTop);
  FIniFile.WriteInteger(MAIN_WINDOW, 'WindowLeft', FWindowLeft);
  FIniFile.WriteBool(MAIN_WINDOW, 'ShowToolbarCaptions', FShowToolbarCaptions);
  FIniFile.WriteBool(MAIN_WINDOW, 'UseColoredIcons', FUseColoredIcons);
  FIniFile.WriteInteger(MAIN_WINDOW, 'GUILanguage', Ord(FGUILanguage));

  FIniFile.WriteInteger(HTML_VIEWER, 'HTMLFontSize', FHTMLFontSize);
  FIniFile.WriteString(HTML_VIEWER, 'HTMLFontName', FHTMLFontName);
  FIniFile.WriteBool(HTML_VIEWER, 'RescalingImage', FRescalingImage);
  FIniFile.WriteInteger(HTML_VIEWER, 'ProcessorDialect', Ord(FProcessorDialect));
  FIniFile.WriteBool(HTML_VIEWER, 'DownloadFromWEB', FDownloadFromWEB);
  FIniFile.WriteString(HTML_VIEWER, 'CurrentFileName', CurrentFileName);
  FIniFile.WriteString(HTML_VIEWER, 'CurrentIndexFileName', CurrentIndexFileName);

  FIniFile.WriteString(VCL_STYLE, 'VCLStyleName', FVCLStyleName);
  FIniFile.WriteInteger(VCL_STYLE, 'ThemeSelection', Ord(FThemeSelection));

  FIniFile.WriteInteger(PDF_SETTINGS, 'Orientation', Ord(PDFPageSettings.PrintOrientation));
  FIniFile.WriteInteger(PDF_SETTINGS, 'PaperSize', Ord(PDFPageSettings.PaperSize));
  FIniFile.WriteInteger(PDF_SETTINGS, 'MarginTop', PDFPageSettings.MarginTop);
  FIniFile.WriteInteger(PDF_SETTINGS, 'MarginBottom', PDFPageSettings.MarginBottom);
  FIniFile.WriteInteger(PDF_SETTINGS, 'MarginLeft', PDFPageSettings.MarginLeft);
  FIniFile.WriteInteger(PDF_SETTINGS, 'MarginRight', PDFPageSettings.MarginRight);

end;

procedure TViewerSettings.SetDownloadFromWEB(const AValue: Boolean);
begin
  FDownloadFromWEB := AValue;
end;

{ TThemeAttribute }

class function TThemeAttribute.GetStyleAttributes(const AStyleName: string;
  out AThemeAttribute: TThemeAttribute): Boolean;
var
  LThemeAttribute: TThemeAttribute;
begin
  for LThemeAttribute in ThemeAttributes do
  begin
    if SameText(AStyleName, LThemeAttribute.StyleName) then
    begin
      AThemeAttribute := LThemeAttribute;
      Exit(True);
    end;
  end;
  Result := False;
  AThemeAttribute := nil;
end;

procedure FreeThemesAttributes;
var
  LThemeAttribute: TThemeAttribute;
begin
  if Assigned(ThemeAttributes) then
  begin
    for LThemeAttribute in ThemeAttributes do
      LThemeAttribute.Free;
    FreeAndNil(ThemeAttributes);
  end;
end;

initialization
  InitDefaultThemesAttributes;

finalization
  FreeThemesAttributes;

end.
