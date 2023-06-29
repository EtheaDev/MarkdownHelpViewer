{******************************************************************************}
{                                                                              }
{       Markdown Help Viewer: Settings Form                                    }
{       (Help Viewer and Help Interfaces for Markdown files)                   }
{                                                                              }
{       Copyright (c) 2023 (Ethea S.r.l.)                                      }
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
unit MDHelpView.SettingsForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, ColorGrd, StdCtrls, CheckLst,
  ActnList, System.ImageList, Vcl.ImgList,
  SVGIconImageListBase, SVGIconImageList, MDHelpView.Settings, Vcl.ButtonGroup,
  Vcl.ToolWin, MDHelpView.Resources, Vcl.VirtualImageList, MDHelpView.About,
  Vcl.WinXCtrls, SVGIconImage, Vcl.NumberBox, Vcl.Samples.Spin;

type
  TMDSettingsForm = class(TForm)
    pc: TPageControl;
    stGeneral: TTabSheet;
    tsFont: TTabSheet;
    stTheme: TTabSheet;
    StatusBar: TStatusBar;
    OpenDialog: TOpenDialog;
    SettingsImageList: TSVGIconImageList;
    MenuButtonGroup: TButtonGroup;
    TitlePanel: TPanel;
    ThemeLeftPanel: TPanel;
    ThemesRadioGroup: TRadioGroup;
    SelectThemeRadioGroup: TRadioGroup;
    ThemeClientPanel: TPanel;
    HTMLGroupBox: TGroupBox;
    FontNameLabel: TLabel;
    FontSizeLabel: TLabel;
    HTMLFontComboBox: TComboBox;
    HTMLFontSizeEdit: TEdit;
    HTMLUpDown: TUpDown;
    RenderingGroupBox: TGroupBox;
    DownloadFromWebCheckBox: TCheckBox;
    RescalingImageCheckBox: TCheckBox;
    tsPDFLayout: TTabSheet;
    OrientationImageList: TSVGIconImageList;
    OrientationRadioGroup: TRadioGroup;
    SVGIconPosition: TSVGIconImage;
    MarginLeftEdit: TSpinEdit;
    MarginRightEdit: TSpinEdit;
    MarginTopEdit: TSpinEdit;
    MarginBottomEdit: TSpinEdit;
    PaperSizeRadioGroup: TRadioGroup;
    MarginTopLabel: TLabel;
    MarginLeftLabel: TLabel;
    MarginRightLabel: TLabel;
    MarginBottomLabel: TLabel;
    MarkdownGroupBox: TGroupBox;
    ProcessorDialectComboBox: TComboBox;
    ProcessorDialectLabel: TLabel;
    ShowCaptionCheckBox: TCheckBox;
    ColoredIconsCheckBox: TCheckBox;
    procedure ExitFromSettings(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MenuButtonGroupButtonClicked(Sender: TObject; Index: Integer);
    procedure SelectThemeRadioGroupClick(Sender: TObject);
    procedure ThemesRadioGroupClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OrientationRadioGroupClick(Sender: TObject);
  private
    FFileName: string;
    FAboutForm: TFrmAbout;
    FTitle: string;
    procedure WMCopyData(var Message: TMessage); message WM_COPYDATA;
    procedure PopulateAvailThemes;
    procedure AssignSettings(ASettings: TViewerSettings);
    procedure UpdateSettings(ASettings: TViewerSettings);
    procedure SetTitle(const Value: string);
    procedure ChangePage(AIndex: Integer);
    procedure CreateAboutForm;
    function SelectedStyleName: string;
    property Title: string read FTitle write SetTitle;
  public
  end;

function ShowSettings(const AParentRect: TRect;
  const ATitle: string;
  const ASettings: TViewerSettings;
  AFromPreview: Boolean): Boolean;

implementation

uses
{$IFNDEF DISABLE_STYLES}
  Vcl.Themes,
{$ENDIF}
  MarkdownProcessor,
  MDHelpView.Registry, MDHelpView.Main;

{$R *.dfm}

function ShowSettings(const AParentRect: TRect;
  const ATitle: string;
  const ASettings: TViewerSettings; AFromPreview: Boolean): Boolean;
var
  LSettingsForm: TMDSettingsForm;
  I: integer;
begin
  Result := False;
  for I := 0 to Screen.FormCount - 1 do
    if Screen.Forms[I].ClassType = TMDSettingsForm then
    begin
      Screen.Forms[I].BringToFront;
      exit;
    end;

  LSettingsForm := TMDSettingsForm.Create(nil);
  with LSettingsForm do
  Try
    Title := ATitle;
    AssignSettings(ASettings);
    if (AparentRect.Left <> 0) and (AparentRect.Right <> 0) then
    begin
      LSettingsForm.Left := (AParentRect.Left + AParentRect.Right - LSettingsForm.Width) div 2;
      LSettingsForm.Top := (AParentRect.Top + AParentRect.Bottom - LSettingsForm.Height) div 2;
    end;
    StatusBar.SimpleText := FFileName;

    Result := ShowModal = mrOk;
    if Result then
      UpdateSettings(ASettings);

  Finally
    LSettingsForm.Free;
  End;
end;

{ TMDSettingsForm }

procedure TMDSettingsForm.SelectThemeRadioGroupClick(Sender: TObject);
begin
  ThemeClientPanel.StyleName := SelectedStyleName;
  CreateAboutForm;
end;

procedure TMDSettingsForm.SetTitle(const Value: string);
begin
  FTitle := Value;
  TitlePanel.Caption := '  '+FTitle+' - '+TitlePanel.Caption;
  Caption := TitlePanel.Caption;
end;

procedure TMDSettingsForm.ThemesRadioGroupClick(Sender: TObject);
begin
  PopulateAvailThemes;
end;

procedure TMDSettingsForm.FormCreate(Sender: TObject);
begin
  HTMLFontComboBox.Items.Assign(Screen.Fonts);
  stGeneral.TabVisible := false;
  tsFont.TabVisible := false;
  stTheme.TabVisible := false;
  tsPDFLayout.TabVisible := false;

  TitlePanel.Font.Height := Round(TitlePanel.Font.Height * 1.5);
  MenuButtonGroup.Font.Height := Round(MenuButtonGroup.Font.Height * 1.2);
end;

procedure TMDSettingsForm.FormDestroy(Sender: TObject);
begin
  FAboutForm.Free;
end;

procedure TMDSettingsForm.ChangePage(AIndex: Integer);
begin
  pc.ActivePageIndex := AIndex;
end;

procedure TMDSettingsForm.AssignSettings(ASettings: TViewerSettings);
begin
  ChangePage(ASettings.ActivePageIndex);
  MenuButtonGroup.ItemIndex := pc.ActivePageIndex +1;
  SettingsImageList.FixedColor := ASettings.ButtonTextColor;
  SVGIconPosition.FixedColor := ASettings.ButtonTextColor;
  FFileName := ASettings.SettingsFileName;
  ThemesRadioGroup.ItemIndex := Ord(ASettings.ThemeSelection);

  HTMLFontComboBox.ItemIndex := HTMLFontComboBox.Items.IndexOf(ASettings.HTMLFontName);
  HTMLUpDown.Position := ASettings.HTMLFontSize;

  ProcessorDialectComboBox.ItemIndex := ord(ASettings.ProcessorDialect);

  RescalingImageCheckBox.Checked := ASettings.RescalingImage;
  DownloadFromWebCheckBox.Visible := ASettings is TViewerSettings;
  if DownloadFromWebCheckBox.Visible then
    DownloadFromWebCheckBox.Checked := TViewerSettings(ASettings).DownloadFromWeb
  else
    DownloadFromWebCheckBox.Checked := False;

  ShowCaptionCheckBox.Checked := ASettings.ShowToolbarCaptions;
  ColoredIconsCheckBox.Checked := ASettings.UseColoredIcons;

  OrientationRadioGroup.ItemIndex := Ord(ASettings.PDFPageSettings.PrintOrientation);
  OrientationRadioGroupClick(OrientationRadioGroup);
  PaperSizeRadioGroup.ItemIndex := Ord(ASettings.PDFPageSettings.PaperSize);
  MarginLeftEdit.Value := ASettings.PDFPageSettings.MarginLeft;
  MarginRightEdit.Value := ASettings.PDFPageSettings.MarginRight;
  MarginTopEdit.Value := ASettings.PDFPageSettings.MarginTop;
  MarginBottomEdit.Value := ASettings.PDFPageSettings.MarginBottom;

  PopulateAvailThemes;
end;

function TMDSettingsForm.SelectedStyleName: string;
begin
  if SelectThemeRadioGroup.ItemIndex <> -1 then
    Result := SelectThemeRadioGroup.Items[SelectThemeRadioGroup.ItemIndex]
  else
    Result := DefaultStyleName;
end;

procedure TMDSettingsForm.UpdateSettings(ASettings: TViewerSettings);
begin
  ASettings.ActivePageIndex := pc.ActivePageIndex;
  ASettings.ThemeSelection := TThemeSelection(ThemesRadioGroup.ItemIndex);

  ASettings.HTMLFontName := HTMLFontComboBox.Text;
  ASettings.HTMLFontSize := HTMLUpDown.Position;

  ASettings.ProcessorDialect := TMarkdownProcessorDialect(ProcessorDialectComboBox.ItemIndex);

  ASettings.VCLStyleName := SelectedStyleName;
  ASettings.RescalingImage := RescalingImageCheckBox.Checked;
  if ASettings is TViewerSettings then
    TViewerSettings(ASettings).DownloadFromWEB := DownloadFromWEBCheckBox.Checked;

  ASettings.ShowToolbarCaptions := ShowCaptionCheckBox.Checked;
  ASettings.UseColoredIcons := ColoredIconsCheckBox.Checked;

  ASettings.PDFPageSettings.PrintOrientation := TPrinterOrientation(OrientationRadioGroup.ItemIndex);
  ASettings.PDFPageSettings.PaperSize := PaperSizeRadioGroup.ItemIndex;
  ASettings.PDFPageSettings.MarginLeft := MarginLeftEdit.Value;
  ASettings.PDFPageSettings.MarginRight := MarginRightEdit.Value;
  ASettings.PDFPageSettings.MarginTop := MarginTopEdit.Value;
  ASettings.PDFPageSettings.MarginBottom := MarginBottomEdit.Value;
end;

procedure TMDSettingsForm.WMCopyData(var Message: TMessage);
begin
  Close;
  MainForm.WMCopyData(Message);
end;

procedure TMDSettingsForm.MenuButtonGroupButtonClicked(Sender: TObject;
  Index: Integer);
begin
  if Sender is TButtonGroup then
  begin
    case Index of
      0: ExitFromSettings(nil);
      1,2,3,4,5: ChangePage(Index -1);
    else
      Beep;
    end;
  end;
end;

procedure TMDSettingsForm.OrientationRadioGroupClick(Sender: TObject);
begin
  SVGIconPosition.ImageIndex := OrientationRadioGroup.ItemIndex;
end;

procedure TMDSettingsForm.CreateAboutForm;
begin
  FAboutForm.Free;
  FAboutForm := TFrmAbout.Create(Self);
  FAboutForm.BorderIcons := [];
  FAboutForm.Title := FTitle;
  FAboutForm.Parent := ThemeClientPanel;
  FAboutForm.Align := alClient;
  FAboutForm.DisableButtons;
  FAboutForm.btnOK.OnClick := ExitFromSettings;
  FAboutForm.Visible := True;
end;

procedure TMDSettingsForm.PopulateAvailThemes;
var
  I: Integer;
  IsLight: Boolean;
  LStyleName: string;
  LThemeAttributes: TThemeAttribute;
begin
  if TThemeSelection(ThemesRadioGroup.ItemIndex) = tsAsWindows then
    IsLight := IsWindowsAppThemeLight
  else
    IsLight := TThemeSelection(ThemesRadioGroup.ItemIndex) = tsLightTheme;

  SelectThemeRadioGroup.Items.Clear;
{$IFNDEF DISABLE_STYLES}
  for I := 0 to High(TStyleManager.StyleNames) do
  begin
    LStyleName := TStyleManager.StyleNames[I];
    TThemeAttribute.GetStyleAttributes(LStyleName, LThemeAttributes);
    if not Assigned(LThemeAttributes) then
      Continue;
    if IsLight and (LThemeAttributes.ThemeType = ttLight) or
      (not IsLight and (LThemeAttributes.ThemeType = ttDark)) then
      SelectThemeRadioGroup.Items.Add(LStyleName);
  end;
{$ELSE}
    LStyleName := 'Windows';
    TThemeAttribute.GetStyleAttributes(LStyleName, LThemeAttributes);
{$ENDIF}
  SelectThemeRadioGroup.OnClick := nil;
  try
    TStringList(SelectThemeRadioGroup.Items).Sort;
{$IFNDEF DISABLE_STYLES}
    SelectThemeRadioGroup.ItemIndex :=
      SelectThemeRadioGroup.Items.IndexOf(TStyleManager.ActiveStyle.Name);
{$ELSE}
    SelectThemeRadioGroup.ItemIndex := 0;
{$ENDIF}
    if SelectThemeRadioGroup.ItemIndex = -1 then
      SelectThemeRadioGroup.ItemIndex := 0;
  finally
    SelectThemeRadioGroup.OnClick := SelectThemeRadioGroupClick;
    SelectThemeRadioGroupClick(SelectThemeRadioGroup);
  end;
end;

procedure TMDSettingsForm.ExitFromSettings(Sender: TObject);
begin
  //Salva i parametri su file
  ModalResult := mrOk;
end;

end.
