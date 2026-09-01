{******************************************************************************}
{                                                                              }
{       Markdown Help Viewer: Main Form                                        }
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
unit MDHelpView.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.ExtCtrls, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.Menus, System.Actions, Vcl.ActnList, Vcl.StdActns,
  Vcl.ComCtrls, Vcl.ToolWin, MDHelpView.Resources, Vcl.FileCtrl,
  System.ImageList, Vcl.ImgList, Vcl.VirtualImageList, Vcl.ExtActns,
  MDHelpView.Settings,
  HTMLUn2, HtmlView, HtmlGlobals,
  vmHtmlToPdf, SVGIconImageListBase,
  SVGIconImageList, CBMultiLanguage,
  MDCodeHighlightEmitter,
  {$IFDEF STYLEDCOMPONENTS}
  Vcl.StyledComponentsHooks,
  Vcl.StyledMessagesHooks,
  {$ENDIF}
  MDHelpView.FormsHookTrx;

type
  TMainForm = class(TFormHook)
    OpenDialog: TFileOpenDialog;
    ActionList: TActionList;
    acOpenFile: TAction;
    PageControl: TPageControl;
    tsIndex: TTabSheet;
    tsFiles: TTabSheet;
    tsSearch: TTabSheet;
    lbIndex: TLabel;
    FileListBox: TFileListBox;
    edFileSearch: TEdit;
    btIndex: TButton;
    btSearch: TButton;
    edSearch: TEdit;
    lbSearch: TLabel;
    SearchListBox: TListBox;
    lbSelectSearch: TLabel;
    acPreviousPage: TAction;
    acNextPage: TAction;
    acView: TAction;
    acHide: TAction;
    acSearch: TAction;
    acHome: TAction;
    acSettings: TAction;
    acShow: TAction;
    Splitter: TSplitter;
    acAbout: TAction;
    paTop: TPanel;
    ProcessorDialectComboBox: TComboBox;
    ToolBar: TToolBar;
    btShowHide: TToolButton;
    sep1: TToolButton;
    btOpen: TToolButton;
    sep2: TToolButton;
    btPrevius: TToolButton;
    btNext: TToolButton;
    btHome: TToolButton;
    btOption: TToolButton;
    btAbout: TToolButton;
    btExportHTML: TToolButton;
    lbSelectFile: TLabel;
    acPrint: TAction;
    SaveDialog: TFileSaveDialog;
    SaveDialogPDF: TFileSaveDialog;
    SaveDialogHTML: TFileSaveDialog;
    btSaveToPdf: TToolButton;
    acSaveToPDF: TAction;
    Sep4: TToolButton;
    acViewSearch: TAction;
    SVGIconImageList: TSVGIconImageList;
    HtmlViewerIndex: THtmlViewer;
    HtmlViewer: THtmlViewer;
    ClientPanel: TPanel;
    SVGIconImageListColored: TSVGIconImageList;
    acRefresh: TAction;
    btRefresh: TToolButton;
    acExportHTML: TAction;
    ProcessorDialectLabel: TLabel;
    btSearchView: TButton;
    procedure FormCreate(Sender: TObject);
    procedure acOpenFileExecute(Sender: TObject);
    procedure acSettingsExecute(Sender: TObject);
    procedure acHideExecute(Sender: TObject);
    procedure ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure acShowExecute(Sender: TObject);
    procedure ProcessorDialectComboBoxSelect(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure acAboutExecute(Sender: TObject);
    procedure acViewUpdate(Sender: TObject);
    procedure acViewExecute(Sender: TObject);
    procedure acHomeUpdate(Sender: TObject);
    procedure acHomeExecute(Sender: TObject);
    function IndexOfCurrentFile: Integer;
    procedure HtmlViewerHotSpotClick(Sender: TObject; const ASource: ThtString;
      var Handled: Boolean);
    procedure HtmlViewerKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure acNextPageUpdate(Sender: TObject);
    procedure acPreviousPageExecute(Sender: TObject);
    procedure acNextPageExecute(Sender: TObject);
    procedure acPreviousPageUpdate(Sender: TObject);
    procedure edFileSearchChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure HTMLToPDF(const APDFFileName: TFileName);
    procedure acSaveToPDFUpdate(Sender: TObject);
    procedure acSaveToPDFExecute(Sender: TObject);
    procedure acSearchUpdate(Sender: TObject);
    procedure acSearchExecute(Sender: TObject);
    procedure acViewSearchUpdate(Sender: TObject);
    procedure acViewSearchExecute(Sender: TObject);
    procedure ClientPanelResize(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure acRefreshUpdate(Sender: TObject);
    procedure acRefreshExecute(Sender: TObject);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure acExportHTMLUpdate(Sender: TObject);
    procedure acExportHTMLExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FRememberToResize: boolean;
    FLoading: boolean;
    //True while the HTML viewer is loading/laying out content: see the
    //re-entrancy guard in ShowMarkdownAsHTML
    FRendering: Boolean;
    //Version string of the running executable, read once from VERSIONINFO
    FVersionString: string;
    FOldViewerResize: Integer;
    FFirstTime: Boolean;
    FOpenedFileList: TStringList;
    FCodeHighlightEmitter: TCodeHighlightEmitterBase;
    FHTMLFontSize: Integer;
    FHtmlContent: string;
    FCssContent: string;
    FMdContent: string;
    FMdIndexContent: string;
    FMdFileName: TFileName;
    FMdIndexFileName: TFileName;
    FHtmlIndexContent: string;
    FHTMLFontName: string;
    FWorkingFolder: string;
    FCurrentFileName: TFileName;
    FCurrentIndexFileName: TFileName;
    FCurrentCSSFileName: TFileName;
    FShowToolbarCaptions: Boolean;
    FUseColoredIcons: Boolean;
    FVCLStyleName: string;
    procedure AdjustConstraint;
    function DialogPosRect: TRect;
    procedure LoadAndTransformFile(const AFileName: TFileName);
    procedure UpdateGui;
    procedure InitDialog(const ADialog: TCustomFileDialog;
      const ADefaultFolder: string);
    procedure InitPDFDialog(const ADialog: TCustomFileDialog;
      const ADefaultFolder: string);
    procedure InitHTMLDialog(const ADialog: TCustomFileDialog;
      const ADefaultFolder: string);
    procedure UpdateFromSettings;
    procedure WriteSettingsToIni;
    procedure UpdateApplicationStyle(const AVCLStyleName: string);
    function Load(const AFileName: TFileName): Boolean;
    function TransformMDToHTML(const AMdContent, AHtmlContent: string): string;
    procedure TransformTo(const AHTMLViewer: THtmlViewer;
      const AMdContent, AHtmlContent: string; const AReloadImage: Boolean;
      const APreservePosition: Boolean);
    function LoadIndex(const AFileName: TFileName): Boolean;
    procedure LoadCSS(const AFileName: TFileName);
    function TryLoadCSS(const AFileName: TFileName): Boolean;
    procedure SetHTMLFontSize(const Value: Integer);
    procedure SetHTMLFontName(const Value: string);
    procedure SetWorkingFolder(const Value: string);
    procedure LoadAndTransformFileIndex(const AFileName: TFileName);
    procedure UpdateCaption;
    procedure UpdateHTMLViewer(const AHTMLViewer: THtmlViewer);
    procedure SetCurrentFileName(const AValue: TFileName);
    procedure SetCurrentIndexFileName(const AValue: TFileName);
    procedure FileSavedAskToOpen(const AFileName: string);
    procedure ShowMarkdownAsHTML(const AHTMLViewer: THTMLViewer;
      const AHTMLContent: string; const AReloadImages: Boolean;
      const APreservePosition: Boolean);
    procedure SetShowToolbarCaptions(const Value: Boolean);
    procedure SetUseColoredIcons(const Value: Boolean);
    procedure UpdateIconsColorByStyle;
    procedure SetCurrentCSSFileName(const Value: TFileName);
    function GetCssContent: string;
    procedure UpdateWindowPos;
    function GetDialectSelectionVisible: Boolean;
    function GetToolbarWidth: Integer;
    function GetIndexOfWorkingFolder(const AWorkingFolder: string): TFileName;
    function OpenExternalLink(const AUrl: string): Boolean;
    property HTMLFontSize: Integer read FHTMLFontSize write SetHTMLFontSize;
    property HTMLFontName: string read FHTMLFontName write SetHTMLFontName;
    property WorkingFolder: string read FWorkingFolder write SetWorkingFolder;
    property CurrentFileName: TFileName read FCurrentFileName write SetCurrentFileName;
    property CurrentIndexFileName: TFileName read FCurrentIndexFileName write SetCurrentIndexFileName;
    property CurrentCSSFileName: TFileName read FCurrentCSSFileName write SetCurrentCSSFileName;
    property ShowToolbarCaptions: Boolean read FShowToolbarCaptions write SetShowToolbarCaptions;
    property UseColoredIcons: Boolean read FUseColoredIcons write SetUseColoredIcons;
    property CSSContent: string read GetCssContent;
    property DialectSelectionVisible: Boolean read GetDialectSelectionVisible;
  public
    procedure ManageExceptions(Sender: TObject; E: Exception);
    procedure WMCopyData(var Message: TMessage); message WM_COPYDATA;
  end;

var
  MainForm: TMainForm;
  FViewerSettings: TViewerSettings;

implementation

{$R *.dfm}

uses
  MarkdownProcessor
  , MarkDownUtils
  , System.Math
  , System.Types
  , System.UITypes
  , System.IOUtils
  , MDHelpView.SettingsForm
  , MDHelpView.About
  , MDHelpView.Misc
  , Vcl.Themes
  , Winapi.ShellAPI
  , System.StrUtils
  , SynPDF
  , MarkDownHelpViewer
  , MarkDownViewerComponents
  //VCLStyles support
  {$IFNDEF NO_VCL_STYLES}
  , Vcl.Styles.Fixes
  , Vcl.Styles.FormStyleHooks
  , Vcl.Styles.OwnerDrawFix
  , Vcl.Styles.Utils.ScreenTips
  , Vcl.Styles.Utils.SysStyleHook
  , Vcl.Styles.Utils
  , Vcl.Styles.Utils.SysControls
  , Vcl.Styles.UxTheme
  , Vcl.Styles.Hooks
  , Vcl.Styles.Utils.Forms
  , Vcl.Styles.Utils.ComCtrls
  , Vcl.Styles.Utils.StdCtrls
  , Vcl.Styles.Ext
  {$ENDIF}
  , Vcl.ButtonGroup
  {$IFDEF STYLEDCOMPONENTS}
  , Vcl.StyledTaskDialog
  , Vcl.StyledButton
  , Vcl.ButtonStylesAttributes
  , Vcl.StyledButtonGroup
  , Vcl.StyledToolbar
  {$ENDIF}
  , System.IniFiles
  , MDHelpView.Messages
  ;

procedure TMainForm.acAboutExecute(Sender: TObject);
begin
  ShowAboutForm(DialogPosRect, Title_MDHViewer, False);
end;

procedure TMainForm.acExportHTMLExecute(Sender: TObject);
var
  I: Integer;
  LFileName: TFileName;
  LOutputFolder: string;
  LResult: TModalResult;
  LMdContent, LHtmlContent: string;

  //Rewrites the links to markdown files so that they point to the exported
  //.htm files.
  //NB: only the end of an href is replaced - the extension followed by the
  //closing quote or by an anchor - and the longest extensions are handled
  //first. A plain replace of every extension over the whole document turned
  //'.mdown' into '.htmown' (because '.md' was replaced first) and rewrote any
  //occurrence of the text ".md" in the prose as well.
  function ReplaceMarkdownLinks(const AHtml: string): string;
  var
    I, J: Integer;
    LExts: TArray<string>;
    LSwap: string;
  begin
    LExts := Copy(AMarkdownFileExt);
    for I := Low(LExts) to High(LExts) - 1 do
      for J := I + 1 to High(LExts) do
        if Length(LExts[J]) > Length(LExts[I]) then
        begin
          LSwap := LExts[I];
          LExts[I] := LExts[J];
          LExts[J] := LSwap;
        end;
    Result := AHtml;
    for I := Low(LExts) to High(LExts) do
    begin
      Result := StringReplace(Result, LExts[I] + '"', '.htm"',
        [rfReplaceAll, rfIgnoreCase]);
      Result := StringReplace(Result, LExts[I] + '#', '.htm#',
        [rfReplaceAll, rfIgnoreCase]);
    end;
  end;

  procedure ConvertAndSaveFile(const AInputFileName, AOutputFileName: TFileName);
  begin
    if FileExists(AInputFileName) then
    begin
      LMdContent := TryLoadTextFile(AInputFileName);
      //The exported files are markdown (the list is filtered by extension), so
      //there is no HTML fallback content to pass
      LHtmlContent := ReplaceMarkdownLinks(TransformMDToHTML(LMdContent, ''));
      SaveUTF8File(AOutputFileName, LHtmlContent);
    end;
  end;

begin
  LOutputFolder := IncludeTrailingPathDelimiter(WorkingFolder)+'..\WebHelp\';
  LResult := StyledMessageDlg(
    Format(CONFIRM_EXPORT_HTML, [FileListBox.Count]),
      TMsgDlgType.mtConfirmation,
      [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbYesToAll, TMsgDlgBtn.mbNo, TMsgDlgBtn.mbCancel],
      0);
  if LResult = mrYes then
  begin
    LFileName := LOutputFolder+
      ExtractFileName(ChangeFileExt(FCurrentFileName, '.htm'));
    SaveDialogHTML.FileName := LFileName;
    if SaveDialogHTML.Execute then
    begin
      Screen.Cursor := crHourGlass;
      try
        ConvertAndSaveFile(FCurrentFileName, SaveDialogHTML.FileName);
        FileSavedAskToOpen(SaveDialogHTML.FileName);
      finally
        Screen.Cursor := crDefault;
      end;
    end;
  end
  else if LResult = mrYesToAll then
  begin
    if FCurrentIndexFileName <> '' then
      LFileName := LOutputFolder+
        ExtractFileName(ChangeFileExt(FCurrentIndexFileName,'.htm'))
    else
      LFileName := LOutputFolder+
        ExtractFileName(ChangeFileExt(FCurrentFileName, '.htm'));
    SaveDialogHTML.FileName := LFileName;
    if SaveDialogHTML.Execute then
    begin
      Screen.Cursor := crHourGlass;
      try
        LFileName := SaveDialogHTML.FileName;
        LOutputFolder := IncludeTrailingPathDelimiter(
          ExtractFilePath(LFileName));
        for I := 0 to FileListBox.Count -1 do
        begin
          ConvertAndSaveFile(FWorkingFolder+FileListBox.Items[I],
            ChangeFileExt(LOutputFolder+FileListBox.Items[I], '.htm'));
        end;
        FileSavedAskToOpen(LFileName);
      finally
         Screen.Cursor := crDefault;
      end;
    end;
  end;
end;

procedure TMainForm.acExportHTMLUpdate(Sender: TObject);
begin
  acExportHTML.Enabled := FileExists(FCurrentFileName);
end;

procedure TMainForm.acOpenFileExecute(Sender: TObject);
begin
  if OpenDialog.Execute then
    LoadAndTransformFile(OpenDialog.FileName);
end;

procedure TMainForm.acShowExecute(Sender: TObject);
var
  LDim: Integer;
begin

  //calculation dimension to add
  LDim := PageControl.Width + Splitter.Width;

  ClientPanel.OnResize := nil;
  try
    //enable the splitter and the pageControl
    PageControl.Visible := True;
    PageControl.Enabled := True;
    Splitter.Left := PageControl.Left+1;
    Splitter.Visible := True;
    Splitter.Enabled := True;

    //move the left edge of the calculated value window
    //add dim window of the calculated value
    SetBounds(Self.Left - LDim, Self.Top, Self.Width + LDim, Self.Height);
  finally
    ClientPanel.OnResize := ClientPanelResize;
  end;
end;

procedure TMainForm.acViewExecute(Sender: TObject);
var
  LFileName:string;
begin
  LFileName := FileListBox.Items[FileListBox.ItemIndex];
  LoadAndTransformFile(WorkingFolder+LFileName);
end;

procedure TMainForm.acViewSearchExecute(Sender: TObject);
var
  LFileName:string;
begin
  LFileName := SearchListBox.Items[SearchListBox.ItemIndex];
  LoadAndTransformFile(WorkingFolder+LFileName);
end;

procedure TMainForm.acViewSearchUpdate(Sender: TObject);
begin
  //Enable acViewSearch only if a file is selected into SearchListBox
  acViewSearch.Enabled := (Pagecontrol.ActivePage = tsSearch) and
    (SearchListBox.ItemIndex >= 0);
end;

procedure TMainForm.acViewUpdate(Sender: TObject);
begin
  //Enable AcView only if a file is selected into FileListBox
  acView.Enabled := (Pagecontrol.ActivePage = tsFiles) and
    (FileListBox.ItemIndex >= 0);
end;

procedure TMainForm.ClientPanelResize(Sender: TObject);
begin
  if (Abs(FOldViewerResize-ClientPanel.Width) > 10) and not FLoading then
  begin
    //Reload content, forcing reloading images if size of ClientPanel changes
    FOldViewerResize := ClientPanel.Width;
    FRememberToResize := True;
  end;
end;

procedure TMainForm.acHideExecute(Sender: TObject);
var
  LDim: Integer;
begin
  //calculation dimension to reduce
  if WindowState <> TWindowState.wsMaximized then
    Ldim := PageControl.Width - Splitter.Width
  else
    ldim := 0;

  ClientPanel.OnResize := nil;
  try
    //disable the splitter and the pageControl
    Splitter.Enabled := False;
    PageControl.Enabled := False;
    Splitter.Visible := False;
    PageControl.Visible := False;

    //reduce dim window of the calculated value
    //move the left edge of the calculated value window
    SetBounds(Self.Left + LDim, Self.Top, Self.Width - Ldim, Self.Height);
  finally
    ClientPanel.OnResize := ClientPanelResize;
  end;
end;

procedure TMainForm.acHomeExecute(Sender: TObject);
begin
  LoadAndTransformFile(FOpenedFileList.Strings[0]);
end;

procedure TMainForm.acHomeUpdate(Sender: TObject);
begin
  acHome.Enabled := (FOpenedFileList.Count > 1)
    and (IndexOfCurrentFile <> 0);
end;

procedure TMainForm.acSaveToPDFExecute(Sender: TObject);
begin
  SaveDialogPDF.FileName := ChangeFileExt(FCurrentFileName, '.pdf');
  if SaveDialogPDF.Execute then
  begin
    Screen.Cursor := crHourGlass;
    try
      HTMLToPDF(SaveDialogPDF.FileName);
      FileSavedAskToOpen(SaveDialogPDF.FileName);
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TMainForm.acSaveToPDFUpdate(Sender: TObject);
begin
  acSaveToPDF.Enabled := (FCurrentFileName <> '');
end;

procedure TMainForm.acSearchExecute(Sender: TObject);
var
  LFileList: TStringList;
  I: Integer;
  LFileName: TFileName;
  LKeyword, LFileContent: string;
begin
  Screen.Cursor := crHourGlass;
  try
    SearchListBox.Clear;
    //Search a text into list of files:
    LKeyword := edSearch.Text;
    LFileList := TStringList.Create;
    try
      if FMdContent <> '' then
      begin
        //Search markdown files
        GetFileNamesWithExtensions(LFileList, FWorkingFolder,
          GetFileMasks(AMarkdownFileExt));
      end
      else
      begin
        //Seach .htm files and .html files
        GetFileNamesWithExtensions(LFileList, FWorkingFolder,
          GetFileMasks(AHTMLFileExt));
      end;
      for I := 0 to LFileList.Count -1 do
      begin
        LFileName := LFileList.Strings[I];
        LFileContent := TryLoadTextFile(FWorkingFolder+LFileName);
        if ContainsText(LFileContent, Lkeyword) then
        begin
          SearchListBox.Items.Add(LFileName);
        end;
      end;
      if SearchListBox.Items.Count > 0 then
        SearchListBox.SetFocus
      else
        //NB: "no match" is a normal outcome of a search, not an error: it used
        //to be raised as an exception and shown as an error dialog.
        StyledMessageDlg(Format(NO_KEYWORD_MATCH, [LKeyword, FWorkingFolder]),
          TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], 0);
    finally
      LFileList.Free;
    end;
  finally
    Screen.Cursor := crDefault;
  end;


end;

procedure TMainForm.acSearchUpdate(Sender: TObject);
begin
  acSearch.Enabled := (FWorkingFolder <> '') and
    (edSearch.Text <> '');
end;

procedure TMainForm.acSettingsExecute(Sender: TObject);
var
  LOldLanguage: TAppLanguage;
  LParam: string;
begin
  LOldLanguage := FViewerSettings.GUILanguage;
  if ShowSettings(DialogPosRect,
    Title_MDHViewer,
    FViewerSettings, False) then
  begin
    WriteSettingsToIni;
    if LOldLanguage <> FViewerSettings.GUILanguage then
    begin
      StyledMessageDlg(
        CLOSE_APP_FOR_LANG,
        TMsgDlgType.mtInformation,
        [TMsgDlgBtn.mbOK], 0);

      if FCurrentFileName <> '' then
        LParam := Format('"%s"', [FCurrentFileName])
      else
        LParam := '';
      ShellExecute(Handle, nil, PChar(Application.ExeName), PChar(LParam), nil, SW_SHOWNORMAL);
      Application.Terminate;
    end;
    UpdateFromSettings;
  end;
end;

function TMainForm.DialogPosRect: TRect;
begin
  GetWindowRect(Self.Handle, Result);
end;

procedure TMainForm.edFileSearchChange(Sender: TObject);
var
  LValue: string;
  L, I: Integer;
begin
  LValue := edFileSearch.Text;
  L := Length(LValue);
  for I := 0 to FileListBox.Count -1 do
  begin
    if SameText(Copy(FileListBox.Items[I],1,L), LValue) then
    begin
      FileListBox.ItemIndex := I;
      break;
    end;
  end;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  //Set Bounds of Windows
  FViewerSettings.WindowState := Self.WindowState;

  Self.WindowState := TWindowState.wsNormal;
  //NB: negative coordinates are saved as they are (a monitor to the left of,
  //or above, the primary one is a normal setup). Clamping them to zero moved
  //the window to the primary monitor at every restart.
  FViewerSettings.WindowLeft := Round(Self.Left / ScaleFactor);
  FViewerSettings.WindowTop := Round(Self.Top / ScaleFactor);
  FViewerSettings.WindowWidth := Round(Self.ClientWidth / ScaleFactor);
  FViewerSettings.WindowHeight := Round(Self.ClientHeight / ScaleFactor);

  WriteSettingsToIni;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Screen.MessageFont.Size := Round(Screen.MessageFont.Size*1.2);
  {$IFDEF STYLEDCOMPONENTS}
  InitializeStyledTaskDialogs(True, Screen.MessageFont);
  {$ENDIF}

  PageControl.ActivePageIndex := 0;
  FOldViewerResize := ClientPanel.Width;

  FOpenedFileList := TStringList.Create;
  //Optional syntax-highlighting emitter for fenced code blocks (nil when the
  //MD_SYNTAX_HIGHLIGHTING define is off, so no SynEdit dependency is linked).
  FCodeHighlightEmitter := CreateCodeHighlightEmitter;
  dmResources.Settings := FViewerSettings;

  UpdateHTMLViewer(HtmlViewer);
  UpdateHTMLViewer(HtmlViewerIndex);

  //Update Form Caption
  UpdateCaption;

  //Load Application Style from settings
  UpdateApplicationStyle(FViewerSettings.VCLStyleName);

  UpdateFromSettings;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FViewerSettings.Free;
  FOpenedFileList.Free;
  FCodeHighlightEmitter.Free;
end;

procedure TMainForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = chr(27) then
  begin
    if dmResources.IsLoadingImages then
      dmResources.StopLoadingImages(True)
    else
      Close;
  end;
end;

procedure TMainForm.FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if (Shift = [ssCtrl]) then
  begin
    HTMLFontSize := HTMLFontSize - 1;
    TransformTo(HtmlViewer, FMdContent, FHtmlContent, True, True);
    TransformTo(HtmlViewerIndex, FMdIndexContent, FHtmlIndexContent, True, True);
    Handled := True;
  end;
end;

procedure TMainForm.InitDialog(const ADialog: TCustomFileDialog;
  const ADefaultFolder: string);
var
  LMarkdownMasks, LHTMLMasks: string;
begin
  LMarkdownMasks := GetFileMasks(AMarkdownFileExt);
  LHTMLMasks := GetFileMasks(AHTMLFileExt);

  ADialog.DefaultFolder := ADefaultFolder;
  // Add File Filters
  with ADialog.FileTypes.Add do
  begin
    DisplayName := MARKDOWN_FILES;
    FileMask := LMarkdownMasks;
  end;
  with ADialog.FileTypes.Add do
  begin
    DisplayName := HTML_FILES;
    FileMask := LHTMLMasks;
  end;
end;

procedure TMainForm.InitPDFDialog(const ADialog: TCustomFileDialog;
  const ADefaultFolder: string);
begin
  ADialog.DefaultFolder := ADefaultFolder;
  // Add File Filters
  with ADialog.FileTypes.Add do
  begin
    DisplayName := PDF_FILES;
    FileMask := '*.pdf';
  end;
end;

procedure TMainForm.InitHTMLDialog(const ADialog: TCustomFileDialog;
  const ADefaultFolder: string);
var
  LHTMLMasks: string;
begin
  LHTMLMasks := GetFileMasks(AHTMLFileExt);
  ADialog.DefaultFolder := ADefaultFolder;
  // Add File Filters
  with ADialog.FileTypes.Add do
  begin
    DisplayName := HTML_FILES;
    FileMask := LHTMLMasks;
  end;
end;

procedure TMainForm.FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if (Shift = [ssCtrl]) then
  begin
    HTMLFontSize := HTMLFontSize + 1;
    TransformTo(HtmlViewer, FMdContent, FHtmlContent, True, True);
    TransformTo(HtmlViewerIndex, FMdIndexContent, FHtmlIndexContent, True, True);
    Handled := True;
  end;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  UpdateWindowPos;
end;

function TMainForm.GetCssContent: string;
begin
  if FCssContent <> '' then
    Result := FCssContent
  else if FViewerSettings.CustomCSS <> '' then
    Result := FViewerSettings.CustomCSS
  else
    Result := GetMarkdownDefaultCSS;
end;

function TMainForm.GetDialectSelectionVisible: Boolean;
begin
  //set visible ProcessorDialectLabel and ProcessorDialectComboBox
  Result := FViewerSettings.ShowDialectSelection and
    ((FMdIndexContent <> '') or (FMdContent <> ''));
end;

function TMainForm.OpenExternalLink(const AUrl: string): Boolean;
var
  LScheme: string;
  P: Integer;
begin
  //Only web links and mail addresses are opened straight away. Anything else -
  //a local executable, a UNC path, an unknown protocol - would reach
  //ShellExecute from a document written by somebody else, so it is confirmed
  //with the user first.
  Result := True;
  P := Pos(':', AUrl);
  if P > 1 then
    LScheme := LowerCase(Copy(AUrl, 1, P - 1))
  else
    LScheme := '';
  if (LScheme <> 'http') and (LScheme <> 'https') and (LScheme <> 'mailto') then
  begin
    if StyledMessageDlg(Format(CONFIRM_OPEN_LINK, [AUrl]),
      TMsgDlgType.mtWarning, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) <> mrYes then
      Exit;
  end;
  ShellExecute(0, 'open', PChar(AUrl), nil, nil, SW_SHOWNORMAL);
end;

procedure TMainForm.HtmlViewerHotSpotClick(Sender: TObject;
  const ASource: ThtString; var Handled: Boolean);
var
  LFileName: TFileName;
begin
  LFileName := ASource;

  //An anchor inside the current page is handled by the viewer itself.
  //NB: it used to end up in ShellExecute (which did nothing) with Handled set
  //to True, so in-page links did not work at all.
  if (LFileName <> '') and (LFileName[1] = '#') then
  begin
    Handled := False;
    Exit;
  end;

  if not FileExists(LFileName) then
    LFileName := FWorkingFolder+LFileName;
  if not FileExists(LFileName) then
  begin
    if FileWithExtExists(LFileName, AMarkdownFileExt) then
    begin
      LoadAndTransformFile(LFileName);
      Handled := True;
    end
    else
    begin
      Handled := OpenExternalLink(ASource);
    end;
  end
  else
  begin
    LoadAndTransformFile(LFileName);
    Handled := True;
  end;
end;

procedure TMainForm.HtmlViewerKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  //Allow copying the selected text from the (read-only) viewer with Ctrl+C
  if (Key = Ord('C')) and (Shift = [ssCtrl]) and (Sender is THtmlViewer) then
  begin
    THtmlViewer(Sender).CopyToClipboard;
    Key := 0;
  end;
end;

function TMainForm.IndexOfCurrentFile: Integer;
begin
  Result := FOpenedFileList.IndexOf(CurrentFileName);
end;

procedure TMainForm.UpdateHTMLViewer(const AHTMLViewer: THtmlViewer);
var
  LDetails: TThemedElementDetails;
  LColor: TColor;
begin
  AHTMLViewer.OnHotSpotClick := HtmlViewerHotSpotClick;
  AHTMLViewer.OnImageRequest := dmResources.HtmlViewerImageRequest;
  AHTMLViewer.ScrollBars := TScrollStyle.ssVertical;
  AHTMLViewer.DefFontName := FViewerSettings.HTMLFontName;
  AHTMLViewer.DefFontSize := FViewerSettings.HTMLFontSize;
  if StyleServices.Enabled then
  begin
    LDetails := StyleServices.GetElementDetails(tbCommandLinkNormal);
    StyleServices.GetElementColor(LDetails, ecTextColor, LColor);
    AHTMLViewer.DefHotSpotColor := LColor;
  end
  else
  begin
    AHTMLViewer.DefHotSpotColor := clBlue;
  end;
  AHTMLViewer.DefOverLinkColor := AHTMLViewer.DefHotSpotColor;
end;

function TMainForm.Load(const AFileName: TFileName): Boolean;
var
  LWorkingFolder: string;
  LIndexFileName: TFileName;
begin
  FLoading := True;
  try
    Result := FileExists(AFileName);
    if Result then
    begin
      //Set content variable based on Extension of FileName
      if IsFileNameWithExt(AFileName, AHTMLFileExt) then
      begin
        //load html content
        FHtmlContent := TryLoadTextFile(AFileName);
        //empty md content
        FMdContent := '';
        FMdFileName := '';
      end
      else
      begin
        //empty html content
        FHtmlContent := '';
        //load md content
        FMdContent := TryLoadTextFile(AFileName);
        FMdFileName := AFileName;
      end;

      CurrentFileName := AFileName;

      //Add loaded filename into OpendFileList if is New
      if FOpenedFileList.IndexOf(AFileName) < 0 then
        FOpenedFileList.Add(AFileName);

      //Update Form Caption
      UpdateCaption;

      //Set WorkingFolder as Path of Markdown File
      LWorkingFolder := ExtractFilePath(AFileName);

      //Search for a css file into this folder
      if not TryLoadCSS(LWorkingFolder+'Home.css')
        and not TryLoadCSS(LWorkingFolder+'Index.css')
        //NB: FCurrentFileName is a full path, so only its file name must be
        //appended to LWorkingFolder (otherwise the result is "C:\dir\C:\dir\file.css")
        and not TryLoadCSS(LWorkingFolder+ExtractFileName(ChangeFileExt(FCurrentFileName,'.css'))) then
      begin
        CurrentCSSFileName := '';
      end;

      if WorkingFolder <> LWorkingFolder then
      begin
        WorkingFolder := LWorkingFolder;
        //Search an Index Name similar to AFileName
        LIndexFileName := GetIndexFileName(AFileName, AMarkdownFileExt);
        if LIndexFileName = '' then
          LIndexFileName := GetIndexOfWorkingFolder(LWorkingFolder);
        if (LIndexFileName <> '') then
          LoadAndTransformFileIndex(LIndexFileName);
      end
      else
      begin
        //Search an Index Name similar to AFileName
        LIndexFileName := GetIndexFileName(AFileName, AMarkdownFileExt);
        if (LIndexFileName <> '') then
          LoadAndTransformFileIndex(LIndexFileName);
      end;
    end;
  finally
    FLoading := False;
  end;
end;

function TMainForm.TransformMDToHTML(const AMdContent, AHtmlContent: string): string;
var
  LMarkdownProcessor: TMarkdownProcessor;
  LBackground, LForeground: TColor;
  LDark: Boolean;
begin
  //Transform Markdown content in HTML
  if (AMdContent <> '') then
  begin
    //Transform file Markdown in HTML using TMarkdownProcessor
    LMarkdownProcessor := TMarkdownProcessor.CreateDialect(
      FViewerSettings.ProcessorDialect);
    Try
      //Safe mode by default: native HTML is neutralized unless the user opted in.
      LMarkdownProcessor.AllowUnsafe := FViewerSettings.AllowUnsafeHTML;
      //Optional syntax highlighting of fenced code blocks. The form owns the
      //emitter, so we detach it before freeing the processor (TConfiguration
      //frees its codeBlockEmitter).
      if FCodeHighlightEmitter <> nil then
      begin
        LBackground := ColorToRGB(HtmlViewer.DefBackground);
        LDark := (GetRValue(LBackground) * 299 + GetGValue(LBackground) * 587 +
          GetBValue(LBackground) * 114) div 1000 < 128;
        if LDark then
          LForeground := clWhite
        else
          LForeground := clBlack;
        FCodeHighlightEmitter.SetTheme(LDark, HtmlViewer.DefBackground, LForeground,
          FViewerSettings.HTMLFontName, FViewerSettings.HTMLFontSize);
        LMarkdownProcessor.Config.codeBlockEmitter := FCodeHighlightEmitter;
      end;
      Result := LMarkdownProcessor.Process(AMdContent);
      Result := CSSContent+Result;
    Finally
      if FCodeHighlightEmitter <> nil then
        LMarkdownProcessor.Config.codeBlockEmitter := nil;
      LMarkdownProcessor.Free;
    End;
  end
  else
  begin
    //No transform required: the source is already HTML.
    //NB: the fallback content is a parameter and no longer the FHtmlContent
    //field. The index pane calls this with FMdIndexContent, so with an HTML
    //index file it used to receive the HTML of the *main* document.
    Result := AHtmlContent;
  end;
end;

procedure TMainForm.TransformTo(const AHTMLViewer: THtmlViewer;
  const AMdContent, AHtmlContent: string; const AReloadImage: Boolean;
  const APreservePosition: Boolean);
var
  LHtml: string;
begin
  LHtml := TransformMDToHTML(AMdContent, AHtmlContent);
  //Load html content into HtmlViewer
  ShowMarkdownAsHTML(AHTMLViewer, LHtml, AReloadImage, APreservePosition);
end;


function TMainForm.TryLoadCSS(const AFileName: TFileName): Boolean;
begin
  Result := FileExists(AFileName);
  if Result then
    LoadCSS(AFileName);
end;

procedure TMainForm.ShowMarkdownAsHTML(const AHTMLViewer: THTMLViewer;
  const AHTMLContent: string; const AReloadImages: Boolean;
  const APreservePosition: Boolean);
var
  LOldPos: Integer;
begin
  //NB: re-entrancy guard. dmResources.HtmlViewerImageRequest calls
  //Application.ProcessMessages to keep the UI responsive (and to let ESC stop
  //the loading) while the images are fetched: that pumps the message queue in
  //the middle of the HTML layout, so a user action - a click on a link, the
  //mouse wheel, a refresh - could start a second rendering while the first one
  //is still running. Re-entering LoadFromString there is a reliable way to get
  //an Access Violation.
  if FRendering then
    Exit;

  //NB: read the scroll position *before* Clear, which resets it to zero:
  //reading it afterwards would always restore the top of the document.
  LOldPos := AHtmlViewer.VScrollBarPosition;
  if AReloadImages then
    AHtmlViewer.clear;
  if AHTMLContent = '' then
    Exit;
  //Load HTML content into HTML-Viewer
  FRendering := True;
  try
    AHtmlViewer.DefFontSize := FViewerSettings.HTMLFontSize;
    AHtmlViewer.DefFontName := FViewerSettings.HTMLFontName;
    AHtmlViewer.LoadFromString(AHTMLContent);
    dmResources.StopLoadingImages(False);
  finally
    FRendering := False;
    if APreservePosition then
      AHtmlViewer.VScrollBarPosition := LOldPos;
  end;
  AHtmlViewer.Update;
end;

function TMainForm.LoadIndex(const AFileName: TFileName): Boolean;
begin
  Result := FileExists(AFileName);
  if Result then
  begin
    if CurrentIndexFileName <> AFileName then
    begin
      //Set content variable based on Extension of FileName
      if IsFileNameWithExt(AFileName, AHTMLFileExt) then
      begin
        //load html content
        FHtmlIndexContent := TryLoadTextFile(AFileName);
        //empty md content
        FMdIndexContent := '';
        FMdIndexFileName := '';
        //change page to show Index
        PageControl.ActivePage := tsIndex;
      end
      else
      begin
        //empty html content
        FHtmlIndexContent := '';
        //load md content
        FMdIndexContent := TryLoadTextFile(AFileName);
        FMdIndexFileName := AFileName;
      end
    end;

    CurrentIndexFileName := AFileName;
  end
  else
  begin
    FMdIndexContent := '';
    CurrentIndexFileName := '';
  end;
end;

procedure TMainForm.LoadAndTransformFile(const AFileName: TFileName);
begin
  Screen.Cursor := crHourGlass;
  try
     if Load(AFileName) then
     begin
       TransformTo(HtmlViewer, FMdContent, FHtmlContent, True, False);
     end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainForm.LoadAndTransformFileIndex(const AFileName: TFileName);
begin
  if LoadIndex(AFileName) then
    TransformTo(HtmlViewerIndex, FMdIndexContent, FHtmlIndexContent, True, False);
end;

procedure TMainForm.ActionListUpdate(Action: TBasicAction;
  var Handled: Boolean);
var
  InitialDir: string;
  LFileName: TFileName;
begin
  if not FFirstTime then
  begin
    FFirstTime := True;

    //automatically load the file that is passed as the first parameter
    LFileName := ParamStr(1);
    if FileExists(LFileName) then
    begin
      //Load file passed at command line
      InitialDir := ExtractFilePath(LFileName);
      LoadAndTransformFile(LFileName);
    end
    else
      InitialDir := '.';

    //Initialize Open and Save Dialog with application path
    InitDialog(OpenDialog, InitialDir);
    InitDialog(SaveDialog, InitialDir);
    InitPDFDialog(SaveDialogPDF, InitialDir);
    InitHTMLDialog(SaveDialogHTML, InitialDir);

    //Check for new version available.
    //NB: the HTTP call runs in background. Performing it here, inside an
    //action-update handler, used to freeze the UI at startup whenever the
    //network was slow or unreachable.
    if FViewerSettings.IsTimeToCheckNewVersion then
      CheckNewSetupAsync(
        procedure(ACurrentVersion, ANewVersion: string)
        begin
          if StyledMessageDlg(Format(NEW_VERSION_AVAILABLE,
            [ACurrentVersion, ANewVersion]),
            TMsgDlgType.mtWarning,
            [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo, TMsgDlgBtn.mbCancel], 0) = mrYes then
            ShowAboutForm(DialogPosRect, Title_MDHViewer, True);
        end);
  end;

  if FRememberToResize then
  begin
    FRememberToResize := False;
    TransformTo(HtmlViewer, FMdContent, FHtmlContent, True, False);
  end;
  UpdateGui;
end;

procedure TMainForm.LoadCSS(const AFileName: TFileName);
var
  LExt: string;
begin
  if FileExists(AFileName) then
  begin
    if FCurrentCSSFileName <> AFileName then
    begin
      //Set content variable based on Extension of FileName
      LExt := ExtractFileExt(AFileName);
      if SameText(LExt, '.css') then
      begin
        //load css content
        FCssContent := TryLoadTextFile(AFileName);
      end
      else
      begin
        //empty css content
        FCssContent := '';
      end;
    end;
    CurrentCSSFileName := AFileName;
  end;
end;

procedure TMainForm.ProcessorDialectComboBoxSelect(Sender: TObject);
var
  LDialect: TMarkdownProcessorDialect;
begin
  LDialect := TMarkdownProcessorDialect(ProcessorDialectComboBox.ItemIndex);
  if FViewerSettings.ProcessorDialect <> LDialect then
  begin
    FViewerSettings.ProcessorDialect:= LDialect;
    WriteSettingsToIni;
    TransformTo(HtmlViewer, FMdContent, FHtmlContent, False, False);

    TransformTo(HtmlViewerIndex, FMdIndexContent, FHtmlIndexContent, False, False);
  end;
end;

procedure TMainForm.SetCurrentCSSFileName(const Value: TFileName);
begin
  FCurrentCSSFileName := Value;
  if FCurrentCSSFileName = '' then
    FCssContent := '';
end;

procedure TMainForm.SetCurrentFileName(const AValue: TFileName);
begin
  if FCurrentFileName <> AValue then
  begin
    FCurrentFileName := AValue;
    FViewerSettings.CurrentFileName := AValue;
    OpenDialog.FileName := FCurrentFileName;
  end;
end;

procedure TMainForm.SetCurrentIndexFileName(const AValue: TFileName);
begin
  if FCurrentIndexFileName <> AValue then
  begin
    FCurrentIndexFileName := AValue;
    FViewerSettings.CurrentIndexFileName := AValue;
    if AValue = '' then
      FMdIndexContent := '';
  end;
end;

procedure TMainForm.SetHTMLFontName(const Value: string);
begin
  if (Value <> HTMLViewer.DefFontName) then
  begin
    HTMLViewer.DefFontName := Value;
    HtmlViewerIndex.DefFontName := Value;
    FViewerSettings.HTMLFontName := Value;
  end;
  FHTMLFontName := Value;
end;

procedure TMainForm.SetHTMLFontSize(const Value: Integer);
begin
  if (Value >= MinfontSize) and (Value <= MaxfontSize) then
  begin
    HTMLViewer.DefFontSize := Value;
    HTMLViewerIndex.DefFontSize := Value;
    FViewerSettings.HTMLFontSize := Value;
  end;
  FHTMLFontSize := Value;
end;

procedure TMainForm.SetShowToolbarCaptions(const Value: Boolean);
begin
  ToolBar.ShowCaptions := Value;
  If not Value then
  begin
    ToolBar.ButtonHeight := Round(32 * ScaleFactor);
    ToolBar.ButtonWidth := ToolBar.ButtonHeight;
  end
  else
  begin
    ToolBar.ButtonHeight := Round(50 * ScaleFactor);
    ToolBar.ButtonWidth := Round(FViewerSettings.ToolbarButtonWidth * ScaleFactor);
  end;
  ToolBar.Height := ToolBar.ButtonHeight + Round(4 * ScaleFactor);
  paTop.Height := ToolBar.Height;
  FShowToolbarCaptions := Value;
  AdjustConstraint;
end;

procedure TMainForm.SetUseColoredIcons(const Value: Boolean);
begin
  if FUseColoredIcons <> Value then
  begin
    if Value then
    begin
      ToolBar.Images := SVGIconImageListColored;
      ActionList.Images := SVGIconImageListColored;
    end
    else
    begin
      ToolBar.Images := SVGIconImageList;
      ActionList.Images := SVGIconImageList;
      UpdateIconsColorByStyle;
    end;
    FUseColoredIcons := Value;
  end;
end;

function TMainForm.GetIndexOfWorkingFolder(const AWorkingFolder: string): TFileName;
var
  LFileName: TFileName;
begin
  //Search and Index file into Working Folder as Index.md or Index.html
  Result := '';
  LFileName := AWorkingFolder;
  if FMdContent <> '' then
  begin
    FileListBox.Mask := GetFileMasks(AMarkdownFileExt);
    //Search for Index(.markdown extension) file into this folder
    if FindHelpFile(LFileName, 0, 'Index', AMarkdownFileExt) then
      Result := LFileName;
  end
  else
  begin
    FileListBox.Mask := GetFileMasks(AHTMLFileExt);
    //Search for Index(.html extension) file into this folder
    if FindHelpFile(LFileName, 0, 'Index', AHTMLFileExt) then
      Result := LFileName;
  end;
end;

procedure TMainForm.SetWorkingFolder(const Value: string);
begin
  //Set working folder
  if FWorkingFolder <> Value then
  begin
    FWorkingFolder := Value;
    //Set root folder for HTMLViewers
    HTMLViewer.ServerRoot := FWorkingFolder;
    HtmlViewerIndex.ServerRoot := FWorkingFolder;

    //Set root folder of FileListBox
    if not SameText(FWorkingFolder, IncludeTrailingPathDelimiter(FileListBox.Directory)) then
    begin
      FileListBox.Directory := FWorkingFolder;
      FileListBox.Update;
    end;
  end;
end;

procedure TMainForm.HTMLToPDF(const APDFFileName: TFileName);
var
  lHtmlToPdf: TvmHtmlToPdfGDI;
  LOldColor: TColor;
begin
  lHtmlToPdf := TvmHtmlToPdfGDI.Create();
  try
    lHtmlToPdf.PDFMarginLeft := FViewerSettings.PDFPageSettings.MarginLeft / 100;
    lHtmlToPdf.PDFMarginTop := FViewerSettings.PDFPageSettings.MarginTop / 100;
    lHtmlToPdf.PDFMarginRight := FViewerSettings.PDFPageSettings.MarginRight / 100;
    lHtmlToPdf.PDFMarginBottom := FViewerSettings.PDFPageSettings.MarginBottom / 100;
    lHtmlToPdf.PDFScaleToFit := True;
    lHtmlToPdf.PrintOrientation := FViewerSettings.PDFPageSettings.PrintOrientation;
    lHtmlToPdf.DefaultPaperSize := TPDFPaperSize(FViewerSettings.PDFPageSettings.PaperSize);

    //Change the background color of HTML Viewer to create a PDF file with white background
    //when a dark theme is active
    LOldColor := HTMLViewer.DefBackground;
    try
      SendMessage(HTMLViewer.Handle, WM_SETREDRAW, WPARAM(False), 0);
      HTMLViewer.DefBackground := clWhite;
      lHtmlToPdf.SrcViewer := HTMLViewer;

      lHtmlToPdf.PrintPageNumber := False;
      lHtmlToPdf.TextPageNumber := 'Page %d/%d';
      lHtmlToPdf.PageNumberPositionPrint := ppBottom;

      lHtmlToPdf.Execute;
      lHtmlToPdf.SaveToFile(APDFFileName);
    finally
      HTMLViewer.DefBackground := LOldColor;
    end;

  finally
    SendMessage(HTMLViewer.Handle, WM_SETREDRAW, WPARAM(True), 0);
    lHtmlToPdf.Free;
  end;
end;


procedure TMainForm.FileSavedAskToOpen(const AFileName: string);
begin
  if StyledMessageDlg(Format(FILE_SAVED,[AFileName]),
    TMsgDlgType.mtInformation, [mbYes, MbNo], 0) = mrYes then
  begin
    ShellExecute(handle, 'open', PChar(AFilename), nil, nil, SW_SHOWNORMAL);
  end;
end;

procedure TMainForm.acNextPageUpdate(Sender: TObject);
begin
  acNextPage.Enabled := (IndexOfCurrentFile < FOpenedFileList.Count-1);
end;

procedure TMainForm.acPreviousPageUpdate(Sender: TObject);
begin
  acPreviousPage.Enabled := (IndexOfCurrentFile > 0);
end;

procedure TMainForm.acRefreshExecute(Sender: TObject);
begin
  dmResources.StopLoadingImages(False);
  LoadAndTransformFile(FCurrentFileName);
  if (FMdIndexContent <> '') then
  begin
    var LFileName := FCurrentIndexFileName;
    FCurrentIndexFileName := '';
    LoadAndTransformFileIndex(LFileName);
  end;
end;

procedure TMainForm.acRefreshUpdate(Sender: TObject);
begin
  acRefresh.Enabled := FCurrentFileName <> '';
end;

procedure TMainForm.acNextPageExecute(Sender: TObject);
var
  LFileName: TFileName;
begin
  LFileName := FOpenedFileList.Strings[IndexOfCurrentFile+1];
  LoadAndTransformFile(LFileName);
end;

procedure TMainForm.acPreviousPageExecute(Sender: TObject);
var
  LFileName: TFileName;
begin
  LFileName := FOpenedFileList.Strings[IndexOfCurrentFile-1];
  LoadAndTransformFile(LFileName);
end;

procedure TMainForm.UpdateWindowPos;
var
  LDesktop: TRect;
begin
  //Set Bounds of Windows
  Self.ClientWidth := Round(FViewerSettings.WindowWidth * ScaleFactor);
  Self.ClientHeight := Round(FViewerSettings.WindowHeight * ScaleFactor);
  Self.Left := Round(FViewerSettings.WindowLeft * ScaleFactor);
  Self.Top := Round(FViewerSettings.WindowTop * ScaleFactor);

  //The saved position can fall outside the monitors currently connected (the
  //window was last closed on a screen that is no longer attached, or the
  //resolution changed): in that case the form would open invisible, so it is
  //brought back to the center of the work area.
  LDesktop := Screen.DesktopRect;
  if not PtInRect(LDesktop,
    Point(Self.Left + (Self.Width div 2), Self.Top + Round(20 * ScaleFactor))) then
  begin
    Self.Left := Screen.WorkAreaLeft + ((Screen.WorkAreaWidth - Self.Width) div 2);
    Self.Top := Screen.WorkAreaTop + ((Screen.WorkAreaHeight - Self.Height) div 2);
  end;

  Self.WindowState := FViewerSettings.WindowState;
end;

procedure TMainForm.UpdateFromSettings;
begin
  FViewerSettings.ReadSettings;
  UpdateApplicationStyle(FViewerSettings.VCLStyleName);

  {$IFDEF STYLEDCOMPONENTS}
  if FViewerSettings.ButtonDrawRounded then
  begin
    TStyledButton.RegisterDefaultRenderingStyle(btRounded);
    TStyledButtonGroup.RegisterDefaultRenderingStyle(btRounded);
    btIndex.StyleDrawType := btRounded;
    btSearch.StyleDrawType := btRounded;
    btSearchView.StyleDrawType := btRounded;
  end
  else
  begin
    TStyledButton.RegisterDefaultRenderingStyle(btRoundRect);
    TStyledButtonGroup.RegisterDefaultRenderingStyle(btRoundRect);
    btIndex.StyleDrawType := btRoundRect;
    btSearch.StyleDrawType := btRoundRect;
    btSearchView.StyleDrawType := btRoundRect;
  end;
  if FViewerSettings.ToolbarDrawRounded then
  begin
    TStyledToolbar.RegisterDefaultRenderingStyle(btRounded);
    Toolbar.StyleDrawType := btRounded;
  end
  else
  begin
    TStyledToolbar.RegisterDefaultRenderingStyle(btRoundRect);
    Toolbar.StyleDrawType := btRoundRect;
  end;
  {$ENDIF}
  Toolbar.ButtonWidth := Round(FViewerSettings.ToolbarButtonWidth * ScaleFactor);

  PageControl.Visible := FViewerSettings.PageControlVisible;
  PageControl.Width := Round(FViewerSettings.PageControlSize * Self.ScaleFactor);

  ProcessorDialectComboBox.ItemIndex := ord(FViewerSettings.ProcessorDialect);
  HTMLFontSize := FViewerSettings.HTMLFontSize;
  HTMLFontName := FViewerSettings.HTMLFontName;

  ShowToolbarCaptions := FViewerSettings.ShowToolbarCaptions;
  paTop.Repaint;
  UseColoredIcons := FViewerSettings.UseColoredIcons;

  TransformTo(HtmlViewer, FMdContent, FHtmlContent, True, True);

  TransformTo(HtmlViewerIndex, FMdIndexContent, FHtmlIndexContent, True, True);
end;

function TMainForm.GetToolbarWidth: Integer;
var
  I: Integer;
begin
  Result := 0;
  For I := 0 to Toolbar.ButtonCount -1 do
    if Toolbar.Buttons[I].Visible then
      Result := Result + Toolbar.Buttons[I].Width + Round(2 * ScaleFactor);
end;

procedure TMainForm.AdjustConstraint;
begin
  if FViewerSettings.ShowDialectSelection then
    Self.Constraints.MinWidth := GetToolbarWidth + ProcessorDialectComboBox.Width
  else
    Self.Constraints.MinWidth := GetToolbarWidth;
end;

procedure TMainForm.UpdateGui;
begin
  //takes care of updating the user interface
  tsIndex.TabVisible := FCurrentIndexFileName <> '';

  //show/hide action management
  if PageControl.Visible then
  begin
    //we enable hide action when pageControl is enabled
    btShowHide.Action := acHide;
    acHide.Enabled := True;
    acShow.Enabled := False;
  end
  else
  begin
    //we enable hide action when pageControl is disabled
    btShowHide.Action := acShow;
    acShow.Enabled := True;
    acHide.Enabled := False;
  end;

  //set visible ProcessorDialectLabel and ProcessorDialectComboBox
  if DialectSelectionVisible then
  begin
    ToolBar.Margins.Right := ProcessorDialectComboBox.Width + Round(10 * ScaleFactor);
    ProcessorDialectComboBox.Visible := True;
    ProcessorDialectLabel.Visible := True;
  end
  else
  begin
    ToolBar.Margins.Right := 0;
    ProcessorDialectComboBox.Visible := False;
    ProcessorDialectLabel.Visible := False;
  end;
end;

procedure TMainForm.WriteSettingsToIni;
begin
  FViewerSettings.PageControlVisible := PageControl.Visible;
  FViewerSettings.PageControlSize := Round(PageControl.Width / FScaleFactor);
  FViewerSettings.WriteSettings;
end;


procedure TMainForm.UpdateIconsColorByStyle;
begin
  if FViewerSettings.UseDarkStyle then
    SVGIconImageList.FixedColor := clWhite
  else if FVCLStyleName = 'Windows' then
    SVGIconImageList.FixedColor := RGB(53,126,199) //Windows Blue
  else
    SVGIconImageList.FixedColor := clBlack;
end;

procedure TMainForm.UpdateApplicationStyle(const AVCLStyleName: string);
begin
  if AVCLStyleName <> '' then
  begin
    FVCLStyleName := AVCLStyleName;
    if StyleServices.Enabled then
      TStyleManager.SetStyle(AVCLStyleName);

    UpdateIconsColorByStyle;
  end;
end;

procedure TMainForm.UpdateCaption;
var
  LTitleAndVersion: string;
begin
  //The version is read from the VERSIONINFO resource only the first time:
  //UpdateCaption is called on every file load and the value never changes.
  if FVersionString = '' then
    FVersionString := GetVersionString(GetModuleLocation(), '%d.%d.%d');
  LTitleAndVersion := Format('%s (Ver. %s)', [Application.Title, FVersionString]);
  if CurrentFileName <> '' then
    Caption := Format('%s - %s', [LTitleAndVersion, CurrentFileName])
  else
    Caption := LTitleAndVersion;
end;

procedure TMainForm.WMCopyData(var Message: TMessage);
var
  p : PCopyDataStruct;
  LFilePath, LFileName: string[255];
  LFullName: string;
  r :  PRecToPass;
  LRecordW: THelpInfoToPassW;
begin
  Message.Result := 0;
  p := PCopyDataStruct( Message.lParam );
  if p = nil then
  begin
    ShowMessage(ERR_MSG_RECEIVED);
    Exit;
  end;

  //Any process can send a WM_COPYDATA to this window: accept only messages
  //carrying one of our signatures and a payload of exactly the expected size,
  //otherwise reading the record would go past the end of the buffer.
  LFullName := '';
  if (p^.dwData = MD_HELP_COPYDATA_ID_W) and
    (p^.cbData = SizeOf(THelpInfoToPassW)) and (p^.lpData <> nil) then
  begin
    //Unicode message: paths with characters outside the system codepage arrive
    //intact. Copied locally and forcibly terminated, so that a malformed
    //payload cannot make the string read past the buffer.
    Move(p^.lpData^, LRecordW, SizeOf(LRecordW));
    LRecordW.FilePath[High(LRecordW.FilePath)] := #0;
    LRecordW.FileName[High(LRecordW.FileName)] := #0;
    LFullName := StripQuotes(string(PWideChar(@LRecordW.FilePath[0])) +
      string(PWideChar(@LRecordW.FileName[0])));
  end
  else if (p^.dwData = MD_HELP_COPYDATA_ID) and
    (p^.cbData = SizeOf(THelpInfoToPass)) and (p^.lpData <> nil) then
  begin
    //Legacy message, sent by a client library that predates the Unicode one
    r := PRecToPass(p^.lpData);
    LFilePath := r^.FilePath;
    LFileName := r^.FileName;
    {$WARN IMPLICIT_STRING_CAST OFF}
    LFullName := StripQuotes(String(LFilePath+LFileName));
  end;

  if LFullName <> '' then
  begin
    LoadAndTransformFile(LFullName);
    if Self.WindowState = wsMinimized then
      Self.WindowState := wsNormal;
    //Tell the sender the request has been handled
    Message.Result := 1;
  end;
  //A WM_COPYDATA without our signature belongs to somebody else: it is ignored
  //silently, leaving Message.Result = 0.
end;

procedure TMainForm.ManageExceptions(Sender: TObject; E: Exception);
begin
  //This is an event-handler for exceptions that replace Delphi standard handler
  if E is EAccessViolation then
  begin
    if StyledMessageDlg(
      Format('Unexpected Error: %s%s',[sLineBreak,E.Message]),
      TMsgDlgType.mtError,
      [TMsgDlgBtn.mbOK, TMsgDlgBtn.mbAbort], 0) = mrAbort then
    Application.Terminate;
  end
  else
  begin

    StyledMessageDlg(
      Format('Error: %s%s',[sLineBreak,E.Message]),
      TMsgDlgType.mtError,
      [TMsgDlgBtn.mbOK, TMsgDlgBtn.mbHelp], 0);
  end;
end;

procedure RegisterTrxProperties(AUserLanguage: TAppLanguage);
var
  LAppFileName: TFileName;
  LAppPath, LTrxRepository: string;
begin
  LAppFileName := Application.ExeName;
  LAppPath := ExtractFilePath(LAppFileName);
  LTrxRepository := ExtractFilePath(LAppFileName)+'..\TrxRepository';
  InitTrxSupport(
    LTrxRepository, //Translation Path Repository
    AUserLanguage, //User Language
   {$IFDEF DEBUG}True,{$ELSE}False,{$ENDIF} //Update Repository
    mlEnglish //GUI Language used at design-time
    );

  RegisterTrxProperty('TComponent', 'Caption', txString);
  RegisterTrxProperty('TComponent', 'Text', txString);
  RegisterTrxProperty('TComponent', 'Hint', txString);
  RegisterTrxProperty('TRadioGroup', 'Items', txStrings);
  RegisterTrxProperty('TListBox', 'Items', txStrings);
  RegisterTrxProperty('TMemo', 'Lines', txStrings);
  RegisterTrxProperty('TComboBox', 'Items', txStrings);
  RegisterTrxProperty('TButtonGroup','Items', txButtonGroup);
  RegisterTrxProperty('TListView','Columns', txListView);
  RegisterTrxProperty('TStatusBar','Panels', txStatusPanel);
  RegisterTrxProperty('TDbGrid','Columns', txDbGrid);

  //Tutte le classi passate alla RegisterTrxProperty vanno registrate
  RegisterClasses([
    TComponent,
    TRadioGroup,
    TListBox,
    TMemo,
    TButtonGroup,
    TComboBox
    ]);

{$IFDEF DEBUG}
  //Update Messages
  MDHelpView.Messages.RegisterMessages;
{$ENDIF}
end;

initialization
  //Create and read settings
  FViewerSettings := TViewerSettings.CreateSettings;
  //Initialize language
  RegisterTrxProperties(FViewerSettings.GUILanguage);

  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}

end.
