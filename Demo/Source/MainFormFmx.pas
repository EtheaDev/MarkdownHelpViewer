unit MainFormFmx;

interface

uses
  System.SysUtils
  , System.Types
  , System.UITypes
  , System.Classes
  , System.Variants
  , FMX.Types
  , FMX.Controls
  , FMX.Forms
  , FMX.Graphics
  , FMX.Dialogs
  , FMX.Memo.Types
  , FMX.StdCtrls
  , FMX.Controls.Presentation
  , FMX.ScrollBox
  , FMX.Memo
  , FMX.WebBrowser
  , MarkdownProcessor
  ;

resourcestring
  FILE_NOT_FOUND = 'File "%s" not found!';
  HELP_FILE_NOT_SET = 'Help file not assigned into Application.HelpFile';
  MD_HELP_VIEWER_NOT_FOUND = 'Markdown Help Viewer not found: please install it to show the Help!';

type
  TMainForm = class(TForm)
    MarkDownMemo: TMemo;
    HTMLMemo: TMemo;
    TransformButton: TButton;
    WebBrowser: TWebBrowser;
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure TransformButtonClick(Sender: TObject);
    procedure WebBrowserShouldStartLoadWithRequest(ASender: TObject;
      const URL: string);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses
  System.IOUtils
{$IFDEF MSWINDOWS}
  , System.Win.Registry
  , Winapi.ShellAPI
  , Winapi.Windows
  , MarkDownHelpViewer
{$ENDIF}
  , MarkdownUtils
  ;

{$R *.fmx}

{$IFDEF MSWINDOWS}
procedure ShowMarkdownFile(const AFileName: TFileName;
  const AHelpString: string; const AContext: Integer = 0);
var
  LViewerExeName: TFileName;
  LRegistry: TRegistry;
begin
  //Check the presence of Markdown file to show
  if not FileExists(AFileName) then
    raise EInOutError.CreateFmt(FILE_NOT_FOUND, [AFileName]);

  //Read the Viewer from the Registry
  LRegistry := TRegistry.Create;
  Try
    LRegistry.RootKey := HKEY_CLASSES_ROOT;
    LRegistry.OpenKeyReadOnly('\Applications\MDHelpViewer.exe\Shell\Open\Command');
    LViewerExeName := LRegistry.ReadString('');
    LViewerExeName := StringReplace(LViewerExeName, ' "%1"', '', []);
    if LViewerExeName <> '' then
      LViewerExeName := Copy(LViewerExeName, 2, Length(LViewerExeName)-2);
    LRegistry.CloseKey;
  Finally
    LRegistry.free;
  End;

  if FileExists(LViewerExeName) then
  begin
    if not SendWMCOPYToProcess(LViewerExeName, AFileName, AContext) then
    begin
      ShellExecute( 0, 'open' , PChar(LViewerExeName), PChar(AFileName),
        PChar(ExtractFilePath(AFileName)), SW_SHOW );
    end;
  end
  {$IFDEF DEBUG}
  else
    raise Exception.Create(MD_HELP_VIEWER_NOT_FOUND);
  {$ENDIF}
end;
{$ENDIF}

procedure TMainForm.FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
{$IFDEF MSWINDOWS}
var
  LFileName: string;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  if Key = vkF1 then
  begin
    LFileName := TPath.Combine(TPath.GetDocumentsPath,'ReadMe.md');
    ShowMarkdownFile(LFileName, 'README',0);
  end;
{$ENDIF}
end;

procedure TMainForm.TransformButtonClick(Sender: TObject);
var
  LProcessor: TMarkdownProcessor;
  LHTML, LRootFolder: string;
begin
  LProcessor := TMarkdownProcessor.CreateDialect(mdCommonMark);
  try
    LHTML := LProcessor.process(MarkDownMemo.Lines.Text);
    HTMLMemo.Lines.Text := LHTML;
    LRootFolder := 'file:///D:\ETHEA\MarkDownHelpViewer';
    WebBrowser.LoadFromStrings(LHTML, LRootFolder);
  finally
    LProcessor.Free;
  end;
end;

procedure TMainForm.WebBrowserShouldStartLoadWithRequest(ASender: TObject;
  const URL: string);
begin
  ;
end;

end.
