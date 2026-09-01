{******************************************************************************}
{                                                                              }
{       Markdown Help Viewer: About Form                                       }
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
unit MDHelpView.About;

interface

uses
  WinApi.Messages, System.SysUtils, System.Variants, System.Types,
  System.Classes, Vcl.Controls, Vcl.Forms,
  Vcl.ImgList, System.ImageList, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.Imaging.GIFImg, Vcl.ComCtrls,
  SVGIconImage,
  {$IFDEF STYLEDCOMPONENTS}
  Vcl.StyledComponentsHooks,
  {$ENDIF}
  MDHelpView.FormsHookTrx;

resourcestring
  Title_MDHViewer = 'Markdown Help Viewer';

const
  HELP_URL = 'https://github.com/EtheaDev/MarkdownHelpViewer';
  GIT_HUB_URL = 'https://github.com/EtheaDev/MarkdownHelpViewer';
  SETUP_FILENAME = 'MarkDownHelpViewerSetup.exe';
type
  TFrmAbout = class(TFormHook)
    BottomPanel: TPanel;
    btnOK: TButton;
    TitleLabel: TLabel;
    LabelVersion: TLabel;
    MemoCopyRights: TMemo;
    btnIssues: TButton;
    btnCheckUpdates: TButton;
    LinkLabel1: TLinkLabel;
    SVGIconImage1: TSVGIconImage;
    SVGIconImage2: TSVGIconImage;
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnIssuesClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnCheckUpdatesClick(Sender: TObject);
    procedure LinkLabel1LinkClick(Sender: TObject; const Link: string;
      LinkType: TSysLinkType);
    procedure FormShow(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    FTitle: string;
    FStopDownload: Boolean;
    FExpectedSize: Int64;
    FProgressBar: TProgressBar;
    procedure SetTitle(const Value: string);
    procedure DownloadAndInstallSetup;
    procedure UpdateGUI(const ADownloadInProgress: Boolean);
    procedure WMCopyData(var Message: TMessage); message WM_COPYDATA;
  public
    procedure HttpClientReceiveData(const Sender: TObject;
      AContentLength, AReadCount: Int64; var Abort: Boolean);
    procedure DisableButtons;
    property Title: string read FTitle write SetTitle;
  end;

procedure ShowAboutForm(const AParentRect: TRect;
  const ATitle: string; const DownloadNewSetup: Boolean);
procedure HideAboutForm;

function AcceptNewSetup(const AShowMsg: Boolean): Boolean;
function GetCurrentVersion(const AApplicationExeName: TFileName): string;

//Automatic (startup) check for a new version. The HTTP call runs in a
//background task and AOnNewVersionAvailable is invoked in the main thread only
//when a newer version exists, receiving current and new version.
//Failures are silent by design: an automatic check must not interrupt the user
//with an error dialog. Use AcceptNewSetup for the manual check, where a
//blocking call and a retry dialog are what the user asked for.
procedure CheckNewSetupAsync(const AOnNewVersionAvailable: TProc<string, string>);

implementation

uses
  WinApi.ShellApi
  , System.UITypes
  , System.Threading
  , Winapi.Windows
  , Vcl.Dialogs
  , Vcl.Graphics
  , MDHelpView.Misc
  , MDHelpView.Main
  , GitHubAPI
  , MDHelpView.Messages
  , Vcl.StyledTaskDialog
  ;

{$R *.dfm}

var
 _HttpClient: TGitHubHttpClient;

function GetCurrentVersion(const AApplicationExeName: TFileName): string;
var
  LMajorVersion, LMinorVersion, LRelease: integer;
  InfoSize, Wnd: DWORD;
  VerBuf: Pointer;
  FI: PVSFixedFileInfo;
  VerSize: DWORD;
begin
  LMajorVersion := 0;
  LMinorVersion := 0;
  LRelease := 0;

  InfoSize := GetFileVersionInfoSize(PChar(AApplicationExeName), Wnd);
  if InfoSize > 0 then
  begin
    GetMem(VerBuf, InfoSize);
    try
      if GetFileVersionInfo(PChar(AApplicationExeName), Wnd, InfoSize, VerBuf) then
      begin
        if VerQueryValue(VerBuf, '\', Pointer(FI), VerSize) then
        begin
          LMajorVersion := HIWORD(FI.dwFileVersionMS);
          LMinorVersion := LOWORD(FI.dwFileVersionMS);
          LRelease := HIWORD(FI.dwFileVersionLS);
        end;
      end;
    finally
      FreeMem(VerBuf);
    end;
  end;
  Result := Format('v%d.%d.%d', [LMajorVersion,LMinorVersion,LRelease]);
end;

function AcceptNewSetup(const AShowMsg: Boolean): Boolean;
var
  LCurrentVersion, LNewVersion: string;
begin
  while True do
  begin
    Screen.Cursor := crHourGlass;
    try try
      LCurrentVersion := GetCurrentVersion(Application.ExeName);
      if not Assigned(_HttpClient) then
        _HttpClient := TGitHubHttpClient.Create(nil);
      Result := _HttpClient.IsNewVersionAvailable(
        LCurrentVersion, GIT_HUB_URL, LNewVersion);
      Break;  
    except
      on E: ECheckNewVersionException do
      begin
        if StyledTaskMessageDlg(CHECK_FOR_UPDATE_BTN,
          Format(Error_Checking_New_Version, [E.Message]),
          TMsgDlgType.mtWarning,
          [mbAbort, mbRetry],0, mbAbort) = mrRetry then
            Continue
          else
            Exit(False);  
      end
      else
        raise;
    end;
    finally
      Screen.Cursor := crDefault;
    end;
  end;
  if Result then
  begin
    Result := StyledMessageDlg(Format(NEW_VERSION_AVAILABLE,
      [LCurrentVersion, LNewVersion]),
      TMsgDlgType.mtWarning, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo, TMsgDlgBtn.mbCancel],
      0) = mrYes;
  end
  else if AShowMsg then
    StyledMessageDlg(Format(NEW_VERSION_NOT_AVAILABLE,
      [LCurrentVersion]), TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], 0);
end;

procedure CheckNewSetupAsync(const AOnNewVersionAvailable: TProc<string, string>);
begin
  if not Assigned(AOnNewVersionAvailable) then
    Exit;
  TTask.Run(
    procedure
    var
      LClient: TGitHubHttpClient;
      LCurrentVersion, LNewVersion: string;
      LAvailable: Boolean;
    begin
      LNewVersion := '';
      LCurrentVersion := GetCurrentVersion(Application.ExeName);
      //A dedicated client: _HttpClient belongs to the main thread, which uses
      //it for the manual check and for the setup download.
      LClient := TGitHubHttpClient.Create(nil);
      try
        try
          LAvailable := LClient.IsNewVersionAvailable(LCurrentVersion,
            GIT_HUB_URL, LNewVersion);
        except
          //No network, proxy, timeout, unexpected answer: an automatic check
          //stays silent. The user can always check manually from the About box.
          LAvailable := False;
        end;
      finally
        LClient.Free;
      end;
      if LAvailable and not Application.Terminated then
        TThread.Queue(nil,
          procedure
          begin
            if not Application.Terminated then
              AOnNewVersionAvailable(LCurrentVersion, LNewVersion);
          end);
    end);
end;

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

procedure ShowAboutForm(const AParentRect: TRect; const ATitle: string;
  const DownloadNewSetup: Boolean);
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
    if (AparentRect.Left <> 0) and (AparentRect.Right <> 0) then
    begin
      LFrm.Left := (AParentRect.Left + AParentRect.Right - LFrm.Width) div 2;
      LFrm.Top := (AParentRect.Top + AParentRect.Bottom - LFrm.Height) div 2;
    end;
    LFrm.Title := ATitle;
    LFrm.FStopDownload := not DownloadNewSetup;
    LFrm.ShowModal;
  finally
    LFrm.Free;
  end;
end;

procedure TFrmAbout.btnCheckUpdatesClick(Sender: TObject);
begin
  if not FStopDownload then
    FStopDownload := True
  else if AcceptNewSetup(True) then
    DownloadAndInstallSetup;
end;

procedure TFrmAbout.btnOKClick(Sender: TObject);
begin
  Close();
end;

procedure TFrmAbout.btnIssuesClick(Sender: TObject);
begin
   ShellExecute(Handle, 'open',
    PChar(HELP_URL+'/issues'),
    nil, nil, SW_SHOW);
end;

procedure TFrmAbout.DisableButtons;
begin
  btnOK.OnClick := nil;
  btnCheckUpdates.OnClick := nil;
end;

procedure TFrmAbout.UpdateGUI(const ADownloadInProgress: Boolean);
begin
  btnIssues.Enabled := not ADownloadInProgress;
  btnOK.Enabled := not ADownloadInProgress;
  FProgressBar.Visible := ADownloadInProgress;
  if ADownloadInProgress then
  begin
    FStopDownload := False;
    btnCheckUpdates.Caption := STOP_DOWNLOAD_BTN;
  end
  else
  begin
    btnCheckUpdates.Caption := CHECK_FOR_UPDATE_BTN;
  end;
end;

procedure TFrmAbout.DownloadAndInstallSetup;
var
  AOutFileName: TFileName;
begin
  UpdateGUI(True);
  try
    //Start Download
    var LSize := _HttpClient.DownloadLatestSetup(
      SETUP_FILENAME,
      HttpClientReceiveData,
      AOutFileName);
    if LSize <> FExpectedSize then
    begin
      if FStopDownload then
        raise Exception.Create(DOWNLOAD_STOPPED)
      else
        raise Exception.Create(DOWNLOADING_ERROR);
    end;
  finally
    UpdateGUI(False);
  end;
  ShellExecute(0, 'open', PChar(AOutFileName), nil, nil, SW_SHOWNORMAL);
  Application.Terminate;
end;

procedure TFrmAbout.FormActivate(Sender: TObject);
begin
  if not FStopDownload then
    DownloadAndInstallSetup;
end;

procedure TFrmAbout.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FStopDownload := True;
  Action := caFree;
end;

procedure TFrmAbout.FormCreate(Sender: TObject);
var
  FileVersionStr: string;
begin
  TitleLabel.Font.Height := Round(TitleLabel.Font.Height * 1.6);

  FileVersionStr := MDHelpView.Misc.GetVersionString(GetModuleLocation(), '%d.%d.%d');
  {$IFDEF WIN32}
  LabelVersion.Caption := Format('Ver. %s (32bit)', [FileVersionStr]);
  {$ELSE}
  LabelVersion.Caption := Format('Ver. %s (64bit)', [FileVersionStr]);
  {$ENDIF}
  //ProgressBar for Download
  FProgressBar := TProgressBar.Create(Self);
  FProgressBar.Visible := False;
  FProgressBar.Align := alBottom;
  FProgressBar.Height := Round(6 * ScaleFactor);
  FProgressBar.Parent := BottomPanel;
  FProgressBar.MarqueeInterval := 1;
  FProgressBar.Max := 100;
  btnCheckUpdates.Visible := True;
  FStopDownload := True;
end;

procedure TFrmAbout.FormShow(Sender: TObject);
begin
  if btnOK.CanFocus then
    btnOK.SetFocus;
end;

procedure TFrmAbout.HttpClientReceiveData(const Sender: TObject; AContentLength,
  AReadCount: Int64; var Abort: Boolean);
begin
  if (AContentLength > 0) then
  begin
    if FExpectedSize = 0 then
      FExpectedSize := AContentLength;
    var LNewPos := Round(AReadCount * 100 / AContentLength)+10;
    if LNewPos <= 100 then
      FProgressBar.Position := LNewPos;
  end;
  Application.ProcessMessages;
  //Abort Download if Stop Download Button was pressed
  Abort := FStopDownload;
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

procedure TFrmAbout.WMCopyData(var Message: TMessage);
begin
  Close;
  MainForm.WMCopyData(Message);
end;

initialization
  _HttpClient := nil;

finalization
  FreeAndNil(_HttpClient);

end.
