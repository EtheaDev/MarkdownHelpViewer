{******************************************************************************}
{                                                                              }
{       This units implements the interfaces for the Help Viewer               }
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
unit MarkDownHelpViewer;

{$WARN IMPLICIT_STRING_CAST_LOSS OFF}
{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Winapi.Windows
  , System.SysUtils
  , System.Classes
  ;

type
  THelpInfoToPass = packed record
    FilePath: string[255]; //Path of help file to open
    FileName: string[255]; //Help file to open
    Context : integer; //Context
  end;
  PRecToPass = ^THelpInfoToPass;

//To register the position of the Viewer if not installed
procedure RegisterMDViewerLocation(AViewerExeFileName: TFileName);

var
  AMarkdownFileExt: TArray<String>;
  AHTMLFileExt: TArray<String>;

//File Utilities
function FileWithExtExists(var AFileName: TFileName;
  const AFileExt: array of string): boolean;
function GetFileMasks(const AFileExt: array of string;
  const ASeparator: Char = ';'): string;
function IsFileNameWithExt(const AFileName: TFileName;
  const AFileExt: array of string): boolean;
procedure GetFileNamesWithExtensions(FileNames: TStrings;
  const PathName: string; const Extensions: string;
  FileAttrib : Integer = faArchive or faReadOnly);

implementation

uses
  System.HelpIntfs
  , Winapi.ShellAPI
  , System.Win.Registry
  , Winapi.TlHelp32
  , Winapi.Messages
  ;

resourcestring
  FILE_NOT_FOUND = 'File "%s" not found!';
  HELP_FILE_NOT_SET = 'Help file not assigned into Application.HelpFile';
  MD_HELP_VIEWER_NOT_FOUND = 'Markdown Help Viewer not found: please install it to show the Help!';

type
  TMarkdownHelpViewer = class(TInterfacedObject, ICustomHelpViewer, IExtendedHelpViewer)
  private
    FViewerID: Integer;
    FHelpManager: IHelpManager;
    procedure ShowMarkdownFile(const AFileName: TFileName;
      const AHelpString: string; const AContext: Integer = 0);
    function FindHelpFile(var AFileName: TFileName; const AContext: Integer;
      const HelpKeyword: string): boolean;
  public
    constructor Create;
    destructor Destroy; override;
    { internal support functions }
    function GetHelpFile(const HelpKeyword: string): TFileName; overload;
    function GetHelpFile(const HelpContext:
      {$if CompilerVersion > 31}THelpContext{$else}Integer{$endif}): TFileName; overload;
    function GetIndexFile: TFileName;
    procedure InternalShutDown;
    { ICustomHelpViewer }
    function GetViewerName : string;
    function UnderstandsKeyword(const HelpString: string): Integer;
    function GetHelpStrings(const HelpString: string): TStringList;
    function CanShowTableOfContents: Boolean;
    procedure ShowTableOfContents;
    procedure ShowHelp(const HelpString: string);
    procedure NotifyID(const ViewerID: Integer);
    procedure SoftShutDown;
    procedure ShutDown;
    { IExtendedHelpViewer }
    function UnderstandsTopic(const Topic: string): Boolean;
    procedure DisplayTopic(const Topic: string); overload;
    function UnderstandsContext(const ContextID: {$if CompilerVersion > 31}THelpContext{$else}Integer{$endif};
      const HelpFileName: string): Boolean;
    procedure DisplayHelpByContext(const ContextId: {$if CompilerVersion > 31}THelpContext{$else}Integer{$endif};
      const HelpFileName: string);
    procedure ClearSetup;
    property ViewerID : Integer read FViewerID;
    property HelpManager : IHelpManager read FHelpManager write FHelpManager;
  end;

var
  Markdown_HelpViewer: TMarkdownHelpViewer;
  Markdown_HelpViewerIntf: ICustomHelpViewer;
  _ViewerLocation: TFileName;

type
  TEnumInfo = record
    ProcessID: DWORD;
    HWND: HWND;
  end;
  PEnumInfo = ^TEnumInfo;

function FileWithExtExists(var AFileName: TFileName;
  const AFileExt: array of string): boolean;
var
  I: Integer;
  LExt: string;
  LFileName: TFileName;
begin
  LExt := ExtractFileExt(AFileName);
  if LExt = '' then
    LFileName := AFileName+AFileExt[0]
  else
    LFileName := AFileName;
  Result := FileExists(LFileName);
  if not Result then
  begin
    LFileName := ExtractFilePath(AFileName)+ChangeFileExt(ExtractFileName(AFileName),'');
    for I := Low(AFileExt) to High(AFileExt) do
    begin
      LExt := AFileExt[I];
      LFileName := ChangeFileExt(LFileName, LExt);
      if FileExists(LFileName) then
      begin
        AFileName := LFileName;
        Result := True;
        break;
      end;
    end;
  end
  else
    AFileName := LFileName;
end;

function GetFileMasks(const AFileExt: array of string;
  const ASeparator: Char = ';'): string;
var
  I: Integer;
  LExt: string;
begin
  for I := Low(AFileExt) to High(AFileExt) do
  begin
    LExt := AFileExt[I];
    if I > 0 then
      Result := Result + ASeparator;
    Result := Result + '*'+LExt;
  end;
end;

function IsFileNameWithExt(const AFileName: TFileName;
  const AFileExt: array of string): boolean;
var
  I: Integer;
  LFileExt, LExt: string;
begin
  Result := False;
  LFileExt := ExtractFileExt(AFileName);
  for I := Low(AFileExt) to High(AFileExt) do
  begin
    LExt := AFileExt[I];
    Result := SameText(LFileExt, LExt);
    if Result then
      break;
  end;
end;

procedure GetFileNamesWithExtensions(FileNames: TStrings;
  const PathName: string; const Extensions: string;
  FileAttrib: Integer = faArchive or faReadOnly);
const
  FileMask = '*.*';
var
  Rec: TSearchRec;
  Path: string;
begin
  Path := IncludeTrailingBackslash(PathName);
  if FindFirst(Path + FileMask, FileAttrib, Rec) = 0 then
  begin
    try
      repeat
        if AnsiPos(ExtractFileExt(Rec.Name), Extensions) > 0 then
          FileNames.Add(Rec.Name);
      until FindNext(Rec) <> 0;
    finally
      FindClose(Rec);
    end;
  end;
end;

function EnumWindowsProc(Wnd: DWORD; var EI: TEnumInfo): Bool; stdcall;
var
  PID: DWORD;
begin
  GetWindowThreadProcessID(Wnd, @PID);
  Result := (PID <> EI.ProcessID) or
    (not IsWindowVisible(WND)) or
    (not IsWindowEnabled(WND));

  if not Result then
    EI.HWND := WND;
end;

function FindMainWindow(PID: DWORD): DWORD;
var
  EI: TEnumInfo;
begin
  EI.ProcessID := PID;
  EI.HWND := 0;
  EnumWindows(@EnumWindowsProc, NativeInt(@EI));
  Result := EI.HWND;
end;

function GetHWndByPID(const hPID: THandle): THandle;
begin
  if hPID<>0 then
    Result:=FindMainWindow(hPID)
  else
    Result:=0;
end;

function SendWMCOPYToProcess(const AExeName, AFileName: TFileName;
  AHelpContext: Integer): Boolean;
var
  ProcessName: string; //Process name
  FSnapshotHandle: THandle; //Process snapshot handle
  FProcessEntry32: TProcessEntry32; //Structural information of the process entry
  ContinueLoop: BOOL;
  MyHwnd: THandle;
  cd: TCopyDataStruct;
  LExeFileName: TFileName;
  LParamFilePath: string[255];
  LParamFileName: string[255];
  LRecord: THelpInfoToPass;
begin
  Result := False;
  FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS,0); //Create a process snapshot
  FProcessEntry32.dwSize := Sizeof(FProcessEntry32);
  ContinueLoop := Process32First(FSnapshotHandle,FProcessEntry32); //Get the first process in the system
  LExeFileName := ExtractFileName(AExeName);
  LParamFileName := ExtractFileName(AFileName);
  LParamFilePath := ExtractFilePath(AFileName);
  while ContinueLoop do
  begin
    ProcessName := FProcessEntry32.szExeFile;
    if SameText(ProcessName, LExeFileName) then
    begin
      MyHwnd := GetHWndByPID(FProcessEntry32.th32ProcessID);
      LRecord.FilePath := LParamFilePath;
      LRecord.FileName := LParamFileName;
      LRecord.Context := AHelpContext;
      cd.dwData := 3232;
      cd.cbData := sizeof(LRecord);
      cd.lpData := @LRecord;
      SendMessage(MyHwnd, WM_ACTIVATE, 0, 0);
      SendMessage(MyHwnd, WM_SETFOCUS, 0, 0);
      SetWindowPos(MyHwnd, HWND_TOP, 0, 0, 0, 0, SWP_NoMove or SWP_NoSize);
      SendMessage(MyHwnd, WM_COPYDATA, 0, NativeInt(@cd) );
      Result := True;
    end;
    ContinueLoop := Process32Next(FSnapshotHandle,FProcessEntry32);
  end;
  CloseHandle(FSnapshotHandle); // Release the snapshot handle
end;

{==========================================================================}

{ ICustomHelpViewer. }

{ GetViewerName returns a string that the Help Manager can use to identify
  this Viewer in a UI element asking users to choose among Viewers. }
function TMarkdownHelpViewer.GetViewerName: string;
begin
  Result := 'Help Viewer for Markdown';
end;

{ UnderstandsKeyword is a querying function that the Help Manager calls to
  determine if the Viewer provide helps on a particular keyword string. }
function TMarkdownHelpViewer.UnderstandsKeyword(const HelpString: string): Integer;
var
  HelpFile: string;
begin
  HelpFile := GetHelpFile(HelpString);
  if HelpFile <> '' then
    Result := 1
  else
    Result := 0;
end;

{ GetHelpStrings is used by the Help Manager to display a list of keyword
  matches from which an application's user can select one. It assumes
  that the String List is properly allocated, so this function should
  never return nil. }
function TMarkdownHelpViewer.GetHelpStrings(const HelpString: string): TStringList;
var
  LHelpFile: TFileName;
begin
  Result := TStringList.Create;
  LHelpFile := GetHelpFile('');
  GetFileNamesWithExtensions(Result,
    ExtractFilePath(LHelpFile), GetFileMasks(AMarkdownFileExt));
end;

function TMarkdownHelpViewer.FindHelpFile(
  var AFileName: TFileName;
  const AContext: Integer; const HelpKeyword: string): boolean;
var
  LHelpFileName: TFileName;
  LName, LPath, LKeyWord: string;
begin
  //WARNING: if changing this function, change also TMarkdownViewer.FindHelpFile
  if HelpKeyword <> '' then
    LKeyWord := HelpKeyword
  else if AContext <> 0 then
    LKeyWord := IntToStr(AContext)+'.md'
  else
    LKeyword := '';

  //First, Try the Keyword only
  LPath := ExtractFilePath(AFileName);
  LHelpFileName := LPath+LKeyword;
  Result := FileWithExtExists(LHelpFileName, AMarkdownFileExt) or
    FileWithExtExists(LHelpFileName, AHTMLFileExt);

  if not Result then
  begin
    //Then, try the Help Name and the Keyword (eg.Home1000.ext)
    LName := ChangeFileExt(ExtractFileName(AFileName),'');
    LHelpFileName := LPath+LName+LKeyword;
    Result := FileWithExtExists(LHelpFileName, AMarkdownFileExt) or
      FileWithExtExists(LHelpFileName, AHTMLFileExt);
    if not Result then
    begin
      //At least, try the Help Name and the Keyword with '_' (eg.Home_1000.ext)
      LHelpFileName := LPath+LName+'_'+LKeyword;
      Result := FileWithExtExists(LHelpFileName, AMarkdownFileExt) or
        FileWithExtExists(LHelpFileName, AHTMLFileExt);
    end;
  end;

  if Result then
    AFileName := LHelpFileName;
end;

function TMarkdownHelpViewer.GetIndexFile: TFileName;
var
  LHelpFileName: TFileName;
begin
  //First try with HelpFileName_Index.ext
  LHelpFileName := GetHelpFile('_Index');

  //Try with Index.ext
  if LHelpFileName = '' then
    LHelpFileName := GetHelpFile('Index');

  //Try with Content.md
  if LHelpFileName = '' then
    LHelpFileName := GetHelpFile('Content');
end;

{ CanShowTableOfContents is a querying function that the Help Manager
  calls to determine if the Viewer supports tables of contents. HtmlHelp does. }

function TMarkdownHelpViewer.CanShowTableOfContents: Boolean;
begin
  Result := GetIndexFile <> '';
end;

{ ShowTableOfContents is a command function that the Help Manager uses
  to direct the Viewer to display a table of contents. It is never
  called without being preceded by a call to CanShowTableOfContents. }
procedure TMarkdownHelpViewer.ShowTableOfContents;
begin
  ; //Non viene lanciato perché CanShowTableOfContents ritorna False
end;

procedure TMarkdownHelpViewer.ShowMarkdownFile(const AFileName: TFileName;
  const AHelpString: string; const AContext: Integer = 0);
var
  LViewerExeName: TFileName;
  LRegistry: TRegistry;
begin
  //Check the presence of Markdown file to show
  if not FileExists(AFileName) then
    raise EInOutError.CreateFmt(FILE_NOT_FOUND, [AFileName]);

  //Show the Markdown file with the Markdown Help Viewer:
  if _ViewerLocation <> '' then
    LViewerExeName := _ViewerLocation
  else
  begin
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
  end;

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

procedure TMarkdownHelpViewer.ShowHelp(const HelpString: string);
var
  FileName : string;
begin
  FileName := GetHelpFile(HelpString);
  ShowMarkdownFile(FileName, HelpString, 0);
end;

{ NotifyID is called by the Help Manager after a successful registration
  to provide the Help Viewer with a cookie which uniquely identifies the
  Viewer to the Manager, and can be used in communications between the two. }

procedure TMarkdownHelpViewer.NotifyId(const ViewerId: Integer);
begin
  FViewerID := ViewerID;
end;

procedure RegisterMDViewerLocation(AViewerExeFileName: TFileName);
begin
  _ViewerLocation := AViewerExeFileName;
end;

{ SoftShutDown is called by the help manager to ask the viewer to
  terminate any externally spawned subsystem without shutting itself down. }
procedure TMarkdownHelpViewer.SoftShutDown;
begin
  ;
  if Assigned(FHelpManager) then HelpManager := nil;
end;

{ IExtendedHelpViewer }

{ UnderstandsTopic is called by the Help Manager to ask if the Viewer
  is capable of displaying a topic-based help query for a given topic.
  It's default behavior is to say 'yes'. }
function TMarkdownHelpViewer.UnderstandsTopic(const Topic: string): Boolean;
var
  HelpFile: string;
begin;
  HelpFile := GetHelpFile(Topic);
  Result := HelpFile <> '';
end;

{ DisplayTopic is called by the Help Manager if a Help Viewer claims
  in its response to UnderstandsTopic to be able to provide Topic-based
  help for a particular keyword. }

procedure TMarkdownHelpViewer.DisplayTopic(const Topic: string);
var
  HelpFile: string;
begin
  HelpFile := GetHelpFile('');
  ShowMarkdownFile(HelpFile, Topic);
end;

{ UnderstandsContext is a querying function called by the Help Manager
  to determine if an Extended Help Viewer is capable of providing
  help for a particular context-ID. Like all querying functions in
  this file, the default behavior is to say 'yes' unless overridden by
  a Tester. }

function TMarkdownHelpViewer.UnderstandsContext(
  const ContextId: {$if CompilerVersion > 31}THelpContext{$else}Integer{$endif};
  const HelpFileName: string): Boolean;
begin
  //Accept ContextId if resolve a file markdown with this context
  Result := GetHelpFile(ContextId) <> '';
end;

{ DisplayHelpByContext is used by the Help Manager to request that a
  Help Viewer display help for a particular Context-ID. It is only
  invoked after a successful call to CanShowContext. }

procedure TMarkdownHelpViewer.DisplayHelpByContext(
  const ContextId: {$if CompilerVersion > 31}THelpContext{$else}Integer{$endif};
  const HelpFileName: string);
var
  FileName: TFileName;
begin
  FileName := GetHelpFile(ContextId);
  ShowMarkdownFile(FileName, '', ContextId);
end;

procedure TMarkdownHelpViewer.ShutDown;
begin
  SoftShutDown;
end;

procedure TMarkdownHelpViewer.ClearSetup;
begin
  ;
end;
{==========================================================================}

constructor TMarkdownHelpViewer.Create;
begin
  inherited Create;
  Markdown_HelpViewerIntf := Self;
  ClearSetup;
end;

destructor TMarkdownHelpViewer.Destroy;
begin
  Markdown_HelpViewer := nil;
  inherited Destroy;
end;

function TMarkdownHelpViewer.GetHelpFile(const HelpKeyword: string): TFileName;
var
  LFileName: TFileName;
begin
  Result := '';
  //Get Help file specified in Application.HelpFile
  if Assigned(FHelpManager) then
    LFileName := HelpManager.GetHelpFile;

  if LFileName = '' then
    raise Exception.Create(HELP_FILE_NOT_SET);

  if HelpKeyword <> '' then
  begin
    if FindHelpFile(LFileName, 0, ChangeFileExt(HelpKeyword,'.md')) then
      Result := LFileName
    else
      Result := '';
  end;
end;

function TMarkdownHelpViewer.GetHelpFile(const HelpContext:
  {$if CompilerVersion > 31}THelpContext{$else}Integer{$ifend}): TFileName;
var
  LFileName: TFileName;
begin
  Result := '';
  //Get Help file specified in Application.HelpFile
  if Assigned(FHelpManager) then
    LFileName := HelpManager.GetHelpFile;

  if LFileName = '' then
    raise Exception.Create(HELP_FILE_NOT_SET);

  if HelpContext <> 0 then
  begin
    if FindHelpFile(LFileName, HelpContext, '') then
      Result := LFileName
    else
      Result := '';
  end;
end;

procedure TMarkdownHelpViewer.InternalShutDown;
begin
  SoftShutDown;
  if Assigned(FHelpManager) then
  begin
    HelpManager.Release(ViewerID);
    HelpManager := nil;
  end;
end;

initialization
  Markdown_HelpViewer := TMarkdownHelpViewer.Create;
  System.HelpIntfs.RegisterViewer(Markdown_HelpViewerIntf,
    Markdown_HelpViewer.FHelpManager);
  _ViewerLocation := '';

  SetLength(AMarkdownFileExt, 9);
  AMarkdownFileExt[0] := '.md';
  AMarkdownFileExt[1] := '.mkd';
  AMarkdownFileExt[2] := '.mdwn';
  AMarkdownFileExt[3] := '.mdown';
  AMarkdownFileExt[4] := '.mdtxt';
  AMarkdownFileExt[5] := '.mdtext';
  AMarkdownFileExt[6] := '.markdown';
  AMarkdownFileExt[7] := '.txt';
  AMarkdownFileExt[8] := '.text';

  SetLength(AHTMLFileExt, 2);
  AHTMLFileExt[0] := '.html';
  AHTMLFileExt[1] := '.htm';

finalization
  if Assigned(Markdown_HelpViewer.HelpManager) then
    Markdown_HelpViewer.InternalShutDown;
  Markdown_HelpViewerIntf := nil;

end.
