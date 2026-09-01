{******************************************************************************}
{                                                                              }
{       This units implements the interfaces for the Help Viewer               }
{                                                                              }
{       Copyright (c) 2023-2026 (Ethea S.r.l.)                                 }
{       Author: Carlo Barazzetta                                               }
{       Contributors: Nicol� Boccignone, Emanuele Biglia                       }
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

const
  //Signatures carried by TCopyDataStruct.dwData: they let the receiver tell a
  //help request from any other WM_COPYDATA sent to its window.
  MD_HELP_COPYDATA_ID = 3232;   //legacy message, ANSI payload
  MD_HELP_COPYDATA_ID_W = 3233; //Unicode payload

type
  //Legacy layout. ShortString means an implicit Unicode->ANSI conversion, so
  //any character outside the system codepage is lost. It is still supported
  //because the viewers already installed understand only this message.
  THelpInfoToPass = packed record
    FilePath: string[255]; //Path of help file to open
    FileName: string[255]; //Help file to open
    Context : integer; //Context
  end;
  PRecToPass = ^THelpInfoToPass;

  //Unicode layout: preserves paths with characters outside the system codepage
  //(the product is localized in six languages, Russian included).
  THelpInfoToPassW = packed record
    FilePath: array[0..MAX_PATH] of WideChar; //Path of help file to open
    FileName: array[0..MAX_PATH] of WideChar; //Help file to open
    Context : Integer; //Context
  end;
  PRecToPassW = ^THelpInfoToPassW;

  TUnderstandsHelpContext = procedure(const AContext: {$if CompilerVersion > 31}THelpContext{$else}Integer{$endif};
      var AKeyword: string);

//To register the position of the Viewer if not installed
procedure RegisterMDViewerLocation(AViewerExeFileName: TFileName);

//To register custom handle procedure to Understand specific HelpContext
procedure RegisterHelpCustomHandlers(const UnderstandsHelpContext: TUnderstandsHelpContext = nil);

var
  AMarkdownFileExt: TArray<String>;
  AHTMLFileExt: TArray<String>;

//File Utilities
function FileWithExtExists(var AFileName: TFileName;
  const AFileExtensions: array of string): boolean;
function GetFileMasks(const AFileExtensions: array of string;
  const ASeparator: Char = ';'): string;
function IsFileNameWithExt(const AFileName: TFileName;
  const AFileExtensions: array of string): boolean;
procedure GetFileNamesWithExtensions(FileNames: TStrings;
  const PathName: string; const Extensions: string;
  FileAttrib : Integer = faArchive or faReadOnly);
function SendWMCOPYToProcess(const AExeName, AFileName: TFileName;
  AHelpContext: Integer): Boolean;
function FindHelpFile(var AFileName: TFileName; const AContext: Integer;
  const HelpKeyword: string; const AFileExtensions: array of string): boolean;
function GetIndexFileName(const AFileName: TFileName;
  const AFileExtensions: array of string): TFileName;

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
  public
    constructor Create;
    destructor Destroy; override;
    { internal support functions }
    function GetHelpFile(const HelpKeyword: string): TFileName; overload;
    function GetHelpFile(const HelpContext:
      {$if CompilerVersion > 31}THelpContext{$else}Integer{$endif}): TFileName; overload;
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
  _OnUnderstandsHelpContext: TUnderstandsHelpContext;

type
  TEnumInfo = record
    ProcessID: DWORD;
    HWND: HWND;
  end;
  PEnumInfo = ^TEnumInfo;

function FindHelpFile(var AFileName: TFileName; const AContext: Integer;
  const HelpKeyword: string; const AFileExtensions: array of string): boolean;
var
  LHelpFileName: TFileName;
  LExtension, LName, LPath, LKeyWord: string;
begin
  if HelpKeyword <> '' then
    LKeyWord := HelpKeyword
  else if AContext <> 0 then
  begin
    LKeyWord := IntToStr(AContext);

    //Custom Keyword based on Context
    if Assigned(_OnUnderstandsHelpContext) then
      _OnUnderstandsHelpContext(AContext, LKeyword);
  end
  else
    LKeyword := '';

  LExtension := ExtractFileExt(AFileName);
  LKeyword := LKeyword+LExtension;

  //First, Try the Keyword only
  LPath := ExtractFilePath(AFileName);
  LHelpFileName := LPath+LKeyword;
  Result := FileWithExtExists(LHelpFileName, AFileExtensions);

  if not Result then
  begin
    //Then, try the Help Name and the Keyword (eg.Home1000.ext)
    LName := ChangeFileExt(ExtractFileName(AFileName),'');
    LHelpFileName := LPath+LName+LKeyword;
    Result := FileWithExtExists(LHelpFileName, AFileExtensions);
    if not Result then
    begin
      //At least, try the Help Name and the Keyword with '_' (eg.Home_1000.ext)
      LHelpFileName := LPath+LName+'_'+LKeyword;
      Result := FileWithExtExists(LHelpFileName, AFileExtensions);
    end;
  end;

  if Result then
    AFileName := LHelpFileName;
end;

function GetIndexFileName(const AFileName: TFileName;
  const AFileExtensions: array of string): TFileName;
begin
  Result := AFileName;

  if not FindHelpFile(Result,0,'_Index', AFileExtensions) and //Try with HelpFileName_Index.ext
    not FindHelpFile(Result,0,'Home_Index', AFileExtensions) and //Try with Home_Index.ext (for VitePress Docs)
    not FindHelpFile(Result,0,'Index', AFileExtensions) and //Try with Index.ext
    not FindHelpFile(Result,0,'Content', AFileExtensions) then //Try with Content.ext
    Result := '';
end;

function FileWithExtExists(var AFileName: TFileName;
  const AFileExtensions: array of string): boolean;
var
  I: Integer;
  LExt: string;
  LFileName: TFileName;
begin
  Result := False;
  if Length(AFileExtensions) = 0 then
    Exit;
  LExt := ExtractFileExt(AFileName);
  if LExt = '' then
    LFileName := AFileName+AFileExtensions[0]
  else
    LFileName := AFileName;
  Result := FileExists(LFileName);
  if not Result then
  begin
    LFileName := ExtractFilePath(AFileName)+ChangeFileExt(ExtractFileName(AFileName),'');
    for I := Low(AFileExtensions) to High(AFileExtensions) do
    begin
      LExt := AFileExtensions[I];
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

function GetFileMasks(const AFileExtensions: array of string;
  const ASeparator: Char = ';'): string;
var
  I: Integer;
  LExt: string;
begin
  for I := Low(AFileExtensions) to High(AFileExtensions) do
  begin
    LExt := AFileExtensions[I];
    if I > 0 then
      Result := Result + ASeparator;
    Result := Result + '*'+LExt;
  end;
end;

function IsFileNameWithExt(const AFileName: TFileName;
  const AFileExtensions: array of string): boolean;
var
  I: Integer;
  LFileExt, LExt: string;
begin
  Result := False;
  LFileExt := ExtractFileExt(AFileName);
  for I := Low(AFileExtensions) to High(AFileExtensions) do
  begin
    LExt := AFileExtensions[I];
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
  LMasks: TArray<string>;
  I: Integer;

  //Extensions is a mask list like '*.md;*.mkd': the extension of the file must
  //match one entry as a whole. The previous AnsiPos test was case sensitive
  //(README.MD was skipped) and matched substrings (the '.md' extension also
  //matched the '*.mdown' entry).
  function HasWantedExtension(const AFileName: string): Boolean;
  var
    LExt: string;
    J: Integer;
  begin
    Result := False;
    LExt := ExtractFileExt(AFileName);
    if LExt = '' then
      Exit;
    for J := Low(LMasks) to High(LMasks) do
      if SameText(LMasks[J], '*' + LExt) then
        Exit(True);
  end;

begin
  LMasks := Extensions.Split([';']);
  for I := Low(LMasks) to High(LMasks) do
    LMasks[I] := Trim(LMasks[I]);
  Path := IncludeTrailingBackslash(PathName);
  if FindFirst(Path + FileMask, FileAttrib, Rec) = 0 then
  begin
    try
      repeat
        if HasWantedExtension(Rec.Name) then
          FileNames.Add(Rec.Name);
      until FindNext(Rec) <> 0;
    finally
      FindClose(Rec);
    end;
  end;
end;

//NB: the window handle is declared HWND and not DWORD: on Win64 a HWND is
//pointer sized, so a DWORD would truncate it.
function EnumWindowsProc(Wnd: HWND; var EI: TEnumInfo): Bool; stdcall;
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

function FindMainWindow(PID: DWORD): HWND;
var
  EI: TEnumInfo;
begin
  EI.ProcessID := PID;
  EI.HWND := 0;
  EnumWindows(@EnumWindowsProc, NativeInt(@EI));
  Result := EI.HWND;
end;

function GetHWndByPID(const hPID: DWORD): HWND;
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
  MyHwnd: HWND;
  cd: TCopyDataStruct;
  LExeFileName: TFileName;
  LFilePath, LFileName: string;
  LParamFilePath: string[255];
  LParamFileName: string[255];
  LRecord: THelpInfoToPass;
  LRecordW: THelpInfoToPassW;
begin
  Result := False;
  FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS,0); //Create a process snapshot
  if FSnapshotHandle = INVALID_HANDLE_VALUE then
    Exit;
  try
    FProcessEntry32.dwSize := Sizeof(FProcessEntry32);
    ContinueLoop := Process32First(FSnapshotHandle,FProcessEntry32); //Get the first process in the system
    LExeFileName := ExtractFileName(AExeName);
    LFileName := ExtractFileName(AFileName);
    LFilePath := ExtractFilePath(AFileName);
    LParamFileName := LFileName;
    LParamFilePath := LFilePath;
    while ContinueLoop do
    begin
      ProcessName := FProcessEntry32.szExeFile;
      if SameText(ProcessName, LExeFileName) then
      begin
        MyHwnd := GetHWndByPID(FProcessEntry32.th32ProcessID);
        //Without a main window there is nothing to send the message to: keep
        //looking, so that the caller can fall back to ShellExecute when no
        //running instance can actually receive the request.
        if MyHwnd <> 0 then
        begin
          SendMessage(MyHwnd, WM_ACTIVATE, 0, 0);
          SendMessage(MyHwnd, WM_SETFOCUS, 0, 0);
          SetWindowPos(MyHwnd, HWND_TOP, 0, 0, 0, 0, SWP_NoMove or SWP_NoSize);

          //The Unicode message is tried first: a viewer that understands it
          //answers 1. Older viewers do not handle this signature and leave the
          //result at 0, so the legacy message is sent instead.
          FillChar(LRecordW, SizeOf(LRecordW), 0);
          StrLCopy(LRecordW.FilePath, PChar(LFilePath), MAX_PATH);
          StrLCopy(LRecordW.FileName, PChar(LFileName), MAX_PATH);
          LRecordW.Context := AHelpContext;
          cd.dwData := MD_HELP_COPYDATA_ID_W;
          cd.cbData := SizeOf(LRecordW);
          cd.lpData := @LRecordW;
          Result := SendMessage(MyHwnd, WM_COPYDATA, 0, NativeInt(@cd)) <> 0;

          if not Result then
          begin
            //Legacy message: the path is converted to ANSI, so characters
            //outside the system codepage are lost. Nothing else is possible
            //with a viewer that predates the Unicode message.
            LRecord.FilePath := LParamFilePath;
            LRecord.FileName := LParamFileName;
            LRecord.Context := AHelpContext;
            cd.dwData := MD_HELP_COPYDATA_ID;
            cd.cbData := sizeof(LRecord);
            cd.lpData := @LRecord;
            SendMessage(MyHwnd, WM_COPYDATA, 0, NativeInt(@cd) );
            //Old viewers never set the message result, so success cannot be
            //verified: as before, the request is assumed to be handled.
            Result := True;
          end;
          Break;
        end;
      end;
      ContinueLoop := Process32Next(FSnapshotHandle,FProcessEntry32);
    end;
  finally
    CloseHandle(FSnapshotHandle); // Release the snapshot handle
  end;
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
  LHelpFileName: string;
begin
  LHelpFileName := GetHelpFile(HelpString);

  if LHelpFileName <> '' then
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

{ CanShowTableOfContents is a querying function that the Help Manager
  calls to determine if the Viewer supports tables of contents. HtmlHelp does. }

function TMarkdownHelpViewer.CanShowTableOfContents: Boolean;
begin
  Result := False;
end;

{ ShowTableOfContents is a command function that the Help Manager uses
  to direct the Viewer to display a table of contents. It is never
  called without being preceded by a call to CanShowTableOfContents. }
procedure TMarkdownHelpViewer.ShowTableOfContents;
begin
  ; //Do nothing
end;

procedure TMarkdownHelpViewer.ShowMarkdownFile(const AFileName: TFileName;
  const AHelpString: string; const AContext: Integer = 0);
var
  LViewerExeName: TFileName;
  LRegistry: TRegistry;
  LFileName: TFileName;
begin
  //No help file resolved: the querying methods stay silent, so the error is
  //reported here, where help is actually being displayed.
  if AFileName = '' then
    raise Exception.Create(HELP_FILE_NOT_SET);

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
    LFileName := '"'+AFileName+'"';
    if not SendWMCOPYToProcess(LViewerExeName, LFileName, AContext) then
    begin
      ShellExecute( 0, 'open' , PChar(LViewerExeName), PChar(LFileName),
        PChar(ExtractFilePath(LFileName)), SW_SHOW );
    end;
  end
  {$IFDEF DEBUG}
  else
    raise Exception.Create(MD_HELP_VIEWER_NOT_FOUND);
  {$ENDIF}
end;

procedure TMarkdownHelpViewer.ShowHelp(const HelpString: string);
var
  LFileName : string;
begin
  LFileName := GetHelpFile(HelpString);
  ShowMarkdownFile(LFileName, HelpString, 0);
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
  if Assigned(FHelpManager) then
    HelpManager := nil;
end;

{ IExtendedHelpViewer }

{ UnderstandsTopic is called by the Help Manager to ask if the Viewer
  is capable of displaying a topic-based help query for a given topic.
  It's default behavior is to say 'yes'. }
function TMarkdownHelpViewer.UnderstandsTopic(const Topic: string): Boolean;
var
  LHelpFileName: string;
begin;
  LHelpFileName := GetHelpFile(Topic);

  Result := LHelpFileName <> '';
end;

{ DisplayTopic is called by the Help Manager if a Help Viewer claims
  in its response to UnderstandsTopic to be able to provide Topic-based
  help for a particular keyword. }

procedure TMarkdownHelpViewer.DisplayTopic(const Topic: string);
var
  LHelpFileName: string;
begin
  LHelpFileName := GetHelpFile('');
  ShowMarkdownFile(LHelpFileName, Topic);
end;

{ UnderstandsContext is a querying function called by the Help Manager
  to determine if an Extended Help Viewer is capable of providing
  help for a particular context-ID. Like all querying functions in
  this file, the default behavior is to say 'yes' unless overridden by
  a Tester. }

function TMarkdownHelpViewer.UnderstandsContext(
  const ContextId: {$if CompilerVersion > 31}THelpContext{$else}Integer{$endif};
  const HelpFileName: string): Boolean;
var
  LHelpFileName: string;
begin
  //Accept ContextId if resolve a file markdown with this context
  LHelpFileName := GetHelpFile(ContextId);

  Result := (LHelpFileName <> '');
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

  //NB: no exception here. This method also serves the querying methods of
  //ICustomHelpViewer/IExtendedHelpViewer (UnderstandsKeyword, UnderstandsTopic,
  //UnderstandsContext), which must simply answer "not handled" when the host
  //application has no help file assigned. The error is raised by
  //ShowMarkdownFile, i.e. only when help is really being displayed.
  if LFileName = '' then
    Exit;

  if HelpKeyword <> '' then
  begin
    if FindHelpFile(LFileName, 0, HelpKeyword, AMarkdownFileExt) then
      Result := LFileName
    else
      Result := '';
  end
  else
    //No keyword: the help file itself (used by DisplayTopic and GetHelpStrings)
    Result := LFileName;
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

  //NB: no exception here, see the overload above: UnderstandsContext must be
  //able to answer "not handled" without raising.
  if LFileName = '' then
    Exit;

  if HelpContext <> 0 then
  begin
    if FindHelpFile(LFileName, HelpContext, '', AMarkdownFileExt) then
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

procedure RegisterHelpCustomHandlers(const UnderstandsHelpContext: TUnderstandsHelpContext = nil);
begin
  _OnUnderstandsHelpContext := UnderstandsHelpContext;
end;

initialization
  Markdown_HelpViewer := TMarkdownHelpViewer.Create;
  System.HelpIntfs.RegisterViewer(Markdown_HelpViewerIntf,
    Markdown_HelpViewer.FHelpManager);
  _ViewerLocation := '';
  _OnUnderstandsHelpContext := nil;

  SetLength(AMarkdownFileExt, 7);
  AMarkdownFileExt[0] := '.md';
  AMarkdownFileExt[1] := '.mkd';
  AMarkdownFileExt[2] := '.mdwn';
  AMarkdownFileExt[3] := '.mdown';
  AMarkdownFileExt[4] := '.mdtxt';
  AMarkdownFileExt[5] := '.mdtext';
  AMarkdownFileExt[6] := '.markdown';
  //NB: keep SetLength in sync with the number of filled entries: empty
  //trailing items would produce a '*' file-mask matching every file.

  SetLength(AHTMLFileExt, 2);
  AHTMLFileExt[0] := '.html';
  AHTMLFileExt[1] := '.htm';

finalization
  //NB: Markdown_HelpViewer is set to nil by its own destructor, so it must be
  //checked before dereferencing it: the interface may already have been
  //released when this unit is finalized.
  if Assigned(Markdown_HelpViewer) and
    Assigned(Markdown_HelpViewer.HelpManager) then
    Markdown_HelpViewer.InternalShutDown;
  Markdown_HelpViewerIntf := nil;

end.
