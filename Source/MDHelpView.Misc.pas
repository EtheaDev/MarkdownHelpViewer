{******************************************************************************}
{                                                                              }
{       MarkDown Help Viewer: Miscellaneous utilities                          }
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
{  The Original Code is uRegistry.pas.                                         }
{                                                                              }
{  The Initial Developer of the Original Code is Rodrigo Ruz V.                }
{  Portions created by Rodrigo Ruz V. are Copyright (C) 2011-2021 Rodrigo Ruz V}
{  All Rights Reserved.                                                        }
{******************************************************************************}
unit MDHelpView.Misc;

{$WARN SYMBOL_PLATFORM OFF}

interface
  uses
    SysUtils,
    System.Classes
    ;

resourcestring
  MARKDOWN_FILES = 'MarkDown text files';
  HTML_FILES = 'HTML text files';
  PDF_FILES = 'PDF files';

  function GetTempDirectory: string;
  function GetSpecialFolder(const CSIDL: integer): string;
  procedure GetFileNames(FileNames: TStrings;
    const PathWithWildCards: string;
    FileAttrib : Integer = SysUtils.faArchive or SysUtils.faReadOnly);
  procedure GetFullFileNames(FileNames: TStrings;
    const PathWithWildCards: string;
    FileAttrib: Integer = SysUtils.faArchive or SysUtils.faReadOnly );
  function  GetModuleLocation: string;
  procedure GetVerInfo(const FileName : string;
    var MajorVersion, MinorVersion, Release, Build : integer);
  function GetVersionString(const FileName: string;
    FormatString: string = '%d.%d.%d'): string;

resourcestring
  STextNotFound = 'Text not found';

implementation

uses
  WinAPI.GDIPObj
  , WinAPI.GDIPApi
  , ComObj
  , WinApi.Windows
  , WinApi.ShlObj
  , Vcl.Forms
  , Vcl.Graphics
  , Vcl.GraphUtil
  , MDHelpView.Registry
//  , uLogExcept
  ;

function GetTempDirectory: string;
var
  lpBuffer: array [0 .. MAX_PATH] of Char;
begin
  GetTempPath(MAX_PATH, @lpBuffer);
  Result := StrPas(lpBuffer);
end;

function GetWindowsDirectory: string;
var
  lpBuffer: array [0 .. MAX_PATH] of Char;
begin
  WinApi.Windows.GetWindowsDirectory(@lpBuffer, MAX_PATH);
  Result := StrPas(lpBuffer);
end;

function GetSpecialFolder(const CSIDL: integer): string;
var
  lpszPath: PWideChar;
begin
  lpszPath := StrAlloc(MAX_PATH);
  try
    ZeroMemory(lpszPath, MAX_PATH);
    if SHGetSpecialFolderPath(0, lpszPath, CSIDL, False) then
      Result := lpszPath
    else
      Result := '';
  finally
    StrDispose(lpszPath);
  end;
end;

procedure GetFileNames(FileNames: TStrings;
  const PathWithWildCards: string;
  FileAttrib : Integer = SysUtils.faArchive or SysUtils.faReadOnly);
var
  SearchRec : TSearchRec;
  R : Integer;
begin
  R := SysUtils.FindFirst( PathWithWildCards, FileAttrib, SearchRec );
  Try
    while R = 0 do
    begin
      FileNames.Append(SearchRec.Name);
      R := SysUtils.FindNext(SearchRec);
    end;
  Finally
    SysUtils.FindClose(SearchRec);
  End;
end;

procedure GetFullFileNames(FileNames: TStrings;
  const PathWithWildCards: string;
  FileAttrib: Integer = SysUtils.faArchive or SysUtils.faReadOnly );
var
  FilePath : string;
  I : Integer;
begin
  // append files found into FileNames
  GetFileNames(FileNames, PathWithWildCards, FileAttrib );
  // calcola il loro path
  FilePath := ExtractFilePath(PathWithWildCards);
  // aggiunge il path ai nomi dei file
  for I := 0 to Filenames.Count - 1 do
    FileNames[I] := ExpandFileName( FilePath+'\'+FileNames[I] );
end;

function  GetModuleLocation: string;
begin
  SetLength(Result, MAX_PATH);
  GetModuleFileName(HInstance, PChar(Result), MAX_PATH);
  Result:=PChar(Result);
end;

procedure GetVerInfo(const FileName : string;
  var MajorVersion, MinorVersion, Release, Build : integer);
type
  cArray   = Array[1..$3FFF] of Char;
  TLangInf = Array[1..2]     of Word;      // Language and charset identifiers

var
  InfoSize, Wnd: DWORD;
  VerBuf: Pointer;
  FI: PVSFixedFileInfo;
   VerSize: DWORD;
begin
  MajorVersion := 0;
  MinorVersion := 0;
  Release := 0;
  Build := 0;

  InfoSize := GetFileVersionInfoSize(PChar(FileName), Wnd);
  if InfoSize > 0 then
  begin
    GetMem(VerBuf, InfoSize);
    try
      if GetFileVersionInfo(PChar(FileName), Wnd, InfoSize, VerBuf) then
      begin
        if VerQueryValue(VerBuf, '\', Pointer(FI), VerSize) then
        begin
          MajorVersion := HIWORD(FI.dwFileVersionMS);
          MinorVersion := LOWORD(FI.dwFileVersionMS);
          Release := HIWORD(FI.dwFileVersionLS);
          Build := LOWORD(FI.dwFileVersionLS);
        end;
      end;
    finally
      FreeMem(VerBuf);
    end;
  end;
end;

function GetVersionString(const FileName: string;
  FormatString: string = '%d.%d.%d') : string;
var
  MajorVersion, MinorVersion, Release, Build : integer;
begin
  GetVerInfo(FileName,MajorVersion, MinorVersion, Release, Build);
  Result := Format(FormatString,[MajorVersion, MinorVersion, Release, Build]);
end;

initialization

end.
