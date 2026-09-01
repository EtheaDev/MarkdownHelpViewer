{******************************************************************************}
{                                                                              }
{       Markdown Help Viewer: Registry utilities                               }
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
{  The Original Code is uRegistry.pas.                                         }
{                                                                              }
{  The Initial Developer of the Original Code is Rodrigo Ruz V.                }
{  Portions created by Rodrigo Ruz V. are Copyright (C) 2011-2021 Rodrigo Ruz V}
{  All Rights Reserved.                                                        }
{******************************************************************************}
unit MDHelpView.Registry;

interface

uses
  Windows,
  Registry;

function RegReadStr(const RegPath, RegValue: string; var Str: string; const RootKey: HKEY): boolean;
function RegReadInt(const RegPath, RegValue: string; var IntValue: integer; const RootKey: HKEY): boolean;
function RegWriteStr(const RegPath, RegValue: string; const Str: string; const RootKey: HKEY): boolean;
function RegWriteInt(const RegPath, RegValue: string; IntValue: integer; const RootKey: HKEY): boolean;
function RegKeyExists(const RegPath: string; const RootKey: HKEY): boolean;
function DefaultStyleName: string;
function IsWindowsAppThemeLight: Boolean;


implementation

uses
  SysUtils;

//A registry failure is not an error for the caller (it falls back to a default),
//but a silently swallowed exception is impossible to diagnose: in a debug build
//it is at least reported to the debug output.
procedure LogRegError(const ARegPath: string; E: Exception);
begin
{$IFDEF DEBUG}
  OutputDebugString(PChar(Format('MDHelpViewer - registry "%s": %s (%s)',
    [ARegPath, E.Message, E.ClassName])));
{$ENDIF}
end;

function RegWriteStr(const RegPath, RegValue: string; const Str: string; const RootKey: HKEY): boolean;
var
  Reg: TRegistry;
begin
  try
    Reg := TRegistry.Create;
    try
      Reg.RootKey := RootKey;
      Result := Reg.OpenKey(RegPath, True);
      if Result then
        Reg.WriteString(RegValue, Str);
    finally
      Reg.Free;
    end;
  except
    on E: Exception do
    begin
      LogRegError(RegPath, E);
      Result := False;
    end;
  end;
end;

function RegReadStr(const RegPath, RegValue: string; var Str: string; const RootKey: HKEY): boolean;
var
  Reg: TRegistry;
begin
  try
    Reg := TRegistry.Create;
    try
      Reg.RootKey := RootKey;
      //Read-only: OpenKey(..., True) would create the key as a side effect
      //(and fail without administrative rights when RootKey is HKLM).
      Result := Reg.OpenKeyReadOnly(RegPath);
      if Result then
        Str := Reg.ReadString(RegValue);
    finally
      Reg.Free;
    end;
  except
    on E: Exception do
    begin
      LogRegError(RegPath, E);
      Result := False;
    end;
  end;
end;

function RegWriteInt(const RegPath, RegValue: string; IntValue: integer; const RootKey: HKEY): boolean;
var
  Reg: TRegistry;
begin
  try
    Reg := TRegistry.Create;
    try
      Reg.RootKey := RootKey;
      Result := Reg.OpenKey(RegPath, True);
      if Result then
        Reg.WriteInteger(RegValue, IntValue);
    finally
      Reg.Free;
    end;
  except
    on E: Exception do
    begin
      LogRegError(RegPath, E);
      Result := False;
    end;
  end;
end;

function RegReadInt(const RegPath, RegValue: string; var IntValue: integer; const RootKey: HKEY): boolean;
var
  Reg: TRegistry;
begin
  try
    Reg := TRegistry.Create;
    try
      Reg.RootKey := RootKey;
      //Read-only: OpenKey(..., True) would create the key as a side effect
      //(and fail without administrative rights when RootKey is HKLM).
      Result := Reg.OpenKeyReadOnly(RegPath);
      if Result then
        IntValue := Reg.ReadInteger(RegValue);
    finally
      Reg.Free;
    end;
  except
    on E: Exception do
    begin
      LogRegError(RegPath, E);
      Result := False;
    end;
  end;
end;

function RegKeyExists(const RegPath: string; const RootKey: HKEY): boolean;
var
  Reg: TRegistry;
begin
  try
    Reg := TRegistry.Create;
    try
      Reg.RootKey := RootKey;
      Result := Reg.KeyExists(RegPath);
    finally
      Reg.Free;
    end;
  except
    on E: Exception do
    begin
      LogRegError(RegPath, E);
      Result := False;
    end;
  end;
end;

function IsWindows11: Boolean;
const
  //First build of Windows 11
  WINDOWS11_FIRST_BUILD = 22000;
var
  Reg: TRegistry;
begin
  //NB: the build number is read from the registry and not from TOSVersion,
  //because the value reported by the API depends on the application manifest.
  //The previous version switched on a TOSVersionInfo record that was never
  //filled by GetVersionEx: it read uninitialized memory and worked only
  //because the "else" branch happens to be the right one.
  Result := False;
  Reg := TRegistry.Create;
  Try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKeyReadOnly('\Software\Microsoft\Windows NT\CurrentVersion') then
    Try
      Result := StrToIntDef(Reg.ReadString('CurrentBuild'), 0) >= WINDOWS11_FIRST_BUILD;
    Finally
      Reg.CloseKey;
    End;
  Finally
    Reg.Free;
  End;
end;

function DefaultStyleName: string;
begin
  if IsWindowsAppThemeLight then
  begin
    if IsWindows11 then
      Result := 'Windows11 Modern Light'
    else
      Result := 'Windows10';
  end
  else
  begin
    if IsWindows11 then
      Result := 'Windows11 Modern Dark'
    else
      Result := 'Windows10 SlateGray';
  end;
end;

function IsWindowsAppThemeLight: Boolean;
var
  LIsLight: Integer;
  Reg: TRegistry;
begin
  LIsLight := 1;
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    Result := Reg.OpenKeyReadOnly('SOFTWARE\Microsoft\Windows\CurrentVersion\Themes\Personalize');
    if Result then
    begin
      if Reg.ValueExists('AppsUseLightTheme') then
        LIsLight := Reg.ReadInteger('AppsUseLightTheme');
    end;
  finally
    Reg.Free;
  end;
  Result := LIsLight = 1;
end;

end.
