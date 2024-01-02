{******************************************************************************}
{                                                                              }
{       CBMultiLanguage                                                        }
{       (Localization engine)                                                  }
{                                                                              }
{       Copyright (c) 2005-2024 (Ethea S.r.l.)                                 }
{       Author: Carlo Barazzetta                                               }
{                                                                              }
{       https://github.com/EtheaDev/CBMultiLanguage                            }
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
unit CBMultiLanguageUtils;

interface

uses
  System.SysUtils
  , CBMultiLanguage;

// ritorna la descrizione della lingua in lingua
function AAppLanguageDX( Language : TAppLanguage) : string;

// ritorna la descrizione dello status
function AAPPtrxStatusDX( Status : TAppTrxStatus) : string;

// verifica se in TrxPath ci sono i files necessari al repository multilingua
function CheckTrxRootDir(const TrxPath: string) : string;

// verifica se in ReposPath ci sono i files del repository
function CheckReposDir(const ReposPath: string) : string;

// imposta le display-label sul dataset delle traduzioni
procedure SetCDSTranslatorDisplayLabels(CDSTranslator : TCDSTranslator);

// imposta le display-label sul dataset del repository
procedure SetTCDSTrxRepositoryDisplayLabels(CDSTrxRepository : TCDSTrxRepository);

//Ritorna la posizione dell'ultima occorrenza di SubStr in S
function LastPos(const SubStr: String; const S: String): Integer;

//Rimuove dal nome di una form il suffisso _999 (es. MyForm_1 -> MyForm)
function RemoveFormNameSuffix(const FormName : string) : string;


implementation

uses
  //Delphi
  System.Classes
  , System.StrUtils
  , CBMultiLanguageMessages;

function AAppLanguageDX( Language : TAppLanguage) : string;
begin
  case Language of
    mlItalian    : Result := STR_ITA;
    mlEnglish    : Result := STR_ENG;
    mlGerman     : Result := STR_DEU;
    mlSpanish    : Result := STR_ESP;
    mlFrench     : Result := STR_FRA;
    mlPortuguese : Result := STR_PTG;
  end;
end;

function AAPPtrxStatusDX( Status : TAppTrxStatus) : string;
begin
  case Status of
    tsDaTradurre   : Result := STR_DA_TRADURRE;
    tsDaVerificare : Result := STR_DA_VERIFICARE;
    tsTradotto     : Result := STR_TRADOTTO;
    tsAutoTradotto : Result := STR_AUTOTRADOTTO;
    tsDaIgnorare   : Result := STR_DA_IGNORARE;
  end;
end;

function CheckTrxRootDir(const TrxPath : string) : string;
var
  FilePath, FileName : string;
begin
  //Verfica e imposta la root dir
  if TrxPath = '' then
  begin
    Result := '';
    exit;
  end;

  if Copy(TrxPath,length(TrxPath),1) <> PathDelim then
    FilePath := TrxPath+PathDelim
  else
    FilePath := TrxPath;
  FileName := FilePath+CDSTrxFileName;
  //Verifico che nella cartella scelta ci sia il file di definizione delle traduzioni
  if FileExists(FileName) then
    Result := FilePath
  else
    raise EFOpenError.CreateFmt(ERR_ROOT_DIR_SELECTION,
      [FilePath,CDSTrxFileName]);
end;

// verifica se in ReposPath ci sono i files del repository
function CheckReposDir(const ReposPath: string) : string;
begin
  //Verfica e imposta la root dir
  if ReposPath = '' then
  begin
    Result := '';
    exit;
  end;

  if Copy(ReposPath,length(ReposPath),1) <> PathDelim then
    Result := ReposPath+PathDelim
  else
    Result := ReposPath;
  //Verifico che la cartella esista
  if not directoryexists(Result) then
    raise EFOpenError.CreateFmt(ERR_REPOS_DIR,[Result]);
end;

procedure SetCDSTranslatorDisplayLabels(CDSTranslator : TCDSTranslator);
begin
  with CDSTranslator do
  begin
    //Imposto displaylabel dei campi
    ClassNameField.DisplayLabel := CLASSNAME_DISPLABEL;
    TrxField.DisplayLabel := TRX_DISPLABEL;
    OrigValueField.DisplayLabel := ORIG_DISPLABEL;
    TypeField.DisplayLabel := TYPE_DISPLABEL;
    StatusField.DisplayLabel := STATUS_DISPLABEL;
    CompNameField.DisplayLabel := COMPNAME_DISPLABEL;
    PropNameField.DisplayLabel := PROPNAME_DISPLABEL;
    OrigStrField.DisplayLabel := ORIG_DISPLABEL;
    TrxStrField.DisplayLabel := TRX_DISPLABEL;
  end;
end;

procedure SetTCDSTrxRepositoryDisplayLabels(CDSTrxRepository : TCDSTrxRepository);
begin
  with CDSTrxRepository do
  begin
    //Imposto displaylabel dei campi
    TrxField.DisplayLabel := TRX_DISPLABEL;
    OrigValueField.DisplayLabel := ORIG_DISPLABEL;
    CountField.DisplayLabel := COUNT_DISPLABEL;
  end;
end;

function LastPos(const SubStr: String; const S: String): Integer;
begin
   Result := Pos(ReverseString(SubStr), ReverseString(S)) ;

   if (Result <> 0) then
     Result := ((Length(S) - Length(SubStr)) + 1) - result + 1;
end;

function RemoveFormNameSuffix(const FormName : string) : string;
var
  p : integer;
  Suffix : string;
begin
  Result := FormName;
  p := LastPos('_',FormName);
  if p > 0 then
  begin
    Suffix := Copy(FormName,p+1,MaxInt);
    Try
      //Se riesco a convertire i in stringa significa che il suffisso è di tipo _999
      //quindi lo rimuovo
      StrToInt(Suffix);
      Result := Copy(FormName,1,p-1);
    Except
      //Se non riesco a convertire il risultato è il nome stesso della form
    End;
  end;
end;

end.
