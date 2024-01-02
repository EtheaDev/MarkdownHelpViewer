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
unit CBMultiLanguage;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows,
  SysUtils, DBClient, Classes, DB, TypInfo;

const
  REPOS_NAME_SUFFIX = 'TrxRepository';
  CDSTrxFileName = 'TrxDefine.xml';
  START_STAT_TAG = '<!--stat:';
  END_STAT_TAG = ':stat-->';
  STAT_TOT_TAG = '#';
  STAT_LANGUAGE= 'Lang:';
  FORMS_FOLDER = 'Forms';
  UNITS_FOLDER = 'Units';
  GENERAL_FOLDER = 'General';
  MAX_MEMO_VIEW = 250;
  SELF_STR = 'Self';
  STR_SIZE = 250;
  PK_INDEX = 'PK';

  FLD_TYPE = 'Type';
  FLD_COMP_NAME = 'ComponentName';
  FLD_PROP_NAME = 'PropertyName';
  FLD_ORIG_VALUE = 'OriginalValue';
  FLD_TRX = 'Translation';
  FLD_STATUS = 'Status';
  FLD_DELETED = 'Deleted';
  FLD_CLASSNAME = 'ClassName';
  FLD_ORIG_STR = 'OriginalStr';
  FLD_TRX_STR = 'TranslationStr';
  FLD_COUNT = 'Count';

  COMPONENT_NAME_FOR_MESSAGE = 'Message';
  TRANSLATION_FILE_EXT = '.xml';

  FLD_CTRLCLASSNAME = 'ComponentClassName';
  FLD_PROPTRXKIND = 'PropertyTrxKind';

  REPOSITORY_FOLDER = 'Repository';
  CONFIG_VALUE = 'Language';
  REG_KEY = '\Software\%s\%s';

  ERR_TRXPATH_NOT_SET   = 'Translation Error! RepositoryPath not set!';
  ERR_CANNOT_CREATE_DIR = 'Translation Error! Cannot create directory %s';
  ERR_FILE_NAME         = 'Translator Error! FileName property not assigned.';
  ERR_LANGUAGE_STR      = 'Translator Error! Language corresponding to %s not allowed.';
  LCK_ERROR             = 'Lock error: file "%s" already in use by another session!';
  LCK_LOG               = 'User: %s; Date: %s';

Type
  EMultiLanguageException = class(Exception);

  ETranslatorLockException = class(EMultiLanguageException)
  public
    FLockFileName : string;
    constructor CreateLckFmt(const LockFileName, Msg: string; const Args: array of const);
  end;

  TAppLanguage = (mlItalian, mlEnglish, mlGerman, mlSpanish, mlFrench, mlPortuguese);

  TAppLanguages = set of TAppLanguage;

  TAppLangType = (ltDictionary, ltApplication, ltForm, ltMessage, ltPrint, ltCustom);

  TAppTrxStatus = (tsDaTradurre, tsDaVerificare, tsTradotto, tsAutoTradotto, tsDaIgnorare);
  TTrxStatusSet = set of TAppTrxStatus;
  TReposTrxStatus = (rsDaTradurre, rsDaVerificare, rsTradotto, rsAutoTradotto);
  TReposStatusSet = set of TReposTrxStatus;

  TTrxKind = (txString, txStrings, txBoundLabel, txListView, txStatusPanel, txDbGrid,
    txActionBars, txMenuItems, txButtonGroup, txButtonCategories, txToggleSwitch);

  TReposType = (rtForms, rtUnits, rtGeneral);

  TDictFilterSpec = record
    ShowUntranslated : boolean;
    ShowAutoTranslated : boolean;
    FromValue : string;
    ToValue : string;
  end;

  TTranslationService = function (const SourceValue: string;
    SourceLanguage, TargetLanguage: TAppLanguage;
    out TranslatedValue: string): boolean of object;

const
  LANG_ITA_ID   = 'ITA';
  LANG_ENG_ID   = 'ENG';
  LANG_DEU_ID   = 'DEU';
  LANG_ESP_ID   = 'ESP';
  LANG_FRA_ID   = 'FRA';
  LANG_PTG_ID   = 'PTG';

  ISO_ITA_ID   = 'it';
  ISO_ENG_ID   = 'en';
  ISO_DEU_ID   = 'de';
  ISO_ESP_ID   = 'es';
  ISO_FRA_ID   = 'fr';
  ISO_PTG_ID   = 'pt';

  AAppLanguageID : Array [TAppLanguage] of string =
    (LANG_ITA_ID, LANG_ENG_ID, LANG_DEU_ID, LANG_ESP_ID, LANG_FRA_ID, LANG_PTG_ID);

  AAppLanguageISOID : Array [TAppLanguage] of string =
    (ISO_ITA_ID, ISO_ENG_ID, ISO_DEU_ID, ISO_ESP_ID, ISO_FRA_ID, ISO_PTG_ID);

  AITALanguageDesc : Array [TAppLanguage] of string =
    ('Italiano', 'Inglese', 'Tedesco', 'Spagnolo', 'Francese', 'Portoghese');

  AENGLanguageDesc : Array [TAppLanguage] of string =
    ('Italian', 'English', 'German', 'Spanish', 'French', 'Portuguese');

  AAppLangType : Array [TAppLangType] of string =
    ('D', 'A', 'F', 'M', 'P', 'C');

  AAppTrxStatus : Array [TAppTrxStatus] of string =
    ('!', '?', '*', '@', '0'); //non usare # che è usato per il totale delle statistiche

  AReposTrxStatus : Array [TReposTrxStatus] of string =
    ('!', '?', '*', '@'); //non usare # che è usato per il totale delle statistiche

  AReposType : Array[TReposType] of string =
    (FORMS_FOLDER, UNITS_FOLDER, GENERAL_FOLDER);

type
  TTrxMemoField = class(TMemoField)
  protected
    procedure GetText(var Text: string; DisplayText: Boolean); override;
  end;

  TTrxClientDataSet = class(TClientDataSet)
  private
    FLockedFile : boolean;
    FSessionUserName: string;
    procedure ReadStatisticsFromFile;
    procedure ReformatTextFile;
    procedure WriteTextFile(const S: string);
  protected
    procedure InternalReadStatistics(const S : string); virtual;
    procedure InternalWriteStatistics(var S : string); virtual;
    procedure CloseCursor; override;
    procedure OpenCDS; virtual;
    function LockFileName : string;
    procedure CreateLockFile;
    procedure DeleteLockFile;
    procedure InternalEdit; override;
    procedure InternalInsert; override;
    procedure InternalDelete; override;
  public
    destructor Destroy; override;
    procedure CancelUpdates;
    procedure SaveReformatXMLUTF8(Force : boolean=False); virtual;
    procedure ReloadData; virtual;
    property SessionUserName : string read FSessionUserName write FSessionUserName;
  end;

  TCDSTrxRepository = class(TTrxClientDataSet)
  private
    FRepositoryPath : string;
    FLanguage : TAppLanguage;
    FStatusField : TField;
    FOrigValueField : TField;
    FTrxField : TField;
    FCountField : TField;
    StatStringList : TStringList;
    FFilterSpec: TDictFilterSpec;
    function GetStatusField: TField;
    function GetOrigValueField: TField;
    function GetTrxField: TField;
    procedure UpdateStatistics;
    procedure SetFilterSpec(const Value: TDictFilterSpec);
    function GetCountField: TField;
    function GetRecordStatus : TReposTrxStatus;
    procedure UpdateRecordStatus(const Value: TReposTrxStatus;
      const TranslatedValue : string = '');
    procedure SetRecordStatus(const Value: TReposTrxStatus);
  protected
    procedure InternalReadStatistics(const S : string); override;
    procedure InternalWriteStatistics(var S : string); override;
    procedure SetActive(Value : boolean); override;
    procedure SetLanguage(Value : TAppLanguage);
    procedure AddMemo(const OriginalValue, TranslatedValue: string;
      NoCapitalsNames : boolean);
    procedure CreateFields; override;
    procedure DoBeforePost; override;
  public
    TotMessages, MsgAutoTranslated, MsgTranslated : integer; //statistics
    procedure SetFileName(ReposType : TReposType; Language : TAppLanguage;
      const Path : string);
    procedure ResetFilterSpec;
    procedure OpenCDS; override;
    procedure ReadStatistics;
    function PercTranslated : double;
    function MsgUntranslated : integer;
    function GetTranslatedValue(const Value : string; out TranslatedValue : string;
      out TranslatedStatus: TReposTrxStatus) : boolean;
    procedure SetTranslatedValue(const Value : string; Status: TReposTrxStatus = rsTradotto);
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    property Language : TAppLanguage read FLanguage write SetLanguage;
    property RecordStatus   : TReposTrxStatus read GetRecordStatus write SetRecordStatus;
    property OrigValueField : TField read GetOrigValueField;
    property StatusField : TField read GetStatusField;
    property TrxField : TField read GetTrxField;
    property CountField : TField read GetCountField;
    property RepositoryPath : string read FRepositoryPath write FRepositoryPath;
    property FilterSpec : TDictFilterSpec read FFilterSpec write SetFilterSpec;
  end;

  TCDSTrxPropList = class(TTrxClientDataSet)
  private
    FFieldComponentClassName : TField;
    FFieldPropertyName : TField;
    FFieldTrxKind : TField;
    FRegisteredClassList : TStringList;
    procedure AddTrxProperty(const ComponentClassName, PropertyName : string;
      TrxKind : TTrxKind);
    function GetFieldTrxKind: TField;
  protected
    procedure SetActive(Value: Boolean); override;
    procedure OpenCDS; override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    property FieldTrxKind : TField read GetFieldTrxKind;
  end;

  TCDSTranslator = class(TTrxClientDataSet)
  private
    FTranslateLang : TAppLanguage;
    FTrxField : TField;
    FOrigValueField : TField;
    FTypeField : TField;
    FStatusField : TField;
    FCompNameField : TField;
    FPropNameField : TField;
    FDeletedField : TField;
    FClassNameField : TField;
    FTrxStrField: TField;
    FOrigStrField: TField;
    FInheritedTranslator: TCDSTranslator;
    StatStringList : TStringList;
    FActiveFilter: TTrxStatusSet;
    procedure SetActiveFilter(const Value: TTrxStatusSet);
    procedure MarkDeleted;
    function GetRecordStatus : TAppTrxStatus;
    procedure UpdateRecordStatus(const Value: TAppTrxStatus;
      const OriginalValue : string = ''; const TranslatedValue : string = '');
    procedure SetRecordStatus(const Value: TAppTrxStatus);
(*
    function UpdatePropertyValue(Component: TComponent;
      const ComponentName, PropertyName : string;
      var OriginalValue: string) : boolean;
*)
    function TranslateInheritedProperty(Component : TComponent;
      const ComponentName, PropertyName, PropertyValue: string): boolean;
    function FindInheritedProperty(Component : TComponent;
      const ComponentName, PropertyName, PropertyValue: string;
      LastInheritedLevel : boolean): boolean;
//    procedure UpdateCharSet(Component: TComponent);
    procedure UpdateStatistics;
    procedure AlignIndexFields;
    procedure SwapStatus(FromStatus, ToStatus: TAppTrxStatus; SaveData: boolean=True);
  protected
    function ReadTrxProperty(Component: TComponent;
      const PropertyName: string; out PropertyValue: string): boolean; virtual;
    procedure WriteTrxProperty(Component: TComponent;
      const PropertyName, TranslatedValue: string); virtual;
    function GetFieldClass(FieldType: TFieldType): TFieldClass; override;
    procedure InternalReadStatistics(const S : string); override;
    procedure InternalWriteStatistics(var S : string); override;
    procedure UpdateDeleted(Deleted : boolean; const ClassName : string='');
    function TranslateProperty( const ComponentName, PropName : string;
      out TranslatedValue : string ) : boolean;
    function TranslateMessage(const IDMessage, OriginalMessage, DefaultTranslation : string;
      out TranslatedValue : string ) : boolean;
    function TranslateComponent(Component : TComponent; ComponentName : string;
      UpdateFile : boolean) : boolean;
    procedure TranslateComponentProperty(Component : TComponent;
      const ComponentName, PropertyName : string; var PropertyValue : string);
    procedure UpdateTranslationFile(Component : TComponent;
      const ComponentName, PropertyName : string; var PropertyValue : string);
    property InheritedTranslator : TCDSTranslator read FInheritedTranslator write FInheritedTranslator;
    procedure SaveStatistics;
    procedure LoadStatistics;
    procedure CreateFields; override;
    procedure TrxFieldChange(Field : TField);
    procedure DoBeforePost; override;
  public
    TotMessages, MsgTranslated, MsgAutoTranslated, MsgToVerify, MsgToIgnore : integer; //statistics
    procedure OpenCDS; override;
    procedure GetStringsFromRepository(ReposType : TReposType);
    procedure LoadStringsFromRepository(TrxRepository : TCDSTrxRepository);
    procedure PutStringsIntoRepository(ReposType : TReposType;
      All : boolean=False; NoCapitalsNames : boolean=False; SaveData : boolean=True);
    procedure SaveStringsIntoRepository(TrxRepository : TCDSTrxRepository;
      All : boolean=False; NoCapitalsNames : boolean=False; SaveData : boolean=True);
    procedure SaveReformatXMLUTF8(Force : boolean=False); override;
    function TotMsgTranslated : integer;
    function TotMsgUntranslated : integer;
    function PercTranslated : double;
    constructor CreateCDSTranslator(AOwner : TComponent;
      const FileName : string; Language : TAppLanguage);
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure ReadStatistics;
    procedure IgnoreAllUntranslated(SaveData : boolean=True);
    procedure UntranslateAllIgnored(SaveData : boolean=True);
    property RecordStatus   : TAppTrxStatus read GetRecordStatus write SetRecordStatus;
    property TrxField       : TField read FTrxField      ;
    property OrigValueField : TField read FOrigValueField;
    property TypeField      : TField read FTypeField     ;
    property StatusField    : TField read FStatusField   ;
    property CompNameField  : TField read FCompNameField ;
    property PropNameField  : TField read FPropNameField ;
    property ClassNameField : TField read FClassNameField;
    property OrigStrField   : TField read FOrigStrField  ;
    property TrxStrField    : TField read FTrxStrField   ;
    property ActiveFilter   : TTrxStatusSet read FActiveFilter write SetActiveFilter;
  end;

  TFormTranslator = class(TComponent)
  private
    FFormList : TStringList; //Lista dei files xml per tradurre la form (o datamodule o frame)
    function GetTranslateLang: TAppLanguage;
  protected
    function CreateInternalCDSTranslator(const FileName : string;
      Language : TAppLanguage) : TCDSTranslator; virtual; abstract;
    procedure DoTranslation(InheritedLevel : integer);
  public
    property TranslateLang : TAppLanguage read GetTranslateLang;
    function AddForm(Form : TComponent; const FormName : string) : integer;
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure UpdateFiles(InheritedLevel : integer);
    procedure DoFormTranslation;
    function GotoComponent(ActiveComponent: TComponent; Out FormIndex: integer) : boolean;
    function GetFormList : TStringList;
//    procedure GetStringsFromRepository;
//    procedure PutStringsIntoRepository;
  end;

type
  TMessageTranslator = class(TComponent)
  private
    FCacheList : TStringList;
    procedure AddToCache( const IDFile : string; Dati : TCDSTranslator );
    function GetDatiFromCache( const IDFile : string ) : TCDSTranslator;
    function GetTranslator( const IDFile : string ) : TCDSTranslator;
    function LeggiDatiDaFile( const IDFile : string ) : TCDSTranslator;
    function GetCurrentLanguage: TAppLanguage;
    procedure MarkDeleted(const UnitName : string);
    procedure SaveTranslator(const UnitName : string);
    function GetCacheList: TStringList;
  public
    procedure EmptyCache;
    destructor Destroy; override;
    function GetRepositoryPath : string;
    function GetPathOfCurrentLanguage : string;
    function GetFullFileName(const IDFile : string ) : string;
    function TranslateMessage(const IDFile : string; const IDMessage : string;
      const OriginalMessage, DefaultTranslation : string ) : string;
    property CurrentLanguage : TAppLanguage read GetCurrentLanguage;
    property CacheList : TStringList read GetCacheList;
  end;

// ritorna l'ordine dello status a partire dalla stringa
function StrStatusToOrd(const status : string) : integer;
function StrStatusReposToOrd(const status : string) : integer;

// ritorna l'informazione che il supporto alla traduzione è attivo
function isTrxSupportActive : boolean;

// ritorna la CodePage di sistema
function GetSystemCodePage : Cardinal;

// ritorna la lingua di default
function GetDefaultLanguage : TAppLanguage;

// ritorna la lingua corrente
function GetCurrentLanguage : TAppLanguage;
function GetCurrentLanguageStr : string;

// ritorna la lingua corrente del Database
function GetCurrentLanguageDB : TAppLanguage;

// cambia la lingua corrente con la NewLanguage
procedure ChangeCurrentLanguage( NewLanguage : TAppLanguage );

// cambia la lingua relativa al Database con la NewLanguage
procedure ChangeCurrentLanguageDB( NewLanguage : TAppLanguage );

// cambia il repository delle lingue
procedure ChangeLanguagesRepository( const NewRepository : string );

// ritorna True se la lingua corrente e' l'italiano
function IsCurrAppLanguageItalian : Boolean;

// ritorna True se la lingua corrente e' l'inglese
function IsCurrAppLanguage(const ALanguage: TAppLanguage) : Boolean;

// se la lingua corrente e' l'italiano ritorna il ItalianMessage se questo e' specificato, altrimenti cerca la sua traduzione nei file delle traduzioni;
// se la lingua corrente e' l'inglese ritorna il EnglishMessage se questo e' specificato, altrimenti cerca la sua traduzione nei file delle traduzioni;
// se la lingua corrente non e' ne' l'italiano ne' l'inglese, cerca la traduzione del messaggio identificato con IDMessage nel file delle traduzioni identificato con IDFile;
// se la traduzione del messaggio non viene trovata, viene comunque ritornato il EnglishMessage se e' specificato, altrimenti il ItalianMessage
function GetMsgMultiLanguage( const UnitName: string; const IDMessage: string;
  const ItalianMessage: string = ''; const EnglishMessage: string = '' ) : string;

// stesse funzionalità ma come lingua secondaria accetta un messaggio tedesco
function GetMsgMultiLanguageDEU( const UnitName: string; const IDMessage : string;
  const ItalianMessage: string = ''; const DeutscheMessage: string = '' ) : string;

// stesse funzionalità ma come lingua secondaria accetta un messaggio spagnolo
function GetMsgMultiLanguageESP( const UnitName: string; const IDMessage : string;
  const ItalianMessage: string = ''; const SpanishMessage: string = '' ) : string;

// stesse funzionalità ma come lingua secondaria accetta un messaggio francese
function GetMsgMultiLanguageFRA( const UnitName: string; const IDMessage : string;
  const ItalianMessage: string = ''; const FrenchMessage: string = '' ) : string;

// stesse funzionalità ma come lingua secondaria accetta un messaggio portoghese
function GetMsgMultiLanguagePOR( const UnitName: string; const IDMessage : string;
  const ItalianMessage: string = ''; const PortugueseMessage: string = '' ) : string;

procedure StartMessagesRegistration(const UnitName : string);

procedure EndMessagesRegistration(const UnitName : string);

procedure RegisterTrxProperty(const ComponentClassName, PropertyName : string;
  TrxKind : TTrxKind);

procedure InitTrxSupport(const RepositoryPath : string;
  ApplicationLanguage : TAppLanguage; UpdateFiles : boolean; ADefaultLanguage : TAppLanguage = mlEnglish);

function GetTrxRepository(ReposType : TReposType; Language : TAppLanguage;
  const Path : string) : TCDSTrxRepository;

//Per registrare un servizio di traduzione di stringhe
procedure RegisterTranslationService(TranslationService: TTranslationService);

procedure DestroyRepositories;

// ritorna l'enumerativo corrispondente alla StrLingua specificata;
// solleva un'eccezione se la StrLingua specificata non corrisponde a nessuna lingua tra quelle gestite
function GetEnumFromStrLanguage( const StrLingua : string ) : TAppLanguage;

// ritorna l'enumerativo corrispondente alla Lingua Iso specificata;
// solleva un'eccezione se la Lingua Iso specificata non corrisponde a nessuna lingua tra quelle gestite
function GetEnumFromIsoLanguage( const IsoLanguangeId : string ) : TAppLanguage;

type
  stringfunction = function : string;

// ritorna la stringa vuota
function function_stringavuota : string;

// ritorna il CDS delle definizioni di traduzione
function GetCDSTrxPropList : TCDSTrxPropList;

// imposta la lingua tramite il codice
function SetAppLanguage(const ID : string; var AppLanguage : TAppLanguage) : boolean;

{$IFDEF MSWINDOWS}
  function CharSetToCodePage(ciCharset: UINT): Cardinal;
  function StringToWideStringEx(const S: AnsiString; CodePage: Cardinal): WideString;
  function WideStringToStringEx(const WS: WideString; CodePage: Cardinal): AnsiString;
{$ENDIF}

//Legge e scrive sul registro di windows la lingua dell'applicazione
function ReadAppLangFromReg(const CompanyName, ApplicationName : string;
  DefaultLang : TAppLanguage = mlItalian) : TAppLanguage;
procedure WriteAppLangToReg(const CompanyName, ApplicationName : string; Language : TAppLanguage);

//Verifica se la lingua è presente tra le lingue attive dell'applicazione
function isLanguageIdActive(LanguageId: string): boolean;
function isLanguageActive(Language: TAppLanguage): boolean;
//Funzione generica per la traduzione di una stringa che usa il servizio registrato
function TranslateValue(const SourceValue: string;
  SourceLanguage, TargetLanguage: TAppLanguage;
  out TranslatedValue: string): boolean;

function IsMoreThanOneAppLanguage: boolean;
function IsMoreThanOneGUILanguage: boolean;
function IsMoreThanOneDBLanguage: boolean;
function IsUpdateRepositoryFiles: boolean;
function IsApplicationTranslated: boolean;

//Funzione per determinare se un campo inizia con il prefisso di una lingua:
function StripLanguageField(var AFieldName: string; out ALanguage: TAppLanguage): Boolean;
var
  DBLanguages: string; //List of database languages supported
  AppLanguages: string; //List of application database languages currently active
  GUILanguages: string; //List of application GUI languages currently active
  ConfigAppLanguages: string; //List of application languages defined in configuration

implementation

uses
  System.Variants
  , System.StrUtils
  , System.Win.Registry
  , CBMultiLanguageMessages
  , CBMultiLanguageUtils
  ;

var
  SystemCodePage : Cardinal; // conterrà la CodePage di sistema
  DefaultLanguage : TAppLanguage; // conterra' la lingua di default
  CurrentApplicationLanguage, CurrentDatabaseLanguage : TAppLanguage;
  MessageTranslator : TMessageTranslator;
  TrxRepositoryPath : string;
  UpdateRepositoryFiles : boolean;
  CDSTrxPropList : TCDSTrxPropList;
  ATrxRepositoryList : Array [TReposType,TAppLanguage] of TCDSTrxRepository;
  MyTranslationService: TTranslationService;

function IsApplicationTranslated: boolean;
begin
  Result := (CurrentApplicationLanguage <> GetDefaultLanguage) and (TrxRepositoryPath <> '');
end;

function StripLanguageField(var AFieldName: string; out ALanguage: TAppLanguage): Boolean;
begin
  //Verifica se un campo ha il prefisso della lingua, es ITA_DX
  Result := False;
  for var I := Low(TAppLanguage) to High(TAppLanguage) do
  begin
    if AAppLanguageID[I]+'_' = Copy(AFieldName,1,4) then
    begin
      AFieldName := Copy(AFieldName,5,MaxInt);
      ALanguage := I;
      Result := True;
      Break;
    end;
  end;
end;

function IsUpdateRepositoryFiles: boolean;
begin
  Result := UpdateRepositoryFiles;
end;

function IsMoreThanOneAppLanguage: boolean;
begin
  Result := (pos(';',AppLanguages) > 0);
end;

function IsMoreThanOneGUILanguage: boolean;
begin
  Result := (pos(';',GUILanguages) > 0);
end;

function IsMoreThanOneDBLanguage: boolean;
begin
  Result := (pos(';',DBLanguages) > 0);
end;

function CalcFilterSpec(DictFilterSpec : TDictFilterSpec): string;

  procedure AddFilter(const Filter : string);
  begin
    if Result <> '' then
      Result := Result + ' and ';
    Result := Result +'('+Filter+')';
  end;

begin
  Result := '';
  if DictFilterSpec.ShowUntranslated then
    AddFilter(Format('Status = ''%s''',[AAppTrxStatus[tsDaTradurre]]));
  if DictFilterSpec.ShowAutoTranslated then
    AddFilter(Format('Status = ''%s''',[AAppTrxStatus[tsAutoTradotto]]));
  if DictFilterSpec.FromValue <> '' then
    AddFilter('OriginalValue >= '''+DictFilterSpec.FromValue+'''');
  if DictFilterSpec.ToValue <> '' then
    AddFilter('OriginalValue <= '''+DictFilterSpec.ToValue+
    'zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz'+'''');
end;

function ReadStatisticsSection(const S : string) : string;
var
  pStart, pEnd : integer;
begin
  Result := '';
  pstart := AnsiPos(START_STAT_TAG,S);
  pEnd := AnsiPos(END_STAT_TAG,S);
  if (pStart > 0) and (pEnd > 0) then
    Result := copy(S,pStart+length(START_STAT_TAG),
      pEnd-pStart+1);
end;

procedure WriteStatisticSection(var S: string; StatText : string);
var
  pStart, pEnd : integer;
  p : integer;
begin
  pstart := AnsiPos(START_STAT_TAG,S);
  pEnd := AnsiPos(END_STAT_TAG,S);
  if (pstart = 0) then
  begin
    //Non ci sono statistiche nel file: le aggiungo
    p := AnsiPos('?>',S);
    if p > 0 then
      S := Copy(S,1,p+1)+sLineBreak+
      START_STAT_TAG+StatText+END_STAT_TAG+
      Copy(S,p+2,MaxInt);
  end
  else if (pEnd <> 0) then
  begin
    //Ci sono già le statistiche nel file: le sostituisco
    S := Copy(s,1,pStart-1)+StatText+Copy(S,pEnd+length(END_STAT_TAG),MaxInt);
  end;
end;

function CheckStrToInt(const S : string) : integer;
begin
  if S = '' then
    Result := 0
  else
    Result := StrToInt(S);
end;

procedure InitTrxSupport(const RepositoryPath : string;
  ApplicationLanguage : TAppLanguage; UpdateFiles : boolean; ADefaultLanguage : TAppLanguage = mlEnglish);
begin
  DefaultLanguage := ADefaultLanguage;
  ChangeCurrentLanguage( ApplicationLanguage );
  ChangeLanguagesRepository( RepositoryPath );

  UpdateRepositoryFiles := UpdateFiles;

  if Assigned(CDSTrxPropList) then
    FreeAndNil(CDSTrxPropList);
  CDSTrxPropList := TCDSTrxPropList.Create(nil);
  CDSTrxPropList.FileName := TrxRepositoryPath+CDSTrxFileName;
  CDSTrxPropList.OpenCDS; //Apre o crea il file
end;

function GetTrxRepository(ReposType : TReposType; Language : TAppLanguage;
  const Path : string) : TCDSTrxRepository;
begin
  Result := nil;
  if Language = GetDefaultLanguage then
    exit;
  if not Assigned(ATrxRepositoryList[ReposType,Language]) then
  begin
    ATrxRepositoryList[ReposType,Language] := TCDSTrxRepository.Create(nil);
    ATrxRepositoryList[ReposType,Language].SetFileName(ReposType,Language,Path);
    ATrxRepositoryList[ReposType,Language].OpenCDS;
  end;
  Result := ATrxRepositoryList[ReposType,Language];
end;

//Per registrare un servizio di traduzione di stringhe
procedure RegisterTranslationService(TranslationService: TTranslationService);
begin
  MyTranslationService := TranslationService;
end;

procedure DestroyRepositories;
var
  i : TReposType;
  j : TAppLanguage;
begin
  for i := Low(TReposType) to High(TReposType) do
  begin
    for j := Low(TAppLanguage) to High(TAppLanguage) do
    begin
      if Assigned(ATrxRepositoryList[i,j]) then
      begin
        ATrxRepositoryList[i,j].Free;
        ATrxRepositoryList[i,j] := nil;
      end;
    end;
  end;
end;

procedure RegisterTrxProperty(const ComponentClassName, PropertyName : string;
  TrxKind : TTrxKind);
begin
  CDSTrxPropList.AddTrxProperty(ComponentClassName,PropertyName,
    TrxKind);
end;

function StrStatusToOrd(const status : string) : integer;
var
  i : TAppTrxStatus;
begin
  Result := -1;
  for i := Low(TAppTrxStatus) to High(TAppTrxStatus) do
  begin
    if AAppTrxStatus[i] = status then
    begin
      Result := Ord(i);
      break;
    end;
  end;
end;

function StrStatusReposToOrd(const status : string) : integer;
var
  i : TReposTrxStatus;
begin
  Result := -1;
  for i := Low(TReposTrxStatus) to High(TReposTrxStatus) do
  begin
    if AReposTrxStatus[i] = status then
    begin
      Result := Ord(i);
      break;
    end;
  end;
end;

function isTrxSupportActive : boolean;
begin
  Result := TrxRepositoryPath <> '';
end;

function GetSystemCodePage : Cardinal;
begin
  Result := SystemCodePage;
end;

function GetDefaultLanguage : TAppLanguage;
begin
  Result := DefaultLanguage;
end;

function GetCurrentLanguage : TAppLanguage;
begin
  Result := CurrentApplicationLanguage;
end;

function GetCurrentLanguageStr : string;
begin
  Result := AAppLanguageID[CurrentApplicationLanguage];
end;

function GetCurrentLanguageDB : TAppLanguage;
begin
  Result := CurrentDatabaseLanguage;
end;

procedure ChangeCurrentLanguage( NewLanguage : TAppLanguage );
//var
//  CharSetDellaLingua :  TFontCharset;
begin
  if NewLanguage <> CurrentApplicationLanguage then // la Lingua è cambiata
  begin
    // imposta la nuova Lingua
    CurrentApplicationLanguage := NewLanguage;
    // svuota la cache dei messaggi della lingua precedente
    if MessageTranslator <> nil then
      MessageTranslator.EmptyCache;
(*
    // imposta il set di caratteri appropriato nei Font dell'oggetto Screen
    with Screen do
    begin
      CharSetDellaLingua := AAppLangCharSet[CurrentApplicationLanguage];
      MenuFont.CharSet := CharSetDellaLingua;
      HintFont.CharSet := CharSetDellaLingua;
      IconFont.CharSet := CharSetDellaLingua;
    end;
*)
  end;
end;

procedure ChangeCurrentLanguageDB( NewLanguage : TAppLanguage );
begin
  if NewLanguage <> CurrentDatabaseLanguage then // la Lingua è cambiata
  begin
    // imposta la nuova Lingua del Database
    CurrentDatabaseLanguage := NewLanguage;
  end;
end;

procedure ChangeLanguagesRepository( const NewRepository : string );
begin
  if NewRepository <> TrxRepositoryPath then // il Repository è cambiato
  begin
    // imposta il nuovo Repository
    TrxRepositoryPath := NewRepository;
    // aggiunge l'eventuale PathDelim finale
    if Copy( TrxRepositoryPath, Length(TrxRepositoryPath), 1 ) <> PathDelim then
      TrxRepositoryPath := TrxRepositoryPath + PathDelim;
    // svuota la cache dei messaggi perché il repository è cambiato
    if MessageTranslator <> nil then
      MessageTranslator.EmptyCache;
  end;
end;

function IsCurrAppLanguageItalian : Boolean;
begin
  Result := CurrentApplicationLanguage = mlItalian;
end;

function IsCurrAppLanguage(const ALanguage: TAppLanguage) : Boolean;
begin
  Result := CurrentApplicationLanguage = ALanguage;
end;

function GetEnumFromStrLanguage( const StrLingua : string ) : TAppLanguage;
var
  I : TAppLanguage;
begin
  if StrLingua = '' then
  begin
    Result := mlEnglish; // se la lingua non e' specificata, usa quella inglese
    Exit;
  end;
  // scorre l'array delle stringhe delle lingue
  for I := Low(TAppLanguage) to High(TAppLanguage) do
  begin
    if AAppLanguageID[I] = StrLingua then
    begin
      Result := I;
      Exit;
    end;
  end;
  // non è stata trovata la lingua corrispondente a StrLingua
  raise EMultiLanguageException.CreateFmt( ERR_LANGUAGE_STR, [StrLingua] );
end;

function GetEnumFromIsoLanguage( const IsoLanguangeId : string ) : TAppLanguage;
var
  I : TAppLanguage;
begin
  for I := Low(AAppLanguageISOID) to High(AAppLanguageISOID) do
  begin
    if SameText(AAppLanguageISOID[I],IsoLanguangeId) then
    begin
      Result := I;
      Exit;
    end;
  end;
  // non è stata trovata la lingua corrispondente a IsoLanguangeId
  raise EMultiLanguageException.CreateFmt( ERR_LANGUAGE_STR, [IsoLanguangeId] );
end;

function InternalGetMsgMultiLanguage( const UnitName : string; const IDMessage : string;
  const ItalianMessage : string = ''; const OtherMessage : string = '';
  const OtherLang: TAppLanguage = mlEnglish) : string;
begin
  if IsCurrAppLanguageItalian then
  begin
    Result := ItalianMessage;
    if ((Result = '') or UpdateRepositoryFiles) and (TrxRepositoryPath <> '') then
    begin
      // il messaggio in italiano non e' specificato, cerca la sua traduzione nei file delle traduzioni
      Result := MessageTranslator.TranslateMessage( UnitName, IDMessage, OtherMessage, ItalianMessage );
      if Result = '' then // traduzione in italiano del messaggio non trovata
        Result := OtherMessage; // ritorna comunque il messaggio in lingua
      Exit;
    end;
  end
  else if IsCurrAppLanguage(OtherLang) then
  begin
    Result := OtherMessage;
    if ((Result = '') or UpdateRepositoryFiles) and (TrxRepositoryPath <> '') then
    begin
      // il messaggio in inglese non e' specificato, cerca la sua traduzione nei file delle traduzioni
      if TrxRepositoryPath <> '' then
        Result := MessageTranslator.TranslateMessage( UnitName, IDMessage, ItalianMessage, OtherMessage );
      if Result = '' then // traduzione in inglese del messaggio non trovata
        Result := ItalianMessage; // ritorna comunque il messaggio in italiano
      Exit;
    end;
  end
  else // la lingua corrente non e' ne' l'italiano ne' l'inglese
  begin
    if DefaultLanguage = OtherLang then
    begin
      // Se la lingua di default è l'inglese cerco la traduzione della stringa inglese senza alcun default
      if TrxRepositoryPath <> '' then
        Result := MessageTranslator.TranslateMessage( UnitName, IDMessage, OtherMessage, '' );
      if Result = '' then // traduzione del messaggio non trovata
        Result := OtherMessage; // ritorna comunque il messaggio in inglese
    end
    else if DefaultLanguage = mlItalian then
    begin
      // Se la lingua di default è l'inglese cerco la traduzione della stringa italiana senza alcun default
      if TrxRepositoryPath <> '' then
        Result := MessageTranslator.TranslateMessage( UnitName, IDMessage, ItalianMessage, '' );
      if Result = '' then // traduzione del messaggio non trovata
        Result := ItalianMessage; // ritorna comunque il messaggio in italiano
    end;
  end;
end;

function GetMsgMultiLanguage(const UnitName : string; const IDMessage: string;
  const ItalianMessage: string = ''; const EnglishMessage: string = '') : string;
begin
  Result := InternalGetMsgMultiLanguage(UnitName, IDMessage, ItalianMessage, EnglishMessage, mlEnglish);
end;

function GetMsgMultiLanguageDEU(const UnitName: string; const IDMessage: string;
  const ItalianMessage: string = ''; const DeutscheMessage: string = '') : string;
begin
  Result := InternalGetMsgMultiLanguage(UnitName, IDMessage, ItalianMessage, DeutscheMessage, mlGerman);
end;

function GetMsgMultiLanguageESP( const UnitName: string; const IDMessage : string;
  const ItalianMessage: string = ''; const SpanishMessage: string = '' ) : string;
begin
  Result := InternalGetMsgMultiLanguage(UnitName, IDMessage, ItalianMessage, SpanishMessage, mlSpanish);
end;

function GetMsgMultiLanguageFRA( const UnitName: string; const IDMessage : string;
  const ItalianMessage: string = ''; const FrenchMessage: string = '' ) : string;
begin
  Result := InternalGetMsgMultiLanguage(UnitName, IDMessage, ItalianMessage, FrenchMessage, mlFrench);
end;

function GetMsgMultiLanguagePOR( const UnitName: string; const IDMessage : string;
  const ItalianMessage: string = ''; const PortugueseMessage: string = '' ) : string;
begin
  Result := InternalGetMsgMultiLanguage(UnitName, IDMessage, ItalianMessage, PortugueseMessage, mlPortuguese);
end;

procedure StartMessagesRegistration(const UnitName : string);
begin
  if GetCurrentLanguage = GetDefaultLanguage then
    Exit
  else
    // marca "cancellati" i record dei messaggi
    MessageTranslator.MarkDeleted(UnitName);
end;

procedure EndMessagesRegistration(const UnitName : string);
begin
  if GetCurrentLanguage = GetDefaultLanguage then
    Exit;
  // salva il file CDS andando a cancellare i record rimasti "cancellati"
  MessageTranslator.SaveTranslator(UnitName);
end;

function function_stringavuota : string;
begin
  Result := '';
end;

function GetCDSTrxPropList : TCDSTrxPropList;
begin
  Result := CDSTrxPropList;
end;

function SetAppLanguage(const ID : string; var AppLanguage : TAppLanguage) : boolean;
var
  AppLang : TAppLanguage;
  L: integer;
begin
  L := Length(ID);
  for AppLang := Low(TAppLanguage) to High(TAppLanguage) do
  begin
    if SameText(Copy(AAppLanguageID[AppLang],1,L),Copy(ID,1,L)) then
    begin
      AppLanguage := AppLang;
      Result := True;
      exit;
    end;
  end;
  Result := False;  //se non lo trova ritorna false
end;

{ TFormTranslator }

function TFormTranslator.AddForm(Form : TComponent; const FormName : string) : integer;
var
  CDSTranslator : TCDSTranslator;
  FileName : string;
  CorrectFormName : string;
begin
  if (CurrentApplicationLanguage = GetDefaultLanguage) or (TrxRepositoryPath = '') then
  begin
    Result := -1;
    exit;
  end;
  CorrectFormName := RemoveFormNameSuffix(FormName);
  //Verifica l'esistenza del file XML per le traduzioni: se non esiste lo crea.
  Result := FFormList.IndexOf(CorrectFormName);
  if Result < 0 then
  begin
    FileName := TrxRepositoryPath+AAppLanguageID[TranslateLang]+PathDelim+
      FORMS_FOLDER+PathDelim+CorrectFormName+TRANSLATION_FILE_EXT;
    CDSTranslator := CreateInternalCDSTranslator(FileName, TranslateLang);
    Result := FFormList.AddObject(CorrectFormName,CDSTranslator);
  end;
  if UpdateRepositoryFiles then
    UpdateFiles(Result);
end;

constructor TFormTranslator.Create(AOwner : TComponent);
begin
  inherited;
  FFormList := TStringList.Create;
end;

destructor TFormTranslator.Destroy;
begin
  FFormList.Free;
  inherited;
end;

procedure TFormTranslator.DoTranslation(InheritedLevel : integer);
var
  i,j : integer;
  Component : TComponent;
  Form : TComponent;
  CDSTranslator : TCDSTranslator;
  z : integer;
begin
  if InheritedLevel = -1 then
    z := FFormList.Count -1
  else
    z := InheritedLevel;

  //Apro tutti i CDS
  for i := 0 to FFormList.Count -1 do
  begin
    CDSTranslator := TCDSTranslator(FFormList.objects[i]);
    //Aggancio ad ogni translator il suo "padre"
    if i > 0 then
      CDSTranslator.InheritedTranslator := TCDSTranslator(FFormList.objects[i-1]);
    if not CDSTranslator.Active then
      CDSTranslator.OpenCDS;
  end;

  //Ciclo sui translator per richiedere la traduzione
  for i := z to FFormList.Count -1 do
  begin
    Form := Owner as TComponent;
    CDSTranslator := TCDSTranslator(FFormList.objects[i]);
    Try
      CDSTranslator.TranslateComponent(Form,SELF_STR,False);
      //Ciclo sui Componenti della form per effettuare la traduzione
      for j := 0 to Form.ComponentCount -1 do
      begin
        Component := Form.Components[j];
        CDSTranslator.TranslateComponent(Component, Component.Name,False);
      end;
    Finally
      CDSTranslator.SaveReformatXMLUTF8;
    End;
  end;
end;

(*
procedure TFormTranslator.GetStringsFromRepository;
var
  i : integer;
  CDSTranslator : TCDSTranslator;
begin
  for i := 0 to FFormList.Count -1 do
  begin
    CDSTranslator := TCDSTranslator(FFormList.objects[i]);
    CDSTranslator.GetStringsFromRepository;
  end;
end;
*)

function TFormTranslator.GetTranslateLang: TAppLanguage;
begin
  Result := CurrentApplicationLanguage;
end;

(*
procedure TFormTranslator.PutStringsIntoRepository;
var
  i : integer;
  CDSTranslator : TCDSTranslator;
begin
  for i := 0 to FFormList.Count -1 do
  begin
    CDSTranslator := TCDSTranslator(FFormList.objects[i]);
    CDSTranslator.PutStringsIntoRepository;
  end;
end;
*)

procedure TFormTranslator.UpdateFiles(InheritedLevel : integer);
var
  i,j : integer;
  Component : TComponent;
  Form : TComponent;
  CDSTranslator : TCDSTranslator;
begin
  //Aggiorna i files XML di traduzione se il dfm italiano è cambiato
  for i := InheritedLevel to FFormList.Count -1 do
  begin
    Form := Owner as TComponent;
    CDSTranslator := TCDSTranslator(FFormList.objects[i]);
    if i > 0 then
      CDSTranslator.InheritedTranslator := TCDSTranslator(FFormList.objects[i-1]);

    CDSTranslator.OpenCDS;  
    Try
      CDSTranslator.MarkDeleted;
      CDSTranslator.TranslateComponent(Form,SELF_STR,True);
      //Ciclo sui Componenti della form per effettuare la traduzione
      for j := 0 to Form.ComponentCount -1 do
      begin
        Component := Form.Components[j];
        CDSTranslator.TranslateComponent(Component, Component.Name, True);
      end;
    Finally
      CDSTranslator.SaveReformatXMLUTF8;
    End;
  end;
end;

procedure TFormTranslator.DoFormTranslation;
begin
  if IsApplicationTranslated then
    DoTranslation(FFormList.Count-1);
end;

function TFormTranslator.GetFormList: TStringList;
begin
  Result := FFormList;
end;

function TFormTranslator.GotoComponent(ActiveComponent: TComponent;
  out FormIndex: integer): boolean;
var
  i : integer;
  CDSTranslator : TCDSTranslator;
begin
  Result := False;
  //Ciclo sui translator per richiedere la traduzione a partire dall'ultimo livello
  //di ereditarietà della form
  for i := FFormList.Count -1 downto 0 do
  begin
    CDSTranslator := TCDSTranslator(FFormList.objects[i]);
    if not CDSTranslator.Active then
      CDSTranslator.OpenCDS;
    //Cerco il componente
    if CDSTranslator.Locate(FLD_COMP_NAME,ActiveComponent.Name,[]) then
    begin
      FormIndex := i;
      Result := True;
      Exit;
    end;
  end;
end;

{ TCDSTranslator }

constructor TCDSTranslator.Create(AOwner: TComponent);
begin
  inherited;
  StatStringList := TStringList.Create;
  fielddefs.Add(FLD_TYPE,ftString,2,True);
  fielddefs.Add(FLD_COMP_NAME,ftString,100,True);
  fielddefs.Add(FLD_PROP_NAME,ftString,100,True);
  fielddefs.Add(FLD_ORIG_VALUE,ftMemo,0,True);
  fielddefs.Add(FLD_TRX,ftMemo,0,False);
  fielddefs.Add(FLD_STATUS,ftString,1,True);
  fielddefs.Add(FLD_DELETED,ftBoolean,0,True);
  fielddefs.Add(FLD_CLASSNAME,ftString,50,False);
  fielddefs.Add(FLD_ORIG_STR,ftString,STR_SIZE,False);
  fielddefs.Add(FLD_TRX_STR,ftString,STR_SIZE,False);
end;

constructor TCDSTranslator.CreateCDSTranslator(AOwner: TComponent;
  const FileName : string; Language : TAppLanguage);
begin
  Create(AOwner);
  FTranslateLang := Language;
  self.FileName := FileName;
end;

// ritorna la stringa tradotta relativa alla proprietà specificata;
// se la proprietà non viene trovata, Found viene impostato a False e viene ritornata la stringa vuota
function TCDSTranslator.TranslateProperty( const ComponentName, PropName : string;
  out TranslatedValue : string ) : boolean;
begin
  IndexName := PK_INDEX;
  if FindKey([ComponentName,PropName]) then
  begin
    Result := True;
    TranslatedValue := TrimRight(FTrxField.AsString);
  end
  else
  begin
    Result := False;
    TranslatedValue := '';
  end;
end;

(*
function TCDSTranslator.UpdatePropertyValue(Component : TComponent;
  const ComponentName, PropertyName : string;
  var OriginalValue : string) : boolean;
begin
  IndexName := PK_INDEX;
  if FindKey([ComponentName,PropertyName]) then
  begin
    OriginalValue := FOrigValueField.AsString;
    Result := True;
  end
  else if InheritedTranslator <> nil then
  begin
    //Cerco la proprietà per leggere correttamente il valore originale
    //della proprietà
    Result := InheritedTranslator.UpdatePropertyValue(Component,
      ComponentName,PropertyName,OriginalValue);
  end
  else
  begin
    Result := False;
  end;
end;
*)

function TCDSTranslator.TranslateInheritedProperty(Component : TComponent;
  const ComponentName, PropertyName, PropertyValue : string) : boolean;
var
  TranslatedValue : string;
begin
  Result := TranslateProperty(ComponentName,PropertyName,TranslatedValue);
  if Result then
  begin
    if (TranslatedValue <> '') then
    begin
      //Ho trovato la traduzione:
      //UpdateCharSet(Component);

      //metto la traduzione nella proprietà
      WriteTrxProperty(Component,PropertyName,TranslatedValue);
    end;
  end
  else if InheritedTranslator <> nil then
  begin
    Result := InheritedTranslator.TranslateInheritedProperty(Component,
      ComponentName,PropertyName,PropertyValue);
  end;
end;

procedure TCDSTranslator.TranslateComponentProperty(Component : TComponent;
  const ComponentName, PropertyName : string; var PropertyValue : string);
begin
  //Recupero il valore originale della proprietà (perché se eredita ha il
  //valore già tradotto!)
//  UpdatePropertyValue(Component, ComponentName, PropertyName, PropertyValue);
  //Effettua la traduzione in modalità di ereditarietà
  TranslateInheritedProperty(Component, ComponentName, PropertyName, PropertyValue);
end;

function TCDSTranslator.TranslateComponent(Component: TComponent; ComponentName : string;
  UpdateFile : boolean) : boolean;
var
  PropertyName: string;
  PropertyValue : string;
  ComponentClassName : string;
  ComponentClass : TClass;
  i : integer;
begin
  Result := False;
  //Ciclo sul CDS contenente le proprietà da tradurre
  CDSTrxPropList.First;
  while not CDSTrxPropList.eof do
  begin
    ComponentClassName := CDSTrxPropList.FFieldComponentClassName.AsString;
    //Cerco la classe nella string-list (che è molto più veloce che fare GetClass)
    i := CDSTrxPropList.FRegisteredClassList.IndexOf(UpperCase(ComponentClassName));
    if i >= 0 then
      ComponentClass := TClass(CDSTrxPropList.FRegisteredClassList.Objects[i])
    else
      ComponentClass := nil;
    if not assigned(ComponentClass) then
    begin
      //Se non la trovo uso GetClass e la aggiungo alla lista
      ComponentClass := GetClass(ComponentClassName);
      if Assigned(ComponentClass) then
        CDSTrxPropList.FRegisteredClassList.AddObject(UpperCase(ComponentClassName), TObject(ComponentClass));
    end;
    if (Component.InheritsFrom(ComponentClass)) then
    begin
      //Ho trovato la classe giusta: verifico la proprietà
      if IsPublishedProp(Component,CDSTrxPropList.FFieldPropertyName.AsString) then
      begin
        PropertyName := CDSTrxPropList.FFieldPropertyName.AsString;
        //la proprietà da tradurre esiste in questo Component
        if ReadTrxProperty(Component, PropertyName, PropertyValue) then
        begin
          Result := True;
          //Effettuo la traduzione o l'aggiornamento del file xml:
          //Non devo mai tradurre stringhe vuote!
          if PropertyValue <> '' then
          begin
            if UpdateFile then
              UpdateTranslationFile(Component, ComponentName, PropertyName,PropertyValue)
            else
              TranslateComponentProperty(Component, ComponentName, PropertyName,PropertyValue);
          end;
        end;
      end;
    end;
    CDSTrxPropList.Next;
  end;
end;


function TCDSTranslator.ReadTrxProperty(Component : TComponent;
  const PropertyName : string; out PropertyValue : string) : boolean;
begin
  Result := False; //nella classe discendente implementare la funzione
end;

procedure TCDSTranslator.WriteTrxProperty(Component: TComponent;
  const PropertyName, TranslatedValue: string);
begin
  ; //nella classe discendente implementare la funzione
end;

procedure TCDSTranslator.OpenCDS;
begin
  inherited;
  AddIndex(PK_INDEX,FLD_COMP_NAME+';'+FLD_PROP_NAME,
    [ ixPrimary, ixUnique ]);
  IndexName := PK_INDEX;
  First;
end;

procedure TCDSTranslator.MarkDeleted;
begin
  First;
  //Ciclo sui record per marcarli "da cancellare" in modo che alla fine quelli che
  //restano così marcati sono proprietà che sono sparite dalla form/datamodule
  if UpdateRepositoryFiles then
  begin
    while not eof do
    begin
      UpdateDeleted(True);
      Next;
    end;
  end;
end;

procedure TCDSTranslator.UpdateDeleted(Deleted : boolean; const ClassName : string='');
begin
  Assert(Assigned(FDeletedField));
  if ((ClassName <> '') and (FClassNameField.AsString <> ClassName)) or
    (FDeletedField.AsBoolean <> Deleted) then
  begin
    Edit;
    Try
      if ClassName <> '' then
        FClassNameField.AsString := ClassName;
      FDeletedField.AsBoolean := Deleted;
      Post;
    Except
      Cancel;
      raise;
    End;
  end;
end;

procedure TCDSTranslator.SaveReformatXMLUTF8(Force : boolean=False);
begin
  //Allineo gli indici
  First;
  while not eof do
  begin
    AlignIndexFields;
    Next;
  end;

  //Prima di salvare i dati se devo aggiornare i files elimino i record
  //delle proprietà cancellate
  if UpdateRepositoryFiles then
  begin
    First;
    while not eof do
    begin
      //Controllo che i campi str siano allineati
      if FDeletedField.AsBoolean then
        Delete
      else
        Next;
    end;
  end;
  inherited;
end;

procedure TCDSTranslator.LoadStringsFromRepository(
  TrxRepository: TCDSTrxRepository);
var
  OriginalList, TranslatedList : TStringList;
  OriginalValue, TranslatedValue : string;
  i : integer;
  TranslateStatus: TAppTrxStatus;
  ReposTranslateStatus: TReposTrxStatus;
  BookMark : TBookmark;

  procedure SetTranslateRecordStatus(const NewValue : string; Status : TAppTrxStatus);
  begin
    if (FTrxField.AsString <> NewValue) or (FStatusField.AsString <> AAppTrxStatus[tsAutoTradotto])  then
    begin
      Edit;
      Try
        FTrxField.AsString := NewValue;
        FStatusField.AsString := AAppTrxStatus[tsAutoTradotto];
        Post;
      Except
        Cancel;
        raise;
      End;
    end;
  end;

begin
  OriginalList := nil;
  TranslatedList := nil;
  //sgancio temporaneamente on-change del field
  FTrxField.OnChange := nil;
  BookMark := GetBookmark;
  DisableControls;
  Try
    OriginalList := TStringList.Create;
    TranslatedList := TStringList.Create;
    First;
    while not eof do
    begin
      //Devo tentare di tradurre le diciture con status DaTradurre o AutoTradotte o DaVerificare
      if (GetRecordStatus in [tsDaTradurre, tsAutoTradotto, tsDaVerificare]) then
      begin
        if AnsiPos(sLineBreak, FOrigValueField.AsString) > 0 then
        begin
          //Sto traducendo una dicitura multiriga
          OriginalList.Text := FOrigValueField.AsString;
          TranslatedList.Clear;
          TranslateStatus := tsDaTradurre;
          for i := 0 to OriginalList.Count -1 do
          begin
            OriginalValue := OriginalList.Strings[i];
            if TrxRepository.GetTranslatedValue(OriginalValue,TranslatedValue,
              ReposTranslateStatus) then
            begin
              //Ho trovato la traduzione nello status ReposTranslateStatus
              TranslatedList.add(TranslatedValue);
              //Solo se tutti gli elementi della lista sono tradotti uso lo status Tradotto
              if (ReposTranslateStatus = rsTradotto) and (TranslateStatus <> tsAutoTradotto) then
                TranslateStatus := tsTradotto
              else
                TranslateStatus := tsAutoTradotto;
            end
            else
            begin
              //Non trovo la riga nel repository: gli copio dentro lo stesso valore originario
              TranslatedList.add(OriginalValue);
              //Siccome non ho autotradotto tutto, imposto lo status "da verificare"
              TranslateStatus := tsDaVerificare;
            end;
          end;
          if TranslateStatus <> tsDaTradurre then
            SetTranslateRecordStatus(TranslatedList.Text, TranslateStatus);
        end
        else if TrxRepository.GetTranslatedValue(FOrigValueField.AsString,TranslatedValue,
          ReposTranslateStatus) then
        begin
          //Sto traducendo una dicitura di "singola riga" uso lo status del repository
          if ReposTranslateStatus = rsTradotto then
            SetTranslateRecordStatus(TranslatedValue, tsTradotto)
          else
            SetTranslateRecordStatus(TranslatedValue, tsAutoTradotto);
        end
        else if (GetRecordStatus=tsAutoTradotto) and (FTrxField.AsString = '') then
          SetTranslateRecordStatus('',tsDaTradurre);
      end;
      Next;
    end;
  Finally
    FTrxField.OnChange := TrxFieldChange;
    OriginalList.Free;
    TranslatedList.Free;
    GotoBookmark(BookMark);
    FreeBookmark(BookMark);
    EnableControls;
  End;
end;

procedure TCDSTranslator.GetStringsFromRepository(ReposType : TReposType);
var
  TrxRepository : TCDSTrxRepository;
begin
  TrxRepository := GetTrxRepository(ReposType,FTranslateLang,TrxRepositoryPath);
  LoadStringsFromRepository(TrxRepository);
end;

procedure TCDSTranslator.SaveStringsIntoRepository(TrxRepository : TCDSTrxRepository;
  All : boolean=False; NoCapitalsNames : boolean=False; SaveData : boolean=True);
var
  BookMark : TBookmark;
  OrigValue : string;
begin
  BookMark := GetBookmark;
  DisableControls;
  Try
    TrxRepository.LogChanges := False;
    First;
    while not eof do
    begin
      if (All and (RecordStatus <> tsDaIgnorare)) or
        ((FTrxField.AsString <> '') and (RecordStatus = tsTradotto)) then
      begin
        OrigValue := FOrigValueField.AsString;
        TrxRepository.AddMemo(OrigValue,FTrxField.AsString,NoCapitalsNames);
      end;
      Next;
    end;
    if SaveData then
      TrxRepository.SaveReformatXMLUTF8(True);
  Finally
    TrxRepository.LogChanges := True;
    GotoBookmark(BookMark);
    FreeBookmark(BookMark);
    EnableControls;
  End;
end;

procedure TCDSTranslator.PutStringsIntoRepository(ReposType : TReposType;
  All : boolean=False; NoCapitalsNames : boolean=False; SaveData : boolean=True);
var
  TrxRepository : TCDSTrxRepository;
begin
  TrxRepository := GetTrxRepository(ReposType,FTranslateLang,TrxRepositoryPath);
  SaveStringsIntoRepository(TrxRepository,All,NoCapitalsNames,SaveData);
end;

procedure TCDSTranslator.LoadStatistics;
begin
  ;
end;

procedure TCDSTranslator.SaveStatistics;
begin
  ;
end;

procedure TCDSTranslator.InternalReadStatistics(const S: string);
begin
  inherited;
  //leggo la sezione delle statistiche
  StatStringList.Text := ReadStatisticsSection(S);
  TotMessages := CheckStrToInt(StatStringList.Values[STAT_TOT_TAG]);
  MsgTranslated := CheckStrToInt(StatStringList.Values[AAppTrxStatus[tsTradotto]]);
  MsgAutoTranslated := CheckStrToInt(StatStringList.Values[AAppTrxStatus[tsAutoTradotto]]);
  MsgToVerify := CheckStrToInt(StatStringList.Values[AAppTrxStatus[tsDaVerificare]]);
  MsgToIgnore := CheckStrToInt(StatStringList.Values[AAppTrxStatus[tsDaIgnorare]]);
end;

procedure TCDSTranslator.InternalWriteStatistics(var S: string);
begin
  inherited;
  StatStringList.Clear;
  UpdateStatistics;
  StatStringList.Values[STAT_TOT_TAG] := IntToStr(TotMessages);
  StatStringList.Values[AAppTrxStatus[tsTradotto]] := IntToStr(MsgTranslated);
  StatStringList.Values[AAppTrxStatus[tsAutoTradotto]] := IntToStr(MsgAutoTranslated);
  StatStringList.Values[AAppTrxStatus[tsDaVerificare]] := IntToStr(MsgToVerify);
  StatStringList.Values[AAppTrxStatus[tsDaIgnorare]] := IntToStr(MsgToIgnore);

  //Aggiornamento sezione delle statistiche
  WriteStatisticSection(S, StatStringList.Text);
end;

destructor TCDSTranslator.Destroy;
begin
  StatStringList.Free;
  inherited;
end;

procedure TCDSTranslator.UpdateStatistics;
begin
  Filtered := False;
  TotMessages := RecordCount;
  MsgTranslated := 0;
  MsgAutoTranslated := 0;
  MsgToVerify := 0;
  MsgToIgnore := 0;
  First;
  while not eof do
  begin
    case RecordStatus of
      tsTradotto : Inc(MsgTranslated);
      tsAutoTradotto : Inc(MsgAutoTranslated);
      tsDaVerificare : Inc(MsgToVerify);
      tsDaIgnorare : Inc(MsgToIgnore);
    end;
    Next;
  end;
  First;
end;

procedure TCDSTranslator.ReadStatistics;
begin
  if not Active then
    ReadStatisticsFromFile;
end;

function TCDSTranslator.TotMsgUntranslated: integer;
begin
  Result := TotMessages - (TotMsgTranslated + MsgToVerify);
end;

function TCDSTranslator.PercTranslated: double;
begin
  if TotMsgTranslated <> 0 then
    Result := (TotMsgTranslated*100) / TotMessages
  else
    Result := 0;
end;

function TCDSTranslator.TotMsgTranslated: integer;
begin
  Result := MsgTranslated + MsgAutoTranslated + MsgToIgnore;
end;

procedure TCDSTranslator.CreateFields;
begin
  inherited;
  FTypeField := FindField(FLD_TYPE);
  FTrxField := FindField(FLD_TRX);
  FOrigValueField := FindField(FLD_ORIG_VALUE);
  FStatusField := FindField(FLD_STATUS);
  FCompNameField := FindField(FLD_COMP_NAME);
  FPropNameField := FindField(FLD_PROP_NAME);
  FDeletedField := FindField(FLD_DELETED);
  FClassNameField := FindField(FLD_CLASSNAME);
  FOrigStrField := FindField(FLD_ORIG_STR);
  FTrxStrField := FindField(FLD_TRX_STR);
  FOrigStrField.Visible := False;
  FTrxStrField.Visible := False;

  //Aggancio l'onchange del campo delle traduzioni
  FTrxField.OnChange := TrxFieldChange;

  //Rendo invisibile alcuni campi
  FStatusField.Visible := False;
  FTypeField.Visible := False;
end;

function TCDSTranslator.GetFieldClass(FieldType: TFieldType): TFieldClass;
begin
  if FieldType in [ftMemo, ftFmtMemo, ftWideMemo] then
    Result := TTrxMemoField
  else
    Result := Inherited GetFieldClass(FieldType);
end;

procedure TCDSTranslator.TrxFieldChange(Field: TField);
begin
  if TrxField.AsString <> '' then
    StatusField.AsString := AAppTrxStatus[tsTradotto]
  else
    StatusField.AsString := AAppTrxStatus[tsDaTradurre];
end;

procedure TCDSTranslator.SetActiveFilter(const Value: TTrxStatusSet);
var
  FilterStr : string;
  i : TAppTrxStatus;
begin
  if FActiveFilter <> Value then
  begin
    if Value = [] then
      Filtered := False
    else
    begin
      for i := Low(TAppTrxStatus) to High(TAppTrxStatus) do
      begin
        if i in Value then
        begin
          if FilterStr <> '' then
            FilterStr := FilterStr + ' or ';
          FilterStr := FilterStr + FLD_STATUS+'='''+AAppTrxStatus[i]+'''';
        end;
      end;
      Filter := FilterStr;
      Filtered := True;
    end;
    FActiveFilter := Value;
  end;
end;

function TCDSTranslator.GetRecordStatus: TAppTrxStatus;
begin
  for Result := Low(TAppTrxStatus) to High(TAppTrxStatus) do
    if AAppTrxStatus[Result] = FStatusField.AsString then
      break;
end;

procedure TCDSTranslator.UpdateRecordStatus(const Value: TAppTrxStatus;
  const OriginalValue : string = ''; const TranslatedValue : string = '');
var
  AutoEdit : boolean;
begin
  AutoEdit := not (state in [dsEdit,dsInsert]);
  if AutoEdit then
    Edit;
  Try
    if OriginalValue <> '' then
      FOrigValueField.AsString := OriginalValue;

    //Se viene passato un valore di traduzione lo memorizzo
    if TranslatedValue <> '' then
    begin
      FTrxField.AsString := TranslatedValue;
    end
    else
    begin
      //Se imposto "tradotto" ma non ho la traduzione la copio dall'origine
      if (Value = tsTradotto) and (FTrxField.AsString = '') then
        FTrxField.AsString := FOrigValueField.AsString;

      //Se imposto "da tradurre" ma non ho già la traduzione la sbianco
      if (Value = tsDaTradurre) and (FTrxField.AsString <> '') then
        FTrxField.AsString := '';
    end;

    FStatusField.AsString := AAppTrxStatus[Value];

    if AutoEdit then
      Post;
  Except
    if AutoEdit then
      Cancel;
    raise;
  End;
end;

procedure TCDSTranslator.SetRecordStatus(const Value: TAppTrxStatus);
begin
  UpdateRecordStatus(Value);
end;

procedure TCDSTranslator.UpdateTranslationFile(Component: TComponent;
  const ComponentName, PropertyName: string; var PropertyValue: string);
begin
  //Recupero il valore originale della proprietà
  if not FindInheritedProperty(Component, ComponentName, PropertyName, PropertyValue, True) then
  begin
    if ComponentName <> '' then // c'era un componente che non aveva nome
    begin
      //Aggiungo la proprietà nuova al file delle traduzioni
      AppendRecord([
        AAppLangType[ltForm],          //FLD_TYPE
        ComponentName,                 //FLD_COMP_NAME
        PropertyName,                  //FLD_PROP_NAME
        PropertyValue,                 //FLD_ORIG_VALUE
        '',                            //FLD_TRX
        AAppTrxStatus[tsDaTradurre],   //FLD_STATUS
        False,                         //FLD_DELETED
        Component.ClassName,           //FLD_CLASSNAME
        Copy(PropertyValue,1,STR_SIZE),//FLD_ORIG_STR
        ''                             //FLD_TRX_STR
        ]);
    end;
  end;
end;

function TCDSTranslator.FindInheritedProperty(Component: TComponent;
  const ComponentName, PropertyName, PropertyValue: string;
  LastInheritedLevel : boolean): boolean;
begin
  IndexName := PK_INDEX;
  Result := FindKey([ComponentName,PropertyName]);
  if Result then
  begin
    //Ho trovato il record nel file xml: devo gestire il caso in cui
    //il valore originario in italiano è diverso da quello a run-time
    if FOrigValueField.AsString <> PropertyValue then
    begin
      if LastInheritedLevel then
      begin
        //Se sono nell'ultimo livello di ereditarietà e la proprietà a run-time
        //è diversa da quella scritta nell'XML aggiorno il
        //valore nell'XML dicendo di verificare la traduzione
        UpdateRecordStatus(tsDaVerificare, PropertyValue);
      end
      else
      begin
        //Se non sono sull'ultimo livello di ereditarietà e la proprietà a run-time
        //è diversa da quella scritta nell'XML significa che il programmatore ha
        //modificato il valore in italiano della proprietà sul livello ereditato
        //quindi dico di non aver trovato il record
        Result := False;
      end;
    end;
    //Se in un livello precedente di ereditarietà trovo la stessa proprietà con lo
    //stesso valore significa che è avvenuto un "revert to inherited" quindi non
    //ripristino lo status di cancellazione, così il record verrà cancellato.
    if (InheritedTranslator = nil) or not InheritedTranslator.FindInheritedProperty(Component,
      ComponentName,PropertyName,PropertyValue,False) then
    begin
      UpdateDeleted(False, Component.ClassName); //ripristina lo status di cancellazione
    end;
  end
  else if (InheritedTranslator <> nil) then
  begin
    Result := InheritedTranslator.FindInheritedProperty(Component,
      ComponentName,PropertyName,PropertyValue,False);
  end;
end;

(*
procedure TCDSTranslator.UpdateCharSet(Component: TComponent);
var
  Font : TFont;
begin
  {$IFDEF MSWINDOWS}
  //Imposto il charset corretto
  if IsPublishedProp(Component,'Font') then
  begin
    Font := TFont(GetObjectProp(Component,'Font',Tfont));
    Font.Charset := AAppLangCharSet[FTranslateLang];
  end;
  {$ENDIF}
end;
*)

function TCDSTranslator.TranslateMessage(const IDMessage,
  OriginalMessage, DefaultTranslation: string; out TranslatedValue: string): boolean;
begin
  // ricerca la traduzione di IDMessage nei Dati
  Result := TranslateProperty( COMPONENT_NAME_FOR_MESSAGE, IDMessage, TranslatedValue );
  if Result then
  begin
    // messaggio trovato
    if UpdateRepositoryFiles then
    begin
      UpdateDeleted(False);  //ripristina lo status di cancellazione
      //Se ho un DefaultTranslation  e il TranslatedValue è vuoto oppure il record è autotranslated
      if (DefaultTranslation <> '') and
        ((TranslatedValue = '') or (GetRecordStatus = tsAutoTradotto))  then
      begin
        UpdateRecordStatus(tsAutoTradotto,'',DefaultTranslation);
        TranslatedValue := DefaultTranslation;
      end;  
      //Se il messaggio originale è cambiato cambio lo status in "da verificare"
      if OriginalMessage <> FOrigValueField.AsString then
        UpdateRecordStatus(tsDaVerificare,OriginalMessage);
    end;
  end;
end;

{ TMessageTranslator }

// ritorna il path (senza backslash finale) del repository delle lingue leggendo la variabile globale TrxRepositoryPath;
// solleva un'eccezione se la variabile globale TrxRepositoryPath contiene la stringa vuota
function TMessageTranslator.GetRepositoryPath : string;
begin
  // se il TrxRepositoryPath è vuoto, solleva un'eccezione
  if TrxRepositoryPath = '' then
    raise EMultiLanguageException.Create(ERR_TRXPATH_NOT_SET);
  Result := TrxRepositoryPath;
end;

// ritorna il Path della lingua corrente dove cercare i file per le traduzioni;
// il path è calcolato tramite il metodo GetRepositoryPath e la proprietà CurrentLanguage;
function TMessageTranslator.GetPathOfCurrentLanguage: string;
begin
  // costruisce il path per la lingua corrente
  Result := GetRepositoryPath+AAppLanguageID[CurrentLanguage];
end;

// ritorna il nome del file completo di path ed estensione corrispondente a IDFile;
// il path viene calcolato tramite il metodo GetPathOfCurrentLanguage
function TMessageTranslator.GetFullFileName(const IDFile: string): string;
begin
  Result := GetPathOfCurrentLanguage+PathDelim+
    UNITS_FOLDER+PathDelim+IDFile+TRANSLATION_FILE_EXT;
end;

// legge i dati dal file identificato da IDFile; se il file non viene trovato e UpdateRepositoryFiles è True, il file viene creato vuoto.
// ritorna il riferimento all'oggetto contenente i dati oppure nil se il file non è stato trovato e UpdateRepositoryFiles è False
function TMessageTranslator.LeggiDatiDaFile(const IDFile: string): TCDSTranslator;
var
  FullFileName, Dir : string;
begin
  FullFileName := GetFullFileName( IDFile );
  if UpdateRepositoryFiles then // crea il file della lingua, se non lo trova
  begin
    Dir := ExtractFilePath(FullFileName);
    if not ForceDirectories( Dir ) then
      raise EMultiLanguageException.CreateFmt( ERR_CANNOT_CREATE_DIR, [Dir] );
    // crea l'oggetto che conterrà il file
    Result := TCDSTranslator.CreateCDSTranslator( nil, FullFileName, CurrentApplicationLanguage );
    Try
      // apre l'oggetto, se il file non esiste lo crea vuoto
      Result.OpenCDS;
    Except
      Result.Free;
      raise;
    End;
  end
  else // non crea il file della lingua, se non lo trova
  begin
    if FileExists(FullFileName) then
    begin
      // crea l'oggetto che conterrà il file
      Result := TCDSTranslator.CreateCDSTranslator( nil, FullFileName, CurrentApplicationLanguage );
      Try
        // apre l'oggetto, il file esiste già
        Result.OpenCDS;
      Except
        Result.Free;
        raise;
      End;
    end
    else
      Result := nil;
  end;
end;

// traduce il messaggio identificato da IDMessage nella lingua corrente, utilizzando il file identificato da IDFile;
// se il messaggio non viene trovato e la proprietà UpdateRepositoryFiles è True, il file per la traduzione del messaggio viene creato o aggiornato;
// se il messaggio non viene trovato e la proprietà UpdateRepositoryFiles è False, non viene creato o aggiornato alcun file;
// in entrambi i casi viene ritornata la stringa vuota
function TMessageTranslator.TranslateMessage( const IDFile : string;
  const IDMessage : string; const OriginalMessage, DefaultTranslation : string ) : string;
var
  CDSTranslator : TCDSTranslator;
  TranslatedValue : string;
  Status : TAppTrxStatus;
begin
  if not IsApplicationTranslated then
    Exit;
  CDSTranslator := GetTranslator(IDFile);
  // se i dati non vengono trovati
  if (CDSTranslator = nil) or (OriginalMessage='') then
  begin
    Result := '';
    Exit;
  end;
  if CDSTranslator.TranslateMessage(IDMessage,OriginalMessage, DefaultTranslation, TranslatedValue) then
  begin
    Result := TranslatedValue;
  end
  else // messaggio non trovato
  begin
    if UpdateRepositoryFiles then
    begin
      if (DefaultTranslation <> '') then
        Status := tsAutoTradotto
      else
        Status := tsDaTradurre;
      // aggiungo il nuovo messaggio al file delle traduzioni
      CDSTranslator.AppendRecord([
        AAppLangType[ltMessage],
        COMPONENT_NAME_FOR_MESSAGE,
        IDMessage,
        OriginalMessage,
        DefaultTranslation,
        AAppTrxStatus[Status],
        False
        ]);
    end;
    Result := '';
  end;
end;

procedure TMessageTranslator.MarkDeleted(const UnitName: string);
var
  CDSTranslator : TCDSTranslator;
begin
  // recupera il translator
  CDSTranslator := GetTranslator( UnitName );
  // marca i record
  if CDSTranslator <> nil then
    CDSTranslator.MarkDeleted;
end;

procedure TMessageTranslator.SaveTranslator(const UnitName: string);
var
  CDSTranslator : TCDSTranslator;
begin
  // legge i dati dal file esterno
  CDSTranslator := GetTranslator( UnitName );
  // Salva i dati (togliendo i record rimasti "cancellati logicamente")
  if (CDSTranslator <> nil) and (CDSTranslator.Active) then
    CDSTranslator.SaveReformatXMLUTF8;
end;

// aggiunge i Dati relativi a IDFile alla Cache;
// se IDFile è già presente nella Cache, i vecchi Dati vengono liberati e poi rimpiazzati
procedure TMessageTranslator.AddToCache(const IDFile: string;
  Dati: TCDSTranslator);
var
  I : Integer;
begin
  if IDFile <> '' then
  begin
    // ricerca IDFile nella Cache
    I := CacheList.IndexOf( IDFile );
    if I < 0 then // IDFile non trovato nella cache, lo aggiunge
    begin
      CacheList.AddObject( IDFile, Dati );
    end
    else // IDFile trovato nella cache, rimpiazza i vecchi dati
    begin
      CacheList.Objects[I].Free; // libera i vecchi dati
      CacheList.Objects[I] := Dati; // rimpiazza i vecchi dati
    end;
  end;
end;

// ritorna i dati relativi a IDFile prelevandoli dalla Cache;
// ritorna nil se non ci sono dati relativi a IDFile nella Cache
function TMessageTranslator.GetDatiFromCache(
  const IDFile: string): TCDSTranslator;
var
  I : Integer;
begin
  I := CacheList.IndexOf(IDFile);
  if I < 0 then // dati non trovati nella Cache
    Result := nil
  else
    Result := CacheList.Objects[I] as TCDSTranslator;
end;

function TMessageTranslator.GetCacheList: TStringList;
begin
  if not Assigned(FCacheList) then
    FCacheList := TStringList.Create;
  Result := FCacheList;
end;

destructor TMessageTranslator.Destroy;
begin
  EmptyCache;
  FreeAndNil(FCacheList);
  inherited;
end;

// azzera la cache salvando i dati in sospeso
procedure TMessageTranslator.EmptyCache;
var
  I : Integer;
  Translator : TCDSTranslator;
begin
  with CacheList do
  begin
    // libera i dati contenuti nella StringList
    for I := Count - 1 downto 0 do
    begin
      Translator := Objects[I] as TCDSTranslator;
      Translator.Close;
      // toglie l'elemento dalla StringList
      Delete(I);
      // libera l'oggetto che era puntato nella StringList
      Translator.Free;
    end;
  end;
end;

function TMessageTranslator.GetCurrentLanguage: TAppLanguage;
begin
  Result := CurrentApplicationLanguage;
end;

//Ritorna il translator in base al nome della unit: se non lo trova già nella
//cache lo legge da file: se non esiste il file ritorna nil
function TMessageTranslator.GetTranslator(const IDFile: string): TCDSTranslator;
begin
  // preleva i dati relativi a IDFile dalla Cache
  Result := GetDatiFromCache( IDFile );
  // se i dati non sono nella Cache, li carica dal file esterno
  if Result = nil then
  begin
    // legge i dati dal file esterno
    Result := LeggiDatiDaFile( IDFile );
    // se i dati non vengono trovati
    if Result = nil then
      Exit;
    // aggiunge i dati alla Cache
    AddToCache( IDFile, Result );
  end;
end;

procedure TCDSTranslator.AlignIndexFields;
var
  AutoEdit : boolean;
begin
  Assert(Assigned(FOrigStrField));
  Assert(Assigned(FOrigValueField));
  Assert(Assigned(FTrxStrField));
  Assert(Assigned(FTrxField));
  //Controllo che i campi di supporto all'ordinamento siano allineati
  if (FOrigStrField.AsString <> Trim(Copy(FOrigValueField.AsString,1,STR_SIZE))) or
     (FTrxStrField.AsString <> Trim(Copy(FTrxField.AsString,1,STR_SIZE))) then
  begin
    AutoEdit := not (state in [dsEdit,dsInsert]);
    if AutoEdit then
      Edit;
    Try
      FOrigStrField.AsString := Trim(Copy(FOrigValueField.AsString,1,STR_SIZE));
      FTrxStrField.AsString := Trim(Copy(FTrxField.AsString,1,STR_SIZE));
      if AutoEdit then
        Post;
    Except
      if AutoEdit then
        Cancel;
      raise;
    End;
  end;
end;

procedure TCDSTranslator.DoBeforePost;
begin
  inherited;
  AlignIndexFields;
end;

procedure TCDSTranslator.SwapStatus(FromStatus, ToStatus : TAppTrxStatus;
  SaveData : boolean=True);
var
  BookMark : TBookmark;
begin
  BookMark := GetBookmark;
  DisableControls;
  Try
    LogChanges := False;
    First;
    while not eof do
    begin
      if RecordStatus=FromStatus then
        SetRecordStatus(ToStatus);
      Next;
    end;
    if SaveData then
      SaveReformatXMLUTF8(True);
  Finally
    LogChanges := True;
    GotoBookmark(BookMark);
    FreeBookmark(BookMark);
    EnableControls;
  End;
end;

procedure TCDSTranslator.IgnoreAllUntranslated(SaveData : boolean=True);
begin
  SwapStatus(tsDaTradurre,tsDaIgnorare,SaveData);
end;

procedure TCDSTranslator.UntranslateAllIgnored(SaveData: boolean);
begin
  SwapStatus(tsDaIgnorare,tsDaTradurre,SaveData);
end;

{ TCDSTrxPropList }

procedure TCDSTrxPropList.AddTrxProperty(const ComponentClassName,
  PropertyName: string; TrxKind: TTrxKind);
begin
  if not Active then
    Open;

  IndexName := PK_INDEX;
  if UpdateRepositoryFiles and not FindKey([ComponentClassName,PropertyName]) then
  begin
    //Aggiungo la proprietà da tradurra nel file della lista
    AppendRecord([
      ComponentClassName, //FLD_CTRLCLASSNAME
      PropertyName,     //FLD_PROP_NAME
      Ord(TrxKind)     //FLD_PROPTRXKIND,ftInteger,0,True);
      ]);
  end;    
end;

constructor TCDSTrxPropList.Create(AOwner: TComponent);
begin
  inherited;
  fielddefs.Add(FLD_CTRLCLASSNAME,ftString,50,True);
  fielddefs.Add(FLD_PROP_NAME,ftString,50,True);
  fielddefs.Add(FLD_PROPTRXKIND,ftInteger,0,True);
  FRegisteredClassList := TStringList.Create;
  FRegisteredClassList.Sorted := True;
end;

destructor TCDSTrxPropList.Destroy;
begin
  FRegisteredClassList.Free;
  inherited;
end;

function TCDSTrxPropList.GetFieldTrxKind: TField;
begin
  Result := FFieldTrxKind;
end;

procedure TCDSTrxPropList.OpenCDS;
begin
  inherited;
  AddIndex(PK_INDEX,FLD_CTRLCLASSNAME+';'+FLD_PROP_NAME,
    [ixPrimary, ixUnique]);
  IndexName := PK_INDEX;
  First;  
end;

procedure TCDSTrxPropList.SetActive(Value: Boolean);
begin
  inherited;
  if Value then
  begin
    FFieldComponentClassName := FieldByName(FLD_CTRLCLASSNAME);
    FFieldPropertyName := FieldByName(FLD_PROP_NAME);
    FFieldTrxKind := FieldByName(FLD_PROPTRXKIND);
  end
  else
  begin
    FFieldComponentClassName := nil;
    FFieldPropertyName := nil;
    FFieldTrxKind := nil;
  end;
end;

{ TTrxClientDataSet }

procedure TTrxClientDataSet.CloseCursor;
var
  OldFileName : string;
begin
  DeleteLockFile;
  OldFileName := FileName; // salva il FileName
  //Per evitare di salvare i dati nel formato sbagliato
  FileName := '';
  Try
    Data := NULL;
    inherited;
  Finally
    FileName := OldFileName; // ripristina il FileName
  End;
end;

procedure TTrxClientDataSet.OpenCDS;
begin
  if FileName = '' then
    raise EMultiLanguageException.Create(ERR_FILE_NAME);
  if not FileExists(FileName) then
  begin
    IndexFieldNames := '';
    CreateDataSet;
  end
  else
  begin
    IndexFieldNames := '';
    Open;
  end;
  if not UpdateRepositoryFiles then
    LogChanges := False;
end;

procedure TTrxClientDataSet.ReadStatisticsFromFile;
var
  F : TextFile;
  S : string;
  C : integer;
  OutPut : string;
begin
  OutPut := '';
  AssignFile(F,FileName);
  Reset(F); // apre il file
  try
    C := 0;
    while not System.Eof(F) do
    begin
      // legge una riga del file
      ReadLn(F,S);
      OutPut := OutPut + S + sLineBreak;
      Inc(C);
      if (C > 20) then
        Break;
    end;
  finally
    InternalReadStatistics(OutPut);
    CloseFile(F);
  end;
end;

procedure TTrxClientDataSet.SaveReformatXMLUTF8(Force : boolean=False);
var
  Dir : string;
begin
  //salvo i dati in formato XMLUTF8 e riformatto l'XML
  if (FileName <> '') and ((ChangeCount > 0) or Force) then
  begin
    MergeChangeLog;
    Dir := ExtractFilePath( FileName );
    if not ForceDirectories( Dir ) then
      raise EMultiLanguageException.CreateFmt( ERR_CANNOT_CREATE_DIR, [Dir] );
    SaveToFile(FileName, dfXMLUTF8);
    //Trasformo il file XML salvato per aggiungervi gli "a-capo" in modo
    //da renderlo utilizzabile con version-Control systems

    ReformatTextFile;
  end;
  DeleteLockFile;
end;

procedure TTrxClientDataSet.InternalReadStatistics(const S: string);
begin
  ;
end;

procedure TTrxClientDataSet.InternalWriteStatistics(var S: string);
begin
  ;
end;

procedure TTrxClientDataSet.ReloadData;
begin
  Close;
  ReadStatisticsFromFile;
  OpenCDS;
end;

procedure TTrxClientDataSet.ReformatTextFile;
var
  F : TextFile;
  S : string;
  XMLSource, XMLOutPut : string;

begin
  XMLSource := '';
  // legge il file
  AssignFile(F,FileName);
  Reset(F);
  try
    while not System.Eof(F) do
    begin
      // legge una riga del file
      ReadLn(F,S);
      XMLSource := XMLSource + S + sLineBreak;
    end;
  finally
    CloseFile(F);
  end;
  XMLOutPut := StringReplace(XMLSource,'><','>'+sLineBreak+'<',[rfReplaceAll]);
  WriteTextFile(XMLOutPut);
end;

procedure TTrxClientDataSet.WriteTextFile(const S : string);
var
  F : TextFile;
  Content : string;
begin
  AssignFile(F,FileName);
  ReWrite(F); // riscrive il file
  try
    Content := S;
    InternalWriteStatistics(Content);
    Write(F,Content); // scrive tutta la stringa sul file
  finally
    CloseFile(F);
  end;
end;

procedure TTrxClientDataSet.InternalEdit;
begin
  CreateLockFile;
  inherited;
end;

procedure TTrxClientDataSet.InternalInsert;
begin
  CreateLockFile;
  inherited;
end;

procedure TTrxClientDataSet.InternalDelete;
begin
  CreateLockFile;
  inherited;
end;

procedure TTrxClientDataSet.CreateLockFile;
var
  DateStr : string;
  FLockFile : TextFile;
  SessionStr : string;
  ReposPath : string;
begin
  if FLockedFile or (FSessionUserName = '') then
    exit;
  AssignFile(FLockFile,LockFileName);

  if FileExists(LockFileName) then
  begin
    //Locking!
    Reset(FLockFile);
    ReadLn(FLockFile,SessionStr);
    CloseFile(FLockFile);
    raise ETranslatorLockException.CreateLckFmt(LockFileName,
      LCK_ERROR+sLineBreak+SessionStr,[FileName]);
  end
  else
  begin
    ReposPath := ExtractFilePath(LockFileName);
    ForceDirectories(ReposPath);
  end;

  DateStr := formatdatetime('dd/mm/yyyy hh:mm:ss', Now);
  ReWrite(FLockFile); // apre il file di lock
  Try
    FLockedFile := True;
    Write(FLockFile, Format(LCK_LOG, [SessionUserName,DateStr])); // scrive l'ora di sessione
  Finally
    CloseFile(FLockFile);
  End;
end;

function TTrxClientDataSet.LockFileName: string;
begin
  Result := ChangeFileExt(Self.FileName,'.lck');
end;

procedure TTrxClientDataSet.CancelUpdates;
begin
  inherited CancelUpdates;
  DeleteLockFile;
end;

procedure TTrxClientDataSet.DeleteLockFile;
begin
  if FLockedFile and FileExists(LockFileName) then
  begin
    SysUtils.DeleteFile(LockFileName);
    FLockedFile := False;
  end;  
end;

destructor TTrxClientDataSet.Destroy;
begin
  DeleteLockFile;
  inherited;
end;

{ TCDSTrxRepository }

procedure TCDSTrxRepository.AddMemo(const OriginalValue, TranslatedValue: string;
  NoCapitalsNames : boolean);
var
  OriginalList, TranslatedList : TStringList;
  i: integer;

  procedure AddString(const Original, Translated : string);
  begin
    if (Trim(Original)='') then
      Exit;
    //verifica che la dicitura originale non sia "simile" a un nome di campo (tutta maiuscola)
    if NoCapitalsNames and
      (UpperCase(Trim(Original))=Trim(Original)) and (pos(' ',Trim(Original))=0) then
      Exit;

    IndexName := PK_INDEX;
    if not FindKey([Original]) then
      Append
    else
      Edit;
    Try
      FOrigValueField.AsString := Original;
      if FStatusField.AsString <> AReposTrxStatus[rsAutoTradotto] then
      begin
        if Translated <> '' then
        begin
          FTrxField.AsString := Translated;
          FStatusField.AsString := AReposTrxStatus[rsTradotto];
        end
        else
          FStatusField.AsString := AReposTrxStatus[rsDaTradurre];
      end;
      //Aggiorna le occorrenze
      FCountField.AsInteger := FCountField.AsInteger+1;
      Post;
    Except
      Cancel;
      raise;
    End;
  end;

begin
  OriginalList := nil;
  TranslatedList := nil;
  Try
    OriginalList := TStringList.Create;
    TranslatedList := TStringList.Create;
    OriginalList.Text := OriginalValue;
    TranslatedList.Text := TranslatedValue;
    //Se le due liste non sono lunghe uguali c'è qualcosa di strano
    //tranne il caso della TranslatedList vuota: significa che sto salvando
    //una traduzione vuota per essere tradotta nel repository
    if (OriginalList.Count <> TranslatedList.Count) and (TranslatedList.Count <> 0) then
      exit;
    for i := 0 to OriginalList.Count -1 do
    begin
      if TranslatedList.Count = 0 then
        AddString(OriginalList.Strings[i], '')
      else
        AddString(OriginalList.Strings[i], TranslatedList.Strings[i]);
    end;
  Finally
    OriginalList.Free;
    TranslatedList.Free;
  End;
end;

constructor TCDSTrxRepository.Create(AOwner: TComponent);
begin
  inherited;
  StatStringList := TStringList.Create;
  fielddefs.Add(FLD_ORIG_VALUE,ftString,250,True);
  fielddefs.Add(FLD_TRX,ftString,250,False);
  fielddefs.Add(FLD_COUNT,ftInteger,0,False);
  fielddefs.Add(FLD_STATUS,ftString,1,True);
  ResetFilterSpec;
end;

procedure TCDSTrxRepository.CreateFields;
begin
  inherited;
  FOrigValueField := FindField(FLD_ORIG_VALUE);
  FTrxField := FindField(FLD_TRX);
  FCountField := FindField(FLD_COUNT);
  FStatusField := FindField(FLD_STATUS);
end;

function TCDSTrxRepository.GetOrigValueField: TField;
begin
  Result := FOrigValueField;
end;

function TCDSTrxRepository.GetRecordStatus: TReposTrxStatus;
begin
  for Result := Low(TReposTrxStatus) to High(TReposTrxStatus) do
    if AReposTrxStatus[Result] = FStatusField.AsString then
      break;
end;

function TCDSTrxRepository.GetStatusField: TField;
begin
  Result := FStatusField;
end;

function TCDSTrxRepository.GetTrxField: TField;
begin
  Result := FTrxField;
end;

procedure TCDSTrxRepository.OpenCDS;
begin
  inherited;
  LogChanges := True; //Il repository deve sempre essere modificabile
  AddIndex(PK_INDEX,FLD_ORIG_VALUE,[ixPrimary, ixUnique, ixCaseInsensitive]);
  IndexName := PK_INDEX;
  First;
end;

procedure TCDSTrxRepository.SetActive(Value: boolean);
begin
  inherited;
end;

procedure TCDSTrxRepository.SetLanguage(Value: TAppLanguage);
begin
  if FLanguage <> Value then
  begin
    Close;
    FLanguage := Value;
  end;
end;

procedure TCDSTrxRepository.SetRecordStatus(const Value: TReposTrxStatus);
begin
  UpdateRecordStatus(Value);
end;

procedure TCDSTrxRepository.SetTranslatedValue(const Value: string;
  Status: TReposTrxStatus);
begin
  UpdateRecordStatus(Status, Value);
end;

function TCDSTrxRepository.GetTranslatedValue(const Value: string;
  out TranslatedValue: string; out TranslatedStatus: TReposTrxStatus): boolean;
begin
  IndexName := PK_INDEX;
  if FindKey([Value]) and (FTrxField.AsString <> '') and (RecordStatus in [rsTradotto, rsAutoTradotto]) then
  begin
    Result := True;
    TranslatedValue := FTrxField.AsString;
    TranslatedStatus := RecordStatus;
  end
  else
  begin
    Result := False;
    TranslatedValue := '';
    TranslatedStatus := rsDaTradurre;
  end;
end;

procedure TCDSTrxRepository.InternalReadStatistics(const S: string);
var
  StrLang : string;
begin
  inherited;
  //leggo la sezione delle statistiche
  StatStringList.Text := ReadStatisticsSection(S);
  TotMessages := CheckStrToInt(StatStringList.Values[STAT_TOT_TAG]);
  StrLang := StatStringList.Values[STAT_LANGUAGE];
  if StrLang <> '' then
    FLanguage := GetEnumFromStrLanguage(StrLang)
  else
    FLanguage := mlEnglish;
  MsgTranslated := CheckStrToInt(StatStringList.Values[AReposTrxStatus[rsTradotto]]);
  MsgAutoTranslated := CheckStrToInt(StatStringList.Values[AReposTrxStatus[rsAutoTradotto]]);
end;

procedure TCDSTrxRepository.InternalWriteStatistics(var S: string);
begin
  inherited;
  StatStringList.Clear;
  UpdateStatistics;
  StatStringList.Values[STAT_TOT_TAG] := IntToStr(TotMessages);
  StatStringList.Values[AReposTrxStatus[rsTradotto]] := IntToStr(MsgTranslated);
  StatStringList.Values[AReposTrxStatus[rsAutoTradotto]] := IntToStr(MsgAutoTranslated);
  StatStringList.Values[STAT_LANGUAGE] := AAppLanguageID[FLanguage];

  //Aggiornamento sezione delle statistiche
  WriteStatisticSection(S,StatStringList.Text);
end;

destructor TCDSTrxRepository.Destroy;
begin
  StatStringList.Free;
  inherited;
end;

procedure TCDSTrxRepository.DoBeforePost;
begin
  inherited;
  if (RecordStatus = rsDaTradurre) and (FTrxField.AsString <> '') then
    SetRecordStatus(rsTradotto)
  else if (FTrxField.AsString = '') then
    SetRecordStatus(rsDaTradurre);
end;

procedure TCDSTrxRepository.UpdateRecordStatus(const Value: TReposTrxStatus;
  const TranslatedValue: string);
var
  AutoEdit : boolean;
begin
  AutoEdit := not (state in [dsEdit,dsInsert]);
  if AutoEdit then
    Edit;
  Try
    //Se viene passato un valore di traduzione lo memorizzo
    if TranslatedValue <> '' then
    begin
      FTrxField.AsString := TranslatedValue;
    end
    else
    begin
      //Se imposto "da tradurre" ma non ho già la traduzione la sbianco
      if (Value = rsDaTradurre) and (FTrxField.AsString <> '') then
        FTrxField.AsString := '';
    end;

    FStatusField.AsString := AReposTrxStatus[Value];

    if AutoEdit then
      Post;
  Except
    if AutoEdit then
      Cancel;
    raise;
  End;
end;

procedure TCDSTrxRepository.UpdateStatistics;
begin
  Filtered := False;
  TotMessages := RecordCount;
  MsgTranslated := 0;
  MsgAutoTranslated := 0;
  First;
  while not eof do
  begin
    if RecordStatus = rsTradotto then
      Inc(MsgTranslated)
    else if RecordStatus = rsAutoTradotto then
      Inc(MsgAutoTranslated);
    Next;
  end;
end;

function TCDSTrxRepository.PercTranslated: double;
begin
  if MsgTranslated <> 0 then
    Result := (MsgTranslated*100) / TotMessages
  else
    Result := 0;
end;

function TCDSTrxRepository.MsgUntranslated: integer;
begin
  Result := TotMessages - MsgTranslated;
end;

procedure TCDSTrxRepository.ReadStatistics;
begin
  if not Active then
    ReadStatisticsFromFile;
end;

procedure TCDSTrxRepository.SetFilterSpec(const Value: TDictFilterSpec);
var
  NewFilter : string;
begin
  NewFilter := CalcFilterSpec(Value);
  if NewFilter <> Filter then
  begin
    FFilterSpec.ShowUntranslated := Value.ShowUntranslated;
    FFilterSpec.ShowAutoTranslated := Value.ShowAutoTranslated;
    FFilterSpec.FromValue := Value.FromValue;
    FFilterSpec.ToValue := Value.ToValue;
    Filter := NewFilter;
    Filtered := Filter <> '';
  end;
end;

procedure TCDSTrxRepository.ResetFilterSpec;
begin
  FFilterSpec.ShowUntranslated := False;
  FFilterSpec.ShowAutoTranslated := False;
  FFilterSpec.FromValue := '';
  FFilterSpec.ToValue := '';
  Filter := CalcFilterSpec(FFilterSpec);
end;

procedure TCDSTrxRepository.SetFileName(ReposType : TReposType; Language : TAppLanguage; const
  Path : string);
begin
  FLanguage := Language;
  FRepositoryPath := Path;
  //Il nome del repository è dato da:
  FileName := FRepositoryPath+AAppLanguageID[FLanguage]+PathDelim+ //Path di base
    REPOSITORY_FOLDER+PathDelim+ //Cartella della lingua
    AReposType[ReposType]+REPOS_NAME_SUFFIX+'.xml'; //Tipo di repository: es. FormsTrxRepository.xml
end;

function TCDSTrxRepository.GetCountField: TField;
begin
  Result := FCountField;
end;

{ TTrxMemoField }

procedure TTrxMemoField.GetText(var Text: string; DisplayText: Boolean);
var
  Testo : string;
begin
  inherited;
  //Sostituisco gli a-capo con il pipe
  Testo := StringReplace(AsString,sLineBreak,' | ',[rfReplaceAll]);
  if (length(Testo)>MAX_MEMO_VIEW) then
  begin
    Text := LeftStr(Testo,MAX_MEMO_VIEW)+'...';
  end
  else
    Text := Testo;
end;

{ ETranslatorLockException }

constructor ETranslatorLockException.CreateLckFmt(const LockFileName,
  Msg: string; const Args: array of const);
begin
  FLockFileName := LockFileName;
  Message := Format(Msg, Args);
end;

{$IFDEF MSWINDOWS}
  function TranslateCharsetInfo(lpSrc: PDWORD; var lpCs: TCharsetInfo; dwFlags: DWORD): BOOL; stdcall; external gdi32 name 'TranslateCharsetInfo';

  function CharSetToCodePage(ciCharset: UINT): Cardinal;
  var
    C: TCharsetInfo;
  begin
    Win32Check(TranslateCharsetInfo(PDWORD(ciCharset), C, TCI_SRCCHARSET));
    Result := C.ciACP
  end;

  function StringToWideStringEx(const S: AnsiString; CodePage: Cardinal): WideString;
  var
    InputLength,
    OutputLength: Integer;
  begin
    InputLength := Length(S);
    OutputLength := MultiByteToWideChar(CodePage, 0, PAnsiChar(S), InputLength, nil, 0);
    SetLength(Result, OutputLength);
    MultiByteToWideChar(CodePage, 0, PAnsiChar(S), InputLength, PWideChar(Result), OutputLength);
  end;

  function WideStringToStringEx(const WS: WideString; CodePage: Cardinal): AnsiString;
  var
    InputLength,
    OutputLength: Integer;
  begin
    InputLength := Length(WS);
    OutputLength := WideCharToMultiByte(CodePage, 0, PWideChar(WS), InputLength, nil, 0, nil, nil);
    SetLength(Result, OutputLength);
    WideCharToMultiByte(CodePage, 0, PWideChar(WS), InputLength, PAnsiChar(Result), OutputLength, nil, nil);
  end;
{$ENDIF}

function ReadAppLangFromReg(const CompanyName, ApplicationName : string;
  DefaultLang : TAppLanguage = mlItalian) : TAppLanguage;
var
  FRegistry : TRegistry;
  RegistryKey : string;
  LangStr : string;
begin
  FRegistry := TRegistry.Create(KEY_ALL_ACCESS);
  try
    FRegistry.RootKey := HKEY_CURRENT_USER;
    RegistryKey := Format(REG_KEY,[CompanyName, ApplicationName]);
    FRegistry.OpenKey(RegistryKey, True);
    LangStr := FRegistry.ReadString(CONFIG_VALUE);
    if LangStr <> '' then
      Result := GetEnumFromStrLanguage(LangStr)
    else
      Result := CurrentApplicationLanguage;
  finally
    FRegistry.Free;
  end;
end;

procedure WriteAppLangToReg(const CompanyName, ApplicationName : string; Language : TAppLanguage);
var
  FRegistry : TRegistry;
  RegistryKey : string;
begin
  FRegistry := TRegistry.Create(KEY_ALL_ACCESS);
  try
    RegistryKey := Format(REG_KEY,[CompanyName, ApplicationName]);
    FRegistry.RootKey := HKEY_CURRENT_USER;
    FRegistry.OpenKey(RegistryKey, True);
    FRegistry.WriteString(CONFIG_VALUE,AAppLanguageID[Language]);
  finally
    FRegistry.Free;
  end;
end;

function isLanguageIdActive(LanguageId: string): boolean;
begin
  Result := (Pos(LanguageId, AppLanguages) > 0)
    or (AAppLanguageID[CurrentDatabaseLanguage] = LanguageId);
end;

function isLanguageActive(Language: TAppLanguage): boolean;
begin
  Result := isLanguageIdActive(AAppLanguageID[Language]);
end;

function TranslateValue(const SourceValue: string;
  SourceLanguage, TargetLanguage: TAppLanguage;
  out TranslatedValue: string): boolean;
begin
  if Assigned(MyTranslationService) then
    Result := MyTranslationService(SourceValue, SourceLanguage, TargetLanguage, TranslatedValue)
  else
  begin
    TranslatedValue := '';
    Result := False;
  end;
end;

initialization
  TrxRepositoryPath := '';
  SystemCodePage := GetACP;
{$IFDEF CBLIB_ITA}
  DefaultLanguage := mlItalian;
{$ELSE}
  DefaultLanguage := mlEnglish;
{$ENDIF}
  ChangeCurrentLanguage(DefaultLanguage);
  ChangeCurrentLanguageDB(DefaultLanguage);
  CDSTrxPropList := nil;
  MessageTranslator := TMessageTranslator.Create(nil);

finalization
  if Assigned(CDSTrxPropList) then
  begin
    CDSTrxPropList.SaveReformatXMLUTF8;
    CDSTrxPropList.Close;
    CDSTrxPropList.Free;
  end;

  DestroyRepositories;

  if Assigned(MessageTranslator) then
    MessageTranslator.Free;

end.
