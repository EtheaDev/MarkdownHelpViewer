{******************************************************************************}
{                                                                              }
{       CBMultiLanguage                                                        }
{       (Localization engine)                                                  }
{                                                                              }
{       Copyright (c) 2005-2025 (Ethea S.r.l.)                                 }
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
unit CBMultiLanguageMessages;

interface

  function CLASSNAME_DISPLABEL: string;
  function TRX_DISPLABEL      : string;
  function ORIG_DISPLABEL     : string;
  function COUNT_DISPLABEL    : string;
  function TYPE_DISPLABEL     : string;
  function COMPNAME_DISPLABEL : string;
  function PROPNAME_DISPLABEL : string;
  function STATUS_DISPLABEL   : string;

  function STR_ITA : string;
  function STR_ENG : string;
  function STR_DEU : string;
  function STR_ESP : string;
  function STR_FRA : string;
  function STR_PTG : string;
  function STR_RUS : string;

  function STR_TOTAL         : string;
  function STR_PERC_TRX      : string;
  function STR_DA_TRADURRE   : string;
  function STR_DA_VERIFICARE : string;
  function STR_TRADOTTO      : string;
  function STR_AUTOTRADOTTO  : string;
  function STR_DA_IGNORARE   : string;

  function ERR_ROOT_DIR_SELECTION : string;
  function ERR_REPOS_DIR : string;

  function ROOT_DIR_TITLE     : string;
  function ERR_ROWS_EDITING   : string;
  function CONFERMA_SALVA     : string;
  function CONFERMA_IMPORT    : string;
  function CONFERMA_EXPORT    : string;
  function CONFERMA_REPOSBUILD: string;
  function CONFERMA_IMPORT_ALL: string;
  function CONFERMA_IGNORE_ALL: string;
  function CONFERMA_UNTRAN_ALL: string;
  function REPOSBUILD_OK      : string;
  function IMPORTALL_OK       : string;
  function STR_ALL_FILES      : string;
  function STR_REPOSITORY     : string;
  function STR_FORMS          : string;
  function STR_MESSAGES       : string;
  function STR_FORM           : string;
  function STR_DATAMODULE     : string;
  function DIALOG_XML_FILTER  : string;
  function DIALOG_EXCEL_FILTER: string;
  function DIALOG_TXT_FILTER  : string;
  function SEARCH_INALL_FILES : string;
  function DELETE_FILE        : string;
  function CANNOT_DELETE_FILE : string;
  function END_EXPORT_REPOSITORY : string;
  function END_IMPORT_REPOSITORY : string;

  function KEYBOARD_ITALIANA : string;
  function KEYBOARD_INGLESE  : string;
  function KEYBOARD_TEDESCA  : string;
  function KEYBOARD_SPAGNOLA : string;
  function KEYBOARD_FRANCESE : string;
  function KEYBOARD_PORTOGHESE : string;
  function KEYBOARD_RUSSA    : string;
  function KEYBOARD_AMERICANA: string;
  function RIAVVIA_APP : string;

  function AUTOTRX_CURRENT_RECORD: string;
  function AUTOTRX_ALL_RECORDS: string;
  function AUTO_TRANSLATION: string;

  procedure RegisterMessages;

implementation

uses
  CBMultiLanguage;

const
  UN = 'CBMultiLanguageMessages';

  function CLASSNAME_DISPLABEL: string; begin Result := GetMsgMultiLanguage(UN,'CLASSNAME_DISPLABEL',
    'Nome classe', 'Class Name'); end;
  function TRX_DISPLABEL      : string; begin Result := GetMsgMultiLanguage(UN,'TRX_DISPLABEL',
    'Testo tradotto', 'Translated text'); end;
  function ORIG_DISPLABEL     : string; begin Result := GetMsgMultiLanguage(UN,'ORIG_DISPLABEL',
    'Testo originale', 'Original text'); end;
  function COUNT_DISPLABEL    : string; begin Result := GetMsgMultiLanguage(UN,'COUNT_DISPLABEL',
    'Occorrenze', 'Occurrences '); end;
  function TYPE_DISPLABEL     : string; begin Result := GetMsgMultiLanguage(UN,'TYPE_DISPLABEL',
    'Tipo','Type'); end;
  function COMPNAME_DISPLABEL : string; begin Result := GetMsgMultiLanguage(UN,'COMPNAME_DISPLABEL',
    'Nome componente','Component name'); end;
  function PROPNAME_DISPLABEL : string; begin Result := GetMsgMultiLanguage(UN,'PROPNAME_DISPLABEL',
    'Nome proprieta''','Property name'); end;
  function STATUS_DISPLABEL   : string; begin Result := GetMsgMultiLanguage(UN,'STATUS_DISPLABEL',
    'Status','Status'); end;

  function STR_ITA : string; begin Result := GetMsgMultiLanguage(UN,'STR_ITA',
    'Italiano','Italian'); end;
  function STR_ENG : string; begin Result := GetMsgMultiLanguage(UN,'STR_ENG',
    'Inglese','English'); end;
  function STR_DEU : string; begin Result := GetMsgMultiLanguage(UN,'STR_DEU',
    'Tedesco','German'); end;
  function STR_ESP : string; begin Result := GetMsgMultiLanguage(UN,'STR_ESP',
    'Spagnolo','Spanish'); end;
  function STR_FRA : string; begin Result := GetMsgMultiLanguage(UN,'STR_FRA',
    'Francese','French'); end;
  function STR_PTG : string; begin Result := GetMsgMultiLanguage(UN,'STR_PTG',
    'Portoghese','Portuguese'); end;
  function STR_RUS : string; begin Result := GetMsgMultiLanguage(UN,'STR_RUS',
    'Russo','Russian'); end;

  function STR_TOTAL         : string; begin Result := GetMsgMultiLanguage(UN,'STR_TOTAL',
    'Totale','Total'); end;
  function STR_PERC_TRX      : string; begin Result := GetMsgMultiLanguage(UN,'STR_PERC_TRX',
    'Traduzione %','% translated'); end;
  function STR_DA_TRADURRE   : string; begin Result := GetMsgMultiLanguage(UN,'STR_DA_TRADURRE',
    'Da tradurre','To translate'); end;
  function STR_DA_VERIFICARE : string; begin Result := GetMsgMultiLanguage(UN,'STR_DA_VERIFICARE',
    'Da verificare','To verify'); end;
  function STR_TRADOTTO      : string; begin Result := GetMsgMultiLanguage(UN,'STR_TRADOTTO',
    'Tradotto','Translated'); end;
  function STR_AUTOTRADOTTO  : string; begin Result := GetMsgMultiLanguage(UN,'STR_AUTOTRADOTTO',
    'Auto tradotto','Auto translated'); end;
  function STR_DA_IGNORARE   : string; begin Result := GetMsgMultiLanguage(UN,'STR_DA_IGNORARE',
    'Da ignorare','To ignore'); end;

  function ERR_ROOT_DIR_SELECTION : string; begin Result := GetMsgMultiLanguage(UN, 'ERR_ROOT_DIR_SELECTION',
    'Errore! Directory "%s" non valida. Manca il file "%s"',
    'Error! Wrong folder "%s". File "%s" missing'); end;

  function ERR_REPOS_DIR : string; begin Result := GetMsgMultiLanguage(UN, 'ERR_REPOS_DIR',
    'Errore! Directory repository "%s" non valida.',
    'Error! Folder repository "%s" not valid'); end;

  function ROOT_DIR_TITLE  : string; begin Result := GetMsgMultiLanguage(UN, 'ROOT_DIR_TITLE'  ,
    'Selezionare la directory radice',
    'Select root directory'); end;
  function ERR_ROWS_EDITING: string; begin Result := GetMsgMultiLanguage(UN, 'ERR_ROWS_EDITING',
    'Errore: numero di righe non equivalente',
    'Error: rows number not equal'); end;
  function CONFERMA_SALVA  : string; begin Result := GetMsgMultiLanguage(UN, 'CONFERMA_SALVA'  ,
    'Attenzione: alcuni dati non sono stati salvati. Volete salvarli?',
    'Warning: some data have been changed. Do you want to save them?'); end;
  function CONFERMA_IMPORT : string; begin Result := GetMsgMultiLanguage(UN, 'CONFERMA_IMPORT' ,
    'Confermi importazione traduzioni dal repository?',
    'Confirm import of translations from repository?'); end;
  function CONFERMA_EXPORT : string; begin Result := GetMsgMultiLanguage(UN, 'CONFERMA_EXPORT' ,
    'Confermi esportazione traduzioni nel repository?',
    'Confirm translation export into repository?'); end;
  function CONFERMA_REPOSBUILD : string; begin Result := GetMsgMultiLanguage(UN, 'CONFERMA_REPOSBUILD'   ,
    'Confermi generazione/aggiornamento repository "%s" per la traduzione?',
    'Confirm build/update of repository "%s" for translation?'); end;
  function CONFERMA_IMPORT_ALL : string; begin Result := GetMsgMultiLanguage(UN, 'CONFERMA_IMPORT_ALL'   ,
    'Confermi aggiornamento di tutti i files tramite il repository "%s"?', 'Do you confirm update of all file through the repository?'); end;
  function CONFERMA_IGNORE_ALL : string; begin Result := GetMsgMultiLanguage(UN, 'CONFERMA_IGNORE_ALL'   ,
    'Confermi impostazione status "da ignorare" su tutte le diciture da tradurre?', 'Do you confirm setting status "to ignore" on all the phrases to translate?'); end;
  function CONFERMA_UNTRAN_ALL : string; begin Result := GetMsgMultiLanguage(UN, 'CONFERMA_UNTRAN_ALL'   ,
    'Confermi impostazione status "da tradurre" su tutte le diciture da ignorare?', 'Do you confirm setting status "to translate" on all the phrases to ignore?'); end;
  function REPOSBUILD_OK : string; begin Result := GetMsgMultiLanguage(UN, 'REPOSBUILD_OK'     ,
    'Generazione/aggiornamento repository "%s" completata con successo!', 'Creation/update repository "%s" completed with happening!'); end;
  function IMPORTALL_OK : string; begin Result := GetMsgMultiLanguage(UN, 'IMPORTALL_OK'       ,
    'Aggiornamento di tutti i files dal repository "%s" completato con successo!', 'Update of all files of the repository "%s" completed with happening!'); end;
  function STR_ALL_FILES   : string; begin Result := GetMsgMultiLanguage(UN, 'STR_ALL_FILES'   ,
    'Tutti i files', 'All files'); end;
  function STR_REPOSITORY  : string; begin Result := GetMsgMultiLanguage(UN, 'STR_REPOSITORY'  ,
    'Repository %s. (%s)','Repository %s. (%s)'); end;
  function STR_FORMS       : string; begin Result := GetMsgMultiLanguage(UN, 'STR_FORMS'       ,
    'Finestre', 'Windows'); end;
  function STR_MESSAGES    : string; begin Result := GetMsgMultiLanguage(UN, 'STR_MESSAGES'    ,
    'Messaggi', 'Messages'); end;
  function STR_FORM        : string; begin Result := GetMsgMultiLanguage(UN, 'STR_FORM'        ,
    'Finestra', 'Window'); end;
  function STR_DATAMODULE  : string; begin Result := GetMsgMultiLanguage(UN, 'STR_DATAMODULE'  ,
    'Modulo dati','Datamodule'); end;
  function DIALOG_XML_FILTER   : string; begin Result := GetMsgMultiLanguage(UN, 'DIALOG_XML_FILTER'   ,
    'File di repository (*.xml)|*.xml', 'Repositoriy files (*.xml)|*.xml'); end;
  function DIALOG_EXCEL_FILTER   : string; begin Result := GetMsgMultiLanguage(UN, 'DIALOG_EXCEL_FILTER'   ,
    'File Excel (*.xls)|*.xls','Excel files (*,xls)|*.xls'); end;
  function DIALOG_TXT_FILTER   : string; begin Result := GetMsgMultiLanguage(UN, 'DIALOG_TXT_FILTER'   ,
    'File di testo (*.txt)|*.txt','Text files (*.txt)|*.txt'); end;
  function SEARCH_INALL_FILES  : string; begin Result := GetMsgMultiLanguage(UN, 'SEARCH_INALL_FILES'  ,
    'Ricerca terminata: proseguire la ricerca nei files successivi?', 'Finished search: do you continue the search in the next files? '); end;
  function DELETE_FILE  : string; begin Result := GetMsgMultiLanguage(UN, 'DELETE_FILE'  ,
    'Vuoi cancellare il file "%s"?', 'Do you want delete the file "%s"?'); end;
  function CANNOT_DELETE_FILE  : string; begin Result := GetMsgMultiLanguage(UN, 'CANNOT_DELETE_FILE'  ,
    'Impossibile cancellare il file "%s"', 'Cannot delete file "%s"'); end;
  function END_EXPORT_REPOSITORY  : string; begin Result := GetMsgMultiLanguage(UN, 'END_EXPORT_REPOSITORY'  ,
    'Il repository corrente e'' stato esportato nel file "%s"', 'The current repository has been exported in the file "%s"'); end;
  function END_IMPORT_REPOSITORY  : string; begin Result := GetMsgMultiLanguage(UN, 'END_IMPORT_REPOSITORY'  ,
    'Fine importazione delle traduzioni dal file "%s"', 'End import of the translations from the file "%s"'); end;

  function KEYBOARD_ITALIANA : string;
  begin
    Result := GetMsgMultiLanguage( UN, 'KEYBOARD_ITALIANA', 'Tastiera italiana', 'Italian keyboard');
  end;

  function KEYBOARD_INGLESE : string;
  begin
    Result := GetMsgMultiLanguage( UN, 'KEYBOARD_INGLESE', 'Tastiera inglese (Regno Unito)', 'English keyboard (United Kingdom)');
  end;

  function KEYBOARD_TEDESCA : string;
  begin
    Result := GetMsgMultiLanguage( UN, 'KEYBOARD_TEDESCA', 'Tastiera tedesca', 'German keyboard');
  end;

  function KEYBOARD_SPAGNOLA : string;
  begin
    Result := GetMsgMultiLanguage( UN, 'KEYBOARD_SPAGNOLA', 'Tastiera spagnola', 'Spanish keyboard');
  end;

  function KEYBOARD_FRANCESE : string;
  begin
    Result := GetMsgMultiLanguage( UN, 'KEYBOARD_FRANCESE', 'Tastiera francese', 'French keyboard');
  end;

  function KEYBOARD_PORTOGHESE : string;
  begin
    Result := GetMsgMultiLanguage( UN, 'KEYBOARD_PORTOGHESE', 'Tastiera portoghese', 'Portuguese keyboard');
  end;

  function KEYBOARD_RUSSA : string;
  begin
    Result := GetMsgMultiLanguage( UN, 'KEYBOARD_RUSSA', 'Tastiera russa', 'Russian keyboard');
  end;

  function KEYBOARD_AMERICANA : string;
  begin
    Result := GetMsgMultiLanguage( UN, 'KEYBOARD_AMERICANA', 'Tastiera inglese (Stati Uniti d''America)', 'English keyboard (USA)');
  end;

  function RIAVVIA_APP : string;
  begin
    Result := GetMsgMultiLanguage( UN,'RIAVVIA_APP','Riavviare l''applicazione per ottenere la nuova lingua "%s"',
      'Restart application to activate the new language "%s"');
  end;

  function AUTOTRX_CURRENT_RECORD: string;
  begin
    Result := GetMsgMultiLanguage( UN,'AUTOTRX_CURRENT_RECORD','Traduci automaticamente il record corrente',
      'Translate automatically current record');
  end;

  function AUTOTRX_ALL_RECORDS: string;
  begin
    Result := GetMsgMultiLanguage( UN,'AUTOTRX_ALL_RECORDS','Traduci automaticamente %d record',
      'Translate automatically %d records');
  end;

  function AUTO_TRANSLATION: string;
  begin
    Result := GetMsgMultiLanguage( UN,'AUTO_TRANSLATION','Traduzione automatica campi multilingua',
      'Auto translation multilanguage fields');
  end;

  procedure RegisterMessages;
  begin
    StartMessagesRegistration(UN);
    Try
      CLASSNAME_DISPLABEL;    
      TRX_DISPLABEL;
      ORIG_DISPLABEL;
      COUNT_DISPLABEL;
      TYPE_DISPLABEL;
      COMPNAME_DISPLABEL;
      PROPNAME_DISPLABEL;
      STATUS_DISPLABEL;

      STR_ITA;
      STR_ENG;
      STR_DEU;
      STR_ESP;
      STR_FRA;
      STR_PTG;
      STR_RUS;

      STR_TOTAL;
      STR_PERC_TRX;
      STR_DA_TRADURRE;
      STR_DA_VERIFICARE;
      STR_TRADOTTO;
      STR_AUTOTRADOTTO;
      STR_DA_IGNORARE;

      ERR_ROOT_DIR_SELECTION;
      ERR_REPOS_DIR;

      ROOT_DIR_TITLE;
      ERR_ROWS_EDITING;
      CONFERMA_SALVA;
      CONFERMA_IMPORT;
      CONFERMA_EXPORT;
      CONFERMA_REPOSBUILD;
      CONFERMA_IMPORT_ALL;
      CONFERMA_IGNORE_ALL;
      CONFERMA_UNTRAN_ALL;
      REPOSBUILD_OK;
      IMPORTALL_OK;
      STR_ALL_FILES;
      STR_REPOSITORY;
      STR_FORMS;
      STR_MESSAGES;
      STR_FORM;
      STR_DATAMODULE;
      DIALOG_XML_FILTER;
      DIALOG_EXCEL_FILTER;
      DIALOG_TXT_FILTER;
      SEARCH_INALL_FILES;
      DELETE_FILE;
      CANNOT_DELETE_FILE;
      END_EXPORT_REPOSITORY;
      END_IMPORT_REPOSITORY;

      KEYBOARD_ITALIANA;
      KEYBOARD_INGLESE;
      KEYBOARD_RUSSA;
      KEYBOARD_AMERICANA;

      RIAVVIA_APP;

      AUTOTRX_CURRENT_RECORD;
      AUTOTRX_ALL_RECORDS;
      AUTO_TRANSLATION;

    Finally
      EndMessagesRegistration(UN);
    End;
  end;
end.
