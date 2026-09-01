{******************************************************************************}
{                                                                              }
{       Markdown Help Viewer: messages Unit                                    }
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
unit MDHelpView.Messages;

interface
  function STR_INFORMATION: string;
  function STR_ERROR: string;
  function STR_CONFIRMATION: string;
  function STR_UNEXPECTED_ERROR: string;
  function HTML_OUTPUT_FOLDER: string;
  function FILE_SAVED: string;
  function NO_KEYWORD_MATCH: string;
  function CONFIRM_EXPORT_HTML: string;
  function ERR_MSG_RECEIVED: string;
  function CLOSE_APP_FOR_LANG: string;
  function NEW_VERSION_AVAILABLE: string;
  function NEW_VERSION_NOT_AVAILABLE: string;
  function STOP_DOWNLOAD_BTN: string;
  function CHECK_FOR_UPDATE_BTN: string;
  function DOWNLOAD_STOPPED: string;
  function DOWNLOADING_ERROR: string;
  function ERROR_CHECKING_NEW_VERSION: string;
  function CONFIRM_OPEN_LINK: string;

  procedure RegisterMessages;

implementation

uses
  CBMultiLanguage;

const
  UN = 'MDHelpViewMessages';

  function STR_INFORMATION: string;
  begin Result := GetMsgMultiLanguage(UN, 'STR_INFORMATION',
    'INFORMAZIONE',
    'INFORMATION'); end;

  function STR_ERROR: string;
  begin Result := GetMsgMultiLanguage(UN,'STR_ERROR',
    'ERRORE!',
    'ERROR!'); end;

  function STR_CONFIRMATION: string;
  begin Result := GetMsgMultiLanguage(UN,'STR_CONFIRMATION',
    'CONFERMA',
    'CONFIRMATION'); end;

  function STR_UNEXPECTED_ERROR: string;
  begin Result := GetMsgMultiLanguage(UN,'STR_UNEXPECTED_ERROR',
    'ERRORE INATTESO!',
    'UNEXPECTED ERROR!'); end;

  function HTML_OUTPUT_FOLDER: string;
  begin Result := GetMsgMultiLanguage(UN,'HTML_OUTPUT_FOLDER',
    'Cartella salvataggio HTML',
    'HTML output folder'); end;

  function FILE_SAVED: string;
  begin Result := GetMsgMultiLanguage(UN,'FILE_SAVED',
    'Il file "%s" è stato salvato correttamente. Vuoi aprirlo ora?',
    'File "%s" successfully saved. Do you want to open it now?'); end;

  function NO_KEYWORD_MATCH: string;
  begin Result := GetMsgMultiLanguage(UN,'NO_KEYWORD_MATCH',
    'La parola chiave "%s" non è stata trovata nella cartella di lavoro: "%s"',
    'Keyword "%s" not found in files into working folder: "%s"'); end;

  function CONFIRM_EXPORT_HTML: string;
  begin Result := GetMsgMultiLanguage(UN,'CONFIRM_EXPORT_HTML',
    'Confermi esportazione di un singolo file [Si] o di %d file [Si a tutto] in formato HTML?',
    'Confirm export of single file [Yes] or %d files [Yes to All] in HTML format?'); end;

  function ERR_MSG_RECEIVED: string;
  begin Result := GetMsgMultiLanguage(UN,'ERR_MSG_RECEIVED',
    'Ricevuto messaggio non corretto.',
    'Incorrect message received'); end;

  function CLOSE_APP_FOR_LANG: string;
  begin Result := GetMsgMultiLanguage(UN,'CLOSE_APP_FOR_LANG',
    'Riavvio l''applicazione per cambiare la lingua.',
    'Closing application to change GUI language'); end;

  function NEW_VERSION_AVAILABLE: string;
  begin Result := GetMsgMultiLanguage(UN,'NEW_VERSION_AVAILABLE',
    'Una nuova versione è disponibile:'+sLineBreak+
    'Versione corrente: %s - Versione disponibile: %s'+sLineBreak+
    'Vuoi scaricare ed installare la nuova versione?',
    'A new version is available:'+sLineBreak+
    'Current version: %s - Available version: %s'+sLineBreak+
    'Do you want to dowload and install the new version?'); end;

  function NEW_VERSION_NOT_AVAILABLE: string;
  begin Result := GetMsgMultiLanguage(UN,'NEW_VERSION_NOT_AVAILABLE',
    'Nessun aggiornamento disponibile.'+sLineBreak+
    'Stai già utilizzando l''ultima versione: %s',
    'No updates available.'+sLineBreak+
    'You have already the latest version: %s'); end;

  function STOP_DOWNLOAD_BTN: string;
  begin Result := GetMsgMultiLanguage(UN,'STOP_DOWNLOAD_BTN',
    'Arresta scaricamento!',
    'Stop download!'); end;

  function CHECK_FOR_UPDATE_BTN: string;
  begin Result := GetMsgMultiLanguage(UN,'CHECK_FOR_UPDATE_BTN',
    'Cerca aggiornamenti',
    'Check for updates'); end;

  function DOWNLOAD_STOPPED: string;
  begin Result := GetMsgMultiLanguage(UN,'DOWNLOAD_STOPPED',
    'Scaricamento dell''installer arrestato!',
    'Download of installer stopped!'); end;

  function DOWNLOADING_ERROR: string;
  begin Result := GetMsgMultiLanguage(UN,'DOWNLOADING_ERROR',
    'Errore in scaricamento!',
    'Error downloading!'); end;

  function ERROR_CHECKING_NEW_VERSION: string;
  begin Result := GetMsgMultiLanguage(UN,'ERROR_CHECKING_NEW_VERSION',
    'Errore durante il controllo se è disponibile un nuovo aggiornamento per l''applicazione: %s',
    'Error checking if a new update for the application is available: %s'); end;

  function CONFIRM_OPEN_LINK: string;
  begin Result := GetMsgMultiLanguage(UN,'CONFIRM_OPEN_LINK',
    'Il documento contiene un collegamento che non è una pagina web:'+sLineBreak+
    '%s'+sLineBreak+
    'Vuoi aprirlo con l''applicazione associata?',
    'The document contains a link which is not a web page:'+sLineBreak+
    '%s'+sLineBreak+
    'Do you want to open it with the associated application?'); end;

  procedure RegisterMessages;
  begin
    StartMessagesRegistration(UN);
    Try
      STR_INFORMATION;
      STR_ERROR;
      STR_CONFIRMATION;
      STR_UNEXPECTED_ERROR;
      HTML_OUTPUT_FOLDER;
      FILE_SAVED;
      NO_KEYWORD_MATCH;
      CONFIRM_EXPORT_HTML;
      ERR_MSG_RECEIVED;
      CLOSE_APP_FOR_LANG;
      NEW_VERSION_AVAILABLE;
      NEW_VERSION_NOT_AVAILABLE;
      STOP_DOWNLOAD_BTN;
      CHECK_FOR_UPDATE_BTN;
      DOWNLOAD_STOPPED;
      DOWNLOADING_ERROR;
      ERROR_CHECKING_NEW_VERSION;
      CONFIRM_OPEN_LINK;
    Finally
      EndMessagesRegistration(UN);
    End;
  end;

end.

