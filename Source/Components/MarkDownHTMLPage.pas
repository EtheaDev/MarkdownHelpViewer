{******************************************************************************}
{                                                                              }
{       Viewer Components to show Markdown and HTML content                    }
{       TMarkdownHTMLPage: the HTML page that wraps the HTML of a document     }
{                                                                              }
{       Copyright (c) 2023-2026 (Ethea S.r.l.)                                 }
{       Author: Carlo Barazzetta                                               }
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
unit MarkDownHTMLPage;

{ No dependency on WebView2: the page is only text. TCustomEdgeMarkdownViewer
  (MarkDownEdgeViewerComponents) builds the page it shows and the page it
  exports (ExportToFileHTMLPage) with these functions, so a tool can write the
  very same file as the viewer. }

interface

uses
  System.SysUtils
  , Winapi.Windows
  , Vcl.Graphics
  ;

const
  //Scripts from CDN, when there is no local scripts folder (the same versions
  //of the Scripts folder distributed with the viewers)
  KaTeXCDN = 'https://cdn.jsdelivr.net/npm/katex@0.16.11/dist/';
  MermaidCDN = 'https://cdn.jsdelivr.net/npm/mermaid@11.17.2/dist/';

type
  TMarkdownHTMLPage = class
  public
    /// <summary>A color as #rrggbb.</summary>
    class function ColorToHTML(const AColor: TColor): string; static;
    /// <summary>True when the background is dark (it decides color-scheme
    /// and the colors of the code blocks).</summary>
    class function IsDarkColor(const AColor: TColor): Boolean; static;
    /// <summary>The base style: font, colors, color-scheme, print.</summary>
    class function PageStyle(const AFontName: string; const AFontSize: Integer;
      const AFontColor, ABackground, AHotSpotColor: TColor): string; static;
    /// <summary>The scripts of the page: KaTeX and mermaid when the HTML has
    /// formulas or diagrams (from AKaTeXURL and AMermaidURL), the links to an
    /// anchor of the page and the report of the scroll position.</summary>
    class function PageScripts(const AHTML: string; const AScrollY: Integer;
      const AKaTeXURL, AMermaidURL: string): string; static;
    /// <summary>The whole page: AHTML is the HTML of the document, ABaseURL
    /// (can be empty) the base of the relative links, ATitle the title (the
    /// name proposed by "Print").</summary>
    class function Build(const AHTML, ATitle, ABaseURL, AStyle, AScripts: string): string; static;
    /// <summary>The title of the page from the name of the document file
    /// ('Markdown' when it is empty), escaped for HTML.</summary>
    class function TitleOf(const ADocumentFileName: string): string; static;
  end;

implementation

class function TMarkdownHTMLPage.ColorToHTML(const AColor: TColor): string;
var
  LRGB: Cardinal;
begin
  LRGB := Cardinal(ColorToRGB(AColor));
  Result := Format('#%.2x%.2x%.2x', [GetRValue(LRGB), GetGValue(LRGB), GetBValue(LRGB)]);
end;

class function TMarkdownHTMLPage.IsDarkColor(const AColor: TColor): Boolean;
var
  LColor: TColor;
begin
  LColor := ColorToRGB(AColor);
  Result := (GetRValue(LColor) * 299 + GetGValue(LColor) * 587 +
    GetBValue(LColor) * 114) div 1000 < 128;
end;

class function TMarkdownHTMLPage.PageStyle(const AFontName: string;
  const AFontSize: Integer; const AFontColor, ABackground,
  AHotSpotColor: TColor): string;
const
  LScheme: array[Boolean] of string = ('light', 'dark');
begin
  //The base style: the stylesheet of HtmlContent (CssStyle) comes after it.
  //color-scheme: scrollbars and form controls as the background of the page
  //(else they follow the theme of Windows: a dark scrollbar on a light page)
  Result :=
    '<style type="text/css">' + sLineBreak +
    Format(':root{color-scheme:%s;}', [LScheme[IsDarkColor(ABackground)]]) + sLineBreak +
    Format('body{font-family:"%s",sans-serif;font-size:%dpt;color:%s;background-color:%s;}',
      [AFontName, AFontSize, ColorToHTML(AFontColor), ColorToHTML(ABackground)]) + sLineBreak +
    Format('a{color:%s;}', [ColorToHTML(AHotSpotColor)]) + sLineBreak +
    //the styles of alerts, mermaid, math... are in CssStyle (MarkdownBaseCSS):
    //here only the refinements HTMLViewer would not understand
    '.markdown-alert>:last-child{margin-bottom:0;}' + sLineBreak +
    '.markdown-alert-title{margin-top:0;}' + sLineBreak +
    //printed (and PDF) pages: dark text on white paper, also with a dark theme
    '@media print{body{color:#000;background-color:#fff;}}' + sLineBreak +
    '</style>' + sLineBreak;
end;

class function TMarkdownHTMLPage.PageScripts(const AHTML: string;
  const AScrollY: Integer; const AKaTeXURL, AMermaidURL: string): string;
begin
  Result := '';
  //KaTeX typesets \( \) and \[ \] (mmrMarkup): only when there are formulas
  if Pos('class="math"', AHTML) > 0 then
    Result := Result +
      '<link rel="stylesheet" href="' + AKaTeXURL + 'katex.min.css">' + sLineBreak +
      '<script defer src="' + AKaTeXURL + 'katex.min.js"></script>' + sLineBreak +
      '<script defer src="' + AKaTeXURL + 'contrib/auto-render.min.js"' +
      ' onload="renderMathInElement(document.body);"></script>' + sLineBreak;
  //mermaid.js draws the <pre class="mermaid"> diagrams
  if Pos('class="mermaid"', AHTML) > 0 then
    Result := Result +
      '<script src="' + AMermaidURL + 'mermaid.min.js"></script>' + sLineBreak +
      '<script>document.addEventListener("DOMContentLoaded",function(){' +
      'mermaid.initialize({startOnLoad:false});mermaid.run();});</script>' + sLineBreak;
  //Links to an anchor of the page (the <base> would navigate away), position
  //of the page (to keep it when the content is shown again)
  Result := Result +
    '<script>' + sLineBreak +
    'document.addEventListener("click",function(e){var a=e.target.closest("a");' +
    'if(a){var h=a.getAttribute("href");if(h&&h.charAt(0)==="#"){e.preventDefault();' +
    'var t=document.getElementById(decodeURIComponent(h.substring(1)));' +
    'if(t)t.scrollIntoView();}}});' + sLineBreak +
    'window.addEventListener("scroll",function(){if(window.chrome&&chrome.webview)' +
    'chrome.webview.postMessage("scrollY:"+Math.round(window.scrollY)+":"+' +
    'Math.max(0,Math.round(document.documentElement.scrollHeight-window.innerHeight)));});' + sLineBreak;
  if AScrollY > 0 then
    Result := Result +
      Format('window.addEventListener("load",function(){window.scrollTo(0,%d);});', [AScrollY]) + sLineBreak;
  Result := Result + '</script>' + sLineBreak;
end;

class function TMarkdownHTMLPage.TitleOf(const ADocumentFileName: string): string;
begin
  //the title: the name proposed by "Print" (a PDF printer) instead of the
  //data: URL of the page
  if ADocumentFileName <> '' then
    Result := ChangeFileExt(ExtractFileName(ADocumentFileName), '')
  else
    Result := 'Markdown';
  Result := StringReplace(StringReplace(StringReplace(Result, '&', '&amp;',
    [rfReplaceAll]), '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]);
end;

class function TMarkdownHTMLPage.Build(const AHTML, ATitle, ABaseURL, AStyle,
  AScripts: string): string;
var
  LBase: string;
begin
  LBase := '';
  if ABaseURL <> '' then
    LBase := '<base href="' + ABaseURL + '">' + sLineBreak;
  Result :=
    '<!DOCTYPE html>' + sLineBreak +
    '<html>' + sLineBreak +
    '<head>' + sLineBreak +
    '<meta charset="utf-8">' + sLineBreak +
    '<title>' + ATitle + '</title>' + sLineBreak +
    LBase +
    AStyle +
    AScripts +
    '</head>' + sLineBreak +
    '<body>' + sLineBreak +
    AHTML +
    '</body>' + sLineBreak +
    '</html>' + sLineBreak;
end;

end.
