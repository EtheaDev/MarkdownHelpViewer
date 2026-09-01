{******************************************************************************}
{                                                                              }
{       Markdown Help Viewer: Image Resources Unit                             }
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
unit MDHelpView.Resources;

interface

uses
  System.SysUtils
  , WinApi.Windows
  , System.Classes
  , Vcl.Graphics
  , MDHelpView.Settings
  , HtmlGlobals
  , HtmlView
  , SVGIconImageCollection
  ;

type
  TdmResources = class(TDataModule)
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    FLoadingImages: Boolean;
    FStream: TMemoryStream;
    FStopImageRequest: Boolean;
    function ConvertImage(AFileName: string;
      const AMaxWidth: Integer; const ABackgroundColor: TColor): Boolean;
    function getStreamData(const AFileName : String;
      const AMaxWidth: Integer; const ABackgroundColor: TColor): TStream;
    function OpenURL(const AUrl: string): Boolean;
  public
    Settings: TViewerSettings;
    procedure TryExpandSpaces(const ARootFolder: string;
      var AFileName: TFileName);
    procedure StopLoadingImages(const AStop: Boolean);
    procedure HtmlViewerImageRequest(Sender: TObject; const ASource: UnicodeString;
      var AStream: TStream);
    procedure HtmlViewerHotSpotClick(Sender: TObject; const ASource: ThtString;
      var Handled: Boolean);
    function LoadFileContent(const AFileName: TFileName;
      const ARootFolder: string; const AMaxWidth: Integer;
      const ABackGroundColor: TColor; out AStream: TStream): Boolean;
    function IsLoadingImages: Boolean;
  end;

var
  dmResources: TdmResources;

implementation

{$R *.dfm}

uses
  System.StrUtils
  , Vcl.Themes
  , Winapi.GDIPOBJ
  , Winapi.GDIPAPI
  , System.IOUtils
  , System.UITypes
  , Winapi.ShellAPI
  , SynPDF
  , Winapi.Messages
  , Vcl.Forms
  , IdHTTP
  , IdSSLOpenSSL
  , SVGIconImage
  , pngimage
  , JPeg
  , GIFImg
  , SVGInterfaces
  , SVGIconUtils
  , Vcl.Skia
  ;

procedure TdmResources.DataModuleCreate(Sender: TObject);
begin
  FStream := TMemoryStream.Create;
end;

procedure TdmResources.DataModuleDestroy(Sender: TObject);
begin
  FreeAndNil(FStream);
  inherited;
end;


procedure TdmResources.HtmlViewerHotSpotClick(Sender: TObject;
  const ASource: ThtString; var Handled: Boolean);
begin
  Handled := OpenUrl(ASource);
end;

procedure TdmResources.TryExpandSpaces(const ARootFolder: string; var AFileName: TFileName);
var
  LOriginalFileName: TFileName;
begin
  LOriginalFileName := AFileName;
  // if "AFileName" is not a local file (eg. is file from internet)
  // replace %20 spaces to normal spaces
  AFileName := StringReplace(AFileName,'%20',' ',[rfReplaceAll]);
  If not FileExists(AFileName) then
  begin
    //If not exists, try to include ARootFolder into FileName
    AFileName := IncludeTrailingPathDelimiter(ARootFolder)+AFileName;
    //Restore original file name because is not a local file
    If not FileExists(AFileName) then
      AFileName := LOriginalFileName;
  end;
end;

procedure TdmResources.HtmlViewerImageRequest(Sender: TObject;
  const ASource: UnicodeString; var AStream: TStream);
var
  LHtmlViewer: THtmlViewer;
  LFullName: TFileName;
  LMaxWidth: Integer;
begin
  if FStopImageRequest then
    Exit;
  FLoadingImages := True;
  Try
    Application.ProcessMessages;
    LHtmlViewer := sender as THtmlViewer;
    LMaxWidth := LHtmlViewer.ClientWidth - LHtmlViewer.VScrollBar.Width - (LHtmlViewer.MarginWidth * 2);

    // HTMLViewer needs to be nil'ed
    AStream := nil;

    LFullName := ASource;
    TryExpandSpaces(LHtmlViewer.ServerRoot, LFullName);
    LFullName := LHtmlViewer.HTMLExpandFilename(LFullName);
  
    LoadFileContent(LFullName, LHtmlViewer.ServerRoot, LMaxWidth,
      LHtmlViewer.DefBackground, AStream);
  Finally
    FLoadingImages := False;
  End;
end;

function TdmResources.LoadFileContent(const AFileName: TFileName;
  const ARootFolder: string; const AMaxWidth: Integer;
  const ABackGroundColor: TColor;
  out AStream: TStream): Boolean;
var
  LDownLoadFromWeb: boolean;
Begin
  Result := True;
  try
    if FileExists(AFileName) then  // if local file, load it..
    Begin
      FStream.LoadFromFile(AFileName);
      //Convert image to stretch size of HTMLViewer
      Result := ConvertImage(AFileName, AMaxWidth, ABackGroundColor);
      if not Result then
        Exit;
      AStream := FStream;
    end
    else if SameText('http', Copy(AFileName,1,4)) then
    Begin
      LDownLoadFromWeb := Settings.DownloadFromWEB;
      if LDownLoadFromWeb then
      begin
        //Load from remote. NB: use the returned stream, which is nil when the
        //download or the decoding failed: assigning FStream unconditionally
        //handed the viewer the leftovers of the previous image.
        AStream := getStreamData(AFileName, AMaxWidth, ABackGroundColor);
        Result := AStream <> nil;
      end;
    End;
  except
    //No exception for EInvalidGraphic
    on E: Exception do
    begin
      Result := False;
      {$IFDEF DEBUG}
      OutputDebugString(PChar(Format('MDHelpViewer - content "%s": %s (%s)',
        [AFileName, E.Message, E.ClassName])));
      {$ENDIF}
    end;
  end;
End;

function TdmResources.OpenURL(const AUrl: string): Boolean;
begin
  ShellExecute(0, 'open', PChar(AURL), nil, nil, SW_SHOWNORMAL);
  Result := True;
end;

function TdmResources.IsLoadingImages: Boolean;
begin
  Result := FLoadingImages;
end;

procedure TdmResources.StopLoadingImages(const AStop: Boolean);
begin
  FStopImageRequest := AStop;
end;

function TdmResources.getStreamData(const AFileName : String;
  const AMaxWidth: Integer; const ABackgroundColor: TColor): TStream;
const
  //A server can answer with an HTML "moved" page instead of a redirect header:
  //the link is followed manually, but only a limited number of times.
  MAX_REDIRECT = 5;
  //Only a very small payload can be an error or "moved" page, not an image
  MAX_HTML_ANSWER_SIZE = 1024;

  //Reads the body as text only to inspect a "moved"/"not found" page. The
  //binary content of FStream is never rebuilt from this string: the previous
  //version round-tripped it through an ANSI TStringStream and wrote it to a
  //temporary file that nobody ever read (and nobody ever deleted).
  function IsHtmlAnswer(out AContent: string): Boolean;
  var
    LBytes: TBytes;
  begin
    Result := (FStream.Size > 0) and (FStream.Size < MAX_HTML_ANSWER_SIZE);
    AContent := '';
    if not Result then
      Exit;
    SetLength(LBytes, FStream.Size);
    FStream.Position := 0;
    FStream.ReadBuffer(LBytes[0], Length(LBytes));
    AContent := TEncoding.ANSI.GetString(LBytes);
    FStream.Position := 0;
  end;

  //Extracts the target of the first <a href="..."> of a "moved" page
  function TryGetMovedUrl(const AContent: string; out AUrl: string): Boolean;
  var
    LLowerContent: string;
    P: Integer;
  begin
    Result := False;
    AUrl := '';
    LLowerContent := LowerCase(AContent);
    if (Pos('301 moved permanently', LLowerContent) = 0) and
       (Pos('<html><body>', LLowerContent) = 0) then
      Exit;
    P := Pos('<a href="', LLowerContent);
    if P = 0 then
      Exit;
    //Searched on the lowercase copy, extracted from the original one
    AUrl := Copy(AContent, P + Length('<a href="'), MaxInt);
    P := Pos('"', AUrl);
    if P <= 1 then
    begin
      AUrl := '';
      Exit;
    end;
    AUrl := Copy(AUrl, 1, P - 1);
    Result := True;
  end;

  //File name of an URL, without query string and fragment: it is what selects
  //the decoder in ConvertImage
  function UrlToFileName(const AUrl: string): TFileName;
  var
    LName: string;
    P: Integer;
  begin
    LName := AUrl;
    P := Pos('?', LName);
    if P > 0 then
      LName := Copy(LName, 1, P - 1);
    P := Pos('#', LName);
    if P > 0 then
      LName := Copy(LName, 1, P - 1);
    Result := ExtractFileName(StringReplace(LName, '/', '\', [rfReplaceAll]));
  end;

var
  LIdHTTP   : TIdHTTP;
  LIdSSLIOHandler: TIdSSLIOHandlerSocketOpenSSL;
  LUrl, LMovedUrl, LContent: string;
  LRedirectCount: Integer;
  LDone: Boolean;
Begin
  //downloading Image from WEB
  Result := nil;
  LUrl := AFileName;
  LRedirectCount := 0;
  LIdHTTP := nil;
  LIdSSLIOHandler := nil;
  try
    LIdHTTP := TIdHTTP.Create;
    LIdHTTP.AllowCookies := True;
    LIdHTTP.HandleRedirects := True;
    LIdSSLIOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(LIdHTTP);
    LIdSSLIOHandler.DefaultPort := 0;
    LIdSSLIOHandler.SSLOptions.SSLVersions := [sslvTLSv1_2];
    LIdHTTP.IOHandler := LIdSSLIOHandler;

    LIdHTTP.Request.UserAgent :=
      'Mozilla/5.0 (Windows NT 6.1; WOW64; rv:12.0) Gecko/20100101 Firefox/12.0';

    repeat
      LDone := True;
      FStream.Clear;
      try
        LIdHTTP.Get(LUrl, FStream);
      except
        //A network failure simply means no image: it is not reported
        FStream.Clear;
        Exit;
      end;

      if FStream.Size = 0 then
        Exit;

      if IsHtmlAnswer(LContent) then
      begin
        if Pos('Not Found', LContent) > 0 then
          Exit;
        if TryGetMovedUrl(LContent, LMovedUrl) and
          (LRedirectCount < MAX_REDIRECT) then
        begin
          LUrl := LMovedUrl;
          Inc(LRedirectCount);
          LDone := False;
        end;
      end;
    until LDone;

    //The decoder is selected from the extension of the URL actually fetched.
    //No temporary file is involved: ConvertImage works on FStream.
    if ConvertImage(UrlToFileName(LUrl), AMaxWidth, ABackgroundColor) then
      Result := FStream;
  finally
    LIdSSLIOHandler.Free;
    LIdHttp.Free;
  end;
end;

function TdmResources.ConvertImage(AFileName: string;
  const AMaxWidth: Integer; const ABackgroundColor: TColor): Boolean;
var
  LPngImage: TPngImage;
  LBitmap: TBitmap;
  LImage, LScaledImage: TWICImage;
  LFileExt: string;
  LScaleFactor: double;
  LSVG: ISVG;

  function CalcScaleFactor(const AWidth: integer): double;
  begin
    if AWidth > AMaxWidth then
      Result := AMaxWidth / AWidth
    else
      Result := 1;
  end;

  procedure MakeTransparent(DC: THandle);
  var
    Graphics: TGPGraphics;
  begin
    Graphics := TGPGraphics.Create(DC);
    try
      Graphics.Clear(aclTransparent);
    finally
      Graphics.Free;
    end;
  end;

begin
  Result := True;
  LFileExt := ExtractFileExt(AFileName);
  try
    FStream.Position := 0;
    if SameText(LFileExt,'.svg') then
    begin
      LSVG := GlobalSVGFactory.NewSvg;
      LSVG.LoadFromStream(FStream);
      LScaleFactor := CalcScaleFactor(Round(Lsvg.Width));
      if (Settings.RescalingImage) and (LScaleFactor <> 1) then
      begin
        LBitmap := TBitmap.Create(
          Round(LSVG.Width * LScaleFactor),
          Round(LSVG.Height* LScaleFactor));
      end
      else
      begin
        LBitmap := TBitmap.Create(Round(LSVG.Width), Round(LSVG.Height));
      end;
      try
        LBitmap.PixelFormat := pf32bit;
        MakeTransparent(LBitmap.Canvas.Handle);
        LSVG.PaintTo(LBitmap.Canvas.Handle,
          TRect.Create(0, 0, LBitmap.Width, LBitmap.Height), True);
        FStream.Clear;
        LPngImage := PNG4TransparentBitMap(LBitmap);
        try
          LPngImage.SaveToStream(FStream);
        finally
          LPngImage.Free;
        end;
      finally
        LBitmap.free;
      end;
    end
    else if SameText(LFileExt,'.webp') or SameText(LFileExt,'.wbmp') then
    begin
      LImage := TWICImage.Create;
      try
        LImage.Transparent := True;
        LImage.LoadFromStream(FStream);
        LScaleFactor := CalcScaleFactor(LImage.Width);
        if (Settings.RescalingImage) and (LScaleFactor <> 1) then
        begin
          //Rescaling bitmap and save to stream
          LScaledImage := LImage.CreateScaledCopy(
            Round(LImage.Width*LScaleFactor),
            Round(LImage.Height*LScaleFactor),
            wipmHighQualityCubic);
          LBitmap := TBitmap.Create(LScaledImage.Width,LScaledImage.Height);
          MakeTransparent(LBitmap.Canvas.Handle);
          LBitmap.Canvas.Draw(0,0,LScaledImage);
        end
        else
        begin
          LBitmap := TBitmap.Create(LImage.Width,LImage.Height);
          MakeTransparent(LBitmap.Canvas.Handle);
          LBitmap.Canvas.Draw(0,0,LImage);
        end;
        try
          FStream.Clear;
          //if LBitmap.TransparentMode = tmAuto then
          //  LBitmap.SaveToStream(FStream)
          //else
          begin
            LPngImage := PNG4TransparentBitMap(LBitmap);
            try
              LPngImage.SaveToStream(FStream);
            finally
              LPngImage.Free;
            end;
          end;
        finally
          LBitmap.Free;
        end;
      finally
        LImage.Free;
      end;
    end
    else
    begin
      LImage := nil;
      LScaledImage := nil;
      try
        begin
          LImage := TWICImage.Create;
          LImage.LoadFromStream(FStream);
          LScaleFactor := CalcScaleFactor(LImage.Width);
          if (Settings.RescalingImage) and (LScaleFactor <> 1) then
          begin
            //Rescaling bitmap and save to stream
            LScaledImage :=  LImage.CreateScaledCopy(
              Round(LImage.Width*LScaleFactor),
              Round(LImage.Height*LScaleFactor),
              wipmHighQualityCubic);
            LBitmap := TBitmap.Create(LScaledImage.Width,LScaledImage.Height);
            try
              MakeTransparent(LBitmap.Canvas.Handle);
              LBitmap.Canvas.Draw(0,0,LScaledImage);
              FStream.Clear;
              if LBitmap.TransparentMode = tmAuto then
                LBitmap.SaveToStream(FStream)
              else
              begin
                LPngImage := PNG4TransparentBitMap(LBitmap);
                try
                  LPngImage.SaveToStream(FStream);
                finally
                  LPngImage.Free;
                end;
              end;
            finally
              LBitmap.Free;
            end;
          end
          else
            FStream.Position := 0;
        end;
      finally
        LImage.Free;
        LScaledImage.Free;
      end;
    end;
  except
    //An image that cannot be decoded must not break the rendering of the whole
    //document, so no error is raised. It is reported to the debug output,
    //otherwise the failure would be completely invisible.
    on E: Exception do
    begin
      Result := False;
      {$IFDEF DEBUG}
      OutputDebugString(PChar(Format('MDHelpViewer - image "%s": %s (%s)',
        [AFileName, E.Message, E.ClassName])));
      {$ENDIF}
    end;
  end;
end;

end.
