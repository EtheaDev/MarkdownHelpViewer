{******************************************************************************}
{                                                                              }
{       Markdown Help Viewer: Image Resources Unit                             }
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
    procedure ConvertImage(AFileName: string;
      const AMaxWidth: Integer; const ABackgroundColor: TColor);
    function getStreamData(const AFileName : String;
      const AMaxWidth: Integer; const ABackgroundColor: TColor): TStream;
  public
    ViewerSettings: TViewerSettings;
    procedure StopLoadingImages(const AStop: Boolean);
    procedure HtmlViewerImageRequest(Sender: TObject; const ASource: UnicodeString;
      var AStream: TStream);
    function IsLoadingImages: Boolean;
  end;

var
  dmResources: TdmResources;

implementation

{$R *.dfm}

uses
  System.StrUtils
  , System.Types
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

procedure TdmResources.HtmlViewerImageRequest(Sender: TObject;
  const ASource: UnicodeString; var AStream: TStream);
var
  LFullName: String;
  LHtmlViewer: THtmlViewer;
  LMaxWidth: Integer;
Begin
  if FStopImageRequest then
    Exit;
  FLoadingImages := True;
  Application.ProcessMessages;
  Try
    LHtmlViewer := sender as THtmlViewer;

    AStream := nil;

    // is "fullName" a local file, if not aquire file from internet
    // replace %20 spaces to normal spaces
    LFullName := StringReplace(ASource,'%20',' ',[rfReplaceAll]);
    If not FileExists(LFullName) then
    begin
      LFullName := IncludeTrailingPathDelimiter(LHtmlViewer.ServerRoot)+LFullName;
      If not FileExists(LFullName) then
        LFullName := ASource;
    end;

    LFullName := LHtmlViewer.HTMLExpandFilename(LFullName);

    LMaxWidth := LHtmlViewer.ClientWidth - LHtmlViewer.VScrollBar.Width - (LHtmlViewer.MarginWidth * 2);
    if FileExists(LFullName) then  // if local file, load it..
    Begin
      FStream.LoadFromFile(LFullName);
      //Convert image to stretch size of HTMLViewer
      ConvertImage(LFullName, LMaxWidth, LHtmlViewer.DefBackground);
      AStream := FStream;
    end
    else if SameText('http', Copy(ASource,1,4)) then
    Begin
      if ViewerSettings.DownloadFromWEB then
      begin
        //Load from remote
        getStreamData(LFullName, LMaxWidth, LHtmlViewer.DefBackground);
        AStream := FStream;
      end;
    End;
  Finally
    FLoadingImages := False;
  End;
End;

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
var
  sl        : TStringList;
  bFail     : Boolean;
  bTryAgain : Boolean;
  LIdHTTP   : TIdHTTP;
  LIdSSLIOHandler: TIdSSLIOHandlerSocketOpenSSL;
  LFileName: string;
Begin
  Result := nil;
  FStream.Clear;
  LIdHTTP := nil;
  sl := nil;
  LFileName := AFileName;
  LIdSSLIOHandler := nil;
  try
    LIdHTTP := TIdHTTP.Create;
    LIdHTTP.AllowCookies := True;
    LIdHTTP.HandleRedirects := True;
    sl := TStringList.Create;
    LIdSSLIOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(LIdHTTP);
    LIdSSLIOHandler.DefaultPort := 0;
    LIdSSLIOHandler.SSLOptions.SSLVersions := [sslvTLSv1_2];
    LIdHTTP.IOHandler := LIdSSLIOHandler;

    LIdHTTP.Request.UserAgent :=
      'Mozilla/5.0 (Windows NT 6.1; WOW64; rv:12.0) Gecko/20100101 Firefox/12.0';

    try
      LIdHTTP.Get(LFileName, FStream);
    except
      On Exception do
        FStream.Clear
      else
        raise;
    end;
    // Need To check For Failed Retrieval...
    FStream.Position:= 0;
    sl.LoadFromStream(FStream);
    bTryAgain := False;
    bFail     := False;
    if Length(sl.Text) = 0 then
      bFail:= True;
    if FStream.Size = 0 then
      bFail:= True;

    if FStream.Size < 1024 then
    Begin
      if Pos('Not Found', sl.Text) > 0 then bFail:= True;
      if (Pos(LowerCase('<title>301 Moved Permanently</title>'), LowerCase(sl.Text)) > 0) or
         (Pos(LowerCase('<html><body>'), LowerCase(sl.Text)) > 0) then
      Begin
        if Pos(LowerCase('<a href="'), LowerCase(sl.Text)) > 0 then
        Begin
          LFileName := Copy(sl.Text, Pos('<a href="', sl.Text) + 9, Length(sl.Text));
          LFileName := Copy(LFileName, 1, Pos('"', LFileName) -1);
          bTryAgain:= True;
        End;
      end;
    end;

    if bTryAgain then
      // Call Function Again...
      Result := getStreamData(LFileName, AMaxWidth, ABackgroundColor);

    if not bTryAgain And not bFail then
    begin
      ConvertImage(LFileName, AMaxWidth, ABackgroundColor);
      Result := FStream;
    end;
  finally
    LIdSSLIOHandler.Free;
    LIdHttp.Free;
    sl.Free;
  end;
end;

procedure TdmResources.ConvertImage(AFileName: string;
  const AMaxWidth: Integer; const ABackgroundColor: TColor);
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
  LFileExt := ExtractFileExt(AFileName);
  try
    FStream.Position := 0;
    if SameText(LFileExt,'.svg') then
    begin
      LSVG := GlobalSVGFactory.NewSvg;
      LSVG.LoadFromStream(FStream);
      LScaleFactor := CalcScaleFactor(Round(Lsvg.Width));
      if (LScaleFactor <> 1) then
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
        //LBitmap.SaveToStream(FStream);
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
    else
    begin
      LImage := nil;
      LScaledImage := nil;
      try
        begin
          LImage := TWICImage.Create;
          LImage.LoadFromStream(FStream);
          LScaleFactor := CalcScaleFactor(LImage.Width);
          if (ViewerSettings.RescalingImage) and (LScaleFactor <> 1) then
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
    //don't raise any error
  end;
end;

end.
