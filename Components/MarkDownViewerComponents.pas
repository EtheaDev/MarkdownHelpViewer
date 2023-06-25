{******************************************************************************}
{                                                                              }
{       Viewer Components to show MarkDown and HTML content                    }
{                                                                              }
{       Copyright (c) 2023 (Ethea S.r.l.)                                      }
{       Author: Carlo Barazzetta                                               }
{                                                                              }
{       https://github.com/EtheaDev/MarkdownHelpViewer                         }
{                                                                              }
{******************************************************************************}
{  Those Components requires Two Libraries:                                    }
{                                                                              }
{   Delphi Markdown                                                            }
{   https://github.com/grahamegrieve/delphi-markdown                           }
{   Copyright (c) 2011+, Health Intersections Pty Ltd All rights reserved.     }
{                                                                              }
{   HTMLViewer - https://github.com/BerndGabriel/HtmlViewer                    }
{   Copyright (c) 1995 - 2008 by L. David Baldwin                              }
{   opyright (c) 1995 - 2023 by Anders Melander (DitherUnit.pas)               }
{   Copyright (c) 1995 - 2023 by Ron Collins (HtmlGif1.pas)                    }
{   Copyright (c) 2008 - 2009 by Sebastian Zierer (Delphi 2009 Port)           }
{   opyright (c) 2008 - 2010 by Arvid Winkelsdorf (Fixes)                      }
{   Copyright (c) 2009 - 2023 by HtmlViewer Team                               }
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
unit MarkDownViewerComponents;

interface

uses
  System.Classes
  , System.SysUtils
  , Vcl.Graphics
  , HTMLUn2
  , HtmlView
  , HtmlGlobals
  , MarkdownProcessor
  ;

Type
  TMarkDownViewer = class(THTMLViewer)
  private
    FFileName: TFileName;
    FFileContent: string;
    FHTMLContent: string;
    FProcessorDialect: TMarkdownProcessorDialect;
    FCssStyle: string;
    FRescalingImage: Boolean;
    FMarkDownContent: string;
    FStream: TMemoryStream;
    FImageRequest: TGetImageEvent;
    procedure SetFileName(const AValue: TFileName);
    procedure SetProcessorDialect(const AValue: TMarkdownProcessorDialect);
    function IsCssStyleStored: Boolean;
    function IsDefFontName: Boolean;

    procedure SetCssStyle(const Value: string);
    procedure SetRescalingImage(const Value: Boolean);
    procedure SetHTMLContent(const Value: string);
    procedure SetMarkDownContent(const Value: string);
    procedure HtmlViewerImageRequest(Sender: TObject;
      const ASource: UnicodeString; var AStream: TStream);
    procedure ConvertImage(AFileName: string; const AMaxWidth: Integer;
      const ABackgroundColor: TColor);
    function GetOnImageRequest: TGetImageEvent;
  protected
    procedure SetOnImageRequest(const AValue: TGetImageEvent); reintroduce;
  public
    procedure LoadFromFile(const AFileName: TFileName);
    procedure LoadFromStream(const AStream: TStringStream);
    procedure LoadFromString(const AValue: string);
    function TransformContent(const AMarkDownContent: string;
      AProcessorDialect: TMarkdownProcessorDialect = mdCommonMark;
      const ACssStyle: string = ''): string;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RefreshViewer(const AReloadImages: Boolean;
      ARescalingImage: Boolean);
  published
    //inherited properties
    property AlignWithMargins default True;
    property BorderStyle default htFocused;
    property DefBackground default clWindow;
    property DefFontName stored IsDefFontName;
    property DefFontSize default 10;
    property NoSelect default False;
    property Text stored False;

    //specific properties
    property CssStyle: string read FCssStyle write SetCssStyle stored IsCssStyleStored;
    property FileName: TFileName read FFileName write SetFileName;
    property ProcessorDialect: TMarkdownProcessorDialect read FProcessorDialect write SetProcessorDialect default mdCommonMark;
    property RescalingImage: Boolean read FRescalingImage write SetRescalingImage;
    property HtmlContent: string read FHTMLContent write SetHTMLContent;
    property MarkDownContent: string read FMarkDownContent write SetMarkDownContent;
    property OnImageRequest: TGetImageEvent read GetOnImageRequest write SetOnImageRequest;
  end;

function TryLoadTextFile(const AFileName: TFileName): string;
function GetMarkDownDefaultCSS: string;

implementation

uses
  System.StrUtils
  , Winapi.GDIPOBJ
  , Winapi.GDIPAPI
  , Vcl.Imaging.pngImage
  ;

function GetMarkDownDefaultCSS: string;
begin
  Result :=
    '<style type="text/css">'+sLineBreak+
    'code{'+sLineBreak+
    '  font-size: medium;'+sLineBreak+
    '  font-family: ui-monospace,SFMono-Regular,SF Mono,Menlo,Consolas,Liberation Mono,monospace;'+sLineBreak+
    '}'+sLineBreak+
    'pre{'+sLineBreak+
    '  border: 1px solid #ddd;'+sLineBreak+
    '  border-left: 3px solid #0d6efd;'+sLineBreak+
    '  overflow: auto;'+sLineBreak+
    '  padding: 1em 1.5em;'+sLineBreak+
    '  display: block;'+sLineBreak+
    '}'+sLineBreak+
    'Blockquote{'+sLineBreak+
    '  border-left: 3px solid #0d6efd;'+sLineBreak+
    '  padding-left: 0.5em;'+sLineBreak+
    '  margin-left:1em;'+sLineBreak+
    '}'+sLineBreak+
    'Blockquote p{'+sLineBreak+
    '  margin: 0;'+sLineBreak+
    '}'+sLineBreak+
    'table{'+sLineBreak+
    '  border:1px solid;'+sLineBreak+
    '  border-collapse:collapse;'+sLineBreak+
    '}'+sLineBreak+
    'th{'+
    '  padding:5px;'+sLineBreak+
    '  border:1px solid;'+sLineBreak+
    '}'+sLineBreak+
    'td{'+sLineBreak+
    '  padding:5px;'+sLineBreak+
    '  border:1px solid;'+sLineBreak+
    '}'+sLineBreak+
    '</style>'+sLineBreak;
end;

function TryLoadTextFile(const AFileName : TFileName): string;
var
  LStream: TStream;
  LStreamReader: TStreamReader;
begin
  Result := '';
  LStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    try
      LStreamReader := TStreamReader.Create(LStream, TEncoding.UTF8);
      try
        while not LStreamReader.EndOfStream do
          Result := Result + LStreamReader.ReadLine + sLineBreak;
      finally
        FreeAndNil(LStreamReader);
      end;
    except
      On E: EEncodingError do
      begin
        LStream.Position := 0;
        LStreamReader := TStreamReader.Create(LStream, TEncoding.ANSI);
        try
          while not LStreamReader.EndOfStream do
            Result := Result + LStreamReader.ReadLine + sLineBreak;
        finally
          FreeAndNil(LStreamReader);
        end;
      end;
    end;
  finally
    FreeAndNil(LStream);
  end;
end;

{ TMarkDownViewer }

constructor TMarkDownViewer.Create(AOwner: TComponent);
begin
  inherited;
  FStream := TMemoryStream.Create;
  inherited OnImageRequest := HtmlViewerImageRequest;

  //inherited properties: default changed
  AlignWithMargins :=  True;
  BorderStyle := htFocused;
  DefBackground := clWindow;
  DefFontName := 'Arial';
  DefFontSize := 10;
  NoSelect := False;

  FCssStyle := GetMarkDownDefaultCSS;
end;

destructor TMarkDownViewer.Destroy;
begin
  FreeAndNil(FStream);
  inherited;
end;

procedure TMarkDownViewer.SetOnImageRequest(const AValue: TGetImageEvent);
begin
  FImageRequest := AValue;
  if Assigned(FImageRequest) then
    inherited OnImageRequest := FImageRequest
  else
    inherited OnImageRequest := HtmlViewerImageRequest;
end;

function TMarkDownViewer.GetOnImageRequest: TGetImageEvent;
begin
  Result := FImageRequest;
end;

function TMarkDownViewer.IsCssStyleStored: Boolean;
begin
  Result := FCssStyle <> GetMarkDownDefaultCSS;
end;

function TMarkDownViewer.IsDefFontName: Boolean;
begin
  Result := DefFontName <> 'Arial';
end;

procedure TMarkDownViewer.LoadFromFile(const AFileName: TFileName);
begin
  //Load file
  LoadFromString(TryLoadTextFile(AFileName));
end;

procedure TMarkDownViewer.LoadFromStream(const AStream: TStringStream);
begin
  LoadFromString(AStream.DataString);
end;

procedure TMarkDownViewer.LoadFromString(const AValue: string);
begin
  //Load file
  FFileContent := AValue;
  if not ContainsText(FFileContent, '<HTML>') then
  begin
    FMarkDownContent := FFileContent;
    FHTMLContent := TransformContent(FMarkDownContent, FProcessorDialect, FCssStyle);
  end
  else
  begin
    //Do not trasform content
    FHTMLContent := AValue;
    FMarkDownContent := '';
  end;
  //Load html content into HtmlViewer
  RefreshViewer(True, FRescalingImage);
end;

procedure TMarkDownViewer.RefreshViewer(const AReloadImages: Boolean;
  ARescalingImage: Boolean);
var
  LOldPos: Integer;
begin
  if AReloadImages then
    Self.Clear;
  if FHTMLContent = '' then
    Exit;
  //Load HTML content into HTML-Viewer
  LOldPos := Self.VScrollBarPosition;
  try
    inherited LoadFromString(FHTMLContent);
  finally
    Self.VScrollBarPosition := LOldPos;
  end;
end;

procedure TMarkDownViewer.SetCssStyle(const Value: string);
begin
  FCssStyle := Value;
end;

procedure TMarkDownViewer.SetFileName(const AValue: TFileName);
begin
  if FFileName <> AValue then
  begin
    LoadFromFile(AValue);
    FFileName := AValue;
  end;
end;

procedure TMarkDownViewer.SetHTMLContent(const Value: string);
begin
  if FHTMLContent <> Value then
    LoadFromString(Value);
end;

procedure TMarkDownViewer.SetMarkDownContent(const Value: string);
begin
  if FMarkDownContent <> Value then
    LoadFromString(Value);
end;

procedure TMarkDownViewer.SetProcessorDialect(
  const AValue: TMarkdownProcessorDialect);
begin
  FProcessorDialect := AValue;
end;

procedure TMarkDownViewer.SetRescalingImage(const Value: Boolean);
begin
  FRescalingImage := Value;
end;

function TMarkDownViewer.TransformContent(const AMarkDownContent: string;
  AProcessorDialect: TMarkdownProcessorDialect = mdCommonMark;
  const ACssStyle: string = ''): string;
var
  LMarkDownProcessor: TMarkdownProcessor;
begin
  //Transform file MarkDown in HTML using TMarkdownProcessor
  LMarkDownProcessor := TMarkdownProcessor.CreateDialect(AProcessorDialect);
  Try
    Result := ACssStyle+LMarkDownProcessor.Process(AMarkDownContent);
  Finally
    LMarkDownProcessor.Free;
  End;
end;

procedure TMarkDownViewer.ConvertImage(AFileName: string;
  const AMaxWidth: Integer; const ABackgroundColor: TColor);
var
  {$if CompilerVersion > 33}
  LPngImage: TPngImage;
  LBitmap: TBitmap;
  LScaleFactor: double;
  {$endif}
  LImage, LScaledImage: TWICImage;
  LFileExt: string;

  {$if CompilerVersion > 33}
  function PNG4TransparentBitMap(aBitmap: TBitmap): TPNGImage;
  type
    TRGB = packed record B, G, R: byte end;
    TRGBA = packed record B, G, R, A: byte end;
    TRGBAArray = array[0..0] of TRGBA;

  var
    X, Y: integer;
    BmpRGBA: ^TRGBAArray;
    PngRGB: ^TRGB;
  begin
    //201011 Thomas Wassermann
    Result := TPNGImage.CreateBlank(COLOR_RGBALPHA, 8, aBitmap.Width , aBitmap.Height);

    Result.CreateAlpha;
    Result.Canvas.CopyMode:= cmSrcCopy;
    Result.Canvas.Draw(0, 0, aBitmap);

    for Y := 0 to Pred(aBitmap.Height) do
    begin
      BmpRGBA := aBitmap.ScanLine[Y];
      PngRGB:= Result.Scanline[Y];

      for X := 0 to Pred(aBitmap.width) do
      begin
        Result.AlphaScanline[Y][X] :=  BmpRGBA[X].A;
        if aBitmap.AlphaFormat in [afDefined, afPremultiplied] then
        begin
          if BmpRGBA[X].A <> 0 then
          begin
            PngRGB^.B := Round(BmpRGBA[X].B / BmpRGBA[X].A * 255);
            PngRGB^.R := Round(BmpRGBA[X].R / BmpRGBA[X].A * 255);
            PngRGB^.G := Round(BmpRGBA[X].G / BmpRGBA[X].A * 255);
          end else begin
            PngRGB^.B := Round(BmpRGBA[X].B * 255);
            PngRGB^.R := Round(BmpRGBA[X].R * 255);
            PngRGB^.G := Round(BmpRGBA[X].G * 255);
          end;
        end;
        Inc(PngRGB);
      end;
    end;
  end;

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
  {$endif}

begin
  LFileExt := ExtractFileExt(AFileName);
  try
    FStream.Position := 0;
    LImage := nil;
    LScaledImage := nil;
    try
      begin
        LImage := TWICImage.Create;
        LImage.LoadFromStream(FStream);
        {$if CompilerVersion > 33}
        //Rescaling bitmap and save to stream
        LScaleFactor := CalcScaleFactor(LImage.Width);
        if (FRescalingImage) and (LScaleFactor <> 1) then
        begin
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
        {$else}
        FStream.Position := 0;
        {$endif}
      end;
    finally
      LImage.Free;
      LScaledImage.Free;
    end;
  except
    //don't raise any error
  end;
end;

procedure TMarkDownViewer.HtmlViewerImageRequest(Sender: TObject;
  const ASource: UnicodeString; var AStream: TStream);
var
  LFullName: String;
  LHtmlViewer: THtmlViewer;
  LMaxWidth: Integer;
Begin
  LHtmlViewer := sender as THtmlViewer;

  AStream := nil;

  // is "fullName" a local file, if not aquire file from internet
  If FileExists(ASource) then
    LFullName := ASource
  else
  begin
    LFullName := IncludeTrailingPathDelimiter(LHtmlViewer.ServerRoot)+ASource;
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
  end;
End;

end.
