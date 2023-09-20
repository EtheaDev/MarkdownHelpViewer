{*******************************************************}
{                                                       }
{            Delphi Visual Component Library            }
{                                                       }
{ Copyright(c) 2010-2023 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}


var
  DesignMode: boolean = false;

const

  WM_INVALIDATESTYLEOBJECT = WM_USER + 1097;
  WM_GETSTYLEFORM = WM_USER + 1098;

  DPI_DEFAULT = 96;
  DPI_15x = 144;
  DPI_20x = 192;

  DefaultFontName = 'Tahoma';
  DefaultFontSize = 8;
  ktoDefault = 'default';

  MaxSysColor = 23;

  SysColors: array[0..MaxSysColor - 1] of TIdentMapEntry = (
    (Value: Vcl.Graphics.clActiveBorder; Name: 'clActiveBorder'),
    (Value: Vcl.Graphics.clActiveCaption; Name: 'clActiveCaption'),
    (Value: Vcl.Graphics.clBtnFace; Name: 'clBtnFace'),
    (Value: Vcl.Graphics.clBtnHighlight; Name: 'clBtnHighlight'),
    (Value: Vcl.Graphics.clBtnShadow; Name: 'clBtnShadow'),
    (Value: Vcl.Graphics.clBtnText; Name: 'clBtnText'),
    (Value: Vcl.Graphics.clCaptionText; Name: 'clCaptionText'),
    (Value: Vcl.Graphics.clGrayText; Name: 'clGrayText'),
    (Value: Vcl.Graphics.clHighlight; Name: 'clHighlight'),
    (Value: Vcl.Graphics.clHighlightText; Name: 'clHighlightText'),
    (Value: Vcl.Graphics.clInactiveBorder; Name: 'clInactiveBorder'),
    (Value: Vcl.Graphics.clInactiveCaption; Name: 'clInactiveCaption'),
    (Value: Vcl.Graphics.clInactiveCaptionText; Name: 'clInactiveCaptionText'),
    (Value: Vcl.Graphics.clInfoBk; Name: 'clInfoBk'),
    (Value: Vcl.Graphics.clInfoText; Name: 'clInfoText'),
    (Value: Vcl.Graphics.clMenu; Name: 'clMenu'),
    (Value: Vcl.Graphics.clMenuText; Name: 'clMenuText'),
    (Value: Vcl.Graphics.clScrollBar; Name: 'clScrollBar'),
    (Value: Vcl.Graphics.cl3DDkShadow; Name: 'cl3DDkShadow'),
    (Value: Vcl.Graphics.cl3DLight; Name: 'cl3DLight'),
    (Value: Vcl.Graphics.clWindow; Name: 'clWindow'),
    (Value: Vcl.Graphics.clWindowFrame; Name: 'clWindowFrame'),
    (Value: Vcl.Graphics.clWindowText; Name: 'clWindowText'));


type

  TSeStyleObject = class;
  TSeStyleSource = class;
  TSeStyleElementObject = string;

   { Color mertic }

  TSeStyleColor = (
    ktcBorder,
    ktcCategoryButtons,
    ktcCategoryPanelGroup,
    ktcComboBox,
    ktcComboBoxDisabled,
    ktcButton,
    ktcButtonHot,
    ktcButtonPressed,
    ktcButtonFocused,
    ktcButtonDisabled,
    ktcEdit,
    ktcEditDisabled,
    ktcGrid,
    ktcGenericGradientBase,
    ktcGenericGradientEnd,
    ktcHintGradientBase,
    ktcListBox,
    ktcListBoxDisabled,
    ktcListView,
    ktcPanel,
    ktcPanelDisabled,
    ktcTreeView,
    ktcWindow,
    ktcSplitter,
    ktcCategoryButtonsGradientBase,
    ktcCategoryButtonsGradientEnd,
    ktcToolBarGradientBase,
    ktcToolBarGradientEnd,
    ktcGenericBackground,
    ktcHintGradientEnd,
    ktcFocusEffect,
    ktsAlternatingRowBackground
  );

  TSeStyleColors = class(TPersistent)
  private
    FColorList: array [TSeStyleColor] of TColor;
    FStyle: TPersistent;
    function GetColor(index: TSeStyleColor): TColor;
    procedure SetColor(index: TSeStyleColor; const Value: TColor);
  public
    constructor Create(AStyle: TPersistent); virtual;
    procedure Assign(Source: TPersistent); override;
    { I/O }
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure SaveToStream(Stream: TStream); virtual;
    property Colors[index: TSeStyleColor]: TColor read GetColor write SetColor; default;
  end;


  TSeStyleSysColors = class(TPersistent)
  private
    FColorList: array [0..MaxSysColor - 1] of TColor;
    FStyle: TPersistent;
    function GetColor(index: TColor): TColor;
    procedure SetColor(index: TColor; const Value: TColor);
  public
    constructor Create(AStyle: TPersistent); virtual;
    procedure Assign(Source: TPersistent); override;
    { I/O }
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure SaveToStream(Stream: TStream); virtual;
    property Colors[index: TColor]: TColor read GetColor write SetColor; default;
  end;


  TSeStyleFont = (
    ktfCaptionTextNormal,
    ktfCaptionTextInactive,
    ktfSmCaptionTextNormal,
    ktfSmCaptionTextInactive,
    ktfStaticTextNormal,
    ktfStaticTextHot,
    ktfStaticTextFocused,
    ktfStaticTextDisabled,
    ktfPanelTextNormal,
    ktfPanelTextDisabled,
    ktfButtonTextNormal,
    ktfButtonTextPressed,
    ktfButtonTextHot,
    ktfButtonTextFocused,
    ktfButtonTextDisabled,
    ktfCheckBoxTextNormal,
    ktfCheckBoxTextPressed,
    ktfCheckBoxTextHot,
    ktfCheckBoxTextFocused,
    ktfCheckBoxTextDisabled,
    ktfRadioButtonTextNormal,
    ktfRadioButtonTextPressed,
    ktfRadioButtonTextHot,
    ktfRadioButtonTextFocused,
    ktfRadioButtonTextDisabled,
    ktfGroupBoxTextNormal,
    ktfGroupBoxTextDisabled,
    ktfWindowTextNormal,
    ktfWindowTextDisabled,
    ktfEditBoxTextNormal,
    ktfEditBoxTextFocused,
    ktfEditBoxTextHot,
    ktfEditBoxTextDisabled,
    ktfEditBoxTextSelected,
    ktfMenuItemTextNormal,
    ktfMenuItemTextSelected,
    ktfMenuItemTextHot,
    ktfMenuItemTextDisabled,
    ktfToolItemTextNormal,
    ktfToolItemTextSelected,
    ktfToolItemTextHot,
    ktfToolItemTextDisabled,
    ktfHeaderSectionTextNormal,
    ktfHeaderSectionTextPressed,
    ktfHeaderSectionTextDraggedOut,
    ktfHeaderSectionTextDragging,
    ktfHeaderSectionTextHot,
    ktfHeaderSectionTextUnderDrag,
    ktfHeaderSectionTextDisabled,
    ktfStatusPanelTextNormal,
    ktfStatusPanelTextDisabled,
    ktfTabTextInactiveNormal,
    ktfTabTextInactiveHot,
    ktfTabTextInactiveDisabled,
    ktfTabTextActiveNormal,
    ktfTabTextActiveHot,
    ktfTabTextActiveDisabled,
    ktfListItemTextNormal,
    ktfListItemTextHot,
    ktfListItemTextSelected,
    ktfListItemTextFocused,
    ktfListItemTextDisabled,
    ktfPopupMenuItemTextNormal,
    ktfPopupMenuItemTextSelected,
    ktfPopupMenuItemTextHot,
    ktfPopupMenuItemTextDisabled,
    //-- Added
    ktfCatgeoryButtonsNormal,
    ktfCatgeoryButtonsHot,
    ktfCatgeoryButtonsSelected,
    ktfCatgeoryButtonsCategoryNormal,
    ktfCatgeoryButtonsCategorySelected,
    ktfCategoryPanelGroupHeaderNormal,
    ktfCategoryPanelGroupHeaderHot,
    ktfComboBoxItemNormal,
    ktfComboBoxItemFocused,
    ktfComboBoxItemHot,
    ktfComboBoxItemDisabled,
    ktfComboBoxItemSelected,
    ktfGridItemNormal,
    ktfGridItemSelected,
    ktfGridItemFixedNormal,
    ktfGridItemFixedHot,
    ktfGridItemFixedPressed,
    ktfTreeItemTextNormal,
    ktfTreeItemTextHot,
    ktfTreeItemTextSelected,
    ktfTreeItemTextFocused,
    ktfTreeItemTextDisabled,
    // FMBox object fonts (for FireMonkey controls)
    ktFMBoxTextNormal,
    ktFMBoxTextPressed,
    ktFMBoxTextHot,
    ktFMBoxTextFocused,
    ktFMBoxTextDisabled
  );

  TSeStyleFonts = class(TPersistent)
  private
    FFontList: array [TSeStyleFont] of TFont;
    FStyle: TPersistent;
    FStyleFont: TFont;
    function GetFont(index: TSeStyleFont): TFont;
    procedure SetFont(index: TSeStyleFont; const Value: TFont);
  public
    constructor Create(AStyle: TPersistent); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    { I/O }
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure SaveToStream(Stream: TStream); virtual;
    { Font access }
    function GetFontForObject(Font: TSeStyleFont;
      const AObject: TSeStyleElementObject = ktoDefault): TFont;
    property Fonts[index: TSeStyleFont]: TFont read GetFont write SetFont; default;
  end;


  TSeAlign = (saNone, saLeft, saMostLeft, saTop, saMostTop, saRight, saMostRight,
    saBottom, saMostBottom, saClient, saTopLeft, saTopRight, saBottomLeft,
    saBottomRight, saCenter, saText, saTopRightAngle, saAll);

  TSeKind = (
    skNone,
    { Forms }
    skForm, skCaption, skTitle, skTop, skLeft, skRight, skBottom, skTopLeft,
    skTopRight, skBottomLeft, skBottomRight, skSysButton, skClient,
    { Menus }
    skMenuBar, skMenuBarItem, skMenuBarItemText,
    skPopupMenu, skPopupMenuItem, skPopupMenuItemText,
    skItemText,
    skTranparentOnly,
    skTranparent,
    skRoundMask,
    skNoMask,
    skMaskOnly,
    skFMXButton,
    skFMXSpeedButton,
    skFMXPanel,
    skFMXEdit,
    skFMXLabel,
    skFMXProgressBar,
    skFMXTrackBar,
    skFMXTabImage,
    skFMXListBox,
    skFMXActiveLabel,
    skFMXGlyphButton,
    skFMXSwitch,
    skFMXListBoxItem,
    skFMXCheckBox,
    skFMXRadioButton
  );

  TSeState = (
    ssNormal,
    { Builder}
    ssDesign,
    { Forms }
    ssMaximized, ssMinimized, ssRollup,
    { Menus and Controls }
    ssHot, ssPressed, ssFocused, ssDisabled
  );

  TSeTextAlign = (taTopLeft, taTopCenter, taTopRight, taLeft, taCenter,
    taRight, taBottomLeft, taBottomCenter, taBottomRight);

  TSeTextEffect = (teNone, teShadow);

  TscTileStyle = (tsTile, tsStretch, tsCenter, tsVertCenterStretch,
    tsVertCenterTile, tsHorzCenterStretch, tsHorzCenterTile);

  TSeAction = (sbaNone, sbaClose, sbaHelp, sbaMinimize, sbaMaximize,
    sbaRestore, sbaRollUp, sbaRollDown, sbaTray, sbaSysMenu, sbaRollPanel,
    sbaCustom);

  TSeButtonState = (sbsNormal, sbsHover, sbsDown);

  TSeRollKind = (rkLeft, rkTop, rkRight, rkBottom);

  TSeFilterSign = array [0..12] of ansichar;

{ TSeStyleFilter }

  TSeStyleFilter = class(TPersistent)
  private
    FStyleSource: TSeStyleSource;
    FSign: TseFilterSign;
    procedure SetStyleSource(const Value: TSeStyleSource);
  protected
    procedure ReadStyleSource(Stream: TStream); virtual;
    procedure WriteStyleSource(Stream: TStream); virtual;
  public
    { }
    class function GetFileExtension: string; virtual;
    class function GetFilterName: string; virtual;
    { Filter }
    procedure ReadStyle(Stream: TStream); virtual;
    procedure WriteStyle(Stream: TStream); virtual;
    { Property }
    property StyleSource: TSeStyleSource read FStyleSource write SetStyleSource;
  end;

{ TSeStyleSource }

  TSeStyleSource = class(TComponent)
  private
    { Private declarations }
    FName: string;
    FAuthorURL: string;
    FVersion: string;
    FAuthor: string;
    FAuthorEMail: string;
    FDisplayNames: TStrings;
    FMobilePlatform: Boolean;
    FRetinaDisplay: Boolean;
    FDescription: string;
    { Objects and images }
    FObjects: TList;
    FBitmaps: TSeBitmapList;
    FColors: TSeStyleColors;
    FSysColors: TSeStyleSysColors;
    FFonts: TSeStyleFonts;
    { States }
    FIsChanging: boolean;
    { Objects }
    function GetForm: TSeStyleObject;
    { Properties }
    function GetObject(index: integer): TSeStyleObject;
    function GetCount: integer;
    procedure SetDisplayNames(Value: TStrings);
  protected
    { Protected declarations }
    procedure CheckingObject;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { I/O methods}
    procedure LoadFromStream(Stream: TStream); dynamic;
    procedure SaveToStream(Stream: TStream); dynamic;
    procedure LoadFromFile(const FileName: string); dynamic;
    procedure SaveToFile(const FileName: string); dynamic;
    procedure FillColorsAndFonts;
    { Assign and copy methods }
    procedure Assign(Source: TPersistent); override;
    procedure Clear;
    { Objects }
    procedure Add(StyleObject: TSeStyleObject);
    procedure Remove(StyleObject: TSeStyleObject);
    function GetObjectByKind(Kind: TSeKind): TSeStyleObject;
    function GetObjectByName(const Name: string): TSeStyleObject;
    procedure ReplaceBitmap(Source, Dest: TSeBitmap);
    { Public property }
    property Count: integer read GetCount;
    property Objects[index: integer]: TSeStyleObject read GetObject; default;
    property Bitmaps: TSeBitmapList read FBitmaps;
    property Colors: TSeStyleColors read FColors;
    property SysColors: TSeStyleSysColors read FSysColors;
    property Fonts: TSeStyleFonts read FFonts;
    { Necessary objects }
    property Form: TSeStyleObject read GetForm;
    { States }
    property DisplayNames: TStrings read FDisplayNames write SetDisplayNames;
    property IsChanging: boolean read FIsChanging write FIsChanging;
  published
    { Published declarations }
    property Name: string read FName write FName;
    property Version: string read FVersion write FVersion;
    property Author: string read FAuthor write FAuthor;
    property AuthorEMail: string read FAuthorEMail write FAuthorEMail;
    property AuthorURL: string read FAuthorURL write FAuthorURL;
    property MobilePlatform: Boolean read FMobilePlatform write FMobilePlatform;
    property RetinaDisplay: Boolean read FRetinaDisplay write FRetinaDisplay;
    property Description: String read FDescription write FDescription;
  end;

{ TSeStyleObject class }

  TSeWindowButtonClass = (
    kwbSysMenu,
    kwbClose,
    kwbHelp,
    kwbMin,
    kwbMinRestore,
    kwbMax,
    kwbMaxRestore,
    kwbRoll,
    kwbRollRestore,
    kwbTray,
    kwbTopmost,
    kwbTopmostRestore,
    kwbCustom
  );

  TSeWindowButtons = set of TSeWindowButtonClass;

  TSeWindowButtonDrawState = (
    kwbdsNormal,
    kwbdsHot,
    kwbdsPressed,
    kwbdsInactive
  );


  TSeStyleObject = class(TComponent)
  private
    FAlign: TSeAlign;
    FKind: TSeKind;
    FLeft: integer;
    FTop: integer;
    FWidth: integer;
    FHeight: integer;
    FVisible: boolean;
    FBitmaps: TSeBitmapList;
    FActive: boolean;
    FState: TSeState;
    FParentControl: TWinControl;
    FOldWidth, FOldHeight, FOldLeft, FOldTop: integer;
    FText: string;
    FTextAlign: TSeTextAlign;
    FTextMarginRight: integer;
    FTextMarginLeft: integer;
    FTextMarginTop: integer;
    FTextEffect: TSeTextEffect;
    FDrawIfOwner: boolean;
    FRighTSext: string;
    FEnabled: boolean;
    FMarginLeft: integer;
    FMarginTop: integer;
    FMarginBottom: integer;
    FMarginRight: integer;
    FBiDiMode: TBiDiMode;
    FMasked: boolean;
    FSysButtons: TSeWindowButtons;
    FTextMarginRightStretch: boolean;
    FLoaded: boolean;
    FTextShiftSysIton: boolean;
    FFixPosition: boolean;
    FCornerRadius: integer;
    FFocusEffect: Boolean;
    FParams: String;
    FLocked: Boolean;

    procedure ReadData(Stream: TStream);
    procedure WriteData(Stream: TStream);

    procedure ReadLocked(Reader: TReader);
    procedure WriteLocked(Writer: TWriter);

    function GetCount: integer;
    function GetObject(index: integer): TSeStyleObject;
    function GetBoundsRect: TRect;
    procedure SetHeight(const Value: integer);
    procedure SetWidth(const Value: integer);
    procedure SetLeft(const Value: integer);
    procedure SetTop(const Value: integer);
    procedure SetBoundsRect(const Value: TRect);
    procedure SetBitmaps(const Value: TSeBitmapList);
    procedure SetFont(const Value: TFont);
    procedure SetActive(const Value: boolean);
    procedure SetParentControl(const Value: TWinControl);
    procedure SetMarginBottom(const Value: integer);
    procedure SetMarginLeft(const Value: integer);
    procedure SetMarginRight(const Value: integer);
    procedure SetMarginTop(const Value: integer);
    procedure SetBiDiMode(const Value: TBiDiMode);
    procedure SetCornerRadius(const Value: integer);
  protected
    FColor: TColor;
    FFont: TFont;
    FStopDrawChilds: Boolean;
    procedure DefineProperties(Filer: TFiler); override;
    function UseRightToLeftAlignment: Boolean;
    function UseRightToLeftReading: Boolean;
    function DrawTextBiDiModeFlags(Flags: Integer): Longint;
    function DrawTextBiDiModeFlagsReadingOnly: Longint;

    function GetFont: TFont; virtual;
    procedure SetSysButtons(const Value: TSeWindowButtons); virtual;
    procedure SetState(const Value: TSeState); virtual;
    function CanDraw(DPI: Integer): Boolean;
    function GetDPI: Integer;
    function GetNameExcludeDPI: String;
    function GetScaledObject(DPI: Integer): TseStyleObject;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AfterLoad; virtual;
    { Assign, Copy }
    procedure Assign(Source: TPersistent); override;
    function CreateCopy(AOwner: TSeStyleObject): TSeStyleObject;
    { Aligning }
    procedure Aligning(ADPI: Integer = 0);
    procedure AligningAll(ADPI: Integer = 0);
    procedure Invalidate;
    { Drawing }
    procedure Draw(Canvas: TCanvas; const ClipRect: TRect; DPI: Integer = 0); virtual;
    procedure DrawChild(Canvas: TCanvas; const ClipRect: TRect; DPI: Integer = 0);
    procedure DrawObjectText(Canvas: TCanvas; DPI: Integer = 0);
    procedure SafeDraw(Canvas: TCanvas; const ClipRect: TRect; DPI: Integer = 0); virtual;
    { Region }
    function GetRegion: HRgn;
    function CreateRegion: HRgn; virtual;
    { Events }
    procedure MouseHover; virtual;
    procedure MouseLeave; virtual;
    procedure MouseDown(Button: TMouseButton; X, Y: integer); virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); virtual;
    procedure MouseUp(Button: TMouseButton; X, Y: integer); virtual;
    procedure MouseDouble(Button: TMouseButton; X, Y: integer); virtual;
    { Children }
    procedure Add(StyleObject: TSeStyleObject);
    procedure Remove(StyleObject: TSeStyleObject);
    function FindObjectByAction(Action: TSeAction): TSeStyleObject;
    function FindObjectByKind(AKind: TSeKind): TSeStyleObject;
    function FindObjectByName(const AName: string): TSeStyleObject;
    function FindObjectByPoint(Point: TPoint): TSeStyleObject;
    function FindObjectByPointAll(Point: TPoint): TSeStyleObject;
    procedure SetCharset(CharSet: TFontCharset); virtual;
    { properties }
    property Active: boolean read FActive write SetActive;
    property BiDiMode: TBiDiMode read FBiDiMode write SetBiDiMode;
    property Bitmaps: TSeBitmapList read FBitmaps write SetBitmaps;
    property BoundsRect: TRect read GetBoundsRect write SetBoundsRect;
    property Count: integer read GetCount;
    property Enabled: boolean read FEnabled write FEnabled;
    property ParentControl: TWinControl read FParentControl write SetParentControl;
    property Objects[index: integer]: TSeStyleObject read GetObject; default;
    property State: TSeState read FState write SetState;
    property Text: string read FText write FText;
    property SysButtons: TSeWindowButtons read FSysButtons write SetSysButtons;
    property RighTSext: string read FRighTSext write FRighTSext;
    property Visible: boolean read FVisible write FVisible;
    property Locked: Boolean read FLocked write FLocked;
  published
    property Align: TSeAlign read FAlign write FAlign;
    property Color: TColor read FColor write FColor;
    property DrawIfOwner: boolean read FDrawIfOwner write FDrawIfOwner;
    property Font: TFont read GetFont write SetFont;
    property Kind: TSeKind read FKind write FKind;
    property Masked: boolean read FMasked write FMasked;
    property Left: integer read FLeft write SetLeft;
    property Top: integer read FTop write SetTop;
    { Specifies the object height }
    property Height: integer read FHeight write SetHeight;
    { Specifies the object width }
    property Width: integer read FWidth write SetWidth;
    { Specifies the left margin }
    property MarginLeft: integer read FMarginLeft write SetMarginLeft;
    { Specifies the right margin }
    property MarginRight: integer read FMarginRight write SetMarginRight;
    { Specifies the top margin }
    property MarginTop: integer read FMarginTop write SetMarginTop;
    { Specifies the bottom margin }
    property MarginBottom: integer read FMarginBottom write SetMarginBottom;
    { Specifies the text aligment }
    property TextAlign: TSeTextAlign read FTextAlign write FTextAlign;
    { Specifies the text effect }
    property TextEffect: TSeTextEffect read FTextEffect write FTextEffect;
    { Specifies the left text margin }
    property TextMarginLeft: integer read FTextMarginLeft write FTextMarginLeft;
    property TextShiftSysIton: boolean read FTextShiftSysIton write FTextShiftSysIton;
    { Specifies the right text margin }
    property TextMarginRight: integer read FTextMarginRight write FTextMarginRight;
    property TextMarginRightStretch: boolean read FTextMarginRightStretch write FTextMarginRightStretch;
    { Specifies the top text margin }
    property TextMarginTop: integer read FTextMarginTop write FTextMarginTop;
    { Fix position in parent object}
    property FixPosition: boolean read FFixPosition write FFixPosition;
    { Corner radius for FireMonkey controls}
    property CornerRadius: integer read FCornerRadius write SetCornerRadius;
    { Enable / disable focus glow effect for FireMonkey controls}
    property FocusEffect: Boolean read FFocusEffect write FFocusEffect;
    { For custom parameters}
    property Params: String read FParams write FParams;
  end;

  TSeStyleObjectClass = class of TSeStyleObject;

{ TSeActiveObject class }

  TSeActiveObject = class(TSeStyleObject)
  private
    FActiveColor: TColor;
    FActiveFont: TFont;
    FDisabledFont: TFont;
    procedure SetActiveFont(const Value: TFont);
    procedure SetDisabledFont(const Value: TFont);
  protected
     function GetFont: TFont; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Assign, Copy }
    procedure Assign(Source: TPersistent); override;
    { Drawing }
    procedure Draw(Canvas: TCanvas; const ClipRect: TRect; DPI: Integer = 0); override;
    { Charset }
    procedure SetCharset(CharSet: TFontCharset); override;
  published
    { Specifies the color if Active = True }
    property ActiveColor: TColor read FActiveColor write FActiveColor;
    { Specifies the font if Active = True }
    property ActiveFont: TFont read FActiveFont write SetActiveFont;
    property DisabledFont: TFont read FDisabledFont write SetDisabledFont;
  end;

{ TSeBitmapObject class }

  TSeBitmapObject = class(TSeStyleObject)
  private
    FBitmap, FScaledBitmap: TSeBitmapLink;
    FTileStyle: TscTileStyle;
    FBorderTileStyle: TscTileStyle;
    FMaskedBorder: boolean;
    FMaskedAngles: boolean;
    procedure SetBitmap(const Value: TSeBitmapLink);
    procedure SetBorderTileStyle(const Value: TscTileStyle);
  protected
    procedure DrawNormal(Canvas: TCanvas; const ClipRect: TRect; DPI: Integer = 0);
    procedure DrawRect(Canvas: TCanvas; const MarginRect, MarginDstRect: TRect;
      ATileStyle: TscTileStyle; AMasked: boolean);
    function CanScale: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AfterLoad; override;
    { Assign, Copy }
    procedure Assign(Source: TPersistent); override;
    { Drawing }
    procedure Draw(Canvas: TCanvas; const ClipRect: TRect; DPI: Integer = 0); override;
    procedure SafeDraw(Canvas: TCanvas; const ClipRect: TRect; DPI: Integer = 0); override;
    { Region }
    function CreateRegion: HRgn; override;
  published
    { Specifies the object's bitmap }
    property Bitmap: TSeBitmapLink read FBitmap write SetBitmap;
    property BorderTileStyle: TscTileStyle read FBorderTileStyle write SetBorderTileStyle;
    property MaskedBorder: boolean read FMaskedBorder write FMaskedBorder;
    property MaskedAngles: boolean read FMaskedAngles write FMaskedAngles;
    property TileStyle: TscTileStyle read FTileStyle write FTileStyle;
  end;

{ TSeActiveBitmap class }

  TSeActiveBitmap = class(TSeBitmapObject)
  private
    FActiveBitmap: TSeBitmapLink;
    FActiveFont: TFont;
    procedure SetActiveFont(const Value: TFont);
    procedure SetActiveBitmap(const Value: TSeBitmapLink);
  protected
    function GetFont: TFont; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AfterLoad; override;
    { Assign, Copy }
    procedure Assign(Source: TPersistent); override;
    { Drawing }
    procedure Draw(Canvas: TCanvas; const ClipRect: TRect; DPI: Integer = 0); override;
    { Charset }
    procedure SetCharset(CharSet: TFontCharset); override;
  published
    { Specifies the bitmap if Active = True }
    property ActiveBitmap: TSeBitmapLink read FActiveBitmap write SetActiveBitmap;
    { Specifies the font if Active = True }
    property ActiveFont: TFont read FActiveFont write SetActiveFont;
  end;

{ TSeSystemButton class }

  TSeSystemButton = class(TSeActiveBitmap)
  private
    FAction: TSeAction;
    FBitmapPressed: TSeBitmapLink;
    FBitmapHot: TSeBitmapLink;
    FButtonState: TSeButtonState;
    procedure SetAction(const Value: TSeAction);
    procedure SetBitmapPressed(const Value: TSeBitmapLink);
    procedure SetBitmapHot(const Value: TSeBitmapLink);
  protected
    procedure SetSysButtons(const Value: TSeWindowButtons); override;
    procedure SetState(const Value: TSeState); override;
    procedure SetChildState(const Value: TSeState);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AfterLoad; override;
    { Assign, Copy }
    procedure Assign(Source: TPersistent); override;
    { Drawing }
    procedure Draw(Canvas: TCanvas; const ClipRect: TRect; DPI: Integer = 0); override;
    property ButtonState: TSeButtonState read FButtonState write FButtonState;
  published
    { Specifies the click action }
    property Action: TSeAction read FAction write SetAction;
    { Specifies bitmap in ssHot state }
    property BitmapHot: TSeBitmapLink read FBitmapHot write SetBitmapHot;
    { Specifies bitmap in ssPressed state }
    property BitmapPressed: TSeBitmapLink read FBitmapPressed write SetBitmapPressed;
  end;

{ TSeButtonObject class }

  TSeButtonObject = class(TSeBitmapObject)
  private
    FBitmapFocused: TSeBitmapLink;
    FBitmapHot: TSeBitmapLink;
    FBitmapPressed: TSeBitmapLink;
    FBitmapDisabled: TSeBitmapLink;
    procedure SetBitmapFocused(const Value: TSeBitmapLink);
    procedure SetBitmapDisabled(const Value: TSeBitmapLink);
    procedure SetBitmapHot(const Value: TSeBitmapLink);
    procedure SetBitmapPressed(const Value: TSeBitmapLink);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AfterLoad; override;
    { Assign, Copy }
    procedure Assign(Source: TPersistent); override;
    { Drawing }
    procedure Draw(Canvas: TCanvas; const ClipRect: TRect; DPI: Integer = 0); override;
  published
    { Specifies the bitmap in ssHot state }
    property BitmapHot: TSeBitmapLink read FBitmapHot write SetBitmapHot;
    { Specifies the bitmap in ssFocused state }
    property BitmapFocused: TSeBitmapLink read FBitmapFocused write SetBitmapFocused;
    { Specifies the bitmap in ssPressed state }
    property BitmapPressed: TSeBitmapLink read FBitmapPressed write SetBitmapPressed;
    { Specifies the bitmap in ssDisabled state }
    property BitmapDisabled: TSeBitmapLink read FBitmapDisabled write SetBitmapDisabled;
  end;

{ TSeTextObject class }

  TSeTextObject = class(TSeStyleObject)
  private
    FFontHot: TFont;
    FFontFocused: TFont;
    FFontPressed: TFont;
    FFontDisabled: TFont;
    procedure SetFontHot(const Value: TFont);
    procedure SetFontDisabled(const Value: TFont);
    procedure SetFontFocused(const Value: TFont);
    procedure SetFontPressed(const Value: TFont);
  protected
    function GetFont: TFont; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Assign, Copy }
    procedure Assign(Source: TPersistent); override;
    { Drawing }
    procedure Draw(Canvas: TCanvas; const ClipRect: TRect; DPI: Integer = 0); override;
    { Font }
    procedure SetCharset(CharSet: TFontCharset); override;
  published
    property FontHot: TFont read FFontHot write SetFontHot;
    property FontFocused: TFont read FFontFocused write SetFontFocused;
    property FontPressed: TFont read FFontPressed write SetFontPressed;
    property FontDisabled: TFont read FFontDisabled write SetFontDisabled;
  end;

{ TSeLinkStyleObject class }

  TSeLinkStyleObject = class(TSeStyleObject)
  private
    FLinkControl: string;
  public
    { Assign, Copy }
    procedure Assign(Source: TPersistent); override;
    { Drawing }
    procedure Draw(Canvas: TCanvas; const ClipRect: TRect; DPI: Integer = 0); override;
  published
    { Specifies the linked VCL control by name }
    property LinkControl: string read FLinkControl write FLinkControl;
  end;

{ TSeActiveStyleObject class }

  TSeActiveStyleObject = class(TSeStyleObject)
  private
    FActiveColor: TColor;
    FActiveFont: TFont;
    procedure SetActiveFont(const Value: TFont);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Assign, Copy }
    procedure Assign(Source: TPersistent); override;
    { Drawing }
    procedure Draw(Canvas: TCanvas; const ClipRect: TRect; DPI: Integer = 0); override;
  published
    { Specifies the color in ssActive state }
    property ActiveColor: TColor read FActiveColor write FActiveColor;
    { Specifies the font in ssActive state }
    property ActiveFont: TFont read FActiveFont write SetActiveFont;
  end;

    { TSeColorButtonObject class }

  TSeColorButtonObject = class(TSeStyleObject)
  private
    FHotColor: TColor;
    FHotFont: TFont;
    FPressedColor: TColor;
    FFocusedColor: TColor;
    FDisabledColor: TColor;
    FPressedFont: TFont;
    FDisabledFont: TFont;
    FFocusedFont: TFont;
    procedure SetDisabledColor(const Value: TColor);
    procedure SetDisabledFont(const Value: TFont);
    procedure SetFocusedColor(const Value: TColor);
    procedure SetFocusedFont(const Value: TFont);
    procedure SetHotColor(const Value: TColor);
    procedure SetHotFont(const Value: TFont);
    procedure SetPressedColor(const Value: TColor);
    procedure SetPressedFont(const Value: TFont);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Assign, Copy }
    procedure Assign(Source: TPersistent); override;
    { Drawing }
    procedure Draw(Canvas: TCanvas; const ClipRect: TRect; DPI: Integer = 0); override;
  published
    { Specifies the color is ssHot state }
    property HotColor: TColor read FHotColor write SetHotColor;
    { Specifies the font is ssHot state }
    property HotFont: TFont read FHotFont write SetHotFont;
    { Specifies the color is ssPressed state }
    property PressedColor: TColor read FPressedColor write SetPressedColor;
    { Specifies the font is ssPressed state }
    property PressedFont: TFont read FPressedFont write SetPressedFont;
    { Specifies the color is ssFocused state }
    property FocusedColor: TColor read FFocusedColor write SetFocusedColor;
    { Specifies the font is ssFocused state }
    property FocusedFont: TFont read FFocusedFont write SetFocusedFont;
    { Specifies the color is ssDisabled state }
    property DisabledColor: TColor read FDisabledColor write SetDisabledColor;
    { Specifies the font is ssDisabled state }
    property DisabledFont: TFont read FDisabledFont write SetDisabledFont;
  end;


{ TSeRollPanelObject class }

  TSeRollPanelObject = class(TSeStyleObject)
  private
    FRollValue: integer;
    FRollKind: TSeRollKind;
    FOldValue: integer;
    procedure OwnerUpdate;
  protected
  public
    { Assign, Copy }
    procedure Assign(Source: TPersistent); override;
    { Roll }
    procedure Roll;
  published
    { Specifies the roll direction }
    property RollKind: TSeRollKind read FRollKind write FRollKind;
    { Specifies the value of width (or height) in rolled state }
    property RollValue: integer read FRollValue write FRollValue;
  end;

{ Registration }

const
  StyleFileSign_1_0: array [0..12] of ansichar = 'VCL_STYLE 1.0';
  StyleFileSign: array [0..12] of ansichar = 'VCL_STYLE 2.0';

var
  RegObjects: TList; { registered class }

procedure RegisterStyleObject(ObjectClass: TSeStyleObjectClass); forward;
function GetStyleObjectClass(const ClassName: string): TSeStyleObjectClass; forward;

procedure SaveStyleObject(Stream: TStream; StyleObject: TSeStyleObject); forward;
function LoadStyleObject(Stream: TStream; Owner: TSeStyleObject): TSeStyleObject; forward;

procedure SaveStyleObjectBinary(Stream: TStream; StyleObject: TSeStyleObject); forward;
function LoadStyleObjectBinary(Stream: TStream; Owner: TSeStyleObject): TSeStyleObject; forward;

function GetDialogFilter3: string; forward;

procedure FillFont(AFont: TFont; StyleObject: TSeStyleObject; State: TSeState); forward;


type

  { Style region }

  TSeRegion = HRgn;

{ Common consts ===============================================================}

const

  { Empty Rect }

  NullRect: TRect = (Left: -1; Top: -1; Right: -1; Bottom: -1);

{ Style classes ===============================================================}

  ktcWindowClass         = 'Window';
  ktcHintClass		 = 'Hint';
  ktcLabelClass		 = 'Label';
  ktcPanelClass	         = 'Panel';
  ktcButtonClass         = 'Button';
  ktcCheckClass          = 'Check';
  ktcSplitterClass	 = 'Splitter';
  ktcScrollClass         = 'Scroll';
  ktcProgressClass       = 'Progress';
  ktcTrackClass		 = 'Track';
  ktcHeaderClass	 = 'Header';
  ktcStatusClass	 = 'Status';
  ktcEditClass           = 'Edit';
  ktcGroupClass          = 'Group';
  ktcMemoClass           = 'Memo';
  ktcMenuClass           = 'Menu';
  ktcDockClass           = 'Dock';
  ktcToolClass           = 'Tool';
  ktcBarClass		 = 'Bar';
  ktcBarDockClass        = 'BarDock';
  ktcTabClass    	 = 'Tab';
  ktcListClass		 = 'List';
  ktcTreeClass		 = 'Tree';
  ktcGridClass		 = 'Grid';
  ktcBoxClass            = 'Box';

{ Background types ============================================================}

type

  TSeBackgroundInfo = record
    Rect: TRect;
    Control: TControl;
    ClipRect: TRect;
  end;

function BackgroundInfo(const ARect: TRect): TSeBackgroundInfo; overload; forward;
function BackgroundInfo(AControl: TControl): TSeBackgroundInfo; overload; forward;
function BackgroundInfo(const ARect: TRect; AControl: TControl;
  const AClipRect: TRect): TSeBackgroundInfo; overload; forward;

{ Text types ==================================================================}

type

  { Text alignment  }

  TSeTextAlign2 = (
    ktxaTopLeft,
    ktxaMiddleLeft,
    ktxaBottomLeft,
    ktxaTopCenter,
    ktxaMiddleCenter,
    ktxaBottomCenter,
    ktxaTopRight,
    ktxaMiddleRight,
    ktxaBottomRight
  );

  { Text style  }

  TSeTextStyle = (
    ktxsDropShadow, { Unavailable now }
    ktxsRTLReading,
    ktxsNoPrefix,
    ktxsWordBreak,
    ktxsSingleLine,
    ktxsEndEllipsis,
    ktxsExpandTabs,
    ktxsWordEllipsis
  );

  TSeTextStyles = set of TSeTextStyle;

  { Text orientation }

  TSeTextOrientation = (
    ktxoHorizontal,
    ktxoVerticalTop,
    ktxoVerticalBottom
  );

  TSeTextInfo = record
    Rect: TRect;
    Text: string;
    Align: TSeTextAlign2;
    Style: TSeTextStyles;
    Orientation: TSeTextOrientation;
  end;

function TextInfo(const ARect: TRect; const AText: string; AAlign: TSeTextAlign2;
  AStyle: TSeTextStyles;
  AOrientation: TSeTextOrientation = ktxoHorizontal): TSeTextInfo; overload; forward;
function TextInfo(const ARect: TRect; const AText: string;
  DT_Flags: cardinal): TSeTextInfo; overload; forward;

{ Glyph types ================================================================}

type

  TSeGlyphAlign = (
    kgfaTopLeft,
    kgfaTopRight,
    kgfaBottomLeft,
    kgfaBottomRight,
    kgfaCenter
  );

  TSeGlyphInfo = record
    Rect: TRect;
    Glyph: TseBitmap;
    Align: TSeGlyphAlign;
  end;

function GlyphInfo: TSeGlyphInfo; overload; forward;
function GlyphInfo(const ARect: TRect; AGlyph: TseBitmap;
  AAlign: TSeGlyphAlign = kgfaCenter): TSeGlyphInfo; overload; forward;
function GlyphInfo(const ARect: TRect; Graphic: TGraphic; AAlign:
  TSeGlyphAlign = kgfaCenter): TSeGlyphInfo; overload; forward;
function GlyphInfo(const ARect: TRect; Images: TCustomImageList; ImageIndex: integer;
  AAlign: TSeGlyphAlign = kgfaCenter): TSeGlyphInfo; overload; forward;

{ Window class ================================================================}

type

  { Window subclasses }

  TSeWindowSubclass = (
    kwscStandard,
    kwscDialog,
    kwscBorder,
    kwscMessage,
    kwscNoBorder,
    kwscTextured,
    kwscToolWindow
  );

  TSeWindowDrawState = (
    kwdsActive,
    kwdsMaximized,
    kwdsMinimized,
    kwdsRolled,
    kwdsTopmost
  );

  TSeWindowDrawStates = set of TSeWindowDrawState;

  { HitTest }

  TSeWindowHitTest = (
    kwhtClient,
    kwhtCaption,
    kwhtLeft,
    kwhtTop,
    kwhtRight,
    kwhtBottom,
    kwhtBorder,
    kwhtTopLeft,
    kwhtTopRight,
    kwhtBottomLeft,
    kwhtBottomRight,
    kwhtCloseButton,
    kwhtHelpButton,
    kwhtMinButton,
    kwhtMaxButton,
    kwhtRollButton,
    kwhtTrayButton,
    kwhtSysMenu,
    kwhtNonClient,
    kwhtMenu
  );

  { Window button classes }




  TSeWindowInfo = record
    Rect: TRect;
    State: TSeWindowDrawStates;
    Buttons: TSeWindowButtons;
    ClipRect: TRect;
    Title: string;
    DisableNCArea: boolean;
  end;

  TSeWindowButtonInfo = record
    Rect: TRect;
    Button: TSeWindowButtonClass;
    DrawState: TSeWindowButtonDrawState;
    Icon: TseBitmap; { Only for SysMenu }
  end;

  TSeWindowGripperInfo = record
    Rect: TRect;
    Sizeable: boolean;
  end;

function WindowInfo(const ARect: TRect; AState: TSeWindowDrawStates;
  const AClipRect: TRect; AButtons: TSeWindowButtons = []; const ATitle: string = ''; ADisableNCArea: boolean = False): TSeWindowInfo; overload; forward;
function WindowButtonInfo(const ARect: TRect; AButton: TSeWindowButtonClass;
  ADrawState: TSeWindowButtonDrawState; AIcon: TseBitmap = nil): TSeWindowButtonInfo; overload; forward;
function WindowGripperInfo(const ARect: TRect; ASizeable: boolean): TSeWindowGripperInfo; overload; forward;

{ Hint class ==================================================================}

type

  { Hint subclasses }

  TSeHintSubclass = (
    khscHintWindow
  );

  TSeHintInfo = record
    Rect: TRect;
  end;

function HintInfo(const ARect: TRect): TSeHintInfo; overload; forward;

{ Label class =================================================================}

type

  { Subclasses }

  TSeLabelSubclass = (
    klscLabel,
    klscHyperlink,
    klscHighlight
  );

  TSeLabelDrawState = (
    kldsNormal,
    kldsHot,
    kldsDisabled
  );

  TSeLabelInfo = record
    DrawState: TSeLabelDrawState;
  end;

function LabelInfo(ADrawState: TSeLabelDrawState): TSeLabelInfo; overload; forward;

{ Button class ================================================================}

type

  { Button subclasses }

  TSeButtonSubclass = (
    kbscButton,
    kbscComboButton,
    kbscComboDropDown,
    kbscDropDown,
    kbscSpinUp,
    kbscSpinDown,
    kbscSpinLeft,
    kbscSpinRight,
    kbscSpinPlus,
    kbscSpinMinus
  );

  { Button draw states }

  TSeButtonDrawState = (
    kbdsNormal,
    kbdsHot,
    kbdsPressed,
    kbdsDisabled,
    kbdsFocused,
    kbdsFocusedHot,
    kbdsDefault,
    kbdsDefaultHot,
    kbdsChecked,
    kbdsCheckedDisabled,
    kbdsCheckedHot,
    kbdsCheckedFocused
  );

  TSeButtonInfo = record
    Rect: TRect;
    DrawState: TSeButtonDrawState;
  end;

function ButtonInfo(const ARect: TRect; ADrawState: TSeButtonDrawState): TSeButtonInfo; overload; forward;

{ Check class =================================================================}

type

  { Check subclass }

  TSeCheckSubclass = (
    kcscCheckBox,
    kcscRadioButton
  );

  { Check draw states }

  TSeCheckDrawState = (
    kcdsCheckedNormal,
    kcdsCheckedHot,
    kcdsCheckedPressed,
    kcdsCheckedDisabled,
    kcdsCheckedFocused,
    kcdsUncheckedNormal,
    kcdsUncheckedHot,
    kcdsUncheckedPressed,
    kcdsUncheckedDisabled,
    kcdsUncheckedFocused,
    kcdsMixedNormal,
    kcdsMixedHot,
    kcdsMixedPressed,
    kcdsMixedDisabled,
    kcdsMixedFocused
  );

  TSeCheckInfo = record
    Rect: TRect;
    DrawState: TSeCheckDrawState;
  end;

function CheckInfo(const ARect: TRect; ADrawState: TSeCheckDrawState): TSeCheckInfo; overload; forward;

{ Edit class ==================================================================}

type

  { Edit subclasses }

  TSeEditSubclass = (
    kescEdit,
    kescEditLeft, { +[edit] }
    kescEditRight, { [edit]+ }
    kescEditBoth, { +[edit]+ }
    kescComboBox,
    kescComboBoxLeft,
    kescComboBoxRight,
    { with our border }
    kescSingleEdit,
    kescSingleComboBox
  );

  { Edit draw state }

  TSeEditDrawState = (
    kedsNormal,
    kedsHot,
    kedsFocused,
    kedsFocusedHot,
    kedsDisabled,
    kedsReadOnly,
    kedsSelection
  );

  { Edit button classes }

  TSeEditButtonClass = (
    kebcDropDown,
    kebcUpDown, { for show menu }
    kebcUp,
    kebcDown,
    kebcLeft,
    kebcRight,
    kebcPlus,
    kebcMinus,
    kebcDialog,
    kebcCustom
  );

  { Edit button draw state }

  TSeEditButtonDrawState = (
    kebdsNormal,
    kebdsHot,
    kebdsPressed,
    kebdsFocused,
    kebdsDisabled
  );

  { EditInfo }

  TSeEditInfo = record
    Rect: TRect;
    DrawState: TSeEditDrawState;
  end;

  TSeEditButtonInfo = record
    Rect: TRect;
    DrawState: TSeEditButtonDrawState;
    ButtonClass: TSeEditButtonClass;
  end;

function EditInfo(const ARect: TRect; ADrawState: TSeEditDrawState): TSeEditInfo; overload; forward;
function EditButtonInfo(const ARect: TRect; ADrawState: TSeEditButtonDrawState;
  AButtonClass: TSeEditButtonClass): TSeEditButtonInfo; overload; forward;

{ Scroll class ================================================================}

type

  { Scroll subclasses }

  TSeScrollSubclass = (
    ksscScrollBar
  );

  { Scroll button classes }

  TSeScrollButtonClass = (
    ksbcUp,
    ksbcDown,
    ksbcPageUp,
    ksbcPageDown,
    ksbcLeft,
    ksbcRight,
    ksbcPageLeft,
    ksbcPageRight,
    ksbcHorzSlider,
    ksbcVertSlider,
    ksbcPlus,
    ksbcMinus,
    ksbcCustom
  );

  { Scroll button draw state }

  TSeScrollButtonDrawState = (
    ksbdsNormal,
    ksbdsPressed,
    ksbdsHot,
    ksbdsDisabled
  );

  { Scroll button area classes }

  TSeScrollAreaClass = (
    ksacVertical,
    ksacHorizontal
  );

  { Scroll area draw state }

  TSeScrollAreaDrawState = (
    ksadsNormal,
    ksadsPressed,
    ksadsHot,
    ksadsDisabled
  );

  { ScrollInfo }

  TSeScrollInfo = record
    Rect: TRect;
  end;

  TSeScrollButtonInfo = record
    Rect: TRect;
    Button: TSeScrollButtonClass;
    DrawState: TSeScrollButtonDrawState;
  end;

  TSeScrollAreaInfo = record
    Rect: TRect;
    Area: TSeScrollAreaClass;
    DrawState: TSeScrollAreaDrawState;
  end;

function ScrollInfo(const ARect: TRect): TSeScrollInfo; overload; forward;
function ScrollButtonInfo(const ARect: TRect; ADrawState: TSeScrollButtonDrawState;
  AButton: TSeScrollButtonClass): TSeScrollButtonInfo; overload; forward;
function ScrollAreaInfo(const ARect: TRect; ADrawState: TSeScrollAreaDrawState;
  AArea: TSeScrollAreaClass): TSeScrollAreaInfo; overload; forward;

{ Progress class ==============================================================}

type

  { Progress subclasses }

  TSeProgressSubclass = (
    kpscProgressBar,
    { no border }
    kpscSingleProgressBar
  );

  { Progress part }

  TSeProgressPart = (
    kppBarVert,
    kppBarHorz,
    kppChunkVert,
    kppChunkHorz,
    kppSolidVert,
    kppSolidHorz
  );

  { Progress draw state }

  TSeProgressDrawState = (
    kpdsNormal,
    kpdsDisabled
  );

  { ProgressInfo }

  TSeProgressInfo = record
    Rect: TRect;
    Part: TSeProgressPart;
    DrawState: TSeProgressDrawState;
  end;

function ProgressInfo(const ARect: TRect; APart: TSeProgressPart;
  ADrawState: TSeProgressDrawState): TSeProgressInfo; overload; forward;

{ Panel class =================================================================}

type

  { Panel subclass }

  TSePanelSubclass = (
    kpscPanel,
    kpscHeader
  );

  { Panel button classes }

  TSePanelButtonClass = (
    kpbcRollUp,
    kpbcRollDown,
    kpbcRollLeft,
    kpbcRollRight,
    kpbcClose,
    kpbcCustom
  );

  { Panel button draw state }

  TSePanelButtonDrawState = (
    kpbdsNormal,
    kpbdsHot,
    kpbdsPressed,
    kpbdsDiabled
  );

  { PanelInfo }
  
  TSePanelInfo = record
    Rect: TRect;
  end;

  TSePanelButtonInfo = record
    Rect: TRect;
    Button: TSePanelButtonClass;
    DrawState: TSePanelButtonDrawState;
  end;

function PanelInfo(const ARect: TRect): TSePanelInfo; overload; forward;
function PanelButtonInfo(const ARect: TRect; AButton: TSePanelButtonClass;
  ADrawState: TSePanelButtonDrawState): TSePanelButtonInfo; overload; forward;

{ Track class =================================================================}

type

  { Trackbar subclasses }

  TSeTrackSubclass = (
    ktscTrackBar,
    ktscRangeBar
  );

  { Track bar }

  TSeTrackBarClass = (
    ktbcVertical,
    ktbcHorizontal
  );

  TSeTrackBarDrawState = (
    ktrbdsNormal,
    ktrbdsDisabled
  );

  { Track thumb }

  TSeTrackThumbClass = (
    ktbmVertBoth,
    ktbmVertRight,
    ktbmVertLeft,
    ktbmHorzBoth,
    ktbmHorzBottom,
    ktbmHorzTop
  );

  TSeTrackThumbDrawState = (
    kttdsNormal,
    kttdsHot,
    kttdsPressed,
    kttdsFocused,
    kttdsDisabled
  );

  { TrackInfo }

  TSeTrackInfo = record
    Rect: TRect;
  end;

  TSeTrackBarInfo = record
    Rect: TRect;
    Bar: TSeTrackBarClass;
    DrawState: TSeTrackBarDrawState;
  end;

  TSeTrackThumbInfo = record
    Rect: TRect;
    Thumb: TSeTrackThumbClass;
    DrawState: TSeTrackThumbDrawState;
  end;

function TrackInfo(const ARect: TRect): TSeTrackInfo; overload; forward;
function TrackBarInfo(const ARect: TRect; ABar: TSeTrackBarClass;
  ADrawState: TSeTrackBarDrawState): TSeTrackBarInfo; overload; forward;
function TrackThumbInfo(const ARect: TRect; AThumb: TSeTrackThumbClass;
  ADrawState: TSeTrackThumbDrawState): TSeTrackThumbInfo; overload; forward;

{ Splitter class ==============================================================}

type

  TSeSplitterSubclass = (
    ksscSplitter
  );

  TSeSplitterInfo = record
    Rect: TRect;
  end;

  TSeSplitterButtonClass = (
    kspbcLeft,
    kspbcUp,
    kspbcDown,
    kspbcRight
  );

  TSeSplitterButtonInfo = record
    Rect: TRect;
    ButtonClass: TSeSplitterButtonClass;
  end;

function SplitterInfo(const ARect: TRect): TSeSplitterInfo; overload; forward;
function SplitterButtonInfo(const ARect: TRect; AButtonClass: TSeSplitterButtonClass): TSeSplitterButtonInfo; overload; forward;

{ Group class =================================================================}

type

  { Group subclasses }

  TSeGroupSubclass = (
    kgscGroupBox,
    kgscRadioGroup
  );

  { Group draw state }

  TSeGroupDrawState = (
    kgdsNormal,
    kgdsDisabled
  );

  { GroupInfo }

  TSeGroupInfo = record
    Rect: TRect;
    EraseRect: TRect;
    DrawState: TSeGroupDrawState;
  end;

function GroupInfo(ARect, AEraseRect: TRect; ADrawState: TSeGroupDrawState): TSeGroupInfo; overload; forward;

{ Memo class ==================================================================}

type

  { Memo subclasses }

  TSeMemoSubclass = (
    kmscMemo
  );

  { Memo draw state }

  TSeMemoDrawState = (
    kmdsNormal,
    kmdsHot,
    kmdsFocused,
    kmdsDisabled,
    kmdsReadOnly,
    kmdsSelection
  );

  { MemoInfo }

  TSeMemoInfo = record
    Rect: TRect;
    DrawState: TSeMemoDrawState;
  end;

  { Memo gutter }

  TSeMemoGutterInfo = record
    Rect: TRect;
  end;

function MemoInfo(const ARect: TRect; ADrawState: TSeMemoDrawState): TSeMemoInfo; overload; forward;
function MemoGutterInfo(const ARect: TRect): TSeMemoGutterInfo; overload; forward;

{ Memo class ==================================================================}

type

  { Box subclasses }

  TSeBoxSubclass = (
    kbscBox
  );

  TSeBoxDrawState = (
    kbxdsNormal,
    kbxdsDisabled
  );

  { BoxInfo }

  TSeBoxInfo = record
    Rect: TRect;
    DrawState: TSeBoxDrawState;
  end;

function BoxInfo(const ARect: TRect; ADrawState: TSeBoxDrawState): TSeBoxInfo; overload; forward;

{ Tab class ===================================================================}

type

  { Tab subclasses }

  TSeTabSubclass = (
    ktscTab,
    ktscPage
  );

  { Tab item (tab) }

  TSeTabItemClass = (
    ktisTopMiddle,
    ktisTopFirst,
    ktisTopLast,
    ktisLeftMiddle,
    ktisLeftFirst,
    ktisLeftLast,
    ktisRightMiddle,
    ktisRightFirst,
    ktisRightLast,
    ktisBottomMiddle,
    ktisBottomFirst,
    ktisBottomLast
  );

  TSeTabItemDrawState = (
    ktidsNormal,
    ktidsHot,
    ktidsDisabled,
    ktidsActive,
    ktidsActiveHot,
    ktidsActiveDisabled
  );

  { Tab button classes }

  TSeTabButtonClass = (
    ktbcClose,
    ktbcLeft,
    ktbcDown,
    ktbcUp,
    ktbcRight,
    ktbcCustom
  );

  TSeTabButtonDrawState = (
    ktbdsNormal,
    ktbdsHot,
    ktbdsPressed,
    ktbdsDisabled
  );

  TSeTabInfo = record
    Rect: TRect;
  end;

  TSeTabItemInfo = record
    Rect: TRect;
    Item: TSeTabItemClass;
    DrawState: TSeTabItemDrawState;
  end;

  TSeTabButtonInfo = record
    Rect: TRect;
    Button: TSeTabButtonClass;
    DrawState: TSeTabButtonDrawState;
  end;

function TabInfo(const ARect: TRect): TSeTabInfo; overload; forward;
function TabItemInfo(const ARect: TRect; AItem: TSeTabItemClass;
  ADrawState: TSeTabItemDrawState): TSeTabItemInfo; overload; forward;
function TabButtonInfo(const ARect: TRect; AButton: TSeTabButtonClass;
  ADrawState: TSeTabButtonDrawState): TSeTabButtonInfo; overload; forward;

{ Header class ================================================================}

type

  { Header subclasses }

  TSeHeaderSubclass = (
    khscHeader
  );

  { Header section state }

  TSeSectionState = (
    khssFirst,
    khssMiddle,
    khssLast
  );

  TSeSectionDrawState = (
    khdsNormal,
    khdsPressed,
    khdsDraggedOut,
    khdsDragging,
    khdsHot,
    khdsUnderDrag,
    khdsDisabled
  );

  TSeHeaderInfo = record
    Rect: TRect;
  end;

  TSeHeaderSectionInfo = record
    Rect: TRect;
    State: TSeSectionState;
    DrawState: TSeSectionDrawState;
  end;

function HeaderInfo(const ARect: TRect): TSeHeaderInfo; overload; forward;
function HeaderSectionInfo(const ARect: TRect; AState: TSeSectionState;
  ADrawState: TSeSectionDrawState): TSeHeaderSectionInfo; overload; forward;

{ Status class ================================================================}

type

  { Status subclasses }

  TSeStatusSubclass = (
    ksscStatusBar
  );

  TSeStatusPanelClass = (
    kspcNormal,
    kspcDisabled,
    kspcGripperPanel,
    kspcGripper
  );

  { StatusInfo }

  TSeStatusInfo = record
    Rect: TRect;
  end;

  TSeStatusPanelInfo = record
    Rect: TRect;
    Panel: TSeStatusPanelClass;
  end;

function StatusInfo(const ARect: TRect): TSeStatusInfo; overload; forward;
function StatusPanelInfo(const ARect: TRect; APanel: TSeStatusPanelClass): TSeStatusPanelInfo; overload; forward;

{ BarDock class ===============================================================}

type

  TSeBarDockSubclass = (
    kbdscBarDock
  );

  TSeBarDockInfo = record
    Rect: TRect;
  end;

function BarDockInfo(const ARect: TRect): TSeBarDockInfo; overload; forward;

{ Bar class ===================================================================}

type

  { Bar subclasses }

  TSeBarSubclass = (
    kbscOutlookBar,
    kbscExplorerBar,
    kbscGroupBar
  );

  TSeBarCaptionDrawState = (
    kbcdsNormal,
    kbcdsHot,
    kbcdsPressed,
    kbcdsDisabled,
    kbcdsRolled
  );

  TSeBarButtonClass = (
    kbbcText,
    kbbcUp,
    kbbcDown,
    kbbcClose
  );

  TSeBarButtonDrawState = (
    kbbdsNormal,
    kbbdsHot,
    kbbdsPressed,
    kbbdsDisabled
  );

  TSeBarItemClass = (
    kbicNormal
  );

  TSeBarItemDrawState = (
    kbidsNormal,
    kbidsHot,
    kbidsPressed,
    kbidsDisabled
  );

  { BarInfo }

  TSeBarInfo = record
    Rect: TRect;
    HaveCaption: boolean;
    HaveBorder: boolean;
  end;

  TSeBarCaptionInfo = record
    Rect: TRect;
    DrawState: TSeBarCaptionDrawState;
  end;

  TSeBarButtonInfo = record
    Rect: TRect;
    Button: TSeBarButtonClass;
    DrawState: TSeBarButtonDrawState;
  end;

  TSeBarItemInfo = record
    Rect: TRect;
    Item: TSeBarItemClass;
    DrawState: TSeBarItemDrawState;
  end;

function BarInfo(const ARect: TRect; HaveBorder, HaveCaption: boolean): TSeBarInfo; overload; forward;
function BarCaptionInfo(const ARect: TRect; const AText: string): TSeBarCaptionInfo; overload; forward;
function BarButtonInfo(const ARect: TRect; AButton: TSeBarButtonClass;
  ADrawState: TSeBarButtonDrawState): TSeBarButtonInfo; overload; forward;
function BarItemInfo(const ARect: TRect; AItem: TSeBarItemClass;
  ADrawState: TSeBarItemDrawState): TSeBarItemInfo; overload; forward;

{ Menu class ==================================================================}

type

  { Menu subclass }

  TSeMenuSubclass = (
    kmscMenuBar,
    kmscPopupMenu
  );

  { Menu item classes }

  TSeMenuItemClass = (
    kmicNormal,
    kmicSeparator,
    kmicSpace,
    kmicHomeButton,
    kmicEndButton,
    kmicExpandButton,
    kmicSysMenu,
    kmicMin,
    kmicRestore,
    kmicClose
  );

  TSeMenuItemDrawState = (
    kmidsNormal,
    kmidsHot,
    kmidsSelected,
    kmidsDisabled,
    kmidsUnused
  );

  TSeMenuGlyph = (
    kmgCheckGlyph,
    kmgRadioGlyph,
    kmgSubMenuGlyph,
    kmgGlyph,
    { for SysMenu }
    kmgSysMenu,
    kmgClose,
    kmgMin,
    kmgMax,
    kmgRoll,
    kmgMinRestore,
    kmgMaxRestore,
    kmgRollRestore,
    kmgTray,
    kmgNone
  );

  TSeMenuInfo = record
    Rect: TRect;
  end;

  TSeMenuItemInfo = record
    Rect: TRect;
    Item: TSeMenuItemClass;
    DrawState: TSeMenuItemDrawState;
  end;

  TSeMenuGlyphInfo = record
    Rect: TRect;
    Glyph: TseBitmap;
    Align: TSeGlyphAlign;
    Kind: TSeMenuGlyph;
  end;

function MenuInfo(const ARect: TRect): TSeMenuInfo; overload; forward;
function MenuItemInfo(const ARect: TRect; AItem: TSeMenuItemClass;
  ADrawState: TSeMenuItemDrawState): TSeMenuItemInfo; overload; forward;

function MenuGlyphInfo: TSeMenuGlyphInfo; overload; forward;
function MenuGlyphInfo(const ARect: TRect; AGlyph: TseBitmap;
  AAlign: TSeGlyphAlign; AKind: TSeMenuGlyph): TSeMenuGlyphInfo; overload; forward;
function MenuGlyphInfo(const ARect: TRect; AKind: TSeMenuGlyph): TSeMenuGlyphInfo; overload; forward;
function MenuGlyphInfo(const ARect: TRect; Graphic: TGraphic; AAlign:
  TSeGlyphAlign = kgfaCenter): TSeMenuGlyphInfo; overload; forward;
function MenuGlyphInfo(const ARect: TRect; Images: TCustomImageList; ImageIndex: integer;
  AAlign: TSeGlyphAlign = kgfaCenter; AEnabled: boolean = True): TSeMenuGlyphInfo; overload; forward;

{ Dock class ==================================================================}

type

  { Dock subclass }

  TSeDockSubclass = (
    kdscDock
  );

  TSeDockInfo = record
    Rect: TRect;
  end;

function DockInfo(const ARect: TRect): TSeDockInfo; overload; forward;

{ Tool class ==================================================================}

type

  { Tool subclass }

  TSeToolSubclass = (
    ktscToolMenu,
    ktscToolBar,
    ktscToolPopup,
    ktscToolWindow,
    ktscRebar
  );

  TSeToolButtonClass = (
    ktlbcCaption,
    ktlbcGripper,
    ktlbcClose,
    ktlbcPick
  );

  TSeToolInfo = record
    Rect: TRect;
  end;

  { Tool Item }

  TSeToolItemClass = (
    kticNormal,
    kticSeparator,
    kticSpace,
    kticComboButton,
    kticComboDropDown,
    kticDropDown,
    kticHomeButton,
    kticendButton,
    kticExpandButton,
    kticSysMenu,
    kticMin,
    kticRestore,
    kticClose,
    kticGripper,
    kticGripperVert
  );

  TSeToolItemDrawState = (
    mtidsNormal,
    mtidsHot,
    mtidsPressed,
    mtidsDisabled,
    mtidsUnused,
    mtidsChecked,
    mtidsCheckedHot,
    mtidsCheckedPressed
  );

  TSeToolGlyph = (
    ktigCheckGlyph,
    ktigRadioGlyph,
    ktigSubMenuGlyph,
    ktigGlyph,
    { for SysMenu }
    ktigSysMenu,
    ktigClose,
    ktigMin,
    ktigMax,
    ktigRoll,
    ktigMinRestore,
    ktigMaxRestore,
    ktigRollRestore,
    ktigTray,
    ktgNone
  );

  TSeToolItemInfo = record
    Rect: TRect;
    Item: TSeToolItemClass;
    DrawState: TSeToolItemDrawState;
  end;

  TSeToolGlyphInfo = record
    Rect: TRect;
    Glyph: TseBitmap;
    Align: TSeGlyphAlign;
    Kind: TSeToolGlyph;
  end;

function ToolInfo(const ARect: TRect): TSeToolInfo; overload; forward;

function ToolItemInfo(const ARect: TRect; AItem: TSeToolItemClass;
  ADrawState: TSeToolItemDrawState): TSeToolItemInfo; overload; forward;

function ToolGlyphInfo: TSeToolGlyphInfo; overload; forward;
function ToolGlyphInfo(const ARect: TRect; AGlyph: TseBitmap;
  AAlign: TSeGlyphAlign; AKind: TSeToolGlyph): TSeToolGlyphInfo; overload; forward;
function ToolGlyphInfo(const ARect: TRect; AKind: TSeToolGlyph): TSeToolGlyphInfo; overload; forward;
function ToolGlyphInfo(const ARect: TRect; Graphic: TGraphic; AAlign:
  TSeGlyphAlign = kgfaCenter): TSeToolGlyphInfo; overload; forward;
function ToolGlyphInfo(const ARect: TRect; Images: TCustomImageList; ImageIndex: integer;
  AAlign: TSeGlyphAlign = kgfaCenter; AEnabled: boolean = True): TSeToolGlyphInfo; overload; forward;

{ List class ==================================================================}

type

  TSeListSubclass = (
    klscListBox
  );

  TSeListItemDrawState = (
    klidsNormal,
    klidsHot,
    klidsSelected,
    klidsFocused,
    klidsDisabled
  );

  TSeListGlyph = (
    klgChecked,
    klgUnchecked,
    klgMixed,
    klgCustom
  );

  TSeListInfo = record
    Rect: TRect;
  end;

  TSeListColumnInfo = record
    Rect: TRect;
  end;

  TSeListItemInfo = record
    Rect: TRect;
    DrawState: TSeListItemDrawState;
  end;

  TSeListGlyphInfo = record
    Rect: TRect;
    Align: TSeGlyphAlign;
    Glyph: TseBitmap;
    Kind: TSeListGlyph;
  end;

function ListInfo(const ARect: TRect): TSeListInfo; overload; forward;
function ListColumnInfo(const ARect: TRect): TSeListColumnInfo; overload; forward;
function ListItemInfo(const ARect: TRect; ADrawState: TSeListItemDrawState): TSeListItemInfo; overload; forward;

function ListGlyphInfo(const ARect: TRect; AGlyph: TseBitmap;
  AAlign: TSeGlyphAlign; AKind: TSeListGlyph): TSeListGlyphInfo; overload; forward;
function ListGlyphInfo(const ARect: TRect; AKind: TSeListGlyph): TSeListGlyphInfo; overload; forward;
function ListGlyphInfo(const ARect: TRect; Graphic: TGraphic; AAlign:
  TSeGlyphAlign = kgfaCenter): TSeListGlyphInfo; overload; forward;
function ListGlyphInfo(const ARect: TRect; Images: TCustomImageList; ImageIndex: integer;
  AAlign: TSeGlyphAlign = kgfaCenter): TSeListGlyphInfo; overload; forward;

{ Tree class ==================================================================}

type

  TSeTreeSubclass = (
    ktscTreeList
  );

  TSeTreeItemDrawState = (
    kridsNormal,
    kridsHot,
    kridsSelected,
    kridsFocused,
    kridsDisabled
  );

  TSeTreeInfo = record
    Rect: TRect;
  end;

  TSeTreeItemInfo = record
    Rect: TRect;
    DrawState: TSeTreeItemDrawState;
  end;

function TreeInfo(const ARect: TRect): TSeTreeInfo; overload; forward;
function TreeItemInfo(const ARect: TRect; ADrawState: TSeTreeItemDrawState): TSeTreeItemInfo; overload; forward;

{ Grid class ==================================================================}

type

  TSeGridSubclass = (
    kgscGrid
  );

  TSeGridItemClass = (
    kgicNormal,
    kgicFixed
  );

  TSeGridItemDrawState = (
    kgidsNormal,
    kgidsHot,
    kgidsSelected,
    kgidsFocused,
    kgidsDisabled
  );

  TSeGridInfo = record
    Rect: TRect;
  end;

  TSeGridItemInfo = record
    Rect: TRect;
    DrawState: TSeGridItemDrawState;
    Item: TSeGridItemClass;
  end;

function GridInfo(const ARect: TRect): TSeGridInfo; overload; forward;
function GridItemInfo(const ARect: TRect; ADrawState: TSeGridItemDrawState;
  AItem: TSeGridItemClass): TSeGridItemInfo; overload; forward;

{ Convertion }

function WindowButtonToHitTest(WindowButton: TSeWindowButtonClass): TSeWindowHitTest; forward;

function GraphicToBitmap(Graphic: TGraphic): TseBitmap; forward;
function ImagesToBitmap(Images: TCustomImageList; ImageIndex: integer; Enabled: boolean = True): TseBitmap; overload; forward;



const
  StyleColorNames: array [TSeStyleColor] of string = (
    'Border',
    'CategoryButtons',
    'CategoryPanelGroup',
    'ComboBox',
    'ComboBoxDisabled',
    'ButtonNormal',
    'ButtonHot',
    'ButtonPressed',
    'ButtonFocused',
    'ButtonDisabled',
    'Edit',
    'EditDisabled',
    'Grid',
    'GenericGradientBase',
    'GenericGradientEnd',
    'HintGradientBase',
    'ListBox',
    'ListBoxDisabled',
    'ListView',
    'Panel',
    'PanelDisabled',
    'TreeView',
    'Window',
    'Splitter',
    'CategoryButtonsGradientBase',
    'CategoryButtonsGradientEnd',
    'ToolBarGradientBase',
    'ToolBarGradientEnd',
    'GenericBackground',
    'HintGradientEnd',
    'FocusEffect',
    'AlternatingRowBackground'
  );

  StyleFontNames: array [TSeStyleFont] of string = (
    'CaptionTextNormal',
    'CaptionTextInactive',
    'SmCaptionTextNormal',
    'SmCaptionTextInactive',
    'StaticTextNormal',
    'StaticTextHot',
    'StaticTextFocused',
    'StaticTextDisabled',
    'PanelTextNormal',
    'PanelTextDisabled',
    'ButtonTextNormal',
    'ButtonTextPressed',
    'ButtonTextHot',
    'ButtonTextFocused',
    'ButtonTextDisabled',
    'CheckBoxTextNormal',
    'CheckBoxTextPressed',
    'CheckBoxTextHot',
    'CheckBoxTextFocused',
    'CheckBoxTextDisabled',
    'RadioButtonTextNormal',
    'RadioButtonTextPressed',
    'RadioButtonTextHot',
    'RadioButtonTextFocused',
    'RadioButtonTextDisabled',
    'GroupBoxTextNormal',
    'GroupBoxTextDisabled',
    'WindowTextNormal',
    'WindowTextDisabled',
    'EditBoxTextNormal',
    'EditBoxTextFocused',
    'EditBoxTextHot',
    'EditBoxTextDisabled',
    'EditBoxTextSelected',
    'MenuItemTextNormal',
    'MenuItemTextSelected',
    'MenuItemTextHot',
    'MenuItemTextDisabled',
    'ToolItemTextNormal',
    'ToolItemTextSelected',
    'ToolItemTextHot',
    'ToolItemTextDisabled',
    'HeaderSectionTextNormal',
    'HeaderSectionTextPressed',
    'HeaderSectionTextDraggedOut',
    'HeaderSectionTextDragging',
    'HeaderSectionTextHot',
    'HeaderSectionTextUnderDrag',
    'HeaderSectionTextDisabled',
    'StatusPanelTextNormal',
    'StatusPanelTextDisabled',
    'TabTextInactiveNormal',
    'TabTextInactiveHot',
    'TabTextInactiveDisabled',
    'TabTextActiveNormal',
    'TabTextActiveHot',
    'TabTextActiveDisabled',
    'ListItemTextNormal',
    'ListItemTextHot',
    'ListItemTextSelected',
    'ListItemTextFocused',
    'ListItemTextDisabled',
    'PopupMenuItemTextNormal',
    'PopupMenuItemTextSelected',
    'PopupMenuItemTextHot',
    'PopupMenuItemTextDisabled',
     // Added
    'CatgeoryButtonsNormal',
    'CatgeoryButtonsHot',
    'CatgeoryButtonsSelected',
    'CatgeoryButtonsCategoryNormal',
    'CatgeoryButtonsCategorySelected',
    'CategoryPanelGroupHeaderNormal',
    'CategoryPanelGroupHeaderHot',
    'ComboBoxItemNormal',
    'ComboBoxItemFocused',
    'ComboBoxItemHot',
    'ComboBoxItemDisabled',
    'ComboBoxItemSelected',
    'GridItemNormal',
    'GridItemSelected',
    'GridItemFixedNormal',
    'GridItemFixedHot',
    'GridItemFixedPressed',
    'TreeItemTextNormal',
    'TreeItemTextHot',
    'TreeItemTextSelected',
    'TreeItemTextFocused',
    'TreeItemTextDisabled',
    'FMBoxTextNormal',
    'FMBoxTextPressed',
    'FMBoxTextHot',
    'FMBoxTextFocused',
    'FMBoxTextDisabled'
    );


procedure SetFont(AFont: TFont; const AFontName: string; AFontSize: Integer;
  AFontStyle: TFontStyles; AFontColor: TColor); overload; forward;

function StringToFontName(const Str: string): string; forward;
function StringToFontSize(const Str: string): Integer; forward;
function StringToFontColor(const Str: string): TColor; forward;
function StringToFontCharset(const Str: string): TFontCharset; forward;
function StringToFontStyle(const Str: string): TFontStyles; forward;


type

{ Style API drawing class }

  TSeStyleDrawing = class(TPersistent)
  private
    FStyle: TPersistent;
  public
    constructor Create(AStyle: TPersistent); virtual;
  end;


type

  TktlMargin = record
    Left, Top, right, Bottom: integer;
  end;

  TktlMarginId = (
    ktmiWindowStandard,
    ktmiWindowDialog,
    ktmiWindowBorder,
    ktmiWindowMessage,
    ktmiWindowTextured,
    ktmiWindowToolWindow,
    ktmiWindowToolbarVert,
    ktmiWindowToolbarHorz
  );

{ Style API metrics class }

  TSeStyleMetrics = class(TPersistent)
  private
    FStyle: TPersistent;
    function GetMargin(MarginId: TktlMarginId): TktlMargin;
  public
    constructor Create(AStyle: TPersistent); virtual;
    { access properties }
    property Margins[MarginId: TktlMarginId]: TktlMargin read GetMargin;
  end;

function Margin(ALeft, ATop, ARight, ABottom: integer): TktlMargin; forward;


type

  { Style options }

  TSeStyleOption = (
    ktoptOneEdit,
    ktoptOneComboBox
  );

const

  StyleOptionNames: array [TSeStyleOption] of string = (
    'One object for kescEditButtonLeft, kescEditButtonRight and kescEditButtonBoth',
    'One object for kescComboBoxLeft, kescComboBoxRight'
  );

type

  TSeStyleOptions = class(TPersistent)
  private
    FOptions: array [TSeStyleOption] of boolean;
    FStyle: TPersistent;
    function GetOptions(index: TSeStyleOption): boolean;
    procedure SetOptions(index: TSeStyleOption; const Value: boolean);
  public
    constructor Create(AStyle: TPersistent); virtual;
    procedure Assign(Source: TPersistent); override;
    { I/O }
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure SaveToStream(Stream: TStream); virtual;
    property Options[index: TSeStyleOption]: boolean read GetOptions write SetOptions; default;
  end;



type

  { Style config }

  TSeStyleConfig = record
    Building: boolean;
    CanStore: boolean;
    CanLoad: boolean;
    DialogFilter: string;
    DefaultExt: string;
  end;

  { TSeStyle class - abstract Style declaration}

  { TSeCustomStyleLink class }

  TSeCustomStyle = class;

  TSeCustomStyleLink = class(TComponent)
  private
  protected
    function GetStyle: TSeCustomStyle; virtual; abstract;
  public
    property Style: TSeCustomStyle read GetStyle;
  published
  end;

  TSeCustomStyle = class(TPersistent)
  private
    FColors: TSeStyleColors;
    FFonts: TSeStyleFonts;
    FName, FStyleFileName: string;
    FSysColors: TSeStyleSysColors;
    FDrawing: TSeStyleDrawing;
    FMetrics: TSeStyleMetrics;
    FOptions: TSeStyleOptions;
  protected
    procedure ResetStyleColors; virtual;
    procedure ResetStyleFonts; virtual;
    procedure SetFontForObject(AFont: TFont; const Font: TSeStyleFont;
      const AObject: TSeStyleElementObject = ktoDefault); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    { }
    procedure Assign(Source: TPersistent); override;
    { Style class name and options }
    class function GetName: string; virtual;
    class function GetStyleConfig: TSeStyleConfig; virtual;
    { I/O }
    class function CheckStream(Stream: TStream): boolean; virtual;
    function LoadFromStream(Stream: TStream): boolean; virtual;
    function SaveToStream(Stream: TStream): boolean; virtual;
    function LoadFromFile(const FileName: string): boolean; virtual;
    function SaveToFile(const FileName: string): boolean; virtual;
    { Reset theme }
    procedure ResetStyle; virtual;
    { Drawing }
    procedure DrawStyleText(Canvas: TCanvas; Text: TSeTextInfo; ADPI: Integer = 0); virtual;
    { Window class }
    function IsWindowDefined(const ASubclass: TSeWindowSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; virtual;
    function WindowGetRegion(const ASubclass: TSeWindowSubclass; const ARect: TRect;
      const AObject: TSeStyleElementObject = ktoDefault): TSeRegion; virtual;
    function WindowGetHitTest(const ASubclass: TSeWindowSubclass; AWindow: TSeWindowInfo;
      X, Y: integer; ADPI: Integer = 0; const AObject: TSeStyleElementObject = ktoDefault): TSeWindowHitTest; virtual;
    function WindowGetClientRect(const ASubclass: TSeWindowSubclass;
      const ARect: TRect; ADPI: Integer = 0; const AObject: TSeStyleElementObject = ktoDefault): TRect; virtual;
    function WindowGetTitleRect(const ASubclass: TSeWindowSubclass;
      AWindow: TSeWindowInfo; ADPI: Integer = 0; const AObject: TSeStyleElementObject = ktoDefault): TRect; virtual;
    function WindowGetButtonRect(const ASubclass: TSeWindowSubclass;
      AWindow: TSeWindowInfo; AButton: TSeWindowButtonClass; ADPI: Integer = 0;
      const AObject: TSeStyleElementObject = ktoDefault): TRect; virtual;
    function WindowGetFixPosition(const ASubclass: TSeWindowSubclass;
       AButton: TSeWindowButtonClass; ADPI: Integer = 0;
      const AObject: TSeStyleElementObject = ktoDefault): Boolean; virtual;
    procedure WindowDraw(const ASubclass: TSeWindowSubclass; Canvas: TCanvas;
      AWindow: TSeWindowInfo; ADPI: Integer = 0; const AObject: TSeStyleElementObject = ktoDefault); virtual;
    procedure WindowDrawClient(const ASubclass: TSeWindowSubclass; Canvas: TCanvas;
      const ARect: TRect; ADPI: Integer = 0; const AObject: TSeStyleElementObject = ktoDefault); virtual;
    procedure WindowDrawText(const ASubclass: TSeWindowSubclass; Canvas: TCanvas;
      AWindow: TSeWindowInfo; const ARect: TRect; RightToLeftReading: Boolean; ADPI: Integer = 0;
      const AObject: TSeStyleElementObject = ktoDefault); virtual;
    procedure WindowDrawButton(const ASubclass: TSeWindowSubclass; Canvas: TCanvas;
      AButton: TSeWindowButtonInfo; ADPI: Integer = 0; const AObject: TSeStyleElementObject = ktoDefault); virtual;
    procedure WindowDrawGripper(const ASubclass: TSeWindowSubclass; Canvas: TCanvas;
      AGripper: TSeWindowGripperInfo; ADPI: Integer = 0; const AObject: TSeStyleElementObject = ktoDefault); virtual;
    { Hint class }
    function IsHintDefined(const ASubclass: TSeHintSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; virtual;
    function HintGetClientRect(const ASubclass: TSeHintSubclass; Canvas: TCanvas;
      AHint: TSeHintInfo; const AObject: TSeStyleElementObject = ktoDefault): TRect; virtual;
    procedure HintDraw(const ASubclass: TSeHintSubclass; Canvas: TCanvas;
      AHint: TSeHintInfo; const AObject: TSeStyleElementObject = ktoDefault); virtual;
    { Label class }
    function IsLabelDefined(const ASubclass: TSeLabelSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; virtual;
    function IsLabelTransparent(const ASubclass: TSeLabelSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; virtual;
    procedure LabelDrawText(const ASubclass: TSeLabelSubclass; Canvas: TCanvas;
      ALabel: TSeLabelInfo; AText: TSeTextInfo; const AObject: TSeStyleElementObject = ktoDefault); virtual;
    { Panel class }
    function IsPanelDefined(const ASubclass: TSePanelSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; virtual;
    function IsPanelTransparent(const ASubclass: TSePanelSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; virtual;
    function PanelGetClientRect(const ASubclass: TSePanelSubclass;
      APanel: TSePanelInfo; const AObject: TSeStyleElementObject = ktoDefault): TRect; virtual;
    procedure PanelDraw(const ASubclass: TSePanelSubclass; Canvas: TCanvas;
      APanel: TSePanelInfo; const AObject: TSeStyleElementObject = ktoDefault); virtual;
    procedure PanelDrawButton(const ASubclass: TSePanelSubclass; Canvas: TCanvas;
      AButton: TSePanelButtonInfo; const AObject: TSeStyleElementObject = ktoDefault); virtual;
    procedure PanelDrawText(const ASubclass: TSePanelSubclass; Canvas: TCanvas;
      APanel: TSePanelInfo; AText: TSeTextInfo;
      const AObject: TSeStyleElementObject = ktoDefault); virtual;
    { Button class }
    function IsButtonDefined(const ASubclass: TSeButtonSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; virtual;
    function IsButtonTransparent(const ASubclass: TSeButtonSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; virtual;
    procedure ButtonDraw(const ASubclass: TSeButtonSubclass; Canvas: TCanvas;
      AButton: TSeButtonInfo; ADPI: Integer = 0; const AObject: TSeStyleElementObject = ktoDefault); virtual;
    procedure ButtonDrawText(const ASubclass: TSeButtonSubclass; Canvas: TCanvas;
      AButton: TSeButtonInfo; AText: TSeTextInfo;
      const AObject: TSeStyleElementObject = ktoDefault); virtual;
    procedure ButtonDrawGlyph(const ASubclass: TSeButtonSubclass; Canvas: TCanvas;
      AButton: TSeButtonInfo; AGlyph: TSeGlyphInfo;
      const AObject: TSeStyleElementObject = ktoDefault); virtual;
    { Checkbox class }
    function IsCheckDefined(const ASubclass: TSeCheckSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; virtual;
    function IsCheckTransparent(const ASubclass: TSeCheckSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; virtual;
    procedure CheckDraw(const ASubclass: TSeCheckSubclass; Canvas: TCanvas;
      ACheck: TSeCheckInfo; ADPI: Integer = 0; const AObject: TSeStyleElementObject = ktoDefault); virtual;
    procedure CheckDrawText(const ASubclass: TSeCheckSubclass; Canvas: TCanvas;
      ACheck: TSeCheckInfo; AText: TSeTextInfo;
      const AObject: TSeStyleElementObject = ktoDefault); virtual;
    function CheckGetSize(const ASubclass: TSeCheckSubclass; ADPI: Integer = 0;
      const AObject: TSeStyleElementObject = ktoDefault): TPoint; virtual;
    { Splitter class }
    function IsSplitterDefined(const ASubclass: TSeSplitterSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; virtual;
    function IsSplitterTransparent(const ASubclass: TSeSplitterSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; virtual;
    procedure SplitterDraw(const ASubclass: TSeSplitterSubclass; Canvas: TCanvas;
      ASplitter: TSeSplitterInfo;
      const AObject: TSeStyleElementObject = ktoDefault); virtual;
    { Scroll class }
    function IsScrollDefined(const ASubclass: TSeScrollSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; virtual;
    function IsScrollTransparent(const ASubclass: TSeScrollSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; virtual;
    procedure ScrollDraw(const ASubclass: TSeScrollSubclass; Canvas: TCanvas;
      AScroll: TSeScrollInfo; ADPI: Integer = 0; const AObject: TSeStyleElementObject = ktoDefault); virtual;
    procedure ScrollDrawButton(const ASubclass: TSeScrollSubclass; Canvas: TCanvas;
      AButton: TSeScrollButtonInfo; ADPI: Integer = 0;
      const AObject: TSeStyleElementObject = ktoDefault); virtual;
    procedure ScrollDrawArea(const ASubclass: TSeScrollSubclass; Canvas: TCanvas;
      AArea: TSeScrollAreaInfo; ADPI: Integer = 0;
      const AObject: TSeStyleElementObject = ktoDefault); virtual;
    { Progress class }
    function IsProgressDefined(const ASubclass: TSeProgressSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; virtual;
    function IsProgressTransparent(const ASubclass: TSeProgressSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; virtual;
    function ProgressGetClientRect(const ASubclass: TSeProgressSubclass;
      AProgress: TSeProgressInfo; const AObject: TSeStyleElementObject = ktoDefault): TRect; virtual;
    procedure ProgressDraw(const ASubclass: TSeProgressSubclass; Canvas: TCanvas;
      AProgress: TSeProgressInfo; const AObject: TSeStyleElementObject = ktoDefault); virtual;
    { Track class }
    function IsTrackDefined(const ASubclass: TSeTrackSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; virtual;
    function IsTrackTransparent(const ASubclass: TSeTrackSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; virtual;
    procedure TrackDraw(const ASubclass: TSeTrackSubclass; Canvas: TCanvas;
      ATrack: TSeTrackInfo; ADPI: Integer = 0; const AObject: TSeStyleElementObject = ktoDefault); virtual;
    procedure TrackBarDraw(const ASubclass: TSeTrackSubclass; Canvas: TCanvas;
      ABar: TSeTrackBarInfo; ADPI: Integer = 0;  const AObject: TSeStyleElementObject = ktoDefault); virtual;
    procedure TrackDrawThumb(const ASubclass: TSeTrackSubclass; Canvas: TCanvas;
      AThumb: TSeTrackThumbInfo; ADPI: Integer = 0;
      const AObject: TSeStyleElementObject = ktoDefault); virtual;
    function TrackGetThumbSize(const ASubclass: TSeTrackSubclass; Canvas: TCanvas;
      AThumb: TSeTrackThumbInfo; ADPI: Integer = 0;
      const AObject: TSeStyleElementObject = ktoDefault): TPoint; virtual;
    { Header class }
    function IsHeaderDefined(const ASubclass: TSeHeaderSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; virtual;
    function IsHeaderTransparent(const ASubclass: TSeHeaderSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; virtual;
    procedure HeaderDraw(const ASubclass: TSeHeaderSubclass; Canvas: TCanvas;
      AHeader: TSeHeaderInfo; const AObject: TSeStyleElementObject = ktoDefault); virtual;
    procedure HeaderDrawSection(const ASubclass: TSeHeaderSubclass; Canvas: TCanvas;
      ASection: TSeHeaderSectionInfo; const AObject: TSeStyleElementObject = ktoDefault); virtual;
    procedure HeaderDrawText(const ASubclass: TSeHeaderSubclass; Canvas: TCanvas;
      ASection: TSeHeaderSectionInfo; AText: TSeTextInfo; const AObject: TSeStyleElementObject = ktoDefault); virtual;
    procedure HeaderDrawGlyph(const ASubclass: TSeHeaderSubclass; Canvas: TCanvas;
      ASection: TSeHeaderSectionInfo; AGlyph: TSeGlyphInfo; const AObject: TSeStyleElementObject = ktoDefault); virtual;
    { Status class }
    function IsStatusDefined(const ASubclass: TSeStatusSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; virtual;
    function IsStatusTransparent(const ASubclass: TSeStatusSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; virtual;
    function StatusGetClientRect(const ASubclass: TSeStatusSubclass;
      AStatus: TSeStatusInfo; const AObject: TSeStyleElementObject = ktoDefault): TRect; virtual;
    procedure StatusDraw(const ASubclass: TSeStatusSubclass; Canvas: TCanvas;
      AStatus: TSeStatusInfo; ADPI: Integer = 0; const AObject: TSeStyleElementObject = ktoDefault); virtual;
    procedure StatusDrawPanel(const ASubclass: TSeStatusSubclass; Canvas: TCanvas;
      APanel: TSeStatusPanelInfo; ADPI: Integer = 0; const AObject: TSeStyleElementObject = ktoDefault); virtual;
    procedure StatusDrawText(const ASubclass: TSeStatusSubclass; Canvas: TCanvas;
      APanel: TSeStatusPanelInfo; AText: TSeTextInfo; const AObject: TSeStyleElementObject = ktoDefault); virtual;
    procedure StatusDrawGlyph(const ASubclass: TSeStatusSubclass; Canvas: TCanvas;
      APanel: TSeStatusPanelInfo; AGlyph: TSeGlyphInfo; const AObject: TSeStyleElementObject = ktoDefault); virtual;
    { Edit class }
    function IsEditDefined(const ASubclass: TSeEditSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; virtual;
    function IsEditTransparent(const ASubclass: TSeEditSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; virtual;
    function EditGetClientRect(const ASubclass: TSeEditSubclass; Canvas: TCanvas;
      AEdit: TSeEditInfo; const AObject: TSeStyleElementObject = ktoDefault): TRect; virtual;
    procedure EditDraw(const ASubclass: TSeEditSubclass; Canvas: TCanvas;
      AEdit: TSeEditInfo; const AObject: TSeStyleElementObject = ktoDefault); virtual;
    procedure EditDrawButton(const ASubclass: TSeEditSubclass; Canvas: TCanvas;
      AButton: TSeEditButtonInfo; ADPI: Integer = 0; const AObject: TSeStyleElementObject = ktoDefault); virtual;
    { Group class }
    function IsGroupDefined(const ASubclass: TSeGroupSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; virtual;
    function IsGroupTransparent(const ASubclass: TSeGroupSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; virtual;
    function GroupGetClientRect(const ASubclass: TSeGroupSubclass;
      AGroup: TSeGroupInfo; const AObject: TSeStyleElementObject = ktoDefault): TRect; virtual;
    procedure GroupDraw(const ASubclass: TSeGroupSubclass; Canvas: TCanvas;
      AGroup: TSeGroupInfo; const AObject: TSeStyleElementObject = ktoDefault); virtual;
    procedure GroupDrawText(const ASubclass: TSeGroupSubclass; Canvas: TCanvas;
      AGroup: TSeGroupInfo; AText: TSeTextInfo;
      const AObject: TSeStyleElementObject = ktoDefault); virtual;
    { Memo class }
    function IsMemoDefined(const ASubclass: TSeMemoSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; virtual;
    function IsMemoTransparent(const ASubclass: TSeMemoSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; virtual;
    function MemoGetClientRect(const ASubclass: TSeMemoSubclass; Canvas: TCanvas;
      AMemo: TSeMemoInfo; const AObject: TSeStyleElementObject = ktoDefault): TRect; virtual;
    procedure MemoDraw(const ASubclass: TSeMemoSubclass; Canvas: TCanvas;
      AMemo: TSeMemoInfo; const AObject: TSeStyleElementObject = ktoDefault); virtual;
    { Box class }
    function IsBoxDefined(const ASubclass: TSeBoxSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; virtual;
    function IsBoxTransparent(const ASubclass: TSeBoxSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; virtual;
    function BoxGetClientRect(const ASubclass: TSeBoxSubclass; Canvas: TCanvas;
      ABox: TSeBoxInfo; const AObject: TSeStyleElementObject = ktoDefault): TRect; virtual;
    procedure BoxDraw(const ASubclass: TSeBoxSubclass; Canvas: TCanvas;
      ABox: TSeBoxInfo; const AObject: TSeStyleElementObject = ktoDefault); virtual;
    { Menu class }
    function IsMenuDefined(const ASubclass: TSeMenuSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; virtual;
    function IsMenuTransparent(const ASubclass: TSeMenuSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; virtual;
    function MenuGetClientRect(const ASubclass: TSeMenuSubclass;
      AMenu: TSeMenuInfo; const AObject: TSeStyleElementObject = ktoDefault): TRect; virtual;
    procedure MenuDraw(const ASubclass: TSeMenuSubclass; Canvas: TCanvas;
      AMenu: TSeMenuInfo; const AObject: TSeStyleElementObject = ktoDefault); virtual;
    procedure MenuDrawItem(const ASubclass: TSeMenuSubclass; Canvas: TCanvas;
      AItem: TSeMenuItemInfo; ADPI: Integer = 0; const AObject: TSeStyleElementObject = ktoDefault); virtual;
    procedure MenuDrawItemText(const ASubclass: TSeMenuSubclass; Canvas: TCanvas;
      AItem: TSeMenuItemInfo; AText: TSeTextInfo;
      const AObject: TSeStyleElementObject = ktoDefault); virtual;
    procedure MenuDrawItemGlyph(const ASubclass: TSeMenuSubclass; Canvas: TCanvas;
      AItem: TSeMenuItemInfo; AGlyph: TSeMenuGlyphInfo;  ADPI: Integer = 0;
      const AObject: TSeStyleElementObject = ktoDefault); virtual;
    { Dock class }
    function IsDockDefined(const ASubclass: TSeDockSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; virtual;
    function DockGetClientRect(const ASubclass: TSeDockSubclass;
      ADock: TSeDockInfo; const AObject: TSeStyleElementObject = ktoDefault): TRect; virtual;
    procedure DockDraw(const ASubclass: TSeDockSubclass; Canvas: TCanvas;
      ADock: TSeDockInfo; ADPI: Integer = 0; const AObject: TSeStyleElementObject = ktoDefault); virtual;
    { Tool class }
    function IsToolDefined(const ASubclass: TSeToolSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; virtual;
    function ToolGetClientRect(const ASubclass: TSeToolSubclass;
      ATool: TSeToolInfo; const AObject: TSeStyleElementObject = ktoDefault): TRect; virtual;
    procedure ToolDraw(const ASubclass: TSeToolSubclass; Canvas: TCanvas;
      ATool: TSeToolInfo; ADPI: Integer = 0; const AObject: TSeStyleElementObject = ktoDefault); virtual;
    procedure ToolDrawItem(const ASubclass: TSeToolSubclass; Canvas: TCanvas;
      AItem: TSeToolItemInfo; ADPI: Integer = 0; const AObject: TSeStyleElementObject = ktoDefault); virtual;
    procedure ToolDrawItemText(const ASubclass: TSeToolSubclass; Canvas: TCanvas;
      AItem: TSeToolItemInfo; AText: TSeTextInfo;
      const AObject: TSeStyleElementObject = ktoDefault); virtual;
    procedure ToolDrawItemGlyph(const ASubclass: TSeToolSubclass; Canvas: TCanvas;
      AItem: TSeToolItemInfo; AGlyph: TSeToolGlyphInfo;
      const AObject: TSeStyleElementObject = ktoDefault); virtual;
    { BarDock class }
    function IsBarDockDefined(const ASubclass: TSeBarDockSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; virtual;
    function BarDockGetClientRect(const ASubclass: TSeBarDockSubclass; Canvas: TCanvas;
      ADock: TSeBarDockInfo; const AObject: TSeStyleElementObject = ktoDefault): TRect; virtual;
    procedure BarDockDraw(const ASubclass: TSeBarDockSubclass; Canvas: TCanvas;
      ADock: TSeBarDockInfo; const AObject: TSeStyleElementObject = ktoDefault); virtual;
    { Bar class }
    function IsBarDefined(const ASubclass: TSeBarSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; virtual;
    function IsBarTransparent(const ASubclass: TSeBarSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; virtual;
    function BarGetClientRect(const ASubclass: TSeBarSubclass; Canvas: TCanvas;
      ABar: TSeBarInfo; const AObject: TSeStyleElementObject = ktoDefault): TRect; virtual;
    procedure BarDraw(const ASubclass: TSeBarSubclass; Canvas: TCanvas;
      ABar: TSeBarInfo; const AObject: TSeStyleElementObject = ktoDefault); virtual;
    procedure BarDrawCaption(const ASubclass: TSeBarSubclass; Canvas: TCanvas;
      ABar: TSeBarInfo; ACaption: TSeBarCaptionInfo; const AObject: TSeStyleElementObject = ktoDefault); virtual;
    procedure BarDrawButton(const ASubclass: TSeBarSubclass; Canvas: TCanvas;
      AButton: TSeBarButtonInfo; const AObject: TSeStyleElementObject = ktoDefault); virtual;
    procedure BarDrawItem(const ASubclass: TSeBarSubclass; Canvas: TCanvas;
      AItem: TSeBarItemInfo; const AObject: TSeStyleElementObject = ktoDefault); virtual;
    procedure BarDrawText(const ASubclass: TSeBarSubclass; Canvas: TCanvas;
      AText: TSeTextInfo; const AObject: TSeStyleElementObject = ktoDefault); virtual;
    { Tab class }
    function IsTabDefined(const ASubclass: TSeTabSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; virtual;
    function IsTabTransparent(const ASubclass: TSeTabSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; virtual;
    function TabGetClientRect(const ASubclass: TSeTabSubclass;
      ATab: TSeTabInfo; const AObject: TSeStyleElementObject = ktoDefault): TRect; virtual;
    procedure TabDraw(const ASubclass: TSeTabSubclass; Canvas: TCanvas;
      ATab: TSeTabInfo; const AObject: TSeStyleElementObject = ktoDefault); virtual;
    procedure TabDrawItem(const ASubclass: TSeTabSubclass; Canvas: TCanvas;
      AItem: TSeTabItemInfo; const AObject: TSeStyleElementObject = ktoDefault); virtual;
    procedure TabDrawButton(const ASubclass: TSeTabSubclass; Canvas: TCanvas;
      AButton: TSeTabButtonInfo; const AObject: TSeStyleElementObject = ktoDefault); virtual;
    procedure TabDrawText(const ASubclass: TSeTabSubclass; Canvas: TCanvas;
      AItem: TSeTabItemInfo; AText: TSeTextInfo;
      const AObject: TSeStyleElementObject = ktoDefault); virtual;
    procedure TabDrawGlyph(const ASubclass: TSeTabSubclass; Canvas: TCanvas;
      AItem: TSeTabItemInfo; AGlyph: TSeGlyphInfo;
      const AObject: TSeStyleElementObject = ktoDefault); virtual;
    { List class }
    function IsListDefined(const ASubclass: TSeListSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; virtual;
    function IsListTransparent(const ASubclass: TSeListSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; virtual;
    function ListGetClientRect(const ASubclass: TSeListSubclass;
      AList: TSeListInfo; const AObject: TSeStyleElementObject = ktoDefault): TRect; virtual;
    procedure ListDraw(const ASubclass: TSeListSubclass; Canvas: TCanvas;
      AList: TSeListInfo; const AObject: TSeStyleElementObject = ktoDefault); virtual;
    procedure ListDrawColumn(const ASubclass: TSeListSubclass; Canvas: TCanvas;
      AColumn: TSeListColumnInfo; const AObject: TSeStyleElementObject = ktoDefault); virtual;
    procedure ListDrawItem(const ASubclass: TSeListSubclass; Canvas: TCanvas;
      AItem: TSeListItemInfo; const AObject: TSeStyleElementObject = ktoDefault); virtual;
    procedure ListDrawText(const ASubclass: TSeListSubclass; Canvas: TCanvas;
      AItem: TSeListItemInfo; AText: TSeTextInfo;
      const AObject: TSeStyleElementObject = ktoDefault); virtual;
    procedure ListDrawGlyph(const ASubclass: TSeListSubclass; Canvas: TCanvas;
      AItem: TSeListItemInfo; AGlyph: TSeListGlyphInfo;
      const AObject: TSeStyleElementObject = ktoDefault); virtual;
    { Tree class }
    function IsTreeDefined(const ASubclass: TSeTreeSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; virtual;
    function IsTreeTransparent(const ASubclass: TSeTreeSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; virtual;
    function TreeGetClientRect(const ASubclass: TSeTreeSubclass;
      ATree: TSeTreeInfo; const AObject: TSeStyleElementObject = ktoDefault): TRect; virtual;
    procedure TreeDraw(const ASubclass: TSeTreeSubclass; Canvas: TCanvas;
      ATree: TSeTreeInfo; const AObject: TSeStyleElementObject = ktoDefault); virtual;
    procedure TreeDrawItem(const ASubclass: TSeTreeSubclass; Canvas: TCanvas;
      AItem: TSeTreeItemInfo; const AObject: TSeStyleElementObject = ktoDefault); virtual;
    procedure TreeDrawText(const ASubclass: TSeTreeSubclass; Canvas: TCanvas;
      AItem: TSeTreeItemInfo; AText: TSeTextInfo;
      const AObject: TSeStyleElementObject = ktoDefault); virtual;
    procedure TreeDrawGlyph(const ASubclass: TSeTreeSubclass; Canvas: TCanvas;
      AItem: TSeTreeItemInfo; AGlyph: TSeGlyphInfo;
      const AObject: TSeStyleElementObject = ktoDefault); virtual;
    { Grid class }
    function IsGridDefined(const ASubclass: TSeGridSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; virtual;
    function IsGridTransparent(const ASubclass: TSeGridSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; virtual;
    function GridGetClientRect(const ASubclass: TSeGridSubclass;
      AGrid: TSeGridInfo; const AObject: TSeStyleElementObject = ktoDefault): TRect; virtual;
    procedure GridDraw(const ASubclass: TSeGridSubclass; Canvas: TCanvas;
      AGrid: TSeGridInfo; const AObject: TSeStyleElementObject = ktoDefault); virtual;
    procedure GridDrawItem(const ASubclass: TSeGridSubclass; Canvas: TCanvas;
      AItem: TSeGridItemInfo; const AObject: TSeStyleElementObject = ktoDefault); virtual;
    procedure GridDrawText(const ASubclass: TSeGridSubclass; Canvas: TCanvas;
      AItem: TSeGridItemInfo; AText: TSeTextInfo;
      const AObject: TSeStyleElementObject = ktoDefault); virtual;
    procedure GridDrawGlyph(const ASubclass: TSeGridSubclass; Canvas: TCanvas;
      AItem: TSeGridItemInfo; AGlyph: TSeGlyphInfo;
      const AObject: TSeStyleElementObject = ktoDefault); virtual;
    { Positions }
    function GetBounds(AObject: TSeStyleElementObject): TRect; virtual;
    { Custom Colors }
    function CustomColor(const AColorName: string): TColor; virtual;
    { Properties }
    property Colors: TSeStyleColors read FColors;
    property SysColors: TSeStyleSysColors read FSysColors;
    property Fonts: TSeStyleFonts read FFonts;
    property Metrics: TSeStyleMetrics read FMetrics;
    property Drawing: TSeStyleDrawing read FDrawing;
    property Options: TSeStyleOptions read FOptions;
    property Name: string read FName write FName;
    property StyleFileName: string read FStyleFileName write FStyleFileName;
  published
  end;

  TSeStyleClass = class of TSeCustomStyle;

const

  SNM_THEMEMESSAGE            = WM_USER + 1024;
  SNM_LINKCHANGED             = WM_USER + 1025;

  { Message wParam flags }

  SMP_CHANGETHEME                  = 0;
  SMP_BEFORECHANGE                 = 1;
  SMP_AFTERCHANGE                  = 2;
  SMP_APPLYTHEME                   = 3;
  SMP_REMOVETHEME                  = 4;
  SMP_REPAINT                      = 5;


type
  TSePropValue = integer;

procedure AddObjectToStore(AObject: TPersistent); forward;
procedure RemoveObjectFromStore(AObject: TPersistent); forward;
procedure AssignFromStoredObject(AObject: TPersistent); forward;

procedure AddPropToStore(AObject: TObject; const APropName: string; AValue: TSePropValue); forward;
procedure RemovePropFromStore(AObject: TObject; const APropName: string); forward;
function GetPropFromStore(AObject: TObject; const APropName: string): TSePropValue; forward;

const

  WM_CUSTOMPAINTBACK = WM_USER + 429;

type

{ Parent background drawing }

  TSeBackground = record
    Rect: TRect;
    ClipRect: TRect;
    Control: TControl;
    Parent: TControl;
  end;

  TSeCustomPaintBack = record
    Rect: TRect;
    ClipRect: TRect;
    Control: HWnd;
    Parent: HWnd;
  end;
  PseCustomPaintBack = ^TSeCustomPaintBack;

  TSeDrawControlBackground = function (Canvas: TCanvas;
    Background: TSeBackground): boolean;

{ Use this routines for drawing control's background, if control
  have transparent part (like CheckBox) }

procedure DrawControlBackground(Control: TControl; DC: HDC); overload; forward;
procedure DrawControlBackground(Control: TControl; Canvas: TCanvas); overload; forward;
procedure DrawControlBackground(Control: HWnd; Canvas: TCanvas); overload; forward;

{ Register third-party DrawControlBackground }

procedure RegisterDrawControlBackground(AClass: TClass; AFunc: TSeDrawControlBackground); forward;

var
  CustomBackroundDrawing: boolean = False;

type

  TSeStyle = class(TSeCustomStyle)
  private
    FStyleSource, FCleanCopy: TSeStyleSource;
    procedure FreeObjects;
  protected
    procedure ResetObjects; virtual;
    procedure ResetStyleColors; override;
    procedure ResetStyleFonts; override;
    procedure SetFontForObject(AFont: TFont; const Font: TSeStyleFont;
      const AObject: TSeStyleElementObject = ktoDefault); override;
  public
    FObjects: array of TSeStyleObject;
    constructor Create; override;
    destructor Destroy; override;
    { Style's name and options }
    class function GetName: string; override;
    class function GetStyleConfig: TSeStyleConfig; override;
    {}
    procedure Assign(Source: TPersistent); override;
    { Reset theme }
    procedure ResetStyle; override;
    { I/O }
    class function CheckStream(Stream: TStream): boolean; override;
    function LoadFromStream(Stream: TStream): boolean; override;
    function SaveToStream(Stream: TStream): boolean; override;
    function LoadFromFile(const FileName: string): boolean; override;
    function SaveToFile(const FileName: string): boolean; override;
    { Form }
    function IsWindowDefined(const ASubclass: TSeWindowSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; override;
    function WindowGetRegion(const ASubclass: TSeWindowSubclass; const ARect: TRect;
      const AObject: TSeStyleElementObject = ktoDefault): TSeRegion; override;
    function WindowGetHitTest(const ASubclass: TSeWindowSubclass; AWindow: TSeWindowInfo;
      X, Y: integer; ADPI: Integer = 0; const AObject: TSeStyleElementObject = ktoDefault): TSeWindowHitTest; override;
    function WindowGetClientRect(const ASubclass: TSeWindowSubclass;
      const ARect: TRect; ADPI: Integer = 0;  const AObject: TSeStyleElementObject = ktoDefault): TRect; override;
    procedure WindowDraw(const ASubclass: TSeWindowSubclass; Canvas: TCanvas;
      AWindow: TSeWindowInfo; ADPI: Integer = 0; const AObject: TSeStyleElementObject = ktoDefault); override;
    procedure WindowDrawClient(const ASubclass: TSeWindowSubclass; Canvas: TCanvas;
      const ARect: TRect; ADPI: Integer = 0; const AObject: TSeStyleElementObject = ktoDefault); override;
    function WindowGetTitleRect(const ASubclass: TSeWindowSubclass;
      AWindow: TSeWindowInfo; ADPI: Integer = 0; const AObject: TSeStyleElementObject = ktoDefault): TRect; override;
    procedure WindowDrawText(const ASubclass: TSeWindowSubclass; Canvas: TCanvas;
      AWindow: TSeWindowInfo; const ARect: TRect; RightToLeftReading: Boolean; ADPI: Integer = 0;
      const AObject: TSeStyleElementObject = ktoDefault); override;
    function WindowGetButtonRect(const ASubclass: TSeWindowSubclass;
      AWindow: TSeWindowInfo; AButton: TSeWindowButtonClass; ADPI: Integer = 0;
      const AObject: TSeStyleElementObject = ktoDefault): TRect; override;
    function WindowGetFixPosition(const ASubclass: TSeWindowSubclass;
      AButton: TSeWindowButtonClass; ADPI: Integer = 0;
      const AObject: TSeStyleElementObject = ktoDefault): Boolean; override;
    procedure WindowDrawButton(const ASubclass: TSeWindowSubclass; Canvas: TCanvas;
      AButton: TSeWindowButtonInfo; ADPI: Integer = 0;
      const AObject: TSeStyleElementObject = ktoDefault); override;
    procedure WindowDrawGripper(const ASubclass: TSeWindowSubclass; Canvas: TCanvas;
      AGripper: TSeWindowGripperInfo; ADPI: Integer = 0;
      const AObject: TSeStyleElementObject = ktoDefault); override;
    { Hint class }
    function IsHintDefined(const ASubclass: TSeHintSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; override;
    function HintGetClientRect(const ASubclass: TSeHintSubclass; Canvas: TCanvas;
      AHint: TSeHintInfo; const AObject: TSeStyleElementObject = ktoDefault): TRect; override;
    procedure HintDraw(const ASubclass: TSeHintSubclass; Canvas: TCanvas;
      AHint: TSeHintInfo; const AObject: TSeStyleElementObject = ktoDefault); override;
    { Label class }
    function IsLabelDefined(const ASubclass: TSeLabelSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; override;
    { Panel class }
    function IsPanelDefined(const ASubclass: TSePanelSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; override;
    function PanelGetClientRect(const ASubclass: TSePanelSubclass;
      APanel: TSePanelInfo; const AObject: TSeStyleElementObject = ktoDefault): TRect; override;
    procedure PanelDraw(const ASubclass: TSePanelSubclass; Canvas: TCanvas;
      APanel: TSePanelInfo; const AObject: TSeStyleElementObject = ktoDefault); override;
    { Button class }
    function IsButtonDefined(const ASubclass: TSeButtonSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; override;
    function IsButtonTransparent(const ASubclass: TSeButtonSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; override;
    procedure ButtonDraw(const ASubclass: TSeButtonSubclass; Canvas: TCanvas;
      AButton: TSeButtonInfo; ADPI: Integer = 0; const AObject: TSeStyleElementObject = ktoDefault); override;
    procedure ButtonDrawText(const ASubclass: TSeButtonSubclass; Canvas: TCanvas;
      AButton: TSeButtonInfo; AText: TSeTextInfo;
      const AObject: TSeStyleElementObject = ktoDefault); override;
    { Checkbox class }
    function IsCheckDefined(const ASubclass: TSeCheckSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; override;
    function IsCheckTransparent(const ASubclass: TSeCheckSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; override;
    procedure CheckDraw(const ASubclass: TSeCheckSubclass; Canvas: TCanvas;
      ACheck: TSeCheckInfo; ADPI: Integer = 0; const AObject: TSeStyleElementObject = ktoDefault); override;
    function CheckGetSize(const ASubclass: TSeCheckSubclass;  ADPI: Integer = 0;
      const AObject: TSeStyleElementObject = ktoDefault): TPoint; override;
    { SpliTSer class }
    function IsSplitterDefined(const ASubclass: TSeSplitterSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; override;
    procedure SplitterDraw(const ASubclass: TSeSplitterSubclass; Canvas: TCanvas;
      ASplitter: TSeSplitterInfo;
      const AObject: TSeStyleElementObject = ktoDefault); override;
    { Scroll class }
    function IsScrollDefined(const ASubclass: TSeScrollSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; override;
    function IsScrollTransparent(const ASubclass: TSeScrollSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; override;
    procedure ScrollDraw(const ASubclass: TSeScrollSubclass; Canvas: TCanvas;
      AScroll: TSeScrollInfo; ADPI: Integer = 0; const AObject: TSeStyleElementObject = ktoDefault); override;
    procedure ScrollDrawButton(const ASubclass: TSeScrollSubclass; Canvas: TCanvas;
      AButton: TSeScrollButtonInfo; ADPI: Integer = 0;
      const AObject: TSeStyleElementObject = ktoDefault); override;
    procedure ScrollDrawArea(const ASubclass: TSeScrollSubclass; Canvas: TCanvas;
      AArea: TSeScrollAreaInfo; ADPI: Integer = 0;
      const AObject: TSeStyleElementObject = ktoDefault); override;
    { Progress class }
    function IsProgressDefined(const ASubclass: TSeProgressSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; override;
    function IsProgressTransparent(const ASubclass: TSeProgressSubclass;
      const AObject: TSeStyleElementObject): boolean; override;
    procedure ProgressDraw(const ASubclass: TSeProgressSubclass; Canvas: TCanvas;
      AProgress: TSeProgressInfo; const AObject: TSeStyleElementObject = ktoDefault); override;
    { Track class }
    function IsTrackDefined(const ASubclass: TSeTrackSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; override;
    function IsTrackTransparent(const ASubclass: TSeTrackSubclass;
      const AObject: TSeStyleElementObject): boolean; override;
    procedure TrackDraw(const ASubclass: TSeTrackSubclass; Canvas: TCanvas;
      ATrack: TSeTrackInfo; ADPI: Integer = 0; const AObject: TSeStyleElementObject = ktoDefault); override;
    procedure TrackBarDraw(const ASubclass: TSeTrackSubclass; Canvas: TCanvas;
      ABar: TSeTrackBarInfo; ADPI: Integer = 0; const AObject: TSeStyleElementObject = ktoDefault); override;
    procedure TrackDrawThumb(const ASubclass: TSeTrackSubclass; Canvas: TCanvas;
      AThumb: TSeTrackThumbInfo; ADPI: Integer = 0;
      const AObject: TSeStyleElementObject = ktoDefault); override;
{    function TrackGetThumbSize(const ASubclass: TSeTrackSubclass; Canvas: TCanvas;
      AThumb: TSeTrackThumbInfo;
      const AObject: TSeStyleElementObject = ktoDefault): TPoint; override;}
    { Header class }
    function IsHeaderDefined(const ASubclass: TSeHeaderSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; override;
    procedure HeaderDraw(const ASubclass: TSeHeaderSubclass; Canvas: TCanvas;
      AHeader: TSeHeaderInfo; const AObject: TSeStyleElementObject = ktoDefault); override;
    procedure HeaderDrawSection(const ASubclass: TSeHeaderSubclass; Canvas: TCanvas;
      ASection: TSeHeaderSectionInfo;
      const AObject: TSeStyleElementObject = ktoDefault); override;
    { Status class }
    function IsStatusDefined(const ASubclass: TSeStatusSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; override;
    function StatusGetClientRect(const ASubclass: TSeStatusSubclass;
      AStatus: TSeStatusInfo; const AObject: TSeStyleElementObject = ktoDefault): TRect; override;
    procedure StatusDraw(const ASubclass: TSeStatusSubclass; Canvas: TCanvas;
      AStatus: TSeStatusInfo; ADPI: Integer = 0; const AObject: TSeStyleElementObject = ktoDefault); override;
    procedure StatusDrawPanel(const ASubclass: TSeStatusSubclass; Canvas: TCanvas;
      APanel: TSeStatusPanelInfo; ADPI: Integer = 0;
      const AObject: TSeStyleElementObject = ktoDefault); override;
    procedure StatusDrawText(const ASubclass: TSeStatusSubclass; Canvas: TCanvas;
      APanel: TSeStatusPanelInfo;
      AText: TSeTextInfo; const AObject: TSeStyleElementObject = ktoDefault); override;
    { Edit class }
    function IsEditDefined(const ASubclass: TSeEditSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; override;
    function EditGetClientRect(const ASubclass: TSeEditSubclass; Canvas: TCanvas;
      AEdit: TSeEditInfo; const AObject: TSeStyleElementObject = ktoDefault): TRect; override;
    procedure EditDraw(const ASubclass: TSeEditSubclass; Canvas: TCanvas;
      AEdit: TSeEditInfo; const AObject: TSeStyleElementObject = ktoDefault); override;
    procedure EditDrawButton(const ASubclass: TSeEditSubclass; Canvas: TCanvas;
      AButton: TSeEditButtonInfo; ADPI: Integer = 0;
      const AObject: TSeStyleElementObject = ktoDefault); override;
    { Group class }
    function IsGroupDefined(const ASubclass: TSeGroupSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; override;
    function IsGroupTransparent(const ASubclass: TSeGroupSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; override;
    procedure GroupDraw(const ASubclass: TSeGroupSubclass; Canvas: TCanvas;
      AGroup: TSeGroupInfo; const AObject: TSeStyleElementObject = ktoDefault); override;
    { Memo class }
    function IsMemoDefined(const ASubclass: TSeMemoSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; override;
    function MemoGetClientRect(const ASubclass: TSeMemoSubclass; Canvas: TCanvas;
      AMemo: TSeMemoInfo; const AObject: TSeStyleElementObject = ktoDefault): TRect; override;
    procedure MemoDraw(const ASubclass: TSeMemoSubclass; Canvas: TCanvas;
      AMemo: TSeMemoInfo; const AObject: TSeStyleElementObject = ktoDefault); override;
    { Box class }
    function IsBoxDefined(const ASubclass: TSeBoxSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; override;
    function IsBoxTransparent(const ASubclass: TSeBoxSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; override;
    { Menu class }
    function IsMenuDefined(const ASubclass: TSeMenuSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; override;
    function MenuGetClientRect(const ASubclass: TSeMenuSubclass;
      AMenu: TSeMenuInfo; const AObject: TSeStyleElementObject = ktoDefault): TRect; override;
    procedure MenuDraw(const ASubclass: TSeMenuSubclass; Canvas: TCanvas;
      AMenu: TSeMenuInfo; const AObject: TSeStyleElementObject = ktoDefault); override;
    procedure MenuDrawItem(const ASubclass: TSeMenuSubclass; Canvas: TCanvas;
      AItem: TSeMenuItemInfo; ADPI: Integer = 0; const AObject: TSeStyleElementObject = ktoDefault); override;
    procedure MenuDrawItemText(const ASubclass: TSeMenuSubclass; Canvas: TCanvas;
      AItem: TSeMenuItemInfo; AText: TSeTextInfo;
      const AObject: TSeStyleElementObject = ktoDefault); override;
    procedure MenuDrawItemGlyph(const ASubclass: TSeMenuSubclass; Canvas: TCanvas;
      AItem: TSeMenuItemInfo; AGlyph: TSeMenuGlyphInfo;   ADPI: Integer = 0;
      const AObject: TSeStyleElementObject = ktoDefault); override;
    { Dock class }
    function IsDockDefined(const ASubclass: TSeDockSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; override;
    function DockGetClientRect(const ASubclass: TSeDockSubclass;
      ADock: TSeDockInfo; const AObject: TSeStyleElementObject = ktoDefault): TRect; override;
    procedure DockDraw(const ASubclass: TSeDockSubclass; Canvas: TCanvas;
      ADock: TSeDockInfo; ADPI: Integer = 0; const AObject: TSeStyleElementObject = ktoDefault); override;
    { Tool class }
    function IsToolDefined(const ASubclass: TSeToolSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; override;
    procedure ToolDraw(const ASubclass: TSeToolSubclass; Canvas: TCanvas;
      ATool: TSeToolInfo; ADPI: Integer = 0; const AObject: TSeStyleElementObject = ktoDefault); override;
    procedure ToolDrawItem(const ASubclass: TSeToolSubclass; Canvas: TCanvas;
      AItem: TSeToolItemInfo; ADPI: Integer = 0; const AObject: TSeStyleElementObject = ktoDefault); override;
    { BarDock class }
    function IsBarDockDefined(const ASubclass: TSeBarDockSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; override;
    function BarDockGetClientRect(const ASubclass: TSeBarDockSubclass; Canvas: TCanvas;
      ADock: TSeBarDockInfo; const AObject: TSeStyleElementObject = ktoDefault): TRect; override;
    procedure BarDockDraw(const ASubclass: TSeBarDockSubclass; Canvas: TCanvas;
      ADock: TSeBarDockInfo; const AObject: TSeStyleElementObject = ktoDefault); override;
    { Bar class }
    function IsBarDefined(const ASubclass: TSeBarSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; override;
    function IsBarTransparent(const ASubclass: TSeBarSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; override;
    function BarGetClientRect(const ASubclass: TSeBarSubclass; Canvas: TCanvas;
      ABar: TSeBarInfo; const AObject: TSeStyleElementObject = ktoDefault): TRect; override;
    procedure BarDraw(const ASubclass: TSeBarSubclass; Canvas: TCanvas;
      ABar: TSeBarInfo; const AObject: TSeStyleElementObject = ktoDefault); override;
    procedure BarDrawCaption(const ASubclass: TSeBarSubclass; Canvas: TCanvas;
      ABar: TSeBarInfo; ACaption: TSeBarCaptionInfo; const AObject: TSeStyleElementObject = ktoDefault); override;
    procedure BarDrawButton(const ASubclass: TSeBarSubclass; Canvas: TCanvas;
      AButton: TSeBarButtonInfo; const AObject: TSeStyleElementObject = ktoDefault); override;
    { Tab class }
    function IsTabDefined(const ASubclass: TSeTabSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; override;
    function TabGetClientRect(const ASubclass: TSeTabSubclass;
      ATab: TSeTabInfo; const AObject: TSeStyleElementObject = ktoDefault): TRect; override;
    procedure TabDraw(const ASubclass: TSeTabSubclass; Canvas: TCanvas;
      ATab: TSeTabInfo; const AObject: TSeStyleElementObject = ktoDefault); override;
    procedure TabDrawItem(const ASubclass: TSeTabSubclass; Canvas: TCanvas;
      AItem: TSeTabItemInfo;
      const AObject: TSeStyleElementObject = ktoDefault); override;
    procedure TabDrawButton(const ASubclass: TSeTabSubclass; Canvas: TCanvas;
      AButton: TSeTabButtonInfo;
      const AObject: TSeStyleElementObject = ktoDefault); override;
    procedure TabDrawText(const ASubclass: TSeTabSubclass; Canvas: TCanvas;
      AItem: TSeTabItemInfo; AText: TSeTextInfo;
      const AObject: TSeStyleElementObject = ktoDefault); override;
    { List class }
    function IsListDefined(const ASubclass: TSeListSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; override;
    function ListGetClientRect(const ASubclass: TSeListSubclass;
      AList: TSeListInfo; const AObject: TSeStyleElementObject = ktoDefault): TRect; override;
    procedure ListDraw(const ASubclass: TSeListSubclass; Canvas: TCanvas;
      AList: TSeListInfo; const AObject: TSeStyleElementObject = ktoDefault); override;
    procedure ListDrawItem(const ASubclass: TSeListSubclass; Canvas: TCanvas;
      AItem: TSeListItemInfo; const AObject: TSeStyleElementObject = ktoDefault); override;
    procedure ListDrawGlyph(const ASubclass: TSeListSubclass; Canvas: TCanvas;
      AItem: TSeListItemInfo; AGlyph: TSeListGlyphInfo;
      const AObject: TSeStyleElementObject = ktoDefault); override;
    { Tree class }
    function IsTreeDefined(const ASubclass: TSeTreeSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; override;
    procedure TreeDraw(const ASubclass: TSeTreeSubclass; Canvas: TCanvas;
      ATree: TSeTreeInfo; const AObject: TSeStyleElementObject = ktoDefault); override;
    procedure TreeDrawItem(const ASubclass: TSeTreeSubclass; Canvas: TCanvas;
      AItem: TSeTreeItemInfo; const AObject: TSeStyleElementObject = ktoDefault); override;
    { Grid class }
    function IsGridDefined(const ASubclass: TSeGridSubclass;
      const AObject: TSeStyleElementObject = ktoDefault): boolean; override;
    procedure GridDraw(const ASubclass: TSeGridSubclass; Canvas: TCanvas;
      AGrid: TSeGridInfo; const AObject: TSeStyleElementObject = ktoDefault); override;
    procedure GridDrawItem(const ASubclass: TSeGridSubclass; Canvas: TCanvas;
      AItem: TSeGridItemInfo; const AObject: TSeStyleElementObject = ktoDefault); override;
    { Custom Colors }
    function CustomColor(const AColorName: string): TColor; override;
    { Positions }
    function GetBounds(AObject: TSeStyleElementObject): TRect; override;
     { Use this routines to check theme's classes, subclasses, objects
     in CurrentStyle object }
    function IsObjectDefined(const ASubclass: TSeWindowSubclass;
      const AObject: TSeStyleElementObject = ktoDefault; const AStyleLink: TSeCustomStyleLink = nil): boolean; overload;
    function IsObjectDefined(const ASubclass: TSeHintSubclass;
      const AObject: TSeStyleElementObject = ktoDefault; const AStyleLink: TSeCustomStyleLink = nil): boolean; overload;
    function IsObjectDefined(const ASubclass: TSeLabelSubclass;
      const AObject: TSeStyleElementObject = ktoDefault; const AStyleLink: TSeCustomStyleLink = nil): boolean; overload;
    function IsObjectDefined(const ASubclass: TSePanelSubclass;
      const AObject: TSeStyleElementObject = ktoDefault; const AStyleLink: TSeCustomStyleLink = nil): boolean; overload;
    function IsObjectDefined(const ASubclass: TSeButtonSubclass;
      const AObject: TSeStyleElementObject = ktoDefault; const AStyleLink: TSeCustomStyleLink = nil): boolean; overload;
    function IsObjectDefined(const ASubclass: TSeCheckSubclass;
      const AObject: TSeStyleElementObject = ktoDefault; const AStyleLink: TSeCustomStyleLink = nil): boolean; overload;
    function IsObjectDefined(const ASubclass: TSeSplitterSubclass;
      const AObject: TSeStyleElementObject = ktoDefault; const AStyleLink: TSeCustomStyleLink = nil): boolean; overload;
    function IsObjectDefined(const ASubclass: TSeScrollSubclass;
      const AObject: TSeStyleElementObject = ktoDefault; const AStyleLink: TSeCustomStyleLink = nil): boolean; overload;
    function IsObjectDefined(const ASubclass: TSeProgressSubclass;
      const AObject: TSeStyleElementObject = ktoDefault; const AStyleLink: TSeCustomStyleLink = nil): boolean; overload;
    function IsObjectDefined(const ASubclass: TSeTrackSubclass;
      const AObject: TSeStyleElementObject = ktoDefault; const AStyleLink: TSeCustomStyleLink = nil): boolean; overload;
    function IsObjectDefined(const ASubclass: TSeHeaderSubclass;
      const AObject: TSeStyleElementObject = ktoDefault; const AStyleLink: TSeCustomStyleLink = nil): boolean; overload;
    function IsObjectDefined(const ASubclass: TSeStatusSubclass;
      const AObject: TSeStyleElementObject = ktoDefault; const AStyleLink: TSeCustomStyleLink = nil): boolean; overload;
    function IsObjectDefined(const ASubclass: TSeEditSubclass;
      const AObject: TSeStyleElementObject = ktoDefault; const AStyleLink: TSeCustomStyleLink = nil): boolean; overload;
    function IsObjectDefined(const ASubclass: TSeGroupSubclass;
      const AObject: TSeStyleElementObject = ktoDefault; const AStyleLink: TSeCustomStyleLink = nil): boolean; overload;
    function IsObjectDefined(const ASubclass: TSeMemoSubclass;
      const AObject: TSeStyleElementObject = ktoDefault; const AStyleLink: TSeCustomStyleLink = nil): boolean; overload;
    function IsObjectDefined(const ASubclass: TSeMenuSubclass;
      const AObject: TSeStyleElementObject = ktoDefault; const AStyleLink: TSeCustomStyleLink = nil): boolean; overload;
    function IsObjectDefined(const ASubclass: TSeToolSubclass;
      const AObject: TSeStyleElementObject = ktoDefault; const AStyleLink: TSeCustomStyleLink = nil): boolean; overload;
    function IsObjectDefined(const ASubclass: TSeBarSubclass;
      const AObject: TSeStyleElementObject = ktoDefault; const AStyleLink: TSeCustomStyleLink = nil): boolean; overload;
    function IsObjectDefined(const ASubclass: TSeTabSubclass;
      const AObject: TSeStyleElementObject = ktoDefault; const AStyleLink: TSeCustomStyleLink = nil): boolean; overload;
    function IsObjectDefined(const ASubclass: TSeListSubclass;
      const AObject: TSeStyleElementObject = ktoDefault; const AStyleLink: TSeCustomStyleLink = nil): boolean; overload;
    function IsObjectDefined(const ASubclass: TSeTreeSubclass;
      const AObject: TSeStyleElementObject = ktoDefault; const AStyleLink: TSeCustomStyleLink = nil): boolean; overload;
    function IsObjectDefined(const ASubclass: TSeGridSubclass;
      const AObject: TSeStyleElementObject = ktoDefault; const AStyleLink: TSeCustomStyleLink = nil): boolean; overload;
    function IsObjectDefined(const ASubclass: TSeDockSubclass;
      const AObject: TSeStyleElementObject = ktoDefault; const AStyleLink: TSeCustomStyleLink = nil): boolean; overload;
    function IsObjectDefined(const ASubclass: TSeBarDockSubclass;
      const AObject: TSeStyleElementObject = ktoDefault; const AStyleLink: TSeCustomStyleLink = nil): boolean; overload;

    property StyleSource: TSeStyleSource read FStyleSource;
  published

  end;

{ implementation }

procedure FillFont(AFont: TFont; StyleObject: TSeStyleObject; State: TSeState);
begin
  AFont.Assign(StyleObject.Font);

  if StyleObject is TSeActiveObject then
  begin
    if (State in [ssFocused, ssHot]) then
      AFont.Assign(TSeActiveObject(StyleObject).ActiveFont);
    if (State = ssDisabled) then
      AFont.Assign(TSeActiveObject(StyleObject).DisabledFont);
    Exit;
  end;

  if StyleObject is TSeActiveBitmap then
  begin
    if (State in [ssFocused, ssHot]) then
      AFont.Assign(TSeActiveBitmap(StyleObject).ActiveFont);
    Exit;
  end;

  if StyleObject is TSeTextObject then
  begin
    case State of
      ssPressed: AFont.Assign(TSeTextObject(StyleObject).FontPressed);
      ssHot: AFont.Assign(TSeTextObject(StyleObject).FontHot);
      ssFocused: AFont.Assign(TSeTextObject(StyleObject).FontFocused);
      ssDisabled: AFont.Assign(TSeTextObject(StyleObject).FontDisabled);
    end;
    Exit;
  end;
end;

{ Filter routines }

function GetDialogFilter3: string;
begin
  Result := TSeStyleFilter.GetFilterName + '(*' + TSeStyleFilter.GetFileExtension + ')|*' + TSeStyleFilter.GetFileExtension;
end;

{ TSeStyleSource ===============================================================}

constructor TSeStyleSource.Create(AOwner: TComponent);
begin
  inherited;
  FName := 'Untitled';
  FVersion := '1.0';
  FAuthor := '';
  FAuthorEMail := '';
  FAuthorURL := '';
  FMobilePlatform := False;
  FRetinaDisplay := False;
  FDescription := '';

  FObjects := TList.Create;
  FBitmaps := TSeBitmapList.Create;
  FColors := TSeStyleColors.Create(nil);
  FSysColors := TSeStyleSysColors.Create(nil);
  FFonts := TSeStyleFonts.Create(nil);
  FDisplayNames := TStringList.Create;
end;

destructor TSeStyleSource.Destroy;
begin
  Clear;

  FreeAndNil(FFonts);
  FreeAndNil(FSysColors);
  FreeAndNil(FColors);
  FDisplayNames.Free;
  FBitmaps.Free;
  FObjects.Free;
  inherited;
end;

procedure TSeStyleSource.SetDisplayNames(Value: TStrings);
begin
  FDisplayNames.Assign(Value);
end;

procedure TSeStyleSource.Clear;
var
  i: integer;
begin
  FBitmaps.Clear;

  for i := 0 to FObjects.Count-1 do
    TSeStyleObject(FObjects[i]).Free;
  FObjects.Clear;
end;

procedure TSeStyleSource.CheckingObject;
var
  i: integer;
  Obj: TSeStyleObject;
begin
  for i := 0 to FObjects.Count-1 do
  begin
    Obj := TSeStyleObject(FObjects[i]).FindObjectByKind(skClient);
    if Obj <> nil then
      Obj.Masked := False;
  end;
end;

procedure TSeStyleSource.Assign(Source: TPersistent);
var
  i: integer;
  B: TSeBitmap;
  S: TSeStyleObject;
begin
  if Source is TSeStyleSource then
  begin
    Clear;
    { Load SourceInfo }
    FName := TSeStyleSource(Source).FName;
    FVersion := TSeStyleSource(Source).FVersion;
    FAuthor := TSeStyleSource(Source).FAuthor;
    FAuthorEMail := TSeStyleSource(Source).FAuthorEMail;
    FAuthorURL := TSeStyleSource(Source).FAuthorURL;
    FMobilePlatform := TSeStyleSource(Source).FMobilePlatform;
    FRetinaDisplay := TSeStyleSource(Source).FRetinaDisplay;
    FDescription := TSeStyleSource(Source).FDescription;

    FDisplayNames.Assign(TSeStyleSource(Source).FDisplayNames);
    { Copy Images }
    FBitmaps.Capacity := TSeStyleSource(Source).Bitmaps.Count;
    for i := 0 to TSeStyleSource(Source).Bitmaps.Count - 1 do
    begin
      B := TSeBitmap.Create;
      B.Assign(TSeStyleSource(Source).Bitmaps[i]);
      B.Name := TSeStyleSource(Source).Bitmaps[i].Name;
      FBitmaps.Add(B);
    end;
    { Copy Objects }
    FObjects.Capacity := TSeStyleSource(Source).FObjects.Count;
    for i := 0 to TSeStyleSource(Source).FObjects.Count - 1 do
    begin
      S := TSeStyleSource(Source).Objects[i].CreateCopy(nil);
      FObjects.Add(S);
      S.Bitmaps := FBitmaps;
      S.AfterLoad;
    end;
    { Copy Fonts }
    FFonts.Assign(TSeStyleSource(Source).Fonts);
    FColors.Assign(TSeStyleSource(Source).Colors);
    FSysColors.Assign(TSeStyleSource(Source).SysColors);
  end
  else
    inherited;
end;

{ I/O Routines ================================================================}

procedure TSeStyleSource.LoadFromStream(Stream: TStream);
var
  Filter: TSeStyleFilter;
begin
  Filter := TSeStyleFilter.Create;
  try
    Clear;
    Filter.StyleSource := Self;
    Filter.ReadStyle(Stream);
    CheckingObject;
  finally
    Filter.Free;
  end;
end;

procedure TSeStyleSource.SaveToStream(Stream: TStream);
var
  Filter: TSeStyleFilter;
begin
  Filter := TSeStyleFilter.Create;
  try
    Filter.StyleSource := Self;
    Filter.WriteStyle(Stream);
  finally
    Filter.Free;
  end;
end;

procedure TSeStyleSource.LoadFromFile(const FileName: string);
var
  Stream: TStream;
  Filter: TSeStyleFilter;
  LName: String;
begin
  { Check file }
  if FileName = '' then Exit;
  if not FileExists(FileName) then
  begin
    if FileName[2] <> ':' then
    begin
      LName := ExtractFilePath(ParamStr(0)) + FileName;
      if not FileExists(LName) then Exit;
    end
    else
      Exit;
  end;

  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    Filter := TSeStyleFilter.Create;
    try
      Clear;
      Filter.StyleSource := Self;
      Filter.ReadStyle(Stream);
    finally
      Filter.Free;
    end;

    CheckingObject;
  finally
    Stream.Free;
  end;
end;

procedure TSeStyleSource.SaveToFile(const FileName: string);
var
  Stream: TStream;
  Filter: TSeStyleFilter;
begin
  if FileName = '' then Exit;

  Stream := TFileStream.Create(FileName, fmCreate);
  try
    Filter := TSeStyleFilter.Create;
    try
      Filter.StyleSource := Self;

      Filter.WriteStyle(Stream);
    finally
      Filter.Free;
    end;
  finally
    Stream.Free;
  end;
end;

procedure TSeStyleSource.FillColorsAndFonts;
var
  StyleObject: TSeStyleObject;
  TextObject: TSeStyleObject;
begin
  { Colros }
  StyleObject := GetObjectByName('form');
  if StyleObject <> nil then
    Colors[ktcWindow] := StyleObject.Color;

  StyleObject := GetObjectByName('listbox');
  if StyleObject <> nil then
  begin
    Colors[ktcListBox] := StyleObject.Color;
    Colors[ktcListBoxDisabled] := StyleObject.Color;
    Colors[ktcListView] := StyleObject.Color;
    Colors[ktcTreeView] := StyleObject.Color;
  end;

  StyleObject := GetObjectByName('categorybuttons');
  if StyleObject <> nil then
    Colors[ktcCategoryButtons] := StyleObject.Color;

  StyleObject := GetObjectByName('categorypanelgroup');
  if StyleObject <> nil then
    Colors[ktcCategoryPanelGroup] := StyleObject.Color;

  StyleObject := GetObjectByName('combobox');
  if StyleObject <> nil then
  begin
    Colors[ktcComboBox] := StyleObject.Color;
    Colors[ktcComboBoxDisabled] := StyleObject.Color;
  end;

  StyleObject := GetObjectByName('grid');
  if StyleObject <> nil then
  begin
    Colors[ktcGrid] := StyleObject.Color;
  end;

  Colors[ktcBorder] := clBlack;

  StyleObject := GetObjectByName('container');
  if StyleObject <> nil then
  begin
    Colors[ktcPanel] := StyleObject.Color;
    Colors[ktcPanelDisabled] := StyleObject.Color;
  end;

  StyleObject := GetObjectByName('button');
  if StyleObject <> nil then
  begin
    Colors[ktcButton] := StyleObject.Color;
    Colors[ktcButtonHot] := StyleObject.Color;
    Colors[ktcButtonPressed] := StyleObject.Color;
    Colors[ktcButtonFocused] := StyleObject.Color;
    Colors[ktcButtonDisabled] := StyleObject.Color;
  end;

  StyleObject := GetObjectByName('edit');
  if StyleObject <> nil then
  begin
    Colors[ktcEdit] := StyleObject.Color;
    Colors[ktcEditDisabled] := StyleObject.Color;
  end;

  StyleObject := GetObjectByName('hint');
  if StyleObject <> nil then
    Colors[ktcHintGradientBase] := StyleObject.Color
  else
    Colors[ktcHintGradientBase] := clInfoBk;

  { Fonts }
  StyleObject := GetObjectByName('form');
  if StyleObject <> nil then
  begin
    TextObject := StyleObject.FindObjectByKind(skTitle);
    if TextObject <> nil then
    begin
      FillFont(Fonts[ktfCaptionTextNormal], TextObject, ssFocused);
      FillFont(Fonts[ktfCaptionTextInactive], TextObject, ssNormal);
    end;
  end;

  StyleObject := GetObjectByName('button');
  if StyleObject <> nil then
  begin
    TextObject := StyleObject.FindObjectByName('text');
    if TextObject <> nil then
    begin
      FillFont(Fonts[ktfButtonTextNormal], TextObject, ssNormal);
      FillFont(Fonts[ktfButtonTextPressed], TextObject, ssPressed);
      FillFont(Fonts[ktfButtonTextHot], TextObject, ssHot);
      FillFont(Fonts[ktfButtonTextFocused], TextObject, ssFocused);
      FillFont(Fonts[ktfButtonTextDisabled], TextObject, ssDisabled);
    end;
  end;

  StyleObject := GetObjectByName('checkbox');
  if StyleObject <> nil then
  begin
    TextObject := StyleObject.FindObjectByName('text');
    if TextObject <> nil then
    begin
      FillFont(Fonts[ktfCheckBoxTextNormal], TextObject, ssNormal);
      FillFont(Fonts[ktfCheckBoxTextPressed], TextObject, ssPressed);
      FillFont(Fonts[ktfCheckBoxTextHot], TextObject, ssHot);
      FillFont(Fonts[ktfCheckBoxTextFocused], TextObject, ssFocused);
      FillFont(Fonts[ktfCheckBoxTextDisabled], TextObject, ssDisabled);
    end;
  end;

  StyleObject := GetObjectByName('radiobutton');
  if StyleObject <> nil then
  begin
    TextObject := StyleObject.FindObjectByName('text');
    if TextObject <> nil then
    begin
      FillFont(Fonts[ktfRadioButtonTextNormal], TextObject, ssNormal);
      FillFont(Fonts[ktfRadioButtonTextPressed], TextObject, ssPressed);
      FillFont(Fonts[ktfRadioButtonTextHot], TextObject, ssHot);
      FillFont(Fonts[ktfRadioButtonTextFocused], TextObject, ssFocused);
      FillFont(Fonts[ktfRadioButtonTextDisabled], TextObject, ssDisabled);
    end;
  end;

  StyleObject := GetObjectByName('groupbox');
  if StyleObject <> nil then
  begin
    TextObject := StyleObject.FindObjectByName('caption');
    if TextObject <> nil then
    begin
      FillFont(Fonts[ktfGroupBoxTextNormal], TextObject, ssNormal);
      FillFont(Fonts[ktfGroupBoxTextDisabled], TextObject, ssDisabled);
    end;
  end;

  SetFont(Fonts[ktfWindowTextNormal], 'Tahoma', 8, [], clBlack);
  SetFont(Fonts[ktfWindowTextDisabled],  'Tahoma', 8, [], clBlack);

  StyleObject := GetObjectByName('edit');
  if StyleObject <> nil then
  begin
    TextObject := StyleObject.FindObjectByName('text');
    if TextObject <> nil then
    begin
      FillFont(Fonts[ktfEditBoxTextNormal], TextObject, ssNormal);
      FillFont(Fonts[ktfEditBoxTextFocused], TextObject, ssFocused);
      FillFont(Fonts[ktfEditBoxTextHot], TextObject, ssHot);
      FillFont(Fonts[ktfEditBoxTextSelected], TextObject, ssNormal);
      FillFont(Fonts[ktfEditBoxTextDisabled], TextObject, ssDisabled);
      { Change selection font }
      Fonts[ktfEditBoxTextSelected].Color := clWhite;
    end;
  end;

  StyleObject := GetObjectByName('listbox');
  if StyleObject <> nil then
  begin
    TextObject := StyleObject.FindObjectByName('item');
    if TextObject <> nil then
    begin
      FillFont(Fonts[ktfListItemTextNormal], TextObject, ssNormal);
      FillFont(Fonts[ktfListItemTextHot], TextObject, ssNormal);
      FillFont(Fonts[ktfListItemTextSelected], TextObject, ssFocused);
      FillFont(Fonts[ktfListItemTextFocused], TextObject, ssNormal);
      FillFont(Fonts[ktfListItemTextDisabled], TextObject, ssDisabled);
    end;
  end;

  StyleObject := GetObjectByName('menubar');
  if StyleObject <> nil then
  begin
    TextObject := StyleObject.FindObjectByName('item');
    if TextObject <> nil then
    begin
      FillFont(Fonts[ktfMenuItemTextNormal], TextObject, ssNormal);
      FillFont(Fonts[ktfMenuItemTextHot], TextObject, ssHot);
      FillFont(Fonts[ktfMenuItemTextSelected], TextObject, ssFocused);
      FillFont(Fonts[ktfMenuItemTextDisabled], TextObject, ssDisabled);
    end;
    TextObject := StyleObject.FindObjectByName('toolbaritem');
    if TextObject <> nil then
    begin
      FillFont(Fonts[ktfToolItemTextNormal], TextObject, ssNormal);
      FillFont(Fonts[ktfToolItemTextHot], TextObject, ssHot);
      FillFont(Fonts[ktfToolItemTextSelected], TextObject, ssFocused);
      FillFont(Fonts[ktfToolItemTextDisabled], TextObject, ssDisabled);
    end;
  end;

  StyleObject := GetObjectByName('popupmenu');
  if StyleObject <> nil then
  begin
    TextObject := StyleObject.FindObjectByName('item');
    if TextObject <> nil then
    begin
      FillFont(Fonts[ktfPopupMenuItemTextNormal], TextObject, ssNormal);
      FillFont(Fonts[ktfPopupMenuItemTextHot], TextObject, ssHot);
      FillFont(Fonts[ktfPopupMenuItemTextSelected], TextObject, ssFocused);
      FillFont(Fonts[ktfPopupMenuItemTextDisabled], TextObject, ssDisabled);
      Fonts[ktfPopupMenuItemTextDisabled].Color := clGray;
    end;
  end;

  //TODO: Assign newly added fonts

end;

{ Objects routines ============================================================}

procedure TSeStyleSource.Add(StyleObject: TSeStyleObject);
begin
  FObjects.Add(StyleObject);
end;

procedure TSeStyleSource.Remove(StyleObject: TSeStyleObject);
begin
  FObjects.Remove(StyleObject);
end;

{ Get form object =============================================================}

function TSeStyleSource.GetObjectByKind(Kind: TSeKind): TSeStyleObject;
var
  i: integer;
begin
  for i := 0 to FObjects.Count-1 do
    if Objects[i].Kind = Kind then
    begin
      Result := Objects[i];
      Exit;
    end;
  Result := nil;
end;

function TSeStyleSource.GetObjectByName(const Name: string): TSeStyleObject;
var
  SlashPos, i: integer;
  CopyMin, CopyPlus: string;
begin
  SlashPos := System.Pos('\', Name);
  if SlashPos > 0 then
  begin
    CopyMin := System.Copy(Name, 1, SlashPos - 1);
    CopyPlus := System.Copy(Name, SlashPos + 1, Length(Name));
  end;

  for i := 0 to FObjects.Count-1 do
  begin
    if CompareText(Objects[i].Name, Name) = 0 then
    begin
      Result := Objects[i];
      Exit;
    end;
    if SlashPos > 0 then
    begin
      if CompareText(Objects[i].Name, CopyMin) = 0 then
      begin
        Result := Objects[i].FindObjectByName(CopyPlus);
        Exit;
      end;
    end;
  end;
  Result := nil;
end;

function TSeStyleSource.GetForm: TSeStyleObject;
begin
  Result := GetObjectByName('Form');
  if Result = nil then
    Result := GetObjectByKind(skForm);
end;

procedure TSeStyleSource.ReplaceBitmap(Source, Dest: TSeBitmap);
var
  i, Index: integer;
begin
  if (Source = nil) or (Dest = nil) then Exit;
  if Bitmaps.Count = 0 then Exit;
  if Count = 0 then Exit;

  Dest.Name := Source.Name;

  Index := FBitmaps.IndexOf(Source);

  if Index <> -1 then
  begin
    FBitmaps.Delete(Index);
    FBitmaps.Insert(Index, Dest);

    for i := 0 to Count - 1 do
      Objects[i].AfterLoad;

    Source.Free;
  end;
end;

{ Properties ==================================================================}

function TSeStyleSource.GetObject(index: integer): TSeStyleObject;
begin
  if (index >= 0) and (index < FObjects.Count) and (not FIsChanging) then
    Result := TSeStyleObject(FObjects[index])
  else
    Result := nil;
end;

function TSeStyleSource.GetCount: integer;
begin
  Result := FObjects.Count
end;

{ TSeStyleFilter ===============================================================}

class function TSeStyleFilter.GetFileExtension: string;
begin
  Result := TStyleEngine.FileExtension;
end;

class function TSeStyleFilter.GetFilterName: string;
begin
  Result := SStyleFileDescription;
end;

procedure TSeStyleFilter.ReadStyle(Stream: TStream);
var
  DStream: TDecompressionStream;
begin
  { Read Filter Sign }
  Stream.Read(FSign, SizeOf(FSign));
  if (FSign = StyleFileSign) or (FSign = StyleFileSign_1_0) then
  begin
    DStream := TDecompressionStream.Create(Stream);
    try
      ReadStyleSource(DStream);
    finally
      DStream.Free;
    end;
  end;
end;

procedure TSeStyleFilter.WriteStyle(Stream: TStream);
var
  CStream: TCompressionStream;
begin
  { Save Sign }
  Stream.Write(StyleFileSign, SizeOf(StyleFileSign));
  { Write Style }
  CStream := TCompressionStream.Create(System.ZLib.clDefault, Stream);
  try
    WriteStyleSource(CStream);
  finally
    CStream.Free;
  end;
end;

procedure TSeStyleFilter.ReadStyleSource(Stream: TStream);
var
  i, ACount: integer;
  StyleObject: TSeStyleObject;
  B: TSeBitmap;
  vBool: boolean;
  MemSize: Int64;
  MemStream: TMemoryStream;
begin
  if FStyleSource = nil then Exit;
  with FStyleSource do
  begin
    Clear;
    { Load SourceInfo }
    FName := ReadString(Stream);
    FVersion := ReadString(Stream);
    FAuthor := ReadString(Stream);
    FAuthorEMail := ReadString(Stream);
    FAuthorURL := ReadString(Stream);
    if FSign = StyleFileSign then
    begin
      Stream.ReadBuffer(FMobilePlatform, SizeOf(FMobilePlatform));
      Stream.ReadBuffer(FRetinaDisplay, SizeOf(FRetinaDisplay));
      FDescription := ReadString(Stream);
    end
    else
    begin
      FMobilePlatform := False;
      FRetinaDisplay := False;
      FDescription := '';
    end;
    {load display names}
    Stream.Read(MemSize, SizeOf(MemSize));
    if MemSize <> 0 then
    begin
      MemStream := TMemoryStream.Create;
      try
        MemStream.CopyFrom(Stream, MemSize);
        MemStream.Position := 0;
        FDisplayNames.LoadFromStream(MemStream);
      finally
        MemStream.Free;
      end;
    end
    else
      FDisplayNames.Clear;
    { Load Count }
    Stream.Read(ACount, SizeOf(Integer));
    { Load Images }
    FBitmaps.Capacity := ACount;
    for i := 0 to ACount - 1 do
    begin
      B := TSeBitmap.Create;
      B.LoadFromStream(Stream);
      Stream.Read(vBool, SizeOf(vBool));
      B.Transparent := False;
      Stream.Read(vBool, SizeOf(vBool));
      FBitmaps.Add(B);
    end;
    { Load Count }
    Stream.Read(ACount, SizeOf(Integer));
    FObjects.Capacity := ACount;
    for i := 0 to ACount-1 do
    begin
      StyleObject := LoadStyleObjectBinary(Stream, nil);
      if StyleObject <> nil then
      begin
        FObjects.Add(StyleObject);
        StyleObject.Bitmaps := FBitmaps;
        StyleObject.AfterLoad;
      end;
    end;
    { Load Colors }
    FColors.LoadFromStream(Stream);
    { Load SysColors }
    FSysColors.LoadFromStream(Stream);
    { Load Fonts }
    FFonts.LoadFromStream(Stream);
  end;
end;

procedure TSeStyleFilter.WriteStyleSource(Stream: TStream);
var
  i: integer;
  BCount: integer;
  B: boolean;
  MemStream: TMemoryStream;
  MemSize: Int64;
begin
  if FStyleSource = nil then Exit;
  with FStyleSource do
  begin
    WriteString(Stream, FName);
    WriteString(Stream, FVersion);
    WriteString(Stream, FAuthor);
    WriteString(Stream, FAuthorEMail);
    WriteString(Stream, FAuthorURL);
    Stream.WriteBuffer(FMobilePlatform, SizeOf(FMobilePlatform));
    Stream.WriteBuffer(FRetinaDisplay, SizeOf(FRetinaDisplay));
    WriteString(Stream, FDescription);
    {save display names}
    MemStream := TMemoryStream.Create;
    try
      FDisplayNames.SaveToStream(MemStream);
      MemSize := MemStream.Size;
      Stream.Write(MemSize, SizeOf(MemSize));
      if MemSize <> 0 then
      begin
        MemStream.SaveToStream(Stream);
      end;
    finally
      MemStream.Free;
    end;
    { Save Image Count }
    BCount := FBitmaps.Count;
    Stream.Write(BCount, SizeOf(Integer));
    { Save Images }
    for i := 0 to FBitmaps.Count-1 do
    begin
      { Write bitmap }
      FBitmaps[i].SaveToStream(Stream);
      B := FBitmaps[i].TransparentMode = tmFixed;
      Stream.Write(B, SizeOf(B));
      B := FBitmaps[i].AlphaFormat = afDefined;
      Stream.Write(B, SizeOf(B));
    end;
    { Save Object Count }
    BCount := FObjects.Count;
    Stream.Write(BCount, SizeOf(Integer));
    { Save Objects }
    for i := 0 to FObjects.Count-1 do
      SaveStyleObjectBinary(Stream, Objects[i]);
    { Save Colors }
    FColors.SaveToStream(Stream);
    { Save SysColors }
    FSysColors.SaveToStream(Stream);
    { Save Fonts }
    FFonts.SaveToStream(Stream);
  end;
end;

{ Properties }

procedure TSeStyleFilter.SetStyleSource(const Value: TSeStyleSource);
begin
  FStyleSource := Value;
end;

procedure RegisterStyleObject(ObjectClass: TSeStyleObjectClass);
begin
  if RegObjects = nil then
    RegObjects := TList.Create;
  if RegObjects <> nil then
    RegObjects.Add(ObjectClass);
end;

function GetStyleObjectClass(const ClassName: string): TSeStyleObjectClass;
var
  i: integer;
begin
  for i := 0 to RegObjects.Count-1 do
    if TSeStyleObjectClass(RegObjects[i]).ClassName = ClassName then
      Exit(TSeStyleObjectClass(RegObjects[i]));

  Result := nil;
end;

{ I/O Routines }

procedure SaveStyleObject(Stream: TStream; StyleObject: TSeStyleObject);
var
  BinStream:TMemoryStream;
  StrStream: TStringStream;
  s: string;
begin
  if StyleObject = nil then Exit;

  BinStream := TMemoryStream.Create;
  try
    StrStream := TStringStream.Create(S);
    try
      BinStream.WriteComponent(StyleObject);
      BinStream.Seek(0, soFromBeginning);
      ObjectBinaryToText(BinStream, StrStream);
      StrStream.Seek(0, soFromBeginning);
      S := StrStream.DataString;
    finally
      StrStream.Free;
    end;
  finally
    BinStream.Free
  end;

  WriteString(Stream, StyleObject.ClassName);
  WriteString(Stream, S);
end;

function LoadStyleObject(Stream: TStream; Owner: TSeStyleObject): TSeStyleObject;
var
  StyleObjectClass: TSeStyleObjectClass;
  S, ClassName: string;
  StrStream:TStringStream;
  BinStream: TMemoryStream;
begin
  Result := nil;

  ClassName := ReadString(Stream);

  { make object }
  StyleObjectClass := GetStyleObjectClass(ClassName);
  if StyleObjectClass = nil then
  begin
    { try load TwStyle }
    if Length(ClassName) > 3 then
    begin
      ClassName[2] := 'S';
      ClassName[3] := 'e';
      StyleObjectClass := GetStyleObjectClass(ClassName);
    end;
  end;
  if StyleObjectClass = nil then
  begin
    { try load TwStyle }
    if Length(ClassName) > 3 then
    begin
      ClassName[2] := 'T';
      ClassName[3] := 'w';
      StyleObjectClass := GetStyleObjectClass(ClassName);
    end;
  end;

  if StyleObjectClass <> nil then
    Result := StyleObjectClass.Create(Owner);

  { Load object }
  if Result <> nil then
  begin
    S := ReadString(Stream);
    StrStream := TStringStream.Create(S);
    try
      BinStream := TMemoryStream.Create;
      try
        ObjectTextToBinary(StrStream, BinStream);
        BinStream.Seek(0, soFromBeginning);

        BinStream.ReadComponent(Result);

        { Set Charset }
        Result.SetCharset(DEFAULT_CHARSET);
      finally
        BinStream.Free;
      end;
    finally
      StrStream.Free;
    end;
  end;
end;

procedure SaveStyleObjectBinary(Stream: TStream; StyleObject: TSeStyleObject);
var
  MemStream: TMemoryStream;
  Size: Cardinal;
begin
  WriteString(Stream, StyleObject.ClassName);
  MemStream := TMemoryStream.Create;
  try
    MemStream.WriteComponent(StyleObject);
    Size := MemStream.Size;
    Stream.WriteBuffer(Size, SizeOf(Size));
    Stream.WriteBuffer(MemStream.Memory^, Size);
  finally
    MemStream.Free;
  end;
end;

function LoadStyleObjectBinary(Stream: TStream; Owner: TSeStyleObject): TSeStyleObject;
var
  StyleObjectClass: TSeStyleObjectClass;
  ClassName: string;
  MemStream: TMemoryStream;
  Size: Cardinal;
begin
  Result := nil;

  ClassName := ReadString(Stream);

  { make object }
  StyleObjectClass := GetStyleObjectClass(ClassName);
  if StyleObjectClass = nil then
  begin
    { try load TwStyle }
    if Length(ClassName) > 3 then
    begin
      ClassName[2] := 'S';
      ClassName[3] := 'e';
      StyleObjectClass := GetStyleObjectClass(ClassName);
    end;
  end;
  if StyleObjectClass = nil then
  begin
    { try load TwStyle }
    if Length(ClassName) > 3 then
    begin
      ClassName[2] := 'T';
      ClassName[3] := 'w';
      StyleObjectClass := GetStyleObjectClass(ClassName);
    end;
  end;

  if StyleObjectClass <> nil then
    Result := StyleObjectClass.Create(Owner);

  { Load object }
  if Result <> nil then
  begin
    Stream.ReadBuffer(Size, SizeOf(Size));
    MemStream := TMemoryStream.Create;
    try
      MemStream.SetSize(Size);
      Stream.ReadBuffer(MemStream.Memory^, Size);
      MemStream.ReadComponent(Result);
    finally
      MemStream.Free;
    end;
    { Set Charset }
    Result.SetCharset(DEFAULT_CHARSET);
  end;
end;

{ TSeStyleObject ===============================================================}

constructor TSeStyleObject.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEnabled := True;
  FVisible := True;
  FFixPosition := False;
  FStopDrawChilds := False;
  FColor := clWhite;
  FFont := TFont.Create;
  with FFont do
  begin
    FFont.Name := DefaultFontName;
    FFont.Size := DefaultFontSize;
  end;
  FTextAlign := taLeft;
  FMasked := True;
  FCornerRadius := 0;
  FFocusEffect := False;
  FParams := '';
  FLocked := True;
end;

destructor TSeStyleObject.Destroy;
begin
  FreeAndNil(FFont);
  inherited Destroy;
end;

procedure TSeStyleObject.AfterLoad;
var
  i: integer;
begin
  if Count = 0 then Exit;

  for i := 0 to Count-1 do
    Objects[i].AfterLoad;

  FLoaded := True;
end;

{ Protected ===================================================================}

procedure TSeStyleObject.Assign(Source: TPersistent);
var
  i: integer;
begin
  if Source is TSeStyleObject then
  begin
    { Assign }
    Name := TSeStyleObject(Source).Name;

    FMasked := TSeStyleObject(Source).FMasked;
    FAlign := TSeStyleObject(Source).FAlign;
    FKind := TSeStyleObject(Source).FKind;
    FAlign := TSeStyleObject(Source).FAlign;
    FBitmaps := TSeStyleObject(Source).FBitmaps;
    FColor := TSeStyleObject(Source).FColor;
    FDrawIfOwner := TSeStyleObject(Source).FDrawIfOwner;
    FFont.Assign(TSeStyleObject(Source).FFont);
    FKind := TSeStyleObject(Source).FKind;
    FTextAlign := TSeStyleObject(Source).FTextAlign;
    FTextEffect := TSeStyleObject(Source).FTextEffect;
    FTextMarginLeft := TSeStyleObject(Source).FTextMarginLeft;
    FTextMarginRight := TSeStyleObject(Source).FTextMarginRight;
    FTextMarginTop := TSeStyleObject(Source).FTextMarginTop;
    FTextMarginRightStretch := TSeStyleObject(Source).FTextMarginRightStretch;
    FTextShiftSysIton := TSeStyleObject(Source).FTextShiftSysIton;

    FMarginLeft := TSeStyleObject(Source).FMarginLeft;
    FMarginTop := TSeStyleObject(Source).FMarginTop;
    FMarginRight := TSeStyleObject(Source).FMarginRight;
    FMarginBottom := TSeStyleObject(Source).FMarginBottom;

    FFixPosition := TSeStyleObject(Source).FFixPosition;
    FParams := TSeStyleObject(Source).FParams;
    FLocked := TSeStyleObject(Source).FLocked;

    { Copy Objects }
    for i := 0 to TSeStyleObject(Source).Count - 1 do
    begin
      TSeStyleObject(Source).Objects[i].CreateCopy(Self);
    end;

    { Bounds }
    FLeft := TSeStyleObject(Source).Left;
    FTop := TSeStyleObject(Source).Top;
    FWidth := TSeStyleObject(Source).Width;
    FHeight := TSeStyleObject(Source).Height;
  end
  else
    inherited Assign(Source);
end;

function TSeStyleObject.CreateCopy(AOwner: TSeStyleObject): TSeStyleObject;
var
  ClassType: TSeStyleObjectClass;
begin
  ClassType := GetStyleObjectClass(ClassName);
  if ClassType <> nil then
  begin
    Result := ClassType.Create(AOwner);
    Result.Assign(Self);
  end
  else
    Result := nil;
end;

{ BiDi Mode }

function TSeStyleObject.DrawTextBiDiModeFlags(Flags: Integer): Longint;
begin
  Result := Flags;
  { do not change center alignment }
  if UseRightToLeftAlignment then
    if Result and DT_RIGHT = DT_RIGHT then
      Result := Result and not DT_RIGHT { removing DT_RIGHT, makes it DT_LEFT }
    else if not (Result and DT_CENTER = DT_CENTER) then
      Result := Result or DT_RIGHT;
  Result := Result or DrawTextBiDiModeFlagsReadingOnly;
end;

function TSeStyleObject.DrawTextBiDiModeFlagsReadingOnly: Longint;
begin
  if UseRightToLeftReading then
    Result := DT_RTLREADING
  else
    Result := 0;
end;

function TSeStyleObject.UseRightToLeftAlignment: Boolean;
begin
  Result := SysLocale.MiddleEast and (BiDiMode <> bdLeftToRight);
end;

function TSeStyleObject.UseRightToLeftReading: Boolean;
begin
  Result := SysLocale.MiddleEast and (BiDiMode = bdRightToLeft);
end;

{ Aligning ====================================================================}

procedure TSeStyleObject.Aligning;
var
  i: integer;
  R: TRect;
  Canvas: TCanvas;
  ChildObjects: array of TSeStyleObject;
  ChildCount: integer;
begin
  ChildCount := GetCount - 1;
  SetLength(ChildObjects, ChildCount + 1);
  for i := 0 to ChildCount do
    ChildObjects[i] := Objects[i];

  R := BoundsRect;
  { most top & most bottom align }
  for i := 0 to ChildCount do
  begin
    if (ChildObjects[i].Align = saMostTop) and (ChildObjects[i].FVisible) then
    with ChildObjects[i] do
    begin
      BoundsRect := Rect(R.Left, R.Top, R.Right, R.Top+Height);
      Inc(R.Top, Height);
    end;
    if (ChildObjects[i].Align = saMostBottom) and (ChildObjects[i].FVisible) then
    with ChildObjects[i] do
    begin
      BoundsRect := Rect(R.Left, R.Bottom-Height, R.Right, R.Bottom);
      Dec(R.Bottom, Height);
    end;
  end;
  { most left & right align }
  for i := 0 to ChildCount do
  begin
    if (ChildObjects[i].Align = saMostLeft) and (ChildObjects[i].FVisible) then
    with ChildObjects[i] do
    begin
      BoundsRect := Rect(R.Left, R.Top, R.Left+Width, R.Bottom);
      Inc(R.Left, Width);
    end;
    if (ChildObjects[i].Align = saMostRight) and (ChildObjects[i].FVisible) then
    with ChildObjects[i] do
    begin
      BoundsRect := Rect(R.Right-Width, R.Top, R.Right, R.Bottom);
      Dec(R.Right, Width);
    end;
  end;
  { top & bottom align }
  for i := 0 to ChildCount do
  begin
    if (ChildObjects[i].Align = saTop) and (ChildObjects[i].FVisible) then
    with ChildObjects[i] do
    begin
      BoundsRect := Rect(R.Left, R.Top, R.Right, R.Top+Height);
      Inc(R.Top, Height);
    end;
    if (ChildObjects[i].Align = saBottom) and (ChildObjects[i].FVisible) then
    with ChildObjects[i] do
    begin
      BoundsRect := Rect(R.Left, R.Bottom-Height, R.Right, R.Bottom);
      Dec(R.Bottom, Height);
    end;
  end;
  { left & right align }
  for i := 0 to ChildCount do
  begin
    if (ChildObjects[i].Align = saLeft) and (ChildObjects[i].FVisible) then
    with ChildObjects[i] do
    begin
      BoundsRect := Rect(R.Left, R.Top, R.Left+Width, R.Bottom);
      Inc(R.Left, Width);
    end;
    if (ChildObjects[i].Align = saRight) and (ChildObjects[i].FVisible) then
    with ChildObjects[i] do
    begin
      BoundsRect := Rect(R.Right-Width, R.Top, R.Right, R.Bottom);
      Dec(R.Right, Width);
    end;
  end;
  { text align }
  for i := ChildCount Downto 0 do
    if (ChildObjects[i].FVisible) and (ChildObjects[i].Align = saText) then
    with ChildObjects[i] do
    begin
      Canvas := TCanvas.Create;
      try
        Canvas.Handle := GetDC(0);
        try
          Canvas.Font := Font;
          Width := TextMarginLeft + TextMarginRight + TextWidth(Canvas, FText);
        finally
          ReleaseDC(0, Canvas.Handle);
        end;
      finally
        Canvas.Handle := 0;
        Canvas.Free;
      end;
    end;
  { center align }
  for i := ChildCount Downto 0 do
    if (ChildObjects[i].Align = saCenter) and (ChildObjects[i].FVisible) then
    with ChildObjects[i] do
    begin
      BoundsRect := Rect(R.Left + (R.Right-R.left-Width) div 2,
        R.Top + (R.Bottom-R.Top-Height) div 2,
        R.Left + (R.Right-R.Left-Width) div 2 + FWidth,
        R.Top + (R.Bottom-R.Top-Height) div 2 + FHeight);
    end;
  { client align }
  for i := 0 to ChildCount do
    if (ChildObjects[i].Align = saClient) and (ChildObjects[i].FVisible) then
      ChildObjects[i].BoundsRect := Rect(R.Left, R.Top, R.Right, R.Bottom);
  { saAll }
  for i := 0 to ChildCount do
    if (ChildObjects[i].Align = saAll) and (ChildObjects[i].FVisible) then
      ChildObjects[i].BoundsRect := Rect(0, 0, FWidth, FHeight);
  { other align }
  for i := 0 to ChildCount do
  begin
    if (ChildObjects[i].Align = saTopRightAngle) then
    begin
      ChildObjects[i].BoundsRect := Rect(FWidth - ChildObjects[i].Width,
        0,
        Width,
        ChildObjects[i].Height);
    end;
    if (ChildObjects[i].Align = saTopLeft) and (FOldWidth > 0) then
    begin
      ChildObjects[i].BoundsRect := Rect(ChildObjects[i].FLeft + (FLeft - FOldLeft),
        ChildObjects[i].FTop + (FTop - FOldTop),
        ChildObjects[i].FLeft + (FLeft - FOldLeft) + ChildObjects[i].Width,
        ChildObjects[i].FTop + (FTop - FOldTop) + ChildObjects[i].Height);
    end;
    if (ChildObjects[i].Align = saTopRight) and (FOldWidth > 0) then
    begin
      ChildObjects[i].BoundsRect := Rect((FLeft - FOldLeft) + FWidth - (FOldWidth - ChildObjects[i].FLeft),
        ChildObjects[i].FTop + (FTop - FOldTop),
        (FLeft - FOldLeft) + FWidth - (FOldWidth - ChildObjects[i].FLeft) + ChildObjects[i].Width,
        ChildObjects[i].FTop + (FTop - FOldTop) + ChildObjects[i].Height);
    end;
    if ChildObjects[i].Align = saBottomLeft then
      ChildObjects[i].FTop := FHeight - (FOldHeight - ChildObjects[i].FTop);
    if ChildObjects[i].Align = saBottomRight then
    begin
      ChildObjects[i].FLeft := FWidth - ChildObjects[i].FLeft;
      ChildObjects[i].FTop := FHeight - ChildObjects[i].FTop;
    end;
  end;
end;

procedure TSeStyleObject.AligningAll;
var
  i: integer;
begin
  Aligning;

  if Count > 0 then
    for i := 0 to Count-1 do
      Objects[i].AligningAll;
end;

procedure TSeStyleObject.Invalidate;
begin
  if not Visible then Exit;

  if FParentControl <> nil then
    SendMessage(FParentControl.Handle, WM_INVALIDATESTYLEOBJECT, 0, Integer(Self));
end;

{ Drawing =====================================================================}

procedure TSeStyleObject.Draw(Canvas: TCanvas; const ClipRect: TRect; DPI: Integer = 0);
var
  R: TRect;
begin
  if not Visible then Exit;
  if FWidth <= 0 then Exit;
  if FHeight <= 0 then Exit;
  if not IsRectEmpty(ClipRect) and not IntersectRect(R, BoundsRect, ClipRect) then Exit;
  if FKind = skRoundMask then Exit;
  if FKind = skMaskOnly then Exit;

  if (Count = 0) or ((Count > 0) and (FDrawIfOwner)) then
  begin
    if FColor = Vcl.Graphics.clNone then
    begin
      if DesignMode then
        FillRect(Canvas, BoundsRect, SeColorToColor(seTransparent));
    end
    else
      FillRect(Canvas, BoundsRect, FColor);

    DrawObjectText(Canvas, DPI);
  end;

  DrawChild(Canvas, ClipRect, DPI);
end;

procedure TSeStyleObject.SafeDraw(Canvas: TCanvas; const ClipRect: TRect; DPI: Integer = 0);
var
  B: TSeBitmap;
  SaveRect: TRect;
begin
  if (MarginTop + MarginBottom > BoundsRect.Height) then
  begin
    SaveRect := BoundsRect;
    B := TSeBitmap.Create;
    try
      B.SetSize(BoundsRect.Width, MarginTop + MarginBottom);
      B.Clear(0);
      BoundsRect := TRect.Create(0, 0, B.Width, B.Height);
      Draw(B.Canvas, NullRect, DPI);
      B.AlphaBlend := True;
      B.Draw(Canvas, SaveRect, TRect.Create(0, 0, B.Width, B.Height));
      BoundsRect := SaveRect;
    finally
      B.Free;
    end;
  end
  else
  if (MarginLeft + MarginRight > BoundsRect.Width) then
  begin
    SaveRect := BoundsRect;
    B := TSeBitmap.Create;
    try
      B.SetSize(MarginLeft + MarginRight, BoundsRect.Height);
      B.Clear(0);
      BoundsRect := TRect.Create(0, 0, B.Width, B.Height);
      Draw(B.Canvas, NullRect, DPI);
      B.AlphaBlend := True;
      B.Draw(Canvas, SaveRect, TRect.Create(0, 0, B.Width, B.Height));
      BoundsRect := SaveRect;
    finally
      B.Free;
    end;
  end
  else
    Draw(Canvas, ClipRect, DPI);
end;

procedure TSeStyleObject.DrawChild(Canvas: TCanvas; const ClipRect: TRect; DPI: Integer = 0);
var
  Child: TSeStyleObject;
  R: TRect;
  i: integer;
begin
  if (Count > 0) and not FStopDrawChilds then
  begin
    { Draw childs }
    for i := 0 to Count-1 do
    begin
      Child := Objects[i];
      if (Child.Visible) and (Child.Width > 0) and (Child.Height > 0) and Child.CanDraw(DPI) then
      begin
        if IsRectEmpty(ClipRect) then
          Child.Draw(Canvas, ClipRect, DPI)
        else
          if IntersectRect(R, Child.BoundsRect, ClipRect) then
            Child.Draw(Canvas, ClipRect, DPI)
      end;
    end;
  end;
end;

procedure TSeStyleObject.DrawObjectText(Canvas: TCanvas; DPI: Integer = 0);
var
  SaveColor: TColor;
  Flags: integer;
  R: TRect;
  S: string;
begin
  if FText = WideChar('-') then
  begin
    { Separator }
    R := BoundsRect;
    InflateRect(R, -3, -(R.Height div 2 - 1));
    R.Bottom := R.Top + 1;
    FillRect(Canvas, R, GetFont.Color);

    Exit;
  end;
  { Draw text }
  if FText <> '' then
  begin
    case FTextAlign of
      taTopLeft: Flags := DT_SINGLELINE or DT_TOP or DT_LEFT;
      taTopCenter: Flags := DT_SINGLELINE or DT_TOP or DT_CENTER;
      taTopRight: Flags := DT_SINGLELINE or DT_TOP or DT_RIGHT;
      taLeft: Flags := DT_SINGLELINE or DT_VCENTER or DT_LEFT;
      taCenter: Flags := DT_SINGLELINE or DT_VCENTER or DT_CENTER;
      taRight: Flags := DT_SINGLELINE or DT_VCENTER or DT_RIGHT;
      taBottomLeft: Flags := DT_SINGLELINE or DT_BOTTOM or DT_LEFT;
      taBottomCenter: Flags := DT_SINGLELINE or DT_BOTTOM or DT_CENTER;
      taBottomRight: Flags := DT_SINGLELINE or DT_BOTTOM or DT_RIGHT;
    else
      Flags := DT_SINGLELINE or DT_CENTER or DT_VCENTER;
    end;
    R := BoundsRect;
    Inc(R.Left, FTextMarginLeft);
    Dec(R.Right, FTextMarginRight);
    Inc(R.Top, FTextMarginTop);
    { Set font }
    Canvas.Font := FFont;
    if not FEnabled then
      Canvas.Font.Color := clGray;
    { Set text }
    if FKind = skTitle then
      S := TrimStr(Canvas.Handle, FText, FWidth - FTextMarginLeft - FTextMarginRight)
    else
      S := FText;

    Flags := DrawTextBiDiModeFlags(Flags) or DT_NOPREFIX;

    { Draw text }
    case FTextEffect of
      teNone: DrawText(Canvas, S, R, Flags);
      teShadow: begin
        { Draw shadow }
        SaveColor := Canvas.Font.Color;
        Canvas.Font.Color := clGray;
        OffsetRect(R, 1, 1);
        DrawText(Canvas, S, R, Flags);
        { Draw text }
        Canvas.Font.Color := SaveColor;
        OffsetRect(R, -1, -1);
        DrawText(Canvas, S, R, Flags);
      end;
    end;
  end;

  if FRighTSext <> '' then
  begin
    { Draw FRighTSext }
    case FTextAlign of
      taTopLeft: Flags := DT_SINGLELINE or DT_TOP or DT_RIGHT;
      taTopCenter: Flags := DT_SINGLELINE or DT_TOP or DT_CENTER;
      taTopRight: Flags := DT_SINGLELINE or DT_TOP or DT_LEFT;
      taLeft: Flags := DT_SINGLELINE or DT_VCENTER or DT_RIGHT;
      taCenter: Flags := DT_SINGLELINE or DT_VCENTER or DT_CENTER;
      taRight: Flags := DT_SINGLELINE or DT_VCENTER or DT_LEFT;
      taBottomLeft: Flags := DT_SINGLELINE or DT_BOTTOM or DT_RIGHT;
      taBottomCenter: Flags := DT_SINGLELINE or DT_BOTTOM or DT_CENTER;
      taBottomRight: Flags := DT_SINGLELINE or DT_BOTTOM or DT_LEFT;
    else
      Flags := DT_SINGLELINE or DT_CENTER or DT_VCENTER;
    end;
    R := BoundsRect;
    Inc(R.Left, FTextMarginLeft);
    Dec(R.Right, FTextMarginRight);
    Inc(R.Top, FTextMarginTop);

    if FParentControl <> nil then
      Flags := FParentControl.DrawTextBiDiModeFlags(Flags);

    DrawText(Canvas, FRighTSext, R, Flags)
  end;
end;

{ Font }

function TSeStyleObject.GetFont: TFont;
begin
  Result := FFont;
end;

{ Region ======================================================================}

function TSeStyleObject.GetRegion: HRgn;
var
  i: integer;
  ChildMask: HRgn;
begin
  if (FKind = skTranparent) or (FKind = skTranparentOnly) or (FKind = skNoMask) then
  begin
    Result := 0;
    Exit;
  end;
  if (FWidth <= 0) or (FHeight <= 0) then
  begin
    Result := 0;
    Exit;
  end;

  { Add child mask }
  if Count > 0 then
  begin
    if DrawIfOwner or not FMasked then
    begin
      Result := CreateRegion;
      Exit;
    end;

    Result := CreateRectRgn(0, 0, 0, 0);

    for i := 0 to Count-1 do
    begin
      if not Objects[i].Visible then Continue;

      ChildMask := Objects[i].GetRegion;
      if ChildMask <> 0 then
      begin
        CombineRgn(Result, Result, ChildMask, RGN_OR);
        DeleteObject(ChildMask);
      end;
    end;
  end
  else
    Result := CreateRegion;
end;

function TSeStyleObject.CreateRegion: HRgn;
begin
  if FKind = skRoundMask then
    Result := CreateRoundRectRgn(FLeft, FTop, FLeft + FWidth + 1, FTop + FHeight + 1, FMarginLeft, FMarginTop)
  else
    Result := CreateRectRgn(FLeft, FTop, FLeft + FWidth, FTop + FHeight);
end;

{ Events ======================================================================}

procedure TSeStyleObject.MouseHover;
begin
end;

procedure TSeStyleObject.MouseLeave;
begin
end;

procedure TSeStyleObject.MouseDouble(Button: TMouseButton; X, Y: integer);
begin

end;

procedure TSeStyleObject.MouseDown(Button: TMouseButton; X, Y: integer);
begin

end;

procedure TSeStyleObject.MouseMove(Shift: TShiftState; X, Y: integer);
begin

end;

procedure TSeStyleObject.MouseUp(Button: TMouseButton; X, Y: integer);
begin

end;

{ Children ====================================================================}

procedure TSeStyleObject.SetCharset(CharSet: TFontCharset);
var
  i: integer;
begin
  Font.Charset := CharSet;

  if Count > 0 then
    for i := 0 to Count - 1 do
      Objects[i].SetCharSet(CharSet);
end;

procedure TSeStyleObject.Add(StyleObject: TSeStyleObject);
begin
  InsertComponent(StyleObject);
end;

procedure TSeStyleObject.Remove(StyleObject: TSeStyleObject);
begin
  RemoveComponent(StyleObject);
end;

function TSeStyleObject.FindObjectByAction(Action: TSeAction): TSeStyleObject;
var
  i: integer;
begin
  Result := nil;
  if (Self is TSeSystemButton) and (TSeSystemButton(Self).FAction = Action) then
  begin
    Result := Self;
    Exit;
  end;
  if Count = 0 then Exit;
  for i := 0 to Count-1 do
  begin
    Result := Objects[i].FindObjectByAction(Action);
    if Result <> nil then
      Break;
  end;
end;

function TSeStyleObject.FindObjectByKind(AKind: TSeKind): TSeStyleObject;
var
  i: integer;
begin
  Result := nil;
  if FKind = AKind then
  begin
    Result := Self;
    Exit;
  end;
  if Count = 0 then Exit;
  for i := 0 to Count-1 do
  begin
    Result := Objects[i].FindObjectByKind(AKind);
    if Result <> nil then
      Break;
  end;
end;

function TSeStyleObject.FindObjectByName(const AName: string): TSeStyleObject;
var
  i: integer;
  SlashPos: integer;
  SlashMin: string;
begin
  Result := nil;
  if CompareText(Name, AName) = 0 then
  begin
    Result := Self;
    Exit;
  end;
  if Count = 0 then Exit;

  SlashPos := System.Pos('\', AName);
  if SlashPos > 0 then
    SlashMin := System.Copy(AName, 1, SlashPos - 1);
  for i := 0 to Count-1 do
  begin
    if SlashPos > 0 then
    begin
      Result := Objects[i].FindObjectByName(SlashMin);
    end
    else
      Result := Objects[i].FindObjectByName(AName);
    if Result <> nil then
      Break;
  end;
end;

function TSeStyleObject.FindObjectByPoint(Point: TPoint): TSeStyleObject;
var
  i: integer;
  StyleObject: TSeStyleObject;
begin
  Result := nil;
  if not FVisible then
    Exit;
  if not PtInRect(BoundsRect, Point) then
    Exit;
  if (FKind = skTranparent) and not DesignMode then
    Exit;

  if Count = 0 then
  begin
    if PtInRect(BoundsRect, Point) then
      Result := Self;
  end
  else
  begin
    for i := 0 to Count-1 do
    begin
      StyleObject := Objects[i].FindObjectByPoint(Point);
      if StyleObject <> nil then
      begin
        Result := StyleObject;
      end;
    end;
    if Result = nil then
      Result := Self;
  end;
end;

function TSeStyleObject.FindObjectByPointAll(Point: TPoint): TSeStyleObject;
var
  i: integer;
  StyleObject: TSeStyleObject;
begin
  Result := nil;
  if not PtInRect(BoundsRect, Point) then
    Exit;
  if (FKind = skTranparent) and not DesignMode then
    Exit;

  if (Count = 0) then
  begin
    if (FKind = skTranparentOnly) then Exit;
    if (FKind = skRoundMask) then Exit;
    if (FKind = skMaskOnly) then Exit;

    if PtInRect(BoundsRect, Point) then
      Result := Self;
  end
  else
  begin
    for i := 0 to Count-1 do
    begin
      StyleObject := Objects[i].FindObjectByPointAll(Point);
      if StyleObject <> nil then
        Result := StyleObject;
    end;

    if (FKind = skTranparentOnly) then Exit;
    if (FKind = skRoundMask) then Exit;
    if FKind = skMaskOnly then Exit;

    if Result = nil then
      Result := Self;
  end;
end;

{ Custom property =============================================================}

procedure TSeStyleObject.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('Objects', ReadData, WriteData, True);
  Filer.DefineProperty('Locked', ReadLocked, WriteLocked, True);
end;

procedure TSeStyleObject.ReadLocked(Reader: TReader);
begin
  FLocked := Reader.ReadBoolean;
end;

procedure TSeStyleObject.WriteLocked(Writer: TWriter);
begin
  Writer.WriteBoolean(FLocked);
end;

const NewObjectFormatFlag = $F0000;

procedure TSeStyleObject.ReadData(Stream: TStream);
var
  i, LCount: integer;
begin
  { Load Count }
  Stream.Read(LCount, SizeOf(Integer));

  { Load Objects }
  if LCount and NewObjectFormatFlag = NewObjectFormatFlag then
  begin
    { New format }
    LCount := LCount and not NewObjectFormatFlag;
    for i := 0 to LCount-1 do
      LoadStyleObjectBinary(Stream, Self);
  end
  else
  begin
    { Old format }
    for i := 0 to LCount-1 do
      LoadStyleObject(Stream, Self);
  end;
end;

procedure TSeStyleObject.WriteData(Stream: TStream);
var
  i, LCount, SCount: integer;
begin
  LCount := GetCount;
  { Save Count }
  SCount := LCount or NewObjectFormatFlag;
  Stream.Write(SCount, SizeOf(Integer));
  { Load Objects }
  for i := 0 to LCount-1 do
    SaveStyleObjectBinary(Stream, Objects[i]);
end;

{ Properties ==================================================================}

procedure TSeStyleObject.SetBoundsRect(const Value: TRect);
begin
  if (Value.Left = FLeft) and (Value.Top = FTop) and (Value.Right = FLeft + FWidth) and
     (Value.Bottom = FTop + FHeight)
  then
  begin
  end
  else
  begin
    { Change }
    FOldLeft := FLeft;
    FOldTop := FTop;
    FOldWidth := FWidth;
    FOldHeight := FHeight;

    FLeft := Value.Left;
    FTop := Value.Top;
    FWidth := Value.Right - Value.Left;
    FHeight := Value.Bottom - Value.Top;
    if FWidth < 0 then FWidth := 0;
    if FHeight < 0 then FHeight := 0;
    Aligning;

    FOldLeft := FLeft;
    FOldTop := FTop;
    FOldWidth := FWidth;
    FOldHeight := FHeight;
  end;
end;

function TSeStyleObject.GetCount: integer;
begin
  Result := ComponentCount;
end;

function TSeStyleObject.GetObject(index: integer): TSeStyleObject;
begin
  Result := TSeStyleObject(Components[index]);
end;

function TSeStyleObject.GetBoundsRect: TRect;
begin
  Result := Rect(FLeft, FTop, FLeft + FWidth, FTop + FHeight);
end;

procedure TSeStyleObject.SetHeight(const Value: integer);
begin
  BoundsRect := Rect(FLeft, FTop, FLeft + FWidth, FTop + Value);
end;

procedure TSeStyleObject.SetWidth(const Value: integer);
begin
  BoundsRect := Rect(FLeft, FTop, FLeft + Value, FTop + FHeight);
end;

procedure TSeStyleObject.SetLeft(const Value: integer);
begin
  BoundsRect := Rect(Value, FTop, Value + FWidth, FTop + FHeight);
end;

procedure TSeStyleObject.SetTop(const Value: integer);
begin
  BoundsRect := Rect(FLeft, Value, FLeft + FWidth, Value + FHeight);
end;

procedure TSeStyleObject.SetBitmaps(const Value: TSeBitmapList);
var
  i: integer;
begin
  FBitmaps := Value;

  if Count = 0 then Exit;

  for i := 0 to Count-1 do
    Objects[i].Bitmaps := Value;
end;

procedure TSeStyleObject.SetBiDiMode(const Value: TBiDiMode);
var
  i: integer;
begin
  FBiDiMode := Value;

  if Count = 0 then Exit;

  for i := 0 to Count-1 do
    Objects[i].BiDiMode := Value;
end;

procedure TSeStyleObject.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TSeStyleObject.SetActive(const Value: boolean);
var
  i: integer;
begin
  if FActive <> Value then
  begin
    FActive := Value;

    if Count = 0 then Exit;

    for i := 0 to Count-1 do
      Objects[i].Active := Value;
  end;
end;

procedure TSeStyleObject.SetParentControl(const Value: TWinControl);
var
  i: integer;
begin
  FParentControl := Value;

  if FParentControl <> nil then
    BiDiMode := FParentControl.BiDiMode;

  if Count = 0 then Exit;

  for i := 0 to Count-1 do
    Objects[i].ParentControl := Value;
end;

function TSeStyleObject.GetNameExcludeDPI: String;
var
  I: Integer;
begin
  Result := Name;
  I := Pos('15x', Name);
  if I = 0 then
    I := Pos('20x', Name);
  if I <> 0 then
    Delete(Result, I, 3);
end;

function TSeStyleObject.GetDPI: Integer;
begin
  Result := DPI_DEFAULT;
  if EndsText('15x', Name) then
    Result := DPI_15x
  else
  if EndsText('20x', Name) then
    Result := DPI_20x;
end;

function TSeStyleObject.GetScaledObject(DPI: Integer): TseStyleObject;
var
  LDPI: Integer;
begin
  Result := nil;
  if DPI <= DPI_DEFAULT then
    Exit(nil);
  LDPI := GetDPI;
  if (LDPI = DPI_DEFAULT) and (Owner <> nil) and (Owner is TseStyleObject) then
  begin
    if DPI <= DPI_15x then
    begin
      Result := TseStyleObject(Owner).FindObjectByName(Name + '15x');
      if Result = nil then
        Result := TseStyleObject(Owner).FindObjectByName(Name + '20x');
    end
    else
    begin
      Result := TseStyleObject(Owner).FindObjectByName(Name + '20x');
      if Result = nil then
        Result := TseStyleObject(Owner).FindObjectByName(Name + '15x');
    end;
  end;
end;

function TSeStyleObject.CanDraw(DPI: Integer): Boolean;
var
  LDPI: Integer;
  LName: String;
begin
  Result := False;
  LDPI := GetDPI;
  LName := GetNameExcludeDPI;
  if LDPI = DPI then
    Result := True
  else
  if (Owner <> nil) and (Owner is TseStyleObject) then
    case LDPI of
      DPI_DEFAULT:
        if DPI > DPI_DEFAULT then
          Result := (TseStyleObject(Owner).FindObjectByName(LName + '15x') = nil) and
                    (TseStyleObject(Owner).FindObjectByName(LName + '20x') = nil)
        else
          Result := True;
      DPI_15x:
        if (DPI > DPI_DEFAULT) and (DPI <= DPI_15x) then
          Result := True
        else
        if DPI > DPI_15x then
          Result := (TseStyleObject(Owner).FindObjectByName(LName + '20x') = nil);
      DPI_20x:
        if DPI > DPI_15x then
          Result := True
        else
        if (DPI > DPI_DEFAULT) and (DPI <= DPI_15x) then
          Result := (TseStyleObject(Owner).FindObjectByName(LName + '15x') = nil);
    end;
end;

procedure TSeStyleObject.SetState(const Value: TSeState);
var
  i: integer;
begin
  FState := Value;

  if Count = 0 then Exit;
  for i := 0 to Count-1 do
    Objects[i].State := Value;
end;

procedure TSeStyleObject.SetSysButtons(const Value: TSeWindowButtons);
var
  i: integer;
begin
  FSysButtons := Value;

  if Count = 0 then Exit;
  for i := 0 to Count-1 do
    Objects[i].SysButtons := Value;
end;

procedure TSeStyleObject.SetMarginBottom(const Value: integer);
begin
  FMarginBottom := Value;
end;

procedure TSeStyleObject.SetMarginLeft(const Value: integer);
begin
  FMarginLeft := Value;
end;

procedure TSeStyleObject.SetMarginRight(const Value: integer);
begin
  FMarginRight := Value;
end;

procedure TSeStyleObject.SetMarginTop(const Value: integer);
begin
  FMarginTop := Value;
end;

procedure TSeStyleObject.SetCornerRadius(const Value: integer);
begin
  if Value >= 0 then Self.FCornerRadius := Value;
end;

{ TSeActiveObject =========================================================}

constructor TSeActiveObject.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActiveFont := TFont.Create;
  with FActiveFont do
  begin
    FFont.Name := DefaultFontName;
    FFont.Size := DefaultFontSize;
  end;
  FDisabledFont := TFont.Create;
  with FDisabledFont do
  begin
    FFont.Name := DefaultFontName;
    FFont.Size := DefaultFontSize;
  end;
end;

destructor TSeActiveObject.Destroy;
begin
  FDisabledFont.Free;
  FActiveFont.Free;
  inherited Destroy;
end;

procedure TSeActiveObject.Assign(Source: TPersistent);
begin
  if Source is TSeActiveObject then
  begin
    inherited Assign(Source);
    ActiveColor := (Source as TSeActiveObject).ActiveColor;
    ActiveFont := (Source as TSeActiveObject).ActiveFont;
    DisabledFont := (Source as TSeActiveObject).DisabledFont;
  end
  else
    inherited;
end;

procedure TSeActiveObject.SetCharset(CharSet: TFontCharset);
begin
  inherited;
  FActiveFont.Charset := CharSet;
end;

function TSeActiveObject.GetFont: TFont;
begin
  case FState of
    ssHot, ssFocused, ssPressed: Result := FActiveFont;
  else
    Result := inherited GetFont;
  end;
end;

procedure TSeActiveObject.Draw(Canvas: TCanvas; const ClipRect: TRect; DPI: Integer = 0);
var
  SaveColor: TColor;
  SaveFont: TFont;
begin
  if FWidth <= 0 then Exit;
  if FHeight <= 0 then Exit;

  if (FActive) or (FState in [ssFocused, ssHot, ssPressed]) then
  begin
    SaveFont := FFont;
    SaveColor := FColor;
    try
      FColor := FActiveColor;
      FFont := FActiveFont;
      inherited Draw(Canvas, ClipRect, DPI);
    finally
      FFont := SaveFont;
      FColor := SaveColor;
    end;
  end
  else
    if FState = ssDisabled then
    begin
      SaveFont := FFont;
      SaveColor := FColor;
      try
        FColor := Color;
        FFont := FDisabledFont;
        inherited Draw(Canvas, ClipRect, DPI);
      finally
        FFont := SaveFont;
        FColor := SaveColor;
      end;
    end
    else
      inherited Draw(Canvas, ClipRect, DPI);
end;

procedure TSeActiveObject.SetActiveFont(const Value: TFont);
begin
  FActiveFont.Assign(Value);
end;

procedure TSeActiveObject.SetDisabledFont(const Value: TFont);
begin
  FDisabledFont.Assign(Value);
end;


{ TSeBitmapObject =========================================================}



constructor TSeBitmapObject.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBitmap := TSeBitmapLink.Create;
  FScaledBitmap := nil;
  FMaskedAngles := True;
  FMaskedBorder := True;
end;

destructor TSeBitmapObject.Destroy;
begin
  FreeAndNil(FBitmap);
  inherited Destroy;
end;

procedure TSeBitmapObject.AfterLoad;
var
  NewLink: TSeBitmapLink;
begin
  inherited AfterLoad;
  if FBitmaps <> nil then
  begin
    NewLink := FBitmaps.GetBitmapLink(FBitmap.Name, FBitmap.Rect);
    if NewLink <> nil then
    begin
      FreeAndNil(FBitmap);
      FBitmap := NewLink;
      FBitmap.CheckingMasked(Rect(FMarginLeft, FMarginTop, FMarginRight, FMarginBottom));
    end;
  end;
end;

procedure TSeBitmapObject.Assign(Source: TPersistent);
begin
  if Source is TSeBitmapObject then
  begin
    inherited Assign(Source);
    Bitmap.Assign((Source as TSeBitmapObject).Bitmap);
    FTileStyle := (Source as TSeBitmapObject).FTileStyle;
    BorderTileStyle := (Source as TSeBitmapObject).FBorderTileStyle;
    MaskedBorder := (Source as TSeBitmapObject).MaskedBorder;
    MaskedAngles := (Source as TSeBitmapObject).MaskedAngles;
  end
  else
    inherited;
end;

procedure TSeBitmapObject.DrawRect(Canvas: TCanvas; const MarginRect, MarginDstRect: TRect;
  ATileStyle: TscTileStyle; AMasked: boolean);
var
  R: TRect;
  W, H: integer;
  SaveTransparent, SaveTransparentAB: boolean;
  DrawBitmap: TseBitmapLink;
begin
  W := MarginRect.Width;
  H := MarginRect.Height;
  if W * H = 0 then Exit;
  DrawBitmap := FBitmap;
  SaveTransparent := False;
  SaveTransparentAB := False;

  if FScaledBitmap = nil then
  begin
    SaveTransparent := DrawBitmap.Image.Transparent;
    SaveTransparentAB := DrawBitmap.Image.AlphaBlend;
    DrawBitmap.Image.Transparent := AMasked;
    DrawBitmap.Image.AlphaBlend := AMasked;
  end
  else
    DrawBitmap := FScaledBitmap;
  case ATileStyle of
    tsTile, tsHorzCenterTile, tsVertCenterTile:
      begin
        R := MarginDstRect;
        OffsetRect(R, Left, Top);
        DrawBitmap.Image.Tile(Canvas, R, MarginRect);
      end;
    tsStretch, tsHorzCenterStretch, tsVertCenterStretch:
      begin
        R := MarginDstRect;
        OffsetRect(R, Left, Top);
        DrawBitmap.Image.Draw(Canvas, R, MarginRect);
      end;
    tsCenter:
      begin
        R := BoundsRect;
        DrawBitmap.Image.Draw(Canvas, Left + (Width - R.Width) div 2,
          Top + (Height - R.Height) div 2, MarginRect);
      end;
  end;
  if FScaledBitmap = nil then
  begin
    DrawBitmap.Image.Transparent := SaveTransparent;
    DrawBitmap.Image.AlphaBlend := SaveTransparentAB;
  end;
end;

procedure TSeBitmapObject.DrawNormal(Canvas: TCanvas; const ClipRect: TRect; DPI: Integer = 0);
var
  R: TRect;
  SaveTransparent: boolean;
  SaveAB: boolean;
  DrawBitmap: TseBitmapLink;
  L, T, W, H, BW, BH: Integer;
  LDPI: Integer;
begin
  if not FBitmap.Assigned or (Color = Vcl.Graphics.clNone) then
  begin
    inherited Draw(Canvas, ClipRect, DPI);
    DrawObjectText(Canvas);
    Aligning;
    DrawChild(Canvas, ClipRect, DPI);
    Exit;
  end;

  if FWidth <= 0 then Exit;
  if FHeight <= 0 then Exit;

  FScaledBitmap := nil;
  SaveTransparent := False;
  SaveAB := False;

  DrawBitmap := FBitmap;
  L := Left;
  T := Top;
  W := Width;
  H := Height;
  LDPI := GetDPI;

  if (DPI > DPI_DEFAULT) and (DPI <> LDPI) then
  begin
    W := MulDiv(W, DPI, LDPI);
    H := MulDiv(H, DPI, LDPI);
    BW := MulDiv(FBitmap.Rect.Width, DPI, LDPI);
    BH := MulDiv(FBitmap.Rect.Height, DPI, LDPI);
    if FTileStyle = tsCenter then
    begin
      L := Left + (Width - W) div 2;
      T := Top + (Height - H) div 2;
    end;
    FScaledBitmap := FBitmap.GetScaledBitmapLink(BW, BH, DPI);
    if FScaledBitmap <> nil then
      DrawBitmap := FScaledBitmap;
  end;

  if FScaledBitmap = nil then
  begin
    SaveTransparent := DrawBitmap.Image.Transparent;
    DrawBitmap.Image.Transparent := DrawBitmap.Masked;
    SaveAB := DrawBitmap.Image.AlphaBlend;
    DrawBitmap.Image.AlphaBlend := DrawBitmap.Masked;
    if FTileStyle <> tsTile then
      DrawBitmap.Image.Transparent := DrawBitmap.Masked;
  end;

  case FTileStyle of
    tsTile:
      begin
        if not IsRectEmpty(ClipRect) then
        begin
          DrawBitmap.Image.TileClip(Canvas, BoundsRect, ClipRect, DrawBitmap.Rect)
        end
        else
          DrawBitmap.Image.Tile(Canvas, BoundsRect, DrawBitmap.Rect)
      end;
    tsStretch:
      begin
        DrawBitmap.Image.Draw(Canvas, BoundsRect, DrawBitmap.Rect);
      end;
    tsCenter:
      begin
        R := DrawBitmap.Rect;
        DrawBitmap.Image.Draw(Canvas, L + (W - R.Width) div 2,
          T + (H - R.Height) div 2, DrawBitmap.Rect);
      end;
    tsVertCenterStretch:
      begin
        R := DrawBitmap.Rect;
        R.Bottom := R.Top + H;
        RectCenter(R, BoundsRect);
        DrawBitmap.Image.Draw(Canvas, R, DrawBitmap.Rect);
      end;
    tsVertCenterTile:
      begin
        R := DrawBitmap.Rect;
        R.Bottom := R.Top + H;
        RectCenter(R, BoundsRect);
        DrawBitmap.Image.Tile(Canvas, R, DrawBitmap.Rect);
      end;
    tsHorzCenterStretch:
      begin
        R := DrawBitmap.Rect;
        R.Right := R.Left + W;
        RectCenter(R, BoundsRect);
        DrawBitmap.Image.Draw(Canvas, R, DrawBitmap.Rect);
      end;
    tsHorzCenterTile:
      begin
        R := DrawBitmap.Rect;
        R.Right := R.Left + W;
        RectCenter(R, BoundsRect);
        DrawBitmap.Image.Tile(Canvas, R, DrawBitmap.Rect);
      end;
  end;

  if FScaledBitmap = nil then
  begin
    DrawBitmap.Image.Transparent := SaveTransparent;
    DrawBitmap.Image.AlphaBlend := SaveAB;
  end
  else
    FreeAndNil(FScaledBitmap);

  DrawObjectText(Canvas, DPI);
  Aligning;
  DrawChild(Canvas, ClipRect, DPI);
end;

procedure TSeBitmapObject.SafeDraw(Canvas: TCanvas; const ClipRect: TRect; DPI: Integer = 0);
var
  B: TSeBitmap;
  SaveRect: TRect;
begin
  if (TileStyle = tsCenter) and
     (FBitmap.Assigned and ((FBitmap.Rect.Width > BoundsRect.Width) or (FBitmap.Rect.Height > BoundsRect.Height)))
  then
  begin
    SaveRect := BoundsRect;
    B := TSeBitmap.Create;
    try
      B.SetSize(BoundsRect.Width, BoundsRect.Width);
      BoundsRect := TRect.Create(0, 0, B.Width, B.Height);
      Draw(B.Canvas, NullRect);
      B.Draw(Canvas, SaveRect, TRect.Create(0, 0, B.Width, B.Height));
      BoundsRect := SaveRect;
    finally
      B.Free;
    end;
  end
  else
    inherited;
end;

function TSeBitmapObject.CanScale: Boolean;
begin
  Result := ((FTileStyle = tsVertCenterStretch) or (FTileStyle = tsVertCenterTile) or
    (FTileStyle = tsHorzCenterStretch) or (FTileStyle = tsHorzCenterTile)) and
    ((FMarginLeft + FMarginRight = 0) or (FMarginTop + FMarginBottom = 0));
end;

procedure TSeBitmapObject.Draw(Canvas: TCanvas; const ClipRect: TRect; DPI: Integer = 0);
var
  SrcRect, DstRect, ObjectDstRect: TRect;
  SaveTransparent, SaveAB: boolean;
  ScaledObject: TseStyleObject;
  DrawBitmap: TseBitmapLink;
  W, H, BW, BH, ML, MT, MB, MR: Integer;
  LDPI: Integer;
  SaveLeft, SaveTop: Integer;
begin
  if not FBitmap.Assigned then Exit;
  if FWidth <= 0 then Exit;
  if FHeight <= 0 then Exit;
  if FKind = skMaskOnly then Exit;

  if DPI > DPI_DEFAULT then
  begin
    ScaledObject := GetScaledObject(DPI);
    if (ScaledObject <> nil) and (ScaledObject is TseBitmapObject) then
    begin
      ScaledObject.State := State;
      ScaledObject.Active := Active;
      ScaledObject.BoundsRect := BoundsRect;
      ScaledObject.Draw(Canvas, ClipRect, DPI);
      Exit;
    end;
  end;

  if (MarginLeft = 0) and (MarginTop = 0) and
     (MarginRight = 0) and (MarginBottom = 0) then
  begin
    DrawNormal(Canvas, ClipRect, DPI);
    Exit;
  end;

  FScaledBitmap := nil;

  SaveTransparent := False;
  SaveLeft := Left;
  SaveTop := Top;
  SaveAB := False;
  DrawBitmap := FBitmap;

  LDPI := GetDPI;

  W := Width;
  H := Height;
  ML := FMarginLeft;
  MT := FMarginTop;
  MB := FMarginBottom;
  MR := FMarginRight;

  if (DPI > DPI_DEFAULT) and (DPI <> LDPI) and CanScale then
  begin
    case FTileStyle of
      tsVertCenterStretch, tsHorzCenterTile:
      begin
        W := MulDiv(W, DPI, LDPI);
        Left := Left + (Width - W) div 2;
      end;
      tsHorzCenterStretch, tsVertCenterTile:
      begin
        H := MulDiv(H, DPI, LDPI);
        Top := Top + (Height - H) div 2;
      end;
      tsCenter:
      begin
        W := MulDiv(W, DPI, LDPI);
        H := MulDiv(H, DPI, LDPI);
        Left := Left + (Width - W) div 2;
        Top := Top + (Height - H) div 2;
      end;
    end;

    BW := MulDiv(FBitmap.Rect.Width, DPI, LDPI);
    BH := MulDiv(FBitmap.Rect.Height, DPI, LDPI);
    ML := MulDiv(ML, DPI, LDPI);
    MT := MulDiv(MT, DPI, LDPI);
    MB := MulDiv(MB, DPI, LDPI);
    MR := MulDiv(MR, DPI, LDPI);

    FScaledBitmap := FBitmap.GetScaledBitmapLink(BW, BH, DPI);
    if FScaledBitmap <> nil then
      DrawBitmap := FScaledBitmap;
  end;

  { Draw Face }
  if FScaledBitmap = nil then
  begin
    SaveTransparent := DrawBitmap.Image.Transparent;
    SaveAB := DrawBitmap.Image.AlphaBlend;
  end;

  case FTileStyle of
    tsTile, tsStretch: ObjectDstRect := Rect(0, 0, W, H);
    tsVertCenterStretch, tsVertCenterTile:
      begin
        ObjectDstRect := DrawBitmap.Rect;
        ObjectDstRect.Bottom := ObjectDstRect.Top + H;
        RectCenter(ObjectDstRect, Rect(0, 0, W, H));
      end;
    tsHorzCenterStretch, tsHorzCenterTile:
      begin
        ObjectDstRect := DrawBitmap.Rect;
        ObjectDstRect.Right := ObjectDstRect.Left + W;
        RectCenter(ObjectDstRect, Rect(0, 0, W, H));
      end;
    tsCenter:
      begin
        ObjectDstRect := DrawBitmap.Rect;
        RectCenter(ObjectDstRect, Rect(0, 0, W, H));
      end;
  end;

  { Draw Center Rect }
  with DrawBitmap.Rect do
    SrcRect := Rect(Left + ML, Top + MT, Right - MR,
      Bottom - MB);
  with ObjectDstRect do
    DstRect := Rect(Left + ML, Top + MT, Right - MR, Bottom - MB);
  DrawRect(Canvas, SrcRect, DstRect, FTileStyle, DrawBitmap.Masked);

  { Draw Top Border }
  with DrawBitmap.Rect do
    SrcRect := Rect(Left + ML, Top, Right - MR,
      Top + MT);
  with ObjectDstRect do
    DstRect := Rect(Left + ML, Top, Right - MR, Top + MT);
  DrawRect(Canvas, SrcRect, DstRect, FBorderTileStyle, DrawBitmap.MaskedBorder);

  { Draw Bottom Border }
  with DrawBitmap.Rect do
    SrcRect := Rect(Left + ML, Bottom - MB, Right - MR,
      Bottom);
  with ObjectDstRect do
    DstRect := Rect(Left + ML, Bottom - MB, Right - MR, Bottom);
  DrawRect(Canvas, SrcRect, DstRect, FBorderTileStyle, DrawBitmap.MaskedBorder);

  { Draw Left Border }
  with DrawBitmap.Rect do
    SrcRect := Rect(Left, Top + MT, Left + ML,
      Bottom - MB);
  with ObjectDstRect do
    DstRect := Rect(Left, Top + MT, Left + ML, Bottom - MB);
  DrawRect(Canvas, SrcRect, DstRect, FBorderTileStyle, DrawBitmap.MaskedBorder);

  { Draw Right Border }
  with DrawBitmap.Rect do
    SrcRect := Rect(Right - MR, Top + MT, Right,
      Bottom - MB);
  with ObjectDstRect do
    DstRect := Rect(Right - MR, Top + MT, Right,
      Bottom - MB);
  DrawRect(Canvas, SrcRect, DstRect, FBorderTileStyle, DrawBitmap.MaskedBorder);

  { Draw Angles }
  OffsetRect(ObjectDstRect, Left, Top);

  if FScaledBitmap = nil then  
  begin
    DrawBitmap.Image.Transparent := DrawBitmap.MaskedAngles;
    DrawBitmap.Image.Alphablend := DrawBitmap.MaskedAngles;
  end;  
  
  with DrawBitmap.Rect do
    SrcRect := Rect(Left, Top, Left + ML, Top + MT);
  DrawBitmap.Image.Draw(Canvas, ObjectDstRect.Left, ObjectDstRect.Top, SrcRect);

  with DrawBitmap.Rect do
    SrcRect := Rect(Right - MR, Top, Right, Top + MT);
  DrawBitmap.Image.Draw(Canvas, ObjectDstRect.Right - MR, ObjectDstRect.Top, SrcRect);

  with DrawBitmap.Rect do
    SrcRect := Rect(Left, Bottom - MB, Left + ML, Bottom);
  DrawBitmap.Image.Draw(Canvas, ObjectDstRect.Left, ObjectDstRect.Bottom - MB, SrcRect);

  with DrawBitmap.Rect do
    SrcRect := Rect(Right - MR, Bottom - MB, Right, Bottom);
  DrawBitmap.Image.Draw(Canvas, ObjectDstRect.Right - MR,
    ObjectDstRect.Bottom - MB, SrcRect);

  if FScaledBitmap = nil then
  begin
    DrawBitmap.Image.Transparent := SaveTransparent;
    DrawBitmap.Image.AlphaBlend := SaveAB;
  end
  else
    FreeAndNil(FScaledBitmap);

  Left := SaveLeft;
  Top := SaveTop;

  { Draw Text }
  DrawObjectText(Canvas, DPI);
  Aligning;
  DrawChild(Canvas, ClipRect, DPI);
end;

function TSeBitmapObject.CreateRegion: HRgn;
var
  TempImage: TSeBitmap;
  SaveRect: TRect;
begin
  Result := 0;
  if not FBitmap.Assigned then Exit;
  if FWidth <= 0 then Exit;
  if FHeight <= 0 then Exit;
  if (FKind = skClient) or (not FMasked) then
  begin
    Result := CreateRectRgn(FLeft, FTop, FLeft + FWidth, FTop + FHeight);
    Exit;
  end;

  { Make mask from bitmap }
  SaveRect := BoundsRect;
  try
    { Set new rect }
    BoundsRect := Rect(0, 0, FWidth, FHeight);
    { Draw to TempImage }
    TempImage := TSeBitmap.Create;
    try
      TempImage.SetSize(FWidth, FHeight);
      FillRect(TempImage.Canvas, Rect(0, 0, FWidth, FHeight), $7F007F);
      { Draw to Temp }
      if FKind = skMaskOnly then
      begin
        FKind := skNone;
        Draw(TempImage.Canvas, BoundsRect);
        FKind := skMaskOnly;
      end
      else
        Draw(TempImage.Canvas, BoundsRect);
      { Create region }
      Result := CreateRegionFromBitmap(TempImage, SaveRect.Left, SaveRect.Top);
    finally
      TempImage.Free;
    end;
  finally
    BoundsRect := SaveRect;
  end;
end;

procedure TSeBitmapObject.SetBitmap(const Value: TSeBitmapLink);
begin
  FBitmap.Assign(Value);
end;

procedure TSeBitmapObject.SetBorderTileStyle(const Value: TscTileStyle);
begin
  FBorderTileStyle := Value;
end;



{ TSeActiveBitmap =============================================================}



constructor TSeActiveBitmap.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActiveBitmap := TSeBitmapLink.Create;
  FActiveFont := TFont.Create;
  with FActiveFont do
  begin
    FFont.Name := DefaultFontName;
    FFont.Size := DefaultFontSize;
  end;
end;

destructor TSeActiveBitmap.Destroy;
begin
  FActiveFont.Free;
  FActiveBitmap.Free;
  inherited Destroy;
end;

procedure TSeActiveBitmap.AfterLoad;
var
  NewLink: TSeBitmapLink;
begin
  inherited AfterLoad;
  if FBitmaps <> nil then
  begin
    NewLink := FBitmaps.GetBitmapLink(FActiveBitmap.Name, FActiveBitmap.Rect);
    if NewLink <> nil then
    begin
      FreeAndNil(FActiveBitmap);
      FActiveBitmap := NewLink;
      FActiveBitmap.CheckingMasked(Rect(FMarginLeft, FMarginTop, FMarginRight, FMarginBottom));
    end;
  end;
end;

procedure TSeActiveBitmap.Assign(Source: TPersistent);
begin
  if Source is TSeActiveBitmap then
  begin
    inherited Assign(Source);
    ActiveBitmap.Assign((Source as TSeActiveBitmap).ActiveBitmap);
    ActiveFont := (Source as TSeActiveBitmap).ActiveFont;
  end
  else
    inherited;
end;

procedure TSeActiveBitmap.SetCharset(CharSet: TFontCharset);
begin
  inherited;
  FActiveFont.Charset := CharSet;
end;

function TSeActiveBitmap.GetFont: TFont;
begin
  case FState of
    ssHot, ssFocused: Result := FActiveFont;
  else
    Result := inherited GetFont;
  end;
end;

procedure TSeActiveBitmap.Draw(Canvas: TCanvas; const ClipRect: TRect; DPI: Integer = 0);
var
  SaveBitmap: TSeBitmapLink;
  ScaledObject: TseStyleObject;
begin
  if DPI > DPI_DEFAULT then
  begin
    ScaledObject := GetScaledObject(DPI);
    if (ScaledObject <> nil) and (ScaledObject is TseActiveBitmap) then
    begin
      ScaledObject.State := State;
      ScaledObject.Active := Active;
      ScaledObject.BoundsRect := BoundsRect;
      ScaledObject.Draw(Canvas, ClipRect, DPI);
      Exit;
    end;
  end;

  if ((FActive) or (FState in [ssPressed, ssFocused, ssHot])) and FActiveBitmap.Assigned then
  begin
    SaveBitmap := FBitmap;
    try
      FBitmap := FActiveBitmap;
      inherited Draw(Canvas, ClipRect, DPI);
    finally
      FBitmap := SaveBitmap;
    end;
  end
  else
    inherited Draw(Canvas, ClipRect, DPI);
end;

procedure TSeActiveBitmap.SetActiveBitmap(const Value: TSeBitmapLink);
begin
  FActiveBitmap.Assign(Value);
end;

procedure TSeActiveBitmap.SetActiveFont(const Value: TFont);
begin
  FActiveFont.Assign(Value);
end;

{ TSeSystemButton }

constructor TSeSystemButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FKind := skSysButton;

  FBitmapHot := TSeBitmapLink.Create;
  FBitmapPressed := TSeBitmapLink.Create;
end;

destructor TSeSystemButton.Destroy;
begin
  FBitmapHot.Free;
  FBitmapPressed.Free;
  inherited Destroy;
end;

procedure TSeSystemButton.AfterLoad;
var
  NewLink: TSeBitmapLink;
begin
  inherited AfterLoad;
  if FBitmaps <> nil then
  begin
    { Hover }
    NewLink := FBitmaps.GetBitmapLink(FBitmapHot.Name, FBitmapHot.Rect);
    if NewLink <> nil then
    begin
      FreeAndNil(FBitmapHot);
      FBitmapHot := NewLink;
      FBitmapHot.CheckingMasked(Rect(FMarginLeft, FMarginTop, FMarginRight, FMarginBottom));
    end;
    { Down }
    NewLink := FBitmaps.GetBitmapLink(FBitmapPressed.Name, FBitmapPressed.Rect);
    if NewLink <> nil then
    begin
      FreeAndNil(FBitmapPressed);
      FBitmapPressed := NewLink;
      FBitmapPressed.CheckingMasked(Rect(FMarginLeft, FMarginTop, FMarginRight, FMarginBottom));
    end;
  end;
end;

procedure TSeSystemButton.Assign(Source: TPersistent);
begin
  if Source is TSeSystemButton then
  begin
    inherited Assign(Source);
    BitmapHot.Assign((Source as TSeSystemButton).BitmapHot);
    BitmapPressed.Assign((Source as TSeSystemButton).BitmapPressed);
    FAction := (Source as TSeSystemButton).FAction;
  end
  else
    inherited;
end;

procedure TSeSystemButton.SetState(const Value: TSeState);
begin
  inherited SetState(Value);

  case FState of
    ssNormal: begin
      Visible := True;
      if FAction = sbaRestore then Visible := False;
      if FAction = sbaRollDown then Visible := False;
    end;
    ssMaximized: begin
      Visible := True;
      if FAction = sbaMaximize then Visible := False;
      if FAction = sbaRollDown then Visible := False;
    end;
    ssMinimized: begin
      Visible := True;
      if FAction = sbaMinimize then Visible := False;
      if FAction = sbaMaximize then Visible := False;
      if FAction = sbaRollUp then Visible := False;
      if FAction = sbaRollDown then Visible := False;
    end;
    ssRollup: begin
      Visible := True;
      if FAction = sbaRestore then Visible := False;
      if FAction = sbaRollUp then Visible := False;
    end;
  end;
end;

procedure TSeSystemButton.SetSysButtons(const Value: TSeWindowButtons);
begin
  inherited SetSysButtons(Value);

  case FAction of
    sbaNone: ;
    sbaClose: Visible := (kwbClose in SysButtons);
    sbaHelp: Visible := (kwbHelp in SysButtons);
    sbaMinimize: Visible := (kwbMin in SysButtons);
    sbaMaximize: Visible := (kwbMax in SysButtons);
    sbaRestore:
      if State = ssMaximized then
        Visible := (kwbMaxRestore in SysButtons)
      else
        Visible := (kwbMinRestore in SysButtons);
    sbaRollUp: Visible := (kwbRoll in SysButtons);
    sbaRollDown: Visible := (kwbRollRestore in SysButtons);
    sbaTray: Visible := (kwbTray in SysButtons);
    sbaSysMenu: Visible := (kwbSysMenu in SysButtons);
    sbaRollPanel: ;
  end;
end;

procedure TSeSystemButton.Draw(Canvas: TCanvas; const ClipRect: TRect; DPI: Integer = 0);
var
  SaveBitmap: TSeBitmapLink;
  IHandle, IHandle2 : HICON;
  IconX, IconY : integer;
  ID: boolean;
  R: TRect;
  SaveActive: boolean;
  ScaledObject: TSeStyleObject;
  SaveAction: TSeAction;
begin
  if FWidth <= 0 then Exit;
  if FHeight <= 0 then Exit;

  if DPI > DPI_DEFAULT then
  begin
    ScaledObject := GetScaledObject(DPI);
    if (ScaledObject <> nil) and (ScaledObject is TSeSystemButton) then
    begin
      ScaledObject.State := State;
      ScaledObject.Active := Active;
      ScaledObject.BoundsRect := BoundsRect;
      SaveAction := TSeSystemButton(ScaledObject).Action;
      TSeSystemButton(ScaledObject).Action := Action;
      TSeSystemButton(ScaledObject).ButtonState := ButtonState;
      ScaledObject.Draw(Canvas, ClipRect, DPI);
      TSeSystemButton(ScaledObject).Action := SaveAction;
      Exit;
    end;
  end;

  if FAction = sbaSysMenu then
  begin
    if FParentControl = nil then Exit;
    if not (FParentControl is TForm) then Exit;

    R := BoundsRect;

    ID := False;
    if TForm(FParentControl).Icon.Handle <> 0 then
      IHandle := TForm(FParentControl).Icon.Handle
    else
      if Application.Icon.Handle <> 0 then
        IHandle := Application.Icon.Handle
      else
      begin
        IHandle := LoadIcon(0, IDI_APPLICATION);
        ID := True;
      end;
    IconX := GetSystemMetrics(SM_CXSMICON);
    if IconX = 0 then IconX := GetSystemMetrics(SM_CXSIZE);
    IconY := GetSystemMetrics(SM_CYSMICON);
    if IconY = 0 then IconY := GetSystemMetrics(SM_CYSIZE);
    IHandle2 := CopyImage(IHandle, IMAGE_ICON, IconX, IconY, LR_COPYFROMRESOURCE);
    DrawIconEx(Canvas.Handle, R.Left, R.Top, IHandle2, 0, 0, 0, 0, DI_NORMAL);
    DestroyIcon(IHandle2);
    if ID then DestroyIcon(IHandle);
    Exit;
  end;

  if (not Active) then
  begin
    try
      SetChildState(ssDisabled);
      inherited Draw(Canvas, ClipRect, DPI);
    finally
      SetChildState(ssNormal);
    end;
  end
  else
    case FButtonState of
      sbsNormal: begin
        inherited Draw(Canvas, ClipRect, DPI);
      end;
      sbsHover: begin
        SaveBitmap := FBitmap;
        try
          if FBitmapHot.Assigned then
            FBitmap := FBitmapHot;
          SaveActive := FActive;
          try
            Active := False;
            SetChildState(ssHot);
            inherited Draw(Canvas, ClipRect, DPI);
          finally
            Active := SaveActive;
            SetChildState(ssNormal);
          end;
        finally
          FBitmap := SaveBitmap;
        end;
      end;
      sbsDown: begin
        SaveBitmap := FBitmap;
        try
          if FBitmapPressed.Assigned then
            FBitmap := FBitmapPressed;

          SaveActive := FActive;
          try
            Active := False;
            SetChildState(ssPressed);
            inherited Draw(Canvas, ClipRect, DPI);
          finally
            SetChildState(ssNormal);
            Active := SaveActive;
          end;
        finally
          FBitmap := SaveBitmap;
        end;
      end;
    end;
end;

procedure TSeSystemButton.SetChildState(const Value: TSeState);
var
  i: integer;
begin
  if Count = 0 then Exit;

  for i := 0 to Count-1 do
    Objects[i].State := Value;
end;

procedure TSeSystemButton.SetAction(const Value: TSeAction);
begin
  FAction := Value;
end;

procedure TSeSystemButton.SetBitmapPressed(const Value: TSeBitmapLink);
begin
  FBitmapPressed.Assign(Value);
end;

procedure TSeSystemButton.SetBitmapHot(const Value: TSeBitmapLink);
begin
  FBitmapHot.Assign(Value);
end;



{ TSeButtonObject =============================================================}



constructor TSeButtonObject.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBitmapFocused := TSeBitmapLink.Create;
  FBitmapHot := TSeBitmapLink.Create;
  FBitmapPressed := TSeBitmapLink.Create;
  FBitmapDisabled := TSeBitmapLink.Create;
end;

destructor TSeButtonObject.Destroy;
begin
  FBitmapFocused.Free;
  FBitmapHot.Free;
  FBitmapPressed.Free;
  FBitmapDisabled.Free;
  inherited Destroy;
end;

procedure TSeButtonObject.AfterLoad;
var
  NewLink: TSeBitmapLink;
begin
  inherited AfterLoad;
  if FBitmaps <> nil then
  begin
    { Focused }
    NewLink := FBitmaps.GetBitmapLink(FBitmapFocused.Name, FBitmapFocused.Rect);
    if NewLink <> nil then
    begin
      FreeAndNil(FBitmapFocused);
      FBitmapFocused := NewLink;
      FBitmapFocused.CheckingMasked(Rect(FMarginLeft, FMarginTop, FMarginRight, FMarginBottom));
    end;
    { Hot }
    NewLink := FBitmaps.GetBitmapLink(FBitmapHot.Name, FBitmapHot.Rect);
    if NewLink <> nil then
    begin
      FreeAndNil(FBitmapHot);
      FBitmapHot := NewLink;
      FBitmapHot.CheckingMasked(Rect(FMarginLeft, FMarginTop, FMarginRight, FMarginBottom));
    end;
    { Pressed }
    NewLink := FBitmaps.GetBitmapLink(FBitmapPressed.Name, FBitmapPressed.Rect);
    if NewLink <> nil then
    begin
      FreeAndNil(FBitmapPressed);
      FBitmapPressed := NewLink;
      FBitmapPressed.CheckingMasked(Rect(FMarginLeft, FMarginTop, FMarginRight, FMarginBottom));
    end;
    { Disabled }
    NewLink := FBitmaps.GetBitmapLink(FBitmapDisabled.Name, FBitmapDisabled.Rect);
    if NewLink <> nil then
    begin
      FreeAndNil(FBitmapDisabled);
      FBitmapDisabled := NewLink;
      FBitmapDisabled.CheckingMasked(Rect(FMarginLeft, FMarginTop, FMarginRight, FMarginBottom));
    end;
  end;
end;

procedure TSeButtonObject.Assign(Source: TPersistent);
begin
  if Source is TSeButtonObject then
  begin
    inherited Assign(Source);
    BitmapFocused.Assign((Source as TSeButtonObject).BitmapFocused);
    BitmapHot.Assign((Source as TSeButtonObject).BitmapHot);
    BitmapDisabled.Assign((Source as TSeButtonObject).BitmapDisabled);
    BitmapPressed.Assign((Source as TSeButtonObject).BitmapPressed);
  end
  else
    inherited;
end;

procedure TSeButtonObject.Draw(Canvas: TCanvas; const ClipRect: TRect; DPI: Integer = 0);
var
  SaveBitmap: TSeBitmapLink;
  ScaledObject: TseStyleObject;
begin
  if not FBitmap.Assigned then Exit;
  if FWidth <= 0 then Exit;
  if FHeight <= 0 then Exit;

  if DPI > DPI_DEFAULT then
  begin
    ScaledObject := GetScaledObject(DPI);
    if (ScaledObject <> nil) and (ScaledObject is TseButtonObject) then
    begin
      ScaledObject.State := State;
      ScaledObject.Active := Active;
      ScaledObject.BoundsRect := BoundsRect;
      ScaledObject.Draw(Canvas, ClipRect, DPI);
      Exit;
    end;
  end;

  if (FState = ssFocused) and (FBitmapFocused.Assigned) then
  begin
    SaveBitmap := FBitmap;
    try
      FBitmap := FBitmapFocused;

      inherited Draw(Canvas, ClipRect, DPI);
    finally
      FBitmap := SaveBitmap;
    end;
    Exit;
  end;

  if (FState = ssHot) and (FBitmapHot.Assigned) then
  begin
    SaveBitmap := FBitmap;
    try
      FBitmap := FBitmapHot;

      inherited Draw(Canvas, ClipRect, DPI);
    finally
      FBitmap := SaveBitmap;
    end;
    Exit;
  end;

  if (FState = ssDisabled) and (FBitmapDisabled.Assigned) then
  begin
    SaveBitmap := FBitmap;
    try
      FBitmap := FBitmapDisabled;

      inherited Draw(Canvas, ClipRect, DPI);
    finally
      FBitmap := SaveBitmap;
    end;
    Exit;
  end;

  if (FState = ssPressed) and (FBitmapPressed.Assigned) then
  begin
    SaveBitmap := FBitmap;
    try
      FBitmap := FBitmapPressed;

      inherited Draw(Canvas, ClipRect, DPI);
    finally
      FBitmap := SaveBitmap;
    end;
    Exit;
  end;

  inherited Draw(Canvas, ClipRect, DPI);
end;

procedure TSeButtonObject.SetBitmapDisabled(const Value: TSeBitmapLink);
begin
  FBitmapDisabled.Assign(Value);
end;

procedure TSeButtonObject.SetBitmapFocused(const Value: TSeBitmapLink);
begin
  FBitmapFocused.Assign(Value);
end;

procedure TSeButtonObject.SetBitmapHot(const Value: TSeBitmapLink);
begin
  FBitmapHot.Assign(Value);
end;

procedure TSeButtonObject.SetBitmapPressed(const Value: TSeBitmapLink);
begin
  FBitmapPressed.Assign(Value);
end;

{ TSeTextObject }

constructor TSeTextObject.Create(AOwner: TComponent);
begin
  inherited;
  FFontHot := TFont.Create;
  with FFontHot do
  begin
    FFont.Name := DefaultFontName;
    FFont.Size := DefaultFontSize;
  end;
  FFontFocused := TFont.Create;
  with FFontFocused do
  begin
    FFont.Name := DefaultFontName;
    FFont.Size := DefaultFontSize;
  end;
  FFontPressed := TFont.Create;
  with FFontPressed do
  begin
    FFont.Name := DefaultFontName;
    FFont.Size := DefaultFontSize;
  end;
  FFontDisabled := TFont.Create;
  with FFontDisabled do
  begin
    FFont.Name := DefaultFontName;
    FFont.Size := DefaultFontSize;
  end;
end;

destructor TSeTextObject.Destroy;
begin
  FFontHot.Free;
  FFontFocused.Free;
  FFontPressed.Free;
  FFontDisabled.Free;
  inherited;
end;

procedure TSeTextObject.Assign(Source: TPersistent);
begin
  if Source is TSeTextObject then
  begin
    inherited Assign(Source);
    FontHot := (Source as TSeTextObject).FontHot;
    FontFocused := (Source as TSeTextObject).FontFocused;
    FontPressed := (Source as TSeTextObject).FontPressed;
    FontDisabled := (Source as TSeTextObject).FontDisabled;
  end
  else
    inherited;
end;

procedure TSeTextObject.Draw(Canvas: TCanvas; const ClipRect: TRect; DPI: Integer = 0);
var
  SaveFont: TFont;
begin
  if FWidth <= 0 then Exit;
  if FHeight <= 0 then Exit;

  case FState of
    ssHot: begin
      SaveFont := FFont;
      try
        FFont := FFontHot;
        inherited Draw(Canvas, ClipRect);
      finally
        FFont := SaveFont;
      end;
    end;
    ssFocused: begin
      SaveFont := FFont;
      try
        FFont := FFontFocused;
        inherited Draw(Canvas, ClipRect);
      finally
        FFont := SaveFont;
      end;
    end;
    ssPressed: begin
      SaveFont := FFont;
      try
        FFont := FFontPressed;
        inherited Draw(Canvas, ClipRect);
      finally
        FFont := SaveFont;
      end;
    end;
    ssDisabled: begin
      SaveFont := FFont;
      try
        FFont := FFontDisabled;
        inherited Draw(Canvas, ClipRect);
      finally
        FFont := SaveFont;
      end;
    end;
  else
    inherited Draw(Canvas, ClipRect);
  end;
end;

function TSeTextObject.GetFont: TFont;
begin
  case FState of
    ssHot: Result := FFontHot;
    ssFocused: Result := FFontFocused;
    ssPressed: Result := FFontPressed;
    ssdisabled: Result := FFontDisabled;
  else
    Result := inherited GetFont;
  end;
end;

procedure TSeTextObject.SetFontDisabled(const Value: TFont);
begin
  FFontDisabled.Assign(Value);
end;

procedure TSeTextObject.SetFontFocused(const Value: TFont);
begin
  FFontFocused.Assign(Value);
end;

procedure TSeTextObject.SetFontHot(const Value: TFont);
begin
  FFontHot.Assign(Value);
end;

procedure TSeTextObject.SetFontPressed(const Value: TFont);
begin
  FFontPressed.Assign(Value);
end;

procedure TSeTextObject.SetCharset(CharSet: TFontCharset);
begin
  inherited;
  FFontHot.Charset := CharSet;
  FFontFocused.Charset := CharSet;
  FFontPressed.Charset := CharSet;
  FFontDisabled.Charset := CharSet;
end;

{ TSeLinkStyleObject ===========================================================}

procedure TSeLinkStyleObject.Assign(Source: TPersistent);
begin
  if Source is TSeLinkStyleObject then
  begin
    inherited Assign(Source);
    FLinkControl := (Source as TSeLinkStyleObject).FLinkControl;
  end
  else
    inherited;
end;

procedure TSeLinkStyleObject.Draw(Canvas: TCanvas; const ClipRect: TRect; DPI: Integer = 0);
var
  R: TRect;
begin
  if State <> ssDesign then Exit;

  if Width <= 0 then Exit;
  if Height <= 0 then Exit;

  FillRect(Canvas, BoundsRect, clWhite);
  DrawRect(Canvas, BoundsRect, clBlack);

  Canvas.Font.Name := 'Arial';
  Canvas.Font.Style := [fsItalic];
  Canvas.Font.Color := clBlack;
  R := BoundsRect;
  DrawText(Canvas, FLinkControl, R, DT_CENTER or DT_SINGLELINE or DT_VCENTER);
end;

{ TSeActiveStyleObject =========================================================}

constructor TSeActiveStyleObject.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActiveFont := TFont.Create;
  with FActiveFont do
  begin
    FFont.Name := DefaultFontName;
    FFont.Size := DefaultFontSize;
  end;
end;

destructor TSeActiveStyleObject.Destroy;
begin
  FActiveFont.Free;
  inherited Destroy;
end;

procedure TSeActiveStyleObject.Assign(Source: TPersistent);
begin
  if Source is TSeActiveStyleObject then
  begin
    inherited Assign(Source);
    ActiveColor := (Source as TSeActiveStyleObject).ActiveColor;
    ActiveFont := (Source as TSeActiveStyleObject).ActiveFont;
  end
  else
    inherited;
end;

procedure TSeActiveStyleObject.Draw(Canvas: TCanvas; const ClipRect: TRect; DPI: Integer = 0);
var
  SaveColor: TColor;
  SaveFont: TFont;
begin
  if Width <= 0 then Exit;
  if Height <= 0 then Exit;

  if (State = ssFocused) or (State = ssHot) then
  begin
    SaveFont := Font;
    SaveColor := Color;
    try
      FColor := FActiveColor;
      FFont := FActiveFont;
      inherited Draw(Canvas, ClipRect, DPI);
    finally
      FFont := SaveFont;
      FColor := SaveColor;
    end;
  end
  else
    inherited Draw(Canvas, ClipRect, DPI);
end;

procedure TSeActiveStyleObject.SetActiveFont(const Value: TFont);
begin
  FActiveFont.Assign(Value);
end;

{ TSeColorButtonObject ========================================================}

constructor TSeColorButtonObject.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHotFont := TFont.Create;
  with FHotFont do
  begin
    FFont.Name := DefaultFontName;
    FFont.Size := DefaultFontSize;
  end;
  FPressedFont := TFont.Create;
  with FPressedFont do
  begin
    FFont.Name := DefaultFontName;
    FFont.Size := DefaultFontSize;
  end;
  FDisabledFont := TFont.Create;
  with FDisabledFont do
  begin
    FFont.Name := DefaultFontName;
    FFont.Size := DefaultFontSize;
  end;
  FFocusedFont := TFont.Create;
  with FFocusedFont do
  begin
    FFont.Name := DefaultFontName;
    FFont.Size := DefaultFontSize;
  end;
end;

destructor TSeColorButtonObject.Destroy;
begin
  FFocusedFont.Free;
  FDisabledFont.Free;
  FPressedFont.Free;
  FHotFont.Free;
  inherited Destroy;
end;

procedure TSeColorButtonObject.Assign(Source: TPersistent);
begin
  if Source is TSeColorButtonObject then
  begin
    inherited Assign(Source);
    HotFont := (Source as TSeColorButtonObject).FHotFont;
    HotColor := (Source as TSeColorButtonObject).FHotColor;
    PressedFont := (Source as TSeColorButtonObject).FPressedFont;
    PressedColor := (Source as TSeColorButtonObject).FPressedColor;
    FocusedFont := (Source as TSeColorButtonObject).FFocusedFont;
    FocusedColor := (Source as TSeColorButtonObject).FFocusedColor;
    DisabledFont := (Source as TSeColorButtonObject).FDisabledFont;
    DisabledColor := (Source as TSeColorButtonObject).FDisabledColor;
  end
  else
    inherited;
end;

procedure TSeColorButtonObject.Draw(Canvas: TCanvas; const ClipRect: TRect; DPI: Integer = 0);
var
  SaveFont: TFont;
  SaveColor: TColor;
begin
  if Width <= 0 then Exit;
  if Height <= 0 then Exit;

  SaveColor := Color;
  SaveFont := TFont.Create;
  try
    SaveFont.Assign(Font);

    case State of
      ssHot: begin
        Font.Assign(FHotFont);
        Color := FHotColor;
      end;
      ssFocused: begin
        Font.Assign(FFocusedFont);
        Color := FFocusedColor;
      end;
      ssPressed: begin
        Font.Assign(FPressedFont);
        Color := FPressedColor;
      end;
      ssDisabled: begin
        Font.Assign(FDisabledFont);
        Color := FDisabledColor;
      end;
    end;
    inherited Draw(Canvas, ClipRect);
  finally
    Font.Assign(SaveFont);
    Color := SaveColor;

    SaveFont.Free;
  end;
end;

procedure TSeColorButtonObject.SetDisabledColor(const Value: TColor);
begin
  FDisabledColor := Value;
end;

procedure TSeColorButtonObject.SetDisabledFont(const Value: TFont);
begin
  FDisabledFont.Assign(Value);
end;

procedure TSeColorButtonObject.SetFocusedColor(const Value: TColor);
begin
  FFocusedColor := Value;
end;

procedure TSeColorButtonObject.SetFocusedFont(const Value: TFont);
begin
  FFocusedFont.Assign(Value);
end;

procedure TSeColorButtonObject.SetHotColor(const Value: TColor);
begin
  FHotColor := Value;
end;

procedure TSeColorButtonObject.SetHotFont(const Value: TFont);
begin
  FHotFont.Assign(Value);
end;

procedure TSeColorButtonObject.SetPressedColor(const Value: TColor);
begin
  FPressedColor := Value;
end;

procedure TSeColorButtonObject.SetPressedFont(const Value: TFont);
begin
  FPressedFont.Assign(Value);
end;

{ TSeRollPanelObject }

procedure TSeRollPanelObject.Assign(Source: TPersistent);
begin
  if Source is TSeRollPanelObject then
  begin
    inherited Assign(Source);
    RollKind := (Source as TSeRollPanelObject).RollKind;
    RollValue := (Source as TSeRollPanelObject).RollValue;
  end
  else
    inherited;
end;

procedure TSeRollPanelObject.OwnerUpdate;
begin
  Application.ProcessMessages;
end;

procedure TSeRollPanelObject.Roll;
const
  Percent = 1;
var
  RollUp: boolean;
  i: integer;
  TmpValue: integer;
begin
  if Owner = nil then Exit;

  RollUp := False;
  case FRollKind of
    rkLeft, rkRight:
      if Width = FRollValue then
        RollUp := False
      else
        RollUp := True;
    rkTop, rkBottom:
      if Height = FRollValue then
        RollUp := False
      else
        RollUp := True;
  end;

  if RollUp then
  begin
    { RollUp }
    case FRollKind of
      rkLeft: begin
        FOldValue := Width;
        TmpValue := Left + Width;

        for i := 1 to Percent do
        begin
          Width := Width + Round((FRollValue - Width) * (i / Percent));
          Left := TmpValue - Width;
          OwnerUpdate;
        end;
      end;
      rkRight: begin
        FOldValue := Width;

        for i := 1 to Percent do
        begin
          Width := Width + Round((FRollValue - Width) * (i / Percent));
          OwnerUpdate;
        end;
      end;
      rkTop: begin
        FOldValue := Height;
        TmpValue := Top + Height;

        for i := 1 to Percent do
        begin
          Height := Height + Round((FRollValue - Height) * (i / Percent));
          Top := TmpValue - Height;
          OwnerUpdate;
        end;
      end;
      rkBottom: begin
        FOldValue := Height;

        for i := 1 to Percent do
        begin
          Height := Height + Round((FRollValue - Height) * (i / Percent));
          OwnerUpdate;
        end;
      end;
    end;
  end
  else
  begin
    { Restore }
    case FRollKind of
      rkLeft: begin
        TmpValue := Left + Width;

        for i := 1 to Percent do
        begin
          Width := Width - Round((Width - FOldValue) * (i / Percent));
          Left := TmpValue - Width;
          OwnerUpdate;
        end;
      end;
      rkRight: begin
        for i := 1 to Percent do
        begin
          Width := Width - Round((Width - FOldValue) * (i / Percent));
          OwnerUpdate;
        end;
      end;
      rkTop: begin
        TmpValue := Top + Height;

        for i := 1 to Percent do
        begin
          Height := Height - Round((Height - FOldValue) * (i / Percent));
          Top := TmpValue - Height;
          OwnerUpdate;
        end;
      end;
      rkBottom: begin
        for i := 1 to Percent do
        begin
          Height := Height - Round((Height - FOldValue) * (i / Percent));
          OwnerUpdate;
        end;
      end;
    end;
  end;
end;

const
 { Link to Style objects }
  soForm                = 0;
  soFormClientRect      = 1;
  soFormSysButton       = 2;
  soFormSysButtonDraw   = 3;
  soClient              = 4;
  soButton              = 5;
  soCheckbox            = 6;
  soRadioButton         = 7;
  soGroupBox            = 8;
  soScrollBar           = 9;
  soProgressBar         = 10;
  soEditBox             = 11;
  soComboBox            = 12;
  soTrackBar            = 13;
  soMenuBar             = 14;
  soPopupMenu           = 15;
  soHeader              = 16;
  soStatus              = 17;
  soTab                 = 18;
  soPanel               = 19;
  soFrame               = 20;
  soScrollBox           = 21;
  soSplitter            = 22;
  soSpinButton          = 23;
  soControlBar          = 24;
  soGrid                = 25;
  soListBox             = 26;
  soHint                = 27;
  soSpeedButton         = 28;
  soToolBar             = 29;
  soSmall               = 30;
  soSmallClientRect     = 31;
  soSmallSysButton      = 32;
  soSmallSysButtonDraw  = 33;
  soSmallClient         = 34;
  soHeaderPanel         = 35;
  soBarDock             = 36;
  soLast                = 50;

type

  PDrawBackgroundFuncRec = ^TDrawBackgroundFuncRec;
  TDrawBackgroundFuncRec = record
    FClass: TClass;
    FFunc: TSeDrawControlBackground;
  end;

  TDrawCustomControlAccess = class(TCustomControl);
  
var
  DrawBackgroundFuncList: TList = nil;

procedure RegisterDrawControlBackground(AClass: TClass; AFunc: TSeDrawControlBackground);
var
  R: PDrawBackgroundFuncRec;
begin
  if DrawBackgroundFuncList = nil then
    DrawBackgroundFuncList := TList.Create;

  New(R);
  R^.FClass := AClass;
  R^.FFunc := AFunc;
  DrawBackgroundFuncList.Add(R);
end;

procedure FreeDrawBackgroundFuncList;
var
  i: integer;
begin
  if DrawBackgroundFuncList <> nil then
  begin
    for i := 0 to DrawBackgroundFuncList.Count - 1 do
      Dispose(PDrawBackgroundFuncRec(DrawBackgroundFuncList[i]));
    DrawBackgroundFuncList.Free;
  end;
end;

function GetDrawBackgroundFunc(AParent: TControl): TSeDrawControlBackground;
var
  i: integer;
  CurClass: TClass; 
begin
  Result := nil;
  CurClass := nil;

  if DrawBackgroundFuncList <> nil then
  begin
    for i := DrawBackgroundFuncList.Count - 1 downto 0 do
      if AParent is PDrawBackgroundFuncRec(DrawBackgroundFuncList[i])^.FClass then
      begin
        if (CurClass <> nil) then
        begin
          if (PDrawBackgroundFuncRec(DrawBackgroundFuncList[i])^.FClass.InheritsFrom(CurClass)) then
          begin
            Result := PDrawBackgroundFuncRec(DrawBackgroundFuncList[i])^.FFunc;
            CurClass := PDrawBackgroundFuncRec(DrawBackgroundFuncList[i])^.FClass;
          end;
        end
        else
        begin
          Result := PDrawBackgroundFuncRec(DrawBackgroundFuncList[i])^.FFunc;
          CurClass := PDrawBackgroundFuncRec(DrawBackgroundFuncList[i])^.FClass;
        end;
      end;
  end;
end;

{ Drawing }

var
  CPB: TSeCustomPaintBack;
type
  TWinControlClass = class(TWinControl);

procedure DrawControlBackground(Control: TControl; DC: HDC);
var
  P: TPoint;
  SaveIndex: Integer;
  Func: TSeDrawControlBackground;
  B: TSeBackground;
  Canvas: TCanvas;
  SaveState: TControlState;
  Buffer: TBitmap;
begin
  if Control = nil then Exit;
  if Control.Parent = nil then Exit;
  if (Control.Width <= 0) or (Control.Height <= 0) then Exit;
  { Paint Owner Background }
  SaveIndex := SaveDC(DC);
  try
    P := Control.ClientOrigin;
    { Offset DC }
    Winapi.Windows.ScreenToClient(Control.Parent.Handle, P);
    MoveWindowOrg(DC, -P.X, -P.Y);
    { Drawing }
    Func := GetDrawBackgroundFunc(Control.Parent);
    if @Func <> nil then
    begin
      { Draw using DrawXXXXBackground function }
      B.Rect := Control.BoundsRect;
      B.ClipRect := Control.BoundsRect;
      B.Control := Control;
      B.Parent := Control.Parent;
      Canvas := TCanvas.Create;
      try
        Canvas.Handle := DC;
        SaveState := Control.ControlState;
        Control.ControlState := Control.ControlState + [csPaintCopy];
        try
          Func(Canvas, B);
        finally
          Control.ControlState := SaveState;
        end;
        Canvas.Handle := 0;
      finally
        Canvas.Free;
      end;
    end
    else
    begin
      { Paint parent to DC }
      SaveState := Control.ControlState;
      Control.ControlState := Control.ControlState + [csPaintCopy];
      try
        if Control is TGraphicControl then
        begin
          { Need double buffering }
          Buffer := TBitmap.Create;
          try
            Buffer.Width := Control.Width;
            Buffer.Height := Control.Height;
            { WinControl drawing }
            MoveWindowOrg(Buffer.Canvas.Handle, -P.X, -P.Y);
            Control.Parent.Perform(WM_ERASEBKGND, Buffer.Canvas.Handle, 1);
            Control.Parent.Perform(WM_PAINT, Buffer.Canvas.Handle, 1);
            MoveWindowOrg(Buffer.Canvas.Handle, P.X, P.Y);
            { Buffer drawing }
            BitBlt(DC, P.X, P.Y, Buffer.Width, Buffer.Height,
              Buffer.Canvas.Handle, 0, 0, SRCCOPY);
          finally
            Buffer.Free;
          end;
        end
        else
          begin
            Control.Parent.Perform(WM_ERASEBKGND, WPARAM(DC), 1);
            Control.Parent.Perform(WM_PAINT, WPARAM(DC), 1);
          end;
      finally
        Control.ControlState := SaveState;
      end;
    end;
    { Restore DC origin }
    MoveWindowOrg(DC, P.X, P.Y);
  finally
    RestoreDC(DC, SaveIndex);
  end;
end;

procedure DrawControlBackground(Control: TControl; Canvas: TCanvas);
begin
  DrawControlBackground(Control, Canvas.Handle);
end;

procedure DrawControlBackground(Control: HWnd; Canvas: TCanvas);
var
  P: TPoint;
  R: TRect;
  Parent: HWnd;
begin
  Parent := GetParent(Control);
  if Parent = 0 then Exit;

  GetWindowRect(Control, R);
  P := R.TopLeft;

  if CustomBackroundDrawing then
  begin
    CPB.Rect := R;
    CPB.ClipRect := R;
    CPB.Control := Control;
    CPB.Parent := Parent;
    MoveWindowOrg(Canvas.Handle, -P.X, -P.Y);
    SendMessage(Parent, WM_CUSTOMPAINTBACK, Integer(Canvas), Cardinal(@CPB));
    MoveWindowOrg(Canvas.Handle, P.X, P.Y);
  end
  else
  begin
    MoveWindowOrg(Canvas.Handle, -P.X, -P.Y);
    SendMessage(Parent, WM_ERASEBKGND, Canvas.Handle, 0);
    SendMessage(Parent, WM_PAINT, Canvas.Handle, 0);
    MoveWindowOrg(Canvas.Handle, P.X, P.Y);
  end;
end;

{ Standard control drawing }


{ TSeStyleDrawing }

constructor TSeStyleDrawing.Create(AStyle: TPersistent);
begin
  FStyle := AStyle;
end;



{ Persitent objects store =====================================================}


var
  StoredObject: TList = nil;

type
  PStoreRec = ^TStoreRec;
  TStoreRec = record
    StoredObject: TPersistent;
    CopyObject: TPersistent;
  end;

procedure AddObjectToStore(AObject: TPersistent);
var
  i: integer;
  Rec: PStoreRec;
begin
  if StoredObject = nil then
    StoredObject := TList.Create;

  for i := 0 to StoredObject.Count - 1 do
  begin
    Rec := PStoreRec(StoredObject[i]);
    if Rec^.StoredObject = AObject then
    begin
      Rec^.CopyObject.Assign(AObject);
      Exit;
    end;
  end;

  New(Rec);
  Rec^.StoredObject := AObject;

  if AObject is TFont then
    Rec^.CopyObject := TPersistent(TFont.Create)
  else
    Rec^.CopyObject := TPersistent(AObject.ClassType.Create);

  Rec^.CopyObject.Assign(AObject);

  StoredObject.Add(Rec);
end;

procedure RemoveObjectFromStore(AObject: TPersistent);
var
  i: integer;
  Rec: PStoreRec;
begin
  if StoredObject <> nil then
  begin
    for i := 0 to StoredObject.Count - 1 do
    begin
      Rec := PStoreRec(StoredObject[i]);
      if Rec^.StoredObject = AObject then
      begin
        Rec^.CopyObject.Free;
        StoredObject.Remove(Rec);
        Dispose(Rec);
        Break;
      end;
    end;
  end;
end;

procedure AssignFromStoredObject(AObject: TPersistent);
var
  i: integer;
  Rec: PStoreRec;
begin
  if StoredObject <> nil then
  begin
    for i := 0 to StoredObject.Count - 1 do
    begin
      Rec := PStoreRec(StoredObject[i]);
      if Rec^.StoredObject = AObject then
      begin
        AObject.Assign(Rec^.CopyObject);
        Break;
      end;
    end;
  end;
end;

procedure ReleaseObjectStore;
var
  i: integer;
  Rec: PStoreRec;
begin
  if StoredObject <> nil then
  begin
    for i := 0 to StoredObject.Count - 1 do
    begin
      Rec := PStoreRec(StoredObject[i]);
      Rec^.CopyObject.Free;
      Dispose(Rec);
    end;
    StoredObject.Free;
    StoredObject := nil;
  end;
end;

{ Prop store }

var
  StoredProp: TList = nil;

type
  PPropStoreRec = ^TPropStoreRec;
  TPropStoreRec = record
    StoredObject: TObject;
    PropName: string;
    Value: TSePropValue;
  end;

procedure AddPropToStore(AObject: TObject; const  APropName: string; AValue: TSePropValue);
var
  i: integer;
  Rec: PPropStoreRec;
begin
  if StoredProp = nil then
    StoredProp := TList.Create;

  for i := 0 to StoredProp.Count - 1 do
  begin
    Rec := PPropStoreRec(StoredProp[i]);
    if (Rec^.StoredObject = AObject) and (Rec^.PropName = LowerCase(APropName)) then
    begin
      Rec^.Value := AValue;
      Exit;
    end;
  end;

  New(Rec);
  Rec^.StoredObject := AObject;
  Rec^.PropName := LowerCase(APropName);
  Rec^.Value := AValue;

  StoredProp.Add(Rec);
end;

procedure RemovePropFromStore(AObject: TObject; const APropName: string);
var
  i: integer;
  Rec: PPropStoreRec;
begin
  if StoredProp <> nil then
  begin
    for i := 0 to StoredProp.Count - 1 do
    begin
      Rec := PPropStoreRec(StoredProp[i]);
      if (Rec^.StoredObject = AObject) and (Rec^.PropName = LowerCase(APropName)) then
      begin
        StoredProp.Remove(Rec);
        Dispose(Rec);
        Break;
      end;
    end;
  end;
end;

function GetPropFromStore(AObject: TObject; const APropName: string): TSePropValue;
var
  i: integer;
  Rec: PPropStoreRec;
begin
  Result := 0;
  if StoredProp <> nil then
  begin
    for i := 0 to StoredProp.Count - 1 do
    begin
      Rec := PPropStoreRec(StoredProp[i]);
      if (Rec^.StoredObject = AObject) and (Rec^.PropName = LowerCase(APropName)) then
      begin
        Result := Rec^.Value;
        Break;
      end;
    end;
  end;
end;

procedure ReleasePropStore;
var
  i: integer;
  Rec: PPropStoreRec;
begin
  if StoredProp <> nil then
  begin
    for i := 0 to StoredProp.Count - 1 do
    begin
      Rec := PPropStoreRec(StoredProp[i]);
      Dispose(Rec);
    end;
    StoredProp.Free;
    StoredProp := nil;
  end;
end;

{ Style managment ==============================================================}

function Margin(ALeft, ATop, ARight, ABottom: integer): TktlMargin;
begin
  Result.Left := ALeft;
  Result.Top := ATop;
  Result.Right := ARight;
  Result.Bottom := ABottom;
end;

{ TSeStyleMetrics }

constructor TSeStyleMetrics.Create(AStyle: TPersistent);
begin
  FStyle := AStyle;
end;

function TSeStyleMetrics.GetMargin(MarginId: TktlMarginId): TktlMargin;
var
  Subclass: TSeWindowSubclass;
  R, CR: TRect;
  IsStyleAvailable: Boolean;
begin
  case MarginId of
    ktmiWindowStandard: Subclass := kwscStandard;
    ktmiWindowDialog: Subclass := kwscDialog;
    ktmiWindowBorder: Subclass := kwscBorder;
    ktmiWindowMessage: Subclass := kwscMessage;
    ktmiWindowTextured: Subclass := kwscTextured;
    ktmiWindowToolWindow: Subclass := kwscToolWindow;
  else
    Subclass := kwscStandard;
  end;

  IsStyleAvailable := (TseStyle(FStyle).StyleSource <> nil) and (TseStyle(FStyle).StyleSource.Count > 0);

  if IsStyleAvailable and not TseStyle(FStyle).IsObjectDefined(Subclass) then
    Subclass := kwscStandard;

  if IsStyleAvailable and TseStyle(FStyle).IsObjectDefined(Subclass) then
  begin
    R := Rect(0, 0, 400, 400);
    CR := TseStyle(FStyle).WindowGetClientRect(Subclass, R);
    Result := Margin(CR.Left, CR.Top, R.Right - CR.Right, R.Bottom - CR.Bottom);
  end
  else
    Result := Margin(0, 0, 0, 0);
end;



{ TSeStyleOptions ============================================================}

constructor TSeStyleOptions.Create;
begin
  inherited Create;
  FStyle := AStyle;
end;

procedure TSeStyleOptions.Assign(Source: TPersistent);
var
  i: TSeStyleOption;
begin
  if Source is TSeStyleOptions then
  begin
    for i := Low(FOptions) to High(FOptions) do
    begin
      FOptions[i] := (Source as TSeStyleOptions).FOptions[i];
    end;
  end
  else
    inherited;
end;

procedure TSeStyleOptions.LoadFromStream(Stream: TStream);
var
  Count, i: TSeStyleOption;
begin
  Stream.Read(Count, SizeOf(Count));

  for i := Low(FOptions) to TSeStyleOption(Count) do
  begin
    Stream.Read(FOptions[i], SizeOf(FOptions[i]));
  end;
end;

procedure TSeStyleOptions.SaveToStream(Stream: TStream);
var
  i, Count: TSeStyleOption;
begin
  Count := High(FOptions);
  Stream.Write(Count, SizeOf(Count));

  for i := Low(FOptions) to High(FOptions) do
  begin
    Stream.Write(FOptions[i], SizeOf(FOptions[i]));
  end;
end;

function TSeStyleOptions.GetOptions(index: TSeStyleOption): boolean;
begin
  Result := FOptions[index];
end;

procedure TSeStyleOptions.SetOptions(index: TSeStyleOption;
  const Value: boolean);
begin
  FOptions[index] := Value;
end;



type

  TSeHackStyle = class(TSeCustomStyle);

procedure SetFont(AFont: TFont; const AFontName: string;
  AFontSize: integer; AFontStyle: TFontStyles; AFontColor: TColor);
begin
  AFont.Name := AFontName;
  AFont.Size := AFontSize;
  AFont.Style := AFontStyle;
  AFont.Color := AFontColor;
end;

function FontToString(Font: TFont): string;
begin
  Result := Font.Name + ',' + IntToStr(Font.Size) + ',' + IntToStr(Font.Charset) +
    ',' + IntToStr(GetRValue(Font.Color)) + ',' + IntToStr(GetGValue(Font.Color)) +
    ',' + IntToStr(GetBValue(Font.Color));

  if fsBold in Font.Style then Result := Result + ',bold';
  if fsItalic in Font.Style then Result := Result + ',italic';
  if fsUnderline in Font.Style then Result := Result + ',underline';
  if fsStrikeOut in Font.Style then Result := Result + ',strikeout';
end;

function StringToFontName(const Str: string): string;
var
  S: string;
begin
  S := Str;
  Result := GetToken(S, ';,');
end;

function StringToFontSize(const Str: string): Integer;
var
  S: string;
begin
  S := Str;
  GetToken(S, ';,'); // Skip font name
  Result := StrToInt(GetToken(S));
end;

function StringToFontCharset(const Str: string): TFontCharset;
var
  S: string;
begin
  S := Str;
  GetToken(S, ';,'); // Skip font name
  GetToken(S, ';,'); // Skip font size
  Result := StrToInt(GetToken(S));
end;

function StringToFontColor(const Str: string): TColor;
var
  R, G, B: integer;
  S: string;
begin
  S := Str;
  GetToken(S, ';,'); // Skip font name
  GetToken(S, ';,'); // Skip font size
  GetToken(S, ';,'); // Skip font charset
  R := StrToInt(GetToken(S));
  G := StrToInt(GetToken(S));
  B := StrToInt(GetToken(S));
  Result := RGB(R, G, B);
end;

function StringToFontStyle(const Str: string): TFontStyles;
var
  S: string;
begin
  S := Str;
  Result := [];
  if System.Pos(',bold', S) <> 0 then Result := Result + [fsBold];
  if System.Pos(',italic', S) <> 0 then Result := Result + [fsItalic];
  if System.Pos(',underline', S) <> 0 then Result := Result + [fsUnderline];
  if System.Pos(',strikeout', S) <> 0 then Result := Result + [fsStrikeOut];
end;

{ TSeColorList ================================================================}

constructor TSeStyleColors.Create;
var
  i: TSeStyleColor;
begin
  inherited Create;
  FStyle := AStyle;
  // initialize colors
  for i := Low(FColorList) to High(FColorList) do
    FColorList[i] := clBlack;
end;

procedure TSeStyleColors.Assign(Source: TPersistent);
var
  i: TSeStyleColor;
begin
  if Source is TSeStyleColors then
  begin
    for i := Low(FColorList) to High(FColorList) do
    begin
      FColorList[i] := (Source as TSeStyleColors).FColorList[i];
    end;
  end
  else
    inherited;
end;

procedure TSeStyleColors.LoadFromStream(Stream: TStream);
var
  Count, i, ItemCount: TSeStyleColor;
  EnumCount: Integer;
begin
  Stream.Read(Count, SizeOf(Count));

  // Check if there are fewer items in the stream than the current declaration
  // of TSeStyleColor, and read only as many as there are in the stream
  EnumCount := Integer(High(FColorList)) - Integer(Low(FColorList));
  if Integer(Count) < EnumCount then
    ItemCount := Count
  else
    ItemCount := High(FColorList);

  for I := Low(FColorList) to ItemCount do
  begin
    ReadString(Stream); // name
    ReadString(Stream); // :
    FColorList[i] := StringToColor(ReadString(Stream));
  end;

  // If the stream contains more items than we read, advance stream to correct position
  if Count > ItemCount then
  begin
    Dec(Count);
    for I := ItemCount to Count do
    begin
      ReadString(Stream); // Name
      ReadString(Stream); // :
      ReadString(Stream); // ColorName
    end;
  end;
end;

procedure TSeStyleColors.SaveToStream(Stream: TStream);
var
  i, Count: TSeStyleColor;
begin
  Count := High(FColorList);
  Stream.Write(Count, SizeOf(Count));
  for i := Low(FColorList) to High(FColorList) do
  begin
    WriteString(Stream, StyleColorNames[i]);
    WriteString(Stream, ':');
    WriteString(Stream, ColorToString(FColorList[i]));
  end;
end;

function TSeStyleColors.GetColor(index: TSeStyleColor): TColor;
begin
  Result := FColorList[index];
end;

procedure TSeStyleColors.SetColor(index: TSeStyleColor; const Value: TColor);
begin
  FColorList[index] := Value;
end;

{ TSeStyleSysColors ==========================================================}

constructor TSeStyleSysColors.Create(AStyle: TPersistent);
var
  i: integer;
begin
  inherited Create;
  FStyle := AStyle;

  for i := 0 to MaxSysColor - 1 do
    FColorList[i] := SysColors[i].Value;
end;

procedure TSeStyleSysColors.Assign(Source: TPersistent);
var
  i: integer;
begin
  if Source is TSeStyleSysColors then
  begin
    for i := 0 to MaxSysColor - 1 do
    begin
      FColorList[i] := (Source as TSeStyleSysColors).FColorList[i];
    end;
  end
  else
    inherited;
end;

procedure TSeStyleSysColors.LoadFromStream(Stream: TStream);
var
  Count, i, ItemCount: integer;
begin
  Stream.Read(Count, SizeOf(Count));

  // Check if there are fewer items in the stream than the current declaration
  // of MaxSysColor, and read only as many as there are in the stream
  if Count < MaxSysColor then
    ItemCount := Count
  else
    ItemCount := MaxSysColor;

  for i := 0 to ItemCount - 1 do
  begin
    ReadString(Stream); // Name
    ReadString(Stream); // :
    FColorList[i] := StringToColor(ReadString(Stream));
  end;

  // If the stream contains more items than we read, advance stream to correct position
  if Count > ItemCount then
    for I := ItemCount to Count - 1 do
    begin
      ReadString(Stream); // Name
      ReadString(Stream); // :
      ReadString(Stream); // ColorName
    end;
end;

procedure TSeStyleSysColors.SaveToStream(Stream: TStream);
var
  i, Count: integer;
begin
  Count := MaxSysColor;
  Stream.Write(Count, SizeOf(Count));

  for i := 0 to MaxSysColor - 1  do
  begin
    WriteString(Stream, SysColors[i].Name);
    WriteString(Stream, ':');
    WriteString(Stream, ColorToString(FColorList[i]));
  end;
end;

function TSeStyleSysColors.GetColor(index: TColor): TColor;
var
  ColorName: string;
  i: integer;
begin
  Result := TColor(clNone);
  if IntToIdent(index, ColorName, SysColors) then
  begin
    for i := 0 to MaxSysColor - 1 do
      if SysColors[i].Value = index then
      begin
        Result := FColorList[i];
        Exit;
      end;
  end;
end;

procedure TSeStyleSysColors.SetColor(index: TColor; const Value: TColor);
var
  ColorName: string;
  i: integer;
begin
  if IntToIdent(index, ColorName, SysColors) then
  begin
    for i := 0 to MaxSysColor - 1 do
      if SysColors[i].Value = index then
      begin
        FColorList[i] := Value;
        Exit;
      end;
  end;
end;

{ TSeStyleFonts ===============================================================}

constructor TSeStyleFonts.Create;
var
  i: TSeStyleFont;
begin
  inherited Create;

  FStyle := AStyle;
  FStyleFont := TFont.Create;
  for i := Low(FFontList) to High(FFontList) do
  begin
    FFontList[i] := TFont.Create;
    FFontList[i].Name := 'Tahoma';
    FFontList[i].Color := clBlack;
  end;
end;

destructor TSeStyleFonts.Destroy;
var
  i: TSeStyleFont;
begin
  for i := Low(FFontList) to High(FFontList) do
    FFontList[i].Free;
  FStyleFont.Free;

  inherited;
end;

procedure TSeStyleFonts.Assign(Source: TPersistent);
var
  i: TSeStyleFont;
begin
  if Source is TSeStyleFonts then
  begin
    for i := Low(FFontList) to High(FFontList) do
      FFontList[i].Assign((Source as TSeStyleFonts).FFontList[i]);
  end
  else
    inherited;
end;

procedure TSeStyleFonts.LoadFromStream(Stream: TStream);
var
  Count, ItemCount, I: TSeStyleFont;
  FontString: string;
  EnumCount: Integer;
begin
  Stream.Read(Count, SizeOf(Count));

  // Check if there are fewer items in the stream than the current declaration
  // of TSeStyleFont, and read only as many as there are in the stream
  EnumCount := Integer(High(FFontList)) - Integer(Low(FFontList));
  if Integer(Count) < EnumCount then
    ItemCount := Count
  else
    ItemCount := High(FFontList);

  for I := Low(FFontList) to ItemCount do
  begin
    ReadString(Stream); // name
    ReadString(Stream); // :
    FontString := ReadString(Stream);

    { Decode }
    FFontList[I].Name := StringToFontName(FontString);
    FFontList[I].Size := StringToFontSize(FontString);
    FFontList[I].Color := StringToFontColor(FontString);
    FFontList[I].Style := StringToFontStyle(FontString);
  end;

  // If the stream contains more items than we read, advance stream to correct position
  if Count > ItemCount then
  begin
    Dec(Count);
    for I := ItemCount to Count do
    begin
      ReadString(Stream); // Name
      ReadString(Stream); // :
      ReadString(Stream); // FontName
    end;
  end;
end;

procedure TSeStyleFonts.SaveToStream(Stream: TStream);
var
  i, Count: TSeStyleFont;
begin
  Count := High(FFontList);
  Stream.Write(Count, SizeOf(Count));

  for i := Low(FFontList) to High(FFontList) do
  begin
    WriteString(Stream, StyleFontNames[i]);
    WriteString(Stream, ':');
    WriteString(Stream, FontToString(FFontList[i]));
  end;
end;

function TSeStyleFonts.GetFont(index: TSeStyleFont): TFont;
begin
  Result := FFontList[index];
end;

procedure TSeStyleFonts.SetFont(index: TSeStyleFont; const Value: TFont);
begin
  FFontList[index].Assign(Value);
end;

function TSeStyleFonts.GetFontForObject(Font: TSeStyleFont;
  const AObject: TSeStyleElementObject): TFont;
begin
  if (FStyle <> nil) and (AObject <> ktoDefault) then
  begin
    FStyleFont.Assign(Fonts[Font]);
    TSeHackStyle(FStyle).SetFontForObject(FStyleFont, Font, AObject);
    Result := FStyleFont;
  end
  else
    Result := Fonts[Font];
end;



{ TSeStyle class ===============================================================}


constructor TSeCustomStyle.Create;
begin
  inherited Create;
  FName := 'Default theme';
  FColors := TSeStyleColors.Create(Self);
  FSysColors := TSeStyleSysColors.Create(Self);
  FFonts := TSeStyleFonts.Create(Self);
  FMetrics := TSeStyleMetrics.Create(Self);
  FDrawing := TSeStyleDrawing.Create(Self);
  FOptions := TSeStyleOptions.Create(Self);

  ResetStyle;
end;

destructor TSeCustomStyle.Destroy;
begin
  FreeAndNil(FOptions);
  FreeAndNil(FMetrics);
  FreeAndNil(FDrawing);
  FreeAndNil(FFonts);
  FreeAndNil(FSysColors);
  FreeAndNil(FColors);
  inherited Destroy;
end;

{ Options }

class function TSeCustomStyle.GetStyleConfig: TSeStyleConfig;
begin
  Result.Building := False;
  Result.CanStore := False;
  Result.CanLoad := False;
  Result.DialogFilter := '';
  Result.DefaultExt := '';
end;

{ Class methods }

class function TseCustomStyle.GetName: string;
begin
  Result := 'Default';
end;

procedure TseCustomStyle.Assign(Source: TPersistent);
begin
  if Source is TSeCustomStyle then
  begin
    FName := TSeCustomStyle(Source).FName;
    FColors.Assign(TSeCustomStyle(Source).FColors);
    FSysColors.Assign(TSeCustomStyle(Source).FSysColors);
    FFonts.Assign(TSeCustomStyle(Source).Fonts);
    FOptions.Assign(TSeCustomStyle(Source).Options);
  end
  else
    inherited;
end;

{ I/O }

class function TseCustomStyle.CheckStream(Stream: TStream): boolean;
begin
  Result := False;
end;

function TseCustomStyle.LoadFromStream(Stream: TStream): boolean;
begin
  Result := False;
end;

function TseCustomStyle.SaveToStream(Stream: TStream): boolean;
begin
  Result := False;
end;

function TseCustomStyle.LoadFromFile(const FileName: string): boolean;
begin
  Result := False;
end;

function TseCustomStyle.SaveToFile(const FileName: string): boolean;
begin
  Result := False;
end;

{ Reset theme }

procedure TseCustomStyle.ResetStyleColors;
begin
  FColors[ktcWindow] := clSilver;
  FColors[ktcButton] := clBtnFace;
  FColors[ktcButtonHot] := clBtnFace;
  FColors[ktcButtonPressed] := clBtnFace;
  FColors[ktcButtonFocused] := clBtnFace;
  FColors[ktcButtonDisabled] := clBtnFace;
  FColors[ktcListBox] := clWindow;
  FColors[ktcTreeView] := clWindow;
  FColors[ktcGrid] := clWindow;
  FColors[ktcPanel] := clBtnFace;
  FColors[ktcPanelDisabled] := clBtnFace;
  FColors[ktcEdit] := clWindow;
  FColors[ktcEditDisabled] := clBtnFace;
  FColors[ktcHintGradientBase] := clInfoBk;

  FColors[ktcBorder] := clWindowFrame;
  FColors[ktcCategoryButtons] := clBtnFace;
  FColors[ktcCategoryPanelGroup] := clMedGray;
  FColors[ktcComboBox] := clWindow;
  FColors[ktcComboBoxDisabled] := clWindow;
  FColors[ktcGenericGradientBase] := $F0F0F0;
  FColors[ktcGenericGradientEnd] := $C0C0C0;
  FColors[ktcListBoxDisabled] := clWindow;
  FColors[ktcListView] := clWindow;
end;

procedure TseCustomStyle.SetFontForObject(AFont: TFont;
  const Font: TSeStyleFont; const AObject: TSeStyleElementObject);
begin
  AFont.Assign(Fonts[Font]);
end;

procedure TseCustomStyle.ResetStyleFonts;
begin
  SetFont(FFonts[ktfCaptionTextNormal], 'Tahoma', 8, [fsBold], clWhite);
  SetFont(FFonts[ktfCaptionTextInactive], 'Tahoma', 8, [fsBold], clSilver);
  SetFont(FFonts[ktfSmCaptionTextNormal], 'Tahoma', 8, [fsBold], clWhite);
  SetFont(FFonts[ktfSmCaptionTextInactive], 'Tahoma', 8, [fsBold], clSilver);
  SetFont(FFonts[ktfButtonTextNormal], 'Tahoma', 8, [], clBlack);
  SetFont(FFonts[ktfButtonTextPressed], 'Tahoma', 8, [], clBlack);
  SetFont(FFonts[ktfButtonTextHot], 'Tahoma', 8, [], clBlack);
  SetFont(FFonts[ktfButtonTextFocused], 'Tahoma', 8, [], clBlack);
  SetFont(FFonts[ktfButtonTextDisabled], 'Tahoma', 8, [], clBtnShadow);
  SetFont(FFonts[ktfCheckBoxTextNormal], 'Tahoma', 8, [], clBlack);
  SetFont(FFonts[ktfCheckBoxTextPressed], 'Tahoma', 8, [], clBlack);
  SetFont(FFonts[ktfCheckBoxTextHot], 'Tahoma', 8, [], clBlack);
  SetFont(FFonts[ktfCheckBoxTextFocused], 'Tahoma', 8, [], clBlack);
  SetFont(FFonts[ktfCheckBoxTextDisabled], 'Tahoma', 8, [], clBlack);
  SetFont(FFonts[ktfRadioButtonTextNormal], 'Tahoma', 8, [], clBlack);
  SetFont(FFonts[ktfRadioButtonTextPressed], 'Tahoma', 8, [], clBlack);
  SetFont(FFonts[ktfRadioButtonTextHot], 'Tahoma', 8, [], clBlack);
  SetFont(FFonts[ktfRadioButtonTextFocused], 'Tahoma', 8, [], clBlack);
  SetFont(FFonts[ktfRadioButtonTextDisabled], 'Tahoma', 8, [], clBlack);
  SetFont(FFonts[ktfGroupBoxTextNormal], 'Tahoma', 8, [], clBlack);
  SetFont(FFonts[ktfGroupBoxTextDisabled], 'Tahoma', 8, [], clBtnShadow);
  SetFont(FFonts[ktfWindowTextNormal], 'Tahoma', 8, [], clBlack);
  SetFont(FFonts[ktfWindowTextDisabled],  'Tahoma', 8, [], clBtnShadow);

  SetFont(FFonts[ktfStaticTextNormal], 'Tahoma', 8, [], clBlack);
  SetFont(FFonts[ktfStaticTextHot], 'Tahoma', 8, [], clBlack);
  SetFont(FFonts[ktfStaticTextFocused], 'Tahoma', 8, [], clBlack);
  SetFont(FFonts[ktfStaticTextDisabled], 'Tahoma', 8, [], clBlack);

  SetFont(FFonts[ktfEditBoxTextNormal],  'Tahoma', 8, [], clBlack);
  SetFont(FFonts[ktfEditBoxTextFocused],  'Tahoma', 8, [], clBlack);
  SetFont(FFonts[ktfEditBoxTextHot],  'Tahoma', 8, [], clBlack);
  SetFont(FFonts[ktfEditBoxTextSelected],  'Tahoma', 8, [], clWhite);
  SetFont(FFonts[ktfEditBoxTextDisabled],  'Tahoma', 8, [], clBtnShadow);

  SetFont(FFonts[ktfMenuItemTextDisabled],  'Tahoma', 8, [], clBtnShadow);
  SetFont(FFonts[ktfToolItemTextDisabled],  'Tahoma', 8, [], clBtnShadow);
  SetFont(FFonts[ktfPopupMenuItemTextDisabled],  'Tahoma', 8, [], clBtnShadow);

  SetFont(FFonts[ktfHeaderSectionTextHot],  'Tahoma', 8, [], clBlack);

end;

procedure TseCustomStyle.ResetStyle;
begin
  ResetStyleColors;
  ResetStyleFonts;
end;

{ Drawing }

procedure TseCustomStyle.DrawStyleText(Canvas: TCanvas; Text: TSeTextInfo; ADPI: Integer = 0);
var
  Flags: cardinal;
  FromTop: boolean;
  R, R1: TRect;
begin
  Flags := 0;
  if ktxsRTLReading in Text.Style then
  begin
    Flags := Flags or DT_RTLREADING;
    { Reverse align }
    if Text.Align = ktxaTopLeft then Text.Align := ktxaTopRight;
    if Text.Align = ktxaMiddleLeft then Text.Align := ktxaMiddleRight;
    if Text.Align = ktxaBottomLeft then Text.Align := ktxaBottomRight;
  end;

  if Text.Align = ktxaMiddleCenter then
    Flags := Flags or DT_CENTER;

  if ktxsNoPrefix in Text.Style then
    Flags := Flags or DT_NOPREFIX;
  if ktxsWordBreak in Text.Style then
    Flags := Flags or DT_WORDBREAK;
  if ktxsSingleLine in Text.Style then
    Flags := Flags or DT_SINGLELINE;
  if ktxsEndEllipsis in Text.Style then
    Flags := Flags or DT_END_ELLIPSIS;
  if ktxsExpandTabs in Text.Style then
    Flags := Flags or DT_EXPANDTABS;
  if ktxsWordEllipsis in Text.Style then
    Flags := Flags or DT_WORD_ELLIPSIS;

  FromTop := Text.Orientation = ktxoVerticalTop;

  if Text.Orientation in [ktxoVerticalTop, ktxoVerticalBottom] then
  begin
    case Text.Align of
      ktxaTopLeft:
        begin
          R := Text.Rect;
          DrawVerticalText(Canvas, Text.Text, R, Flags, FromTop);
        end;
      ktxaMiddleLeft:
        begin
          R := Text.Rect;
        end;
      ktxaBottomLeft:
        begin
          R := Text.Rect;
        end;
      ktxaTopCenter:
        begin
          R := Text.Rect;
        end;
      ktxaMiddleCenter:
        begin
          R := Text.Rect;
         with Text.Rect do
            R1 := TRect.Create(Left, Top, Bottom, Right);
          DrawText(Canvas, Text.Text, R1, Flags or DT_CALCRECT);
          R.Right := R.Left + R1.Height;
          R.Bottom := R.Top + R1.Width;
          OffsetRect(R, (Text.Rect.Width - R1.Height) div 2, (Text.Rect.Height - R1.Width) div 2);
          InflateRect(R, 0, 2);
          DrawVerticalText(Canvas, Text.Text, R, Flags, FromTop);
        end;
      ktxaBottomCenter:
        begin
          R := Text.Rect;
        end;
      ktxaTopRight:
        begin
          R := Text.Rect;
        end;
      ktxaMiddleRight:
        begin
          R := Text.Rect;
        end;
      ktxaBottomRight:
        begin
          R := Text.Rect;
        end;
    end;
  end
  else
  begin
    case Text.Align of
      ktxaTopLeft:
        begin
          R := Text.Rect;
          DrawText(Canvas, Text.Text, R, Flags);
        end;
      ktxaMiddleLeft:
        begin
          R := Text.Rect;
          DrawText(Canvas, Text.Text, R, Flags or DT_CALCRECT);
          OffsetRect(R, 0, (Text.Rect.Height - R.Height) div 2);
          DrawText(Canvas, Text.Text, R, Flags);
        end;
      ktxaBottomLeft:
        begin
          R := Text.Rect;
          DrawText(Canvas, Text.Text, R, Flags or DT_CALCRECT);
          OffsetRect(R, 0, Text.Rect.Height - R.Height);
          DrawText(Canvas, Text.Text, R, Flags);
        end;
      ktxaTopCenter:
        begin
          R := Text.Rect;
          DrawText(Canvas, Text.Text, R, Flags or DT_CALCRECT);
          OffsetRect(R, (Text.Rect.Width - R.Width) div 2, 0);
          DrawText(Canvas, Text.Text, R, Flags);
        end;
      ktxaMiddleCenter:
        begin
          R := Text.Rect;
          DrawText(Canvas, Text.Text, R, Flags or DT_CALCRECT);
          OffsetRect(R, (Text.Rect.Width - R.Width) div 2, (Text.Rect.Height - R.Height) div 2);
          DrawText(Canvas, Text.Text, R, Flags);
        end;
      ktxaBottomCenter:
        begin
          R := Text.Rect;
          DrawText(Canvas, Text.Text, R, Flags or DT_CALCRECT);
          OffsetRect(R, (Text.Rect.Width - R.Width) div 2, Text.Rect.Height - R.Height);
          DrawText(Canvas, Text.Text, R, Flags);
        end;
      ktxaTopRight:
        begin
          R := Text.Rect;
          DrawText(Canvas, Text.Text, R, Flags or DT_CALCRECT);
          OffsetRect(R, Text.Rect.Width - R.Width, 0);
          DrawText(Canvas, Text.Text, R, Flags);
        end;
      ktxaMiddleRight:
        begin
          R := Text.Rect;
          DrawText(Canvas, Text.Text, R, Flags or DT_CALCRECT);
          OffsetRect(R, Text.Rect.Width - R.Width, (Text.Rect.Height - R.Height) div 2);
          DrawText(Canvas, Text.Text, R, Flags);
        end;
      ktxaBottomRight:
        begin
          R := Text.Rect;
          DrawText(Canvas, Text.Text, R, Flags or DT_CALCRECT);
          OffsetRect(R, Text.Rect.Width - R.Width, Text.Rect.Height - R.Height);
          DrawText(Canvas, Text.Text, R, Flags);
        end;
    end;
  end;
end;

{ Window class ================================================================}

function TseCustomStyle.IsWindowDefined(const ASubclass: TSeWindowSubclass;
  const AObject: TSeStyleElementObject = ktoDefault): boolean;
begin
  {$IFDEF THEME_DEBUG}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function TseCustomStyle.WindowGetRegion(const ASubclass: TSeWindowSubclass; const ARect: TRect;
  const AObject: TSeStyleElementObject = ktoDefault): TSeRegion;
begin
  Result := 0;
end;

function TseCustomStyle.WindowGetHitTest(const ASubclass: TSeWindowSubclass; AWindow: TSeWindowInfo;
  X, Y: integer; ADPI: Integer = 0; const AObject: TSeStyleElementObject = ktoDefault): TSeWindowHitTest;
var
  P: TPoint;
  CR, LeftR, TopR, RightR, BottomR: TRect;
  SysBtn: TSeWindowButtonClass;
begin
  Result := kwhtClient;
  P := Point(X, Y);
  case ASubclass of
    kwscStandard, kwscDialog, kwscBorder, kwscToolWindow:
      begin
        { Calc rects }
        CR := WindowGetClientRect(ASubclass, AWindow.Rect);
        with AWindow.Rect do
        begin
          LeftR := Rect(Left, Top, CR.Left, Bottom);
          TopR := Rect(Left, Top, Right, CR.Top);
          RightR := Rect(CR.Right, Top, Right, Bottom);
          BottomR := Rect(Left, CR.Bottom, Right, Bottom);
        end;

        { Check sides }
        if PtInRect(LeftR, P) then Result := kwhtLeft;
        if PtInRect(TopR, P) then Result := kwhtTop;
        if PtInRect(RightR, P) then Result := kwhtRight;
        if PtInRect(BottomR, P) then Result := kwhtBottom;

        { Angles }
        if PtInRect(LeftR, P) and PtInRect(TopR, P) then Result := kwhtTopLeft;
        if PtInRect(RightR, P) and PtInRect(TopR, P) then Result := kwhtTopRight;
        if PtInRect(LeftR, P) and PtInRect(BottomR, P) then Result := kwhtBottomLeft;
        if PtInRect(RightR, P) and PtInRect(BottomR, P) then Result := kwhtBottomRight;

        { Title }
        if PtInRect(WindowGetTitleRect(ASubclass, AWindow), P) then Result := kwhtCaption;

        { WindowButton }
        for SysBtn := Low(SysBtn) to High(SysBtn) do
          if PtInRect(WindowGetButtonRect(ASubclass, AWindow, SysBtn), P) then
          begin
            Result := WindowButtonToHitTest(SysBtn);
            Break;
          end;
      end;
  end;
end;

function TseCustomStyle.WindowGetClientRect(const ASubclass: TSeWindowSubclass;
  const ARect: TRect; ADPI: Integer = 0; const AObject: TSeStyleElementObject = ktoDefault): TRect;
begin
  Result := NullRect;
  Result.Left := ARect.Left + 4;
  Result.Top := ARect.Top + 22;
  Result.Right := ARect.Right - 4;
  Result.Bottom := ARect.Bottom - 4;
end;

function TseCustomStyle.WindowGetTitleRect(const ASubclass: TSeWindowSubclass;
  AWindow: TSeWindowInfo; ADPI: Integer = 0; const AObject: TSeStyleElementObject = ktoDefault): TRect;
begin
  { This function return TitleRect. Title can posisition on all sides }
  Result := NullRect;
  Result := WindowGetClientRect(ASubClass, AWindow.Rect);
  Result.Bottom := Result.Top;
  Result.Top := 3;
end;

function TseCustomStyle.WindowGetButtonRect(const ASubclass: TSeWindowSubclass;
  AWindow: TSeWindowInfo; AButton: TSeWindowButtonClass; ADPI: Integer = 0;  const AObject: TSeStyleElementObject = ktoDefault): TRect;
const
  StdBtnWidth = 22;
  SmBtnWidth = 16;
var
  CR, BtnRect: TRect;
begin
  Result := NullRect;
  CR := WindowGetClientRect(ASubclass, AWindow.Rect);

  with AWindow.Rect do
    BtnRect := Rect(CR.Right - StdBtnWidth - 1, 4, CR.Right - 1, CR.Top - 1);
  Result := BtnRect;

  case AButton of
    kwbClose:
      if kwbClose in AWindow.Buttons then
      begin
        Result := BtnRect;
      end;
    kwbHelp:
      if kwbHelp in AWindow.Buttons then
      begin
        Result := BtnRect;
        if (kwbClose in AWindow.Buttons) then
          OffsetRect(Result, -StdBtnWidth, 0);
        if (kwbMax in AWindow.Buttons) or (kwbMaxRestore in AWindow.Buttons) then
          OffsetRect(Result, -StdBtnWidth, 0);
        if (kwbMin in AWindow.Buttons) or (kwbMinRestore in AWindow.Buttons) then
          OffsetRect(Result, -StdBtnWidth, 0);
        if (kwbRoll in AWindow.Buttons) or (kwbRollRestore in AWindow.Buttons) then
          OffsetRect(Result, -StdBtnWidth, 0);
        if (kwbTray in AWindow.Buttons) then
          OffsetRect(Result, -StdBtnWidth, 0);
      end;
    kwbMin, kwbMinRestore:
      if (kwbMin in AWindow.Buttons) or (kwbMinRestore in AWindow.Buttons) then
      begin
        Result := BtnRect;
        if (kwbClose in AWindow.Buttons) then
          OffsetRect(Result, -StdBtnWidth, 0);
        if (kwbMax in AWindow.Buttons) or (kwbMaxRestore in AWindow.Buttons) then
          OffsetRect(Result, -StdBtnWidth, 0);
      end;
    kwbMax, kwbMaxRestore:
      if (kwbMax in AWindow.Buttons) or (kwbMaxRestore in AWindow.Buttons) then
      begin
        Result := BtnRect;
        if (kwbClose in AWindow.Buttons) then
          OffsetRect(Result, -StdBtnWidth, 0);
      end;
    kwbRoll, kwbRollRestore:
      if (kwbRoll in AWindow.Buttons) or (kwbRollRestore in AWindow.Buttons) then
      begin
        Result := BtnRect;
        if (kwbClose in AWindow.Buttons) then
          OffsetRect(Result, -StdBtnWidth, 0);
        if (kwbMax in AWindow.Buttons) or (kwbMaxRestore in AWindow.Buttons) then
          OffsetRect(Result, -StdBtnWidth, 0);
        if (kwbMin in AWindow.Buttons) or (kwbMinRestore in AWindow.Buttons) then
          OffsetRect(Result, -StdBtnWidth, 0);
      end;
    kwbTray:
      if (kwbTray in AWindow.Buttons) then
      begin
        Result := BtnRect;
        if (kwbClose in AWindow.Buttons) then
          OffsetRect(Result, -StdBtnWidth, 0);
        if (kwbMax in AWindow.Buttons) or (kwbMaxRestore in AWindow.Buttons) then
          OffsetRect(Result, -StdBtnWidth, 0);
        if (kwbMin in AWindow.Buttons) or (kwbMinRestore in AWindow.Buttons) then
          OffsetRect(Result, -StdBtnWidth, 0);
        if (kwbRoll in AWindow.Buttons) or (kwbRollRestore in AWindow.Buttons) then
          OffsetRect(Result, -StdBtnWidth, 0);
      end;
    kwbSysMenu:
      if (kwbSysMenu in AWindow.Buttons) then
      begin
        Result := Rect(CR.Left, 4, CR.Left + StdBtnWidth, CR.Top - 1);
      end;
  end;
end;

function TseCustomStyle.WindowGetFixPosition(const ASubclass: TSeWindowSubclass;
   AButton: TSeWindowButtonClass; ADPI: Integer = 0;
   const AObject: TSeStyleElementObject = ktoDefault): Boolean;
begin
  Result := False;
end;


procedure TseCustomStyle.WindowDraw(const ASubclass: TSeWindowSubclass; Canvas: TCanvas;
  AWindow: TSeWindowInfo; ADPI: Integer = 0; const AObject: TSeStyleElementObject = ktoDefault);
begin
end;

procedure TseCustomStyle.WindowDrawClient(const ASubclass: TSeWindowSubclass;
  Canvas: TCanvas; const ARect: TRect; ADPI: Integer = 0;
  const AObject: TSeStyleElementObject = ktoDefault);
begin
end;

procedure TseCustomStyle.WindowDrawText(const ASubclass: TSeWindowSubclass; Canvas: TCanvas;
  AWindow: TSeWindowInfo; const ARect: TRect; RightToLeftReading: Boolean; ADPI: Integer = 0;
  const AObject: TSeStyleElementObject = ktoDefault);
begin
  if kwdsActive in AWindow.State then
    Canvas.Font := Fonts[ktfCaptionTextNormal]
  else
    Canvas.Font := Fonts[ktfCaptionTextInactive];

  DrawStyleText(Canvas, TextInfo(ARect, AWindow.Title, DT_CENTER or DT_VCENTER or DT_SINGLELINE), ADPI);
end;

procedure TseCustomStyle.WindowDrawButton(const ASubclass: TSeWindowSubclass; Canvas: TCanvas;
  AButton: TSeWindowButtonInfo; ADPI: Integer = 0; const AObject: TSeStyleElementObject = ktoDefault);
begin
end;

procedure TseCustomStyle.WindowDrawGripper(const ASubclass: TSeWindowSubclass; Canvas: TCanvas;
  AGripper: TSeWindowGripperInfo; ADPI: Integer = 0; const AObject: TSeStyleElementObject = ktoDefault);
begin
end;

{ Hint class ==================================================================}

function TseCustomStyle.IsHintDefined(const ASubclass: TSeHintSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin
  {$IFDEF THEME_DEBUG}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function TseCustomStyle.HintGetClientRect(const ASubclass: TSeHintSubclass; Canvas: TCanvas;
  AHint: TSeHintInfo; const AObject: TSeStyleElementObject = ktoDefault): TRect;
begin
  Result := AHint.Rect;
  InflateRect(Result, -10, -13);
end;

procedure TseCustomStyle.HintDraw(const ASubclass: TSeHintSubclass; Canvas: TCanvas;
  AHint: TSeHintInfo; const AObject: TSeStyleElementObject = ktoDefault);
begin
end;

{ Label class =================================================================}

function TseCustomStyle.IsLabelDefined(const ASubclass: TSeLabelSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin
  {$IFDEF THEME_DEBUG}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function TseCustomStyle.IsLabelTransparent(const ASubclass: TSeLabelSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin
  Result := True;
end;

procedure TseCustomStyle.LabelDrawText(const ASubclass: TSeLabelSubclass; Canvas: TCanvas;
  ALabel: TSeLabelInfo; AText: TSeTextInfo; const AObject: TSeStyleElementObject = ktoDefault);
begin
  with ALabel do
    DrawStyleText(Canvas, AText);
end;

{ Panel class =================================================================}

function TseCustomStyle.IsPanelDefined(const ASubclass: TSePanelSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin
  {$IFDEF THEME_DEBUG}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function TseCustomStyle.IsPanelTransparent(const ASubclass: TSePanelSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin
  Result := False;
end;

function TseCustomStyle.PanelGetClientRect(const ASubclass: TSePanelSubclass;
  APanel: TSePanelInfo; const AObject: TSeStyleElementObject = ktoDefault): TRect;
begin
  Result := APanel.Rect;
end;

procedure TseCustomStyle.PanelDraw(const ASubclass: TSePanelSubclass; Canvas: TCanvas;
  APanel: TSePanelInfo; const AObject: TSeStyleElementObject = ktoDefault);
begin
end;

procedure TseCustomStyle.PanelDrawButton(const ASubclass: TSePanelSubclass;
  Canvas: TCanvas; AButton: TSePanelButtonInfo;
  const AObject: TSeStyleElementObject);
begin
end;

procedure TseCustomStyle.PanelDrawText(const ASubclass: TSePanelSubclass; Canvas: TCanvas;
  APanel: TSePanelInfo; AText: TSeTextInfo; const AObject: TSeStyleElementObject = ktoDefault);
begin
  DrawStyleText(Canvas, AText);
end;

{ Button class ================================================================}

function TseCustomStyle.IsButtonDefined(const ASubclass: TSeButtonSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin
  {$IFDEF THEME_DEBUG}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function TseCustomStyle.IsButtonTransparent(const ASubclass: TSeButtonSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin
  Result := False;
end;

procedure TseCustomStyle.ButtonDraw(const ASubclass: TSeButtonSubclass; Canvas: TCanvas;
  AButton: TSeButtonInfo; ADPI: Integer = 0; const AObject: TSeStyleElementObject = ktoDefault);
begin

end;

procedure TseCustomStyle.ButtonDrawText(const ASubclass: TSeButtonSubclass; Canvas: TCanvas;
  AButton: TSeButtonInfo; AText: TSeTextInfo; const AObject: TSeStyleElementObject = ktoDefault);
begin
  { Draw }
  DrawStyleText(Canvas, AText);
end;

procedure TseCustomStyle.ButtonDrawGlyph(const ASubclass: TSeButtonSubclass; Canvas: TCanvas;
  AButton: TSeButtonInfo; AGlyph: TSeGlyphInfo; const AObject: TSeStyleElementObject = ktoDefault);
begin
  with AButton do
  begin
    AGlyph.Glyph.Draw(Canvas, AGlyph.Rect.Left, AGlyph.Rect.Top);
  end;
end;

{ Check class =================================================================} 

function TseCustomStyle.IsCheckDefined(const ASubclass: TSeCheckSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin
  {$IFDEF THEME_DEBUG}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function TseCustomStyle.IsCheckTransparent(const ASubclass: TSeCheckSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin
  Result := True;
end;

function TseCustomStyle.CheckGetSize(const ASubclass: TSeCheckSubclass; ADPI: Integer = 0;
      const AObject: TSeStyleElementObject = ktoDefault): TPoint;
begin
  Result.X := 13;
  Result.Y := 13;
  if ADPI > DPI_DEFAULT then
  begin
    Result.X := MulDiv(Result.X, ADPI, DPI_DEFAULT);
    Result.Y := MulDiv(Result.Y, ADPI, DPI_DEFAULT);
  end;
end;

procedure TseCustomStyle.CheckDraw(const ASubclass: TSeCheckSubclass; Canvas: TCanvas;
  ACheck: TSeCheckInfo; ADPI: Integer = 0; const AObject: TSeStyleElementObject = ktoDefault);
begin
end;

procedure TseCustomStyle.CheckDrawText(const ASubclass: TSeCheckSubclass; Canvas: TCanvas;
  ACheck: TSeCheckInfo; AText: TSeTextInfo; const AObject: TSeStyleElementObject = ktoDefault);
begin
  DrawStyleText(Canvas, AText);
end;

{ Group class =================================================================}

function TseCustomStyle.IsGroupDefined(const ASubclass: TSeGroupSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin
  {$IFDEF THEME_DEBUG}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function TseCustomStyle.IsGroupTransparent(const ASubclass: TSeGroupSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin
  Result := True;
end;

function TseCustomStyle.GroupGetClientRect(const ASubclass: TSeGroupSubclass;
  AGroup: TSeGroupInfo; const AObject: TSeStyleElementObject = ktoDefault): TRect;
begin
  Result := AGroup.Rect;
end;

procedure TseCustomStyle.GroupDraw(const ASubclass: TSeGroupSubclass; Canvas: TCanvas;
  AGroup: TSeGroupInfo; const AObject: TSeStyleElementObject = ktoDefault);
begin
end;

procedure TseCustomStyle.GroupDrawText(const ASubclass: TSeGroupSubclass; Canvas: TCanvas;
  AGroup: TSeGroupInfo; AText: TSeTextInfo; const AObject: TSeStyleElementObject = ktoDefault);
begin
  DrawStyleText(Canvas, AText);
end;

{ Scroll class ================================================================}

function TseCustomStyle.IsScrollDefined(const ASubclass: TSeScrollSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin
  {$IFDEF THEME_DEBUG}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function TseCustomStyle.IsScrollTransparent(const ASubclass: TSeScrollSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin
  Result := False;
end;

procedure TseCustomStyle.ScrollDraw(const ASubclass: TSeScrollSubclass; Canvas: TCanvas;
  AScroll: TSeScrollInfo; ADPI: Integer = 0;  const AObject: TSeStyleElementObject = ktoDefault);
begin
end;

procedure TseCustomStyle.ScrollDrawButton(const ASubclass: TSeScrollSubclass; Canvas: TCanvas;
  AButton: TSeScrollButtonInfo; ADPI: Integer = 0;
  const AObject: TSeStyleElementObject = ktoDefault);
begin
end;

procedure TseCustomStyle.ScrollDrawArea(const ASubclass: TSeScrollSubclass; Canvas: TCanvas;
  AArea: TSeScrollAreaInfo; ADPI: Integer = 0;  const AObject: TSeStyleElementObject = ktoDefault);
begin
end;

{ Progress class ==============================================================}

function TseCustomStyle.IsProgressDefined(const ASubclass: TSeProgressSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin
  {$IFDEF THEME_DEBUG}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function TseCustomStyle.IsProgressTransparent(const ASubclass: TSeProgressSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin
  Result := False;
end;

function TseCustomStyle.ProgressGetClientRect(const ASubclass: TSeProgressSubclass;
  AProgress: TSeProgressInfo; const AObject: TSeStyleElementObject): TRect;
begin
  Result := AProgress.Rect;
end;

procedure TseCustomStyle.ProgressDraw(const ASubclass: TSeProgressSubclass; Canvas: TCanvas;
  AProgress: TSeProgressInfo; const AObject: TSeStyleElementObject = ktoDefault);
begin
end;

{ Edit class ==================================================================}

function TseCustomStyle.IsEditDefined(const ASubclass: TSeEditSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin
  {$IFDEF THEME_DEBUG}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function TseCustomStyle.IsEditTransparent(const ASubclass: TSeEditSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin
  Result := False;
end;

function TseCustomStyle.EditGetClientRect(const ASubclass: TSeEditSubclass; Canvas: TCanvas;
  AEdit: TSeEditInfo; const AObject: TSeStyleElementObject = ktoDefault): TRect;
begin
  Result := AEdit.Rect;
  InflateRect(Result, -3, -3);
end;

procedure TseCustomStyle.EditDraw(const ASubclass: TSeEditSubclass; Canvas: TCanvas;
  AEdit: TSeEditInfo; const AObject: TSeStyleElementObject = ktoDefault);
begin
end;

procedure TseCustomStyle.EditDrawButton(const ASubclass: TSeEditSubclass; Canvas: TCanvas;
  AButton: TSeEditButtonInfo; ADPI: Integer = 0; const AObject: TSeStyleElementObject = ktoDefault);
begin
end;

{ Splitter class ==============================================================}

function TseCustomStyle.IsSplitterDefined(const ASubclass: TSeSplitterSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin
  {$IFDEF THEME_DEBUG}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function TseCustomStyle.IsSplitterTransparent(
  const ASubclass: TSeSplitterSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin
  Result := False;
end;

procedure TseCustomStyle.SplitterDraw(const ASubclass: TSeSplitterSubclass; Canvas: TCanvas;
  ASplitter: TSeSplitterInfo; const AObject: TSeStyleElementObject = ktoDefault);
begin
end;

{ List class ==================================================================}

function TseCustomStyle.IsListDefined(const ASubclass: TSeListSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin
  {$IFDEF THEME_DEBUG}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function TseCustomStyle.IsListTransparent(const ASubclass: TSeListSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin
  Result := False;
end;

function TseCustomStyle.ListGetClientRect(const ASubclass: TSeListSubclass;
  AList: TSeListInfo; const AObject: TSeStyleElementObject = ktoDefault): TRect;
begin
  Result := AList.Rect;
  InflateRect(Result, -4, -5);
end;

procedure TseCustomStyle.ListDraw(const ASubclass: TSeListSubclass; Canvas: TCanvas;
  AList: TSeListInfo; const AObject: TSeStyleElementObject = ktoDefault);
begin
end;

procedure TseCustomStyle.ListDrawColumn(const ASubclass: TSeListSubclass;
  Canvas: TCanvas; AColumn: TSeListColumnInfo; const AObject: TSeStyleElementObject = ktoDefault);
begin

end;

procedure TseCustomStyle.ListDrawItem(const ASubclass: TSeListSubclass; Canvas: TCanvas;
  AItem: TSeListItemInfo; const AObject: TSeStyleElementObject = ktoDefault);
begin
end;

procedure TseCustomStyle.ListDrawGlyph(const ASubclass: TSeListSubclass; Canvas: TCanvas;
  AItem: TSeListItemInfo; AGlyph: TSeListGlyphInfo; const AObject: TSeStyleElementObject = ktoDefault);
begin
end;

procedure TseCustomStyle.ListDrawText(const ASubclass: TSeListSubclass; Canvas: TCanvas;
  AItem: TSeListItemInfo; AText: TSeTextInfo; const AObject: TSeStyleElementObject = ktoDefault);
begin
  DrawStyleText(Canvas, AText);
end;

{ Track class =================================================================}

function TseCustomStyle.IsTrackDefined(const ASubclass: TSeTrackSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin
  {$IFDEF THEME_DEBUG}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function TseCustomStyle.IsTrackTransparent(const ASubclass: TSeTrackSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin
  Result := True;
end;

procedure TseCustomStyle.TrackDraw(const ASubclass: TSeTrackSubclass; Canvas: TCanvas;
  ATrack: TSeTrackInfo; ADPI: Integer = 0; const AObject: TSeStyleElementObject = ktoDefault);
begin
end;

procedure TseCustomStyle.TrackBarDraw(const ASubclass: TSeTrackSubclass;
  Canvas: TCanvas; ABar: TSeTrackBarInfo; ADPI: Integer = 0; const AObject: TSeStyleElementObject = ktoDefault);
begin
end;

procedure TseCustomStyle.TrackDrawThumb(const ASubclass: TSeTrackSubclass; Canvas: TCanvas;
  AThumb: TSeTrackThumbInfo; ADPI: Integer = 0; const AObject: TSeStyleElementObject = ktoDefault);
begin
end;

{ Header class ================================================================}

function TseCustomStyle.IsHeaderDefined(const ASubclass: TSeHeaderSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin
  {$IFDEF THEME_DEBUG}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function TseCustomStyle.IsHeaderTransparent(const ASubclass: TSeHeaderSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin
  Result := False;
end;

procedure TseCustomStyle.HeaderDraw(const ASubclass: TSeHeaderSubclass; Canvas: TCanvas;
  AHeader: TSeHeaderInfo; const AObject: TSeStyleElementObject = ktoDefault);
begin
end;

procedure TseCustomStyle.HeaderDrawSection(const ASubclass: TSeHeaderSubclass; Canvas: TCanvas;
  ASection: TSeHeaderSectionInfo; const AObject: TSeStyleElementObject = ktoDefault);
begin
end;

procedure TseCustomStyle.HeaderDrawText(const ASubclass: TSeHeaderSubclass; Canvas: TCanvas;
  ASection: TSeHeaderSectionInfo; AText: TSeTextInfo; const AObject: TSeStyleElementObject = ktoDefault);
begin
  DrawStyleText(Canvas, AText);
end;

procedure TseCustomStyle.HeaderDrawGlyph(const ASubclass: TSeHeaderSubclass; Canvas: TCanvas;
  ASection: TSeHeaderSectionInfo; AGlyph: TSeGlyphInfo; const AObject: TSeStyleElementObject = ktoDefault);
begin
  if AGlyph.Glyph <> nil then
    AGlyph.Glyph.Draw(Canvas, AGlyph.Rect);
end;

{ Status class ================================================================}

function TseCustomStyle.IsStatusDefined(const ASubclass: TSeStatusSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin
  {$IFDEF THEME_DEBUG}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function TseCustomStyle.IsStatusTransparent(const ASubclass: TSeStatusSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin
  Result := False;
end;

function TseCustomStyle.StatusGetClientRect(const ASubclass: TSeStatusSubclass;
  AStatus: TSeStatusInfo; const AObject: TSeStyleElementObject = ktoDefault): TRect;
begin
  Result := AStatus.Rect;
  InflateRect(Result, -5, -2);
end;

procedure TseCustomStyle.StatusDraw(const ASubclass: TSeStatusSubclass; Canvas: TCanvas;
  AStatus: TSeStatusInfo; ADPI: Integer = 0; const AObject: TSeStyleElementObject = ktoDefault);
begin
end;

procedure TseCustomStyle.StatusDrawPanel(const ASubclass: TSeStatusSubclass; Canvas: TCanvas;
  APanel: TSeStatusPanelInfo; ADPI: Integer = 0; const AObject: TSeStyleElementObject = ktoDefault);
begin
end;

procedure TseCustomStyle.StatusDrawGlyph(const ASubclass: TSeStatusSubclass; Canvas: TCanvas;
  APanel: TSeStatusPanelInfo; AGlyph: TSeGlyphInfo; const AObject: TSeStyleElementObject = ktoDefault);
begin
  if AGlyph.Glyph <> nil then
    AGlyph.Glyph.Draw(Canvas, AGlyph.Rect);
end;

procedure TseCustomStyle.StatusDrawText(const ASubclass: TSeStatusSubclass; Canvas: TCanvas;
  APanel: TSeStatusPanelInfo; AText: TSeTextInfo; const AObject: TSeStyleElementObject = ktoDefault);
begin
  DrawStyleText(Canvas, AText);
end;

{ Tab class ===================================================================}

function TseCustomStyle.IsTabDefined(const ASubclass: TSeTabSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin
  {$IFDEF THEME_DEBUG}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function TseCustomStyle.IsTabTransparent(const ASubclass: TSeTabSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin
  Result := True;
end;

function TseCustomStyle.TabGetClientRect(const ASubclass: TSeTabSubclass;
  ATab: TSeTabInfo; const AObject: TSeStyleElementObject = ktoDefault): TRect;
begin
  Result := ATab.Rect;
end;

procedure TseCustomStyle.TabDraw(const ASubclass: TSeTabSubclass; Canvas: TCanvas;
  ATab: TSeTabInfo; const AObject: TSeStyleElementObject = ktoDefault);
begin
end;

procedure TseCustomStyle.TabDrawItem(const ASubclass: TSeTabSubclass; Canvas: TCanvas;
  AItem: TSeTabItemInfo; const AObject: TSeStyleElementObject = ktoDefault);
begin
end;

procedure TseCustomStyle.TabDrawGlyph(const ASubclass: TSeTabSubclass; Canvas: TCanvas;
  AItem: TSeTabItemInfo; AGlyph: TSeGlyphInfo;
  const AObject: TSeStyleElementObject = ktoDefault);
begin
  AGlyph.Glyph.Draw(Canvas, AGlyph.Rect.Left, AGlyph.Rect.Top);
end;

procedure TseCustomStyle.TabDrawText(const ASubclass: TSeTabSubclass; Canvas: TCanvas;
  AItem: TSeTabItemInfo; AText: TSeTextInfo;
  const AObject: TSeStyleElementObject = ktoDefault);
begin
  DrawStyleText(Canvas, AText);
end;

procedure TseCustomStyle.TabDrawButton(const ASubclass: TSeTabSubclass; Canvas: TCanvas;
  AButton: TSeTabButtonInfo; const AObject: TSeStyleElementObject = ktoDefault);
begin
end;

{ BarDock class ===============================================================}

function TseCustomStyle.IsBarDockDefined(const ASubclass: TSeBarDockSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin
  {$IFDEF THEME_DEBUG}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

procedure TseCustomStyle.BarDockDraw(const ASubclass: TSeBarDockSubclass;
  Canvas: TCanvas; ADock: TSeBarDockInfo; const AObject: TSeStyleElementObject);
begin
end;

function TseCustomStyle.BarDockGetClientRect(const ASubclass: TSeBarDockSubclass; Canvas: TCanvas;
  ADock: TSeBarDockInfo; const AObject: TSeStyleElementObject): TRect;
begin
  Result := ADock.Rect;
  InflateRect(Result, -3, -3);
end;

{ Bar class ===================================================================}

function TseCustomStyle.IsBarDefined(const ASubclass: TSeBarSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin
  {$IFDEF THEME_DEBUG}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function TseCustomStyle.IsBarTransparent(const ASubclass: TSeBarSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin
  Result := False;
end;

function TseCustomStyle.BarGetClientRect(const ASubclass: TSeBarSubclass; Canvas: TCanvas;
  ABar: TSeBarInfo; const AObject: TSeStyleElementObject = ktoDefault): TRect;
begin
  Result := ABar.Rect;
end;

procedure TseCustomStyle.BarDraw(const ASubclass: TSeBarSubclass; Canvas: TCanvas;
  ABar: TSeBarInfo; const AObject: TSeStyleElementObject = ktoDefault);
begin

end;

procedure TseCustomStyle.BarDrawCaption(const ASubclass: TSeBarSubclass;
  Canvas: TCanvas; ABar: TSeBarInfo; ACaption: TSeBarCaptionInfo;
  const AObject: TSeStyleElementObject);
begin
end;

procedure TseCustomStyle.BarDrawButton(const ASubclass: TSeBarSubclass; Canvas: TCanvas;
  AButton: TSeBarButtonInfo; const AObject: TSeStyleElementObject = ktoDefault);
begin
end;

procedure TseCustomStyle.BarDrawItem(const ASubclass: TSeBarSubclass;
  Canvas: TCanvas; AItem: TSeBarItemInfo; const AObject: TSeStyleElementObject);
begin
end;

procedure TseCustomStyle.BarDrawText(const ASubclass: TSeBarSubclass;
  Canvas: TCanvas; AText: TSeTextInfo;
  const AObject: TSeStyleElementObject);
begin
  DrawStyleText(Canvas, AText);
end;

{ Menu class ==================================================================}

function TseCustomStyle.IsMenuDefined(const ASubclass: TSeMenuSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin
  {$IFDEF THEME_DEBUG}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function TseCustomStyle.IsMenuTransparent(const ASubclass: TSeMenuSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin
  Result := False;
end;

function TseCustomStyle.MenuGetClientRect(const ASubclass: TSeMenuSubclass;
  AMenu: TSeMenuInfo; const AObject: TSeStyleElementObject = ktoDefault): TRect;
begin
  Result := AMenu.Rect;
  InflateRect(Result, -3, -5);
end;

procedure TseCustomStyle.MenuDraw(const ASubclass: TSeMenuSubclass; Canvas: TCanvas;
  AMenu: TSeMenuInfo; const AObject: TSeStyleElementObject = ktoDefault);
begin
end;

procedure TseCustomStyle.MenuDrawItem(const ASubclass: TSeMenuSubclass; Canvas: TCanvas;
  AItem: TSeMenuItemInfo; ADPI: Integer = 0; const AObject: TSeStyleElementObject = ktoDefault);
begin
  with AItem do
  begin
    case Item of
      kmicNormal:
        begin
          if DrawState = kmidsDisabled then
            FillRect(Canvas, Rect, clWhite)
          else
            if DrawState = kmidsSelected then
              FillRect(Canvas, Rect, clGray)
            else
              if DrawState = kmidsHot then
                FillRect(Canvas, Rect, clYellow)
              else
                FillRect(Canvas, Rect, clSilver);
           DrawRect(Canvas, Rect, clRed);
        end;
      kmicSeparator:
        begin
          InflateRect(Rect, -3, - (Rect.Height div 2));
          FillRect(Canvas, Rect, clRed);
        end;
    end;
  end;
end;


procedure TseCustomStyle.MenuDrawItemGlyph(const ASubclass: TSeMenuSubclass; Canvas: TCanvas;
  AItem: TSeMenuItemInfo; AGlyph: TSeMenuGlyphInfo; ADPI: Integer = 0; const AObject: TSeStyleElementObject = ktoDefault);

procedure DrawMenuCheckImage(ACanvas: TCanvas; ARect: TRect; AColor: TColor; AScaleFactor: Double);
var
  LSaveIndex: Integer;
  LOldPen: HPEN;
  X, Y: Integer;
  LSize, LSize2, LSize3: Integer;
  I: Integer;
begin
  LSaveIndex := SaveDC(ACanvas.Handle);
  try
    LOldPen := SelectObject(ACanvas.Handle, GetStockObject(DC_PEN));
    SetDCPenColor(ACanvas.Handle, ColorToRGB(AColor));
    LSize := Round(10 * AScaleFactor);
    LSize2 := LSize div 2;
    LSize := LSize2 * 2;
    LSize3 := Round(2 * AScaleFactor);
    X := ARect.Left + ARect.Width div 2 + LSize3 div 2;
    Y := ARect.Top + ARect.Height div 2 + LSize3 div 2;
    X := X - LSize2;
    for I := 1 to Round(AScaleFactor) * 2 do
    begin
      ACanvas.MoveTo(X, Y);
      ACanvas.LineTo(X + LSize2 - LSize3, Y + LSize2 - LSize3);
      ACanvas.LineTo(X + LSize, Y - LSize3 * 2);
      Dec(Y);
      ACanvas.MoveTo(X + 1, Y + 1);
      ACanvas.LineTo(X + LSize2 - LSize3, Y + LSize2 - LSize3);
      ACanvas.LineTo(X + LSize - 1, Y - LSize3 * 2 + 1);
    end;
    SelectObject(ACanvas.Handle, LOldPen);
  finally
    RestoreDC(ACanvas.Handle, LSaveIndex);
  end;
end;

var
  R: TRect;
  X, CX: integer;
  LColor: TColor;
  LScaleFactor: Double;
begin
  with AGlyph do
  begin
    LScaleFactor := ADPI / Screen.DefaultPixelsPerInch;
    if LScaleFactor < 1 then
      LScaleFactor := 1;
    case Kind of
      kmgCheckGlyph:
        begin
          CX := 16;
          if ADPI > 0 then
            CX := MulDiv(CX, ADPI, Screen.DefaultPixelsPerInch);
          R := TRect.Create(0, 0, CX, CX);
          RectCenter(R, Rect);
          if AItem.DrawState = kmidsDisabled then
            LColor := Fonts[ktfPopupMenuItemTextDisabled].Color
          else
            LColor := Fonts[ktfPopupMenuItemTextNormal].Color;
          DrawMenuCheckImage(Canvas, R, LColor, LScaleFactor);
        end;
      kmgRadioGlyph:
        begin
          CX := 6;
          if ADPI > 0 then
            CX := MulDiv(CX, ADPI, Screen.DefaultPixelsPerInch);
          R := TRect.Create(0, 0, CX, CX);
          RectCenter(R, Rect);
          case AItem.DrawState of
            kmidsNormal: FillRoundRect(Canvas, R, CX div 2 + Round(LScaleFactor), Fonts[ktfPopupMenuItemTextNormal].Color);
            kmidsDisabled: FillRoundRect(Canvas, R, CX div 2 + Round(LScaleFactor), Fonts[ktfPopupMenuItemTextDisabled].Color);
          end;
        end;
      kmgSubmenuGlyph:
        begin
          if ADPI > DPI_DEFAULT then
          begin
            R := TRect.Create(0, 0, MulDiv(16, ADPI, DPI_DEFAULT), MulDiv(16, ADPI, DPI_DEFAULT));
            X := MulDiv(3, ADPI, DPI_DEFAULT);
          end
          else
          begin
            R := TRect.Create(0, 0, 16, 16);
            X := 3;
          end;
          RectCenter(R, Rect);
          case AItem.DrawState of
            kmidsNormal: Canvas.Pen.Color := Fonts[ktfPopupMenuItemTextNormal].Color;
            kmidsDisabled: Canvas.Pen.Color := Fonts[ktfPopupMenuItemTextDisabled].Color;
          end;
          DrawArrow(Canvas, Vcl.GraphUtil.sdRight, Point(R.Right - X * 2, R.Height div 2), X);
        end;
      kmgGlyph: if Glyph <> nil then
        begin
          R := TRect.Create(0, 0, Glyph.Width, Glyph.Height);
          RectCenter(R, Rect);
          Glyph.Draw(Canvas, R);
        end;
      kmgSysMenu: ;
      kmgClose: ;
      kmgMin: ;
      kmgMax: ;
      kmgRoll: ;
      kmgMinRestore: ;
      kmgMaxRestore: ;
      kmgRollRestore: ;
      kmgTray: ;
    end;
  end;
end;

procedure TseCustomStyle.MenuDrawItemText(const ASubclass: TSeMenuSubclass; Canvas: TCanvas;
  AItem: TSeMenuItemInfo; AText: TSeTextInfo; const AObject: TSeStyleElementObject = ktoDefault);
begin
  DrawStyleText(Canvas, AText);
end;

{ Dock class ==================================================================}

function TseCustomStyle.IsDockDefined(const ASubclass: TSeDockSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin
  {$IFDEF THEME_DEBUG}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

procedure TseCustomStyle.DockDraw(const ASubclass: TSeDockSubclass;
  Canvas: TCanvas; ADock: TSeDockInfo; ADPI: Integer = 0; const AObject: TSeStyleElementObject = ktoDefault);
begin

end;

function TseCustomStyle.DockGetClientRect(const ASubclass: TSeDockSubclass;
  ADock: TSeDockInfo; const AObject: TSeStyleElementObject): TRect;
begin
  Result := ADock.Rect;
end;

{ Tool class ==================================================================}

function TseCustomStyle.IsToolDefined(const ASubclass: TSeToolSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin
  {$IFDEF THEME_DEBUG}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function TseCustomStyle.ToolGetClientRect(const ASubclass: TSeToolSubclass;
  ATool: TSeToolInfo; const AObject: TSeStyleElementObject = ktoDefault): TRect;
begin
  Result := ATool.Rect;
  InflateRect(Result, -2, -2);
  Inc(Result.Left, 9);
end;

procedure TseCustomStyle.ToolDraw(const ASubclass: TSeToolSubclass; Canvas: TCanvas;
  ATool: TSeToolInfo; ADPI: Integer = 0; const AObject: TSeStyleElementObject = ktoDefault);
begin
end;

procedure TseCustomStyle.ToolDrawItem(const ASubclass: TSeToolSubclass; Canvas: TCanvas;
  AItem: TSeToolItemInfo; ADPI: Integer = 0; const AObject: TSeStyleElementObject = ktoDefault);
begin

end;

procedure TseCustomStyle.ToolDrawItemGlyph(const ASubclass: TSeToolSubclass;
  Canvas: TCanvas; AItem: TSeToolItemInfo; AGlyph: TSeToolGlyphInfo;
  const AObject: TSeStyleElementObject);
const
  CheckMarkPoints: array[0..11] of TPoint = (
    { Black }
    (X: -2; Y: -2), (X: 0; Y:  0), (X:  4; Y: -4),
    (X:  4; Y: -3), (X: 0; Y:  1), (X: -2; Y: -1),
    (X: -2; Y: -2),
    { White }
    (X: -3; Y: -2), (X: -3; Y: -1), (X: 0; Y: 2),
    (X:  5; Y: -3), (X:  5; Y: -5));
var
  R: TRect;
  i, X, Y: integer;
  Points: array[0..11] of TPoint;
begin
  with AGlyph do
  begin
    case Kind of
      ktigCheckGlyph:
        begin
          R := TRect.Create(0, 0, 16, 16);
          RectCenter(R, Rect);

          X := (R.Left + R.Right) div 2 - 2;
          Y := (R.Top + R.Bottom) div 2 + 1;
          System.Move(CheckMarkPoints, Points, 12 * SizeOf(TPoint));
          for i := Low(Points) to High(Points) do begin
            Inc (Points[I].X, X);
            Inc (Points[I].Y, Y);
          end;
          Canvas.Pen.Color := Canvas.Font.Color;
          Polyline(Canvas.Handle, Points[0], 7);
          Canvas.Pen.Color := Canvas.Font.Color;
          Polyline(Canvas.Handle, Points[7], 5);
        end;
      ktigRadioGlyph:
        begin
          R := TRect.Create(0, 0, 6, 6);
          RectCenter(R, Rect);
          FillRoundRect(Canvas, R, 3, clBlack);
        end;
      ktigSubmenuGlyph:
        begin
          R := TRect.Create(0, 0, 16, 16);
          RectCenter(R, Rect);

          DrawFrameControlGlyph(Canvas, R, DFC_MENU, DFCS_MENUARROW, Canvas.Font.Color);
        end;
      ktigGlyph: if Glyph <> nil then
        begin
          R := TRect.Create(0, 0, Glyph.Width, Glyph.Height);
          RectCenter(R, Rect);
          Glyph.Draw(Canvas, R);
        end;
    end;
  end;
end;

procedure TseCustomStyle.ToolDrawItemText(const ASubclass: TSeToolSubclass;
  Canvas: TCanvas; AItem: TSeToolItemInfo; AText: TSeTextInfo;
  const AObject: TSeStyleElementObject);
begin
  DrawStyleText(Canvas, AText);
end;

{ Grid class ==================================================================}

function TseCustomStyle.IsGridDefined(const ASubclass: TSeGridSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin
  {$IFDEF THEME_DEBUG}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function TseCustomStyle.IsGridTransparent(const ASubclass: TSeGridSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin
  Result := False;
end;

function TseCustomStyle.GridGetClientRect(const ASubclass: TSeGridSubclass;
  AGrid: TSeGridInfo; const AObject: TSeStyleElementObject = ktoDefault): TRect;
begin
  Result := AGrid.Rect;
  InflateRect(Result, -2, -2);
end;

procedure TseCustomStyle.GridDraw(const ASubclass: TSeGridSubclass; Canvas: TCanvas;
  AGrid: TSeGridInfo; const AObject: TSeStyleElementObject = ktoDefault);
begin
end;

procedure TseCustomStyle.GridDrawItem(const ASubclass: TSeGridSubclass; Canvas: TCanvas;
  AItem: TSeGridItemInfo; const AObject: TSeStyleElementObject = ktoDefault);
begin
end;

procedure TseCustomStyle.GridDrawGlyph(const ASubclass: TSeGridSubclass;
  Canvas: TCanvas; AItem: TSeGridItemInfo; AGlyph: TSeGlyphInfo;
  const AObject: TSeStyleElementObject = ktoDefault);
begin

end;

procedure TseCustomStyle.GridDrawText(const ASubclass: TSeGridSubclass;
  Canvas: TCanvas; AItem: TSeGridItemInfo; AText: TSeTextInfo;
  const AObject: TSeStyleElementObject = ktoDefault);
begin
  DrawStyleText(Canvas, AText);
end;

{ Tree class ==================================================================}

function TseCustomStyle.IsTreeDefined(const ASubclass: TSeTreeSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin
  {$IFDEF THEME_DEBUG}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function TseCustomStyle.IsTreeTransparent(const ASubclass: TSeTreeSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin
  Result := False;
end;

procedure TseCustomStyle.TreeDraw(const ASubclass: TSeTreeSubclass;
  Canvas: TCanvas; ATree: TSeTreeInfo; const AObject: TSeStyleElementObject = ktoDefault);
begin
end;

procedure TseCustomStyle.TreeDrawItem(const ASubclass: TSeTreeSubclass;
  Canvas: TCanvas; AItem: TSeTreeItemInfo; const AObject: TSeStyleElementObject = ktoDefault);
begin
end;

procedure TseCustomStyle.TreeDrawGlyph(const ASubclass: TSeTreeSubclass;
  Canvas: TCanvas; AItem: TSeTreeItemInfo; AGlyph: TSeGlyphInfo;
  const AObject: TSeStyleElementObject = ktoDefault);
begin

end;

procedure TseCustomStyle.TreeDrawText(const ASubclass: TSeTreeSubclass;
  Canvas: TCanvas; AItem: TSeTreeItemInfo; AText: TSeTextInfo;
  const AObject: TSeStyleElementObject = ktoDefault);
begin
  DrawStyleText(Canvas, AText);
end;

function TseCustomStyle.TreeGetClientRect(const ASubclass: TSeTreeSubclass;
  ATree: TSeTreeInfo; const AObject: TSeStyleElementObject = ktoDefault): TRect;
begin
  Result := ATree.Rect;
  InflateRect(Result, -2, -2);
end;

{ Memo class ==================================================================}

function TseCustomStyle.IsMemoDefined(const ASubclass: TSeMemoSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin
  {$IFDEF THEME_DEBUG}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function TseCustomStyle.IsMemoTransparent(const ASubclass: TSeMemoSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin
  Result := False;
end;

function TseCustomStyle.MemoGetClientRect(const ASubclass: TSeMemoSubclass;
  Canvas: TCanvas; AMemo: TSeMemoInfo;
  const AObject: TSeStyleElementObject): TRect;
begin
  Result := AMemo.Rect;
end;

procedure TseCustomStyle.MemoDraw(const ASubclass: TSeMemoSubclass;
  Canvas: TCanvas; AMemo: TSeMemoInfo; const AObject: TSeStyleElementObject);
begin
end;

{ Box class ===================================================================}

function TseCustomStyle.IsBoxDefined(const ASubclass: TSeBoxSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin
  {$IFDEF THEME_DEBUG}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function TseCustomStyle.BoxGetClientRect(const ASubclass: TSeBoxSubclass;
  Canvas: TCanvas; ABox: TSeBoxInfo;
  const AObject: TSeStyleElementObject): TRect;
begin
  Result := ABox.Rect;
end;

procedure TseCustomStyle.BoxDraw(const ASubclass: TSeBoxSubclass;
  Canvas: TCanvas; ABox: TSeBoxInfo; const AObject: TSeStyleElementObject);
begin
end;

function TseCustomStyle.IsBoxTransparent(const ASubclass: TSeBoxSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin
  Result := False;
end;

{ TSeCustomStyleLink ==========================================================}

function TseCustomStyle.GetBounds(AObject: TSeStyleElementObject): TRect;
begin
  Result := NullRect;
end;

function TseCustomStyle.CustomColor(const AColorName: string): TColor;
begin
  Result := TColor(clNone);
end;

function TseCustomStyle.TrackGetThumbSize(const ASubclass: TSeTrackSubclass;
  Canvas: TCanvas; AThumb: TSeTrackThumbInfo; ADPI: Integer = 0;
  const AObject: TSeStyleElementObject = ktoDefault): TPoint;
begin
  Result := Point(0, 0);
end;



{ BackgrounInfo }

function BackgroundInfo(const ARect: TRect): TSeBackgroundInfo;
begin
  Result.Rect := ARect;
  Result.ClipRect := ARect;
  Result.Control := nil;
end;

function BackgroundInfo(AControl: TControl): TSeBackgroundInfo;
begin
  if AControl <> nil then
    Result.Rect := AControl.ClientRect
  else
    Result.Rect := NullRect;
  Result.ClipRect := Result.Rect;
  Result.Control := AControl;
end;

function BackgroundInfo(const ARect: TRect; AControl: TControl;
  const AClipRect: TRect): TSeBackgroundInfo;
begin
  Result.Rect := ARect;
  Result.ClipRect := AClipRect;
  Result.Control := AControl;
end;

{ TextInfo }

function TextInfo(const ARect: TRect; const AText: string; AAlign: TSeTextAlign2;
  AStyle: TSeTextStyles;
  AOrientation: TSeTextOrientation = ktxoHorizontal): TSeTextInfo;
begin
  Result.Rect := ARect;
  Result.Text := AText;
  Result.Align := AAlign;
  Result.Style := AStyle;
  Result.Orientation := AOrientation;
end;

function TextInfo(const ARect: TRect; const AText: string;
  DT_Flags: cardinal): TSeTextInfo;
begin
  Result.Rect := ARect;
  Result.Text := AText;
  { Extract DT_flags }
  if DT_VCENTER and DT_Flags = DT_VCENTER then
  begin
    if DT_CENTER and DT_Flags = DT_CENTER then
      Result.Align := ktxaMiddleCenter
    else
      if DT_RIGHT and DT_Flags = DT_RIGHT then
        Result.Align := ktxaMiddleRight
      else
        Result.Align := ktxaMiddleLeft;
  end
  else
    if DT_BOTTOM and DT_Flags = DT_BOTTOM then
    begin
      if DT_CENTER and DT_Flags = DT_CENTER then
        Result.Align := ktxaBottomCenter
      else
        if DT_RIGHT and DT_Flags = DT_RIGHT then
          Result.Align := ktxaBottomRight
        else
          Result.Align := ktxaBottomLeft;
    end
    else
    begin
      if DT_CENTER and DT_Flags = DT_CENTER then
        Result.Align := ktxaTopCenter
      else
        if DT_RIGHT and DT_Flags = DT_RIGHT then
          Result.Align := ktxaTopRight
        else
          Result.Align := ktxaTopLeft;
    end;

  Result.Style := [];
  if DT_RTLREADING and DT_Flags = DT_RTLREADING then
    Result.Style := Result.Style + [ktxsRTLReading];
  if DT_NOPREFIX and DT_Flags = DT_NOPREFIX then
    Result.Style := Result.Style + [ktxsNoPrefix];
  if DT_SINGLELINE and DT_Flags = DT_SINGLELINE then
    Result.Style := Result.Style + [ktxsSingleLine];
  if DT_WORDBREAK and DT_Flags = DT_WORDBREAK then
    Result.Style := Result.Style + [ktxsWordBreak];
  if DT_END_ELLIPSIS and DT_Flags = DT_END_ELLIPSIS then
    Result.Style := Result.Style + [ktxsEndEllipsis];
  if DT_EXPANDTABS and DT_Flags = DT_EXPANDTABS then
    Result.Style := Result.Style + [ktxsExpandTabs];
  if DT_WORD_ELLIPSIS and DT_Flags = DT_WORD_ELLIPSIS then
    Result.Style := Result.Style + [ktxsWordEllipsis];

  Result.Orientation := ktxoHorizontal;
end;

{ GlyphInfo }

function GlyphInfo: TSeGlyphInfo;
begin
  Result.Rect := NullRect;
  Result.Glyph := nil;
end;

function GlyphInfo(const ARect: TRect; AGlyph: TseBitmap; AAlign: TSeGlyphAlign = kgfaCenter): TSeGlyphInfo;
begin
  Result.Rect := ARect;
  Result.Glyph := AGlyph;
  Result.Align := AAlign;
end;

function GlyphInfo(const ARect: TRect; Graphic: TGraphic; AAlign: TSeGlyphAlign = kgfaCenter): TSeGlyphInfo; overload;
begin
  Result.Rect := ARect;
  Result.Glyph := GraphicToBitmap(Graphic);
  Result.Align := AAlign;
end;

function GlyphInfo(const ARect: TRect; Images: TCustomImageList; ImageIndex: integer; AAlign: TSeGlyphAlign = kgfaCenter): TSeGlyphInfo; overload;
begin
  Result.Rect := ARect;
  Result.Glyph := ImagesToBitmap(Images, ImageIndex);
  Result.Align := AAlign;
end;

{ FormInfo }

function WindowInfo(const ARect: TRect; AState: TSeWindowDrawStates; const AClipRect: TRect; AButtons: TSeWindowButtons = [];
  const ATitle: string = ''; ADisableNCArea: boolean = False): TSeWindowInfo;
begin
  Result.Rect := ARect;
  Result.ClipRect := AClipRect;
  Result.State := AState;
  Result.Buttons := AButtons;
  Result.Title := ATitle;
  Result.DisableNCArea := ADisableNCArea;
end;

function WindowButtonInfo(const ARect: TRect; AButton: TSeWindowButtonClass;
  ADrawState: TSeWindowButtonDrawState; AIcon: TseBitmap = nil): TSeWindowButtonInfo; overload;
begin
  Result.Rect := ARect;
  Result.Button := AButton;
  Result.DrawState := ADrawState;
  Result.Icon := AIcon;
end;

function WindowGripperInfo(const ARect: TRect; ASizeable: boolean): TSeWindowGripperInfo;
begin
  Result.Rect := ARect;
  Result.Sizeable := ASizeable;
end;

{ HintInfo }

function HintInfo(const ARect: TRect): TSeHintInfo;
begin
  Result.Rect := ARect;
end;

{ LabelInfo }

function LabelInfo(ADrawState: TSeLabelDrawState): TSeLabelInfo;
begin
  Result.DrawState := ADrawState;
end;

{ ButtonInfo }

function ButtonInfo(const ARect: TRect; ADrawState: TSeButtonDrawState): TSeButtonInfo;
begin
  Result.Rect := ARect;
  Result.DrawState := ADrawState;
end;

{ CheckInfo }

function CheckInfo(const ARect: TRect; ADrawState: TSeCheckDrawState): TSeCheckInfo;
begin
  Result.Rect := ARect;
  Result.DrawState := ADrawState;
end;

{ EditInfo }

function EditInfo(const ARect: TRect; ADrawState: TSeEditDrawState): TSeEditInfo;
begin
  Result.Rect := ARect;
  Result.DrawState := ADrawState;
end;

function EditButtonInfo(const ARect: TRect; ADrawState: TSeEditButtonDrawState;
  AButtonClass: TSeEditButtonClass): TSeEditButtonInfo;
begin
  Result.Rect := ARect;
  Result.DrawState := ADrawState;
  Result.ButtonClass := AButtonClass;
end;

{ ScrolkInfo }

function ScrollInfo(const ARect: TRect): TSeScrollInfo;
begin
  Result.Rect := ARect;
end;

function ScrollButtonInfo(const ARect: TRect; ADrawState: TSeScrollButtonDrawState;
  AButton: TSeScrollButtonClass): TSeScrollButtonInfo;
begin
  Result.Rect := ARect;
  Result.DrawState := ADrawState;
  Result.Button := AButton;
end;

function ScrollAreaInfo(const ARect: TRect; ADrawState: TSeScrollAreaDrawState;
  AArea: TSeScrollAreaClass): TSeScrollAreaInfo;
begin
  Result.Rect := ARect;
  Result.DrawState := ADrawState;
  Result.Area := AArea;
end;

{ ProgressInfo }

function ProgressInfo(const ARect: TRect; APart: TSeProgressPart;
  ADrawState: TSeProgressDrawState): TSeProgressInfo;
begin
  Result.Rect := ARect;
  Result.Part := APart;
  Result.DrawState := ADrawState;
end;

{ PanelInfo }

function PanelInfo(const ARect: TRect): TSePanelInfo;
begin
  Result.Rect := ARect;
end;

function PanelButtonInfo(const ARect: TRect; AButton: TSePanelButtonClass;
  ADrawState: TSePanelButtonDrawState): TSePanelButtonInfo;
begin
  Result.Rect := ARect;
  Result.Button := AButton;
  Result.DrawState := ADrawState;
end;

{ TrackInfo }

function TrackInfo(const ARect: TRect): TSeTrackInfo;
begin
  Result.Rect := ARect;
end;

function TrackBarInfo(const ARect: TRect; ABar: TSeTrackBarClass;
  ADrawState: TSeTrackBarDrawState): TSeTrackBarInfo;
begin
  Result.Rect := ARect;
  Result.Bar := ABar;
  Result.DrawState := ADrawState;
end;

function TrackThumbInfo(const ARect: TRect; AThumb: TSeTrackThumbClass;
  ADrawState: TSeTrackThumbDrawState): TSeTrackThumbInfo;
begin
  Result.Rect := ARect;
  Result.Thumb := AThumb;
  Result.DrawState := ADrawState;
end;

{ Splitter }

function SplitterInfo(const ARect: TRect): TSeSplitterInfo;
begin
  Result.Rect := ARect;
end;

function SplitterButtonInfo(const ARect: TRect; AButtonClass: TSeSplitterButtonClass): TSeSplitterButtonInfo;
begin
  Result.Rect := ARect;
  Result.ButtonClass := AButtonClass;
end;

{ GroupInfo }

function GroupInfo(ARect, AEraseRect: TRect; ADrawState: TSeGroupDrawState): TSeGroupInfo;
begin
  Result.Rect := ARect;
  Result.EraseRect := AEraseRect;
  Result.DrawState := ADrawState;
end;

{ MemoInfo }

function MemoInfo(const ARect: TRect; ADrawState: TSeMemoDrawState): TSeMemoInfo;
begin
  Result.Rect := ARect;
  Result.DrawState := ADrawState;
end;

function MemoGutterInfo(const ARect: TRect): TSeMemoGutterInfo;
begin
  Result.Rect := ARect;
end;

{ Box }

function BoxInfo(const ARect: TRect; ADrawState: TSeBoxDrawState): TSeBoxInfo;
begin
  Result.Rect := ARect;
  Result.DrawState := ADrawState;
end;

{ TabInfo }

function TabInfo(const ARect: TRect): TSeTabInfo;
begin
  Result.Rect := ARect;
end;

function TabItemInfo(const ARect: TRect; AItem: TSeTabItemClass;
  ADrawState: TSeTabItemDrawState): TSeTabItemInfo;
begin
  Result.Rect := ARect;
  Result.Item := AItem;
  Result.DrawState := ADrawState;
end;

function TabButtonInfo(const ARect: TRect; AButton: TSeTabButtonClass;
  ADrawState: TSeTabButtonDrawState): TSeTabButtonInfo;
begin
  Result.Rect := ARect;
  Result.DrawState := ADrawState;
  Result.Button := AButton;
end;

{ HeaderInfo }

function HeaderInfo(const ARect: TRect): TSeHeaderInfo;
begin
  Result.Rect := ARect;
end;

function HeaderSectionInfo(const ARect: TRect; AState: TSeSectionState;
  ADrawState: TSeSectionDrawState): TSeHeaderSectionInfo;
begin
  Result.Rect := ARect;
  Result.DrawState := ADrawState;
  Result.State := AState;
end;

{ StatusInfo }

function StatusInfo(const ARect: TRect): TSeStatusInfo;
begin
  Result.Rect := ARect;
end;

function StatusPanelInfo(const ARect: TRect; APanel: TSeStatusPanelClass): TSeStatusPanelInfo;
begin
  Result.Rect := ARect;
  Result.Panel := APanel;
end;

{ BarDockInfo }

function BarDockInfo(const ARect: TRect): TSeBarDockInfo;
begin
  Result.Rect := ARect;
end;

{ BarInfo }

function BarInfo(const ARect: TRect; HaveBorder, HaveCaption: boolean): TSeBarInfo;
begin
  Result.Rect := ARect;
  Result.HaveBorder := HaveBorder;
  Result.HaveCaption := HaveCaption;
end;

function BarCaptionInfo(const ARect: TRect; const AText: string): TSeBarCaptionInfo;
begin
  Result.Rect := ARect;
end;

function BarButtonInfo(const ARect: TRect; AButton: TSeBarButtonClass;
  ADrawState: TSeBarButtonDrawState): TSeBarButtonInfo;
begin
  Result.Rect := ARect;
  Result.Button := AButton;
  Result.DrawState := ADrawState;
end;

function BarItemInfo(const ARect: TRect; AItem: TSeBarItemClass;
  ADrawState: TSeBarItemDrawState): TSeBarItemInfo;
begin
  Result.Rect := ARect;
  Result.Item := AItem;
  Result.DrawState := ADrawState;
end;

{ MenuInfo }

function MenuInfo(const ARect: TRect): TSeMenuInfo;
begin
  Result.Rect := ARect;
end;

function MenuItemInfo(const ARect: TRect; AItem: TSeMenuItemClass;
  ADrawState: TSeMenuItemDrawState): TSeMenuItemInfo;
begin
  Result.Rect := ARect;
  Result.Item := AItem;
  Result.DrawState := ADrawState;
end;

function MenuGlyphInfo: TSeMenuGlyphInfo;
begin
  Result.Rect := NullRect;
  Result.Glyph := nil;
  Result.Kind := kmgNone;
end;

function MenuGlyphInfo(const ARect: TRect; AGlyph: TseBitmap;
  AAlign: TSeGlyphAlign; AKind: TSeMenuGlyph): TSeMenuGlyphInfo;
begin
  Result.Rect := ARect;
  Result.Glyph := AGlyph;
  Result.Align := AAlign;
  Result.Kind := AKind;
end;

function MenuGlyphInfo(const ARect: TRect; AKind: TSeMenuGlyph): TSeMenuGlyphInfo;
begin
  Result.Rect := ARect;
  Result.Glyph := nil;
  Result.Align := kgfaCenter;
  Result.Kind := AKind;
end;

function MenuGlyphInfo(const ARect: TRect; Graphic: TGraphic; AAlign:
  TSeGlyphAlign = kgfaCenter): TSeMenuGlyphInfo;
begin
  Result.Rect := ARect;
  Result.Glyph := GraphicToBitmap(Graphic);
  Result.Align := AAlign;
  Result.Kind := kmgGlyph;
end;

function MenuGlyphInfo(const ARect: TRect; Images: TCustomImageList; ImageIndex: integer;
  AAlign: TSeGlyphAlign = kgfaCenter; AEnabled: boolean = True): TSeMenuGlyphInfo;
begin
  Result.Rect := ARect;
  Result.Glyph := ImagesToBitmap(Images, ImageIndex, AEnabled);
  Result.Align := AAlign;
  Result.Kind := kmgGlyph;
end;

{ DockInfo }

function DockInfo(const ARect: TRect): TSeDockInfo;
begin
  Result.Rect := ARect;
end;

{ ToolInfo }

function ToolInfo(const ARect: TRect): TSeToolInfo;
begin
  Result.Rect := ARect;
end;

function ToolItemInfo(const ARect: TRect; AItem: TSeToolItemClass;
  ADrawState: TSeToolItemDrawState): TSeToolItemInfo;
begin
  Result.Rect := ARect;
  Result.Item := AItem;
  Result.DrawState := ADrawState;
end;

function ToolGlyphInfo: TSeToolGlyphInfo;
begin
  Result.Rect := NullRect;
  Result.Glyph := nil;
  Result.Kind := ktgNone;
end;

function ToolGlyphInfo(const ARect: TRect; AGlyph: TseBitmap;
  AAlign: TSeGlyphAlign; AKind: TSeToolGlyph): TSeToolGlyphInfo;
begin
  Result.Rect := ARect;
  Result.Glyph := AGlyph;
  Result.Align := AAlign;
  Result.Kind := AKind;
end;

function ToolGlyphInfo(const ARect: TRect; AKind: TSeToolGlyph): TSeToolGlyphInfo;
begin
  Result.Rect := ARect;
  Result.Glyph := nil;
  Result.Align := kgfaCenter;
  Result.Kind := AKind;
end;

function ToolGlyphInfo(const ARect: TRect; Graphic: TGraphic; AAlign:
  TSeGlyphAlign = kgfaCenter): TSeToolGlyphInfo;
begin
  Result.Rect := ARect;
  Result.Glyph := GraphicToBitmap(Graphic);
  Result.Align := AAlign;
  Result.Kind := ktigGlyph;
end;

function ToolGlyphInfo(const ARect: TRect; Images: TCustomImageList; ImageIndex: integer;
  AAlign: TSeGlyphAlign = kgfaCenter; AEnabled: boolean = True): TSeToolGlyphInfo;
begin
  Result.Rect := ARect;
  Result.Glyph := ImagesToBitmap(Images, ImageIndex, AEnabled);
  Result.Align := AAlign;
  Result.Kind := ktigGlyph;
end;

{ ListInfo }

function ListInfo(const ARect: TRect): TSeListInfo;
begin
  Result.Rect := ARect;
end;

function ListColumnInfo(const ARect: TRect): TSeListColumnInfo;
begin
  Result.Rect := ARect;
end;

function ListItemInfo(const ARect: TRect; ADrawState: TSeListItemDrawState): TSeListItemInfo;
begin
  Result.Rect := ARect;
  Result.DrawState := ADrawState;
end;

function ListGlyphInfo(const ARect: TRect; AGlyph: TseBitmap;
  AAlign: TSeGlyphAlign; AKind: TSeListGlyph): TSeListGlyphInfo;
begin
  Result.Rect := ARect;
  Result.Glyph := AGlyph;
  Result.Align := AAlign;
  Result.Kind := AKind;
end;

function ListGlyphInfo(const ARect: TRect; AKind: TSeListGlyph): TSeListGlyphInfo;
begin
  Result.Rect := ARect;
  Result.Glyph := nil;
  Result.Align := kgfaCenter;
  Result.Kind := AKind;
end;

function ListGlyphInfo(const ARect: TRect; Graphic: TGraphic; AAlign:
  TSeGlyphAlign = kgfaCenter): TSeListGlyphInfo;
begin
  Result.Rect := ARect;
  Result.Glyph := GraphicToBitmap(Graphic);
  Result.Align := kgfaCenter;
  Result.Kind := klgCustom;
end;

function ListGlyphInfo(const ARect: TRect; Images: TCustomImageList; ImageIndex: integer;
  AAlign: TSeGlyphAlign = kgfaCenter): TSeListGlyphInfo;
begin
  Result.Rect := ARect;
  Result.Glyph := ImagesToBitmap(Images, ImageIndex);
  Result.Align := kgfaCenter;
  Result.Kind := klgCustom;
end;

{ TreeInfo }

function TreeInfo(const ARect: TRect): TSeTreeInfo;
begin
  Result.Rect := ARect;
end;

function TreeItemInfo(const ARect: TRect; ADrawState: TSeTreeItemDrawState): TSeTreeItemInfo;
begin
  Result.Rect := ARect;
  Result.DrawState := ADrawState;
end;

{ GridInfo }

function GridInfo(const ARect: TRect): TSeGridInfo;
begin
  Result.Rect := ARect;
end;

function GridItemInfo(const ARect: TRect; ADrawState: TSeGridItemDrawState;
  AItem: TSeGridItemClass): TSeGridItemInfo;
begin
  Result.Rect := ARect;
  Result.DrawState := ADrawState;
  Result.Item := AItem;
end;

{ Convertion ==================================================================}

function WindowButtonToHitTest(WindowButton: TSeWindowButtonClass): TSeWindowHitTest;
begin
  case WindowButton of
    kwbClose: Result := kwhtCloseButton;
    kwbHelp: Result := kwhtHelpButton;
    kwbMin: Result := kwhtMinButton;
    kwbMax: Result := kwhtMaxButton;
    kwbRoll: Result := kwhtRollButton;
    kwbMinRestore: Result := kwhtMinButton;
    kwbMaxRestore: Result := kwhtMaxButton;
    kwbRollRestore: Result := kwhtRollButton;
    kwbTray: Result := kwhtTrayButton;
    kwbSysMenu: Result := kwhtSysMenu;
  else
    Result := kwhtClient;
  end;
end;

var
  ConvertBitmap: TseBitmap;

function GraphicToBitmap(Graphic: TGraphic): TseBitmap;
begin
  if Assigned(ConvertBitmap) then FreeAndNil(ConvertBitmap);

  ConvertBitmap := TseBitmap.Create;
  ConvertBitmap.Assign(Graphic);

  Result := ConvertBitmap;
end;

function ImagesToBitmap(Images: TCustomImageList; ImageIndex: integer; Enabled: boolean = True): TseBitmap;
var
  Canvas: TCanvas;
begin
  if Assigned(ConvertBitmap) then FreeAndNil(ConvertBitmap);

  ConvertBitmap := TseBitmap.Create;
  ConvertBitmap.SetSize(Images.Width, Images.Height);

  if (Images.Masked) or (Images.DrawingStyle = dsTransparent) then
  begin
    ConvertBitmap.Transparent := True;
  end;

  Canvas := TCanvas.Create;
  try
    Canvas.Handle := ConvertBitmap.Canvas.Handle;
    Images.Draw(Canvas, 0, 0, ImageIndex, Enabled);
  finally
    Canvas.Handle := 0;
    Canvas.Free;
  end;

  Result := ConvertBitmap;
end;

{ TSeStyle class ===============================================================}

constructor TSeStyle.Create;
begin
  inherited Create;
  FMetrics.FStyle := Self;
  FCleanCopy := TSeStyleSource.Create(nil);
  FStyleSource := TSeStyleSource.Create(nil);
  SetLength(FObjects, soLast);
end;

destructor TSeStyle.Destroy;
begin
  FreeObjects;
  FStyleSource.Free;
  FCleanCopy.Free;
  inherited Destroy;
end;

{ Options }

class function TSeStyle.GetStyleConfig: TSeStyleConfig;
begin
  Result.Building := False;
  Result.CanStore := True;
  Result.CanLoad := True;
  Result.DialogFilter := GetDialogFilter3;
  Result.DefaultExt := TStyleEngine.FileExtension;
end;

{}

procedure TSeStyle.Assign(Source: TPersistent);
begin
  if Source is TSeStyle then
  begin
    inherited ;

    FreeObjects;
    FCleanCopy.Assign(TSeStyle(Source).FCleanCopy);
    ResetStyle;
  end
  else
    inherited;
end;

{ I/O }

class function TSeStyle.CheckStream(Stream: TStream): boolean;
var
  Sign: array [0..12] of ansichar;
begin
  Result := False;

  Stream.Read(Sign, SizeOf(Sign));
  if (Sign = StyleFileSign) or (Sign = StyleFileSign_1_0) then Result := True;
end;

function TSeStyle.LoadFromStream(Stream: TStream): boolean;
begin
  FreeObjects;
  FCleanCopy.LoadFromStream(Stream);
  Result := FCleanCopy.Count > 0;
  if Result then ResetStyle;
end;

function TSeStyle.SaveToStream(Stream: TStream): boolean;
begin
  FCleanCopy.Fonts.Assign(Fonts);
  FCleanCopy.Colors.Assign(Colors);
  FCleanCopy.SaveToStream(Stream);
  Result := True;
end;

function TSeStyle.LoadFromFile(const FileName: string): boolean;
begin
  FreeObjects;
  FCleanCopy.LoadFromFile(FileName);
  Result := FCleanCopy.Count > 0;
  if Result then
  begin
    StyleFileName := FileName;
    ResetStyle;
  end;
end;

function TSeStyle.SaveToFile(const FileName: string): boolean;
begin
  FCleanCopy.Fonts.Assign(Fonts);
  FCleanCopy.Colors.Assign(Colors);
  FCleanCopy.SaveToFile(FileName);
  Result := True;
end;

{ Class methods }

class function TSeStyle.GetName: string;
begin
  Result := 'VCL Style';
end;

{ Style  }

procedure TSeStyle.ResetStyle;
begin
  if FCleanCopy <> nil then
  begin
    FStyleSource.Assign(FCleanCopy);
    ResetObjects;
    Name := FCleanCopy.Name;
  end;
  inherited;
end;

procedure TSeStyle.ResetObjects;
var
  SOwner, StyleObject: TSeStyleObject;
begin
  { Clear }
  FreeObjects;

  if FStyleSource = nil then Exit;
  if FStyleSource.Count = 0 then Exit;

  { Set Objects array }
  FObjects[soForm] := FStyleSource.GetObjectByName('Form');
  if FObjects[soForm] <> nil then
    StyleObject := FObjects[soForm].FindObjectByKind(skClient)
  else
    StyleObject := nil;
  if StyleObject <> nil then
  begin
    FObjects[soClient] := StyleObject.CreateCopy(nil);
    SOwner := TSeStyleObject(StyleObject.Owner);
    TSeStyleObject(StyleObject.Owner).Remove(StyleObject);
    FreeAndNil(StyleObject);

    FObjects[soFormClientRect] := TSeStyleObject.Create(SOwner);
    FObjects[soFormClientRect].Color := TColor(clNone);
    FObjects[soFormClientRect].Align := FObjects[soClient].Align;
    FObjects[soFormClientRect].Kind := skClient;

    FObjects[soFormSysButton] := FObjects[soForm].CreateCopy(nil);
    FObjects[soFormSysButtonDraw] := FObjects[soForm].CreateCopy(nil);

    StyleObject := FObjects[soForm].FindObjectByAction(sbaSysMenu);
    if StyleObject <> nil then StyleObject.Visible := False;
    StyleObject := FObjects[soForm].FindObjectByAction(sbaClose);
    if StyleObject <> nil then StyleObject.Visible := False;
    StyleObject := FObjects[soForm].FindObjectByAction(sbaHelp);
    if StyleObject <> nil then StyleObject.Visible := False;
    StyleObject := FObjects[soForm].FindObjectByAction(sbaMinimize);
    if StyleObject <> nil then StyleObject.Visible := False;
    StyleObject := FObjects[soForm].FindObjectByAction(sbaMaximize);
    if StyleObject <> nil then StyleObject.Visible := False;
    StyleObject := FObjects[soForm].FindObjectByAction(sbaRestore);
    if StyleObject <> nil then StyleObject.Visible := False;
    StyleObject := FObjects[soForm].FindObjectByAction(sbaRollup);
    if StyleObject <> nil then StyleObject.Visible := False;
    StyleObject := FObjects[soForm].FindObjectByAction(sbaRolldown);
    if StyleObject <> nil then StyleObject.Visible := False;
    StyleObject := FObjects[soForm].FindObjectByAction(sbaTray);
    if StyleObject <> nil then StyleObject.Visible := False;
  end;

  { Set Objects array }
  FObjects[soSmall] := FStyleSource.GetObjectByName('ToolWindow');
  if FObjects[soSmall] <> nil then
    StyleObject := FObjects[soSmall].FindObjectByKind(skClient)
  else
    StyleObject := nil;
  if StyleObject <> nil then
  begin
    FObjects[soSmallClient] := StyleObject.CreateCopy(nil);
    SOwner := TSeStyleObject(StyleObject.Owner);
    TSeStyleObject(StyleObject.Owner).Remove(StyleObject);
    FreeAndNil(StyleObject);

    FObjects[soSmallClientRect] := TSeStyleObject.Create(SOwner);
    FObjects[soSmallClientRect].Color := TColor(clNone);
    FObjects[soSmallClientRect].Align := FObjects[soSmallClient].Align;
    FObjects[soSmallClientRect].Kind := skClient;

    FObjects[soSmallSysButton] := FObjects[soSmall].CreateCopy(nil);
    FObjects[soSmallSysButtonDraw] := FObjects[soSmall].CreateCopy(nil);

    StyleObject := FObjects[soSmall].FindObjectByAction(sbaSysMenu);
    if StyleObject <> nil then StyleObject.Visible := False;
    StyleObject := FObjects[soSmall].FindObjectByAction(sbaClose);
    if StyleObject <> nil then StyleObject.Visible := False;
    StyleObject := FObjects[soSmall].FindObjectByAction(sbaHelp);
    if StyleObject <> nil then StyleObject.Visible := False;
    StyleObject := FObjects[soSmall].FindObjectByAction(sbaMinimize);
    if StyleObject <> nil then StyleObject.Visible := False;
    StyleObject := FObjects[soSmall].FindObjectByAction(sbaMaximize);
    if StyleObject <> nil then StyleObject.Visible := False;
    StyleObject := FObjects[soSmall].FindObjectByAction(sbaRestore);
    if StyleObject <> nil then StyleObject.Visible := False;
    StyleObject := FObjects[soSmall].FindObjectByAction(sbaRollup);
    if StyleObject <> nil then StyleObject.Visible := False;
    StyleObject := FObjects[soSmall].FindObjectByAction(sbaRolldown);
    if StyleObject <> nil then StyleObject.Visible := False;
    StyleObject := FObjects[soSmall].FindObjectByAction(sbaTray);
    if StyleObject <> nil then StyleObject.Visible := False;
  end;

  FObjects[soButton] := FStyleSource.GetObjectByName('Button');
  FObjects[soCheckbox] := FStyleSource.GetObjectByName('Checkbox');
  FObjects[soRadioButton] := FStyleSource.GetObjectByName('RadioButton');
  FObjects[soGroupBox] := FStyleSource.GetObjectByName('GroupBox');
  FObjects[soScrollBar] := FStyleSource.GetObjectByName('ScrollBar');
  FObjects[soProgressBar] := FStyleSource.GetObjectByName('ProgressBar');
  FObjects[soEditBox] := FStyleSource.GetObjectByName('Edit');
  FObjects[soComboBox] := FStyleSource.GetObjectByName('ComboBox');
  FObjects[soTrackBar] := FStyleSource.GetObjectByName('TrackBar');
  FObjects[soMenuBar] := FStyleSource.GetObjectByName('MenuBar');
  FObjects[soPopupMenu] := FStyleSource.GetObjectByName('PopupMenu');
  FObjects[soHeader] := FStyleSource.GetObjectByName('Header');
  FObjects[soStatus] := FStyleSource.GetObjectByName('StatusBar');
  FObjects[soTab] := FStyleSource.GetObjectByName('Tabs');
  FObjects[soPanel] := FStyleSource.GetObjectByName('Container');
  FObjects[soFrame] := FStyleSource.GetObjectByName('Frame');
  FObjects[soScrollBox] := FStyleSource.GetObjectByName('ScrollBox');
  FObjects[soSplitter] := FStyleSource.GetObjectByName('Splitter');
  FObjects[soSpinButton] := FStyleSource.GetObjectByName('SpinButton');
  FObjects[soControlBar] := FStyleSource.GetObjectByName('ControlBar');
  FObjects[soGrid] := FStyleSource.GetObjectByName('Grid');
  FObjects[soListBox] := FStyleSource.GetObjectByName('ListBox');
  FObjects[soHint] := FStyleSource.GetObjectByName('Hint');
  FObjects[soSpeedButton] := FStyleSource.GetObjectByName('SpeedButton');
  FObjects[soToolBar] := FStyleSource.GetObjectByName('ToolBar');
  FObjects[soHeaderPanel] := FStyleSource.GetObjectByName('Panel');
  FObjects[soBarDock] := FStyleSource.GetObjectByName('ScrollBox');
end;

procedure TSeStyle.FreeObjects;
var
  i: integer;
begin
  if FObjects[soClient] <> nil then FreeAndNil(FObjects[soClient]);
  if FObjects[soFormSysButton] <> nil then FreeAndNil(FObjects[soFormSysButton]);
  if FObjects[soFormSysButtonDraw] <> nil then FreeAndNil(FObjects[soFormSysButtonDraw]);
  if FObjects[soSmallClient] <> nil then FreeAndNil(FObjects[soSmallClient]);
  if FObjects[soSmallSysButton] <> nil then FreeAndNil(FObjects[soSmallSysButton]);
  if FObjects[soSmallSysButtonDraw] <> nil then FreeAndNil(FObjects[soSmallSysButtonDraw]);

  for i := Low(FObjects) to High(FObjects) do
    FObjects[i] := nil;
end;

procedure TSeStyle.ResetStyleColors;
begin
  if FStyleSource <> nil then
  begin
    Colors.Assign(FStyleSource.Colors);
    SysColors.Assign(FStyleSource.SysColors);
  end;
end;

procedure TSeStyle.ResetStyleFonts;
begin
  if FStyleSource <> nil then
    Fonts.Assign(FStyleSource.Fonts);
end;

{ Window class ================================================================}

function TSeStyle.IsWindowDefined(const ASubclass: TSeWindowSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin


  Result := False;
  if AObject <> ktoDefault then
    Result := FStyleSource.GetObjectByName(AObject) <> nil;

  if not Result then
    case ASubclass of
      kwscStandard, kwscDialog: Result := (FObjects[soForm] <> nil);
      kwscToolWindow: Result := (FObjects[soSmall] <> nil);
    else
      Result := False;
    end;
end;

function TSeStyle.WindowGetRegion(const ASubclass: TSeWindowSubclass;
  const ARect: TRect; const AObject: TSeStyleElementObject): TSeRegion;
var
  StyleObject: TSeStyleObject;
begin
  StyleObject := nil;
  if AObject <> ktoDefault then
    StyleObject := FStyleSource.GetObjectByName(AObject);

  if StyleObject = nil then
    case ASubclass of
      kwscStandard: StyleObject := FObjects[soForm];
      kwscDialog: StyleObject := FObjects[soForm];
      kwscBorder: StyleObject := FObjects[soForm];
      kwscNoBorder: StyleObject := FObjects[soForm];
      kwscToolWindow: StyleObject := FObjects[soSmall];
    else
      StyleObject := nil;
    end;

  if StyleObject <> nil then
  begin
    StyleObject.BoundsRect := ARect;
    Result := StyleObject.GetRegion;
  end
  else
    Result := 0;
end;

function TSeStyle.WindowGetFixPosition(const ASubclass: TSeWindowSubclass;
  AButton: TSeWindowButtonClass; ADPI: Integer = 0;
    const AObject: TSeStyleElementObject = ktoDefault): Boolean;
var
  StyleObject: TSeStyleObject;
begin
  Result := False;
  StyleObject := nil;
  if AObject <> ktoDefault then
    StyleObject := FStyleSource.GetObjectByName(AObject);

  if StyleObject = nil then
    case ASubclass of
      kwscStandard: StyleObject := FObjects[soFormSysButton];
      kwscDialog: StyleObject := FObjects[soFormSysButton];
      kwscBorder: StyleObject := FObjects[soFormSysButton];
      kwscNoBorder: StyleObject := FObjects[soFormSysButton];
      kwscToolWindow: StyleObject := FObjects[soSmallSysButton];
    else
      StyleObject := nil;
    end;

  if StyleObject = nil then Exit;

  case AButton of
    kwbClose: StyleObject := StyleObject.FindObjectByAction(sbaClose);
    kwbHelp: StyleObject := StyleObject.FindObjectByAction(sbaHelp);
    kwbMin: StyleObject := StyleObject.FindObjectByAction(sbaMinimize);
    kwbMax: StyleObject := StyleObject.FindObjectByAction(sbaMaximize);
    kwbMinRestore: StyleObject := StyleObject.FindObjectByAction(sbaRestore);
    kwbMaxRestore: StyleObject := StyleObject.FindObjectByAction(sbaRestore);
    kwbRoll: StyleObject := StyleObject.FindObjectByAction(sbaRollUp);
    kwbRollRestore: StyleObject := StyleObject.FindObjectByAction(sbaRollDown);
    kwbTray: StyleObject := StyleObject.FindObjectByAction(sbaTray);
    kwbSysMenu: StyleObject := StyleObject.FindObjectByAction(sbaSysMenu);
  end;

  if StyleObject <> nil then
    Result := StyleObject.FixPosition;
end;

function TSeStyle.WindowGetButtonRect(const ASubclass: TSeWindowSubclass;
  AWindow: TSeWindowInfo; AButton: TSeWindowButtonClass; ADPI: Integer = 0;
  const AObject: TSeStyleElementObject = ktoDefault): TRect;
var
  StyleObject: TSeStyleObject;
begin
  Result := NullRect;

  StyleObject := nil;
  if AObject <> ktoDefault then
    StyleObject := FStyleSource.GetObjectByName(AObject);

  if StyleObject = nil then
    case ASubclass of
      kwscStandard: StyleObject := FObjects[soFormSysButton];
      kwscDialog: StyleObject := FObjects[soFormSysButton];
      kwscBorder: StyleObject := FObjects[soFormSysButton];
      kwscNoBorder: StyleObject := FObjects[soFormSysButton];
      kwscToolWindow: StyleObject := FObjects[soSmallSysButton];
    else
      StyleObject := nil;
    end;

  if StyleObject = nil then
    Result := NullRect
  else
  begin
    { Set WindowState }
    StyleObject.State := ssNormal;
    if kwbMaxRestore in AWindow.Buttons then
      StyleObject.State := ssMaximized;
    if kwbMinRestore in AWindow.Buttons then
      StyleObject.State := ssMinimized;
    if kwbRollRestore in AWindow.Buttons then
      StyleObject.State := ssRollup;
    StyleObject.SysButtons := AWindow.Buttons;
    StyleObject.BoundsRect := AWindow.Rect;
    StyleObject.AligningAll;

    case AButton of
      kwbClose:
        if kwbClose in AWindow.Buttons then
        begin
          StyleObject := StyleObject.FindObjectByAction(sbaClose);
          if StyleObject <> nil then
            Result := StyleObject.BoundsRect;
        end;
      kwbHelp:
        if kwbHelp in AWindow.Buttons then
        begin
          StyleObject := StyleObject.FindObjectByAction(sbaHelp);
          if StyleObject <> nil then
            Result := StyleObject.BoundsRect;
        end;
      kwbMin:
        if (kwbMin in AWindow.Buttons) then
        begin
          StyleObject := StyleObject.FindObjectByAction(sbaMinimize);
          if StyleObject <> nil then
            Result := StyleObject.BoundsRect;
        end;
      kwbMinRestore:
        if (kwbMinRestore in AWindow.Buttons) then
        begin
          StyleObject := StyleObject.FindObjectByAction(sbaRestore);
          if StyleObject <> nil then
            Result := StyleObject.BoundsRect;
        end;
      kwbMax:
        if (kwbMax in AWindow.Buttons) then
        begin
          StyleObject := StyleObject.FindObjectByAction(sbaMaximize);
          if StyleObject <> nil then
            Result := StyleObject.BoundsRect;
        end;
      kwbMaxRestore:
        if (kwbMaxRestore in AWindow.Buttons) then
        begin
          StyleObject := StyleObject.FindObjectByAction(sbaRestore);
          if StyleObject <> nil then
            Result := StyleObject.BoundsRect;
        end;
      kwbRoll:
        if (kwbRoll in AWindow.Buttons) then
        begin
          StyleObject := StyleObject.FindObjectByAction(sbaRollup);
          if StyleObject <> nil then
            Result := StyleObject.BoundsRect;
        end;
      kwbRollRestore:
        if (kwbRollRestore in AWindow.Buttons) then
        begin
          StyleObject := StyleObject.FindObjectByAction(sbaRolldown);
          if StyleObject <> nil then
            Result := StyleObject.BoundsRect;
        end;
      kwbTray:
        if (kwbTray in AWindow.Buttons) then
        begin
          StyleObject := StyleObject.FindObjectByAction(sbaTray);
          if StyleObject <> nil then
            Result := StyleObject.BoundsRect;
        end;
      kwbSysMenu:
        if (kwbSysMenu in AWindow.Buttons) then
        begin
          if ASubclass = kwscToolWindow then
          begin
            Result := NullRect;
            Exit;
          end;
          StyleObject := StyleObject.FindObjectByAction(sbaSysMenu);
          if StyleObject <> nil then
          begin
            if StyleObject.Owner is TSeStyleObject then
              TSeStyleObject(StyleObject.Owner).Aligning;
            Result := StyleObject.BoundsRect;
          end;
        end;
    end;
  end;
end;

function TSeStyle.WindowGetClientRect(const ASubclass: TSeWindowSubclass;
  const ARect: TRect; ADPI: Integer = 0; const AObject: TSeStyleElementObject = ktoDefault): TRect;
var
  WindowObject, ClientObject: TSeStyleObject;
begin
  Result := NullRect;

  WindowObject := nil;
  ClientObject := nil;
  if AObject <> ktoDefault then
  begin
    WindowObject := FStyleSource.GetObjectByName(AObject);
    if WindowObject <> nil then
      ClientObject := WindowObject.FindObjectByKind(skClient)
  end;

  if (WindowObject = nil) or (ClientObject = nil) then
  begin
    case ASubclass of
      kwscStandard:
        begin
          WindowObject := FObjects[soForm];
          ClientObject := FObjects[soFormClientRect];
        end;
      kwscDialog:
        begin
          WindowObject := FObjects[soForm];
          ClientObject := FObjects[soFormClientRect];
        end;
      kwscBorder:
        begin
          WindowObject := FObjects[soForm];
          ClientObject := FObjects[soFormClientRect];
        end;
      kwscNoBorder:
        begin
          WindowObject := FObjects[soForm];
          ClientObject := FObjects[soFormClientRect];
        end;
      kwscToolWindow:
        begin
          WindowObject := FObjects[soSmall];
          ClientObject := FObjects[soSmallClientRect];
        end;
    end;
  end;

  if (ClientObject = nil) or (WindowObject = nil) then
    Result := inherited WindowGetClientRect(ASubclass, ARect, ADPI, AObject)
  else
  begin
    WindowObject.BoundsRect := ARect;
    if AObject <> ktoDefault then
      WindowObject.AligningAll;
    Result := ClientObject.BoundsRect;
  end;
end;

function TSeStyle.WindowGetHitTest(const ASubclass: TSeWindowSubclass;
  AWindow: TSeWindowInfo; X, Y: integer; ADPI: Integer = 0;
  const AObject: TSeStyleElementObject = ktoDefault): TSeWindowHitTest;
var
  StyleObject: TSeStyleObject;
begin
  StyleObject := nil;
  if AObject <> ktoDefault then
    StyleObject := FStyleSource.GetObjectByName(AObject);

  if StyleObject = nil then
    case ASubclass of
      kwscStandard: StyleObject := FObjects[soFormSysButton];
      kwscDialog: StyleObject := FObjects[soFormSysButton];
      kwscBorder: StyleObject := FObjects[soFormSysButton];
      kwscNoBorder: StyleObject := FObjects[soFormSysButton];
      kwscToolWindow: StyleObject := FObjects[soSmallSysButton];
    else
      StyleObject := nil;
    end;

  if StyleObject = nil then
    Result := inherited WindowGetHitTest(ASubclass, AWindow, X, Y, ADPI, AObject)
  else
  begin
    StyleObject.State := ssNormal;
    if kwbMaxRestore in AWindow.Buttons then
      StyleObject.State := ssMaximized;
    if kwbMinRestore in AWindow.Buttons then
      StyleObject.State := ssMinimized;
    if kwbRollRestore in AWindow.Buttons then
      StyleObject.State := ssRollup;
    StyleObject.SysButtons := AWindow.Buttons;

    StyleObject.BoundsRect := AWindow.Rect;
    StyleObject.AligningAll;

    if AWindow.DisableNCArea then
    begin
      StyleObject := StyleObject.FindObjectByPoint(Point(X, Y));
      if (StyleObject <> nil) and (StyleObject.Kind in [skCaption, skTitle]) then
        Result := kwhtCaption
      else
        if (StyleObject <> nil) and (StyleObject.Kind in [skTop]) then
          Result := kwhtTop
        else
        if (StyleObject <> nil) and (StyleObject.Kind in [skLeft]) then
          Result := kwhtLeft
        else
        if (StyleObject <> nil) and (StyleObject.Kind in [skRight]) then
          Result := kwhtRight
        else
        if (StyleObject <> nil) and (StyleObject.Kind in [skBottom]) then
          Result := kwhtBottom
        else
        if (StyleObject <> nil) and (StyleObject.Kind in [skBottomRight]) then
          Result := kwhtBottomRight
        else
        if (StyleObject <> nil) and (StyleObject.Kind in [skBottomLeft]) then
          Result := kwhtBottomLeft
        else
          if (StyleObject <> nil) and (StyleObject is TSeSystemButton) then
          begin
            AWindow.Buttons := [];
            case TSeSystemButton(StyleObject).Action of
              sbaClose: Result := kwhtCloseButton;
              sbaMinimize: Result := kwhtMinButton;
              sbaMaximize: Result := kwhtMaxButton;
              sbaRestore: Result := kwhtMaxButton;
              sbaRollUp: Result := kwhtRollButton;
              sbaRollDown: Result := kwhtRollButton;
              sbaTray: Result := kwhtTrayButton;
            else
              Result := inherited WindowGetHitTest(ASubclass, AWindow, X, Y, ADPI, AObject);
              if Result in [kwhtBottom, kwhtLeft, kwhtRight, kwhtTop] then
                Result := kwhtClient;
            end;
          end
          else
          begin
            AWindow.Buttons := [];
            Result := inherited WindowGetHitTest(ASubclass, AWindow, X, Y, ADPI, AObject);
            if Result in [kwhtBottom, kwhtLeft, kwhtRight, kwhtTop] then
              Result := kwhtClient;
          end;
    end
    else
    begin
      StyleObject := StyleObject.FindObjectByPoint(Point(X, Y));
      if (StyleObject <> nil) and (StyleObject.Kind in [skCaption, skTitle]) then
        Result := kwhtCaption
      else
        if (StyleObject <> nil) and (StyleObject.Kind in [skTop]) then
          Result := kwhtTop
        else
          if (StyleObject <> nil) and (StyleObject is TSeSystemButton) then
          begin
            case TSeSystemButton(StyleObject).Action of
              sbaClose: Result := kwhtCloseButton;
              sbaMinimize: Result := kwhtMinButton;
              sbaMaximize: Result := kwhtMaxButton;
              sbaRestore: Result := kwhtMaxButton;
              sbaRollUp: Result := kwhtRollButton;
              sbaRollDown: Result := kwhtRollButton;
              sbaTray: Result := kwhtTrayButton;
            else
              Result := inherited WindowGetHitTest(ASubclass, AWindow, X, Y, ADPI, AObject);
            end;
          end
          else
          begin
            Result := inherited WindowGetHitTest(ASubclass, AWindow, X, Y, ADPI, AObject);
          end;
    end;
  end;
end;

function TSeStyle.WindowGetTitleRect(const ASubclass: TSeWindowSubclass;
  AWindow: TSeWindowInfo;  ADPI: Integer = 0; const AObject: TSeStyleElementObject = ktoDefault): TRect;
var
  StyleObject: TSeStyleObject;
begin
  Result := NullRect;


  StyleObject := nil;
  if AObject <> ktoDefault then
    StyleObject := FStyleSource.GetObjectByName(AObject);

  if StyleObject = nil then
    case ASubclass of
      kwscStandard: StyleObject := FObjects[soForm];
      kwscDialog: StyleObject := FObjects[soForm];
      kwscBorder: StyleObject := FObjects[soForm];
      kwscNoBorder: StyleObject := FObjects[soForm];
      kwscToolWindow: StyleObject := FObjects[soSmall];
    else
      StyleObject := nil;
    end;

  if StyleObject = nil then
    Result := inherited WindowGetTitleRect(ASubclass, AWindow, ADPI, AObject)
  else
  begin
    StyleObject.Active := kwdsActive in AWindow.State;
    StyleObject.BoundsRect := AWindow.Rect;
    StyleObject.AligningAll;
    StyleObject := StyleObject.FindObjectByKind(skTitle);
    if StyleObject <> nil then
    begin
      StyleObject.Text := AWindow.Title;
      if StyleObject.Owner is TSeStyleObject then
        TSeStyleObject(StyleObject.Owner).Aligning;
      Result := StyleObject.BoundsRect;
      StyleObject.Text := '';
    end;
  end;
end;

procedure TSeStyle.WindowDraw(const ASubclass: TSeWindowSubclass;
  Canvas: TCanvas; AWindow: TSeWindowInfo;  ADPI: Integer = 0; const AObject: TSeStyleElementObject = ktoDefault);
var
  StyleObject: TSeStyleObject;
  Client: TSeStyleObject;
  R, SaveBoundsRect: TRect;
  Buffer: TBitmap;

  procedure DrawStretchBitmap(ABitmap: TBitmap; ACanvas: TCanvas; ARect: TRect);
  var
    R1, R2: TRect;
    Margin: Integer;
  begin
    Margin := ABitmap.Height div 3;
    R1 := Rect(0, 0, ABitmap.Width, Margin);
    R2 := Rect(ARect.Left, ARect.Top, ARect.Right, ARect.Top + Margin);
    ACanvas.CopyRect(R2, ABitmap.Canvas, R1);
    R1 := Rect(0, Margin, ABitmap.Width, ABitmap.Height - Margin);
    R2 := Rect(ARect.Left, ARect.Top + Margin, ARect.Right, ARect.Bottom - Margin);
    ACanvas.CopyRect(R2, ABitmap.Canvas, R1);
    R1 := Rect(0, ABitmap.Height - Margin, ABitmap.Width, ABitmap.Height);
    R2 := Rect(ARect.Left, ARect.Bottom - Margin, ARect.Right, ARect.Bottom);
    ACanvas.CopyRect(R2, ABitmap.Canvas, R1);
  end;

begin
  StyleObject := nil;
  Client := nil;
  if (AObject <> ktoDefault) and (AObject <> 'Title') then
  begin
    StyleObject := FStyleSource.GetObjectByName(AObject);
    if StyleObject <> nil then
    begin
      Client := StyleObject.FindObjectByKind(skClient);
      if Client <> nil then
        Client.Visible := False;
    end;
  end;

  if StyleObject = nil then
    case ASubclass of
      kwscStandard: StyleObject := FObjects[soForm];
      kwscDialog: StyleObject := FObjects[soForm];
      kwscBorder: StyleObject := FObjects[soForm];
      kwscNoBorder: StyleObject := FObjects[soForm];
      kwscToolWindow: StyleObject := FObjects[soSmall];
    else
      StyleObject := nil;
    end;

  if StyleObject <> nil then
  begin
    StyleObject.Active := kwdsActive in AWindow.State;
    StyleObject.BoundsRect := AWindow.Rect;
    if (ADPI > DPI_DEFAULT) and (AObject = 'Title') then
    begin
      StyleObject := StyleObject.FindObjectByName('Image');
      if StyleObject <> nil then
      begin
        StyleObject := StyleObject.FindObjectByName('Title');
        if StyleObject <> nil then
        begin
          R := StyleObject.BoundsRect;
          R.Bottom := MulDiv(AWindow.ClipRect.Bottom, ADPI, DPI_DEFAULT);
          SaveBoundsRect := StyleObject.BoundsRect;
          StyleObject.BoundsRect := R;
          StyleObject.FStopDrawChilds := True;
          if (StyleObject.MarginLeft + StyleObject.MarginRight = 0) or
             (StyleObject.MarginTop + StyleObject.MarginBottom = 0) then
          begin
            Buffer := TBitmap.Create;
            try
              Buffer.SetSize(R.Width, AWindow.ClipRect.Height);
              R := Rect(0, 0, Buffer.Width, Buffer.Height);
              StyleObject.BoundsRect := R;
              StyleObject.Draw(Buffer.Canvas, R);
              R := AWindow.ClipRect;
              R.Bottom := MulDiv(R.Bottom, ADPI, DPI_DEFAULT);
              DrawStretchBitmap(Buffer, Canvas, R);
            finally
              Buffer.Free;
            end;
          end
          else
            StyleObject.Draw(Canvas, R, ADPI);
          StyleObject.FStopDrawChilds := False;
          StyleObject.BoundsRect := SaveBoundsRect;
        end;
      end;
    end
    else
      StyleObject.Draw(Canvas, AWindow.ClipRect, ADPI);
  end;

  if Client <> nil then
    Client.Visible := True;
end;

procedure TSeStyle.WindowDrawClient(const ASubclass: TSeWindowSubclass;
  Canvas: TCanvas; const ARect: TRect; ADPI: Integer = 0;
  const AObject: TSeStyleElementObject= ktoDefault);
var
  StyleObject: TSeStyleOBject;
begin


  if AObject <> ktoDefault then
  begin
    StyleObject := FStyleSource.GetObjectByName(AObject);
    if StyleObject <> nil then
    begin
      StyleObject := StyleObject.FindObjectByKind(skClient);
      if StyleObject <> nil then
      begin
        StyleObject.BoundsRect := ARect;
        StyleObject.Draw(Canvas, NullRect, ADPI);
        Exit;
      end;
    end;
  end;
  if (ASubclass in [kwscStandard, kwscDialog]) and (FObjects[soClient] <> nil) then
  begin
    FObjects[soClient].BoundsRect := ARect;
    FObjects[soClient].Draw(Canvas, NullRect);
    Exit;
  end;
  if (ASubclass = kwscToolWindow) and (FObjects[soSmallClient] <> nil) then
  begin
    FObjects[soSmallClient].BoundsRect := ARect;
    FObjects[soSmallClient].Draw(Canvas, NullRect, ADPI);
    Exit;
  end;
end;

procedure TSeStyle.WindowDrawButton(const ASubclass: TSeWindowSubclass;
  Canvas: TCanvas; AButton: TSeWindowButtonInfo;  ADPI: Integer = 0;
  const AObject: TSeStyleElementObject = ktoDefault);
var
  StyleObject, Root: TSeStyleObject;
begin
  if IsRectEmpty(AButton.Rect) then Exit;

  Root := nil;
  if AObject <> ktoDefault then
    Root := FStyleSource.GetObjectByName(AObject);

  if Root = nil then
    case ASubclass of
      kwscStandard: Root := FObjects[soFormSysButtonDraw];
      kwscDialog: Root := FObjects[soFormSysButtonDraw];
      kwscBorder: Root := FObjects[soFormSysButtonDraw];
      kwscNoBorder: Root := FObjects[soFormSysButtonDraw];
      kwscToolWindow: Root := FObjects[soSmallSysButtonDraw];
    else
      Root := nil;
    end;

  if Root <> nil then
  begin
    case AButton.Button of
      kwbClose:
        begin
          StyleObject := Root.FindObjectByAction(sbaClose);
          if StyleObject <> nil then
          begin
            StyleObject.Active := not (AButton.DrawState = kwbdsInactive);
            if StyleObject is TSeSystemButton then
            begin
              if kwbdsPressed = AButton.DrawState then TSeSystemButton(StyleObject).ButtonState := sbsDown
              else
                if kwbdsHot = AButton.DrawState then
                  TSeSystemButton(StyleObject).ButtonState := sbsHover
                else
                  TSeSystemButton(StyleObject).ButtonState := sbsNormal;
            end;
            StyleObject.BoundsRect := AButton.Rect;
            StyleObject.Draw(Canvas, NullRect, ADPI);
          end;
        end;
      kwbHelp:
        begin
          StyleObject := Root.FindObjectByAction(sbaHelp);
          if StyleObject <> nil then
          begin
            StyleObject.Active := not (AButton.DrawState = kwbdsInactive);
            if StyleObject is TSeSystemButton then
            begin
              if kwbdsPressed = AButton.DrawState then TSeSystemButton(StyleObject).ButtonState := sbsDown
              else
                if kwbdsHot = AButton.DrawState then TSeSystemButton(StyleObject).ButtonState := sbsHover
                else
                  TSeSystemButton(StyleObject).ButtonState := sbsNormal;
            end;
            StyleObject.BoundsRect := AButton.Rect;
            StyleObject.Draw(Canvas, NullRect, ADPI);
          end;
        end;
      kwbMin:
        begin
          StyleObject := Root.FindObjectByAction(sbaMinimize);
          if StyleObject <> nil then
          begin
            StyleObject.Active := not (AButton.DrawState = kwbdsInactive);
            if StyleObject is TSeSystemButton then
            begin
              if kwbdsPressed = AButton.DrawState then TSeSystemButton(StyleObject).ButtonState := sbsDown
              else
                if kwbdsHot = AButton.DrawState then TSeSystemButton(StyleObject).ButtonState := sbsHover
                else
                  TSeSystemButton(StyleObject).ButtonState := sbsNormal;
            end;
            StyleObject.BoundsRect := AButton.Rect;
            StyleObject.Draw(Canvas, NullRect, ADPI);
          end;
        end;
      kwbMinRestore:
        begin
          StyleObject := Root.FindObjectByAction(sbaRestore);
          if StyleObject <> nil then
          begin
            StyleObject.Active := not (AButton.DrawState = kwbdsInactive);
            if StyleObject is TSeSystemButton then
            begin
              if kwbdsPressed = AButton.DrawState then TSeSystemButton(StyleObject).ButtonState := sbsDown
              else
                if kwbdsHot = AButton.DrawState then TSeSystemButton(StyleObject).ButtonState := sbsHover
                else
                  TSeSystemButton(StyleObject).ButtonState := sbsNormal;
            end;
            StyleObject.BoundsRect := AButton.Rect;
            StyleObject.Draw(Canvas, NullRect, ADPI);
          end;
        end;
      kwbMax:
        begin
          StyleObject := Root.FindObjectByAction(sbaMaximize);
          if StyleObject <> nil then
          begin
            StyleObject.Active := not (AButton.DrawState = kwbdsInactive);
            if StyleObject is TSeSystemButton then
            begin
              if kwbdsPressed = AButton.DrawState then TSeSystemButton(StyleObject).ButtonState := sbsDown
              else
                if kwbdsHot = AButton.DrawState then TSeSystemButton(StyleObject).ButtonState := sbsHover
                else
                  TSeSystemButton(StyleObject).ButtonState := sbsNormal;
            end;
            StyleObject.BoundsRect := AButton.Rect;
            StyleObject.Draw(Canvas, NullRect, ADPI);
          end;
        end;
      kwbMaxRestore:
        begin
          StyleObject := Root.FindObjectByAction(sbaRestore);
          if StyleObject <> nil then
          begin
            StyleObject.Active := not (AButton.DrawState = kwbdsInactive);
            if StyleObject is TSeSystemButton then
            begin
              if kwbdsPressed = AButton.DrawState then TSeSystemButton(StyleObject).ButtonState := sbsDown
              else
                if kwbdsHot = AButton.DrawState then TSeSystemButton(StyleObject).ButtonState := sbsHover
                else
                  TSeSystemButton(StyleObject).ButtonState := sbsNormal;
            end;
            StyleObject.BoundsRect := AButton.Rect;
            StyleObject.Draw(Canvas, NullRect, ADPI);
          end;
        end;
      kwbRoll:
        begin
          StyleObject := Root.FindObjectByAction(sbaRollup);
          if StyleObject <> nil then
          begin
            StyleObject.Active := not (AButton.DrawState = kwbdsInactive);
            if StyleObject is TSeSystemButton then
            begin
              if kwbdsPressed = AButton.DrawState then TSeSystemButton(StyleObject).ButtonState := sbsDown
              else
                if kwbdsHot = AButton.DrawState then TSeSystemButton(StyleObject).ButtonState := sbsHover
                else
                  TSeSystemButton(StyleObject).ButtonState := sbsNormal;
            end;
            StyleObject.BoundsRect := AButton.Rect;
            StyleObject.Draw(Canvas, NullRect, ADPI);
          end;
        end;
      kwbRollRestore:
        begin
          StyleObject := Root.FindObjectByAction(sbaRolldown);
          if StyleObject <> nil then
          begin
            StyleObject.Active := not (AButton.DrawState = kwbdsInactive);
            if StyleObject is TSeSystemButton then
            begin
              if kwbdsPressed = AButton.DrawState then TSeSystemButton(StyleObject).ButtonState := sbsDown
              else
                if kwbdsHot = AButton.DrawState then TSeSystemButton(StyleObject).ButtonState := sbsHover
                else
                  TSeSystemButton(StyleObject).ButtonState := sbsNormal;
            end;
            StyleObject.BoundsRect := AButton.Rect;
            StyleObject.Draw(Canvas, NullRect, ADPI);
          end;
        end;
      kwbTray:
        begin
          StyleObject := Root.FindObjectByAction(sbaTray);
          if StyleObject <> nil then
          begin
            StyleObject.Active := not (AButton.DrawState = kwbdsInactive);
            if StyleObject is TSeSystemButton then
            begin
              if kwbdsPressed = AButton.DrawState then TSeSystemButton(StyleObject).ButtonState := sbsDown
              else
                if kwbdsHot = AButton.DrawState then TSeSystemButton(StyleObject).ButtonState := sbsHover
                else
                  TSeSystemButton(StyleObject).ButtonState := sbsNormal;
            end;
            StyleObject.BoundsRect := AButton.Rect;
            StyleObject.Draw(Canvas, NullRect, ADPI);
          end;
        end;
      kwbSysMenu:
        begin
          StyleObject := Root.FindObjectByAction(sbaSysMenu);
          if StyleObject <> nil then
          begin
            StyleObject.Active := not (AButton.DrawState = kwbdsInactive);
            if StyleObject is TSeSystemButton then
            begin
              if kwbdsPressed = AButton.DrawState then TSeSystemButton(StyleObject).ButtonState := sbsDown
              else
                if kwbdsHot = AButton.DrawState then TSeSystemButton(StyleObject).ButtonState := sbsHover
                else
                  TSeSystemButton(StyleObject).ButtonState := sbsNormal;
            end;
            StyleObject.BoundsRect := AButton.Rect;
            StyleObject.Draw(Canvas, NullRect, ADPI);
          end;
          inherited ;
        end;
    end;
  end;
end;

procedure TSeStyle.WindowDrawGripper(const ASubclass: TSeWindowSubclass;
  Canvas: TCanvas; AGripper: TSeWindowGripperInfo;  ADPI: Integer = 0;
  const AObject: TSeStyleElementObject = ktoDefault);
var
  StyleObject: TSeStyleObject;
  Buffer: TBitmap;
begin
  if FObjects[soStatus] <> nil then
  begin
    StyleObject := FObjects[soStatus].FindObjectByName('Gripper');
    if StyleObject <> nil then
    begin
      Buffer := TBitmap.Create;
      try
        Buffer.SetSize(AGripper.Rect.Width, AGripper.Rect.Height);
        Buffer.Canvas.Brush.Color := FColors[ktcWindow];
        Buffer.Canvas.FillRect(Rect(0, 0, Buffer.Width, Buffer.Height));
        StyleObject.BoundsRect := Rect(0, 0, Buffer.Width, Buffer.Height);
        StyleObject.Draw(Buffer.Canvas, NullRect, ADPI);
        Canvas.Draw(AGripper.Rect.Left, AGripper.Rect.Top, Buffer);
      finally
        Buffer.Free;
      end;
    end;
  end;
end;

procedure TSeStyle.WindowDrawText(const ASubclass: TSeWindowSubclass; Canvas: TCanvas;
  AWindow: TSeWindowInfo; const ARect: TRect; RightToLeftReading: Boolean; ADPI: Integer = 0;
   const AObject: TSeStyleElementObject = ktoDefault);
const
  BiDiModes: array[Boolean] of TBiDiMode = (bdLeftToRight, bdRightToLeft);
var
  StyleObject: TSeStyleObject;
  Btn: TSeWindowButtonClass;
  R: TRect;
  FirstButton: integer;
begin


  StyleObject := nil;
  if AObject <> ktoDefault then
    StyleObject := FStyleSource.GetObjectByName(AObject);

  if StyleObject = nil then
    case ASubclass of
      kwscStandard: StyleObject := FObjects[soForm];
      kwscDialog: StyleObject := FObjects[soForm];
      kwscBorder: StyleObject := FObjects[soForm];
      kwscNoBorder: StyleObject := FObjects[soForm];
      kwscToolWindow: StyleObject := FObjects[soSmall];
    else
      StyleObject := nil;
    end;

  if StyleObject <> nil then
  begin
    StyleObject := StyleObject.FindObjectByKind(skTitle);
    if StyleObject <> nil then
    begin
      if AObject = ktoDefault then
      begin
        if kwdsActive in AWindow.State then
          StyleObject.Font.Assign(Fonts[ktfCaptionTextNormal])
        else
          StyleObject.Font.Assign(Fonts[ktfCaptionTextInactive]);

        if (ADPI <> 0) and (StyleObject.Font.PixelsPerInch <> ADPI) then
          StyleObject.Font.Height := MulDiv(StyleObject.Font.Height, ADPI, StyleObject.Font.PixelsPerInch);
      end
      else
      begin
        if kwdsActive in AWindow.State then
          StyleObject.State := ssFocused
        else
          StyleObject.State := ssNormal;
        StyleObject.Font.Assign(StyleObject.Font);
      end;

      if AObject = ktoDefault then
      begin
        StyleObject.Text := AWindow.Title;
        StyleObject.BoundsRect := ARect;
        StyleObject.TextMarginLeft := 0;
        StyleObject.TextMarginRight := 0;
        StyleObject.BiDiMode := BiDiModes[RightToLeftReading];
        StyleObject.DrawObjectText(Canvas);
        StyleObject.Text := '';
        Exit;
      end;

     if StyleObject.TextMarginRightStretch then
      begin
        { Calc new TextMarginRight }
        FirstButton := ARect.Right;
       for Btn := Low(TSeWindowButtonClass) to High(TSeWindowButtonClass) do
        begin
          R := WindowGetButtonRect(ASubclass, AWindow, Btn);
          if not IsRectEmpty(R) then
            if (R.Left > StyleObject.Left) and (R.Left < FirstButton) then
              FirstButton := R.Left;
        end;
        StyleObject.TextMarginRight := (ARect.Right - FirstButton)
      end;

      StyleObject.Text := AWindow.Title;
      StyleObject.BoundsRect := ARect;
      if (kwbSysMenu in AWindow.Buttons) and StyleObject.TextShiftSysIton then
        StyleObject.TextMarginLeft := StyleObject.TextMarginLeft + 20;
      StyleObject.Draw(Canvas, NullRect);
      if (kwbSysMenu in AWindow.Buttons) and StyleObject.TextShiftSysIton then
        StyleObject.TextMarginLeft := StyleObject.TextMarginLeft - 20;
      StyleObject.Text := '';
    end;
  end;
end;

{ Hint class ==================================================================}

function TSeStyle.IsHintDefined(const ASubclass: TSeHintSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin
  Result := FObjects[soHint] <> nil;
end;

procedure TSeStyle.HintDraw(const ASubclass: TSeHintSubclass;
  Canvas: TCanvas; AHint: TSeHintInfo; const AObject: TSeStyleElementObject);
begin
  if FObjects[soHint] <> nil then
  begin
    FObjects[soHint].BoundsRect := AHint.Rect;
    FObjects[soHint].Draw(Canvas, NullRect);
  end
  else
  begin
    FillRect(Canvas, AHint.Rect, Application.HintColor);
    DrawRect(Canvas, AHint.Rect, clGray);
  end;
end;

function TSeStyle.HintGetClientRect(const ASubclass: TSeHintSubclass;
  Canvas: TCanvas; AHint: TSeHintInfo;
  const AObject: TSeStyleElementObject): TRect;
begin
  Result := AHint.Rect;
  InflateRect(Result, -2, -1);
end;

{ Label class =================================================================}

function TSeStyle.IsLabelDefined(const ASubclass: TSeLabelSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin
  Result := True;
end;

{ Panel class =================================================================}

function TSeStyle.IsPanelDefined(const ASubclass: TSePanelSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin
  Result := False;
  if AObject <> ktoDefault then
    Result := (FStyleSource.GetObjectByName(AObject) <> nil);
  if not Result then
    case ASubclass of
      kpscPanel: Result := (FObjects[soPanel] <> nil);
    else
      Result := False;
    end;
end;

procedure TSeStyle.PanelDraw(const ASubclass: TSePanelSubclass;
  Canvas: TCanvas; APanel: TSePanelInfo; const AObject: TSeStyleElementObject);
var
  StyleObject: TSeStyleObject;
begin
  if AObject <> ktoDefault then
  begin
    StyleObject := FStyleSource.GetObjectByName(AObject);
    if StyleObject <> nil then
      StyleObject := StyleObject.FindObjectByName('Panel');
  end
  else
    StyleObject := FObjects[soPanel].FindObjectByName('Panel');
  if StyleObject <> nil then
  begin
    StyleObject.BoundsRect := APanel.Rect;
    StyleObject.Draw(Canvas, NullRect);
  end;
end;

function TSeStyle.PanelGetClientRect(const ASubclass: TSePanelSubclass;
  APanel: TSePanelInfo; const AObject: TSeStyleElementObject): TRect;
var
  StyleObject: TSeStyleObject;
begin
  Result := APanel.Rect;


  if AObject <> ktoDefault then
  begin
    StyleObject := FStyleSource.GetObjectByName(AObject);
    if StyleObject <> nil then
      StyleObject := StyleObject.FindObjectByName('Panel');
  end
  else
    StyleObject := FObjects[soPanel].FindObjectByName('Panel');

  if StyleObject <> nil then
  begin
    Result := APanel.Rect;
    StyleObject.BoundsRect := APanel.Rect;
    with StyleObject, Result do
      Result := Rect(Left + MarginLeft, Top + MarginTop, Right - MarginRight,
        Bottom - MarginBottom);
  end;
end;

{ Button class ================================================================}

function TSeStyle.IsButtonDefined(const ASubclass: TSeButtonSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin
  Result := False;
  if AObject <> ktoDefault then
    Result := (FStyleSource.GetObjectByName(AObject) <> nil);
  if not Result then
    case ASubclass of
      kbscButton: Result := (FObjects[soButton] <> nil);
      kbscSpinUp: Result := (FObjects[soSpinButton] <> nil);
      kbscSpinDown: Result := (FObjects[soSpinButton] <> nil);
    else
      Result := False;
    end;
end;

function TSeStyle.IsButtonTransparent(const ASubclass: TSeButtonSubclass;
  const AObject: TSeStyleElementObject): boolean;
var
  StyleObject: TSeStyleObject;
begin


  StyleObject := nil;
  if AObject <> ktoDefault then
    StyleObject := FStyleSource.GetObjectByName(AObject);
  if StyleObject = nil then
    case ASubclass of
      kbscButton: StyleObject := FObjects[soButton];
      kbscSpinUp: StyleObject := FObjects[soSpinButton];
      kbscSpinDown: StyleObject := FObjects[soSpinButton];
    else
      StyleObject := nil;
    end;

  if (StyleObject <> nil) and (StyleObject.FindObjectByName('Face') <> nil) then
  begin
    StyleObject := StyleObject.FindObjectByName('Face');
    Result := StyleObject.Masked;
  end
  else
    Result := False;
end;

procedure TSeStyle.ButtonDraw(const ASubclass: TSeButtonSubclass;
  Canvas: TCanvas; AButton: TSeButtonInfo; ADPI: Integer = 0; const AObject: TSeStyleElementObject = ktoDefault);
var
  R, SaveR: TRect;
  StyleObject: TSeStyleObject;
  StyleState: TSeState;
begin
  with AButton do
  begin
    R := AButton.Rect;
    StyleObject := nil;

    if AObject <> ktoDefault then
      StyleObject := FStyleSource.GetObjectByName(AObject);

    if StyleObject = nil then
       case ASubclass of
        kbscButton: StyleObject := FObjects[soButton];
        kbscSpinUp, kbscSpinDown: StyleObject := FObjects[soScrollBar];
        kbscSpinLeft, kbscSpinRight: StyleObject := FObjects[soScrollBar];
      else
        StyleObject := nil;
      end;

    if StyleObject = nil then Exit;

     if StyleObject.FindObjectByName('Face') <> nil then
           StyleObject := StyleObject.FindObjectByName('Face');

    if ASubclass = kbscSpinLeft then
    begin
      StyleObject := StyleObject.FindObjectByName('LeftButton');
      if StyleObject = nil then Exit;
    end;
    if ASubclass = kbscSpinRight then
    begin
      StyleObject := StyleObject.FindObjectByName('RightButton');
      if StyleObject = nil then Exit;
    end;
    if ASubclass = kbscSpinUp then
    begin
      StyleObject := StyleObject.FindObjectByName('TopButton');
      if StyleObject = nil then Exit;
    end;
    if ASubclass = kbscSpinDown then
    begin
      StyleObject := StyleObject.FindObjectByName('BottomButton');
      if StyleObject = nil then Exit;
    end;

    if DrawState in [kbdsDisabled] then
      StyleState := ssDisabled
    else
      if DrawState in [kbdsPressed] then
        StyleState := ssPressed
      else
        if DrawState in [kbdsHot, kbdsDefaultHot, kbdsFocusedHot] then
          StyleState := ssHot
        else
          if DrawState in [kbdsFocused, kbdsDefault] then
            StyleState := ssFocused
          else
            if DrawState in [kbdsChecked] then
              StyleState := ssPressed
            else
              StyleState := ssNormal;

    { Draw face }
    StyleObject.State := StyleState;
    SaveR := StyleObject.BoundsRect;
    StyleObject.BoundsRect := R;
    StyleObject.SafeDraw(Canvas, NullRect, ADPI);
    StyleObject.BoundsRect := SaveR;
  end;
end;

procedure TSeStyle.ButtonDrawText(const ASubclass: TSeButtonSubclass;
  Canvas: TCanvas; AButton: TSeButtonInfo; AText: TSeTextInfo;
  const AObject: TSeStyleElementObject);
var
  StyleObject: TSeStyleObject;
  R: TRect;
begin

  if AObject <> ktoDefault then
    StyleObject := FStyleSource.GetObjectByName(AObject)
  else
    StyleObject := FStyleSource.GetObjectByName('Button');

  if StyleObject = nil then
  begin
    inherited;
    Exit;
  end;
  R := AButton.Rect;
  if StyleObject is TSeTextObject then
    case TSeTextObject(StyleObject).TextAlign of
      taTopLeft: AText.Align := ktxaTopLeft;
      taTopCenter: AText.Align := ktxaTopCenter;
      taTopRight: AText.Align := ktxaTopRight;
      taLeft: AText.Align := ktxaMiddleLeft;
      taCenter: AText.Align := ktxaMiddleCenter;
      taRight: AText.Align := ktxaMiddleRight;
      taBottomLeft: AText.Align := ktxaBottomLeft;
      taBottomCenter: AText.Align := ktxaBottomCenter;
      taBottomRight: AText.Align := ktxaBottomRight;
    end;

  { Draw }
  DrawStyleText(Canvas, AText);
end;

{ Check class =================================================================}

function TSeStyle.IsCheckDefined(const ASubclass: TSeCheckSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin
  Result := False;
  if AObject <> ktoDefault then
    Result := (FStyleSource.GetObjectByName(AObject) <> nil);
  if not Result then
    case ASubclass of
      kcscCheckBox: Result := (FObjects[soCheckBox] <> nil);
      kcscRadioButton: Result := (FObjects[soRadioButton] <> nil);
    end;
end;

function TSeStyle.IsCheckTransparent(const ASubclass: TSeCheckSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin
  Result := True;
end;

function TSeStyle.CheckGetSize(const ASubclass: TSeCheckSubclass; ADPI: Integer = 0;
  const AObject: TSeStyleElementObject = ktoDefault): TPoint;
var
  StyleObject: TSeStyleObject;
begin
  Result := inherited CheckGetSize(ASubclass, ADPI, AObject);

  StyleObject := nil;
  if AObject <> ktoDefault then StyleObject := FStyleSource.GetObjectByName(AObject);
  if StyleObject = nil then
  begin
    case ASubclass of
      kcscCheckBox: StyleObject := FObjects[soCheckbox];
      kcscRadioButton: StyleObject := FObjects[soRadioButton];
    end;
  end;

  if StyleObject <> nil then
  begin
    StyleObject := StyleObject.FindObjectByName('Checked');
    if StyleObject <> nil then
    begin
      Result.X := StyleObject.Width;
      Result.Y := StyleObject.Height;
      if ADPI > DPI_DEFAULT then
      begin
        Result.X := MulDiv(Result.X, ADPI, DPI_DEFAULT);
        Result.Y := MulDiv(Result.Y, ADPI, DPI_DEFAULT);
      end;
    end;
  end;
end;

procedure TSeStyle.CheckDraw(const ASubclass: TSeCheckSubclass; Canvas: TCanvas;
      ACheck: TSeCheckInfo; ADPI: Integer = 0; const AObject: TSeStyleElementObject = ktoDefault);
var
  R: TRect;
  StyleObject: TSeStyleObject;
  StyleState: TSeState;
begin
  StyleObject := nil;
  StyleState := ssNormal;
  with ACheck do
  begin
    case ASubclass of
      kcscCheckBox:
        begin
          if AObject <> ktoDefault then StyleObject := FStyleSource.GetObjectByName(AObject);
          if StyleObject = nil then StyleObject := FObjects[soCheckbox];
          case DrawState of
            kcdsCheckedNormal:
              begin
                StyleObject := StyleObject.FindObjectByName('Checked');
                StyleState := ssNormal;
              end;
            kcdsCheckedHot:
              begin
                StyleObject := StyleObject.FindObjectByName('Checked');
                StyleState := ssHot;
              end;
            kcdsCheckedPressed:
              begin
                StyleObject := StyleObject.FindObjectByName('Checked');
                StyleState := ssPressed;
              end;
            kcdsCheckedDisabled:
              begin
                StyleObject := StyleObject.FindObjectByName('Checked');
                StyleState := ssDisabled;
              end;
            kcdsUncheckedNormal:
              begin
                StyleObject := StyleObject.FindObjectByName('Unchecked');
                StyleState := ssNormal;
              end;
            kcdsUncheckedHot:
              begin
                StyleObject := StyleObject.FindObjectByName('Unchecked');
                StyleState := ssHot;
              end;
            kcdsUncheckedPressed:
              begin
                StyleObject := StyleObject.FindObjectByName('Unchecked');
                StyleState := ssPressed;
              end;
            kcdsUncheckedDisabled:
              begin
                StyleObject := StyleObject.FindObjectByName('Unchecked');
                StyleState := ssDisabled;
              end;
            kcdsMixedNormal:
              begin
                StyleObject := StyleObject.FindObjectByName('Mixed');
                StyleState := ssNormal;
              end;
            kcdsMixedHot:
              begin
                StyleObject := StyleObject.FindObjectByName('Mixed');
                StyleState := ssHot;
              end;
            kcdsMixedPressed:
              begin
                StyleObject := StyleObject.FindObjectByName('Mixed');
                StyleState := ssPressed;
              end;
            kcdsMixedDisabled:
              begin
                StyleObject := StyleObject.FindObjectByName('Mixed');
                StyleState := ssDisabled;
              end;
          end
        end;
      kcscRadioButton:
        begin
          if AObject <> ktoDefault then StyleObject := FStyleSource.GetObjectByName(AObject);
          if StyleObject = nil then StyleObject := FObjects[soRadioButton];
          case DrawState of
            kcdsCheckedNormal:
              begin
                StyleObject := StyleObject.FindObjectByName('Checked');
                StyleState := ssNormal;
              end;
            kcdsCheckedHot:
              begin
                StyleObject := StyleObject.FindObjectByName('Checked');
                StyleState := ssHot;
              end;
            kcdsCheckedPressed:
              begin
                StyleObject := StyleObject.FindObjectByName('Checked');
                StyleState := ssPressed;
              end;
            kcdsCheckedDisabled:
              begin
                StyleObject := StyleObject.FindObjectByName('Checked');
                StyleState := ssDisabled;
              end;
            kcdsUncheckedNormal:
              begin
                StyleObject := StyleObject.FindObjectByName('Unchecked');
                StyleState := ssNormal;
              end;
            kcdsUncheckedHot:
              begin
                StyleObject := StyleObject.FindObjectByName('Unchecked');
                StyleState := ssHot;
              end;
            kcdsUncheckedPressed:
              begin
                StyleObject := StyleObject.FindObjectByName('Unchecked');
                StyleState := ssPressed;
              end;
            kcdsUncheckedDisabled:
              begin
                StyleObject := StyleObject.FindObjectByName('Unchecked');
                StyleState := ssDisabled;
              end;
          end
        end;
    end;

    if StyleObject <> nil then
    begin
      R := StyleObject.BoundsRect;
      RectCenter(R, Rect);
      StyleObject.State := StyleState;
      StyleObject.BoundsRect := R;
      StyleObject.Draw(Canvas, NullRect, ADPI);
    end;
  end;
end;

{ Splitter class ==============================================================}

function TSeStyle.IsSplitterDefined(const ASubclass: TSeSplitterSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin
  case ASubclass of
    ksscSplitter: Result := (FObjects[soSplitter] <> nil);
  else
    Result := False;
  end;
end;

procedure TSeStyle.SplitterDraw(const ASubclass: TSeSplitterSubclass;
  Canvas: TCanvas; ASplitter: TSeSplitterInfo;
  const AObject: TSeStyleElementObject);
var
  StyleObject: TSeStyleObject;
begin
  StyleObject := FObjects[soSplitter];
  if StyleObject <> nil then
  begin
    StyleObject.BoundsRect := ASplitter.Rect;
    StyleObject.Draw(Canvas, NullRect);
  end;
end;

{ Scroll class ================================================================}

function TSeStyle.IsScrollDefined(const ASubclass: TSeScrollSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin
  Result := False;
  if AObject <> ktoDefault then
    Result := (FStyleSource.GetObjectByName(AObject) <> nil);

  if not Result then
    case ASubclass of
      ksscScrollBar: Result := (FObjects[soScrollBar] <> nil);
    else
      Result := False;
    end;
end;

function TSeStyle.IsScrollTransparent(const ASubclass: TSeScrollSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin
  case ASubclass of
    ksscScrollBar: Result := FObjects[soScrollBar].Masked;
  else
    Result := False;
  end;
end;

procedure TSeStyle.ScrollDraw(const ASubclass: TSeScrollSubclass;
  Canvas: TCanvas; AScroll: TSeScrollInfo; ADPI: Integer = 0;  const AObject: TSeStyleElementObject = ktoDefault);
var
  StyleObject: TSeStyleObject;
begin
  StyleObject := nil;
  if AObject <> ktoDefault then
  begin
    StyleObject := FStyleSource.GetObjectByName(AObject);
    if StyleObject <> nil then
      StyleObject := StyleObject.FindObjectByName('Frame');
  end;
  if StyleObject = nil then
    StyleObject := FObjects[soScrollBox].FindObjectByName('Frame');

  if StyleObject <> nil then
  begin
    StyleObject.BoundsRect := AScroll.Rect;
    StyleObject.Draw(Canvas, NullRect, ADPI);
  end;
end;

procedure TSeStyle.ScrollDrawArea(const ASubclass: TSeScrollSubclass;
  Canvas: TCanvas; AArea: TSeScrollAreaInfo; ADPI: Integer = 0;
  const AObject: TSeStyleElementObject = ktoDefault);
var
  Root, StyleObject: TSeStyleObject;
  DrawState: TSeState;
begin


  Root := nil;
  if AObject <> ktoDefault then
    Root := FStyleSource.GetObjectByName(AObject);
  if Root = nil then Root := FObjects[soScrollBar];

  if AArea.Area <> ksacVertical then
    StyleObject := Root.FindObjectByName('HorzFrame')
  else
    StyleObject := Root.FindObjectByName('VertFrame');

  if StyleObject <> nil then
  begin
    if AArea.DrawState = ksadsDisabled then
      DrawState := ssDisabled
    else
      if AArea.DrawState = ksadsPressed then
        DrawState := ssPressed
      else
        if AArea.DrawState = ksadsHot then
          DrawState := ssHot
        else
          DrawState := ssNormal;

    StyleObject.State := DrawState;
    StyleObject.BoundsRect := AArea.Rect;
    StyleObject.Draw(Canvas, NullRect, ADPI);
  end;
end;

procedure TSeStyle.ScrollDrawButton(const ASubclass: TSeScrollSubclass;
  Canvas: TCanvas; AButton: TSeScrollButtonInfo; ADPI: Integer = 0;
  const AObject: TSeStyleElementObject = ktoDefault);
var
  Root, StyleObject: TSeStyleObject;
  StyleState: TSeState;
begin
  Root := nil;
  if AObject <> ktoDefault then
    Root := FStyleSource.GetObjectByName(AObject);
  if Root = nil then Root := FObjects[soScrollBar];

  case AButton.Button of
    ksbcUp: StyleObject := Root.FindObjectByName('TopButton');
    ksbcDown: StyleObject := Root.FindObjectByName('BottomButton');
    ksbcPageUp: StyleObject := nil;
    ksbcPageDown: StyleObject := nil;
    ksbcLeft: StyleObject := Root.FindObjectByName('LeftButton');
    ksbcRight: StyleObject := Root.FindObjectByName('RightButton');
    ksbcPageLeft: StyleObject := nil;
    ksbcPageRight: StyleObject := nil;
    ksbcHorzSlider: StyleObject := Root.FindObjectByName('HorzSlider');
    ksbcVertSlider: StyleObject := Root.FindObjectByName('VertSlider');
    ksbcPlus: StyleObject := nil;
    ksbcMinus: StyleObject := nil;
    ksbcCustom: StyleObject := nil;
  else
    StyleObject := nil;
  end;

  if StyleObject <> nil then
    with AButton do
    begin
      if DrawState = ksbdsDisabled then
        StyleState := ssDisabled
      else
        if DrawState = ksbdsPressed then
          StyleState := ssPressed
        else
          if DrawState = ksbdsHot then
            StyleState := ssHot
          else
            StyleState := ssNormal;

      if AButton.Button in [ksbcVertSlider, ksbcHorzSlider, ksbcUp, ksbcDown] then
      begin
        StyleObject.State := StyleState;
        StyleObject.BoundsRect := Rect;
        StyleObject.SafeDraw(Canvas, NullRect, ADPI);
      end
      else
      begin
        StyleObject.State := StyleState;
        StyleObject.BoundsRect := Rect;
        StyleObject.Draw(Canvas, NullRect, ADPI);
      end;
    end;
end;

{ Progress class ==============================================================}

function TSeStyle.IsProgressDefined(const ASubclass: TSeProgressSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin
  Result := False;
  if AObject <> ktoDefault then
    Result := (FStyleSource.GetObjectByName(AObject) <> nil);
  if not Result then
    case ASubclass of
      kpscProgressBar: Result := (FObjects[soProgressBar] <> nil);
    else
      Result := False;
    end;
end;

function TSeStyle.IsProgressTransparent(const ASubclass: TSeProgressSubclass;
  const AObject: TSeStyleElementObject): boolean;
var
  StyleObject, S: TSeStyleObject;
begin
  StyleObject := nil;
  if AObject <> ktoDefault then
    StyleObject := FStyleSource.GetObjectByName(AObject);

  if StyleObject = nil then
    StyleObject := FObjects[soProgressBar];

  Result := False;
  case ASubclass of
    kpscProgressBar:
      begin
        S := StyleObject.FindObjectByName('Frame');
        if S <> nil then Result := S.Masked;
      end;
  end;
end;

procedure TSeStyle.ProgressDraw(const ASubclass: TSeProgressSubclass;
  Canvas: TCanvas; AProgress: TSeProgressInfo;
  const AObject: TSeStyleElementObject);
var
  Root, StyleObject: TSeStyleObject;
begin
  Root := nil;
  if AObject <> ktoDefault then
    Root := FStyleSource.GetObjectByName(AObject);
  if Root = nil then
    Root := FObjects[soProgressBar];

  StyleObject := nil;
  case AProgress.Part of
    kppBarVert:
      begin
        StyleObject := Root.FindObjectByName('FrameVert');
        if StyleObject = nil then
          StyleObject := Root.FindObjectByName('Frame');
      end;
    kppBarHorz:
      begin
        StyleObject := Root.FindObjectByName('FrameHorz');
        if StyleObject = nil then
          StyleObject := Root.FindObjectByName('Frame');
      end;
    kppChunkVert:
      begin
        StyleObject := Root.FindObjectByName('BarVert');
        if StyleObject = nil then
          StyleObject := Root.FindObjectByName('Bar');
      end;
    kppChunkHorz:
      begin
        StyleObject := Root.FindObjectByName('BarHorz');
        if StyleObject = nil then
          StyleObject := Root.FindObjectByName('Bar');
      end;
    kppSolidVert:
      begin
        StyleObject := Root.FindObjectByName('SolidVert');
        if StyleObject = nil then
          StyleObject := Root.FindObjectByName('Solid');
        if StyleObject = nil then
          StyleObject := Root.FindObjectByName('BarVert');
        if StyleObject = nil then
          StyleObject := Root.FindObjectByName('Bar');
      end;
    kppSolidHorz:
      begin
        StyleObject := Root.FindObjectByName('SolidHorz');
        if StyleObject = nil then
          StyleObject := Root.FindObjectByName('Solid');
        if StyleObject = nil then
          StyleObject := Root.FindObjectByName('BarHorz');
        if StyleObject = nil then
          StyleObject := Root.FindObjectByName('Bar');
      end;
  end;

  if StyleObject <> nil then
  begin
    StyleObject.BoundsRect := AProgress.Rect;
    StyleObject.SafeDraw(Canvas, NullRect);
  end;
end;

{ Track class =================================================================}

function TSeStyle.IsTrackDefined(const ASubclass: TSeTrackSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin
  Result := False;
  if AObject <> ktoDefault then
    Result := (FStyleSource.GetObjectByName(AObject) <> nil);
  if not Result then
    case ASubclass of
      ktscTrackBar: Result := (FObjects[soTrackBar] <> nil);
      ktscRangeBar: Result := (FObjects[soTrackBar] <> nil);
    else
      Result := False;
    end;
end;

function TSeStyle.IsTrackTransparent(const ASubclass: TSeTrackSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin
  Result := True;
end;

procedure TSeStyle.TrackDraw(const ASubclass: TSeTrackSubclass;
  Canvas: TCanvas; ATrack: TSeTrackInfo; ADPI: Integer = 0; const AObject: TSeStyleElementObject = ktoDefault);
begin
end;

procedure TSeStyle.TrackBarDraw(const ASubclass: TSeTrackSubclass;
  Canvas: TCanvas; ABar: TSeTrackBarInfo; ADPI: Integer = 0; const AObject: TSeStyleElementObject = ktoDefault);
var
  Root: TSeStyleObject;
  StyleObject: TSeStyleObject;
begin
  Root := nil;
  if AObject <> ktoDefault then
    Root := FStyleSource.GetObjectByName(AObject);
  if Root = nil then
    Root := FObjects[soTrackBar];
  if Root = nil then Exit;

  StyleObject := nil;
  case ABar.Bar of
    ktbcVertical:
      begin
        StyleObject := Root.FindObjectByName('TrackVert');
        if StyleObject = nil then
          StyleObject := Root.FindObjectByName('Track');
      end;
    ktbcHorizontal:
      begin
        StyleObject := Root.FindObjectByName('TrackHorz');
        if StyleObject = nil then
          StyleObject := Root.FindObjectByName('Track');
      end;
  end;

  if StyleObject <> nil then
  begin
    if ABar.DrawState = ktrbdsDisabled then
      StyleObject.State := ssDisabled
    else
      StyleObject.State := ssNormal;

    StyleObject.BoundsRect := ABar.Rect;
    StyleObject.Draw(Canvas, NullRect, ADPI);
  end
end;

procedure TSeStyle.TrackDrawThumb(const ASubclass: TSeTrackSubclass;
  Canvas: TCanvas; AThumb: TSeTrackThumbInfo; ADPI: Integer = 0;
  const AObject: TSeStyleElementObject = ktoDefault);
var
  Root: TSeStyleObject;
  StyleObject: TSeStyleObject;
  R: TRect;
begin
  Root := nil;
  if AObject <> ktoDefault then
    Root := FStyleSource.GetObjectByName(AObject);
  if Root = nil then
    Root := FObjects[soTrackBar];
  if Root = nil then Exit;

  StyleObject := nil;
  case AThumb.Thumb of
    ktbmVertBoth:
      begin
        StyleObject := Root.FindObjectByName('ThumbVertBoth');
        if StyleObject = nil then
          StyleObject := Root.FindObjectByName('ThumbVert');
      end;
    ktbmVertRight:
      begin
        StyleObject := Root.FindObjectByName('ThumbVertRight');
        if StyleObject = nil then
          StyleObject := Root.FindObjectByName('ThumbVert');
      end;
    ktbmVertLeft:
      begin
        StyleObject := Root.FindObjectByName('ThumbVertLeft');
        if StyleObject = nil then
          StyleObject := Root.FindObjectByName('ThumbVert');
      end;
    ktbmHorzBoth:
      begin
        StyleObject := Root.FindObjectByName('ThumbHorzBoth');
        if StyleObject = nil then
          StyleObject := Root.FindObjectByName('ThumbHorz');
      end;
    ktbmHorzBottom:
      begin
        StyleObject := Root.FindObjectByName('ThumbHorzBottom');
        if StyleObject = nil then
          StyleObject := Root.FindObjectByName('ThumbHorz');
      end;
    ktbmHorzTop:
      begin
        StyleObject := Root.FindObjectByName('ThumbHorzTop');
        if StyleObject = nil then
          StyleObject := Root.FindObjectByName('ThumbHorz');
      end;
  end;
  if StyleObject = nil then
    StyleObject := Root.FindObjectByName('Thumb');

  if StyleObject <> nil then
  begin
    case AThumb.DrawState of
      kttdsNormal: StyleObject.State := ssNormal;
      kttdsHot: StyleObject.State := ssHot;
      kttdsPressed: StyleObject.State := ssPressed;
      kttdsFocused: StyleObject.State := ssFocused;
      kttdsDisabled: StyleObject.State := ssDisabled;
    end;

    R := Rect(0, 0, StyleObject.Width, StyleObject.Height);
    RectCenter(R, AThumb.Rect);

    StyleObject.BoundsRect := R;
    StyleObject.Draw(Canvas, NullRect, ADPI);
  end
end;


{ Header class ================================================================}

function TSeStyle.IsHeaderDefined(const ASubclass: TSeHeaderSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin


  case ASubclass of
    khscHeader: Result := (FObjects[soHeader] <> nil);
  else
    Result := False;
  end;
end;

procedure TSeStyle.HeaderDraw(const ASubclass: TSeHeaderSubclass;
  Canvas: TCanvas; AHeader: TSeHeaderInfo; const AObject: TSeStyleElementObject);
var
  StyleObject: TSeStyleObject;
begin
  StyleObject := FObjects[soHeader].FindObjectByName('Frame');
  if StyleObject = nil then
    inherited
  else
  begin
    StyleObject.BoundsRect := AHeader.Rect;
    StyleObject.Draw(Canvas, NullRect);
  end;
end;

procedure TSeStyle.HeaderDrawSection(const ASubclass: TSeHeaderSubclass;
  Canvas: TCanvas; ASection: TSeHeaderSectionInfo;
  const AObject: TSeStyleElementObject);
var
  StyleObject: TSeStyleObject;
  StyleState: TSeState;
begin
  StyleObject := nil;
  if ASection.State = khssFirst then
    StyleObject := FObjects[soHeader].FindObjectByName('SectionFirst');
  if ASection.State = khssLast then
    StyleObject := FObjects[soHeader].FindObjectByName('SectionLast');
  if StyleObject = nil then
    StyleObject := FObjects[soHeader].FindObjectByName('Section');

  if StyleObject <> nil then
    with ASection do
    begin
      if DrawState in [khdsDraggedOut, khdsDisabled] then
        StyleState := ssDisabled
      else
        if DrawState in [khdsPressed, khdsUnderDrag] then
          StyleState := ssPressed
        else
          if DrawState in [khdsHot, khdsDragging] then
            StyleState := ssHot
          else
            StyleState := ssNormal;

      StyleObject.BoundsRect := Rect;
      StyleObject.State := StyleState;
      StyleObject.Draw(Canvas, NullRect);
    end;
end;

{ Status class ================================================================}

function TSeStyle.IsStatusDefined(const ASubclass: TSeStatusSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin
  case ASubclass of
    ksscStatusBar: Result := (FObjects[soStatus] <> nil);
  else
    Result := False;
  end;
end;

procedure TSeStyle.StatusDraw(const ASubclass: TSeStatusSubclass;
  Canvas: TCanvas; AStatus: TSeStatusInfo; ADPI: Integer = 0;  const AObject: TSeStyleElementObject = ktoDefault);
var
  StyleObject: TSeStyleObject;
begin
  StyleObject := FObjects[soStatus].FindObjectByName('Frame');
  if StyleObject <> nil then
  begin
    StyleObject.BoundsRect := AStatus.Rect;
    StyleObject.Draw(Canvas, NullRect);
  end;
end;

procedure TSeStyle.StatusDrawPanel(const ASubclass: TSeStatusSubclass;
  Canvas: TCanvas; APanel: TSeStatusPanelInfo; ADPI: Integer = 0;
  const AObject: TSeStyleElementObject = ktoDefault);
var
  StyleObject: TSeStyleObject;
  DrawState: TSeState;
begin
  case APanel.Panel of
    kspcNormal, kspcDisabled, kspcGripperPanel:
      begin
        StyleObject := FObjects[soStatus].FindObjectByName('Panel');
        if StyleObject <> nil then
          with APanel do
          begin
            DrawState := ssNormal;
            if APanel.Panel = kspcGripperPanel then
              Rect.Right := Rect.Right + 7;

            StyleObject.BoundsRect := Rect;
            StyleObject.State := DrawState;
            StyleObject.Draw(Canvas, NullRect);
          end;
      end;
    kspcGripper:
      begin
        StyleObject := FObjects[soStatus].FindObjectByName('Gripper');
        if StyleObject <> nil then
          with APanel do
          begin
            DrawState := ssNormal;
            StyleObject.BoundsRect := Rect;
            StyleObject.State := DrawState;
            StyleObject.Draw(Canvas, NullRect, ADPI);
          end;
      end;
  end;
end;

procedure TSeStyle.StatusDrawText(const ASubclass: TSeStatusSubclass; Canvas: TCanvas;
  APanel: TSeStatusPanelInfo; AText: TSeTextInfo;
  const AObject: TSeStyleElementObject);
var
  StyleObject: TSeStyleObject;
begin
  if AObject <> ktoDefault then
  begin
    StyleObject := FStyleSource.GetObjectByName(AObject);
    if StyleObject <> nil then
      StyleObject := StyleObject.FindObjectByName('Panel');
  end
  else
    StyleObject := FObjects[soStatus].FindObjectByName('Panel');

  if StyleObject <> nil then
    with StyleObject, AText do
    begin
      Inc(Rect.Left, TextMarginLeft);
      Dec(Rect.Right, TextMarginRight);
      Inc(Rect.Top, TextMarginTop);
    end;
  inherited;
end;

function TSeStyle.StatusGetClientRect(const ASubclass: TSeStatusSubclass;
  AStatus: TSeStatusInfo; const AObject: TSeStyleElementObject): TRect;
begin
  Result := inherited StatusGetClientRect(ASubclass, AStatus, AObject);
end;

{ Edit class ==================================================================}

function TSeStyle.IsEditDefined(const ASubclass: TSeEditSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin


  case ASubclass of
    kescEdit: Result := (FObjects[soEditBox] <> nil);
    kescComboBoxLeft, kescComboBoxRight: Result := (FObjects[soComboBox] <> nil);
  else
    Result := False;
  end;
end;

procedure TSeStyle.EditDraw(const ASubclass: TSeEditSubclass;
  Canvas: TCanvas; AEdit: TSeEditInfo; const AObject: TSeStyleElementObject);
var
  StyleObject: TSeStyleObject;
  StyleState: TSeState;
begin
  StyleObject := FObjects[soEditBox].FindObjectByName('Frame');
  if StyleObject <> nil then
  begin
    with AEdit do
    begin
      if DrawState = kedsDisabled then
        StyleState := ssDisabled
      else
        if DrawState = kedsHot then
          StyleState := ssHot
        else
          StyleState := ssNormal;
    end;
    StyleObject.State := StyleState;
    StyleObject.BoundsRect := AEdit.Rect;
    StyleObject.Draw(Canvas, NullRect);
  end;
end;

procedure TSeStyle.EditDrawButton(const ASubclass: TSeEditSubclass;
  Canvas: TCanvas; AButton: TSeEditButtonInfo; ADPI: Integer = 0;
  const AObject: TSeStyleElementObject = ktoDefault);
var
  StyleObject: TSeStyleObject;
  StyleState: TSeState;
begin
  with AButton do
  begin
    if DrawState = kebdsDisabled then
      StyleState := ssDisabled
    else
      if DrawState = kebdsPressed then
        StyleState := ssPressed
      else
        if DrawState = kebdsHot then
          StyleState := ssHot
        else
          StyleState := ssNormal;
  end;

  StyleObject := nil;
  case AButton.ButtonClass of
    kebcDropDown:
      if FObjects[soComboBox] <> nil then
        StyleObject := FObjects[soComboBox].FindObjectByName('Button');
    kebcUp: StyleObject := FObjects[soEditBox].FindObjectByName('Button');
    kebcDown: StyleObject := FObjects[soEditBox].FindObjectByName('Button');
    kebcLeft: StyleObject := FObjects[soEditBox].FindObjectByName('Button');
    kebcRight: StyleObject := FObjects[soEditBox].FindObjectByName('Button');
    kebcPlus: StyleObject := FObjects[soEditBox].FindObjectByName('Button');
    kebcMinus: StyleObject := FObjects[soEditBox].FindObjectByName('Button');
    kebcDialog: StyleObject := FObjects[soEditBox].FindObjectByName('Button');
    kebcCustom: StyleObject := FObjects[soEditBox].FindObjectByName('Button');
  end;

  if StyleObject <> nil then
  begin
    StyleObject.State := StyleState;
    StyleObject.BoundsRect := AButton.Rect;
    StyleObject.Draw(Canvas, NullRect, ADPI);
  end;
end;

function TSeStyle.EditGetClientRect(const ASubclass: TSeEditSubclass;
  Canvas: TCanvas; AEdit: TSeEditInfo;
  const AObject: TSeStyleElementObject): TRect;
begin
  Result := AEdit.Rect;
  InflateRect(Result, -4, -4);
end;

{ Group class =================================================================}

function TSeStyle.IsGroupDefined(const ASubclass: TSeGroupSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin
  case ASubclass of
    kgscGroupBox: Result := (FObjects[soGroupBox] <> nil);
  else
    Result := False;
  end;
end;

function TSeStyle.IsGroupTransparent(const ASubclass: TSeGroupSubclass;
  const AObject: TSeStyleElementObject): boolean;
var
  StyleObject: TSeStyleObject;
begin
  StyleObject := FObjects[soGroupBox].FindObjectByName('Frame');
  Result := StyleObject.Masked;
end;

procedure TSeStyle.GroupDraw(const ASubclass: TSeGroupSubclass;
  Canvas: TCanvas; AGroup: TSeGroupInfo; const AObject: TSeStyleElementObject);
var
  StyleObject: TSeStyleObject;
  B: TSeBitmap;
  R: TRect;
begin
  if FObjects[soGroupBox] = nil then Exit;
  with AGroup do
  begin
    StyleObject := FObjects[soGroupBox].FindObjectByName('Frame');
    if StyleObject = nil then
      StyleObject := FObjects[soGroupBox];

    if StyleObject <> nil then
    begin
      if DrawState = kgdsDisabled then
        StyleObject.State := ssDisabled
      else
        StyleObject.State := ssNormal;

      if IsRectEmpty(EraseRect) then
      begin
        { Draw groupbox}
        StyleObject.BoundsRect := Rect;
        StyleObject.Draw(Canvas, NullRect);
      end
      else
      begin
        { Draw with our erase rect }
        R := EraseRect;

        B := TSeBitmap.Create;
        try
          B.SetSize(R.Width, R.Height);
          BitBlt(B.Canvas.Handle, 0, 0, B.Width, B.Height, Canvas.Handle, R.Left, R.Top, SRCCOPY);

          { Draw groupbox}
          StyleObject.BoundsRect := Rect;
          StyleObject.Draw(Canvas, NullRect);

          { Draw caption background }
          BitBlt(Canvas.Handle, R.Left, R.Top, B.Width, B.Height, B.Canvas.Handle, 0, 0, SRCCOPY);
        finally
          B.Free;
        end;
      end;
    end;
  end;
end;

{ Memo class ==================================================================}

function TSeStyle.IsMemoDefined(const ASubclass: TSeMemoSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin
  case ASubclass of
    kmscMemo: Result := (FObjects[soEditBox] <> nil);
  else
    Result := False;
  end;
end;

procedure TSeStyle.MemoDraw(const ASubclass: TSeMemoSubclass;
  Canvas: TCanvas; AMemo: TSeMemoInfo; const AObject: TSeStyleElementObject);
var
  StyleObject: TSeStyleObject;
  StyleState: TSeState;
begin
  StyleObject := FObjects[soEditBox].FindObjectByName('Frame');
  if StyleObject <> nil then
  begin
    with AMemo do
    begin
      if DrawState = kmdsDisabled then
        StyleState := ssDisabled
      else
        StyleState := ssNormal;
    end;
    StyleObject.State := StyleState;
    StyleObject.BoundsRect := AMemo.Rect;
    StyleObject.Draw(Canvas, NullRect);
  end;
end;

function TSeStyle.MemoGetClientRect(const ASubclass: TSeMemoSubclass;
  Canvas: TCanvas; AMemo: TSeMemoInfo;
  const AObject: TSeStyleElementObject): TRect;
begin
  Result := AMemo.Rect;
  InflateRect(Result, -4, -4);
end;

{ Menu class ==================================================================}

function TSeStyle.IsMenuDefined(const ASubclass: TSeMenuSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin
  case ASubclass of
    kmscMenuBar: Result := (FObjects[soMenuBar] <> nil);
    kmscPopupMenu: Result := (FObjects[soPopupMenu] <> nil);
  else
    Result := False;
  end;
end;

procedure TSeStyle.MenuDraw(const ASubclass: TSeMenuSubclass;
  Canvas: TCanvas; AMenu: TSeMenuInfo; const AObject: TSeStyleElementObject);
var
  StyleObject, Root: TSeStyleObject;
begin
  Root := nil;
  StyleObject := nil;
  if AObject <> ktoDefault then
    Root := FStyleSource.GetObjectByName(AObject);

  if Root = nil then
    case ASubclass of
      kmscMenuBar: Root := FObjects[soMenuBar];
      kmscPopupMenu: Root := FObjects[soPopupMenu];
    end;

  if Root <> nil then
    case ASubclass of
      kmscMenuBar: StyleObject := Root.FindObjectByName('Frame');
      kmscPopupMenu:
        StyleObject := Root.FindObjectByName('Frame');
    else
      StyleObject := nil;
    end;

  if StyleObject <> nil then
  begin
    StyleObject.BoundsRect := AMenu.Rect;
    StyleObject.Draw(Canvas, NullRect);
  end;
end;

procedure TSeStyle.MenuDrawItem(const ASubclass: TSeMenuSubclass;
  Canvas: TCanvas; AItem: TSeMenuItemInfo; ADPI: Integer = 0; const AObject: TSeStyleElementObject = ktoDefault);
var
  Root, StyleObject: TSeStyleObject;
  StyleState: TSeState;
  Color: TColor;
  R: TRect;
begin
  Root := FObjects[soMenuBar];
  case ASubclass of
    kmscMenuBar: Root := FObjects[soMenuBar];
    kmscPopupMenu: Root := FObjects[soPopupMenu];
  end;

  if Root = nil then
    Exit;

  with AItem do
  begin
    case Item of
      kmicNormal:
        begin
          if DrawState = kmidsDisabled then
            StyleState := ssDisabled
          else
            if DrawState = kmidsSelected then
              StyleState := ssFocused
            else
              if DrawState = kmidsHot then
                StyleState := ssHot
              else
                StyleState := ssNormal;

          case ASubclass of
            kmscMenuBar:
              begin
                StyleObject := Root.FindObjectByName(AObject);
                if StyleObject = nil then
                  StyleObject := Root.FindObjectByName('Item');
              end;
            kmscPopupMenu:
              begin
                StyleObject := Root.FindObjectByName(AObject);
                if StyleObject = nil then
                  StyleObject := Root.FindObjectByName('Item');
              end;
          else
            StyleObject := nil;
          end;
          if StyleObject = nil then Exit;

          StyleObject.BoundsRect := Rect;
          StyleObject.State := StyleState;
          StyleObject.Draw(Canvas, NullRect);
        end;
      kmicHomeButton, kmicendButton:
        begin
          if DrawState = kmidsDisabled then
            StyleState := ssDisabled
          else
            if DrawState = kmidsSelected then
              StyleState := ssFocused
            else
              if DrawState = kmidsHot then
                StyleState := ssHot
              else
                StyleState := ssNormal;

          case ASubclass of
            kmscMenuBar:
              begin
                StyleObject := Root.FindObjectByName('item');
              end;
            kmscPopupMenu:
              begin
                if Item = kmicHomeButton then
                  StyleObject := Root.FindObjectByName('upbutton')
                else
                  StyleObject := Root.FindObjectByName('downbutton');
                if StyleObject = nil then
                  StyleObject := Root.FindObjectByName('item');
              end;
          else
            StyleObject := nil;
          end;
          if StyleObject = nil then Exit;

          StyleObject.BoundsRect := Rect;
          StyleObject.State := StyleState;
          StyleObject.Draw(Canvas, NullRect);

          if Item in [kmicHomeButton, kmicEndButton] then
          begin
            if DrawState = kmidsDisabled then
              Color := Fonts[ktfMenuItemTextDisabled].Color
            else
              if DrawState = kmidsSelected then
                Color := Fonts[ktfMenuItemTextSelected].Color
              else
                if DrawState = kmidsHot then
                  Color := Fonts[ktfMenuItemTextHot].Color
                else
                  Color := Fonts[ktfMenuItemTextNormal].Color;

            R := TRect.Create(0, 0, 7, 4);
            RectCenter(R, Rect);

            Canvas.Pen.Style := psSolid;
            Canvas.Pen.Color := Color;
            Canvas.Brush.Style := bsSolid;
            Canvas.Brush.Color := Color;
            if Item = kmicHomeButton then
              with R do
                Canvas.Polygon([Point(Left, Bottom - 1),
                  Point(Right - 1, Bottom - 1),
                  Point((Right - 1 + Left) div 2, Top)])
            else
              with R do
                Canvas.Polygon([Point(Left, Top),
                  Point(Right - 1, Top),
                  Point((Right - 1 + Left) div 2, Bottom - 1)]);
          end;
        end;
      kmicSeparator:
        begin
          case ASubclass of
            kmscMenuBar:
              begin
                StyleObject := Root.FindObjectByName('separator');
                if StyleObject = nil then
                  StyleObject := Root.FindObjectByName('item');
              end;
            kmscPopupMenu:
              begin
                StyleObject := Root.FindObjectByName(AObject);
                if StyleObject = nil then
                  StyleObject := Root.FindObjectByName('Separator');
                if StyleObject = nil then
                  StyleObject := Root.FindObjectByName('item');
              end;
          else
            StyleObject := nil;
          end;
          if StyleObject <> nil then
          begin
            StyleObject.BoundsRect := Rect;
            StyleObject.State := ssNormal;
            StyleObject.Draw(Canvas, NullRect);
          end;
        end;
      kmicMin:
        begin
          StyleObject := Root.FindObjectByName('Min');
          if StyleObject = nil then
            DrawFrameControlGlyph(Canvas, Rect, DFC_CAPTION, DFCS_CAPTIONMIN, clBlack)
          else
          begin
            if AItem.DrawState = kmidsDisabled then
              StyleState := ssDisabled
            else
              if AItem.DrawState = kmidsSelected then
                StyleState := ssFocused
              else
                if AItem.DrawState = kmidsHot then
                  StyleState := ssHot
                else
                  StyleState := ssNormal;

            R := TRect.Create(0, 0, StyleObject.Width, StyleObject.Height);
            RectCenter(R, Rect);
            StyleObject.State := StyleState;
            StyleObject.BoundsRect := R;
            StyleObject.Draw(Canvas, NullRect, ADPI);
          end;
        end;
      kmicRestore:
        begin
          StyleObject := Root.FindObjectByName('Restore');
          if StyleObject = nil then
            DrawFrameControlGlyph(Canvas, Rect, DFC_CAPTION, DFCS_CAPTIONRESTORE, clBlack)
          else
          begin
            if AItem.DrawState = kmidsDisabled then
              StyleState := ssDisabled
            else
              if AItem.DrawState = kmidsSelected then
                StyleState := ssFocused
              else
                if AItem.DrawState = kmidsHot then
                  StyleState := ssHot
                else
                  StyleState := ssNormal;

            R := TRect.Create(0, 0, StyleObject.Width, StyleObject.Height);
            RectCenter(R, Rect);
            StyleObject.State := StyleState;
            StyleObject.BoundsRect := R;
            StyleObject.Draw(Canvas, NullRect, ADPI);
          end;
        end;
      kmicClose:
        begin
          StyleObject := Root.FindObjectByName('Close');
          if StyleObject = nil then
            DrawFrameControlGlyph(Canvas, Rect, DFC_CAPTION, DFCS_CAPTIONCLOSE, clBlack)
          else
          begin
            if AItem.DrawState = kmidsDisabled then
              StyleState := ssDisabled
            else
              if AItem.DrawState = kmidsSelected then
                StyleState := ssFocused
              else
                if AItem.DrawState = kmidsHot then
                  StyleState := ssHot
                else
                  StyleState := ssNormal;

            R := TRect.Create(0, 0, StyleObject.Width, StyleObject.Height);
            RectCenter(R, Rect);
            StyleObject.State := StyleState;
            StyleObject.BoundsRect := R;
            StyleObject.Draw(Canvas, NullRect, ADPI);
          end;
        end;
    end;
  end;
end;

procedure TSeStyle.MenuDrawItemGlyph(const ASubclass: TSeMenuSubclass;
  Canvas: TCanvas; AItem: TSeMenuItemInfo; AGlyph: TSeMenuGlyphInfo;
  ADPI: Integer = 0;  const AObject: TSeStyleElementObject = ktoDefault);
var
  R: TRect;
begin
  inherited;
  with AGlyph do
  begin
    case Kind of
      kmgSysMenu: if Glyph <> nil then
        begin
          R := TRect.Create(0, 0, Glyph.Width, Glyph.Height);
          RectCenter(R, Rect);
          Glyph.Draw(Canvas, R);
        end;
    end
  end;
end;

procedure TSeStyle.MenuDrawItemText(const ASubclass: TSeMenuSubclass;
  Canvas: TCanvas; AItem: TSeMenuItemInfo; AText: TSeTextInfo;
  const AObject: TSeStyleElementObject);
var
  LColor: TColor;
begin
  LColor := Canvas.Font.Color;
  try
    if ASubClass = kmscPopupMenu then
    begin
      case AItem.DrawState of
        kmidsNormal: Canvas.Font.Color := Fonts[ktfPopupMenuItemTextNormal].Color;
        kmidsHot: Canvas.Font.Color := Fonts[ktfPopupMenuItemTextHot].Color;
        kmidsSelected: Canvas.Font.Color := Fonts[ktfPopupMenuItemTextSelected].Color;
        kmidsDisabled: Canvas.Font.Color := Fonts[ktfPopupMenuItemTextDisabled].Color;
      else
        Canvas.Font.Color := Fonts[ktfPopupMenuItemTextNormal].Color;
      end;
    end
    else
    case AItem.DrawState of
      kmidsNormal: Canvas.Font.Color := Fonts[ktfMenuItemTextNormal].Color;
      kmidsHot: Canvas.Font.Color := Fonts[ktfMenuItemTextHot].Color;
      kmidsSelected: Canvas.Font.Color := Fonts[ktfMenuItemTextSelected].Color;
      kmidsDisabled: Canvas.Font.Color := Fonts[ktfMenuItemTextDisabled].Color;
    else
      Canvas.Font.Color := Fonts[ktfMenuItemTextNormal].Color;
    end;
    inherited;
  finally
    Canvas.Font.Color := LColor;
  end;
end;

function TSeStyle.MenuGetClientRect(const ASubclass: TSeMenuSubclass;
  AMenu: TSeMenuInfo; const AObject: TSeStyleElementObject): TRect;
var
  StyleObject: TSeStyleObject;
begin

  case ASubclass of
    kmscMenuBar: StyleObject := FObjects[soMenuBar].FindObjectByName('Frame');
    kmscPopupMenu: StyleObject := FObjects[soPopupMenu].FindObjectByName('Frame');
  else
    StyleObject := nil;
  end;

  Result := AMenu.Rect;
  if StyleObject <> nil then
  begin
    StyleObject.BoundsRect := AMenu.Rect;
    with StyleObject, Result do
      Result := Rect(Left + MarginLeft, Top + MarginTop, Right - MarginRight,
        Bottom - MarginBottom);
  end;
end;

{ Dock class ==================================================================}

function TSeStyle.IsDockDefined(const ASubclass: TSeDockSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin

  if AObject <> ktoDefault then
    Result := (FStyleSource.GetObjectByName(AObject) <> nil)
  else
    Result := (FObjects[soControlBar] <> nil);
end;

procedure TSeStyle.DockDraw(const ASubclass: TSeDockSubclass;
  Canvas: TCanvas; ADock: TSeDockInfo; ADPI: Integer = 0; const AObject: TSeStyleElementObject = ktoDefault);
var
  StyleObject: TSeStyleObject;
begin
  StyleObject := nil;
  if (AObject <> ktoDefault) then
    StyleObject := FStyleSource.GetObjectByName(AObject);
  if StyleObject = nil then
    StyleObject := FObjects[soControlBar];
  if StyleObject <> nil then
    StyleObject := StyleObject.FindObjectByName('Back');

  if StyleObject <> nil then
  begin
    StyleObject.BoundsRect := ADock.Rect;
    StyleObject.Draw(Canvas, NullRect);
  end;
end;

function TSeStyle.DockGetClientRect(const ASubclass: TSeDockSubclass;
  ADock: TSeDockInfo; const AObject: TSeStyleElementObject): TRect;
begin
  Result := ADock.Rect;
end;

{ Tool class ==================================================================}

function TSeStyle.IsToolDefined(const ASubclass: TSeToolSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin

  if AObject <> ktoDefault then
    Result := (FStyleSource.GetObjectByName(AObject) <> nil)
  else
    case ASubclass of
      ktscToolMenu: Result := (FObjects[soControlBar] <> nil);
      ktscToolBar: Result := (FObjects[soControlBar] <> nil);
    else
      Result := False;
    end;
end;


procedure TSeStyle.ToolDraw(const ASubclass: TSeToolSubclass;
  Canvas: TCanvas; ATool: TSeToolInfo; ADPI: Integer = 0; const AObject: TSeStyleElementObject = ktoDefault);
var
  BarObject, StyleObject: TSeStyleObject;
begin
  if ASubclass = ktscToolPopup then
  begin
    MenuDraw(kmscPopupMenu, Canvas, MenuInfo(ATool.Rect));
    Exit;
  end;

  StyleObject := nil;
  BarObject := nil;
  if (AObject <> ktoDefault) then
    BarObject := FStyleSource.GetObjectByName(AObject);
  if BarObject = nil then
    BarObject := FObjects[soControlBar];

  if BarObject <> nil then
    StyleObject := BarObject.FindObjectByName('Frame');

  if StyleObject <> nil then
  begin
    StyleObject.BoundsRect := ATool.Rect;
    StyleObject.Draw(Canvas, NullRect);
  end;
end;

procedure TSeStyle.ToolDrawItem(const ASubclass: TSeToolSubclass;
  Canvas: TCanvas; AItem: TSeToolItemInfo; ADPI: Integer = 0; const AObject: TSeStyleElementObject = ktoDefault);
var
  R: TRect;
  BarObject, StyleObject: TSeStyleObject;
  StyleState: TSeState;
begin
  BarObject := nil;

  case AItem.Item of
    kticNormal, kticComboButton, kticDropDown:
      with AItem do
      begin
        R := Rect;
        StyleObject := nil;

        if AObject <> ktoDefault then
          StyleObject := FStyleSource.GetObjectByName(AObject);

        if StyleObject = nil then
          StyleObject := FObjects[soSpeedButton];
        if StyleObject = nil then Exit;

        if StyleObject.FindObjectByName('FlatFace') <> nil then
           StyleObject := StyleObject.FindObjectByName('FlatFace')
        else
          if StyleObject.FindObjectByName('Face') <> nil then
             StyleObject := StyleObject.FindObjectByName('Face');
        if StyleObject = nil then Exit;

        if DrawState in [mtidsDisabled] then
          StyleState := ssDisabled
        else
          if DrawState in [mtidsPressed] then
            StyleState := ssPressed
          else
            if DrawState in [mtidsHot] then
              StyleState := ssHot
            else
              if DrawState in [mtidsChecked, mtidsCheckedHot] then
                StyleState := ssPressed
              else
                StyleState := ssNormal;

        { Draw face }
        StyleObject.State := StyleState;
        StyleObject.BoundsRect := R;
        StyleObject.Draw(Canvas, NullRect);
      end;
    kticSeparator:
      with AItem do
      begin
        R := Rect;
        InflateRect(R, 0, -2);
        StyleObject := nil;

        if AObject <> ktoDefault then
          StyleObject := FStyleSource.GetObjectByName(AObject);

        if StyleObject = nil then
          StyleObject := FObjects[soSpeedButton];
        if StyleObject = nil then Exit;

        if StyleObject.FindObjectByName('Separator') <> nil then
        begin
          StyleObject := StyleObject.FindObjectByName('Separator');
          { Draw face }
          StyleObject.State := ssNormal;
          StyleObject.BoundsRect := R;
          StyleObject.Draw(Canvas, NullRect);
        end;
      end;
    kticGripper:
      begin
        StyleObject := nil;
        BarObject := nil;
        if (AObject <> ktoDefault) then
          BarObject := FStyleSource.GetObjectByName(AObject);
        if BarObject = nil then
          BarObject := FObjects[soControlBar];
        if BarObject <> nil then
          StyleObject := BarObject.FindObjectByName('Grabber');
        if StyleObject <> nil then
        begin
          StyleObject.BoundsRect := AItem.Rect;
          StyleObject.Draw(Canvas, NullRect, ADPI);
        end;
      end;
    kticGripperVert:
      begin
        StyleObject := nil;
        if (AObject <> ktoDefault) then
          BarObject := FStyleSource.GetObjectByName(AObject);
        if BarObject = nil then
          BarObject := FObjects[soControlBar];
        if BarObject <> nil then
        begin
          StyleObject := BarObject.FindObjectByName('GrabberVert');
          if StyleObject = nil then
            StyleObject := BarObject.FindObjectByName('Grabber');
        end;

        if StyleObject <> nil then
        begin
          StyleObject.BoundsRect := AItem.Rect;
          StyleObject.Draw(Canvas, NullRect);
        end;
      end;
    kticSpace: ;
    kticComboDropDown:
      with AItem do
      begin
        R := Rect;
        StyleObject := nil;

        if AObject <> ktoDefault then
          StyleObject := FStyleSource.GetObjectByName(AObject);

        if StyleObject = nil then
          StyleObject := FObjects[soSpeedButton];
        if StyleObject = nil then Exit;

        if StyleObject.FindObjectByName('FlatChevron') <> nil then
           StyleObject := StyleObject.FindObjectByName('FlatChevron')
        else
          if StyleObject.FindObjectByName('Chevron') <> nil then
             StyleObject := StyleObject.FindObjectByName('Chevron');

        if StyleObject = nil then
        begin
          StyleObject := FObjects[soSpeedButton];
          if StyleObject.FindObjectByName('FlatFace') <> nil then
             StyleObject := StyleObject.FindObjectByName('FlatFace')
          else
            if StyleObject.FindObjectByName('Face') <> nil then
               StyleObject := StyleObject.FindObjectByName('Face');
          if StyleObject = nil then Exit;
        end;

        if DrawState in [mtidsDisabled] then
          StyleState := ssDisabled
        else
          if DrawState in [mtidsPressed] then
            StyleState := ssPressed
          else
            if DrawState in [mtidsHot] then
              StyleState := ssHot
            else
              if DrawState in [mtidsChecked, mtidsCheckedHot] then
                StyleState := ssPressed
              else
                StyleState := ssNormal;

        StyleObject.State := StyleState;
        StyleObject.BoundsRect := R;
        StyleObject.Draw(Canvas, NullRect, ADPI);
      end;
    kticHomeButton: ;
    kticEndButton: ;
    kticExpandButton: ;
    kticSysMenu: ;
    kticMin: ;
    kticRestore: ;
    kticClose: ;
  end;
end;

{ BarDock class ===============================================================}

function TSeStyle.IsBarDockDefined(const ASubclass: TSeBarDockSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin
  case ASubclass of
    kbdscBarDock:
      begin
        Result := False;
        if AObject <> ktoDefault then
          Result := (FStyleSource.GetObjectByName(AObject) <> nil);
        if not Result then
          Result := FObjects[soBarDock] <> nil;
      end;
  else
    Result := False;
  end;
end;

procedure TSeStyle.BarDockDraw(const ASubclass: TSeBarDockSubclass;
  Canvas: TCanvas; ADock: TSeBarDockInfo; const AObject: TSeStyleElementObject);
var
  StyleObject: TSeStyleObject;
begin
  StyleObject := nil;
  if AObject <> ktoDefault then
  begin
    StyleObject := FStyleSource.GetObjectByName(AObject);
    if StyleObject <> nil then
      StyleObject := StyleObject.FindObjectByName('Frame')
  end;
  if StyleObject = nil then
    StyleObject := FObjects[soBarDock].FindObjectByName('Frame');
  if StyleObject <> nil then
  begin
    StyleObject.BoundsRect := ADock.Rect;
    StyleObject.Draw(Canvas, NullRect);
  end;
end;

function TSeStyle.BarDockGetClientRect(const ASubclass: TSeBarDockSubclass; Canvas: TCanvas;
  ADock: TSeBarDockInfo; const AObject: TSeStyleElementObject): TRect;
begin
  Result := ADock.Rect;
end;

{ Bar class ===================================================================}

function TSeStyle.IsBarDefined(const ASubclass: TSeBarSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin
  case ASubclass of
    kbscOutlookBar,
    kbscGroupBar: Result := False;
    kbscExplorerBar:
      begin
        Result := False;
        if AObject <> ktoDefault then
          Result := (FStyleSource.GetObjectByName(AObject) <> nil);
        if not Result then
          Result := FObjects[soHeaderPanel] <> nil;
      end;
  else
    Result := False;
  end;
end;

function TSeStyle.IsBarTransparent(const ASubclass: TSeBarSubclass;
  const AObject: TSeStyleElementObject): boolean;
var
  StyleObject: TSeStyleObject;
begin
  case ASubclass of
    kbscOutlookBar,
    kbscGroupBar: Result := False;
    kbscExplorerBar:
      begin
        Result := False;
        StyleObject := nil;
        if AObject <> ktoDefault then
        begin
          StyleObject := FStyleSource.GetObjectByName(AObject);
          if StyleObject <> nil then
            Result := StyleObject.Masked;
        end;
        if (StyleObject = nil) and (FObjects[soHeaderPanel] <> nil) then
        begin
          if FObjects[soHeaderPanel].FindObjectByName('Caption') <> nil then
            Result := FObjects[soHeaderPanel].FindObjectByName('Caption').Masked;
        end;
      end;
  else
    Result := False;
  end;
end;

function TSeStyle.BarGetClientRect(const ASubclass: TSeBarSubclass;
  Canvas: TCanvas; ABar: TSeBarInfo; const AObject: TSeStyleElementObject): TRect;
var
  StyleObject: TSeStyleObject;
begin
  Result := ABar.Rect;

  StyleObject := nil;
  if AObject <> ktoDefault then
  begin
    StyleObject := FStyleSource.GetObjectByName(AObject);
    if StyleObject <> nil then
    begin
      if not ABar.HaveBorder and not ABar.HaveCaption then
        StyleObject := StyleObject.FindObjectByName('FrameNoBorderCaption')
      else
        if not ABar.HaveBorder and ABar.HaveCaption  then
          StyleObject := StyleObject.FindObjectByName('FrameNoBorder')
        else
          if ABar.HaveBorder and not ABar.HaveCaption  then
            StyleObject := StyleObject.FindObjectByName('FrameNoCaption')
          else
            StyleObject := StyleObject.FindObjectByName('Frame')
    end;
  end;
  if (StyleObject = nil) and (FObjects[soHeaderPanel] <> nil) then
  begin
    if not ABar.HaveBorder and not ABar.HaveCaption then
      StyleObject := FObjects[soHeaderPanel].FindObjectByName('FrameNoBorderCaption')
    else
      if not ABar.HaveBorder and ABar.HaveCaption  then
        StyleObject := FObjects[soHeaderPanel].FindObjectByName('FrameNoBorder')
      else
        if ABar.HaveBorder and not ABar.HaveCaption  then
          StyleObject := FObjects[soHeaderPanel].FindObjectByName('FrameNoCaption');

    if StyleObject = nil then
      StyleObject := FObjects[soHeaderPanel].FindObjectByName('Frame');
  end;

  if StyleObject <> nil then
  begin
    Result := ABar.Rect;
    StyleObject.BoundsRect := ABar.Rect;
    with StyleObject do
      Result := Rect(MarginLeft, MarginTop, Width - MarginRight,
        Height - MarginBottom);
  end;
end;

procedure TSeStyle.BarDraw(const ASubclass: TSeBarSubclass;
  Canvas: TCanvas; ABar: TSeBarInfo; const AObject: TSeStyleElementObject);
var
  StyleObject: TSeStyleObject;
begin

  StyleObject := nil;
  if AObject <> ktoDefault then
  begin
    StyleObject := FStyleSource.GetObjectByName(AObject);
    if StyleObject <> nil then
    begin
      if not ABar.HaveBorder and not ABar.HaveCaption then
        StyleObject := StyleObject.FindObjectByName('FrameNoBorderCaption')
      else
        if not ABar.HaveBorder and ABar.HaveCaption  then
          StyleObject := StyleObject.FindObjectByName('FrameNoBorder')
        else
          if ABar.HaveBorder and not ABar.HaveCaption  then
            StyleObject := StyleObject.FindObjectByName('FrameNoCaption')
          else
            StyleObject := StyleObject.FindObjectByName('Frame')
    end;
  end;
  if (StyleObject = nil) and (FObjects[soHeaderPanel] <> nil) then
  begin
    if not ABar.HaveBorder and not ABar.HaveCaption then
      StyleObject := FObjects[soHeaderPanel].FindObjectByName('FrameNoBorderCaption')
    else
      if not ABar.HaveBorder and ABar.HaveCaption  then
        StyleObject := FObjects[soHeaderPanel].FindObjectByName('FrameNoBorder')
      else
        if ABar.HaveBorder and not ABar.HaveCaption  then
          StyleObject := FObjects[soHeaderPanel].FindObjectByName('FrameNoCaption');

    if StyleObject = nil then
      StyleObject := FObjects[soHeaderPanel].FindObjectByName('Frame');
  end;

  if StyleObject <> nil then
  begin
    StyleObject.BoundsRect := ABar.Rect;
    StyleObject.Draw(Canvas, NullRect);
  end;
end;

procedure TSeStyle.BarDrawCaption(const ASubclass: TSeBarSubclass;
  Canvas: TCanvas; ABar: TSeBarInfo; ACaption: TSeBarCaptionInfo;
  const AObject: TSeStyleElementObject);
var
  StyleObject: TSeStyleObject;
begin

  StyleObject := nil;
  if AObject <> ktoDefault then
  begin
    StyleObject := FStyleSource.GetObjectByName(AObject);
    if StyleObject <> nil then
    begin
      if not ABar.HaveBorder then
        StyleObject := StyleObject.FindObjectByName('CaptionNoBorder')
      else
        StyleObject := StyleObject.FindObjectByName('Caption');
    end;
  end;
  if (StyleObject = nil) and (FObjects[soHeaderPanel] <> nil) then
  begin
    if not ABar.HaveBorder then
      StyleObject := FObjects[soHeaderPanel].FindObjectByName('CaptionNoBorder');
    if StyleObject = nil then
      StyleObject := FObjects[soHeaderPanel].FindObjectByName('Caption');
  end;
  if StyleObject <> nil then
  begin
    StyleObject.BoundsRect := ACaption.Rect;
    StyleObject.Draw(Canvas, NullRect);
    StyleObject.Text := '';
  end;
end;

procedure TSeStyle.BarDrawButton(const ASubclass: TSeBarSubclass;
  Canvas: TCanvas; AButton: TSeBarButtonInfo;
  const AObject: TSeStyleElementObject);
var
  StyleObject: TSeStyleObject;
  StyleState: TSeState;
begin
  StyleObject := nil;
  if AObject <> ktoDefault then
    StyleObject := FStyleSource.GetObjectByName(AObject);
  if StyleObject = nil then
    StyleObject := FObjects[soHeaderPanel];

  case AButton.Button of
    kbbcClose: StyleObject := StyleObject.FindObjectByName('ButtonHide');
    kbbcUp: StyleObject := StyleObject.FindObjectByName('ButtonRollup');
    kbbcDown: StyleObject := StyleObject.FindObjectByName('ButtonRollDown');
  else
    StyleObject := nil;
  end;

  if StyleObject <> nil then
  begin
    if AButton.DrawState = kbbdsDisabled then
      StyleState := ssDisabled
    else
      if AButton.DrawState = kbbdsPressed then
        StyleState := ssPressed
      else
        if AButton.DrawState = kbbdsHot then
          StyleState := ssHot
        else
          StyleState := ssNormal;

    StyleObject.State := StyleState;
    StyleObject.BoundsRect := AButton.Rect;
    StyleObject.Draw(Canvas, NullRect);
  end;
end;

{ Tab class ===================================================================}

function TSeStyle.IsTabDefined(const ASubclass: TSeTabSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin
  Result := False;
  if AObject <> ktoDefault then
    Result := FStyleSource.GetObjectByName(AObject) <> nil;

  if not Result then
    case ASubclass of
      ktscTab, ktscPage: Result := FObjects[soTab] <> nil;
    else
      Result := False;
    end;
end;

procedure TSeStyle.TabDraw(const ASubclass: TSeTabSubclass;
  Canvas: TCanvas; ATab: TSeTabInfo; const AObject: TSeStyleElementObject);
var
  StyleObject: TSeStyleObject;
begin
  StyleObject := nil;
  if AObject <> ktoDefault then
  begin
    StyleObject := FStyleSource.GetObjectByName(AObject);
    if StyleObject <> nil then
      StyleObject := StyleObject.FindObjectByName('Frame');
  end;

  if StyleObject = nil then
    StyleObject := FObjects[soTab].FindObjectByName('Frame');

  if StyleObject <> nil then
    with ATab do
    begin
      StyleObject.BoundsRect := Rect;
      StyleObject.Draw(Canvas, NullRect);
    end;
end;

procedure TSeStyle.TabDrawButton(const ASubclass: TSeTabSubclass;
  Canvas: TCanvas; AButton: TSeTabButtonInfo;
  const AObject: TSeStyleElementObject);
var
  StyleObject: TSeStyleObject;
  StyleState: TSeState;
begin
  StyleObject := nil;
  if AObject <> ktoDefault then
  begin
    StyleObject := FStyleSource.GetObjectByName(AObject);
    if StyleObject <> nil then
    begin
      if AButton.Button = ktbcLeft then
        StyleObject := StyleObject.FindObjectByName('LeftButton')
      else
        StyleObject := StyleObject.FindObjectByName('RightButton');
    end;
  end;

  if StyleObject = nil then
  begin
    if AButton.Button = ktbcLeft then
      StyleObject := FObjects[soTab].FindObjectByName('LeftButton')
    else
      StyleObject := FObjects[soTab].FindObjectByName('RightButton');
  end;

  if StyleObject <> nil then
    with AButton do
    begin
      if DrawState = ktbdsDisabled then
        StyleState := ssDisabled
      else
        if DrawState = ktbdsPressed then
          StyleState := ssPressed
        else
          if DrawState = ktbdsHot then
            StyleState := ssHot
          else
            StyleState := ssNormal;

      StyleObject.BoundsRect := Rect;
      StyleObject.State := StyleState;
      StyleObject.Draw(Canvas, NullRect);
    end;
end;

procedure TSeStyle.TabDrawItem(const ASubclass: TSeTabSubclass;
  Canvas: TCanvas; AItem: TSeTabItemInfo;
  const AObject: TSeStyleElementObject);
var
  Root, StyleObject: TSeStyleObject;
  StyleState: TSeState;
  NeedRotate: boolean;
  Buffer, RotateBuffer: TSeBitmap;
  BufferCanvas: TCanvas;
  BufferRect: TRect;
begin

  StyleObject := nil;
  NeedRotate := False;
  Root := nil;
  if AObject <> ktoDefault then
    Root := FStyleSource.GetObjectByName(AObject);

  if Root = nil then
    Root := FObjects[soTab];

  if Root = nil then Exit;

  case AItem.Item of
    ktisTopMiddle: StyleObject := Root.FindObjectByName('TabTop');
    ktisTopFirst: StyleObject := Root.FindObjectByName('TabTop');
    ktisTopLast: StyleObject := Root.FindObjectByName('TabTop');
    ktisLeftMiddle: StyleObject := Root.FindObjectByName('TabLeft');
    ktisLeftFirst: StyleObject := Root.FindObjectByName('TabLeft');
    ktisLeftLast: StyleObject := Root.FindObjectByName('TabLeft');
    ktisRightMiddle: StyleObject := Root.FindObjectByName('TabRight');
    ktisRightFirst: StyleObject := Root.FindObjectByName('TabRight');
    ktisRightLast: StyleObject := Root.FindObjectByName('TabRight');
    ktisBottomMiddle: StyleObject := Root.FindObjectByName('TabTop');
    ktisBottomFirst: StyleObject := Root.FindObjectByName('TabTop');
    ktisBottomLast: StyleObject := Root.FindObjectByName('TabTop');
  end;
  if StyleObject = nil then
  begin
    if (AItem.DrawState in [ktidsActive, ktidsHot]) then
      StyleObject := Root.FindObjectByName('TabActive');
    if StyleObject = nil then
      StyleObject := Root.FindObjectByName('Tab');
    NeedRotate := True;
  end;

  if StyleObject <> nil then
    with AItem do
    begin
      if DrawState = ktidsDisabled then
        StyleState := ssDisabled
      else
        if (DrawState = ktidsActive) or (DrawState = ktidsActiveHot) then
          StyleState := ssFocused
        else
          if DrawState = ktidsHot then
            StyleState := ssHot
          else
            StyleState := ssNormal;

      if NeedRotate and (AItem.Item in [ktisLeftMiddle, ktisLeftFirst, ktisLeftLast, ktisRightMiddle,
         ktisRightFirst, ktisRightLast, ktisBottomMiddle, ktisBottomFirst, ktisBottomLast]) then
      begin
        StyleObject.BoundsRect := Rect;
        StyleObject.State := StyleState;

        Buffer := TSeBitmap.Create;
        try
          case AItem.Item of
            ktisLeftMiddle, ktisLeftFirst, ktisLeftLast:
              begin
                Buffer.SetSize(Rect.Height, Rect.Width);
              end;
            ktisBottomMiddle, ktisBottomFirst, ktisBottomLast:
              begin
                Buffer.SetSize(Rect.Width, Rect.Height);
              end;
            ktisRightMiddle, ktisRightFirst, ktisRightLast:
              begin
                Buffer.SetSize(Rect.Height, Rect.Width);
              end;
          end;
          with Buffer, Buffer.Canvas do
          begin
            Brush.Color := SysColors[clBtnFace];
            FillRect(System.Classes.Rect(0, 0, Width, Height));
          end;
          BufferCanvas := TCanvas.Create;
          try
            BufferCanvas.Handle := Buffer.Canvas.Handle;
            BufferRect := StyleObject.BoundsRect;
            StyleObject.BoundsRect := TRect.Create(0, 0, Buffer.Width, Buffer.Height);
            StyleObject.Draw(BufferCanvas, NullRect);
            StyleObject.BoundsRect := BufferRect;
            BufferCanvas.Handle := 0;
            RotateBuffer := TSeBitmap.Create;
            try
              case AItem.Item of
                ktisLeftMiddle, ktisLeftFirst, ktisLeftLast:
                  begin
                    RotateBuffer.SetSize(Buffer.Height, Buffer.Width);
                    Buffer.Rotate90_1(RotateBuffer);
                  end;
                ktisBottomMiddle, ktisBottomFirst, ktisBottomLast:
                  begin
                    RotateBuffer.Assign(Buffer);
                    RotateBuffer.FlipVert;
                  end;
                ktisRightMiddle, ktisRightFirst, ktisRightLast:
                  begin
                   RotateBuffer.SetSize(Buffer.Height, Buffer.Width);
                   Buffer.Rotate90_2(RotateBuffer);
                  end;
              end;
              RotateBuffer.Draw(Canvas, Rect.Left, Rect.Top);
            finally
              RotateBuffer.Free;
            end;
          finally
            BufferCanvas.Free;
          end;
        finally
          Buffer.Free;
        end;
      end
      else
      begin
        StyleObject.BoundsRect := Rect;
        StyleObject.State := StyleState;
        StyleObject.Draw(Canvas, NullRect);
      end;
    end;
end;

procedure TSeStyle.TabDrawText(const ASubclass: TSeTabSubclass;
  Canvas: TCanvas; AItem: TSeTabItemInfo;
  AText: TSeTextInfo; const AObject: TSeStyleElementObject);
begin
  if AItem.Item in [ktisLeftMiddle, ktisLeftFirst, ktisLeftLast] then
    AText.Orientation := ktxoVerticalBottom;
  if AItem.Item in [ktisRightMiddle, ktisRightFirst, ktisRightLast] then
    AText.Orientation := ktxoVerticalTop;
  inherited;
end;

function TSeStyle.TabGetClientRect(const ASubclass: TSeTabSubclass;
  ATab: TSeTabInfo; const AObject: TSeStyleElementObject): TRect;
begin
  Result := ATab.Rect;
  InflateRect(Result, -2, -2);
end;

{ List class ==================================================================}

function TSeStyle.IsListDefined(const ASubclass: TSeListSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin
  Result := False;
  if AObject <> ktoDefault then
    Result := FStyleSource.GetObjectByName(AObject) <> nil;

  if not Result then
    case ASubclass of
      klscListBox: Result := FObjects[soListBox] <> nil;
    else
      Result := False;
    end;
end;

procedure TSeStyle.ListDraw(const ASubclass: TSeListSubclass;
  Canvas: TCanvas; AList: TSeListInfo; const AObject: TSeStyleElementObject);
var
  StyleObject: TSeStyleObject;
begin
  StyleObject := nil;
  if AObject <> ktoDefault then
  begin
    StyleObject := FStyleSource.GetObjectByName(AObject);
    if StyleObject <> nil then
      StyleObject := StyleObject.FindObjectByName('Frame');
  end;

  if StyleObject = nil then
    StyleObject := FObjects[soListBox].FindObjectByName('Frame');

  if StyleObject <> nil then
  begin
    StyleObject.BoundsRect := AList.Rect;
    StyleObject.Draw(Canvas, NullRect);
  end;
end;

procedure TSeStyle.ListDrawItem(const ASubclass: TSeListSubclass;
  Canvas: TCanvas; AItem: TSeListItemInfo; const AObject: TSeStyleElementObject);
var
  StyleObject: TSeStyleObject;
  StyleState: TSeState;
begin
  StyleObject := nil;
  if AObject <> ktoDefault then
  begin
    StyleObject := FStyleSource.GetObjectByName(AObject);
    if StyleObject <> nil then
      StyleObject := StyleObject.FindObjectByName('Item');
  end;

  if StyleObject = nil then
  begin
    if Assigned(FObjects[soListBox]) then
      StyleObject := FObjects[soListBox].FindObjectByName('Item');
  end;

  if StyleObject <> nil then
  begin
    case AItem.DrawState of
      klidsNormal: StyleState := ssNormal;
      klidsHot: StyleState := ssNormal;
      klidsSelected: StyleState := ssFocused;
      klidsFocused: StyleState := ssFocused;
      klidsDisabled: StyleState := ssDisabled;
    else
      StyleState := ssNormal;
    end;
    StyleObject.BoundsRect := AItem.Rect;
    StyleObject.State := StyleState;
    StyleObject.Draw(Canvas, NullRect);
  end;
end;

procedure TSeStyle.ListDrawGlyph(const ASubclass: TSeListSubclass;
  Canvas: TCanvas; AItem: TSeListItemInfo; AGlyph: TSeListGlyphInfo;
  const AObject: TSeStyleElementObject);
var
  StyleObject, S: TSeStyleObject;
  R: TRect;
begin
  StyleObject := nil;
  if AObject <> ktoDefault then
  begin
    StyleObject := FStyleSource.GetObjectByName(AObject);
  end;

  if StyleObject = nil then
    StyleObject := FObjects[soListBox];

  case AGlyph.Kind of
    klgChecked:
      begin
        S := StyleObject.FindObjectByName('Checked');
        if (S = nil) and (FObjects[soCheckBox] <> nil) then
          S := FObjects[soCheckBox].FindObjectByName('Checked');
        if S <> nil then
        begin
          R := Rect(0, 0, S.Width, S.Height);
          RectCenter(R, AGlyph.Rect);
          S.BoundsRect := R;
          S.Draw(Canvas, NullRect);
        end;
      end;
    klgUnchecked:
      begin
        S := StyleObject.FindObjectByName('Unchecked');
        if (S = nil) and (FObjects[soCheckBox] <> nil) then
          S := FObjects[soCheckBox].FindObjectByName('Unchecked');
        if S <> nil then
        begin
          R := Rect(0, 0, S.Width, S.Height);
          RectCenter(R, AGlyph.Rect);
          S.BoundsRect := R;
          S.Draw(Canvas, NullRect);
        end;
      end;
    klgMixed:
      begin
        S := StyleObject.FindObjectByName('Mixed');
        if S = nil then S := FObjects[soListBox].FindObjectByName('Checked');
        if (S = nil) and (FObjects[soCheckBox] <> nil) then
        begin
          S := FObjects[soCheckBox].FindObjectByName('Mixed');
          if S = nil then S := FObjects[soCheckBox].FindObjectByName('Checked');
        end;
        if S <> nil then
        begin
          R := Rect(0, 0, S.Width, S.Height);
          RectCenter(R, AGlyph.Rect);
          S.BoundsRect := R;
          S.Draw(Canvas, NullRect);
        end;
      end;
    klgCustom: if AGlyph.Glyph <> nil then
      begin
        R := TRect.Create(0, 0, AGlyph.Glyph.Width, AGlyph.Glyph.Height);
        RectCenter(R, AGlyph.Rect);
        AGlyph.Glyph.Draw(Canvas, R);
      end;
  end;
end;

function TSeStyle.ListGetClientRect(const ASubclass: TSeListSubclass;
  AList: TSeListInfo; const AObject: TSeStyleElementObject): TRect;
var
  StyleObject: TSeStyleObject;
begin
  Result := AList.Rect;
  StyleObject := FObjects[soListBox].FindObjectByName('Frame');

  if StyleObject <> nil then
  begin
    StyleObject.BoundsRect := AList.Rect;
    with StyleObject, Result do
      Result := Rect(Left + MarginLeft, Top + MarginTop, Right - MarginRight,
        Bottom - MarginBottom);
  end;
end;

{ Tree class ==================================================================}

function TSeStyle.IsTreeDefined(const ASubclass: TSeTreeSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin
  case ASubclass of
    ktscTreeList: Result := FObjects[soListBox] <> nil;
  else
    Result := False;
  end;
end;

procedure TSeStyle.TreeDraw(const ASubclass: TSeTreeSubclass;
  Canvas: TCanvas; ATree: TSeTreeInfo; const AObject: TSeStyleElementObject);
var
  StyleObject: TSeStyleObject;
begin

  StyleObject := FObjects[soListBox].FindObjectByName('Frame');
  if StyleObject <> nil then
  begin
    StyleObject.BoundsRect := ATree.Rect;
    StyleObject.Draw(Canvas, NullRect);
  end;
end;

procedure TSeStyle.TreeDrawItem(const ASubclass: TSeTreeSubclass;
  Canvas: TCanvas; AItem: TSeTreeItemInfo; const AObject: TSeStyleElementObject);
var
  StyleObject: TSeStyleObject;
  StyleState: TSeState;
begin
  StyleObject := FObjects[soListBox].FindObjectByName('Item');
  if StyleObject <> nil then
  begin
    case AItem.DrawState of
      kridsNormal: StyleState := ssNormal;
      kridsHot: StyleState := ssNormal;
      kridsSelected: StyleState := ssFocused;
      kridsFocused: StyleState := ssFocused;
      kridsDisabled: StyleState := ssDisabled;
    else
      StyleState := ssNormal;
    end;
    StyleObject.BoundsRect := AItem.Rect;
    StyleObject.State := StyleState;
    StyleObject.Draw(Canvas, NullRect);
  end;
end;

{ Grid class ==================================================================}

function TSeStyle.IsGridDefined(const ASubclass: TSeGridSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin
  case ASubclass of
    kgscGrid: Result := FObjects[soGrid] <> nil;
  else
    Result := False;
  end;
end;

procedure TSeStyle.GridDraw(const ASubclass: TSeGridSubclass;
  Canvas: TCanvas; AGrid: TSeGridInfo; const AObject: TSeStyleElementObject);
var
  StyleObject: TSeStyleObject;
begin
  StyleObject := FObjects[soGrid].FindObjectByName('Frame');
  if StyleObject <> nil then
  begin
    StyleObject.BoundsRect := AGrid.Rect;
    StyleObject.Draw(Canvas, NullRect);
  end;
end;

procedure TSeStyle.GridDrawItem(const ASubclass: TSeGridSubclass;
  Canvas: TCanvas; AItem: TSeGridItemInfo; const AObject: TSeStyleElementObject);
var
  StyleObject: TSeStyleObject;
  StyleState: TSeState;
begin
  with AItem do
  begin

    case DrawState of
      kgidsNormal: StyleState := ssNormal;
      kgidsHot: StyleState := ssHot;
      kgidsSelected: StyleState := ssFocused;
      kgidsFocused: StyleState := ssFocused;
      kgidsDisabled: StyleState := ssDisabled;
    else
      StyleState := ssNormal;
    end;

    if Item = kgicFixed then
      StyleObject := FObjects[soGrid].FindObjectByName('Fixed')
    else
      StyleObject := FObjects[soGrid].FindObjectByName('Cell');

    if StyleObject <> nil then
    begin
      StyleObject.State := StyleState;
      StyleObject.BoundsRect := AItem.Rect;
      StyleObject.Draw(Canvas, NullRect);
    end;
  end;
end;

{ Box class ===================================================================}

function TSeStyle.IsBoxDefined(const ASubclass: TSeBoxSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin
  Result := False;
end;

function TSeStyle.IsBoxTransparent(const ASubclass: TSeBoxSubclass;
  const AObject: TSeStyleElementObject): boolean;
begin
  Result := False;
end;

{ Positions ===================================================================}

function TSeStyle.GetBounds(AObject: TSeStyleElementObject): TRect;
var
  StyleObject, Bounds: TSeStyleObject;
begin
  StyleObject := FStyleSource.GetObjectByName(AObject);
  if StyleObject <> nil then
  begin
    Bounds := StyleObject.FindObjectByName('Bounds');
    if Bounds <> nil then
    begin
      Result := Bounds.BoundsRect;
    end
    else
      Result := inherited GetBounds(AObject);
  end
  else
    Result := inherited GetBounds(AObject);
end;

function TSeStyle.CustomColor(const AColorName: string): TColor;
var
  StyleObject: TSeStyleObject;
begin
  StyleObject := nil;
  if System.Pos('\', AColorName) > 0 then
    StyleObject := FStyleSource.GetObjectByName(AColorName);
  if StyleObject = nil then
  begin
    StyleObject := FStyleSource.GetObjectByName('CustomColors');
    if StyleObject <> nil then
      StyleObject := StyleObject.FindObjectByName(AColorName)
    else
      StyleObject := FStyleSource.GetObjectByName(AColorName);
  end;

  if StyleObject <> nil then
  begin
    Result := StyleObject.Color;
  end
  else
    Result := inherited CustomColor(AColorName);
end;

procedure TSeStyle.SetFontForObject(AFont: TFont;
  const Font: TSeStyleFont; const AObject: TSeStyleElementObject);
var
  SE: TSeStyleObject;
begin
  inherited;
  if (FStyleSource <> nil) and (Font in [ktfStaticTextNormal..ktfStaticTextDisabled]) then
  begin
    SE := FStyleSource.GetObjectByName(AObject);
    if SE <> nil then
    begin
      AFont.Assign(SE.Font);
    end;
  end;
end;

function TSeStyle.IsObjectDefined(const ASubclass: TSeWindowSubclass;
  const AObject: TSeStyleElementObject = ktoDefault; const AStyleLink: TSeCustomStyleLink = nil): boolean;
begin
  if (AStyleLink <> nil) and (AStyleLink.Style <> nil) then Result := AStyleLink.Style.IsWindowDefined(ASubclass, AObject)
  else
    if (FStyleSource <> nil) and (FStyleSource.Count > 0) then
    begin
      Result := IsWindowDefined(ASubclass, AObject);
      if not Result then
        Result := IsWindowDefined(ASubclass, ktoDefault)
    end
    else
      Result := False;
end;

function TSeStyle.IsObjectDefined(const ASubclass: TSeHintSubclass;
  const AObject: TSeStyleElementObject = ktoDefault; const AStyleLink: TSeCustomStyleLink = nil): boolean;
begin
  if (AStyleLink <> nil) and (AStyleLink.Style <> nil) then Result := AStyleLink.Style.IsHintDefined(ASubclass, AObject)
  else
    if (FStyleSource <> nil) and (FStyleSource.Count > 0) then
    begin
      Result := IsHintDefined(ASubclass, AObject);
      if not Result then
        Result := IsHintDefined(ASubclass, ktoDefault)
    end
    else
      Result := False;
end;

function TSeStyle.IsObjectDefined(const ASubclass: TSeLabelSubclass;
  const AObject: TSeStyleElementObject = ktoDefault; const AStyleLink: TSeCustomStyleLink = nil): boolean;
begin
  if (AStyleLink <> nil) and (AStyleLink.Style <> nil) then Result := AStyleLink.Style.IsLabelDefined(ASubclass, AObject)
  else
    if (FStyleSource <> nil) and (FStyleSource.Count > 0) then
    begin
      Result := IsLabelDefined(ASubclass, AObject);
      if not Result then
        Result := IsLabelDefined(ASubclass, ktoDefault)
    end
    else
      Result := False;
end;

function TSeStyle.IsObjectDefined(const ASubclass: TSePanelSubclass;
  const AObject: TSeStyleElementObject = ktoDefault; const AStyleLink: TSeCustomStyleLink = nil): boolean;
begin
  if (AStyleLink <> nil) and (AStyleLink.Style <> nil) then Result := AStyleLink.Style.IsPanelDefined(ASubclass, AObject)
  else
   if (FStyleSource <> nil) and (FStyleSource.Count > 0) then
    begin
      Result := IsPanelDefined(ASubclass, AObject);
      if not Result then
        Result := IsPanelDefined(ASubclass, ktoDefault)
    end
    else
      Result := False;
end;

function TSeStyle.IsObjectDefined(const ASubclass: TSeButtonSubclass;
  const AObject: TSeStyleElementObject = ktoDefault; const AStyleLink: TSeCustomStyleLink = nil): boolean;
begin
  if (AStyleLink <> nil) and (AStyleLink.Style <> nil) then Result := AStyleLink.Style.IsButtonDefined(ASubclass, AObject)
  else
    if (FStyleSource <> nil) and (FStyleSource.Count > 0) then
    begin
      Result := IsButtonDefined(ASubclass, AObject);
      if not Result then
        Result := IsButtonDefined(ASubclass, ktoDefault)
    end
    else
      Result := False;
end;

function TSeStyle.IsObjectDefined(const ASubclass: TSeCheckSubclass;
  const AObject: TSeStyleElementObject = ktoDefault; const AStyleLink: TSeCustomStyleLink = nil): boolean;
begin
  if (AStyleLink <> nil) and (AStyleLink.Style <> nil) then Result := AStyleLink.Style.IsCheckDefined(ASubclass, AObject)
  else
    if (FStyleSource <> nil) and (FStyleSource.Count > 0) then
    begin
      Result := IsCheckDefined(ASubclass, AObject);
      if not Result then
        Result := IsCheckDefined(ASubclass, ktoDefault)
    end
    else
      Result := False;
end;

function TSeStyle.IsObjectDefined(const ASubclass: TSeSplitterSubclass;
  const AObject: TSeStyleElementObject = ktoDefault; const AStyleLink: TSeCustomStyleLink = nil): boolean;
begin
  if (AStyleLink <> nil) and (AStyleLink.Style <> nil) then Result := AStyleLink.Style.IsSplitterDefined(ASubclass, AObject)
  else
    if (FStyleSource <> nil) and (FStyleSource.Count > 0) then
    begin
      Result := IsSplitterDefined(ASubclass, AObject);
      if not Result then
        Result := IsSplitterDefined(ASubclass, ktoDefault)
    end
    else
      Result := False;
end;

function TSeStyle.IsObjectDefined(const ASubclass: TSeScrollSubclass;
  const AObject: TSeStyleElementObject = ktoDefault; const AStyleLink: TSeCustomStyleLink = nil): boolean;
begin
  if (AStyleLink <> nil) and (AStyleLink.Style <> nil) then Result := AStyleLink.Style.IsScrollDefined(ASubclass, AObject)
  else
    if (FStyleSource <> nil) and (FStyleSource.Count > 0) then
    begin
      Result := IsScrollDefined(ASubclass, AObject);
      if not Result then
        Result := IsScrollDefined(ASubclass, ktoDefault)
    end
    else
      Result := False;
end;

function TSeStyle.IsObjectDefined(const ASubclass: TSeProgressSubclass;
  const AObject: TSeStyleElementObject = ktoDefault; const AStyleLink: TSeCustomStyleLink = nil): boolean;
begin
  if (AStyleLink <> nil) and (AStyleLink.Style <> nil) then Result := AStyleLink.Style.IsProgressDefined(ASubclass, AObject)
  else
    if (FStyleSource <> nil) and (FStyleSource.Count > 0) then
    begin
      Result := IsProgressDefined(ASubclass, AObject);
      if not Result then
        Result := IsProgressDefined(ASubclass, ktoDefault)
    end
    else
      Result := False;
end;

function TSeStyle.IsObjectDefined(const ASubclass: TSeTrackSubclass;
  const AObject: TSeStyleElementObject = ktoDefault; const AStyleLink: TSeCustomStyleLink = nil): boolean;
begin
  if (AStyleLink <> nil) and (AStyleLink.Style <> nil) then Result := AStyleLink.Style.IsTrackDefined(ASubclass, AObject)
  else
    if (FStyleSource <> nil) and (FStyleSource.Count > 0) then
    begin
      Result := IsTrackDefined(ASubclass, AObject);
      if not Result then
        Result := IsTrackDefined(ASubclass, ktoDefault)
    end
    else
      Result := False;
end;

function TSeStyle.IsObjectDefined(const ASubclass: TSeHeaderSubclass;
  const AObject: TSeStyleElementObject = ktoDefault; const AStyleLink: TSeCustomStyleLink = nil): boolean;
begin
  if (AStyleLink <> nil) and (AStyleLink.Style <> nil) then Result := AStyleLink.Style.IsHeaderDefined(ASubclass, AObject)
  else
    if (FStyleSource <> nil) and (FStyleSource.Count > 0) then
    begin
      Result := IsHeaderDefined(ASubclass, AObject);
      if not Result then
        Result := IsHeaderDefined(ASubclass, ktoDefault)
    end
    else
      Result := False;
end;

function TSeStyle.IsObjectDefined(const ASubclass: TSeStatusSubclass;
  const AObject: TSeStyleElementObject = ktoDefault; const AStyleLink: TSeCustomStyleLink = nil): boolean;
begin
  if (AStyleLink <> nil) and (AStyleLink.Style <> nil) then Result := AStyleLink.Style.IsStatusDefined(ASubclass, AObject)
  else
    if (FStyleSource <> nil) and (FStyleSource.Count > 0) then
    begin
      Result := IsStatusDefined(ASubclass, AObject);
      if not Result then
        Result := IsStatusDefined(ASubclass, ktoDefault)
    end
    else
      Result := False;
end;

function TSeStyle.IsObjectDefined(const ASubclass: TSeEditSubclass;
  const AObject: TSeStyleElementObject = ktoDefault; const AStyleLink: TSeCustomStyleLink = nil): boolean;
begin
  if (AStyleLink <> nil) and (AStyleLink.Style <> nil) then Result := AStyleLink.Style.IsEditDefined(ASubclass, AObject)
  else
    if (FStyleSource <> nil) and (FStyleSource.Count > 0) then
    begin
      Result := IsEditDefined(ASubclass, AObject);
      if not Result then
        Result := IsEditDefined(ASubclass, ktoDefault)
    end
    else
      Result := False;
end;

function TSeStyle.IsObjectDefined(const ASubclass: TSeGroupSubclass;
  const AObject: TSeStyleElementObject = ktoDefault; const AStyleLink: TSeCustomStyleLink = nil): boolean;
begin
  if (AStyleLink <> nil) and (AStyleLink.Style <> nil) then Result := AStyleLink.Style.IsGroupDefined(ASubclass, AObject)
  else
    if (FStyleSource <> nil) and (FStyleSource.Count > 0) then
    begin
      Result := IsGroupDefined(ASubclass, AObject);
      if not Result then
        Result := IsGroupDefined(ASubclass, ktoDefault)
    end
    else
      Result := False;
end;

function TSeStyle.IsObjectDefined(const ASubclass: TSeMemoSubclass;
  const AObject: TSeStyleElementObject = ktoDefault; const AStyleLink: TSeCustomStyleLink = nil): boolean;
begin
  if (AStyleLink <> nil) and (AStyleLink.Style <> nil) then Result := AStyleLink.Style.IsMemoDefined(ASubclass, AObject)
  else
    if (FStyleSource <> nil) and (FStyleSource.Count > 0) then
    begin
      Result := IsMemoDefined(ASubclass, AObject);
      if not Result then
        Result := IsMemoDefined(ASubclass, ktoDefault)
    end
    else
      Result := False;
end;

function TSeStyle.IsObjectDefined(const ASubclass: TSeMenuSubclass;
  const AObject: TSeStyleElementObject = ktoDefault; const AStyleLink: TSeCustomStyleLink = nil): boolean;
begin
  if (AStyleLink <> nil) and (AStyleLink.Style <> nil) then Result := AStyleLink.Style.IsMenuDefined(ASubclass, AObject)
  else
    if (FStyleSource <> nil) and (FStyleSource.Count > 0) then
    begin
      Result := IsMenuDefined(ASubclass, AObject);
      if not Result then
        Result := IsMenuDefined(ASubclass, ktoDefault)
    end
    else
      Result := False;
end;

function TSeStyle.IsObjectDefined(const ASubclass: TSeToolSubclass;
  const AObject: TSeStyleElementObject = ktoDefault; const AStyleLink: TSeCustomStyleLink = nil): boolean;
begin
  if (AStyleLink <> nil) and (AStyleLink.Style <> nil) then Result := AStyleLink.Style.IsToolDefined(ASubclass, AObject)
  else
    if (FStyleSource <> nil) and (FStyleSource.Count > 0) then
    begin
      Result := IsToolDefined(ASubclass, AObject);
      if not Result then
        Result := IsToolDefined(ASubclass, ktoDefault)
    end
    else
      Result := False;
end;

function TSeStyle.IsObjectDefined(const ASubclass: TSeBarSubclass;
  const AObject: TSeStyleElementObject = ktoDefault; const AStyleLink: TSeCustomStyleLink = nil): boolean;
begin
  if (AStyleLink <> nil) and (AStyleLink.Style <> nil) then Result := AStyleLink.Style.IsBarDefined(ASubclass, AObject)
  else
    if (FStyleSource <> nil) and (FStyleSource.Count > 0) then
    begin
      Result := IsBarDefined(ASubclass, AObject);
      if not Result then
        Result := IsBarDefined(ASubclass, ktoDefault)
    end
    else
      Result := False;
end;

function TSeStyle.IsObjectDefined(const ASubclass: TSeTabSubclass;
  const AObject: TSeStyleElementObject = ktoDefault; const AStyleLink: TSeCustomStyleLink = nil): boolean;
begin
  if (AStyleLink <> nil) and (AStyleLink.Style <> nil) then Result := AStyleLink.Style.IsTabDefined(ASubclass, AObject)
  else
    if (FStyleSource <> nil) and (FStyleSource.Count > 0) then
    begin
      Result := IsTabDefined(ASubclass, AObject);
      if not Result then
        Result := IsTabDefined(ASubclass, ktoDefault)
    end
    else
      Result := False;
end;

function TSeStyle.IsObjectDefined(const ASubclass: TSeListSubclass;
  const AObject: TSeStyleElementObject = ktoDefault; const AStyleLink: TSeCustomStyleLink = nil): boolean;
begin
  if (AStyleLink <> nil) and (AStyleLink.Style <> nil) then Result := AStyleLink.Style.IsListDefined(ASubclass, AObject)
  else
    if (FStyleSource <> nil) and (FStyleSource.Count > 0) then
    begin
      Result := IsListDefined(ASubclass, AObject);
      if not Result then
        Result := IsListDefined(ASubclass, ktoDefault)
    end
    else
      Result := False;
end;

function TSeStyle.IsObjectDefined(const ASubclass: TSeTreeSubclass;
  const AObject: TSeStyleElementObject = ktoDefault; const AStyleLink: TSeCustomStyleLink = nil): boolean;
begin
  if (AStyleLink <> nil) and (AStyleLink.Style <> nil) then Result := AStyleLink.Style.IsTreeDefined(ASubclass, AObject)
  else
    if (FStyleSource <> nil) and (FStyleSource.Count > 0) then
    begin
      Result := IsTreeDefined(ASubclass, AObject);
      if not Result then
        Result := IsTreeDefined(ASubclass, ktoDefault)
    end
    else
      Result := False;
end;

function TSeStyle.IsObjectDefined(const ASubclass: TSeGridSubclass;
  const AObject: TSeStyleElementObject = ktoDefault; const AStyleLink: TSeCustomStyleLink = nil): boolean;
begin
  if (AStyleLink <> nil) and (AStyleLink.Style <> nil) then Result := AStyleLink.Style.IsGridDefined(ASubclass, AObject)
  else
    if (FStyleSource <> nil) and (FStyleSource.Count > 0) then
    begin
      Result := IsGridDefined(ASubclass, AObject);
      if not Result then
        Result := IsGridDefined(ASubclass, ktoDefault)
    end
    else
      Result := False;
end;

function TSeStyle.IsObjectDefined(const ASubclass: TSeDockSubclass;
  const AObject: TSeStyleElementObject = ktoDefault; const AStyleLink: TSeCustomStyleLink = nil): boolean;
begin
  if (AStyleLink <> nil) and (AStyleLink.Style <> nil) then Result := AStyleLink.Style.IsDockDefined(ASubclass, AObject)
  else
    if (FStyleSource <> nil) and (FStyleSource.Count > 0) then
    begin
      Result := IsDockDefined(ASubclass, AObject);
      if not Result then
        Result := IsDockDefined(ASubclass, ktoDefault)
    end
    else
      Result := False;
end;

function TSeStyle.IsObjectDefined(const ASubclass: TSeBarDockSubclass;
  const AObject: TSeStyleElementObject = ktoDefault; const AStyleLink: TSeCustomStyleLink = nil): boolean;
begin
  if (AStyleLink <> nil) and (AStyleLink.Style <> nil) then Result := AStyleLink.Style.IsBarDockDefined(ASubclass, AObject)
  else
    if (FStyleSource <> nil) and (FStyleSource.Count > 0) then
    begin
      Result := IsBarDockDefined(ASubclass, AObject);
      if not Result then
        Result := IsBarDockDefined(ASubclass, ktoDefault)
    end
    else
      Result := False;
end;


procedure InitStyleAPI;
begin
  RegisterStyleObject(TSeStyleObject);
  RegisterStyleObject(TSeActiveObject);
  RegisterStyleObject(TSeBitmapObject);
  RegisterStyleObject(TSeActiveBitmap);
  RegisterStyleObject(TSeSystemButton);
  RegisterStyleObject(TSeButtonObject);
  RegisterStyleObject(TSeTextObject);
  RegisterStyleObject(TSeLinkStyleObject);
  RegisterStyleObject(TSeActiveStyleObject);
  RegisterStyleObject(TSeColorButtonObject);
  RegisterStyleObject(TSeRollPanelObject);
end;

procedure FinalizeStyleAPI;
begin
  FreeDrawBackgroundFuncList;

  RegObjects.Free;
  ReleaseObjectStore;
  ReleasePropStore;

  if Assigned(ConvertBitmap) then
    FreeAndNil(ConvertBitmap);
end;
