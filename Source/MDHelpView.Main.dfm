object MainForm: TMainForm
  Left = 403
  Top = 200
  Caption = 'MarkDown Help Viewer'
  ClientHeight = 424
  ClientWidth = 744
  Color = clBtnFace
  Constraints.MinHeight = 450
  Constraints.MinWidth = 760
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object Splitter: TSplitter
    Left = 300
    Top = 48
    Width = 6
    Height = 376
    ExplicitLeft = 322
    ExplicitTop = 54
    ExplicitHeight = 531
  end
  object PageControl: TPageControl
    Left = 0
    Top = 48
    Width = 300
    Height = 376
    ActivePage = tsIndex
    Align = alLeft
    TabOrder = 0
    ExplicitHeight = 377
    object tsIndex: TTabSheet
      Hint = 'MarkDown Content/Index'
      Caption = 'Content/Index'
      object HtmlViewerIndex: THtmlViewer
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 286
        Height = 341
        BorderStyle = htFocused
        DefBackground = clWindow
        HistoryMaxCount = 0
        NoSelect = False
        PrintMarginBottom = 0.800000000000000000
        PrintMarginLeft = 0.800000000000000000
        PrintMarginRight = 0.800000000000000000
        PrintMarginTop = 0.800000000000000000
        PrintScale = 1.000000000000000000
        Text = ''
        Align = alClient
        TabOrder = 0
        Touch.InteractiveGestures = [igPan]
        Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia]
      end
    end
    object tsFiles: TTabSheet
      Hint = 'List of Files into Working Directory'
      Caption = 'Working Dir'
      ImageIndex = 1
      DesignSize = (
        292
        346)
      object lbIndex: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 72
        Height = 15
        Caption = 'Search for file'
      end
      object lbSelectFile: TLabel
        Left = 3
        Top = 59
        Width = 94
        Height = 15
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Select file to view:'
      end
      object FileListBox: TFileListBox
        AlignWithMargins = True
        Left = 4
        Top = 80
        Width = 284
        Height = 234
        Anchors = [akLeft, akTop, akRight, akBottom]
        FileType = [ftReadOnly, ftNormal]
        ItemHeight = 15
        Mask = '*.md'
        TabOrder = 1
        OnDblClick = acViewExecute
      end
      object edFileSearch: TEdit
        AlignWithMargins = True
        Left = 4
        Top = 24
        Width = 284
        Height = 23
        Hint = 'Insert file name to search'
        Anchors = [akLeft, akTop, akRight]
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        OnChange = edFileSearchChange
      end
      object btIndex: TButton
        Left = 178
        Top = 320
        Width = 110
        Height = 24
        Cursor = crHandPoint
        Action = acView
        Anchors = [akRight, akBottom]
        Default = True
        TabOrder = 2
      end
    end
    object tsSearch: TTabSheet
      Caption = 'Search'
      ImageIndex = 2
      DesignSize = (
        292
        347)
      object lbSearch: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 286
        Height = 15
        Align = alTop
        Caption = 'Input keyword to search in files'
        ExplicitWidth = 164
      end
      object lbSelectSearch: TLabel
        Left = 3
        Top = 79
        Width = 94
        Height = 15
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Select file to view:'
      end
      object edSearch: TEdit
        AlignWithMargins = True
        Left = 4
        Top = 24
        Width = 284
        Height = 23
        Hint = 'Input the keyword to search into files in the working folder'
        Anchors = [akLeft, akTop, akRight]
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
      end
      object SearchListBox: TListBox
        AlignWithMargins = True
        Left = 4
        Top = 97
        Width = 284
        Height = 216
        Anchors = [akLeft, akTop, akRight, akBottom]
        ItemHeight = 15
        TabOrder = 2
        OnDblClick = acViewSearchExecute
      end
      object btSearch: TButton
        Left = 178
        Top = 53
        Width = 110
        Height = 24
        Cursor = crHandPoint
        Action = acSearch
        Anchors = [akTop, akRight]
        Default = True
        TabOrder = 1
      end
      object btSearchView: TButton
        Left = 178
        Top = 319
        Width = 110
        Height = 24
        Cursor = crHandPoint
        Action = acViewSearch
        Anchors = [akRight, akBottom]
        TabOrder = 3
      end
    end
  end
  object paTop: TPanel
    Left = 0
    Top = 0
    Width = 744
    Height = 48
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitWidth = 748
    DesignSize = (
      744
      48)
    object ProcessorDialectLabel: TLabel
      Left = 521
      Top = 8
      Width = 83
      Height = 15
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      Caption = 'Transformation:'
      ExplicitLeft = 529
    end
    object ProcessorDialectComboBox: TComboBox
      Left = 610
      Top = 5
      Width = 127
      Height = 23
      Hint = 'Markdown Tranformation Dialect'
      Style = csDropDownList
      Anchors = [akTop, akRight]
      TabOrder = 0
      OnSelect = ProcessorDialectComboBoxSelect
      Items.Strings = (
        'DaringFireball'
        'CommonMark')
    end
    object ToolBar: TToolBar
      AlignWithMargins = True
      Left = 0
      Top = 3
      Width = 518
      Height = 46
      Margins.Left = 0
      Margins.Right = 230
      Margins.Bottom = 0
      AutoSize = True
      ButtonHeight = 46
      ButtonWidth = 55
      Images = SVGIconImageList
      ShowCaptions = True
      TabOrder = 1
      object btShowHide: TToolButton
        Left = 0
        Top = 0
        Cursor = crHandPoint
        Action = acHide
      end
      object sep1: TToolButton
        Left = 55
        Top = 0
        Width = 8
        ImageIndex = 8
        ImageName = 'folder-open'
        Style = tbsSeparator
      end
      object btOpen: TToolButton
        Left = 63
        Top = 0
        Cursor = crHandPoint
        Action = acFileOpen
      end
      object btHome: TToolButton
        Left = 118
        Top = 0
        Cursor = crHandPoint
        Action = acHome
      end
      object sep2: TToolButton
        Left = 173
        Top = 0
        Width = 8
        ImageIndex = 8
        ImageName = 'folder-open'
        Style = tbsSeparator
      end
      object btPrevius: TToolButton
        Left = 181
        Top = 0
        Cursor = crHandPoint
        Action = acPreviousPage
      end
      object btNext: TToolButton
        Left = 236
        Top = 0
        Cursor = crHandPoint
        Action = acNextPage
      end
      object sep3: TToolButton
        Left = 291
        Top = 0
        Width = 8
        ImageIndex = 8
        ImageName = 'folder-open'
        Style = tbsSeparator
      end
      object btSaveToPdf: TToolButton
        Left = 299
        Top = 0
        Cursor = crHandPoint
        Action = acSaveToPDF
      end
      object Sep4: TToolButton
        Left = 354
        Top = 0
        Width = 8
        ImageName = 'crosshairs-question'
        Style = tbsSeparator
      end
      object btOption: TToolButton
        Left = 362
        Top = 0
        Cursor = crHandPoint
        Action = acSettings
      end
      object btAbout: TToolButton
        Left = 417
        Top = 0
        Cursor = crHandPoint
        Action = acAbout
      end
    end
  end
  object ClientPanel: TPanel
    Left = 306
    Top = 48
    Width = 442
    Height = 377
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    OnResize = ClientPanelResize
    object HtmlViewer: THtmlViewer
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 436
      Height = 371
      BorderStyle = htFocused
      DefBackground = clWindow
      HistoryMaxCount = 0
      NoSelect = False
      PrintMarginBottom = 0.800000000000000000
      PrintMarginLeft = 0.800000000000000000
      PrintMarginRight = 0.800000000000000000
      PrintMarginTop = 0.800000000000000000
      PrintScale = 1.000000000000000000
      Text = ''
      Align = alClient
      TabOrder = 0
      Touch.InteractiveGestures = [igPan]
      Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia]
    end
  end
  object TActionList: TActionList
    Images = SVGIconImageList
    OnUpdate = TActionListUpdate
    Left = 512
    Top = 136
    object acFileOpen: TFileOpen
      Category = 'File'
      Caption = '&Open...'
      Dialog.Title = 'Open MarkDown or HTML file...'
      Hint = 'Open an existing MarkDown or HTML file...'
      ImageIndex = 8
      ShortCut = 16463
      OnAccept = acFileOpenAccept
    end
    object acPreviousPage: TAction
      Category = 'Page'
      Caption = '&Previous'
      Enabled = False
      Hint = 'Previous Opened File'
      ImageIndex = 6
      ImageName = 'arrow-left-thick'
      OnExecute = acPreviousPageExecute
      OnUpdate = acPreviousPageUpdate
    end
    object acNextPage: TAction
      Category = 'Page'
      Caption = '&Next'
      Enabled = False
      Hint = 'Next Opened File'
      ImageIndex = 5
      ImageName = 'arrow-right-thick'
      OnExecute = acNextPageExecute
      OnUpdate = acNextPageUpdate
    end
    object acView: TAction
      Category = 'PageControl'
      Caption = 'View'
      Hint = 'View File'
      OnExecute = acViewExecute
      OnUpdate = acViewUpdate
    end
    object acHide: TAction
      Category = 'PageControl'
      Caption = 'Hide'
      Hint = 'Hide Content/Search Panel'
      ImageIndex = 12
      ImageName = 'page-next-outline'
      OnExecute = acHideExecute
    end
    object acSearch: TAction
      Category = 'Search'
      Caption = 'Search'
      Hint = 'Search keyword into files'
      OnExecute = acSearchExecute
      OnUpdate = acSearchUpdate
    end
    object acHome: TAction
      Category = 'Tab'
      Caption = 'Home'
      Enabled = False
      Hint = 'Back to Home File'
      ImageIndex = 4
      ImageName = 'home'
      OnExecute = acHomeExecute
      OnUpdate = acHomeUpdate
    end
    object acShow: TAction
      Category = 'Tab'
      Caption = 'Show'
      Enabled = False
      Hint = 'Show Content/Search Panel'
      ImageIndex = 13
      ImageName = 'page-previous-outline'
      OnExecute = acShowExecute
    end
    object acSettings: TAction
      Category = 'App'
      Caption = 'Settings'
      Hint = 'Show Settings'
      ImageIndex = 9
      ImageName = 'tools'
      OnExecute = acSettingsExecute
    end
    object acAbout: TAction
      Category = 'App'
      Caption = 'About'
      Hint = 'Show Application Information'
      ImageIndex = 7
      ImageName = 'information-outline'
      OnExecute = acAboutExecute
    end
    object acPrint: TAction
      Category = 'File'
      Caption = 'Print'
      Hint = 'Print file in PDF'
      ImageIndex = 10
      ImageName = 'printer'
    end
    object acSaveToPDF: TAction
      Caption = 'Save PDF'
      Hint = 'Save current file in PDF format...'
      ImageIndex = 11
      ImageName = 'file-pdf-outline'
      OnExecute = acSaveToPDFExecute
      OnUpdate = acSaveToPDFUpdate
    end
    object acViewSearch: TAction
      Category = 'PageControl'
      Caption = 'View'
      Hint = 'View File'
      OnExecute = acViewSearchExecute
      OnUpdate = acViewSearchUpdate
    end
  end
  object SaveDialog: TSaveDialog
    Filter = 'MarkDown files (.md)|*.md|Html files|*.html, *.htm'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 508
    Top = 320
  end
  object SVGIconImageList: TSVGIconImageList
    Size = 24
    SVGIconItems = <
      item
        IconName = 'markdown-black'
        SVGText = 
          '<?xml version="1.0" encoding="UTF-8" standalone="no"?>'#13#10'<svg wid' +
          'th="200mm" height="200mm" viewBox="0 0 200 200">'#13#10'  <path'#13#10'     ' +
          'id="rect849"'#13#10'     style="display:inline;opacity:1;fill:#000000;' +
          'stroke-width:0.669135"'#13#10'     d="M 35.576689,25 C 29.717309,25 25' +
          ',32.68125 25,42.22221 V 157.77779 C 25,167.31871 29.717309,175 3' +
          '5.576689,175 H 164.42332 C 170.2827,175 175,167.31871 175,157.77' +
          '779 V 42.22221 C 175,32.68125 170.2827,25 164.42332,25 Z m 8.344' +
          '986,34.869643 h 14.495189 l 14.367165,29.01939 14.398905,-28.939' +
          '32 14.420426,-0.0798 V 140.13227 H 87.462534 L 87.182934,94.5393' +
          '83 72.784029,123.87404 58.866482,94.933013 58.586874,140.13227 H' +
          ' 43.921675 Z m 82.851875,0 h 14.68158 v 40.985047 h 14.62318 l -' +
          '21.92969,39.17057 -21.88973,-39.17057 h 14.51466 z"/>'#13#10'</svg>'
        FixedColor = clBlack
      end
      item
        IconName = 'markdown-white'
        SVGText = 
          '<?xml version="1.0" encoding="UTF-8" standalone="no"?>'#13#10'<svg wid' +
          'th="200mm" height="200mm" viewBox="0 0 200 200">'#13#10'  <path'#13#10'     ' +
          'id="rect849"'#13#10'     style="display:inline;opacity:1;fill:#FFFFFF;' +
          'stroke-width:0.669135"'#13#10'     d="M 35.576689,25 C 29.717309,25 25' +
          ',32.68125 25,42.22221 V 157.77779 C 25,167.31871 29.717309,175 3' +
          '5.576689,175 H 164.42332 C 170.2827,175 175,167.31871 175,157.77' +
          '779 V 42.22221 C 175,32.68125 170.2827,25 164.42332,25 Z m 8.344' +
          '986,34.869643 h 14.495189 l 14.367165,29.01939 14.398905,-28.939' +
          '32 14.420426,-0.0798 V 140.13227 H 87.462534 L 87.182934,94.5393' +
          '83 72.784029,123.87404 58.866482,94.933013 58.586874,140.13227 H' +
          ' 43.921675 Z m 82.851875,0 h 14.68158 v 40.985047 h 14.62318 l -' +
          '21.92969,39.17057 -21.88973,-39.17057 h 14.51466 z"/>'#13#10'</svg>'
        FixedColor = clWhite
      end
      item
        IconName = 'markdown-black-gray'
        SVGText = 
          '<?xml version="1.0" encoding="UTF-8" standalone="no"?>'#13#10'<svg wid' +
          'th="200mm" height="200mm" viewBox="0 0 200 200">'#13#10'  <path'#13#10'     ' +
          'id="rect849"'#13#10'     style="display:inline;opacity:1;fill:#808080;' +
          'stroke-width:0.669135"'#13#10'     d="M 35.576689,25 C 29.717309,25 25' +
          ',32.68125 25,42.22221 V 157.77779 C 25,167.31871 29.717309,175 3' +
          '5.576689,175 H 164.42332 C 170.2827,175 175,167.31871 175,157.77' +
          '779 V 42.22221 C 175,32.68125 170.2827,25 164.42332,25 Z m 8.344' +
          '986,34.869643 h 14.495189 l 14.367165,29.01939 14.398905,-28.939' +
          '32 14.420426,-0.0798 V 140.13227 H 87.462534 L 87.182934,94.5393' +
          '83 72.784029,123.87404 58.866482,94.933013 58.586874,140.13227 H' +
          ' 43.921675 Z m 82.851875,0 h 14.68158 v 40.985047 h 14.62318 l -' +
          '21.92969,39.17057 -21.88973,-39.17057 h 14.51466 z"/>'#13#10'</svg>'
        FixedColor = clGray
      end
      item
        IconName = 'markdown-white-gray'
        SVGText = 
          '<?xml version="1.0" encoding="UTF-8" standalone="no"?>'#13#10'<svg wid' +
          'th="200mm" height="200mm" viewBox="0 0 200 200">'#13#10'  <path'#13#10'     ' +
          'id="path891"'#13#10'     style="fill:#808080;stroke:none;stroke-width:' +
          '0.669133px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opac' +
          'ity:1"'#13#10'     d="m 35.577626,25.000001 c -5.85934,0 -13.250324,7.' +
          '681211 -13.250324,17.222207 V 157.77781 c 0,9.54093 7.390984,17.' +
          '22219 13.250324,17.22219 H 164.42236 c 5.85934,0 13.25034,-7.681' +
          '26 13.25034,-17.22219 V 42.222208 c 0,-9.540996 -7.391,-17.22220' +
          '7 -13.25034,-17.222207 z m 3.502656,11.795528 H 160.91972 c 3.63' +
          '365,0 6.55979,4.76472 6.55979,10.681542 V 152.5246 c 0,5.9168 -2' +
          '.92614,10.67986 -6.55979,10.67986 H 39.080282 c -3.633659,0 -6.5' +
          '59801,-4.76306 -6.559801,-10.67986 V 47.477071 c 0,-5.916822 2.9' +
          '26142,-10.681542 6.559801,-10.681542 z m 19.336356,23.072449 -14' +
          '.494083,0.0017 v 80.262382 h 14.66512 l 0.278573,-45.200962 13.9' +
          '1745,28.941022 14.39881,-29.334582 0.27959,45.594522 H 101.60283' +
          ' V 59.869643 l -14.420322,0.08004 -14.39881,28.937686 z m 68.356' +
          '202,0.0017 v 40.983372 h -14.51455 l 21.89063,39.17061 21.92853,' +
          '-39.17061 H 141.45433 V 59.869643 Z"/>'#13#10'</svg>'#13#10
        FixedColor = clGray
      end
      item
        IconName = 'home'
        SVGText = 
          '<?xml version="1.0" encoding="UTF-8"?><!DOCTYPE svg PUBLIC "-//W' +
          '3C//DTD SVG 1.1//EN" "http://www.w3.org/Graphics/SVG/1.1/DTD/svg' +
          '11.dtd"><svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="htt' +
          'p://www.w3.org/1999/xlink" version="1.1" id="mdi-home" width="24' +
          '" height="24" viewBox="0 0 24 24"><path d="M10,20V14H14V20H19V12' +
          'H22L12,3L2,12H5V20H10Z" /></svg>'
      end
      item
        IconName = 'arrow-right-thick'
        SVGText = 
          '<?xml version="1.0" encoding="UTF-8"?><!DOCTYPE svg PUBLIC "-//W' +
          '3C//DTD SVG 1.1//EN" "http://www.w3.org/Graphics/SVG/1.1/DTD/svg' +
          '11.dtd"><svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="htt' +
          'p://www.w3.org/1999/xlink" version="1.1" id="mdi-arrow-right-thi' +
          'ck" width="24" height="24" viewBox="0 0 24 24"><path d="M4,10V14' +
          'H13L9.5,17.5L11.92,19.92L19.84,12L11.92,4.08L9.5,6.5L13,10H4Z" /' +
          '></svg>'
      end
      item
        IconName = 'arrow-left-thick'
        SVGText = 
          '<?xml version="1.0" encoding="UTF-8"?><!DOCTYPE svg PUBLIC "-//W' +
          '3C//DTD SVG 1.1//EN" "http://www.w3.org/Graphics/SVG/1.1/DTD/svg' +
          '11.dtd"><svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="htt' +
          'p://www.w3.org/1999/xlink" version="1.1" id="mdi-arrow-left-thic' +
          'k" width="24" height="24" viewBox="0 0 24 24"><path d="M20,10V14' +
          'H11L14.5,17.5L12.08,19.92L4.16,12L12.08,4.08L14.5,6.5L11,10H20Z"' +
          ' /></svg>'
      end
      item
        IconName = 'information-outline'
        SVGText = 
          '<?xml version="1.0" encoding="UTF-8"?><!DOCTYPE svg PUBLIC "-//W' +
          '3C//DTD SVG 1.1//EN" "http://www.w3.org/Graphics/SVG/1.1/DTD/svg' +
          '11.dtd"><svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="htt' +
          'p://www.w3.org/1999/xlink" version="1.1" id="mdi-information-out' +
          'line" width="24" height="24" viewBox="0 0 24 24"><path d="M11,9H' +
          '13V7H11M12,20C7.59,20 4,16.41 4,12C4,7.59 7.59,4 12,4C16.41,4 20' +
          ',7.59 20,12C20,16.41 16.41,20 12,20M12,2A10,10 0 0,0 2,12A10,10 ' +
          '0 0,0 12,22A10,10 0 0,0 22,12A10,10 0 0,0 12,2M11,17H13V11H11V17' +
          'Z" /></svg>'
      end
      item
        IconName = 'folder-open'
        SVGText = 
          '<?xml version="1.0" encoding="UTF-8"?><!DOCTYPE svg PUBLIC "-//W' +
          '3C//DTD SVG 1.1//EN" "http://www.w3.org/Graphics/SVG/1.1/DTD/svg' +
          '11.dtd"><svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="htt' +
          'p://www.w3.org/1999/xlink" version="1.1" id="mdi-folder-open" wi' +
          'dth="24" height="24" viewBox="0 0 24 24"><path d="M19,20H4C2.89,' +
          '20 2,19.1 2,18V6C2,4.89 2.89,4 4,4H10L12,6H19A2,2 0 0,1 21,8H21L' +
          '4,8V18L6.14,10H23.21L20.93,18.5C20.7,19.37 19.92,20 19,20Z" /></' +
          'svg>'
      end
      item
        IconName = 'tools'
        SVGText = 
          '<?xml version="1.0" encoding="UTF-8"?><!DOCTYPE svg PUBLIC "-//W' +
          '3C//DTD SVG 1.1//EN" "http://www.w3.org/Graphics/SVG/1.1/DTD/svg' +
          '11.dtd"><svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="htt' +
          'p://www.w3.org/1999/xlink" version="1.1" id="mdi-tools" width="2' +
          '4" height="24" viewBox="0 0 24 24"><path d="M21.71 20.29L20.29 2' +
          '1.71A1 1 0 0 1 18.88 21.71L7 9.85A3.81 3.81 0 0 1 6 10A4 4 0 0 1' +
          ' 2.22 4.7L4.76 7.24L5.29 6.71L6.71 5.29L7.24 4.76L4.7 2.22A4 4 0' +
          ' 0 1 10 6A3.81 3.81 0 0 1 9.85 7L21.71 18.88A1 1 0 0 1 21.71 20.' +
          '29M2.29 18.88A1 1 0 0 0 2.29 20.29L3.71 21.71A1 1 0 0 0 5.12 21.' +
          '71L10.59 16.25L7.76 13.42M20 2L16 4V6L13.83 8.17L15.83 10.17L18 ' +
          '8H20L22 4Z" /></svg>'
      end
      item
        IconName = 'printer'
        SVGText = 
          '<?xml version="1.0" encoding="UTF-8"?><!DOCTYPE svg PUBLIC "-//W' +
          '3C//DTD SVG 1.1//EN" "http://www.w3.org/Graphics/SVG/1.1/DTD/svg' +
          '11.dtd"><svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="htt' +
          'p://www.w3.org/1999/xlink" version="1.1" id="mdi-printer" width=' +
          '"24" height="24" viewBox="0 0 24 24"><path d="M18,3H6V7H18M19,12' +
          'A1,1 0 0,1 18,11A1,1 0 0,1 19,10A1,1 0 0,1 20,11A1,1 0 0,1 19,12' +
          'M16,19H8V14H16M19,8H5A3,3 0 0,0 2,11V17H6V21H18V17H22V11A3,3 0 0' +
          ',0 19,8Z" /></svg>'
      end
      item
        IconName = 'file-pdf-outline'
        SVGText = 
          '<?xml version="1.0" encoding="UTF-8"?><!DOCTYPE svg PUBLIC "-//W' +
          '3C//DTD SVG 1.1//EN" "http://www.w3.org/Graphics/SVG/1.1/DTD/svg' +
          '11.dtd"><svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="htt' +
          'p://www.w3.org/1999/xlink" version="1.1" id="mdi-file-pdf-outlin' +
          'e" width="24" height="24" viewBox="0 0 24 24"><path d="M14,2L20,' +
          '8V20A2,2 0 0,1 18,22H6A2,2 0 0,1 4,20V4A2,2 0 0,1 6,2H14M18,20V9' +
          'H13V4H6V20H18M10.92,12.31C10.68,11.54 10.15,9.08 11.55,9.04C12.9' +
          '5,9 12.03,12.16 12.03,12.16C12.42,13.65 14.05,14.72 14.05,14.72C' +
          '14.55,14.57 17.4,14.24 17,15.72C16.57,17.2 13.5,15.81 13.5,15.81' +
          'C11.55,15.95 10.09,16.47 10.09,16.47C8.96,18.58 7.64,19.5 7.1,18' +
          '.61C6.43,17.5 9.23,16.07 9.23,16.07C10.68,13.72 10.9,12.35 10.92' +
          ',12.31M11.57,13.15C11.17,14.45 10.37,15.84 10.37,15.84C11.22,15.' +
          '5 13.08,15.11 13.08,15.11C11.94,14.11 11.59,13.16 11.57,13.15M14' +
          '.71,15.32C14.71,15.32 16.46,15.97 16.5,15.71C16.57,15.44 15.17,1' +
          '5.2 14.71,15.32M9.05,16.81C8.28,17.11 7.54,18.39 7.72,18.39C7.9,' +
          '18.4 8.63,17.79 9.05,16.81M11.57,11.26C11.57,11.21 12,9.58 11.57' +
          ',9.53C11.27,9.5 11.56,11.22 11.57,11.26Z" /></svg>'
      end
      item
        IconName = 'page-next-outline'
        SVGText = 
          '<?xml version="1.0" encoding="UTF-8"?><!DOCTYPE svg PUBLIC "-//W' +
          '3C//DTD SVG 1.1//EN" "http://www.w3.org/Graphics/SVG/1.1/DTD/svg' +
          '11.dtd"><svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="htt' +
          'p://www.w3.org/1999/xlink" version="1.1" id="mdi-page-next-outli' +
          'ne" width="24" height="24" viewBox="0 0 24 24"><path d="M22,3H5A' +
          '2,2 0 0,0 3,5V9H5V5H22V19H5V15H3V19A2,2 0 0,0 5,21H22A2,2 0 0,0 ' +
          '24,19V5A2,2 0 0,0 22,3M7,15V13H0V11H7V9L11,12L7,15M20,13H13V11H2' +
          '0V13M20,9H13V7H20V9M17,17H13V15H17V17Z" /></svg>'
      end
      item
        IconName = 'page-previous-outline'
        SVGText = 
          '<?xml version="1.0" encoding="UTF-8"?><!DOCTYPE svg PUBLIC "-//W' +
          '3C//DTD SVG 1.1//EN" "http://www.w3.org/Graphics/SVG/1.1/DTD/svg' +
          '11.dtd"><svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="htt' +
          'p://www.w3.org/1999/xlink" version="1.1" id="mdi-page-previous-o' +
          'utline" width="24" height="24" viewBox="0 0 24 24"><path d="M2,3' +
          'H19A2,2 0 0,1 21,5V9H19V5H2V19H19V15H21V19A2,2 0 0,1 19,21H2A2,2' +
          ' 0 0,1 0,19V5A2,2 0 0,1 2,3M17,15V13H24V11H17V9L13,12L17,15M4,13' +
          'H11V11H4V13M4,9H11V7H4V9M4,17H8V15H4V17Z" /></svg>'
      end>
    Scaled = True
    Left = 408
    Top = 232
  end
  object SVGIconImageListColored: TSVGIconImageList
    Size = 24
    SVGIconItems = <
      item
        IconName = 'markdown-black'
        SVGText = 
          '<?xml version="1.0" encoding="UTF-8" standalone="no"?>'#13#10'<svg wid' +
          'th="200mm" height="200mm" viewBox="0 0 200 200">'#13#10'  <path'#13#10'     ' +
          'id="rect849"'#13#10'     style="display:inline;opacity:1;fill:#000000;' +
          'stroke-width:0.669135"'#13#10'     d="M 35.576689,25 C 29.717309,25 25' +
          ',32.68125 25,42.22221 V 157.77779 C 25,167.31871 29.717309,175 3' +
          '5.576689,175 H 164.42332 C 170.2827,175 175,167.31871 175,157.77' +
          '779 V 42.22221 C 175,32.68125 170.2827,25 164.42332,25 Z m 8.344' +
          '986,34.869643 h 14.495189 l 14.367165,29.01939 14.398905,-28.939' +
          '32 14.420426,-0.0798 V 140.13227 H 87.462534 L 87.182934,94.5393' +
          '83 72.784029,123.87404 58.866482,94.933013 58.586874,140.13227 H' +
          ' 43.921675 Z m 82.851875,0 h 14.68158 v 40.985047 h 14.62318 l -' +
          '21.92969,39.17057 -21.88973,-39.17057 h 14.51466 z"/>'#13#10'</svg>'
        FixedColor = clBlack
      end
      item
        IconName = 'markdown-white'
        SVGText = 
          '<?xml version="1.0" encoding="UTF-8" standalone="no"?>'#13#10'<svg wid' +
          'th="200mm" height="200mm" viewBox="0 0 200 200">'#13#10'  <path'#13#10'     ' +
          'id="rect849"'#13#10'     style="display:inline;opacity:1;fill:#FFFFFF;' +
          'stroke-width:0.669135"'#13#10'     d="M 35.576689,25 C 29.717309,25 25' +
          ',32.68125 25,42.22221 V 157.77779 C 25,167.31871 29.717309,175 3' +
          '5.576689,175 H 164.42332 C 170.2827,175 175,167.31871 175,157.77' +
          '779 V 42.22221 C 175,32.68125 170.2827,25 164.42332,25 Z m 8.344' +
          '986,34.869643 h 14.495189 l 14.367165,29.01939 14.398905,-28.939' +
          '32 14.420426,-0.0798 V 140.13227 H 87.462534 L 87.182934,94.5393' +
          '83 72.784029,123.87404 58.866482,94.933013 58.586874,140.13227 H' +
          ' 43.921675 Z m 82.851875,0 h 14.68158 v 40.985047 h 14.62318 l -' +
          '21.92969,39.17057 -21.88973,-39.17057 h 14.51466 z"/>'#13#10'</svg>'
        FixedColor = clWhite
      end
      item
        IconName = 'markdown-black-gray'
        SVGText = 
          '<?xml version="1.0" encoding="UTF-8" standalone="no"?>'#13#10'<svg wid' +
          'th="200mm" height="200mm" viewBox="0 0 200 200">'#13#10'  <path'#13#10'     ' +
          'id="rect849"'#13#10'     style="display:inline;opacity:1;fill:#808080;' +
          'stroke-width:0.669135"'#13#10'     d="M 35.576689,25 C 29.717309,25 25' +
          ',32.68125 25,42.22221 V 157.77779 C 25,167.31871 29.717309,175 3' +
          '5.576689,175 H 164.42332 C 170.2827,175 175,167.31871 175,157.77' +
          '779 V 42.22221 C 175,32.68125 170.2827,25 164.42332,25 Z m 8.344' +
          '986,34.869643 h 14.495189 l 14.367165,29.01939 14.398905,-28.939' +
          '32 14.420426,-0.0798 V 140.13227 H 87.462534 L 87.182934,94.5393' +
          '83 72.784029,123.87404 58.866482,94.933013 58.586874,140.13227 H' +
          ' 43.921675 Z m 82.851875,0 h 14.68158 v 40.985047 h 14.62318 l -' +
          '21.92969,39.17057 -21.88973,-39.17057 h 14.51466 z"/>'#13#10'</svg>'
        FixedColor = clGray
      end
      item
        IconName = 'markdown-white-gray'
        SVGText = 
          '<?xml version="1.0" encoding="UTF-8" standalone="no"?>'#13#10'<svg wid' +
          'th="200mm" height="200mm" viewBox="0 0 200 200">'#13#10'  <path'#13#10'     ' +
          'id="path891"'#13#10'     style="fill:#808080;stroke:none;stroke-width:' +
          '0.669133px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opac' +
          'ity:1"'#13#10'     d="m 35.577626,25.000001 c -5.85934,0 -13.250324,7.' +
          '681211 -13.250324,17.222207 V 157.77781 c 0,9.54093 7.390984,17.' +
          '22219 13.250324,17.22219 H 164.42236 c 5.85934,0 13.25034,-7.681' +
          '26 13.25034,-17.22219 V 42.222208 c 0,-9.540996 -7.391,-17.22220' +
          '7 -13.25034,-17.222207 z m 3.502656,11.795528 H 160.91972 c 3.63' +
          '365,0 6.55979,4.76472 6.55979,10.681542 V 152.5246 c 0,5.9168 -2' +
          '.92614,10.67986 -6.55979,10.67986 H 39.080282 c -3.633659,0 -6.5' +
          '59801,-4.76306 -6.559801,-10.67986 V 47.477071 c 0,-5.916822 2.9' +
          '26142,-10.681542 6.559801,-10.681542 z m 19.336356,23.072449 -14' +
          '.494083,0.0017 v 80.262382 h 14.66512 l 0.278573,-45.200962 13.9' +
          '1745,28.941022 14.39881,-29.334582 0.27959,45.594522 H 101.60283' +
          ' V 59.869643 l -14.420322,0.08004 -14.39881,28.937686 z m 68.356' +
          '202,0.0017 v 40.983372 h -14.51455 l 21.89063,39.17061 21.92853,' +
          '-39.17061 H 141.45433 V 59.869643 Z"/>'#13#10'</svg>'#13#10
        FixedColor = clGray
      end
      item
        IconName = 'home'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <polygon fill="#' +
          'E8EAF6" points="42,39 6,39 6,23 24,6 42,23"/>'#13#10'    <g fill="#C5C' +
          'AE9">'#13#10'        <polygon points="39,21 34,16 34,9 39,9"/>'#13#10'      ' +
          '  <rect x="6" y="39" width="36" height="5"/>'#13#10'    </g>'#13#10'    <pol' +
          'ygon fill="#B71C1C" points="24,4.3 4,22.9 6,25.1 24,8.4 42,25.1 ' +
          '44,22.9"/>'#13#10'    <rect x="18" y="28" fill="#D84315" width="12" he' +
          'ight="16"/>'#13#10'    <rect x="21" y="17" fill="#01579B" width="6" he' +
          'ight="6"/>'#13#10'    <path fill="#FF8A65" d="M27.5,35.5c-0.3,0-0.5,0.' +
          '2-0.5,0.5v2c0,0.3,0.2,0.5,0.5,0.5S28,38.3,28,38v-2C28,35.7,27.8,' +
          '35.5,27.5,35.5z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'arrow-right-thick'
        SVGText = 
          '<?xml version="1.0"?>'#13#10'<svg xmlns="http://www.w3.org/2000/svg" x' +
          'mlns:xlink="http://www.w3.org/1999/xlink" version="1.1" id="mdi-' +
          'arrow-right-thick" width="24" height="24" viewBox="0 0 24 24">'#13#10 +
          '  <path fill="#0d6efd" d="M4,10V14H13L9.5,17.5L11.92,19.92L19.84' +
          ',12L11.92,4.08L9.5,6.5L13,10H4Z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'arrow-left-thick'
        SVGText = 
          '<?xml version="1.0"?>'#13#10'<svg xmlns="http://www.w3.org/2000/svg" x' +
          'mlns:xlink="http://www.w3.org/1999/xlink" version="1.1" id="mdi-' +
          'arrow-left-thick" width="24" height="24" viewBox="0 0 24 24">'#13#10' ' +
          ' <path fill="#198754" d="M20,10V14H11L14.5,17.5L12.08,19.92L4.16' +
          ',12L12.08,4.08L14.5,6.5L11,10H20Z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'information-outline'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <circle fill="#2' +
          '196F3" cx="24" cy="24" r="21"/>'#13#10'    <rect x="22" y="22" fill="#' +
          'fff" width="4" height="11"/>'#13#10'    <circle fill="#fff" cx="24" cy' +
          '="16.5" r="2.5"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'folder-open'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#FFA' +
          '000" d="M38,12H22l-4-4H8c-2.2,0-4,1.8-4,4v24c0,2.2,1.8,4,4,4h31c' +
          '1.7,0,3-1.3,3-3V16C42,13.8,40.2,12,38,12z"/>'#13#10'    <path fill="#F' +
          'FCA28" d="M42.2,18H15.3c-1.9,0-3.6,1.4-3.9,3.3L8,40h31.7c1.9,0,3' +
          '.6-1.4,3.9-3.3l2.5-14C46.6,20.3,44.7,18,42.2,18z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'tools'
        SVGText = 
          '<svg xmlns="http://www.w3.org/2000/svg" width="64" height="64" v' +
          'ersion="1">'#13#10' <rect style="fill:#42a5f5" width="56" height="56" ' +
          'x="4" y="4" rx="3" ry="3"/>'#13#10' <path style="opacity:0.1;fill:#fff' +
          'fff" d="M 7,4 C 5.338,4 4,5.338 4,7 V 8 C 4,6.338 5.338,5 7,5 h ' +
          '50 c 1.662,0 3,1.338 3,3 V 7 C 60,5.338 58.662,4 57,4 Z"/>'#13#10' <pa' +
          'th style="opacity:0.3" d="m 15,16 c -1.662,0 -3,1.338 -3,3 v 30 ' +
          'c 0,1.662 1.338,3 3,3 1.662,0 3,-1.338 3,-3 V 19 c 0,-1.662 -1.3' +
          '38,-3 -3,-3 z m 16,0 c -1.662,0 -3,1.338 -3,3 v 30 c 0,1.662 1.3' +
          '38,3 3,3 1.662,0 3,-1.338 3,-3 V 19 c 0,-1.662 -1.338,-3 -3,-3 z' +
          ' m 16,0 c -1.662,0 -3,1.338 -3,3 v 30 c 0,1.662 1.338,3 3,3 1.66' +
          '2,0 3,-1.338 3,-3 V 19 c 0,-1.662 -1.338,-3 -3,-3 z"/>'#13#10' <path s' +
          'tyle="opacity:0.2" d="m 4,57 v 1 c 0,1.662 1.338,3 3,3 h 50 c 1.' +
          '662,0 3,-1.338 3,-3 v -1 c 0,1.662 -1.338,3 -3,3 H 7 C 5.338,60 ' +
          '4,58.662 4,57 Z"/>'#13#10' <path style="fill:#3f3f3f" d="m 12,29 v 20 ' +
          'c 0,1.662 1.338,3 3,3 1.662,0 3,-1.338 3,-3 V 29 Z m 32,2 v 18 c' +
          ' 0,1.662 1.338,3 3,3 1.662,0 3,-1.338 3,-3 V 31 Z M 28,41 v 8 c ' +
          '0,1.662 1.338,3 3,3 1.662,0 3,-1.338 3,-3 v -8 z"/>'#13#10' <path styl' +
          'e="opacity:0.2" d="m 15,24 c -3.313708,0 -6,2.686292 -6,6 0,3.31' +
          '3708 2.686292,6 6,6 3.313708,0 6,-2.686292 6,-6 0,-3.313708 -2.6' +
          '86292,-6 -6,-6 z m 32,3 c -3.313708,0 -6,2.686292 -6,6 0,3.31370' +
          '8 2.686292,6 6,6 3.313708,0 6,-2.686292 6,-6 0,-3.313708 -2.6862' +
          '92,-6 -6,-6 z m -16,7 c -3.313708,0 -6,2.686292 -6,6 0,3.313708 ' +
          '2.686292,6 6,6 3.313708,0 6,-2.686292 6,-6 0,-3.313708 -2.686292' +
          ',-6 -6,-6 z"/>'#13#10' <path style="fill:#ffffff" d="m 15,23 c -3.3137' +
          '08,0 -6,2.686292 -6,6 0,3.313708 2.686292,6 6,6 3.313708,0 6,-2.' +
          '686292 6,-6 0,-3.313708 -2.686292,-6 -6,-6 z m 32,3 c -3.313708,' +
          '0 -6,2.686292 -6,6 0,3.313708 2.686292,6 6,6 3.313708,0 6,-2.686' +
          '292 6,-6 0,-3.313708 -2.686292,-6 -6,-6 z m -16,7 c -3.313708,0 ' +
          '-6,2.686292 -6,6 0,3.313708 2.686292,6 6,6 3.313708,0 6,-2.68629' +
          '2 6,-6 0,-3.313708 -2.686292,-6 -6,-6 z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'printer'
        SVGText = 
          '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 48 48">'#13#10' <' +
          'path fill="#424242" d="M9 11h30v3H9z"/>'#13#10' <path fill="#616161" d' +
          '="M4 25h40v-7c0-2.2-1.8-4-4-4H8c-2.2 0-4 1.8-4 4v7z"/>'#13#10' <path f' +
          'ill="#424242" d="M8 36h32c2.2 0 4-1.8 4-4v-8H4v8c0 2.2 1.8 4 4 4' +
          'z"/>'#13#10' <circle fill="#00e676" cx="40" cy="18" r="1"/>'#13#10' <path fi' +
          'll="#90caf9" d="M11 4h26v10H11z"/>'#13#10' <path fill="#242424" d="M37' +
          '.5,31h-27C9.7,31,9,30.3,9,29.5v0c0-0.8,0.7-1.5,1.5-1.5h27c0.8,0,' +
          '1.5,0.7,1.5,1.5v0 C39,30.3,38.3,31,37.5,31z"/>'#13#10' <path fill="#90' +
          'caf9" d="M11 31h26v11H11z"/>'#13#10' <path fill="#42a5f5" d="M11 29h26' +
          'v2H11z"/>'#13#10' <path fill="#1976d2" d="M16 33h17v2H16zm0 4h13v2H16z' +
          '"/>'#13#10'</svg>'
      end
      item
        IconName = 'file-pdf-outline'
        SVGText = 
          '<svg xmlns="http://www.w3.org/2000/svg" width="48" height="48" v' +
          'ersion="1">'#13#10' <path style="opacity:0.2" d="M 10,5 C 8.892,5 8,5.' +
          '892 8,7 v 36 c 0,1.108 0.892,2 2,2 h 28 c 1.108,0 2,-0.892 2,-2 ' +
          'V 17 L 29,16 28,5 Z"/>'#13#10' <path style="fill:#c03630" d="M 10,4 C ' +
          '8.892,4 8,4.892 8,6 v 36 c 0,1.108 0.892,2 2,2 h 28 c 1.108,0 2,' +
          '-0.892 2,-2 V 16 L 29,15 28,4 Z"/>'#13#10' <path style="opacity:0.1;fi' +
          'll:#ffffff" d="M 10,4 C 8.892,4 8,4.892 8,6 V 7 C 8,5.892 8.892,' +
          '5 10,5 h 18 l 11,11 h 1 L 28,4 Z"/>'#13#10' <path style="opacity:0.2" ' +
          'd="m 28,5 v 10 c 0,1.1046 0.89543,2 2,2 h 10 z"/>'#13#10' <path style=' +
          '"fill:#f36961" d="m 28,4 v 10 c 0,1.1046 0.89543,2 2,2 h 10 z"/>' +
          #13#10' <path style="opacity:0.2" d="m 22.78,19.179905 c -0.46126,0 -' +
          '0.89261,0.22577 -0.99696,0.59841 -0.38752,1.4286 0.0462,3.6379 0' +
          '.76956,6.3906 l -0.21821,0.53294 c -0.55387,1.3501 -1.2462,2.694' +
          '7 -1.855,3.8879 -2.5135,4.918 -4.4689,7.5715 -5.7727,7.7574 l -0' +
          '.005,-0.054 c -0.0283,-0.61346 1.1038,-2.1951 2.6382,-3.4526 0.1' +
          '6005,-0.12944 0.84304,-0.79021 0.84304,-0.79021 0,0 -0.92191,0.4' +
          '8667 -1.129,0.61218 -1.9228,1.1477 -2.8796,2.2976 -3.0356,3.0609' +
          ' -0.0463,0.22672 -0.0166,0.5057 0.18375,0.62024 l 0.4916,0.24694' +
          ' c 1.3384,0.66995 2.9841,-1.0916 5.172,-4.9262 2.2264,-0.73036 5' +
          '.0043,-1.418 7.5335,-1.7906 2.264,1.2936 4.861,1.9095 5.8588,1.6' +
          '436 0.18987,-0.0502 0.3896,-0.19924 0.4916,-0.33652 0.08,-0.1263' +
          '1 0.19183,-0.63172 0.19183,-0.63172 0,0 -0.18773,0.25547 -0.3423' +
          ',0.33078 -0.63152,0.29811 -2.6253,-0.19924 -4.6712,-1.2002 1.769' +
          ',-0.18828 3.2427,-0.19554 4.0303,0.0562 1.0003,0.3193 1.0011,0.6' +
          '4659 0.98778,0.71326 0.0135,-0.0549 0.0583,-0.27418 0.0528,-0.36' +
          '753 -0.0227,-0.24006 -0.0967,-0.45443 -0.27795,-0.63172 -0.37027' +
          ',-0.36479 -1.2845,-0.54862 -2.5303,-0.56509 -0.939,-0.0102 -2.06' +
          '5,0.072 -3.2872,0.24694 -0.56012,-0.32164 -1.1512,-0.67522 -1.61' +
          '95,-1.113 -1.1877,-1.1093 -2.1832,-2.6494 -2.8014,-4.376 0.0422,' +
          '-0.16553 0.0826,-0.32727 0.11944,-0.49045 0.17183,-0.77271 0.295' +
          '17,-3.3274 0.29517,-3.3274 0,0 -0.48934,1.9192 -0.56622,2.2087 -' +
          '0.0494,0.18357 -0.11086,0.3795 -0.18147,0.58347 -0.375,-1.3179 -' +
          '0.56509,-2.5952 -0.56509,-3.564 0,-0.27379 0.0235,-0.80656 0.101' +
          '05,-1.2278 0.0378,-0.30045 0.1466,-0.45647 0.2596,-0.53179 0.223' +
          '57,0.0542 0.47383,0.39708 0.73509,0.97054 0.22435,0.4958 0.21017' +
          ',1.07 0.21017,1.4254 0,0 0.24061,-0.87999 0.18492,-1.4001 -0.033' +
          '9,-0.31223 -0.33096,-1.1155 -0.96248,-1.1061 h -0.0517 l -0.2813' +
          '9,-0.003 z m 0.21478,7.9791 c 0.65351,1.314 1.5548,2.5619 2.7371' +
          ',3.5629 0.26357,0.22279 0.544,0.43475 0.83269,0.634 -2.1471,0.39' +
          '931 -4.4021,0.96102 -6.4975,1.8389 0.37891,-0.67309 0.78861,-1.4' +
          '064 1.2083,-2.1972 0.81274,-1.5368 1.3052,-2.7222 1.7194,-3.8385' +
          ' z"/>'#13#10' <path style="fill:#ffffff" d="m 22.78,18.18 c -0.46126,0' +
          ' -0.89261,0.22577 -0.99696,0.59841 -0.38752,1.4286 0.0462,3.6379' +
          ' 0.76956,6.3906 l -0.21821,0.53294 c -0.55387,1.3501 -1.2462,2.6' +
          '947 -1.855,3.8879 -2.5135,4.918 -4.4689,7.5715 -5.7727,7.7574 l ' +
          '-0.005,-0.054 c -0.0283,-0.61346 1.1038,-2.1951 2.6382,-3.4526 0' +
          '.16005,-0.12944 0.84304,-0.79021 0.84304,-0.79021 0,0 -0.92191,0' +
          '.48667 -1.129,0.61218 -1.9228,1.1477 -2.8796,2.2976 -3.0356,3.06' +
          '09 -0.0463,0.22672 -0.0166,0.5057 0.18375,0.62024 l 0.4916,0.246' +
          '94 c 1.3384,0.66995 2.9841,-1.0916 5.172,-4.9262 2.2264,-0.73036' +
          ' 5.0043,-1.418 7.5335,-1.7906 2.264,1.2936 4.861,1.9095 5.8588,1' +
          '.6436 0.18987,-0.0502 0.3896,-0.19924 0.4916,-0.33652 0.08,-0.12' +
          '631 0.19183,-0.63172 0.19183,-0.63172 0,0 -0.18773,0.25547 -0.34' +
          '23,0.33078 -0.63152,0.29811 -2.6253,-0.19924 -4.6712,-1.2002 1.7' +
          '69,-0.18828 3.2427,-0.19554 4.0303,0.0562 1.0003,0.3193 1.0011,0' +
          '.64659 0.98778,0.71326 0.0135,-0.0549 0.0583,-0.27418 0.0528,-0.' +
          '36753 -0.0227,-0.24006 -0.0967,-0.45443 -0.27795,-0.63172 -0.370' +
          '27,-0.36479 -1.2845,-0.54862 -2.5303,-0.56509 -0.939,-0.0102 -2.' +
          '065,0.072 -3.2872,0.24694 -0.56012,-0.32164 -1.1512,-0.67522 -1.' +
          '6195,-1.113 -1.1877,-1.1093 -2.1832,-2.6494 -2.8014,-4.376 0.042' +
          '2,-0.16553 0.0826,-0.32727 0.11944,-0.49045 0.17183,-0.77271 0.2' +
          '9517,-3.3274 0.29517,-3.3274 0,0 -0.48934,1.9192 -0.56622,2.2087' +
          ' -0.0494,0.18357 -0.11086,0.3795 -0.18147,0.58347 -0.375,-1.3179' +
          ' -0.56509,-2.5952 -0.56509,-3.564 0,-0.27379 0.0235,-0.80656 0.1' +
          '0105,-1.2278 0.0378,-0.30045 0.1466,-0.45647 0.2596,-0.53179 0.2' +
          '2357,0.0542 0.47383,0.39708 0.73509,0.97054 0.22435,0.4958 0.210' +
          '17,1.07 0.21017,1.4254 0,0 0.24061,-0.87999 0.18492,-1.4001 -0.0' +
          '339,-0.31223 -0.33096,-1.1155 -0.96248,-1.1061 h -0.0517 l -0.28' +
          '139,-0.003 z m 0.21478,7.9791 c 0.65351,1.314 1.5548,2.5619 2.73' +
          '71,3.5629 0.26357,0.22279 0.544,0.43475 0.83269,0.634 -2.1471,0.' +
          '39931 -4.4021,0.96102 -6.4975,1.8389 0.37891,-0.67309 0.78861,-1' +
          '.4064 1.2083,-2.1972 0.81274,-1.5368 1.3052,-2.7222 1.7194,-3.83' +
          '85 z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'page-next-outline'
        SVGText = 
          '<?xml version="1.0"?>'#13#10'<svg xmlns="http://www.w3.org/2000/svg" x' +
          'mlns:xlink="http://www.w3.org/1999/xlink" version="1.1" id="mdi-' +
          'page-next-outline" width="24" height="24" viewBox="0 0 24 24">'#13#10 +
          '  <path fill="#fd7e14" d="M22,3H5A2,2 0 0,0 3,5V9H5V5H22V19H5V15' +
          'H3V19A2,2 0 0,0 5,21H22A2,2 0 0,0 24,19V5A2,2 0 0,0 22,3M7,15V13' +
          'H0V11H7V9L11,12L7,15M20,13H13V11H20V13M20,9H13V7H20V9M17,17H13V1' +
          '5H17V17Z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'page-previous-outline'
        SVGText = 
          '<?xml version="1.0"?>'#13#10'<svg xmlns="http://www.w3.org/2000/svg" x' +
          'mlns:xlink="http://www.w3.org/1999/xlink" version="1.1" id="mdi-' +
          'page-previous-outline" width="24" height="24" viewBox="0 0 24 24' +
          '">'#13#10'  <path fill="#198754" d="M2,3H19A2,2 0 0,1 21,5V9H19V5H2V19' +
          'H19V15H21V19A2,2 0 0,1 19,21H2A2,2 0 0,1 0,19V5A2,2 0 0,1 2,3M17' +
          ',15V13H24V11H17V9L13,12L17,15M4,13H11V11H4V13M4,9H11V7H4V9M4,17H' +
          '8V15H4V17Z"/>'#13#10'</svg>'#13#10
      end>
    Scaled = True
    Left = 408
    Top = 288
  end
end
