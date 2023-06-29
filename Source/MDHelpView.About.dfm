object FrmAbout: TFrmAbout
  Left = 651
  Top = 323
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  ClientHeight = 312
  ClientWidth = 431
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poDefault
  OnClose = FormClose
  OnCreate = FormCreate
  DesignSize = (
    431
    312)
  TextHeight = 13
  object TitleLabel: TLabel
    Left = 190
    Top = 8
    Width = 187
    Height = 41
    AutoSize = False
    Caption = 'Markdown HelpViewer'
    WordWrap = True
  end
  object LabelVersion: TLabel
    Left = 400
    Top = 64
    Width = 35
    Height = 13
    Alignment = taRightJustify
    Caption = 'Version'
  end
  object SVGIconImage1: TSVGIconImage
    Left = 383
    Top = 8
    Width = 52
    Height = 49
    AutoSize = False
    SVGText = 
      '<?xml version="1.0" encoding="UTF-8"?>'#13#10'<svg width="100" height=' +
      '"100" version="1.1" xmlns="http://www.w3.org/2000/svg">'#13#10' <path ' +
      'd="m87.77 23.809v71.504c0 2.598-2.09 4.688-4.687 4.688h-65.626c-' +
      '2.598 0-4.687-2.09-4.687-4.687v-90.626c0-2.598 2.09-4.688 4.687-' +
      '4.688h46.485z" fill="#3771c8" opacity=".995"/>'#13#10' <path d="M63.94' +
      '2 23.809L87.77 47.637V23.809z" opacity=".4"/>'#13#10' <path d="M86.403' +
      ' 20.508L67.262 1.367C66.383.488 65.192 0 63.942 0v23.809H87.77c0' +
      '-1.23-.488-2.422-1.367-3.301z" fill="#afc6e9"/>'#13#10' <path d="m31.9' +
      '89 84.202v-32.463h6.275l6.275 11.935 6.275-11.935h6.275v32.463h-' +
      '6.275v-18.619l-6.275 11.935-6.275-11.935v18.619zm39.219 0-9.4126' +
      '-15.754h6.275v-16.709h6.275v16.709h6.275z" display="none" opacit' +
      'y=".996" stroke-width=".38702"/>'#13#10' <path d="m29.106 82.326v-39.5' +
      '18h9.4299l9.4299 14.529 9.4299-14.529h9.4299v39.518h4.6084l-9.19' +
      '88 12.011-9.2002-12.011h4.3608v-22.665l-9.4299 14.529-9.4299-14.' +
      '529v22.665z" display="none" opacity=".995" stroke-width=".34045"' +
      '/>'#13#10' <path d="m22.059 24.408q0-3.0924 1.9857-6.25 1.9857-3.1901 ' +
      '5.7943-5.2734t8.8867-2.0833q4.7201 0 8.3333 1.7578 3.6133 1.7253' +
      ' 5.5664 4.7201 1.9857 2.9948 1.9857 6.5104 0 2.7669-1.1393 4.850' +
      '3-1.1068 2.0833-2.6693 3.6133-1.5299 1.4974-5.5339 5.0781-1.1068' +
      ' 1.0091-1.7904 1.7904-0.65104 0.7487-0.97656 1.3997-0.32552 0.61' +
      '849-0.52083 1.2695-0.16276 0.61849-0.52083 2.2135-0.61849 3.3854' +
      '-3.8737 3.3854-1.6927 0-2.8646-1.1068-1.1393-1.1068-1.1393-3.287' +
      '8 0-2.7344 0.84635-4.7201 0.84636-2.0182 2.2461-3.5156 1.3997-1.' +
      '5299 3.776-3.6133 2.0833-1.8229 2.9948-2.7344 0.94401-0.94401 1.' +
      '5625-2.0833 0.65104-1.1393 0.65104-2.474 0-2.6042-1.9531-4.3945-' +
      '1.9206-1.7904-4.9805-1.7904-3.5807 0-5.2734 1.8229-1.6927 1.7904' +
      '-2.8646 5.306-1.1068 3.6784-4.1992 3.6784-1.8229 0-3.0924-1.2695' +
      '-1.237-1.3021-1.237-2.7995zm15.885 35.677q-1.9857 0-3.4831-1.269' +
      '5-1.4648-1.3021-1.4648-3.6133 0-2.0508 1.4323-3.4505t3.5156-1.39' +
      '97q2.0508 0 3.4505 1.3997t1.3997 3.4505q0 2.2786-1.4648 3.5807t-' +
      '3.3854 1.3021z" fill="#fff"/>'#13#10' <path d="m12.77 70.886v24.428c0 ' +
      '2.597 2.0895 4.6875 4.6875 4.6875h65.625c2.597 0 4.6875-2.0895 4' +
      '.6875-4.6875v-24.428zm14.84 3.7734h5.8477l5.8477 7.9297 5.8477-7' +
      '.9297h5.8477v21.568h-5.8477v-12.369l-5.8477 7.9297-5.8477-7.9297' +
      'v12.369h-5.8477zm33.623 0h5.8496v11.102h5.8477l-8.7715 10.467-8.' +
      '7715-10.467h5.8457z"/>'#13#10'</svg>'#13#10
  end
  object SVGIconImage2: TSVGIconImage
    Left = 9
    Top = 3
    Width = 175
    Height = 53
    AutoSize = False
    SVGText = 
      '<svg xmlns="http://www.w3.org/2000/svg" width="755.906" height="' +
      '226.772" viewBox="0 0 200 60">'#13#10' <path fill="#005e98" filter="ur' +
      'l(#A)" transform="matrix(.8 0 0 .8 18 6)" d="M0 60l4.557-11.2H37' +
      '.6V60zm52.8-48.8h-9.2V0h29.6v11.2H64V60H52.8zM79.2 0h11.2v60H79.' +
      '2zm30.4 0h11.2v60h-11.2zm17.2 48.8h29.6V60h-29.6zm52.075-13.2H16' +
      '2.4V24.4h11.92l-5.368-13.2H162.4V0h13.2L200 60h-11.2zM94.4 24.4h' +
      '11.2v11.2H94.4zm32.4 0h29.598v11.2H126.8zm-112.325 0H37.6v11.2H9' +
      '.924zm5.368-13.2L24.4 0h13.2v11.2zM126.8 0h29.6v11.2h-29.6z"/>'#13#10 +
      '</svg>'
  end
  object Panel1: TPanel
    Left = 0
    Top = 261
    Width = 431
    Height = 51
    Align = alBottom
    BevelOuter = bvNone
    Color = clWindow
    ParentBackground = False
    TabOrder = 0
    object btnOK: TButton
      Left = 360
      Top = 16
      Width = 75
      Height = 25
      Caption = 'CLOSE'
      Default = True
      TabOrder = 2
      OnClick = btnOKClick
    end
    object btnIssues: TButton
      Left = 8
      Top = 16
      Width = 125
      Height = 25
      Caption = 'Submit issue...'
      ImageIndex = 0
      TabOrder = 0
      OnClick = btnIssuesClick
    end
    object btnCheckUpdates: TButton
      Left = 139
      Top = 16
      Width = 125
      Height = 25
      Caption = 'Check for updates'
      ImageIndex = 3
      TabOrder = 1
      Visible = False
      OnClick = btnCheckUpdatesClick
    end
  end
  object MemoCopyRights: TMemo
    Left = 8
    Top = 88
    Width = 427
    Height = 166
    Anchors = [akLeft, akTop, akBottom]
    Color = clBtnFace
    Lines.Strings = (
      'Author:'
      'Carlo Barazzetta'
      'https://github.com/EtheaDev/MarkdownHelpViewer'
      'Copyright '#169' 2023 all rights reserved.'
      'Contributors:'
      'Nicol'#242' Boccignone, Emanuele Biglia'
      ''
      'Other libraries from Ethea:'
      'SVGIconImageList'
      'https://github.com/EtheaDev/SVGIconImageList/'
      ''
      'Delphi Markdown'
      'https://github.com/grahamegrieve/delphi-markdown'
      
        'Copyright (c) 2011+, Health Intersections Pty Ltd All rights res' +
        'erved.'
      ''
      'Synopse/SynPDF https://github.com/synopse/SynPDF'
      'Copyright '#169' Synopse: all right reserved.'
      ''
      'HtmlToPdf https://github.com/MuzioValerio/HtmlToPdf'
      'Copyright '#169' Muzio Valerio.'
      ''
      
        '- Image32 Library - http://www.angusj.com/delphi/image32/Docs/_B' +
        'ody.htm'
      'Copyright '#169'2019-2023 Angus Johnson.'
      ''
      'HTMLViewer - https://github.com/BerndGabriel/HtmlViewer'
      'Copyright (c) 1995 - 2008 by L. David Baldwin'
      'Copyright (c) 1995 - 2023 by Anders Melander (DitherUnit.pas)'
      'Copyright (c) 1995 - 2023 by Ron Collins (HtmlGif1.pas)'
      'Copyright (c) 2008 - 2009 by Sebastian Zierer (Delphi 2009 Port)'
      'Copyright (c) 2008 - 2010 by Arvid Winkelsdorf (Fixes)'
      'Copyright (c) 2009 - 2023 by HtmlViewer Team')
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object LinkLabel1: TLinkLabel
    Left = 8
    Top = 62
    Width = 282
    Height = 19
    Caption = 
      '<a href="https://github.com/EtheaDev/MarkdownHelpViewer">https:/' +
      '/github.com/EtheaDev/MarkdownHelpViewer</a>'
    TabOrder = 2
    UseVisualStyle = True
    OnLinkClick = LinkLabel1LinkClick
  end
end
