object FrmAbout: TFrmAbout
  Left = 651
  Top = 323
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  ClientHeight = 312
  ClientWidth = 436
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poDefault
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    436
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
      '<?xml version="1.0" encoding="UTF-8"?>'#13#10'<svg version="1.1" viewB' +
      'ox="0 0 1500 500" xmlns="http://www.w3.org/2000/svg">'#13#10' <path d=' +
      '"m 29.959524,376.13727c-0.76451-0.65379-6.0819-10.147-9.7722-17.' +
      '448-12.307-24.347-18.221-44.831-19.118-66.237-0.86096-20.522 6.3' +
      '28-41.535 20.128-58.833 16.357-20.505 41.513-34.639 70.082-39.37' +
      '6 7.1343-1.183 11.248-1.5176 20.132-1.637 4.6266-0.0622 8.4572-0' +
      '.15828 8.5124-0.21351 0.0553-0.0552-0.1747-1.3857-0.51098-2.9567' +
      '-0.53126-2.4817-0.61063-3.5816-0.60524-8.387 7e-3 -6.2771 0.3420' +
      '2-8.2278 2.2667-13.199 1.2078-3.1196 1.6207-3.5926 3.0344-3.4764' +
      'l1.0134 0.0833 3.0432 5.9258c5.5025 10.714 10.216 17.563 17.574 ' +
      '25.534 1.6238 1.7589 1.9119 1.962 2.7833 1.962 0.64759 0 1.1636-' +
      '0.19159 1.5461-0.57405l0.57407-0.57406-0.70016-2.8365c-3.6152-14' +
      '.647-2.2014-26.711 4.2573-36.329 2.2951-3.4176 3.1823-4.3009 4.3' +
      '2-4.3009 1.0179 0 1.6943 0.50473 1.8776 1.4011 0.0644 0.31581 0.' +
      '43375 2.2334 0.82046 4.2614 1.8272 9.5824 5.785 20.44 10.921 29.' +
      '961 4.0554 7.5178 7.9975 13.592 16.45 25.346 7.444 10.352 8.7704' +
      ' 12.251 10.854 15.539 7.0094 11.059 9.6667 18.641 9.6451 27.522-' +
      '0.0113 4.6803-0.5214 7.6596-2.0764 12.127-1.0191 2.9278-3.5089 8' +
      '.1077-4.1546 8.6438-0.22033 0.18285-0.85331 0.29518-1.4066 0.249' +
      '57l-1.006-0.083-5.5398-11.061c-6.3211-12.621-9.0304-17.596-10.14' +
      '8-18.633-0.59271-0.54997-1.0452-0.72432-1.8808-0.72432-0.98261 0' +
      '-1.2426 0.14785-2.432 1.3827-2.3867 2.4779-3.8608 6.6791-3.3356 ' +
      '9.5062 0.36987 1.991 0.51832 2.2984 3.5294 7.309 10.68 17.771 14' +
      '.324 32.016 11.222 43.858-1.5491 5.9143-5.8604 11.878-11.474 15.' +
      '87-2.5304 1.7998-3.3379 1.9794-4.2996 0.95558l-0.61494-0.65449 0' +
      '.50117-3.2302c0.65217-4.2032 0.66424-10.016 0.0271-13.035-2.4378' +
      '-11.55-11.258-23.668-24.147-33.174-8.0866-5.9645-17.017-10.226-2' +
      '5.81-12.315-7.9227-1.8827-19.198-2.0312-28.97-0.38191-19.669 3.3' +
      '199-36.586 14.363-48.172 31.447-7.7223 11.386-13.611 25.896-16.1' +
      '91 39.9-1.2558 6.8129-1.5873 10.664-1.5873 18.436 0 7.276 0.3376' +
      '8 11.622 1.3195 16.984 0.78891 4.3082 0.78674 4.7164-0.02831 5.3' +
      '574-0.87842 0.69101-1.7289 0.72922-2.4526 0.11033z" fill="#9c9b9' +
      'b" stroke="#7a7a7a" stroke-width="2"/>'#13#10' <path d="m 209.67,498.7' +
      '6727c-49.73-2.4409-94.499-24.132-120.6-58.433-10.493-13.787-18.0' +
      '28-29.405-21.983-45.562-3.3018-13.493-4.5368-30.212-3.0544-41.34' +
      '9 1.7015-12.783 6.6481-26.898 12.468-35.579 8.5726-12.788 21.628' +
      '-21.916 34.982-24.461 4.1317-0.78725 7.7046-1.0841 8.5095-0.7069' +
      '2 1.0166 0.47634 1.258 1.5346 0.68668 3.0122-1.5375 3.9774-2.547' +
      ' 7.1288-3.1404 9.8039-4.627 20.86 4.2672 41.937 23.959 56.779 22' +
      '.585 17.022 55.759 22.871 85.69 15.111 27.034-7.0098 47.752-24.1' +
      '84 54.636-45.288 2.0096-6.1608 2.3807-8.6319 2.3739-15.805-6e-3 ' +
      '-5.7157-0.0603-6.4431-0.71715-9.5114-0.3912-1.8272-1.3184-5.205-' +
      '2.0606-7.506-5.4327-16.845-15.016-33.609-30.696-53.696-19.119-24' +
      '.493-33.999-44.335-46.613-62.154-41.496-58.622-60.928-99.326-60.' +
      '937-127.64-7e-3 -22.885 12.471-38.088 38.327-46.7 21.684-7.2219 ' +
      '54.628-9.6274 97.438-7.1147 17.919 1.0517 47.601 3.8904 48.503 4' +
      '.6385 1.0235 0.84942 0.94317 2.8249-0.13282 3.266-0.24024 0.0984' +
      '29-2.57 0.58531-5.1774 1.0818-19.711 3.7538-32.714 9.5741-41.244' +
      ' 18.462-4.7938 4.9944-7.7248 10.472-9.1962 17.186-3.8243 17.452 ' +
      '4.0794 41.758 24.368 74.935 8.5349 13.957 18.359 28.39 37.103 54' +
      '.509 4.7815 6.6632 11.436 15.967 14.786 20.675 3.3509 4.7077 7.3' +
      '44 10.159 8.8736 12.115 8.5163 10.888 17.588 24.64 23.713 35.95 ' +
      '17.538 32.378 24.604 63.991 21.134 94.548-0.21386 1.8831-0.5746 ' +
      '5.1422-0.80164 7.2427-2.2681 20.983-9.8142 43.451-21.318 63.471-' +
      '14.952 26.022-36.383 47.977-61.367 62.865-30.6 18.235-70.426 27.' +
      '724-108.51 25.855z" fill="#3ba08b" stroke="#227b69" stroke-width' +
      '="2"/>'#13#10' <path d="m 565.03665,411.47 21.235-52.192h153.98v52.192' +
      'zm246.04-227.41h-42.872v-52.192h137.93v52.192h-42.872v227.41h-52' +
      '.192zm123.03-52.192h52.192v279.6h-52.192zm141.66 0h52.192v279.6h' +
      '-52.192zm80.152 227.41h137.93v52.192h-137.93zm242.67-61.512h-76.' +
      '773v-52.192h55.547l-25.015-61.512h-30.532v-52.192h61.512l113.7 2' +
      '79.6h-52.192zm-393.66-52.192h52.192v52.192h-52.192zm150.99 0h137' +
      '.93v52.192h-137.93zm-523.44 0h107.76v52.192h-128.97zm25.015-61.5' +
      '12 21.235-52.192h61.512v52.192zm498.42-52.192h137.93v52.192h-137' +
      '.93z" fill="#3ba08b" stroke="#227b69" stroke-width="4"/>'#13#10'</svg>' +
      #13#10
  end
  object Panel1: TPanel
    Left = 0
    Top = 261
    Width = 436
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
      Cancel = True
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
      'StyledComponents'
      'https://github.com/EtheaDev/StyledComponents'
      ''
      'Delphi MarkdownProcessor'
      'https://github.com/EtheaDev/MarkdownProcessor'
      ''
      'Third parties libraries:'
      'OpenSLL Library: Cryptography and SSL/TLS Toolkit'
      'Copyright '#169' 1998-2018 The OpenSSL Project.  All rights reserved.'
      ''
      'Delphi Markdown'
      'https://github.com/grahamegrieve/delphi-markdown'
      
        'Copyright (c) 2011+, Health Intersections Pty Ltd All rights res' +
        'erved'
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
