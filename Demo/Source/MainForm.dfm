object fmMain: TfmMain
  Left = 0
  Top = 0
  HelpType = htKeyword
  HelpKeyword = 'Home'
  Caption = 'Main Form'
  ClientHeight = 449
  ClientWidth = 651
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object RightSplitter: TSplitter
    Left = 476
    Top = 41
    Width = 4
    Height = 408
    Align = alRight
    Visible = False
    ExplicitLeft = 462
  end
  object PageControl: TPageControl
    Left = 0
    Top = 41
    Width = 476
    Height = 408
    ActivePage = BrowseTab
    Align = alClient
    TabOrder = 0
    OnChange = PageControlChange
    object BrowseTab: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'BrowseTab'
      Caption = 'Browse Data'
      object DBGrid1: TDBGrid
        Left = 0
        Top = 0
        Width = 468
        Height = 208
        Align = alClient
        DataSource = DataSource
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -12
        TitleFont.Name = 'Segoe UI'
        TitleFont.Style = []
        Columns = <
          item
            Expanded = False
            FieldName = 'Species No'
            Width = 74
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'Category'
            Width = 105
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'Common_Name'
            Width = 171
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'Species Name'
            Width = 187
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'Length (cm)'
            Width = 99
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'Length_In'
            Width = 91
            Visible = True
          end>
      end
      object BottomPanel: TPanel
        Left = 0
        Top = 208
        Width = 468
        Height = 170
        Align = alBottom
        TabOrder = 1
        object DBMemo: TDBMemo
          Left = 1
          Top = 1
          Width = 256
          Height = 168
          Align = alLeft
          DataField = 'Notes'
          DataSource = DataSource
          ScrollBars = ssVertical
          TabOrder = 0
        end
        object DBImage: TDBImage
          Left = 257
          Top = 1
          Width = 280
          Height = 168
          Align = alLeft
          DataSource = DataSource
          Proportional = True
          Stretch = True
          TabOrder = 1
        end
      end
    end
    object EditTab: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'EditTab'
      Caption = 'Edit Data'
      ImageIndex = 1
      object Label1: TLabel
        Left = 8
        Top = 24
        Width = 58
        Height = 15
        Caption = 'Species No'
        FocusControl = DBEdit1
      end
      object Label2: TLabel
        Left = 211
        Top = 24
        Width = 48
        Height = 15
        Caption = 'Category'
        FocusControl = DBEdit2
      end
      object Label3: TLabel
        Left = 8
        Top = 72
        Width = 86
        Height = 15
        Caption = 'Common Name'
        FocusControl = DBEdit3
      end
      object Label4: TLabel
        Left = 8
        Top = 120
        Width = 74
        Height = 15
        Caption = 'Species Name'
        FocusControl = DBEdit4
      end
      object Label5: TLabel
        Left = 8
        Top = 168
        Width = 65
        Height = 15
        Caption = 'Length (cm)'
        FocusControl = DBEdit5
      end
      object Label6: TLabel
        Left = 211
        Top = 168
        Width = 82
        Height = 15
        Caption = 'Length (inches)'
        FocusControl = DBEdit6
      end
      object Label7: TLabel
        Left = 411
        Top = 24
        Width = 31
        Height = 15
        Caption = 'Notes'
        FocusControl = DBMemo1
      end
      object Label8: TLabel
        Left = 8
        Top = 424
        Width = 34
        Height = 15
        Caption = 'Label8'
      end
      object Label9: TLabel
        Left = 8
        Top = 213
        Width = 33
        Height = 15
        Caption = 'Image'
      end
      object DBEdit1: TDBEdit
        Left = 8
        Top = 40
        Width = 197
        Height = 23
        DataField = 'Species No'
        DataSource = DataSource
        TabOrder = 0
      end
      object DBEdit2: TDBEdit
        Left = 211
        Top = 40
        Width = 194
        Height = 23
        DataField = 'Category'
        DataSource = DataSource
        TabOrder = 1
      end
      object DBEdit3: TDBEdit
        Left = 8
        Top = 88
        Width = 397
        Height = 23
        DataField = 'Common_Name'
        DataSource = DataSource
        TabOrder = 2
      end
      object DBEdit4: TDBEdit
        Left = 8
        Top = 136
        Width = 397
        Height = 23
        DataField = 'Species Name'
        DataSource = DataSource
        TabOrder = 3
      end
      object DBEdit5: TDBEdit
        Left = 8
        Top = 184
        Width = 197
        Height = 23
        DataField = 'Length (cm)'
        DataSource = DataSource
        TabOrder = 4
      end
      object DBEdit6: TDBEdit
        Left = 211
        Top = 184
        Width = 194
        Height = 23
        DataField = 'Length_In'
        DataSource = DataSource
        TabOrder = 5
      end
      object DBMemo1: TDBMemo
        Left = 411
        Top = 40
        Width = 233
        Height = 328
        DataField = 'Notes'
        DataSource = DataSource
        TabOrder = 6
      end
      object DBImage1: TDBImage
        Left = 8
        Top = 440
        Width = 105
        Height = 105
        DataSource = DataSource
        TabOrder = 7
      end
      object DBImage2: TDBImage
        Left = 8
        Top = 232
        Width = 251
        Height = 136
        DataSource = DataSource
        Proportional = True
        Stretch = True
        TabOrder = 8
      end
    end
  end
  object TopPanel: TPanel
    Left = 0
    Top = 0
    Width = 651
    Height = 41
    Align = alTop
    TabOrder = 1
    object TitleLabel: TLabel
      AlignWithMargins = True
      Left = 11
      Top = 7
      Width = 260
      Height = 23
      Margins.Left = 10
      Margins.Top = 6
      Margins.Right = 10
      Margins.Bottom = 10
      Align = alLeft
      AutoSize = False
      Caption = 'Markdown Help Viewer Demo'
    end
    object DBNavigator1: TDBNavigator
      Left = 284
      Top = 1
      Width = 366
      Height = 39
      DataSource = DataSource
      Align = alRight
      TabOrder = 0
    end
  end
  object EmbeddedHelpPanel: TPanel
    Left = 480
    Top = 41
    Width = 171
    Height = 408
    Align = alRight
    TabOrder = 2
    Visible = False
    object HelpTitleLabel: TLabel
      Left = 1
      Top = 1
      Width = 169
      Height = 15
      Align = alTop
      Alignment = taCenter
      Caption = 'Instant Help'
      ExplicitWidth = 64
    end
  end
  object MainMenu: TMainMenu
    Left = 288
    Top = 288
    object FileMenu: TMenuItem
      Caption = '&File'
      object NewMenuitem: TMenuItem
        Caption = '&New'
        HelpContext = 1100
        OnClick = MenuItemClick
      end
      object OpenMenuItem: TMenuItem
        Caption = '&Open...'
        HelpContext = 1100
        OnClick = MenuItemClick
      end
      object SaveMenuItem: TMenuItem
        Caption = '&Save'
        HelpContext = 1100
        OnClick = MenuItemClick
      end
      object SaveAsMenuItem: TMenuItem
        Caption = 'Save &As...'
        HelpContext = 1100
        OnClick = MenuItemClick
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object PrintMenuItem: TMenuItem
        Caption = '&Print...'
        HelpContext = 1100
        OnClick = MenuItemClick
      end
      object PrintSetupMenuItem: TMenuItem
        Caption = 'P&rint Setup...'
        HelpContext = 1100
        OnClick = MenuItemClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object ExitMenuItem: TMenuItem
        Caption = 'E&xit'
        HelpContext = 1100
        OnClick = ExitMenuItemClick
      end
    end
    object EditMenu: TMenuItem
      Caption = '&Edit'
      object UndoMenuItem: TMenuItem
        Caption = '&Undo'
        HelpContext = 1200
        ShortCut = 16474
        OnClick = MenuItemClick
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object CutMenuItem: TMenuItem
        Caption = 'Cu&t'
        HelpContext = 1200
        ShortCut = 16472
        OnClick = MenuItemClick
      end
      object CopyMenuItem: TMenuItem
        Caption = '&Copy'
        HelpContext = 1200
        ShortCut = 16451
        OnClick = MenuItemClick
      end
      object PasteMenuItem: TMenuItem
        Caption = '&Paste'
        HelpContext = 1200
        ShortCut = 16470
        OnClick = MenuItemClick
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object FindMenuItem: TMenuItem
        Caption = '&Find...'
        HelpContext = 1200
        OnClick = MenuItemClick
      end
      object ReplaceMenuItem: TMenuItem
        Caption = 'R&eplace...'
        HelpContext = 1200
        OnClick = MenuItemClick
      end
    end
    object AboutMenu: TMenuItem
      Caption = 'Help'
      object HelpMenuItem: TMenuItem
        Caption = 'Show Help'
        OnClick = HelpMenuItemClick
      end
      object ShowHelpEmbedded: TMenuItem
        AutoCheck = True
        Caption = 'Show Help Embedded'
        OnClick = ShowHelpEmbeddedClick
      end
      object AboutMenuItem: TMenuItem
        Caption = 'About'
        OnClick = AboutMenuItemClick
      end
    end
  end
  object ClientDataSet: TClientDataSet
    Aggregates = <>
    FieldDefs = <>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 344
    Top = 288
    object ClientDataSetSpeciesNo: TFloatField
      FieldName = 'Species No'
    end
    object ClientDataSetCategory: TStringField
      FieldName = 'Category'
      Size = 15
    end
    object ClientDataSetCommon_Name: TStringField
      DisplayLabel = 'Common Name'
      FieldName = 'Common_Name'
      Size = 30
    end
    object ClientDataSetSpeciesName: TStringField
      FieldName = 'Species Name'
      Size = 40
    end
    object ClientDataSetLengthcm: TFloatField
      FieldName = 'Length (cm)'
    end
    object ClientDataSetLength_In: TFloatField
      DisplayLabel = 'Length (inches)'
      FieldName = 'Length_In'
    end
    object ClientDataSetNotes: TMemoField
      FieldName = 'Notes'
      BlobType = ftMemo
      Size = 50
    end
    object ClientDataSetGraphic: TGraphicField
      DisplayLabel = 'Image'
      FieldName = 'Graphic'
      BlobType = ftGraphic
    end
  end
  object DataSource: TDataSource
    DataSet = ClientDataSet
    OnStateChange = DataSourceStateChange
    Left = 344
    Top = 352
  end
end
