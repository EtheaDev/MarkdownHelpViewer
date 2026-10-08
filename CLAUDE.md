# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Markdown Help Viewer is a Delphi VCL Windows application and component library by Ethea S.r.l. that provides a Markdown-based help system for Delphi applications. It has three deliverables:

1. **MDHelpViewer.exe** - Standalone viewer application for Markdown/HTML help files
2. **MarkDownHelpViewer.pas** (`Source/AppInterface/`) - Unit implementing `ICustomHelpViewer`/`IExtendedHelpViewer` to integrate with the Delphi Help System
3. **TMarkdownViewer** component (`Source/Components/MarkDownViewerComponents.pas`) - VCL component inheriting from `THtmlViewer` for embedding Markdown rendering

**Viewer components architecture** (plan agreed in Oct 2026, in progress):
- `MarkDownViewerCommon.pas` - no HTMLViewer/Edge dependency: `TMarkdownViewerEngine` (a `TCustomMarkdownToHTML` of the Markdown Processor, unit `MarkdownProcessorComponents`), help-file resolution (`FindMarkdownHelpFile`, `ResolveHelpFile`), link routing (`HandleLinkClicked`), `TFolderName` (a distinct type: the design-time folder editor applies only to it), file utilities, the viewers' default CSS (`GetMarkdownDefaultCSS`, different from the Processor's `MarkdownDefaultCSS`).
- The viewers hold a `TMarkdownViewerEngine` and forward the Markdown properties with the same names as `TMarkdownToHTML`: `ProcessorDialect` (default `mdGitHub`), `Extensions` (dialect defaults + `LegacyExtensions`), `AllowUnsafe`, `MathRendering`, `CssStyle`, `MarkdownContent`, `HtmlContent`. The viewer is refreshed by the `OnChange` of the engine's `HtmlContent`; `ReadState` wraps the DFM reading in `BeginUpdate`/`EndUpdate` (a single conversion).
- `TMarkdownViewer` (HTMLViewer, XE6…D11, optional on D12+): `MathRendering` default `mmrCodeCogsImage` (no JavaScript).
- `TEdgeMarkdownViewer` (unit `MarkDownEdgeViewerComponents`, derives from `TCustomEdgeBrowser`, D12+ i.e. `CompilerVersion >= 36`, no `.inc` file): `MathRendering` default `mmrMarkup`, KaTeX/mermaid from CDN or `ScriptsFolder`. The page is built by `BuildPage` (base style from `DefFontName`/`DefFontSize`/`DefFontColor`/`DefBackground`/`DefHotSpotColor`, alert styles, scripts only when needed) and shown with `NavigateToString`; relative images/links resolve through the virtual host `mdviewer-content.local` mapped on the document folder; clicked links are cancelled in `NavigationStarting` and routed through `HandleLinkClicked`; the WebView is created on demand (`EnsureWebView`), `UserDataFolder` defaults to `%LOCALAPPDATA%\<exe>\WebView2`; `EdgeAvailable` checks `WebView2Loader.dll` + runtime. Needs `WebView2Loader.dll` (32/64) next to the exe (GetIt "EdgeView2 SDK"; not in the BDS folder).
- Icons: `Packages\MarkDownViewer.dcr`, `Packages\EdgeMarkDownViewer.dcr` (design-time packages only).
- Packages up to D11: `MarkDownViewer` (Common + `TMarkdownViewer`, requires `FrameViewer`), `dclMarkDownViewer` (`MarkDownViewerDesign` + `MarkDownViewerRegister`). D12/D13: `MarkDownViewer` (Common + `TEdgeMarkdownViewer`, requires `vcledge`, no `FrameViewer`), `dclMarkDownViewer` (`MarkDownViewerDesign` + `MarkDownEdgeViewerRegister`) and the optional `MarkDownViewerHTML`/`dclMarkDownViewerHTML` (`TMarkdownViewer`, require `FrameViewer`). `MarkDownViewerDesign.pas` holds the shared design-time editors (folder property, "Load from file..." verb).

Supports Delphi XE6 through Delphi 13, both Win32 and Win64.

## Build Commands

Build requires Delphi (RAD Studio) with MSBuild. The build script (`Build.bat`) uses RAD Studio 37.0 (Delphi 13):

```batch
call "C:\BDS\Studio\37.0\bin\rsvars.bat"
msbuild.exe "Source\MDHelpViewer.dproj" /target:Clean;Build /p:Platform=Win64 /p:config=release
msbuild.exe "Source\MDHelpViewer.dproj" /target:Clean;Build /p:Platform=Win32 /p:config=release
```

Output binaries: `Bin32/MDHelpViewer.exe` and `Bin64/MDHelpViewer.exe`.

The installer is built with Inno Setup 6 from `Setup/MarkDownHelpViewerSetup.iss`.

There is no automated test suite. Testing is done manually via the demo applications in `Demo/Source/`.

## Key Compiler Defines

- `STYLEDCOMPONENTS` - Enables StyledComponents UI framework
- `SKIA` - Enables SKIA rendering engine (used with StyledComponents)
- `NO_VCL_STYLES` - Disables standard VCL styles

## Architecture

**Data flow**: Load Markdown file -> Parse with MarkdownProcessor (GitHub default, GFM, CommonMark, DaringFireball, TxtMark dialects) -> Convert to HTML -> Render in THtmlViewer -> Display with theme/CSS styling.

**Rendering of MDHelpViewer.exe**: the main form (`MDHelpView.Main.pas`) keeps its own conversion (`TransformMDToHTML`: dialect from the settings, extensions = `TMarkdownViewerEngine.DefaultExtensions`, CSS chooser) and two `THtmlViewer` in the DFM (`HtmlViewer`, `HtmlViewerIndex`). Built with Delphi 12+ (`{$IF CompilerVersion >= 36}`), when `TEdgeMarkdownViewer.EdgeAvailable` it creates two `TEdgeMarkdownViewer` in their place (`CreateEdgeViewers`, the THtmlViewers are hidden): `ShowMarkdownAsHTML` sends them the HTML, links go through `EdgeViewerFileNameClicked`/`EdgeViewerURLClicked` (same rules as `HtmlViewerHotSpotClick`), PDF uses WebView2 `PrintToPDF` (asynchronous, margins converted from hundredths of cm to inches); math is `mmrMarkup` with WebView2, `mmrCodeCogsImage` with HTMLViewer. Otherwise everything stays on HTMLViewer. `WebView2Loader.dll` is in `Bin32`/`Bin64` and installed by the Setup (with `Setup\LICENSE-WebView2.txt`).

**Main application units** (`Source/`):
- `MDHelpViewer.dpr` - Program entry point; creates splash, data module, then main form
- `MDHelpView.Main.pas` - Main form with tabbed UI (Index, Files, Search), toolbar, HTML viewer
- `MDHelpView.Resources.pas` - TDataModule holding shared resources (images, actions)
- `MDHelpView.Settings.pas` / `MDHelpView.SettingsForm.pas` - Settings persistence and UI
- `MDHelpView.Messages.pas` - Localized UI strings
- `MDHelpView.FormsHookTrx.pas` - Form hooks for ISMultiLanguage translation system
- `vmHtmlToPdf.pas` - PDF export via SynPDF
- `GitHubAPI.pas` - GitHub release checking for auto-updates

**Help file resolution** (in `MarkDownHelpViewer.pas`): When F1 is pressed in a Delphi app, the interface searches the help folder using these precedence rules:
1. File named as the keyword/context (e.g., `MainForm.md`, `1000.md`)
2. Help name + keyword (e.g., `HomeMainForm.md`)
3. Help name + underscore + keyword (e.g., `Home_MainForm.md`)

**Component packages** (`Packages/`): Each supported Delphi version (DXE6, DXE7, DXE8, D10, D10.1-D10.4, D11, D12, D13) has its own subfolder with runtime (`MarkDownViewer.dpk`) and design-time (`dclMarkDownViewer.dpk`) packages plus a group project, `MarkDownViewerGroup.groupproj`. Up to D11 `MarkDownViewer` **requires** `FrameViewer` (`Ext/HTMLViewer/package`) and `MarkdownProcessor` (`Ext/MarkdownProcessor/Packages`, the library's own package) and the group builds `FrameViewer`, `MarkdownProcessor`, `MarkDownViewer`, `dclMarkDownViewer`. On D12/D13 the group holds only `MarkdownProcessor`, `MarkDownViewer`, `dclMarkDownViewer` (no `FrameViewer`); the optional `MarkDownViewerHTML`/`dclMarkDownViewerHTML` packages are stand-alone projects, outside the group. Build scripts: `Packages\BuildAllPackages<Version>.ps1` (e.g. `BuildAllPackagesD13.ps1`: Rebuild Release Win32+Win64 of the whole group, read from the groupproj; design-time Win64 only on D12+), on top of `BuildAllPackages.ps1` and `BuildPackages.ps1`.

## External Dependencies

All vendored in `Ext/`:
- **HTMLViewer** - HTML rendering component (`THtmlViewer` base class)
- **MarkdownProcessor** - Markdown-to-HTML conversion (multi-dialect)
- **SVGIconImageList** - SVG icon support with Image32
- **StyledComponents** - Modern styled UI with SKIA rendering
- **SynPDF** - PDF generation
- **VCLStyleUtils** - VCL styling utilities and DDetours
- **ISMultiLanguage** - Multi-language translation engine

## Localization

Translation files are XML-based in `TrxRepository/` with subfolders per language: ITA, DEU, FRA, ESP, PTG, RUS. The translation system uses ISMultiLanguage (`CBMultiLanguage` unit).

## Version Control

This repository uses Subversion (`.svn/` metadata present), not Git.
