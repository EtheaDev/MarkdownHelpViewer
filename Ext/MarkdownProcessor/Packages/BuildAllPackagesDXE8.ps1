<#
.SYNOPSIS
    Rebuilds all MarkdownProcessor packages for Delphi XE8.

.DESCRIPTION
    Calls BuildAllPackages.ps1 -DelphiVersion DXE8: runtime package
    (MarkdownProcessor) and design-time package (dclMarkdownProcessor).
    The IDE is 32-bit: the design-time package is built for Win32
    only; the runtime package for Win32 and Win64.

.PARAMETER BDSRoot
    Root folder of RAD Studio installations (default: C:\BDS\Studio).

.PARAMETER Platform
    Target platform: Win32, Win64 or Both (default: Both).

.PARAMETER Config
    Build configuration: Debug or Release (default: Release).

.PARAMETER Target
    MSBuild target: Build, Clean or Rebuild (default: Rebuild).

.EXAMPLE
    .\BuildAllPackagesDXE8.ps1
    # Rebuilds the packages for DXE8 on Win32 + Win64 in Release.

.EXAMPLE
    .\BuildAllPackagesDXE8.ps1 -Platform Win32 -Target Build -Config Debug
    # Incremental Debug build for DXE8, only Win32.
#>
param(
    [string]$BDSRoot = "C:\BDS\Studio",

    [ValidateSet("Win32", "Win64", "Both")]
    [string]$Platform = "Both",

    [ValidateSet("Debug", "Release")]
    [string]$Config = "Release",

    [ValidateSet("Build", "Clean", "Rebuild")]
    [string]$Target = "Rebuild"
)

$BuildAll = Join-Path $PSScriptRoot "BuildAllPackages.ps1"
& $BuildAll -DelphiVersion "DXE8" -BDSRoot $BDSRoot -Platform $Platform -Config $Config -Target $Target
exit $LASTEXITCODE
