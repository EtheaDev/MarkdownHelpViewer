<#
.SYNOPSIS
    Rebuilds all MarkdownProcessor packages for Delphi 12 Athens.

.DESCRIPTION
    Calls BuildAllPackages.ps1 -DelphiVersion D12: runtime package
    (MarkdownProcessor) and design-time package (dclMarkdownProcessor).
    The IDE is 64-bit: the design-time package is built for Win32 and Win64.

.PARAMETER BDSRoot
    Root folder of RAD Studio installations (default: C:\BDS\Studio).

.PARAMETER Platform
    Target platform: Win32, Win64 or Both (default: Both).

.PARAMETER Config
    Build configuration: Debug or Release (default: Release).

.PARAMETER Target
    MSBuild target: Build, Clean or Rebuild (default: Rebuild).

.EXAMPLE
    .\BuildAllPackagesD12.ps1
    # Rebuilds the packages for D12 on Win32 + Win64 in Release.

.EXAMPLE
    .\BuildAllPackagesD12.ps1 -Platform Win32 -Target Build -Config Debug
    # Incremental Debug build for D12, only Win32.
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
& $BuildAll -DelphiVersion "D12" -BDSRoot $BDSRoot -Platform $Platform -Config $Config -Target $Target
exit $LASTEXITCODE
