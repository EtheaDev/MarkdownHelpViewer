<#
.SYNOPSIS
    Rebuilds all the packages of the MarkdownHelpViewer group for one Delphi version.

.DESCRIPTION
    Wrapper around BuildPackages.ps1 that builds, for each requested platform,
    the projects of MarkDownViewerGroup.groupproj in the order of the
    group: FrameViewer (Ext\HTMLViewer), MarkdownProcessor
    (Ext\MarkdownProcessor), MarkDownViewer and dclMarkDownViewer, each one
    requiring the previous ones.
    At the end prints a summary table with the result of each
    (package, platform) combination.

    Design-time packages are built for Win64 only on D12 and later: before
    D12 the IDE is 32-bit and loads only Win32 design-time BPLs, so those
    combinations are reported as SKIP.

    The BuildAllPackages<Version>.ps1 scripts call this one with their
    version.

.PARAMETER DelphiVersion
    Package folder / Delphi version: DXE6 ... D13 (default: D13).

.PARAMETER BDSRoot
    Root folder of RAD Studio installations (default: C:\BDS\Studio).

.PARAMETER Platform
    Target platform: Win32, Win64 or Both (default: Both).

.PARAMETER Config
    Build configuration: Debug or Release (default: Release).

.PARAMETER Target
    MSBuild target: Build, Clean or Rebuild (default: Rebuild).

.EXAMPLE
    .\BuildAllPackages.ps1 -DelphiVersion D12
    # Rebuilds the whole group for D12 on Win32 + Win64 in Release.

.EXAMPLE
    .\BuildAllPackages.ps1 -DelphiVersion D13 -Platform Win64 -Target Build
    # Builds (incremental) the whole group for D13, only Win64.
#>
param(
    [ValidateSet("DXE6", "DXE7", "DXE8", "D10", "D10.1", "D10.2", "D10.3", "D10.4", "D11", "D12", "D13")]
    [string]$DelphiVersion = "D13",

    [string]$BDSRoot = "C:\BDS\Studio",

    [ValidateSet("Win32", "Win64", "Both")]
    [string]$Platform = "Both",

    [ValidateSet("Debug", "Release")]
    [string]$Config = "Release",

    [ValidateSet("Build", "Clean", "Rebuild")]
    [string]$Target = "Rebuild"
)

$ErrorActionPreference = "Stop"

$ScriptDir = $PSScriptRoot
$BuildScript = Join-Path $ScriptDir "BuildPackages.ps1"
if (-not (Test-Path $BuildScript)) {
    Write-Error "BuildPackages.ps1 not found in $ScriptDir"
    exit 1
}

# the projects of the group, in build order (paths relative to the version folder)
$GroupFile = Join-Path (Join-Path $ScriptDir $DelphiVersion) "MarkDownViewerGroup.groupproj"
if (-not (Test-Path $GroupFile)) {
    Write-Error "Group project not found: $GroupFile"
    exit 1
}
$Packages = @(Select-String -Path $GroupFile -Pattern '<Projects Include="([^"]+)"' -AllMatches |
    ForEach-Object { $_.Matches } | ForEach-Object { $_.Groups[1].Value })
if ($Platform -eq "Both") { $Platforms = @("Win32", "Win64") } else { $Platforms = @($Platform) }

# 64-bit IDE (and Win64 design-time packages) from D12 on
$Win64DesignTime = @("D12", "D13") -contains $DelphiVersion

Write-Host ""
Write-Host "============================================" -ForegroundColor Cyan
Write-Host " BuildAllPackages $DelphiVersion" -ForegroundColor Cyan
Write-Host "============================================" -ForegroundColor Cyan
Write-Host " Packages:  $(($Packages | ForEach-Object { Split-Path -Leaf $_ }) -join ', ')" -ForegroundColor White
Write-Host " Platforms: $($Platforms -join ', ')" -ForegroundColor White
Write-Host " Config:    $Config" -ForegroundColor White
Write-Host " Target:    $Target" -ForegroundColor White
Write-Host "============================================" -ForegroundColor Cyan

$Results = @()
$Failed = 0
$StartTime = Get-Date

foreach ($plat in $Platforms) {
    foreach ($pkg in $Packages) {
        $pkgName = Split-Path -Leaf $pkg
        if (($pkgName -like "dcl*") -and ($plat -eq "Win64") -and -not $Win64DesignTime) {
            $Results += [pscustomobject]@{
                Package  = $pkgName
                Platform = $plat
                Status   = "SKIP"
                ExitCode = ""
                Seconds  = 0
            }
            continue
        }

        Write-Host ""
        Write-Host "############################################" -ForegroundColor Magenta
        Write-Host "  $DelphiVersion / $pkgName / $plat / $Config" -ForegroundColor Magenta
        Write-Host "############################################" -ForegroundColor Magenta

        $stepStart = Get-Date
        & $BuildScript -DelphiVersion $DelphiVersion -BDSRoot $BDSRoot -Platform $plat -Config $Config -Project $pkg -Target $Target
        $exitCode = $LASTEXITCODE
        $duration = (Get-Date) - $stepStart

        $Results += [pscustomobject]@{
            Package  = $pkgName
            Platform = $plat
            Status   = if ($exitCode -eq 0) { "OK" } else { "FAIL" }
            ExitCode = $exitCode
            Seconds  = [math]::Round($duration.TotalSeconds, 1)
        }
        if ($exitCode -ne 0) { $Failed++ }
    }
}

$totalDuration = (Get-Date) - $StartTime
Write-Host ""
Write-Host "============================================" -ForegroundColor Cyan
Write-Host " SUMMARY [$DelphiVersion]" -ForegroundColor Cyan
Write-Host "============================================" -ForegroundColor Cyan
$Results | Format-Table -AutoSize | Out-String | Write-Host
Write-Host ("Total elapsed: {0:N1} s" -f $totalDuration.TotalSeconds) -ForegroundColor White

$built = @($Results | Where-Object { $_.Status -ne "SKIP" }).Count
if ($Failed -gt 0) {
    Write-Host "$Failed of $built builds FAILED" -ForegroundColor Red
    exit 1
}
else {
    Write-Host "All $built builds SUCCEEDED" -ForegroundColor Green
    exit 0
}
