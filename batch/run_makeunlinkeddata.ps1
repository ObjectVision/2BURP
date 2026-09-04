<#
.SYNOPSIS
  Process the MakeUnlinkedData buttons of the CRISP configuration one by one with GeoDmsRun.

.DESCRIPTION
  For every button label in -Steps the script runs
      GeoDmsRun.exe /L<log> cfg\main.dms /MakeUnlinkedData/CreateFiles/<label>
  and, when GeoDMS reports that item as not found (its files already exist),
      GeoDmsRun.exe /L<log> cfg\main.dms /MakeUnlinkedData/RecreateFiles/<label>
  If neither exists the button is "unavailable": a button it depends on has not produced
  its files yet (see MakeUnlinkedData/Buttons_List/depends in cfg\main\MakeUnlinkedData.dms).
  Each step gets its own GeoDMS log; the script stops at the first failing step unless
  -ContinueOnError is given, and prints a table with exit codes and durations.

.PARAMETER StudyArea
  Value for the StudyArea environment variable that ModelParameters/StudyArea picks up
  (Europe, Africa, Netherlands, ...). Defaults to Europe.

.PARAMETER Engine
  Folder of the GeoDMS engine to use. Defaults to the highest-numbered
  C:\Program Files\ObjectVision\GeoDms<major>.<minor>.<patch>.m folder.

.PARAMETER Steps
  Button labels to process, in order. Defaults to the full chain a1 .. f3.

.PARAMETER LogDir
  Where the per-step logs go. Defaults to %LocalDataDir%-style c:\LocalData\CRISP\log.

.EXAMPLE
  .\run_makeunlinkeddata.ps1 -StudyArea Europe
  .\run_makeunlinkeddata.ps1 -StudyArea Africa -Steps c2_PastDoU, d1_Allocation
#>
param(
    [string]   $StudyArea = 'Europe',
    [string]   $Engine    = '',
    [string[]] $Steps     = @('a1_Countries', 'a2_Continents', 'b1_studyarea_def', 'b2_Coastline_InlandWater', 'b3_Hasland',
                              'c1_RoundPastPop_and_Builtup', 'c2_PastDoU', 'd1_Allocation',
                              'e1_reporting_countries', 'e2_reporting_regions',
                              'f1_pop_mozaik', 'f2_builtup_mozaik', 'f3_degurba_mozaik'),
    [string]   $LogDir    = 'c:\LocalData\CRISP\log',
    [switch]   $ContinueOnError
)

$ErrorActionPreference = 'Stop'
$cfg = Join-Path (Split-Path $PSScriptRoot -Parent) 'cfg\main.dms'
if (-not (Test-Path $cfg)) { throw "Configuration not found: $cfg" }

if ($Engine -eq '') {
    $Engine = Get-ChildItem 'C:\Program Files\ObjectVision' -Directory -Filter 'GeoDms*.m' |
        Where-Object { $_.Name -match '^GeoDms\d+\.\d+\.\d+\.m$' } |
        Sort-Object { [version](($_.Name -replace '^GeoDms', '') -replace '\.m$', '') } |
        Select-Object -Last 1 -ExpandProperty FullName
}
$exe = Join-Path $Engine 'GeoDmsRun.exe'
if (-not (Test-Path $exe)) { throw "GeoDmsRun.exe not found in $Engine" }
New-Item -ItemType Directory -Force -Path $LogDir | Out-Null

$env:StudyArea = $StudyArea
Write-Host "Engine   : $exe"
Write-Host "Config   : $cfg"
Write-Host "StudyArea: $StudyArea"
Write-Host "Logs     : $LogDir"

$results = @()
Push-Location (Split-Path $cfg -Parent)
try {
    foreach ($step in $Steps) {
        $outcome = $null
        foreach ($container in 'CreateFiles', 'RecreateFiles') {
            $log = Join-Path $LogDir ("{0}_{1}_{2}.log" -f $StudyArea, $step, $container)
            if (Test-Path $log) { Remove-Item $log }
            $item = "/MakeUnlinkedData/$container/$step"
            Write-Host ("[{0}] {1} ..." -f (Get-Date -Format 'HH:mm:ss'), $item)
            $t0 = Get-Date
            & $exe "/L$log" $cfg $item *> $null
            $rc = $LASTEXITCODE
            $dur = [int]((Get-Date) - $t0).TotalSeconds
            $notFound = (Test-Path $log) -and (Select-String -Path $log -Pattern "because the specified item '.*' was not found" -Quiet)
            if ($notFound) { continue }
            $errors = 0; $fatals = 0
            if (Test-Path $log) {
                $errors = (Select-String -Path $log -Pattern '\[E\]' | Measure-Object).Count
                $fatals = (Select-String -Path $log -Pattern '\[F\]' | Measure-Object).Count
            }
            $outcome = [pscustomobject]@{ Step = $step; Item = $item; ExitCode = $rc; Seconds = $dur; ErrorLines = $errors; FatalLines = $fatals; Log = $log }
            break
        }
        if ($null -eq $outcome) {
            $outcome = [pscustomobject]@{ Step = $step; Item = 'unavailable (dependency not met)'; ExitCode = 3; Seconds = 0; ErrorLines = 0; FatalLines = 0; Log = '' }
        }
        $results += $outcome
        $outcome | Format-Table -AutoSize | Out-String | Write-Host
        if ($outcome.ExitCode -ne 0 -and -not $ContinueOnError) {
            Write-Host "Stopping at $step (exit code $($outcome.ExitCode)); see $($outcome.Log)"
            break
        }
    }
}
finally { Pop-Location }

Write-Host "`nSummary"
$results | Format-Table Step, ExitCode, Seconds, ErrorLines, FatalLines, Item -AutoSize | Out-String | Write-Host
exit ($results | Where-Object { $_.ExitCode -ne 0 } | Measure-Object).Count
