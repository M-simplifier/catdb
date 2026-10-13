param(
  [switch]$Test,
  [switch]$Offline,
  [string]$BuildDir = (Join-Path $PSScriptRoot 'build')
)
$ErrorActionPreference = 'Stop'
$BuildDir = [IO.Path]::GetFullPath($BuildDir)
New-Item -ItemType Directory -Force -Path $BuildDir, (Join-Path $BuildDir 'tmp'), (Join-Path $BuildDir 'logs') | Out-Null
$previousTemp = $env:TEMP
$previousTmp = $env:TMP
Push-Location $PSScriptRoot
try {
  # GHC's resource compiler also uses TEMP, independently of -tmpdir.
  $env:TEMP = Join-Path $BuildDir 'tmp'
  $env:TMP = $env:TEMP
  # A host's build-summary can point outside the writable workspace. Keep
  # dependencies in the existing store, but all build logs and temp files here.
  $cabalPaths = (& cabal path --output-format=json | ConvertFrom-Json)
  if ($LASTEXITCODE -ne 0) { throw 'cabal path failed' }
  $localConfig = Join-Path $BuildDir 'cabal.config'
  $localConfigText = [IO.File]::ReadAllText($cabalPaths.'config-file')
  $localConfigText = [regex]::Replace($localConfigText, '(?m)^build-summary:.*\r?$', '')
  $localConfigText = [regex]::Replace($localConfigText, '(?m)^logs-dir:.*\r?$', '')
  $localConfigText += "`nlogs-dir: " + (Join-Path $BuildDir 'logs')
  $localConfigText += "`nbuild-summary: " + (Join-Path $BuildDir 'logs\build.log') + "`n"
  [IO.File]::WriteAllText($localConfig, $localConfigText)
  $globalArgs = @('--config-file=' + $localConfig)
  $buildArgs = @('--builddir=' + (Join-Path $BuildDir 'dist'))
  if ($Offline) { $buildArgs += '--offline' }
  & cabal @globalArgs build all @buildArgs
  if ($LASTEXITCODE -ne 0) { throw 'cabal build failed' }
  if ($Test) {
    $testDirectory = Join-Path $BuildDir ('tests-' + [Guid]::NewGuid().ToString('N'))
    & cabal @globalArgs test laws @buildArgs --test-show-details=direct "--test-options=$testDirectory"
    if ($LASTEXITCODE -ne 0) { throw 'cabal test failed' }
  }
  foreach ($target in @('catdb', 'relay-desk')) {
    $executablePath = & cabal @globalArgs list-bin "exe:$target" @buildArgs
    if ($LASTEXITCODE -ne 0) { throw 'cabal list-bin failed' }
    Copy-Item -LiteralPath $executablePath.Trim() -Destination (Join-Path $BuildDir "$target.exe") -Force
    Write-Output (Join-Path $BuildDir "$target.exe")
  }
  $assetDir = Join-Path $BuildDir 'relay-web'
  New-Item -ItemType Directory -Force -Path $assetDir | Out-Null
  Get-ChildItem -LiteralPath (Join-Path $PSScriptRoot 'reference/web') -File | ForEach-Object {
    Copy-Item -LiteralPath $_.FullName -Destination $assetDir -Force
  }
} finally {
  if ($localConfig -and (Test-Path -LiteralPath $localConfig)) {
    Remove-Item -LiteralPath $localConfig
  }
  $env:TEMP = $previousTemp
  $env:TMP = $previousTmp
  Pop-Location
}
