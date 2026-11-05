param(
  [string]$NativeBuildDirectory=(Join-Path $PSScriptRoot 'build'),
  [string]$WebDirectory=(Join-Path $PSScriptRoot 'studio\web'),
  [string]$OutputDirectory=(Join-Path $PSScriptRoot 'release'),
  [Parameter(Mandatory=$true)][string]$GhcSourceArchive,
  [string]$CompilerDirectory='C:\ghcup\ghc\9.6.7',
  [string]$HackageCache='C:\cabal\packages\hackage.haskell.org',
  [string]$FieldServiceSamples='',
  [string]$StudioSamples=''
)
$ErrorActionPreference='Stop'
$packageArgs=@('--repo',$PSScriptRoot,'--build',$NativeBuildDirectory,'--web',$WebDirectory,'--output',$OutputDirectory,'--ghc',$CompilerDirectory,'--ghc-source',$GhcSourceArchive,'--cache',$HackageCache)
if($FieldServiceSamples){$packageArgs+=@('--field-service-samples',$FieldServiceSamples)}
if($StudioSamples){$packageArgs+=@('--studio-samples',$StudioSamples)}
& python (Join-Path $PSScriptRoot 'scripts\package_studio.py') @packageArgs
if($LASTEXITCODE -ne 0){throw 'Studio packaging failed. Existing packages are never overwritten.'}
