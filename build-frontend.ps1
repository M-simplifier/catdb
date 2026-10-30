param([string]$Distribution='Ubuntu',[string]$OutputDirectory='')
$ErrorActionPreference='Stop'
$frontendPath=Join-Path $PSScriptRoot 'studio\frontend'
$linuxPath=& wsl.exe --distribution $Distribution --exec wslpath -a -u $frontendPath
if($LASTEXITCODE -ne 0){throw 'Could not locate the frontend in WSL.'}
$webPath=if($OutputDirectory){[IO.Path]::GetFullPath($OutputDirectory)}else{Join-Path $PSScriptRoot 'studio\web'}
$linuxOutput=& wsl.exe --distribution $Distribution --exec wslpath -a -u $webPath
if($LASTEXITCODE -ne 0){throw 'Could not locate the output directory in WSL.'}
& wsl.exe --distribution $Distribution --cd $linuxPath --exec /nix/var/nix/profiles/default/bin/nix develop .#wasm --command env ('CATDB_WEB_OUTPUT='+$linuxOutput) bash ./build.sh
if($LASTEXITCODE -ne 0){throw 'The Miso WASM build failed.'}
Get-Item -LiteralPath (Join-Path $webPath 'studio-gui.wasm') | Select-Object Name,Length
