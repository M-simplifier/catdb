param([Parameter(Mandatory)][string]$LawsExe,
      [Parameter(Mandatory)][string]$CatdbExe,
      [Parameter(Mandatory)][string]$DatabasePath)
$ErrorActionPreference = 'Stop'
$processInfo = [Diagnostics.ProcessStartInfo]::new()
$processInfo.FileName = [IO.Path]::GetFullPath($LawsExe)
$processInfo.ArgumentList.Add('hold')
$processInfo.ArgumentList.Add([IO.Path]::GetFullPath($DatabasePath))
$processInfo.UseShellExecute = $false
$processInfo.CreateNoWindow = $true
$processInfo.RedirectStandardInput = $true
$processInfo.RedirectStandardOutput = $true
$processInfo.RedirectStandardError = $true
$child = [Diagnostics.Process]::Start($processInfo)
try {
  $readMarker = $child.StandardOutput.ReadLineAsync()
  if (-not $readMarker.Wait(15000)) { throw 'Child did not acknowledge commit' }
  if ($readMarker.Result -ne 'COMMITTED') { throw $child.StandardError.ReadToEnd() }
  $child.Kill()
  $child.WaitForExit()
  $result = & $CatdbExe path $DatabasePath Task first status
  if ($LASTEXITCODE -ne 0 -or $result -ne '"done"') { throw 'Acknowledged commit was not recovered' }
  Write-Output 'PASS: acknowledged commit survives process kill; OS lock released'
} finally {
  if (-not $child.HasExited) { $child.Kill(); $child.WaitForExit() }
  $child.Dispose()
}
