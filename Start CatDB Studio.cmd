@echo off
cd /d "%~dp0"
"%~dp0catdb-studio.exe" --browser %*
if errorlevel 1 pause
