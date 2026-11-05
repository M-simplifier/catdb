@echo off
cd /d "%~dp0"
if not exist "%~dp0data" mkdir "%~dp0data"
if not exist "%~dp0data\reading-notes.catdb" copy "%~dp0samples\reading-notes.catdb" "%~dp0data\reading-notes.catdb" >nul
"%~dp0catdb-studio.exe" --browser --data-dir "%~dp0data" --file "%~dp0data\reading-notes.catdb"
if errorlevel 1 pause
