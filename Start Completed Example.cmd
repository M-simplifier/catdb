@echo off
cd /d "%~dp0"
if not exist "%~dp0data" mkdir "%~dp0data"
if not exist "%~dp0data\field-service-complete.catdb" copy /y "%~dp0samples\field-service-complete.catdb" "%~dp0data\field-service-complete.catdb" >nul
"%~dp0catdb-studio.exe" --browser --data-dir "%~dp0data" --file "%~dp0data\field-service-complete.catdb"
if errorlevel 1 pause
