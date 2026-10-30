@echo off
cd /d "%~dp0"
if not exist "%~dp0data" mkdir "%~dp0data"
if not exist "%~dp0data\field-service.catdb" copy /y "%~dp0samples\field-service-source.catdb" "%~dp0data\field-service.catdb" >nul
"%~dp0catdb-studio.exe" --browser --data-dir "%~dp0data" --file "%~dp0data\field-service.catdb"
if errorlevel 1 pause
