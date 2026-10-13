@echo off
cd /d "%~dp0"
echo Open http://127.0.0.1:4283 in your browser.
echo Your workspace is desk.catdb in this folder.
relay-desk.exe desk.catdb --demo
pause
