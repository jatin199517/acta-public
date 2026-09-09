@echo off
REM Double-click to launch the ACTA app. %~dp0 is this file's folder (with trailing backslash).
cd /d "%~dp0"
REM R does not put itself on PATH by default, so probe the usual locations.
set "RSCRIPT="
for %%P in ("%ProgramFiles%\R" "%ProgramFiles(x86)%\R" "%LOCALAPPDATA%\Programs\R") do (
  if exist "%%~P" for /f "delims=" %%D in ('dir /b /ad /o-n "%%~P\R-*" 2^>nul') do (
    if not defined RSCRIPT if exist "%%~P\%%D\bin\Rscript.exe" set "RSCRIPT=%%~P\%%D\bin\Rscript.exe"
  )
)
if not defined RSCRIPT where Rscript.exe >nul 2>nul && set "RSCRIPT=Rscript.exe"
if not defined RSCRIPT (
  echo R not found. Install R from https://cran.r-project.org
  pause & exit /b 1
)
echo Launching ACTA from %CD%
"%RSCRIPT%" -e "shiny::runApp('ACTA_App.R', launch.browser = TRUE)"
echo.
pause
