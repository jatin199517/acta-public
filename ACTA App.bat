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
REM The app moved under inst\pipeline\ when 3.0 became a package, and this launcher still
REM asked for ACTA_App.R at the root. Forward slashes in the R string: a backslash there is
REM an escape, and 'inst\pipeline\...' is not a valid R literal.
set "APP=inst/pipeline/ACTA_App.R"
if not exist "inst\pipeline\ACTA_App.R" set "APP=ACTA_App.R"
if not exist "%APP%" (
  echo ACTA_App.R not found next to this launcher.
  pause & exit /b 1
)
REM The app reads its working folder from here; otherwise it would use its own code folder.
set "ACTA_WORK_DIR=%CD%"
echo Launching ACTA from "%CD%"
"%RSCRIPT%" -e "shiny::runApp('%APP%', launch.browser = TRUE)"
echo.
pause
