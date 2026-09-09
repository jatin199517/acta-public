@echo off
REM Double-click to set up ACTA on Windows, or run it from a Command Prompt to pass flags:
REM     Setup.bat --no-tex      skip TeX: everything except the PDF report
REM
REM WHY THIS FILE EXISTS. The README used to say only "Rscript Setup.R", which assumes Rscript is on
REM PATH. The R for Windows installer does NOT add it, so that command fails on a stock install with
REM "'Rscript' is not recognized" -- reported from a Windows test run 2026-09-09. The probe below is
REM the same one "ACTA App.bat" uses; keep the two in step.
cd /d "%~dp0"
set "RSCRIPT="
for %%P in ("%ProgramFiles%\R" "%ProgramFiles(x86)%\R" "%LOCALAPPDATA%\Programs\R") do (
  if exist "%%~P" for /f "delims=" %%D in ('dir /b /ad /o-n "%%~P\R-*" 2^>nul') do (
    if not defined RSCRIPT if exist "%%~P\%%D\bin\Rscript.exe" set "RSCRIPT=%%~P\%%D\bin\Rscript.exe"
  )
)
if not defined RSCRIPT where Rscript.exe >nul 2>nul && set "RSCRIPT=Rscript.exe"
if not defined RSCRIPT (
  echo R not found. Install R 4.4.0 or newer from https://cran.r-project.org
  pause & exit /b 1
)
echo Setting up ACTA from %CD%
REM %* forwards --no-tex and anything else straight through to Setup.R.
"%RSCRIPT%" Setup.R %*
echo.
pause
