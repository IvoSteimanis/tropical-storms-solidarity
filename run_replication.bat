@echo off
REM ============================================================================
REM  Replication driver -- "Moderate tropical cyclone exposure erodes solidarity needed for recovery"
REM
REM  Runs the Stata pipeline (run.do) FIRST, then the R pipeline (run_R.R).
REM  ORDER MATTERS: the R rebuilds of Figures 2, 3 and 5 read CSV files that
REM  run.do writes into results\intermediate\. Running R first will skip those
REM  three figures with a message.
REM
REM  HOW TO USE:
REM    1. Edit the two paths below to match your Stata and R installations.
REM    2. Double-click this file, or run it from a command prompt opened in the
REM       replication_package folder.
REM
REM  NOTE: in Windows batch mode, Stata may take a few minutes to close its
REM  window after the analysis finishes; this is normal -- the script waits for
REM  Stata to exit before starting R.
REM ============================================================================

setlocal EnableExtensions
cd /d "%~dp0"

REM --- EDIT THESE TWO PATHS -------------------------------------------------
set "STATA=C:\Program Files\Stata18\StataMP-64.exe"
set "RSCRIPT=C:\Program Files\R\R-4.3.1\bin\Rscript.exe"
REM -------------------------------------------------------------------------

if not exist "%STATA%"   ( echo ERROR: Stata not found at "%STATA%".   Edit the STATA path in run_replication.bat.   & pause & exit /b 1 )
if not exist "%RSCRIPT%" ( echo ERROR: Rscript not found at "%RSCRIPT%". Edit the RSCRIPT path in run_replication.bat. & pause & exit /b 1 )

echo.
echo [1/2] Stata pipeline (run.do) -- runs FIRST, please wait...
start "" /wait "%STATA%" -b do run.do
if errorlevel 1 ( echo Stata reported an error. Check scripts\logs\ and run.log. & pause & exit /b 1 )

echo.
echo [2/2] R pipeline (run_R.R)...
"%RSCRIPT%" run_R.R
if errorlevel 1 ( echo R reported an error. & pause & exit /b 1 )

echo.
echo ============================================================================
echo  Replication complete. Outputs:
echo    results\figures\   (Figures 1-5 + SI figures)
echo    results\tables\    (Tables S1-S33)
echo    results\R_output\  (headline numbers)
echo ============================================================================
pause
endlocal
