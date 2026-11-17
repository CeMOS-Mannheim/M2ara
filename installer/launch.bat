@echo off
rem ===========================================================================
rem  M2ara - start the bundled R/Shiny application.
rem  Everything this script needs lives next to it (R\, chrome\).
rem ===========================================================================
setlocal EnableExtensions

set "APPDIR=%~dp0"
pushd "%APPDIR%" >nul 2>&1

if not exist "%APPDIR%R\bin\x64\Rscript.exe" (
    echo.
    echo [M2ara] ERROR: the bundled R installation was not found in
    echo          "%APPDIR%R"
    echo         The installation seems to be incomplete - please reinstall M2ara.
    echo.
    popd >nul 2>&1
    pause
    exit /b 1
)

rem Make sure the bundled library is used, not a per-user library of another R.
set "R_LIBS=%APPDIR%R\library"
set "R_LIBS_USER=%APPDIR%R\library"

echo Starting M2ara ... this window closes together with the application.
echo.

"%APPDIR%R\bin\x64\Rscript.exe" --vanilla "%APPDIR%run.R"
set "EXITCODE=%ERRORLEVEL%"

if not "%EXITCODE%"=="0" (
    echo.
    echo [M2ara] M2ara stopped with exit code %EXITCODE%.
    echo         Please report the messages above at
    echo         https://github.com/CeMOS-Mannheim/M2ara/issues
    echo.
    pause
)

popd >nul 2>&1
exit /b %EXITCODE%
