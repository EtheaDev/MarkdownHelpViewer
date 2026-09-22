@echo off
setlocal

REM Works out whether this batch was started from Explorer, in which case FROM_EXPLORER is set to 1 instead of 0.
set FROM_EXPLORER=0
echo %cmdcmdline% | find /i "%~nx0" > NUL
if not errorlevel 1 (
  REM This batch was started by a program and not from a command prompt window
  echo %cmdcmdline% | find /i "call " > NUL
  if errorlevel 1 (
    REM This batch was started from Explorer and not from another batch or PowerShell script
    set FROM_EXPLORER=1
  )
)

if "%FROM_EXPLORER%" == "1" (
  echo.
  echo ###############################################################################
  echo Building MDHelpViewer ^(Win32 and Win64^) with Delphi 13 ^(Config=Release^)
  echo ###############################################################################
  echo.
  echo Press ENTER to continue, or close this window to abort...
  pause > NUL
)

set BDS=C:\BDS\Studio\37.0
if exist "c:\program files (x86)\embarcadero\studio\37.0" set BDS=c:\program files (x86)\embarcadero\studio\37.0

if not exist "%BDS%\bin\rsvars.bat" (
  echo.
  echo ERROR: Delphi 13 not found
  if "%FROM_EXPLORER%" == "1" (
    echo.
    echo Press ENTER to exit...
    pause > NUL
  )
  exit /b 1
)

call "%BDS%\bin\rsvars.bat"

echo.
echo **************************************************************************
echo Building MDHelpViewer ^(Delphi13/Win64/Release^)
echo **************************************************************************
echo.
msbuild.exe "Source\MDHelpViewer.dproj" /target:Clean;Build /p:Platform=Win64 /p:config=Release /v:minimal /p:DCC_Hints=false
if errorlevel 1 (
  echo.
  echo ERROR building MDHelpViewer ^(Delphi13/Win64/Release^)
  if "%FROM_EXPLORER%" == "1" (
    echo.
    echo Press ENTER to exit...
    pause > NUL
  )
  exit /b 1
)

echo.
echo Adding the digital signature to: D:\ETHEA\MarkDownHelpViewer\Bin64\MDHelpViewer.exe
call D:\ETHEA\Certificate\SignFileWithSectico.bat D:\ETHEA\MarkDownHelpViewer\Bin64\MDHelpViewer.exe
if errorlevel 1 (
  echo.
  echo ERROR signing: D:\ETHEA\MarkDownHelpViewer\Bin64\MDHelpViewer.exe
  if "%FROM_EXPLORER%" == "1" (
    echo.
    echo Press ENTER to continue...
    pause > NUL
  )
)

echo.
echo **************************************************************************
echo Building MDHelpViewer ^(Delphi13/Win32/Release^)
echo **************************************************************************
echo.
msbuild.exe "Source\MDHelpViewer.dproj" /target:Clean;Build /p:Platform=Win32 /p:config=Release /v:minimal /p:DCC_Hints=false
if errorlevel 1 (
  echo.
  echo ERROR building MDHelpViewer ^(Delphi13/Win32/Release^)
  if "%FROM_EXPLORER%" == "1" (
    echo.
    echo Press ENTER to exit...
    pause > NUL
  )
  exit /b 1
)

echo.
echo Adding the digital signature to: D:\ETHEA\MarkDownHelpViewer\Bin32\MDHelpViewer.exe
call D:\ETHEA\Certificate\SignFileWithSectico.bat D:\ETHEA\MarkDownHelpViewer\Bin32\MDHelpViewer.exe
if errorlevel 1 (
  echo.
  echo ERROR signing: D:\ETHEA\MarkDownHelpViewer\Bin32\MDHelpViewer.exe
  if "%FROM_EXPLORER%" == "1" (
    echo.
    echo Press ENTER to continue...
    pause > NUL
  )
)

echo.
echo **********************************************************************************************
echo Building "D:\ETHEA\MarkDownHelpViewer\Setup\MarkDownHelpViewerSetup.iss" with InnoSetup
echo **********************************************************************************************
echo.
"C:\Program Files (x86)\Inno Setup 6\iscc.exe" "D:\ETHEA\MarkDownHelpViewer\Setup\MarkDownHelpViewerSetup.iss"
if errorlevel 1 (
  echo.
  echo ERROR building "D:\ETHEA\MarkDownHelpViewer\Setup\MarkDownHelpViewerSetup.iss" with InnoSetup
  if "%FROM_EXPLORER%" == "1" (
    echo.
    echo Press ENTER to exit...
    pause > NUL
  )
  exit /b 1
)

echo.
echo Adding the digital signature to: D:\ETHEA\MarkDownHelpViewer\Setup\Output\MarkDownHelpViewerSetup.exe
call D:\ETHEA\Certificate\SignFileWithSectico.bat D:\ETHEA\MarkDownHelpViewer\Setup\Output\MarkDownHelpViewerSetup.exe
if errorlevel 1 (
  echo.
  echo ERROR signing: D:\ETHEA\MarkDownHelpViewer\Setup\Output\MarkDownHelpViewerSetup.exe
  if "%FROM_EXPLORER%" == "1" (
    echo.
    echo Press ENTER to continue...
    pause > NUL
  )
)

echo.
echo Done.
if "%FROM_EXPLORER%" == "1" (
  echo.
  echo Press ENTER to exit...
  pause > NUL
)
exit /b 0
