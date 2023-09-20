call "C:\BDS\Studio\23.0\bin\rsvars.bat"
msbuild.exe "Source\MDHelpViewer.dproj" /target:Clean;Build /p:Platform=Win64 /p:config=release
msbuild.exe "Source\MDHelpViewer.dproj" /target:Clean;Build /p:Platform=Win32 /p:config=release

call C:\ETHEA\Certificate\SignFileWithSectico.bat \ETHEA\MarkDownHelpViewer\Bin32\MDHelpViewer.exe
call C:\ETHEA\Certificate\SignFileWithSectico.bat \ETHEA\MarkDownHelpViewer\Bin64\MDHelpViewer.exe

:INNO
"C:\Program Files (x86)\Inno Setup 6\iscc.exe" "\ETHEA\MarkDownHelpViewer\Setup\MarkDownHelpViewerSetup.iss"
set INNO_STATUS=%ERRORLEVEL%
if %INNO_STATUS%==0 GOTO SIGNSETUP
pause
EXIT

:SIGNSETUP
call \ETHEA\Certificate\SignFileWithSectico.bat \ETHEA\MarkDownHelpViewer\Setup\Output\MarkDownHelpViewerSetup.exe

:END
pause
