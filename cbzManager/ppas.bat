@echo off
SET THEFILE=C:\Dev\GitHub\cbzManager\cbzManager\cbzManagerWin.exe
echo Linking %THEFILE%
C:\fpcupdeluxe\fpc\bin\x86_64-win64\ld.exe -b pei-x86-64  --gc-sections  -s --subsystem windows --entry=_WinMainCRTStartup    -o C:\Dev\GitHub\cbzManager\cbzManager\cbzManagerWin.exe C:\Dev\GitHub\cbzManager\cbzManager\link.res
if errorlevel 1 goto linkend
goto end
:asmend
echo An error occurred while assembling %THEFILE%
goto end
:linkend
echo An error occurred while linking %THEFILE%
:end
