cd C:\Dev\cbzManager\cbzManager
C:\fpcupdeluxe\fpc\bin\x86_64-win64\fpc.exe  -Twin64 -MObjFPC -Scghi -CX -O2 -XX -WG -l -B -vewnhibq -vm5076,5044,4066,4056,4055 -Filib\x86_64-win64 -Fu..\..\Utils -Fu..\..\XmlReader\XmlDoc -FuC:\fpcupdeluxe\lazarus\components\turbopower_ipro\units\x86_64-win64\win32 -FuC:\fpcupdeluxe\lazarus\components\printers\lib\x86_64-win64\win32 -FuC:\fpcupdeluxe\lazarus\components\cairocanvas\lib\x86_64-win64\win32 -FuC:\fpcupdeluxe\lazarus\lcl\units\x86_64-win64\win32 -FuC:\fpcupdeluxe\lazarus\lcl\units\x86_64-win64 -FuC:\fpcupdeluxe\lazarus\components\freetype\lib\x86_64-win64 -FuC:\fpcupdeluxe\lazarus\components\lazutils\lib\x86_64-win64 -FuC:\fpcupdeluxe\lazarus\packager\units\x86_64-win64 -Fu. -FUlib\x86_64-win64 -FEBin-win -oBin-win\cbzManager.exe -dLCL -dLCLwin32 -dUseCThreads -dRELEASE cbzManagerWin.lpr

cd C:\Dev\cbzManager\cbzManager\Library
C:\fpcupdeluxe\fpc\bin\x86_64-win64\fpc.exe  -MObjFPC -Scghi -CX -O3 -XX -WG -l -B -vewnhibq -Filib\x86_64-win64 -Fu.. -Fu..\..\..\Utils -Fu..\..\..\XmlReader\XmlDoc -Fu..\..\..\Synapse\source\lib -FuC:\fpcupdeluxe\lazarus\components\sqlite\lib\x86_64-win64\win32 -FuC:\fpcupdeluxe\lazarus\components\ideintf\units\x86_64-win64\win32 -FuC:\fpcupdeluxe\lazarus\components\lazcontrols\lib\x86_64-win64\win32 -FuC:\fpcupdeluxe\lazarus\lcl\units\x86_64-win64\win32 -FuC:\fpcupdeluxe\lazarus\lcl\units\x86_64-win64 -FuC:\fpcupdeluxe\lazarus\components\freetype\lib\x86_64-win64 -FuC:\fpcupdeluxe\lazarus\components\buildintf\units\x86_64-win64 -FuC:\fpcupdeluxe\lazarus\components\lazutils\lib\x86_64-win64 -FuC:\fpcupdeluxe\lazarus\packager\units\x86_64-win64 -Fu. -FUlib\x86_64-win64 -FE..\bin-win -o..\bin-win\cbzLibrary.exe -dLCL -dLCLwin32 -ddUseCThreads -dLibrary -dRELEASE cbzLibraryWin.lpr

cd C:\Dev\cbzManager\cbzManager
copy .\Bin-Win\*.* "..\precompiled binairies\Windows"
cd "..\precompiled binairies\Windows"

rem pause

del cbzManagerWin.zip
copy cbzManager.exe "%USERPROFILE%\Documents\cbzManager\"
copy cbzLibrary.exe "%USERPROFILE%\Documents\cbzManager\"

7z a -tzip -sdel cbzManagerWin.zip *.exe *.dll *.txt

del *.exe *.dll *.txt
pause
