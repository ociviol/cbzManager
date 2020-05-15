C:\fpcupdeluxe\fpc\bin\x86_64-win64\fpc.exe  -Twin64 -MObjFPC -Scghi -CX -O3 -XX -WG -l -vewnhibq -vm5076,5044,4066,4056,4055 -Filib\x86_64-win64 -Fu..\..\Utils -Fu..\..\XmlReader\XmlDoc -FuC:\fpcupdeluxe\lazarus\components\virtualtreeview\lib\x86_64-win64-win32 -FuC:\fpcupdeluxe\lazarus\components\turbopower_ipro\units\x86_64-win64\win32 -FuC:\fpcupdeluxe\lazarus\components\printers\lib\x86_64-win64\win32 -FuC:\fpcupdeluxe\lazarus\components\ideintf\units\x86_64-win64\win32 -FuC:\fpcupdeluxe\lazarus\components\lclextensions\lib\x86_64-win64-win32 -FuC:\fpcupdeluxe\lazarus\components\lazcontrols\lib\x86_64-win64\win32 -FuC:\fpcupdeluxe\lazarus\components\cairocanvas\lib\x86_64-win64\win32 -FuC:\fpcupdeluxe\lazarus\lcl\units\x86_64-win64\win32 -FuC:\fpcupdeluxe\lazarus\lcl\units\x86_64-win64 -FuC:\fpcupdeluxe\lazarus\components\lazutils\lib\x86_64-win64 -FuC:\fpcupdeluxe\lazarus\packager\units\x86_64-win64 -Fu. -FUlib\x86_64-win64 -FE. -o.\Bin-Win\cbzManager.exe -dLCL -dLCLwin32 -dUseCThreads -dRELEASE cbzManagerWin.lpr

rem C:\fpcupdeluxe\fpc\bin\x86_64-win64\fpc.exe -B -Twin64 -MObjFPC -Scghi -CX -XX -WG -l -vewnhibq -vm5076,5044,4066,4056,4055 -Filib\x86_64-win64 -Fu..\..\Utils -Fu..\..\XmlReader\XmlDoc -FuC:\fpcupdeluxe\lazarus\components\turbopower_ipro\units\x86_64-win64\win32 -FuC:\fpcupdeluxe\lazarus\components\printers\lib\x86_64-win64\win32 -FuC:\fpcupdeluxe\lazarus\components\cairocanvas\lib\x86_64-win64\win32 -FuC:\fpcupdeluxe\lazarus\lcl\units\x86_64-win64\win32 -FuC:\fpcupdeluxe\lazarus\lcl\units\x86_64-win64 -FuC:\fpcupdeluxe\lazarus\components\lazutils\lib\x86_64-win64 -FuC:\fpcupdeluxe\lazarus\packager\units\x86_64-win64 -Fu. -FUlib\x86_64-win64 -FE. -ocbzManagerWin.exe -dLCL -dLCLwin32 -dUseCThreads -dRELEASE

copy .\Bin-Win\*.* "..\precompiled binairies\Windows"
cd "..\precompiled binairies\Windows"

rem pause

del cbzManagerWin.zip
copy cbzManager.exe C:\Users\Ollivier\Documents\cbzManager\
7z a -tzip -sdel cbzManagerWin.zip *.exe *.dll

del *.exe *.dll
pause
