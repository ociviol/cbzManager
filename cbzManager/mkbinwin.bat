
C:\lazarus\fpc\3.0.4\bin\x86_64-win64\fpc.exe -B -Twin64 -MObjFPC -Scghi -CX -O3 -XX -WG -l -vewnhibq -vm5076,5044,4066,4056,4055 -Fi.\lib\x86_64-win64 -Fu..\..\Utils  -FuC:\lazarus\components\turbopower_ipro\units\x86_64-win64\win32 -FuC:\lazarus\components\printers\lib\x86_64-win64\win32 -FuC:\lazarus\components\cairocanvas\lib\x86_64-win64\win32 -FuC:\lazarus\lcl\units\x86_64-win64\win32 -FuC:\lazarus\lcl\units\x86_64-win64 -FuC:\lazarus\components\lazutils\lib\x86_64-win64 -FuC:\lazarus\packager\units\x86_64-win64 -Fu.\ -FU.\lib\x86_64-win64\ -FE.\ -o"..\precompiled binairies\Windows\cbzManager.exe" -dLCL -dLCLwin32 -dUseCThreads -dRELEASE cbzManagerWin.lpr

copy .\Bin-Win\*.* "..\precompiled binairies\Windows"
cd "..\precompiled binairies\Windows"

del cbzManagerWin.zip
copy cbzManager.exe C:\Users\Ollivier\Documents\cbzManager\
7z a -tzip -sdel cbzManagerWin.zip *.exe *.dll

del *.exe *.dll
pause
