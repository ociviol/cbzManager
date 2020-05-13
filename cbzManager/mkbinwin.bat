'/home/mat/fpcupdeluxe/fpc/bin/x86_64-linux/fpc.sh'  -Tlinux -MObjFPC -Scghi -CX -Cg -O3 -XX -l -vewnhibq -vm5076,5044,4066,4056,4055 -Filib/x86_64-linux -Fu../../Utils -Fu../../XmlReader/XmlDoc -Fu../../../fpcupdeluxe/lazarus/components/turbopower_ipro/units/x86_64-linux/gtk2 -Fu../../../fpcupdeluxe/lazarus/components/printers/lib/x86_64-linux/gtk2 -Fu../../../fpcupdeluxe/lazarus/components/cairocanvas/lib/x86_64-linux/gtk2 -Fu../../../fpcupdeluxe/lazarus/lcl/units/x86_64-linux/gtk2 -Fu../../../fpcupdeluxe/lazarus/lcl/units/x86_64-linux -Fu../../../fpcupdeluxe/lazarus/components/lazutils/lib/x86_64-linux -Fu../../../fpcupdeluxe/lazarus/packager/units/x86_64-linux -Fu. -FUlib/x86_64-linux -FE. -ocbzManager -dLCL -dLCLgtk2 -dUseCThreads -dRELEASE cbzManager.lpr

rem C:\fpcupdeluxe\fpc\bin\x86_64-win64\fpc.exe -B -Twin64 -MObjFPC -Scghi -CX -XX -WG -l -vewnhibq -vm5076,5044,4066,4056,4055 -Filib\x86_64-win64 -Fu..\..\Utils -Fu..\..\XmlReader\XmlDoc -FuC:\fpcupdeluxe\lazarus\components\turbopower_ipro\units\x86_64-win64\win32 -FuC:\fpcupdeluxe\lazarus\components\printers\lib\x86_64-win64\win32 -FuC:\fpcupdeluxe\lazarus\components\cairocanvas\lib\x86_64-win64\win32 -FuC:\fpcupdeluxe\lazarus\lcl\units\x86_64-win64\win32 -FuC:\fpcupdeluxe\lazarus\lcl\units\x86_64-win64 -FuC:\fpcupdeluxe\lazarus\components\lazutils\lib\x86_64-win64 -FuC:\fpcupdeluxe\lazarus\packager\units\x86_64-win64 -Fu. -FUlib\x86_64-win64 -FE. -ocbzManagerWin.exe -dLCL -dLCLwin32 -dUseCThreads -dRELEASE

copy .\Bin-Win\*.* "..\precompiled binairies\Windows"
cd "..\precompiled binairies\Windows"

rem pause

del cbzManagerWin.zip
copy cbzManager.exe C:\Users\Ollivier\Documents\cbzManager\
7z a -tzip -sdel cbzManagerWin.zip *.exe *.dll

del *.exe *.dll
pause
