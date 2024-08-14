cd C:\Dev\cbzManager\cbzManager
C:\fpcupdeluxe\lazarus\Lazbuild -B cbzManagerWin.lpi 

cd C:\Dev\cbzManager\cbzManager\Library
C:\fpcupdeluxe\lazarus\Lazbuild -B cbzLibraryWin.lpi

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
