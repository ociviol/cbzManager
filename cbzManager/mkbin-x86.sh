cd ~/Dev/cbzManager/cbzManager 
/Users/ollivierciviol/fpcupdeluxe/lazarus/lazbuild -B --build-mode=Release cbzManagerOsx.lpr

cd ~/Dev/cbzManager/cbzManager/Library 
//Users/ollivierciviol/fpcupdeluxe/lazarus/lazbuild -B --build-mode=Release cbzLibraryOsx.lpr


status=$?
if test $status -eq 0 
then
	cp -r ~/Dev/cbzManager/cbzManager/Library/cbzLibraryOsx.app ~/Dev/cbzManager/precompiled\ binairies/Mac\ OsX/x86/ 
	cp -r ~/Dev/cbzManager/cbzManager/cbzManagerOsx.app ~/Dev/cbzManager/precompiled\ binairies/Mac\ OsX/x86/ 

	rm ~/Dev/cbzManager/precompiled\ binairies/Mac\ OsX/x86/cbzManagerOsx.app/Contents/MacOS/cbzManagerOsx 
	cp ~/Dev/cbzManager/cbzManager/cbzManagerOsx ~/Dev/cbzManager/precompiled\ binairies/Mac\ OsX/x86/cbzManagerOsx.app/Contents/MacOS/

	rm ~/Dev/cbzManager/precompiled\ binairies/Mac\ OsX/x86/cbzLibraryOsx.app/Contents/MacOS/cbzLibraryOsx 
	cp ~/Dev/cbzManager/cbzManager/Library/cbzLibraryOsx ~/Dev/cbzManager/precompiled\ binairies/Mac\ OsX/x86/cbzLibraryOsx.app/Contents/MacOS/

	cp -r ~/Dev/cbzManager/precompiled\ binairies/Mac\ OsX/x86/cbzManagerOsx.app /Applications/ 
	cp -r ~/Dev/cbzManager/precompiled\ binairies/Mac\ OsX/x86/cbzLibraryOsx.app /Applications/

	cd ~/Dev/cbzManager/precompiled\ binairies/Mac\ OsX/x86/
	rm cbzManagerOsx-x86.zip
	zip -r cbzManagerOsx-x86.zip cp -r cbzManagerOsx.app cbzLibraryOsx.app
 
else
	echo 'Compile failed'
fi
