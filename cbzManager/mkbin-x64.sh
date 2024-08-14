
cd ~/Dev/cbzManager/cbzManager 
/Users/ollivierciviol/fpcupdeluxe/lazarus/lazbuild -B cbzManagerOsx.lpi

cd ~/Dev/cbzManager/cbzManager/Library 
/Users/ollivierciviol/fpcupdeluxe/lazarus/lazbuild -B cbzLibraryOsx.lpi

status=$?
if test $status -eq 0 
then
	#remove local binary from app
	rm -Rf ~/Dev/cbzManager/cbzManager/Library/cbzLibraryOsx.app/Contents/MacOS/cbzLibraryOsx 
	cp ~/Dev/cbzManager/cbzManager/Library/cbzLibraryOsx ~/Dev/cbzManager/cbzManager/Library/cbzLibraryOsx.app/Contents/MacOS/
	rm -Rf ~/Dev/cbzManager/cbzManager/cbzManagerOsx.app/Contents/MacOS/cbzManagerOsx 
	cp ~/Dev/cbzManager/cbzManager/cbzManagerOsx ~/Dev/cbzManager/cbzManager/cbzManagerOsx.app/Contents/MacOS/
	
	#replace dest apps	
	rm -Rf ~/Dev/cbzManager/precompiled\ binairies/Mac\ OsX/x64/cbzLibraryOsx.app
	cp -r ~/Dev/cbzManager/cbzManager/Library/cbzLibraryOsx.app ~/Dev/cbzManager/precompiled\ binairies/Mac\ OsX/x64/ 
	rm -Rf ~/Dev/cbzManager/precompiled\ binairies/Mac\ OsX/x64/cbzManagerOsx.app
	cp -r ~/Dev/cbzManager/cbzManager/cbzManagerOsx.app ~/Dev/cbzManager/precompiled\ binairies/Mac\ OsX/x64/ 

    #replace local apps
	rm -Rf /Applications/cbzManagerOsx.app 
	rm -Rf /Applications/cbzLibraryOsx.app
	cp -r ~/Dev/cbzManager/precompiled\ binairies/Mac\ OsX/x64/cbzManagerOsx.app /Applications/ 
	cp -r ~/Dev/cbzManager/precompiled\ binairies/Mac\ OsX/x64/cbzLibraryOsx.app /Applications/

	cd ~/Dev/cbzManager/precompiled\ binairies/Mac\ OsX/x64/
	rm cbzManagerOsx-x64.zip
	zip -r cbzManagerOsx-x64.zip cbzManagerOsx.app cbzLibraryOsx.app
 
else
	echo 'Compile failed'
fi
