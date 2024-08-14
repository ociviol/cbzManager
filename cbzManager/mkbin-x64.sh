#cd ~/Dev/cbzManager/cbzManager 
#/Users/ollivierciviol/fpcupdeluxe/fpc/bin/x86_64-darwin/fpc.sh -B -MObjFPC -Scghi -CX -O3 -XX -k-framework -kCocoa -l -vewnhibq -Filib/x86_64-darwin -Fu../../Utils -Fu../../XmlReader/XmlDoc -Fu../../../fpcupdeluxe/lazarus/lcl/units/x86_64-darwin/cocoa -Fu../../../fpcupdeluxe/lazarus/lcl/units/x86_64-darwin -Fu../../../fpcupdeluxe/lazarus/components/freetype/lib/x86_64-darwin -Fu../../../fpcupdeluxe/lazarus/components/lazutils/lib/x86_64-darwin -Fu../../../fpcupdeluxe/lazarus/packager/units/x86_64-darwin -Fu. -FUlib/x86_64-darwin -FE. -ocbzManagerOsx -dLCL -dLCLcocoa -dUseCThreads cbzManagerOsXx64.lpr

#cd ~/Dev/cbzManager/cbzManager/Library 
#/Users/ollivierciviol/fpcupdeluxe/fpc/bin/x86_64-darwin/fpc.sh -B -MObjFPC -Scghi -CX -O3 -XX -k-framework -kCocoa -l -vewnhibq -Filib/x86_64-darwin -Fu../../../Utils -Fu../../../XmlReader/XmlDoc -Fu.. -Fu../../../../fpcupdeluxe/lazarus/components/sqlite/lib/x86_64-darwin/cocoa -Fu../../../../fpcupdeluxe/lazarus/components/ideintf/units/x86_64-darwin/cocoa -Fu../../../../fpcupdeluxe/lazarus/components/lazcontrols/lib/x86_64-darwin/cocoa -Fu../../../../fpcupdeluxe/lazarus/lcl/units/x86_64-darwin/cocoa -Fu../../../../fpcupdeluxe/lazarus/lcl/units/x86_64-darwin -Fu../../../../fpcupdeluxe/lazarus/components/freetype/lib/x86_64-darwin -Fu../../../../fpcupdeluxe/lazarus/components/buildintf/units/x86_64-darwin -Fu../../../../fpcupdeluxe/lazarus/components/lazutils/lib/x86_64-darwin -Fu../../../../fpcupdeluxe/lazarus/packager/units/x86_64-darwin -Fu. -FUlib/x86_64-darwin -FE. -ocbzLibraryOsx -dLCL -dLCLcocoa -dUseCThreads -dLibrary cbzlibraryOsXx64.lpr


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
