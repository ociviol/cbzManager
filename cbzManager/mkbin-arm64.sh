cd ~/Dev/cbzManager/cbzManager 
/Users/ollivierciviol/fpcupdeluxe/fpc/bin/aarch64-darwin/fpc.sh -B -MObjFPC -Scghi -CX -O3 -XX -k-framework -kCocoa -l -vewnhibq -Filib/aarch64-darwin -Fu../../Utils -Fu../../XmlReader/XmlDoc -Fu../../../fpcupdeluxe/lazarus/lcl/units/aarch64-darwin/cocoa -Fu../../../fpcupdeluxe/lazarus/lcl/units/aarch64-darwin -Fu../../../fpcupdeluxe/lazarus/components/freetype/lib/aarch64-darwin -Fu../../../fpcupdeluxe/lazarus/components/lazutils/lib/aarch64-darwin -Fu../../../fpcupdeluxe/lazarus/packager/units/aarch64-darwin -Fu. -FUlib/aarch64-darwin -FE. -ocbzManagerOsx -dLCL -dLCLcocoa -dUseCThreads -darm cbzmanagerosxarm.lpr

cd ~/Dev/cbzManager/cbzManager/Library 
/Users/ollivierciviol/fpcupdeluxe/fpc/bin/aarch64-darwin/fpc.sh -B -MObjFPC -Scghi -CX -O3 -XX -k-framework -kCocoa -l -vewnhibq -Filib/aarch64-darwin -Fu../../../Utils -Fu../../../XmlReader/XmlDoc -Fu.. -Fu../../../../fpcupdeluxe/lazarus/components/sqlite/lib/aarch64-darwin/cocoa -Fu../../../../fpcupdeluxe/lazarus/components/ideintf/units/aarch64-darwin/cocoa -Fu../../../../fpcupdeluxe/lazarus/components/lazcontrols/lib/aarch64-darwin/cocoa -Fu../../../../fpcupdeluxe/lazarus/lcl/units/aarch64-darwin/cocoa -Fu../../../../fpcupdeluxe/lazarus/lcl/units/aarch64-darwin -Fu../../../../fpcupdeluxe/lazarus/components/freetype/lib/aarch64-darwin -Fu../../../../fpcupdeluxe/lazarus/components/buildintf/units/aarch64-darwin -Fu../../../../fpcupdeluxe/lazarus/components/lazutils/lib/aarch64-darwin -Fu../../../../fpcupdeluxe/lazarus/packager/units/aarch64-darwin -Fu. -FUlib/aarch64-darwin -FE. -ocbzLibraryOsx -dLCL -dLCLcocoa -darm -dUseCThreads -dLibrary cbzlibraryosxarm.lpr

status=$?
if test $status -eq 0 
then
#	cp -r ~/Dev/cbzManager/cbzManager/Library/cbzLibraryOsx.app ~/Dev/cbzManager/precompiled\ binairies/Mac\ OsX/arm64/ 
#	cp -r ~/Dev/cbzManager/cbzManager/cbzManagerOsx.app ~/Dev/cbzManager/precompiled\ binairies/Mac\ OsX/arm64/ 

	rm ~/Dev/cbzManager/precompiled\ binairies/Mac\ OsX/arm64/cbzManagerOsx.app/Contents/MacOS/cbzManagerOsx 
	cp ~/Dev/cbzManager/cbzManager/cbzManagerOsx ~/Dev/cbzManager/precompiled\ binairies/Mac\ OsX/arm64/cbzManagerOsx.app/Contents/MacOS/
	rm /Applications/cbzManagerOsx.app/Contents/MacOS/cbzManagerOsx
	cp ~/Dev/cbzManager/cbzManager/cbzManagerOsx /Applications/cbzManagerOsx.app/Contents/MacOS/

	rm ~/Dev/cbzManager/precompiled\ binairies/Mac\ OsX/arm64/cbzLibraryOsx.app/Contents/MacOS/cbzLibraryOsx 
	cp ~/Dev/cbzManager/cbzManager/Library/cbzLibraryOsx ~/Dev/cbzManager/precompiled\ binairies/Mac\ OsX/arm64/cbzLibraryOsx.app/Contents/MacOS/
	rm /Applications/cbzLibraryOsx.app/Contents/MacOS/cbzLibraryOsx
    cp ~/Dev/cbzManager/cbzManager/Library/cbzLibraryOsx /Applications/cbzLibraryOsx.app/Contents/MacOS/
	
	cd ~/Dev/cbzManager/precompiled\ binairies/Mac\ OsX/arm64/
	rm cbzManagerOsx-arm64.zip
	zip -r cbzManagerOsx-arm64.zip cp -r cbzManagerOsx.app cbzLibraryOsx.app
 
else
	echo 'Compile failed'
fi
