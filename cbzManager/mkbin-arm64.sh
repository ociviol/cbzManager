#cd ~/Dev/cbzManager/cbzManager 
#/Users/ollivierciviol/fpcupdeluxe/fpc/bin/x86_64-darwin/fpc.sh -Px86_64 -MObjFPC -Scghi -CX -O3 -XX -k-framework -kCocoa -l -vewnhibq -Filib/x86_64-darwin -Fu../../Utils -Fu../../XmlReader/XmlDoc -Fu../../../fpcupdeluxe/lazarus/lcl/units/x86_64-darwin/cocoa -Fu../../../fpcupdeluxe/lazarus/lcl/units/x86_64-darwin -Fu../../../fpcupdeluxe/lazarus/components/freetype/lib/x86_64-darwin -Fu../../../fpcupdeluxe/lazarus/components/lazutils/lib/x86_64-darwin -Fu../../../fpcupdeluxe/lazarus/packager/units/x86_64-darwin -Fu. -FUlib/x86_64-darwin -FE. -ocbzManagerOsx -dLCL -dLCLcocoa -dUseCThreads cbzManagerOsXx64.lpr
#cd ~/Dev/cbzManager/cbzManager/Library 

#/Users/ollivierciviol/fpcupdeluxe/fpc/bin/x86_64-darwin/fpc.sh  -MObjFPC -Scghi -CX -O3 -XX -k-framework -kCocoa -l -vewnhibq -Filib/x86_64-darwin -Fu../../../Utils -Fu../../../XmlReader/XmlDoc -Fu.. -Fu../../../../fpcupdeluxe/lazarus/lcl/units/x86_64-darwin/cocoa -Fu../../../../fpcupdeluxe/lazarus/lcl/units/x86_64-darwin -Fu../../../../fpcupdeluxe/lazarus/components/freetype/lib/x86_64-darwin -Fu../../../../fpcupdeluxe/lazarus/components/lazutils/lib/x86_64-darwin -Fu../../../../fpcupdeluxe/lazarus/packager/units/x86_64-darwin -Fu. -FUlib/x86_64-darwin -FE. -ocbzLibraryOsx -dLibrary -dLCL -dLCLcocoa -dUseCThreads cbzLibraryOsx.lpr


status=$?
if test $status -eq 0 
then
	cp -r ~/Dev/cbzManager/cbzManager/Library/cbzLibraryOsx.app ~/Dev/cbzManager/precompiled\ binairies/Mac\ OsX/arm64/ 
	cp -r ~/Dev/cbzManager/cbzManager/cbzManagerOsx.app ~/Dev/cbzManager/precompiled\ binairies/Mac\ OsX/arm64/ 

	rm ~/Dev/cbzManager/precompiled\ binairies/Mac\ OsX/arm64/cbzManagerOsx.app/Contents/MacOS/cbzManagerOsx 
	cp ~/Dev/cbzManager/cbzManager/cbzManagerOsx ~/Dev/cbzManager/precompiled\ binairies/Mac\ OsX/arm64/cbzManagerOsx.app/Contents/MacOS/

	rm ~/Dev/cbzManager/precompiled\ binairies/Mac\ OsX/arm64/cbzLibraryOsx.app/Contents/MacOS/cbzLibraryOsx 
	cp ~/Dev/cbzManager/cbzManager/Library/cbzLibraryOsx ~/Dev/cbzManager/precompiled\ binairies/Mac\ OsX/arm64/cbzLibraryOsx.app/Contents/MacOS/

	cp -r ~/Dev/cbzManager/precompiled\ binairies/Mac\ OsX/arm64/cbzManagerOsx.app /Applications/ 
	cp -r ~/Dev/cbzManager/precompiled\ binairies/Mac\ OsX/arm64/cbzLibraryOsx.app /Applications/

	cd ~/Dev/cbzManager/precompiled\ binairies/Mac\ OsX/arm64/
	rm cbzManagerOsx-arm64.zip
	zip -r cbzManagerOsx-arm64.zip cp -r cbzManagerOsx.app cbzLibraryOsx.app
 
else
	echo 'Compile failed'
fi
