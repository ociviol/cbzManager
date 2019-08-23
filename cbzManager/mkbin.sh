cd /Users/ollivierciviol/Dev/cbzManager/cbzManager 
/Users/ollivierciviol/fpcupdeluxe/fpc/bin/x86_64-darwin/fpc.sh -B -MObjFPC -Scghi -CX -O3 -XX -k-framework -kCocoa -l -vewnhibq -Fi/Users/ollivierciviol/Dev/cbzManager/cbzManager/lib/x86_64-darwin -Fu/Users/ollivierciviol/Dev/cbzManager/Utils -Fu/Users/ollivierciviol/Dev/cbzManager/Utils/zipfile -Fu/Users/ollivierciviol/fpcupdeluxe/lazarus/lcl/units/x86_64-darwin/cocoa -Fu/Users/ollivierciviol/fpcupdeluxe/lazarus/lcl/units/x86_64-darwin -Fu/Users/ollivierciviol/fpcupdeluxe/lazarus/components/lazutils/lib/x86_64-darwin -Fu/Users/ollivierciviol/fpcupdeluxe/lazarus/packager/units/x86_64-darwin -Fu/Users/ollivierciviol/Dev/cbzManager/cbzManager/ -FU/Users/ollivierciviol/Dev/cbzManager/cbzManager/lib/x86_64-darwin/ -FE/Users/ollivierciviol/Dev/cbzManager/cbzManager/ -o/Users/ollivierciviol/Dev/cbzManager/cbzManager/cbzManagerOsx -dLCL -dLCLcocoa -dUseCThreads cbzManagerOsx.lpr
cp -r cbzManagerOsx.app ../precompiled\ binairies/Mac\ OsX/ 
rm ../precompiled\ binairies/Mac\ OsX/cbzManagerOsx.app/Contents/MacOS/cbzManagerOsx 
cp cbzManagerOsx ../precompiled\ binairies/Mac\ OsX/cbzManagerOsx.app/Contents/MacOS/
cd ../precompiled\ binairies/Mac\ OsX/
rm cbzManagerOsx.zip
zip -r cbzManagerOsx.zip cbzManagerOsx.app
cp -r cbzManagerOsx.app /Applications/ 

