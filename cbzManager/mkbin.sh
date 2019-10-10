cd ~/Dev/cbzManager/cbzManager 
~/fpcupdeluxe/fpc/bin/x86_64-darwin/fpc.sh -B -MObjFPC -Scghi -CX -O3 -XX -k-framework -kCocoa -l -vewnhibq -Fi./lib/x86_64-darwin -Fu../../Utils -Fu~/fpcupdeluxe/lazarus/lcl/units/x86_64-darwin/cocoa -Fu~/fpcupdeluxe/lazarus/lcl/units/x86_64-darwin -Fu~/fpcupdeluxe/lazarus/components/lazutils/lib/x86_64-darwin -Fu~/fpcupdeluxe/lazarus/packager/units/x86_64-darwin -Fu./ -FU./lib/x86_64-darwin/ -FE./ -o./cbzManagerOsx -dLCL -dLCLcocoa -dUseCThreads cbzManagerOsx.lpr
cp -r cbzManagerOsx.app ../precompiled\ binairies/Mac\ OsX/ 
rm ../precompiled\ binairies/Mac\ OsX/cbzManagerOsx.app/Contents/MacOS/cbzManagerOsx 
cp cbzManagerOsx ../precompiled\ binairies/Mac\ OsX/cbzManagerOsx.app/Contents/MacOS/
cd ../precompiled\ binairies/Mac\ OsX/
rm cbzManagerOsx.zip
zip -r cbzManagerOsx.zip cbzManagerOsx.app
cp -r cbzManagerOsx.app /Applications/ 

