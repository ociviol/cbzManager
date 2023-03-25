cd ~/Dev/cbzManager/cbzManager

'/home/mat/fpcupdeluxe/fpc/bin/x86_64-linux/fpc.sh' -B -Tlinux -MObjFPC -Scghi -CX -Cg -O3 -XX -l -vewnhibq -vm5076,5044,4066,4056,4055 -Filib/x86_64-linux -Fu../../Utils -Fu../../XmlReader/XmlDoc -Fu../../../fpcupdeluxe/lazarus/components/turbopower_ipro/units/x86_64-linux/gtk2 -Fu../../../fpcupdeluxe/lazarus/components/printers/lib/x86_64-linux/gtk2 -Fu../../../fpcupdeluxe/lazarus/components/cairocanvas/lib/x86_64-linux/gtk2 -Fu../../../fpcupdeluxe/lazarus/lcl/units/x86_64-linux/gtk2 -Fu../../../fpcupdeluxe/lazarus/lcl/units/x86_64-linux -Fu../../../fpcupdeluxe/lazarus/components/freetype/lib/x86_64-linux -Fu../../../fpcupdeluxe/lazarus/components/lazutils/lib/x86_64-linux -Fu../../../fpcupdeluxe/lazarus/packager/units/x86_64-linux -Fu. -FUlib/x86_64-linux -FE. -ocbzManager -dLCL -dLCLgtk2 -dUseCThreads -dRELEASE cbzManager.lpr

cd ~/Dev/cbzManager/cbzManager/Library
'/home/mat/fpcupdeluxe/fpc/bin/x86_64-linux/fpc.sh'  -B -MObjFPC -Scghi -CX -Cg -O3 -XX -l -vewnhibq -Filib/x86_64-linux -Fu.. -Fu../../../../fpcupdeluxe/lazarus/lcl/units/x86_64-linux/gtk2 -Fu../../../../fpcupdeluxe/lazarus/lcl/units/x86_64-linux -Fu../../../../fpcupdeluxe/lazarus/components/freetype/lib/x86_64-linux -Fu../../../../fpcupdeluxe/lazarus/components/lazutils/lib/x86_64-linux -Fu../../../../fpcupdeluxe/lazarus/packager/units/x86_64-linux -Fu. -FUlib/x86_64-linux -FE. -ocbzLibrary -dLCL -dLCLgtk2 -dUseCThreads -dRELEASE cbzLibraryLinux.lpr

cp ~/Dev/cbzManager/cbzManager/cbzManager ~/Dev/cbzManager/precompiled\ binairies/Linux/Files
cp ~/Dev/cbzManager/cbzManager/Library/cbzLibrary ~/Dev/cbzManager/precompiled\ binairies/Linux/Files

sudo cp ~/Dev/cbzManager/cbzManager/cbzManager /usr/local/bin
sudo cp ~/Dev/cbzManager/cbzManager/Library/cbzLibrary /usr/local/bin

cd ~/Dev/cbzManager/precompiled\ binairies/Linux/Files/
cp ~/Dev/cbzManager/cbzManager/cbzManagerOsx.iconset/icon_128x128.png cbzManager.png
rm ~/Dev/cbzManager/precompiled\ binairies/Linux/cbzManagerLinux.zip
zip cbzManagerLinux.zip *
cp cbzManagerLinux.zip ..
rm cbzManagerLinux.zip

