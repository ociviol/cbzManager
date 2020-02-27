cd ~/Dev/cbzManager/cbzManager

/usr/bin/fpc -B -Tlinux -MObjFPC -Scghi -CX -Cg -O3 -XX -l -vewnhibq -vm5076,5044,4066,4056,4055 -Fi./lib/x86_64-linux -Fu../../Utils -Fu/usr/share/lazarus/2.0.4/components/turbopower_ipro/units/x86_64-linux/gtk2 -Fu/usr/share/lazarus/2.0.4/components/printers/lib/x86_64-linux/gtk2 -Fu/usr/share/lazarus/2.0.4/components/cairocanvas/lib/x86_64-linux/gtk2 -Fu/usr/share/lazarus/2.0.4/lcl/units/x86_64-linux/gtk2 -Fu/usr/share/lazarus/2.0.4/lcl/units/x86_64-linux -Fu/usr/share/lazarus/2.0.4/components/lazutils/lib/x86_64-linux -Fu/usr/share/lazarus/2.0.4/packager/units/x86_64-linux -Fu./ -FU./lib/x86_64-linux/ -FE./ -o./ -dLCL -dLCLgtk2 -dUseCThreads -dRELEASE cbzManager.lpr

cp cbzManager ../precompiled\ binairies/Linux/Files/
sudo cp cbzManager /usr/local/bin
cd ~/Dev/cbzManager/precompiled\ binairies/Linux/Files/
cp ~/Dev/cbzManager/cbzManager/cbzManagerOsx.iconset/icon_128x128.png cbzManager.png
rm ../cbzManagerLinux.zip
zip cbzManagerLinux.zip *
cp cbzManagerLinux.zip ..

