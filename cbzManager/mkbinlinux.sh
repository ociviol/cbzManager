cd ~/Dev/cbzManager/cbzManager
/home/mat/fpcupdeluxe/lazarus/lazbuild -B cbzManager.lpi

cd ~/Dev/cbzManager/cbzManager/Library 
/home/mat/fpcupdeluxe/lazarus/lazbuild -B cbzLibraryLinux.lpi

cp ~/Dev/cbzManager/cbzManager/cbzManager ~/Dev/cbzManager/precompiled\ binairies/Linux/Files
cp ~/Dev/cbzManager/cbzManager/Library/cbzLibrary ~/Dev/cbzManager/precompiled\ binairies/Linux/Files

sudo cp ~/Dev/cbzManager/cbzManager/cbzManager /usr/local/bin
sudo cp ~/Dev/cbzManager/cbzManager/Library/cbzLibrary /usr/local/bin

cd ~/Dev/cbzManager/precompiled\ binairies/Linux/Files/
cp ~/Dev/cbzManager/cbzManager/cbzManagerOsx.iconset/icon_128x128.png cbzManager.png
cp ~/Dev/cbzManager/cbzManager/Library/cbzLibrary.png .
rm ~/Dev/cbzManager/precompiled\ binairies/Linux/cbzManagerLinux.zip
zip cbzManagerLinux.zip *
cp cbzManagerLinux.zip ..
rm cbzManagerLinux.zip

