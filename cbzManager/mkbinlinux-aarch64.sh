cd ~/Dev/cbzManager/cbzManager
/home/ubuntu/Downloads/fpcupdeluxe/lazarus/lazbuild -B --build-mode=Release cbzmanager-linux-aarch64.lpi

cd ~/Dev/cbzManager/cbzManager/Library 
/home/ubuntu/Downloads/fpcupdeluxe/lazarus/lazbuild -B --build-mode=Release cbzlibrarylinux-aarch64.lpi

cp ~/Dev/cbzManager/cbzManager/cbzManager ~/Dev/cbzManager/precompiled\ binairies/Linux/aarch64/Files
cp ~/Dev/cbzManager/cbzManager/Library/cbzLibrary ~/Dev/cbzManager/precompiled\ binairies/Linux/aarch64/Files

sudo cp ~/Dev/cbzManager/cbzManager/cbzManager /usr/local/bin
sudo cp ~/Dev/cbzManager/cbzManager/Library/cbzLibrary /usr/local/bin

cd ~/Dev/cbzManager/precompiled\ binairies/Linux/aarch64/Files/
cp ~/Dev/cbzManager/cbzManager/cbzManagerOsx.iconset/icon_128x128.png cbzManager.png
cp ~/Dev/cbzManager/cbzManager/Library/cbzLibrary.png .
rm ~/Dev/cbzManager/precompiled\ binairies/Linux-aarch64/cbzManagerLinux-aarch64.zip
zip cbzManagerLinux-aarch64.zip *
cp cbzManagerLinux-aarch64.zip ..
rm cbzManagerLinux-aarch64.zip

