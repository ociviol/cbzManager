cp cbzManager ../precompiled\ binairies/Linux/
cp /usr/share/applications/cbzManager.desktop ../precompiled\ binairies/Linux/
sudo cp cbzManager /usr/local/bin
cd /home/matugenos/Dev/cbzManager/precompiled\ binairies/Linux/
cp /home/matugenos/Dev/cbzManager/cbzManager/cbzManagerOsx.iconset/icon_128x128.png cbzManager.png
rm cbzManager.zip
zip cbzManager.zip *
