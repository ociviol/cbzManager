chmod +x cbzManager
chmod +x cbzManager.desktop
chmod +x cbzLibrary
chmod +x cbzLibrary.desktop
cp cbzManager /usr/local/bin
cp cbzManager.png /usr/local/bin
cp cbzLibrary /usr/local/bin
cp cbzLibrary.png /usr/local/bin
ln -s /usr/local/bin/cbzManager /usr/bin/cbzManager
cp cbzManager.desktop /usr/share/applications/ 
ln -s /usr/local/bin/cbzLibrary /usr/bin/cbzLibrary
cp cbzLibrary.desktop /usr/share/applications/ 

