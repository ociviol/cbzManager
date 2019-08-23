
cp -r cbzManagerOsx.app ../precompiled\ binairies/Mac\ OsX/ 
rm ../precompiled\ binairies/Mac\ OsX/cbzManagerOsx.app/Contents/MacOS/cbzManagerOsx 
cp cbzManagerOsx ../precompiled\ binairies/Mac\ OsX/cbzManagerOsx.app/Contents/MacOS/
cd ../precompiled\ binairies/Mac\ OsX/
rm cbzManagerOsx.zip
zip -r cbzManagerOsx.zip cbzManagerOsx.app
cp -r cbzManagerOsx.app /Applications/ 

