#!/bin/sh
DoExitAsm ()
{ echo "An error occurred while assembling $1"; exit 1; }
DoExitLink ()
{ echo "An error occurred while linking $1"; exit 1; }
echo Assembling ucbzviewerframe
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target x86_64-apple-macosx10.9 -o /Users/ollivierciviol/Dev/cbzManager/cbzManager/Library/lib/x86_64-darwin/ucbzviewerframe.o  -x assembler /Users/ollivierciviol/Dev/cbzManager/cbzManager/Library/lib/x86_64-darwin/ucbzviewerframe.s
if [ $? != 0 ]; then DoExitAsm ucbzviewerframe; fi
rm /Users/ollivierciviol/Dev/cbzManager/cbzManager/Library/lib/x86_64-darwin/ucbzviewerframe.s
echo Assembling cbzlibraryosx
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target x86_64-apple-macosx10.9 -o /Users/ollivierciviol/Dev/cbzManager/cbzManager/Library/lib/x86_64-darwin/cbzLibraryOsx.o  -x assembler /Users/ollivierciviol/Dev/cbzManager/cbzManager/Library/lib/x86_64-darwin/cbzLibraryOsx.s
if [ $? != 0 ]; then DoExitAsm cbzlibraryosx; fi
rm /Users/ollivierciviol/Dev/cbzManager/cbzManager/Library/lib/x86_64-darwin/cbzLibraryOsx.s
echo Linking /Users/ollivierciviol/Dev/cbzManager/cbzManager/Library/cbzLibraryOsx
OFS=$IFS
IFS="
"
/Library/Developer/CommandLineTools/usr/bin/ld     -framework Cocoa   -dead_strip -no_dead_strip_inits_and_terms -x  -order_file /Users/ollivierciviol/Dev/cbzManager/cbzManager/Library/symbol_order.fpc -multiply_defined suppress -L. -o /Users/ollivierciviol/Dev/cbzManager/cbzManager/Library/cbzLibraryOsx `cat /Users/ollivierciviol/Dev/cbzManager/cbzManager/Library/link1587.res` -filelist /Users/ollivierciviol/Dev/cbzManager/cbzManager/Library/linkfiles1587.res
if [ $? != 0 ]; then DoExitLink /Users/ollivierciviol/Dev/cbzManager/cbzManager/Library/cbzLibraryOsx; fi
IFS=$OFS
