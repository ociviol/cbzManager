#!/bin/sh
DoExitAsm ()
{ echo "An error occurred while assembling $1"; exit 1; }
DoExitLink ()
{ echo "An error occurred while linking $1"; exit 1; }
echo Assembling ucbzviewerframe
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target x86_64-apple-macosx10.8 -o /Users/ollivierciviol/Dev/cbzManager/cbzManager/lib/x86_64-darwin/ucbzviewerframe.o  -x assembler /Users/ollivierciviol/Dev/cbzManager/cbzManager/lib/x86_64-darwin/ucbzviewerframe.s
if [ $? != 0 ]; then DoExitAsm ucbzviewerframe; fi
rm /Users/ollivierciviol/Dev/cbzManager/cbzManager/lib/x86_64-darwin/ucbzviewerframe.s
echo Assembling cbzmanagerosx
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target x86_64-apple-macosx10.8 -o /Users/ollivierciviol/Dev/cbzManager/cbzManager/lib/x86_64-darwin/cbzManagerOsx.o  -x assembler /Users/ollivierciviol/Dev/cbzManager/cbzManager/lib/x86_64-darwin/cbzManagerOsx.s
if [ $? != 0 ]; then DoExitAsm cbzmanagerosx; fi
rm /Users/ollivierciviol/Dev/cbzManager/cbzManager/lib/x86_64-darwin/cbzManagerOsx.s
echo Linking /Users/ollivierciviol/Dev/cbzManager/cbzManager/cbzManagerOsx
OFS=$IFS
IFS="
"
/Library/Developer/CommandLineTools/usr/bin/ld     -framework Cocoa   -dead_strip -no_dead_strip_inits_and_terms -x  -order_file /Users/ollivierciviol/Dev/cbzManager/cbzManager/symbol_order.fpc -multiply_defined suppress -L. -o /Users/ollivierciviol/Dev/cbzManager/cbzManager/cbzManagerOsx `cat /Users/ollivierciviol/Dev/cbzManager/cbzManager/link613.res` -filelist /Users/ollivierciviol/Dev/cbzManager/cbzManager/linkfiles613.res
if [ $? != 0 ]; then DoExitLink /Users/ollivierciviol/Dev/cbzManager/cbzManager/cbzManagerOsx; fi
IFS=$OFS
