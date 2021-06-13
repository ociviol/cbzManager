#!/bin/sh
DoExitAsm ()
{ echo "An error occurred while assembling $1"; exit 1; }
DoExitLink ()
{ echo "An error occurred while linking $1"; exit 1; }
echo Assembling ucbz
/usr/bin/clang -c -o /Users/ollivierciviol/Dev/cbzManager/cbzManager/lib/x86_64-darwin/ucbz.o  -arch x86_64 -mmacosx-version-min=10.8.0 -x assembler /Users/ollivierciviol/Dev/cbzManager/cbzManager/lib/x86_64-darwin/ucbz.s
if [ $? != 0 ]; then DoExitAsm ucbz; fi
rm /Users/ollivierciviol/Dev/cbzManager/cbzManager/lib/x86_64-darwin/ucbz.s
echo Assembling ucbzviewerframe
/usr/bin/clang -c -o /Users/ollivierciviol/Dev/cbzManager/cbzManager/lib/x86_64-darwin/ucbzviewerframe.o  -arch x86_64 -mmacosx-version-min=10.8.0 -x assembler /Users/ollivierciviol/Dev/cbzManager/cbzManager/lib/x86_64-darwin/ucbzviewerframe.s
if [ $? != 0 ]; then DoExitAsm ucbzviewerframe; fi
rm /Users/ollivierciviol/Dev/cbzManager/cbzManager/lib/x86_64-darwin/ucbzviewerframe.s
echo Assembling uthreadextract
/usr/bin/clang -c -o /Users/ollivierciviol/Dev/cbzManager/cbzManager/lib/x86_64-darwin/uthreadextract.o  -arch x86_64 -mmacosx-version-min=10.8.0 -x assembler /Users/ollivierciviol/Dev/cbzManager/cbzManager/lib/x86_64-darwin/uthreadextract.s
if [ $? != 0 ]; then DoExitAsm uthreadextract; fi
rm /Users/ollivierciviol/Dev/cbzManager/cbzManager/lib/x86_64-darwin/uthreadextract.s
echo Assembling uworkerthread
/usr/bin/clang -c -o /Users/ollivierciviol/Dev/cbzManager/cbzManager/lib/x86_64-darwin/uworkerthread.o  -arch x86_64 -mmacosx-version-min=10.8.0 -x assembler /Users/ollivierciviol/Dev/cbzManager/cbzManager/lib/x86_64-darwin/uworkerthread.s
if [ $? != 0 ]; then DoExitAsm uworkerthread; fi
rm /Users/ollivierciviol/Dev/cbzManager/cbzManager/lib/x86_64-darwin/uworkerthread.s
echo Assembling ulibraryclasses
/usr/bin/clang -c -o /Users/ollivierciviol/Dev/cbzManager/cbzManager/lib/x86_64-darwin/ulibraryclasses.o  -arch x86_64 -mmacosx-version-min=10.8.0 -x assembler /Users/ollivierciviol/Dev/cbzManager/cbzManager/lib/x86_64-darwin/ulibraryclasses.s
if [ $? != 0 ]; then DoExitAsm ulibraryclasses; fi
rm /Users/ollivierciviol/Dev/cbzManager/cbzManager/lib/x86_64-darwin/ulibraryclasses.s
echo Assembling ulogreader
/usr/bin/clang -c -o /Users/ollivierciviol/Dev/cbzManager/cbzManager/lib/x86_64-darwin/ulogreader.o  -arch x86_64 -mmacosx-version-min=10.8.0 -x assembler /Users/ollivierciviol/Dev/cbzManager/cbzManager/lib/x86_64-darwin/ulogreader.s
if [ $? != 0 ]; then DoExitAsm ulogreader; fi
rm /Users/ollivierciviol/Dev/cbzManager/cbzManager/lib/x86_64-darwin/ulogreader.s
echo Assembling ufilecleaner
/usr/bin/clang -c -o /Users/ollivierciviol/Dev/cbzManager/cbzManager/lib/x86_64-darwin/ufilecleaner.o  -arch x86_64 -mmacosx-version-min=10.8.0 -x assembler /Users/ollivierciviol/Dev/cbzManager/cbzManager/lib/x86_64-darwin/ufilecleaner.s
if [ $? != 0 ]; then DoExitAsm ufilecleaner; fi
rm /Users/ollivierciviol/Dev/cbzManager/cbzManager/lib/x86_64-darwin/ufilecleaner.s
echo Assembling main
/usr/bin/clang -c -o /Users/ollivierciviol/Dev/cbzManager/cbzManager/lib/x86_64-darwin/main.o  -arch x86_64 -mmacosx-version-min=10.8.0 -x assembler /Users/ollivierciviol/Dev/cbzManager/cbzManager/lib/x86_64-darwin/main.s
if [ $? != 0 ]; then DoExitAsm main; fi
rm /Users/ollivierciviol/Dev/cbzManager/cbzManager/lib/x86_64-darwin/main.s
echo Assembling ucbzlibrary
/usr/bin/clang -c -o /Users/ollivierciviol/Dev/cbzManager/cbzManager/lib/x86_64-darwin/ucbzlibrary.o  -arch x86_64 -mmacosx-version-min=10.8.0 -x assembler /Users/ollivierciviol/Dev/cbzManager/cbzManager/lib/x86_64-darwin/ucbzlibrary.s
if [ $? != 0 ]; then DoExitAsm ucbzlibrary; fi
rm /Users/ollivierciviol/Dev/cbzManager/cbzManager/lib/x86_64-darwin/ucbzlibrary.s
echo Assembling cbzmanagerosx
/usr/bin/clang -c -o /Users/ollivierciviol/Dev/cbzManager/cbzManager/lib/x86_64-darwin/cbzManagerOsx.o  -arch x86_64 -mmacosx-version-min=10.8.0 -x assembler /Users/ollivierciviol/Dev/cbzManager/cbzManager/lib/x86_64-darwin/cbzManagerOsx.s
if [ $? != 0 ]; then DoExitAsm cbzmanagerosx; fi
rm /Users/ollivierciviol/Dev/cbzManager/cbzManager/lib/x86_64-darwin/cbzManagerOsx.s
echo Linking /Users/ollivierciviol/Dev/cbzManager/cbzManager/cbzManagerOsx
OFS=$IFS
IFS="
"
/usr/bin/ld     -framework Cocoa   -dead_strip -no_dead_strip_inits_and_terms -x   -order_file /Users/ollivierciviol/Dev/cbzManager/cbzManager/symbol_order.fpc -multiply_defined suppress -L. -o /Users/ollivierciviol/Dev/cbzManager/cbzManager/cbzManagerOsx `cat /Users/ollivierciviol/Dev/cbzManager/cbzManager/link7714.res` -filelist /Users/ollivierciviol/Dev/cbzManager/cbzManager/linkfiles.res
if [ $? != 0 ]; then DoExitLink /Users/ollivierciviol/Dev/cbzManager/cbzManager/cbzManagerOsx; fi
IFS=$OFS
