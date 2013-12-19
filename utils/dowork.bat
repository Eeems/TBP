@echo off
hextobin input.txt middle.bin
pucrunch -c0 -d middle.bin end.bin
bintohex end.bin output.txt
del middle.bin

pause