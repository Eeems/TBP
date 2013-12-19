@echo off
cgstobin input.gsm output.bin
pucrunch -c0 -d output.bin compressed.bin
bintohex compressed.bin output.txt
del output.bin
del compressed.bin
pause