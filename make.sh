#!/bin/bash
if [[ "$1" == "clean" ]]
then
    rm -f main.o main.rom
else
    wla-6502 -o main.s main.o && wlalink -v linkfile main.rom
fi
