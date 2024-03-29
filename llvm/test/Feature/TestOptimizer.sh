#!/bin/sh
LD_LIBRARY_PATH=../lib/Assembly/Parser/Debug:../lib/Assembly/Writer/Debug:../lib/Analysis/Debug:../lib/VMCore/Debug:../lib/Bytecode/Writer/Debug:../lib/Bytecode/Reader/Debug:../lib/Optimizations/Debug
export LD_LIBRARY_PATH


../tools/as/as < $1 | ../tools/opt/opt -q -inline -constprop -dce | ../tools/dis/dis | ../tools/as/as > $1.bc.1 || exit 1

# Should not be able to optimize further!
../tools/opt/opt -q -inline -constprop -dce < $1.bc.1 > $1.bc.2 || exit 2

diff $1.bc.[12] || exit 3
rm $1.bc.[12]

