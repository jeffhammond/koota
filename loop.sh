#!/bin/bash

for t in real complex integer logical ; do
    for s in 1 2 4 8 16 ; do
        for op in min max sum prod band bor bxor land lor lxor ; do
            ./generate.sh $t $s $op > ${t}${s}_${op}.F90
            make ${t}${s}_${op}.o >& /dev/null || rm ${t}${s}_${op}.F90
        done
    done
done

for t in real complex integer logical ; do
    for s in 1 2 4 8 16 ; do
        for op in min max sum prod band bor bxor land lor lxor ; do
            if [ -f ${t}${s}_${op}.F90 ] ; then
                cat ${t}${s}_${op}.F90 >> koota.F90
            fi
            rm -f ${t}${s}_${op}.F90 ${t}${s}_${op}.o
        done
    done
done
