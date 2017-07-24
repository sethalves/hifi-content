#!/bin/bash

L_SYSTEM_TREE=../L-system-tree/L-system-tree

IFS=' '
while read -ra treespec
do
    X=${treespec[0]}
    Z=${treespec[1]}
    Y=${treespec[2]}
    R=${treespec[3]}
    S=${treespec[4]}
    W=${treespec[5]}
    ${L_SYSTEM_TREE} -s 0.$S -p $X $Y $Z -r 0 $R 0 -v -w $W -t iixxyiO -o tree.pov
    cat tree.pov
    rm tree.pov
done
