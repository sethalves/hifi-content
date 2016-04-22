#!/bin/bash

L_SYSTEM_TREE=../L-system-tree/L-system-tree

cat skybox-header.pov > trees.pov

# ${L_SYSTEM_TREE} -p 0 0 100 -v -w 5 -t iixxyiO -o tree.pov
# cat tree.pov >> trees.pov

# ${L_SYSTEM_TREE} -p 20 0 100 -v -w 5 -t iixxyiO -o tree.pov
# cat tree.pov >> trees.pov


for ((i=1;i<=200;i++));
do
    X=$(( ( RANDOM % 150 )  + -100 ))
    Y=0
    Z=$(( ( RANDOM % 50 )  + 50 ))
    R=$(( ( RANDOM % 360 ) ))
    W=$(( ( RANDOM % 3 )  + 3))

    ${L_SYSTEM_TREE} -p $X $Y $Z -r 0 $R 0 -v -w $W -t iixxyiO -o tree.pov
    cat tree.pov >> trees.pov
    rm tree.pov
done


for ((i=1;i<=200;i++));
do
    X=$(( ( RANDOM % 150 )  + -50 ))
    Y=0
    Z=$(( ( RANDOM % 50 )  + -100 ))
    R=$(( ( RANDOM % 360 ) ))
    W=$(( ( RANDOM % 3 )  + 3))

    ${L_SYSTEM_TREE} -p $X $Y $Z -r 0 $R 0 -v -w $W -t iixxyiO -o tree.pov
    cat tree.pov >> trees.pov
    rm tree.pov
done


for ((i=1;i<=200;i++));
do
    X=$(( ( RANDOM % 50 )  + -100 ))
    Y=0
    Z=$(( ( RANDOM % 150 )  + -100 ))
    R=$(( ( RANDOM % 360 ) ))
    W=$(( ( RANDOM % 3 )  + 3))

    ${L_SYSTEM_TREE} -p $X $Y $Z -r 0 $R 0 -v -w $W -t iixxyiO -o tree.pov
    cat tree.pov >> trees.pov
    rm tree.pov
done


for ((i=1;i<=200;i++));
do
    X=$(( ( RANDOM % 50 )  + 50 ))
    Y=0
    Z=$(( ( RANDOM % 150 )  + -50 ))
    R=$(( ( RANDOM % 360 ) ))
    W=$(( ( RANDOM % 3 )  + 3))

    ${L_SYSTEM_TREE} -p $X $Y $Z -r 0 $R 0 -v -w $W -t iixxyiO -o tree.pov
    cat tree.pov >> trees.pov
    rm tree.pov
done
