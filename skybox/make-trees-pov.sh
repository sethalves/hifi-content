#!/bin/bash

L_SYSTEM_TREE=../L-system-tree/L-system-tree

cat skybox-header.pov > trees.pov

pi=`echo "4*a(1)" | bc -l`

for ((i=1;i<=400;i++));
do
    A=$(( ( RANDOM % 360 ) ))
    rad=`echo "$A*($pi/180)" | bc -l`

    D=$(( ( RANDOM % 500 )  + 40))

    Z=`echo "$D*s($rad)" | bc -l`
    X=`echo "$D*c($rad)" | bc -l`

    Y=0
    R=$(( ( RANDOM % 360 ) ))
    W=$(( ( RANDOM % 3 )  + 3))

    S=$(( ( RANDOM % 2 )  + 2))

    ${L_SYSTEM_TREE} -s 0.$S -p $X $Y $Z -r 0 $R 0 -v -w $W -t iixxyiO -o tree.pov
    cat tree.pov >> trees.pov
    rm tree.pov
done
