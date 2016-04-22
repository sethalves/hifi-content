#!/bin/sh

cat skybox-header.pov > trees.pov

${L_SYSTEM_TREE} -p 0 0 100 -v -w 5 -t iixxyiO -o tree.pov
cat tree.pov >> trees.pov

${L_SYSTEM_TREE} -p 20 0 100 -v -w 5 -t iixxyiO -o tree.pov
cat tree.pov >> trees.pov
