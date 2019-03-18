#
#
#

# size of one segment.  the final image will be of size: (WIDTH * 4, HEIGTH * 3)
WIDTH=900
HEIGTH=900

L_SYSTEM_TREE=../L-system-tree/L-system-tree

%.pnm: %.png
	pngtopnm $^ > $@

%.png: %.pnm
	pnmtopng $^ > $@


###

all: trees-skybox.png trees-skybox-night.png

clouds-with-shadows0.png clouds-with-shadows1.png clouds-with-shadows2.png clouds-with-shadows3.png clouds-with-shadows4.png clouds-with-shadows5.png: clouds-with-shadows.pov clouds-with-shadows.ini
	povray +W${WIDTH} +H${HEIGTH} clouds-with-shadows.ini

blank.pnm:
	pbmmake -black ${WIDTH} ${HEIGTH} > $@

row0.pnm: clouds-with-shadows0.pnm blank.pnm
	pnmcat -lr blank.pnm $^ blank.pnm > $@

row1.pnm: clouds-with-shadows1.pnm clouds-with-shadows2.pnm clouds-with-shadows3.pnm clouds-with-shadows4.pnm
	pnmcat -lr $^ > $@

row2.pnm: clouds-with-shadows5.pnm blank.pnm
	pnmcat -lr blank.pnm $^ blank.pnm > $@


skybox.pnm: row0.pnm row1.pnm row2.pnm
	pnmcat -tb $^ > $@


### trees

trees.pov: skybox-header.pov make-trees-pov.sh
	./make-trees-pov.sh

### day

trees0.png trees1.png trees2.png trees3.png trees4.png trees5.png: trees.pov trees.ini
	povray +W${WIDTH} +H${HEIGTH} trees.ini

trees-row0.pnm: trees0.pnm blank.pnm
	pnmcat -lr blank.pnm $^ blank.pnm > $@

trees-row1.pnm: trees1.pnm trees2.pnm trees3.pnm trees4.pnm
	pnmcat -lr $^ > $@

trees-row2.pnm: trees5.pnm blank.pnm
	pnmcat -lr blank.pnm $^ blank.pnm > $@

trees-skybox.pnm: trees-row0.pnm trees-row1.pnm trees-row2.pnm
	pnmcat -tb $^ > $@

grass.png: trees5.png
	cp $^ $@


### night

trees-night.pov: trees.pov
	cp $^ $@

trees-night06.png trees-night07.png trees-night08.png trees-night09.png trees-night10.png trees-night11.png: \
	trees-night.pov trees-night.ini
	povray +W${WIDTH} +H${HEIGTH} trees-night.ini

trees-night-row0.pnm: trees-night06.pnm blank.pnm
	pnmcat -lr blank.pnm $^ blank.pnm > $@

trees-night-row1.pnm: trees-night07.pnm trees-night08.pnm trees-night09.pnm trees-night10.pnm
	pnmcat -lr $^ > $@

trees-night-row2.pnm: trees-night11.pnm blank.pnm
	pnmcat -lr blank.pnm $^ blank.pnm > $@

trees-skybox-night.pnm: trees-night-row0.pnm trees-night-row1.pnm trees-night-row2.pnm
	pnmcat -tb $^ > $@


###

clean:
	rm -f *~ *.pnm *.png
	rm -f tree.pov trees.pov trees-night.pov
	rm -f *.pov-state

upload: trees-skybox.png grass.png # skybox.png
	scp $^ headache:public_html/hifi/
