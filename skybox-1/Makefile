#
#
#

# size of one segment.  the final image will be of size: (WIDTH * 4, HEIGTH * 3)
WIDTH=100
HEIGTH=100

# WIDTH=900
# HEIGTH=900



# WAVEFRONT_OBJ_TOOL=wavefront-obj-tool
WAVEFRONT_OBJ_TOOL=../../wavefront-obj-tools/wavefront-obj-tool-gauche.scm

VHACD=~/src/hifi/build/tools/vhacd-util/vhacd-util
LIBS='(foldling command-line)' '(seth obj-model)' '(seth scad-model)' '(srfi 27)'

%.pnm: %.png
	pngtopnm $^ > $@

%.png: %.pnm
	pnmtopng $^ > $@


all: grass.png trees-skybox-0.png trees-skybox-1.png


blank.pnm:
	pbmmake -black ${WIDTH} ${HEIGTH} > $@

###


tree-locations.dat: generate-tree-locations-gauche.scm generate-tree-locations-main.sld
	./$< > $@


tree-locations.png: tree-locations.dat
	gnuplot trees.gnuplot
	eog $@

trees.pov: tree-locations.dat skybox-header.pov make-povray-input.sh
	cat skybox-header.pov > $@
	cat $< | ./make-povray-input.sh >> $@


###


trees00.png trees01.png trees02.png trees03.png trees04.png trees05.png \
trees06.png trees07.png trees08.png trees09.png trees10.png trees11.png: trees.pov trees.ini
	povray +W${WIDTH} +H${HEIGTH} trees.ini

###

trees0-row0.pnm: trees00.pnm blank.pnm
	pnmcat -lr blank.pnm $^ blank.pnm > $@

trees0-row1.pnm: trees01.pnm trees02.pnm trees03.pnm trees04.pnm
	pnmcat -lr $^ > $@

trees0-row2.pnm: trees05.pnm blank.pnm
	pnmcat -lr blank.pnm $^ blank.pnm > $@

trees-skybox-0.pnm: trees0-row0.pnm trees0-row1.pnm trees0-row2.pnm
	pnmcat -tb $^ > $@

grass.png: trees05.png
	cp $^ $@

###

trees1-row0.pnm: trees06.pnm blank.pnm
	pnmcat -lr blank.pnm $^ blank.pnm > $@

trees1-row1.pnm: trees07.pnm trees08.pnm trees09.pnm trees10.pnm
	pnmcat -lr $^ > $@

trees1-row2.pnm: trees11.pnm blank.pnm
	pnmcat -lr blank.pnm $^ blank.pnm > $@

trees-skybox-1.pnm: trees1-row0.pnm trees1-row1.pnm trees1-row2.pnm
	pnmcat -tb $^ > $@


###

libs:
	snow2 install $(LIBS)

link-libs: very-clean
	snow2 -s \
		-p 'http://foldling.org/snow2/index.scm' \
		-p '../../snow2-packages/seth' \
		-p '../../snow2-packages/snow' \
		-p '../../seth-snow2-misc' \
		install $(LIBS)

clean:
	rm -f *~
	rm -f tree-locations.dat tree-locations.png
	rm -f trees.pov
	rm -f *.pov-state
	rm -f blank.pnm grass.png
	rm -f trees00.png trees01.png trees02.png trees03.png trees04.png trees05.png
	rm -f trees00.pnm trees01.pnm trees02.pnm trees03.pnm trees04.pnm trees05.pnm
	rm -f trees0-row0.pnm trees0-row1.pnm trees0-row2.pnm trees-skybox-0.pnm
	rm -f trees-skybox-0.png
	rm -f trees06.png trees07.png trees08.png trees09.png trees10.png trees11.png
	rm -f trees06.pnm trees07.pnm trees08.pnm trees09.pnm trees10.pnm trees11.pnm
	rm -f trees1-row0.pnm trees1-row1.pnm trees1-row2.pnm trees-skybox-1.pnm
	rm -f trees-skybox-1.png


very-clean: clean
	rm -rf seth snow srfi foldling
