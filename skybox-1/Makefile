#
#
#

# size of one segment.  the final image will be of size: (WIDTH * 4, HEIGTH * 3)
# WIDTH=900
# HEIGTH=900

WIDTH=640
HEIGTH=640


# WAVEFRONT_OBJ_TOOL=wavefront-obj-tool
WAVEFRONT_OBJ_TOOL=../../wavefront-obj-tools/wavefront-obj-tool-gauche.scm

VHACD=~/src/hifi/build/tools/vhacd-util/vhacd-util
LIBS='(foldling command-line)' '(seth obj-model)' '(seth scad-model)' '(srfi 27)'

%.pnm: %.png
	pngtopnm $^ > $@

%.png: %.pnm
	pnmtopng $^ > $@


TARGETS=trees-skybox-0.png trees-skybox-1.png trees-skybox-2.png trees-skybox-3.png trees-skybox-4.png \
	trees-skybox-5.png trees-skybox-6.png trees-skybox-7.png trees-skybox-8.png trees-skybox-9.png

all: ${TARGETS}


blank.pnm: Makefile
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

upload: ${TARGETS}
	mkdir -p skyboxes
	cp $^ skyboxes/
	rsync --delete -avP skyboxes/ headache.hungry.com:public_html/hifi/skyboxes/


trees.make: generate-makefile.scm
	./generate-makefile.scm > $@

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
	rm -f *.png *.pnm


very-clean: clean
	rm -rf seth snow srfi foldling trees.make

include trees.make
