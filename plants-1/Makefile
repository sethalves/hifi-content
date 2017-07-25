#
#
#

# WAVEFRONT_OBJ_TOOL=wavefront-obj-tool
WAVEFRONT_OBJ_TOOL=../../wavefront-obj-tools/wavefront-obj-tool-gauche.scm


LIBS='(foldling command-line)' '(seth obj-model)' '(seth scad-model)' '(srfi 27)'

all: plant-0.obj plant-1.obj plant-2.obj


plant-0.obj: plants-main.sld plants-gauche.scm
	./plants-gauche.scm --type 0 $@

plant-1.obj: plants-main.sld plants-gauche.scm
	./plants-gauche.scm --type 1 $@

plant-2.obj: plants-main.sld plants-gauche.scm
	./plants-gauche.scm --type 2 $@


libs:
	snow2 install $(LIBS)

link-libs: very-clean
	snow2 -s \
		-p 'http://foldling.org/snow2/index.scm' \
		-p '../../snow2-packages/seth' \
		-p '../../snow2-packages/snow' \
		-p '../../seth-snow2-misc' \
		install $(LIBS)


upload: plant-0.obj plant-1.obj plant-2.obj grow-plants.js plants.svg
	mkdir -p plants-1
	cp $^ plants-1/
	rsync -avP plants-1/ headache.hungry.com:public_html/hifi/plants-1/


clean:
	rm -f *~
	rm -rf plants-1
	rm -f plant-0.obj
	rm -f plant-1.obj
	rm -f plant-2.obj


very-clean: clean
	rm -rf seth snow srfi foldling
