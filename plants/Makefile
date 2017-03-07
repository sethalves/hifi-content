#
#
#

# WAVEFRONT_OBJ_TOOL=wavefront-obj-tool
WAVEFRONT_OBJ_TOOL=../../wavefront-obj-tools/wavefront-obj-tool-gauche.scm


# plant-%.obj: plant-%-no-txtr.obj
# 	wavefront-obj-tool -m 0.8 -S plant_mtl -L plant.mtl $< -o $@


VHACD=~/src/hifi/build/tools/vhacd-util/vhacd-util
LIBS='(foldling command-line)' '(seth obj-model)' '(seth scad-model)' '(srfi 27)'
OUTPUTS=plant-0.obj

all: $(OUTPUTS)


plant-0.obj: plants-main.sld plants-gauche.scm
	./plants-gauche.scm --type 0 $@


libs:
	snow2 install $(LIBS)

link-libs: very-clean
	snow2 -s \
		-p 'http://foldling.org/snow2/index.scm' \
		-p '../../snow2-packages/seth' \
		-p '../../snow2-packages/snow' \
		-p '../../seth-snow2-misc' \
		install $(LIBS)


upload: $(OUTPUTS) plant-0.obj plants.mtl grow-plants.js plants.svg plant-consolidator.cgi
	mkdir -p plants
	cp $^ plants/
	rsync -avP plants/ headache.hungry.com:public_html/hifi/plants/


clean:
	rm -f *~
	rm -rf plants
	rm -f plant-0.obj

very-clean: clean
	rm -rf seth snow srfi foldling
