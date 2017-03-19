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


plants.obj: plants-3e4f2ae2-46b9-4060-9f7e-bb4ef295ab93.sexp plants-4ec771c8-fa90-42b1-9dfb-e412e6ed351c.sexp plants-c4a58a3f-78fd-40a7-b6fc-b5e5f1f35f5a.sexp
	./plants-gauche.scm --group plants-3e4f2ae2-46b9-4060-9f7e-bb4ef295ab93.sexp --group plants-4ec771c8-fa90-42b1-9dfb-e412e6ed351c.sexp --group plants-c4a58a3f-78fd-40a7-b6fc-b5e5f1f35f5a.sexp $@


upload: $(OUTPUTS) plant-0.obj plants.mtl grow-plants.js plants.svg plant-consolidator.cgi plants.obj
	mkdir -p plants
	cp $^ plants/
	rsync -avP plants/ headache.hungry.com:public_html/hifi/plants/


clean:
	rm -f *~
	rm -rf plants
	rm -f plant-0.obj
	rm -f plants.obj

very-clean: clean
	rm -rf seth snow srfi foldling
