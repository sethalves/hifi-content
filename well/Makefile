#
#
#

WAVEFRONT_OBJ_TOOL=../../wavefront-obj-tools/wavefront-obj-tool-gauche.scm


all: well.obj


well-main.stl: well-main.scad
	openscad -o $@ $<


well.obj: well-main.stl
	$(WAVEFRONT_OBJ_TOOL) -c -n $^ -o $@ -L well.mtl -S well_mtl


upload: well.obj well.mtl
	mkdir -p well
	cp $^ well/
	rsync --delete -avP well/ headache.hungry.com:public_html/hifi/well/


clean:
	rm -f *~ well.obj well-main.stl
	rm -rf well
