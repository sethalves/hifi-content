#
#
#

WAVEFRONT_OBJ_TOOL=../../wavefront-obj-tools/wavefront-obj-tool-gauche.scm


all: well.obj brick-well.obj


well-main.stl: well-main.scad
	openscad -o $@ $<


well.obj: well-main.stl
	$(WAVEFRONT_OBJ_TOOL) -c -n $^ -o $@ -L well.mtl -S well_mtl


brick-well.obj: brick-well-orig.obj
	$(WAVEFRONT_OBJ_TOOL) -c -m 1.0 -n $^ -o $@ -L brick-well.mtl -S brick_well_mtl


upload: well.obj well.mtl brick-well.obj brick-well.mtl concrete-texture-small.png
	mkdir -p well
	cp $^ well/
	rsync --delete -avP well/ headache.hungry.com:public_html/hifi/well/


clean:
	rm -f *~ well.obj well-main.stl
	rm -f brick-well.obj
	rm -rf well
