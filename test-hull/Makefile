#
#
#

# WAVEFRONT_OBJ_TOOL=wavefront-obj-tool
WAVEFRONT_OBJ_TOOL=../../wavefront-obj-tools/wavefront-obj-tool-gauche.scm

VHACD=/home/seth/src/hifi/build/tools/vhacd-util/vhacd-util



all: cube-hull.obj cube-vhacd-hull.obj

cube-hull.obj: cube.obj
	$(WAVEFRONT_OBJ_TOOL) --bullet-hull -c $^ > $@

cube-vhacd-hull.obj: cube.obj
	$(VHACD) -g -i $^ -o $@

upload: cube.obj cube-hull.obj cube-vhacd-hull.obj
	mkdir -p test-hull
	cp $^ test-hull/
	rsync --delete -avP test-hull/ headache.hungry.com:public_html/hifi/test-hull/

print: cube.obj cube-hull.obj cube-vhacd-hull.obj
	$(WAVEFRONT_OBJ_TOOL) -p cube.obj
	@echo
	$(WAVEFRONT_OBJ_TOOL) -p cube-hull.obj
	@echo
	$(WAVEFRONT_OBJ_TOOL) -p cube-vhacd-hull.obj

clean:
	rm -f *~ cube-hull.obj cube-vhacd-hull.obj
