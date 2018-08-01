#
#
#

# WAVEFRONT_OBJ_TOOL=../../wavefront-obj-tools/wavefront-obj-tool
WAVEFRONT_OBJ_TOOL=wavefront-obj-tool

all: terrain-textured.obj


terrain-textured.obj: terrain.obj
	$(WAVEFRONT_OBJ_TOOL) \
		-L terrain.mtl -S terrain_mtl \
		-R 0 0 0.7071067690849304 0.7071067690849304 \
		-s 2000 \
		$^ \
		> $@



upload: terrain-textured.obj terrain-texture.png terrain.mtl
	mkdir -p wm-terrain
	cp $^ wm-terrain/
	rsync --delete -avP wm-terrain/ headache.hungry.com:public_html/hifi/wm-terrain/


clean:
	rm -rf *~ wm-terrain terrain-rotated.obj terrain-textured.obj
