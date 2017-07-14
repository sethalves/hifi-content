#
#
#

DOMAIN=eschatology

# VHACD=~/src/hifi/build/tools/vhacd-util/vhacd-util
ATP_CLIENT=../../hifi/build/tools/atp-client/atp-client
WAVEFRONT_OBJ_TOOL=wavefront-obj-tool

%.obj: %.stl
	wavefront-obj-tool -m 0.4 -n -c $^ -o $@

%.obj.gz: %.obj
	rm -f $@
	gzip -9 $<


TARGETS=ring-tower.obj


all: ${TARGETS}

ring-tower.stl: ring-tower.scad
	openscad -o $@ $^

ring-tower.obj: ring-tower.stl
	wavefront-obj-tool -m 0.4 -n -c $^ -o $@ -L ring-tower.mtl -S ring_tower_mtl

clean:
	rm -f *~ ring-tower.stl ring-tower.obj
