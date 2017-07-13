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

box-hull-parts/%.stl: chest.scad
	mkdir -p box-hull-parts
	openscad -D output_box_hull=1 -D n="`basename -s .stl $@`" -o $@ $^

lid-hull-parts/%.stl: chest.scad
	mkdir -p lid-hull-parts
	openscad -D output_lid_hull=1 -D n="`basename -s .stl $@`" -o $@ $^

TARGETS=chest-box.obj chest-box-hull.obj chest-lid.obj chest-lid-hull.obj chest.mtl chest.json


BOX_HULL_PARTS=$(foreach nth,0 1 2 3 4,box-hull-parts/${nth}.obj)
LID_HULL_PARTS=$(foreach nth,0 1 2 3 4 5 6,lid-hull-parts/${nth}.obj)


all: ${TARGETS}

#
# visual box model
#

chest-box.stl: chest.scad
	openscad -D output_box=1 -o $@ $^

chest-box.obj: chest-box.stl
	wavefront-obj-tool -m 0.4 -n -c $^ -o $@ -L chest.mtl -S chest_wood_mtl

#
# visual lid model
#

chest-lid.stl: chest.scad
	openscad -D output_lid=1 -o $@ $^

chest-lid.obj: chest-lid.stl
	wavefront-obj-tool -m 0.4 -n -c $^ -o $@ -L chest.mtl -S chest_wood_mtl

# chest-lid-hull.obj: chest-lid.obj
# 	$(VHACD) -g -i $^ -o $@ --maxvertices 64 --concavity 0.001 --resolution 400000 --depth 30

chest-lid-hull.obj: ${LID_HULL_PARTS}
	wavefront-obj-tool -n -c $^ -o $@


#
# collision hull made of convex parts
#

chest-box-hull.obj: ${BOX_HULL_PARTS}
	wavefront-obj-tool -n -c $^ -o $@

#
#
#

upload-www: ${TARGETS}
	mkdir -p chest
	cp $^ chest/
	rsync --delete -avP chest/ headache.hungry.com:public_html/hifi/chest/


upload: chest-box.obj.gz chest-box-hull.obj.gz chest-lid.obj.gz chest-lid-hull.obj.gz chest.mtl
	- $(ATP_CLIENT) -T chest-box.obj.gz atp://$(DOMAIN)/chest/chest-box.obj.gz
	- $(ATP_CLIENT) -T chest-box-hull.obj.gz atp://$(DOMAIN)/chest/chest-box-hull.obj.gz
	- $(ATP_CLIENT) -T chest-lid.obj.gz atp://$(DOMAIN)/chest/chest-lid.obj.gz
	- $(ATP_CLIENT) -T chest-lid-hull.obj.gz atp://$(DOMAIN)/chest/chest-lid-hull.obj.gz
	- $(ATP_CLIENT) -T chest.mtl atp://$(DOMAIN)/chest/chest.mtl


clean:
	rm -f *~
	rm -f chest-box.stl chest-box.obj chest-box-hull.obj chest-box.obj.gz chest-box-hull.obj.gz
	rm -f chest-lid.stl chest-lid.obj chest-lid-hull.obj chest-lid.obj.gz chest-lid-hull.obj.gz
	rm -rf box-hull-parts
	rm -rf lid-hull-parts
	rm -rf chest
