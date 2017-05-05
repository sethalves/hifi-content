#
#
#

VHACD=~/src/hifi/build/tools/vhacd-util/vhacd-util
N=-n

%.obj: %.stl
	wavefront-obj-tool -m 0.4 ${N} -c $^ -o $@


box-hull-parts/%.stl: chest.scad
	mkdir -p box-hull-parts
	openscad -D output_box_hull=1 -D n="`basename -s .stl $@`" -o $@ $^


BOX_HULL_PARTS=$(foreach nth,0 1 2 3 4,box-hull-parts/${nth}.obj)


all: chest-box.obj chest-box-hull.obj

#
# visual box model
#

chest-box.stl: chest.scad
	openscad -D output_box=1 -o $@ $^

chest-box.obj: chest-box.stl
	wavefront-obj-tool -m 0.4 ${N} -c $^ -o $@ -S chest_wood_mtl

#
# collision hull made of convex parts
#

chest-box-hull.obj: ${BOX_HULL_PARTS}
	wavefront-obj-tool ${N} -c $^ -o $@

#
#
#

clean:
	rm -f *~
	rm -f chest-box.stl chest-box.obj
	rm -f chest-box-hull.obj
	rm -rf box-hull-parts
