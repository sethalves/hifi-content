#
#
#

landing-pad-parts/%.stl: landing-pad.scad
	mkdir -p landing-pad-parts
	openscad -D nth="`basename -s .stl $@`" -o $@ $^

%.obj: %.stl
	wavefront-obj-tool -m 0.2 -n $^ -o $@ # -L landing-pad.mtl

COLLISION_HULL_PARTS=$(foreach nth,1 2 3,landing-pad-parts/${nth}.obj)
WALKWAY_PARTS=$(foreach nth,1 2,landing-pad-parts/${nth}.obj)
PLATFORM_PARTS=$(foreach nth,3,landing-pad-parts/${nth}.obj)

all: landing-pad-collision-hull.obj landing-pad.obj

# ---


platform.stl: landing-platform.scad
	openscad -D combined=1 -D thrusters=1 -o $@ $^

platform.obj: platform.stl
	wavefront-obj-tool -n -c $^ -o $@ -S landing_pad_platform_mtl


walkway.stl: landing-platform.scad
	openscad -D combined=1 -D thrusters=1 -o $@ $^

walkway.obj: walkway.stl
	wavefront-obj-tool -n -c $^ -o $@ -S landing_pad_walkway_mtl


# ---

landing-pad.obj: platform.obj walkway.obj
	wavefront-obj-tool -n -L landing-pad.mtl -c $^ -o $@

landing-pad-collision-hull.obj: $(COLLISION_HULL_PARTS)
	wavefront-obj-tool -c -o $@ $^


# ---

clean:
	rm -rf *~ landing-pad-parts landing-pad-collision-hull.obj landing-pad.obj
	rm -f landing-pad.obj.tmp.obj landing-pad.stl


upload: landing-pad.obj landing-pad-collision-hull.obj landing-pad.mtl landing-pad.jpg
	scp $^ headache:public_html/hifi/
