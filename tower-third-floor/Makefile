#
#
#

# http://headache.hungry.com/~seth/hifi/tower-third-floor.obj
# http://headache.hungry.com/~seth/hifi/tower-third-floor-collision-hull.obj


# WAVEFRONT_OBJ_TOOL=wavefront-obj-tool
WAVEFRONT_OBJ_TOOL=../../wavefront-obj-tools/wavefront-obj-tool-gauche.scm
# WAVEFRONT_OBJ_TOOL=../../wavefront-obj-tools/wavefront-obj-tool


tower-third-floor-parts/%.stl: tower-third-floor.scad
	mkdir -p tower-third-floor-parts
	openscad -D nth="`basename -s .stl $@`" -o $@ $^

%.obj: %.stl
	$(WAVEFRONT_OBJ_TOOL) -m 0.2 -n $^ -o $@ # -L tower-third-floor.mtl

COLLISION_HULL_PARTS=$(foreach nth,1 3 5 6 7 9 10,tower-third-floor-parts/${nth}.obj)

all: tower-third-floor-collision-hull.obj tower-third-floor.obj

# ---


platform.stl: tower-third-floor.scad
	openscad -D nth=0 -o $@ $^

platform.obj: platform.stl
	$(WAVEFRONT_OBJ_TOOL) -n -c $^ -o $@ -S ramp_mtl

# ---

tower-third-floor.obj: platform.obj
	$(WAVEFRONT_OBJ_TOOL) -m 0.2 -n -L tower-third-floor.mtl -S ramp_mtl -C 0.05 -c $^ -o $@

tower-third-floor-collision-hull.obj: $(COLLISION_HULL_PARTS)
	$(WAVEFRONT_OBJ_TOOL) -c -o $@ $^


# ---

clean:
	rm -rf *~ tower-third-floor-parts tower-third-floor-collision-hull.obj tower-third-floor.obj
	rm -f platform.obj platform.stl
	rm -f tower-third-floor.obj.tmp.obj tower-third-floor.stl


upload: tower-third-floor.obj tower-third-floor-collision-hull.obj tower-third-floor.mtl
	scp $^ headache:public_html/hifi/
