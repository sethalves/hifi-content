#
#
#

# http://headache.hungry.com/~seth/hifi/tower-third-floor-rail.obj

%.obj: %.stl
	wavefront-obj-tool -m 0.2 -n $^ -o $@ -L tower-third-floor-rail.mtl -S tower_third_floor_rail_mtl

tower-third-floor-rail.stl: tower-third-floor-rail.scad
	openscad -D nth=0 -o $@ $^

all: tower-third-floor-rail.obj

# ---

clean:
	rm -rf *~ tower-third-floor-rail.stl tower-third-floor-rail.obj

upload: tower-third-floor-rail.obj tower-third-floor-rail.mtl
	scp $^ headache:public_html/hifi/
