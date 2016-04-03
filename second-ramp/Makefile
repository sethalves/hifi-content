#
#
#

# http://headache.hungry.com/~seth/hifi/second-ramp.obj
# http://headache.hungry.com/~seth/hifi/second-ramp-collision-hull.obj


second-ramp-parts/%.stl: second-ramp.scad
	mkdir -p second-ramp-parts
	openscad -D combined=0 -D nth="`basename -s .stl $@`" -o $@ $^

%.obj: %.stl
	wavefront-obj-tool -m 0.2 -n $^ -o $@

PARTS=$(foreach nth,0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15,second-ramp-parts/${nth}.obj)

all: second-ramp-collision-hull.obj second-ramp.obj

# ---

second-ramp.stl: second-ramp.scad
	openscad -D combined=1 -o $@ $^

second-ramp.obj: second-ramp.stl
	wavefront-obj-tool -m 0.2 -n $^ -o $@ -L curved-ramp.mtl -S ramp_mtl

second-ramp-collision-hull.obj: $(PARTS)
	wavefront-obj-tool -c -o $@ $^


# ---

clean:
	rm -rf *~ second-ramp-parts second-ramp-collision-hull.obj second-ramp.obj
	rm -f second-ramp.obj.tmp.obj second-ramp.stl


upload: second-ramp.obj second-ramp-collision-hull.obj
	scp $^ headache:public_html/hifi/
