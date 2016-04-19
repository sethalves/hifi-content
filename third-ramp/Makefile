#
#
#

# http://headache.hungry.com/~seth/hifi/third-ramp.obj
# http://headache.hungry.com/~seth/hifi/third-ramp-collision-hull.obj


third-ramp-parts/%.stl: third-ramp.scad
	mkdir -p third-ramp-parts
	openscad -D combined=0 -D nth="`basename -s .stl $@`" -o $@ $^

%.obj: %.stl
	wavefront-obj-tool -m 0.2 -n $^ -o $@

PARTS=$(foreach nth,0 1 2 3 4 5 6 7 8 9 10 11,third-ramp-parts/${nth}.obj)

all: third-ramp-collision-hull.obj third-ramp.obj

# ---

third-ramp.stl: third-ramp.scad
	openscad -D combined=1 -o $@ $^

third-ramp.obj: third-ramp.stl
	wavefront-obj-tool -m 0.2 -n $^ -o $@ -L curved-ramp.mtl -S ramp_mtl

third-ramp-collision-hull.obj: $(PARTS)
	wavefront-obj-tool -c -o $@ $^


# ---

clean:
	rm -rf *~ third-ramp-parts third-ramp-collision-hull.obj third-ramp.obj
	rm -f third-ramp.obj.tmp.obj third-ramp.stl


upload: third-ramp.obj third-ramp-collision-hull.obj
	scp $^ headache:public_html/hifi/
