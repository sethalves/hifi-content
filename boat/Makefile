#
#
#

VHACD=~/src/hifi/build/tools/vhacd-util/vhacd-util


%.obj: %.stl
	wavefront-obj-tool -n $^ -o $@


all: boat-hull.obj boat-collision-hull.obj


## visual model


# boat-hull.obj: boat-hull.scm
# 	./boat-hull.scm

boat-hull.stl: boat-hull.scad
	openscad -o $@ $^

boat-hull.obj: boat-hull.stl
	wavefront-obj-tool -n -c $^ -o $@ -S boat_hull_mtl


## collision hull

boat-rail.stl: boat-hull.scad
	openscad -D only_output_rail=1 -o $@ $^

boat-rail.obj: boat-rail.stl
	wavefront-obj-tool -n -c $^ -o $@

boat-rail-collision-hull.obj: boat-rail.obj
	$(VHACD) -g -i $^ -o $@


boat-collision-hull.obj: boat-rail-collision-hull.obj
	cp $^ $@

##

upload: boat-hull.obj boat-collision-hull.obj boat.js
	mkdir -p export
	cp -a $^ export/
	rsync -avP --delete export/ headache.hungry.com:public_html/hifi/boat/


clean:
	rm -f *~
	rm -f boat-hull.obj boat-hull.stl
	rm -f boat-rail.obj boat-rail.stl
	rm -f boat-collision-hull.obj boat-rail-collision-hull.obj
	rm -rf export/
