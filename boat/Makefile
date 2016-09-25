#
#
#

VHACD=~/src/hifi/build/tools/vhacd-util/vhacd-util


%.obj: %.stl
	wavefront-obj-tool -n -c $^ -o $@


all: boat.obj boat-collision-hull.obj


## visual model


# boat.obj: boat.scm
# 	./boat.scm

boat.stl: boat.scad
	openscad -o $@ $^

boat.obj: boat.stl
	wavefront-obj-tool -n -c $^ -o $@ -S boat_hull_mtl


## collision hull

boat-walls.stl: boat.scad
	openscad -D output_walls=1 -o $@ $^

boat-walls.obj: boat-walls.stl

boat-walls-collision-hull.obj: boat-walls.obj
	$(VHACD) -g -i $^ -o $@ # --concavity 0.001 --resolution 400000 --depth 30


boat-deck-collision-hull.stl: boat.scad
	openscad -D output_deck_collision_hull=1 -o $@ $^

boat-deck-collision-hull.obj: boat-deck-collision-hull.stl

boat-hold-floor-collision-hull.stl: boat.scad
	openscad -D output_hold_floor_collision_hull=1 -o $@ $^

boat-hold-floor-collision-hull.obj: boat-hold-floor-collision-hull.stl


boat-collision-hull.obj: boat-walls-collision-hull.obj boat-deck-collision-hull.obj boat-hold-floor-collision-hull.obj
	wavefront-obj-tool $^ > $@


##

upload: boat.obj boat-collision-hull.obj boat.js
	mkdir -p export
	cp -a $^ export/
	rsync -avP --delete export/ headache.hungry.com:public_html/hifi/boat/


clean:
	rm -f *~
	rm -f boat.obj boat.stl
	rm -f boat-walls.obj boat-walls.stl
	rm -f boat-collision-hull.obj boat-walls-collision-hull.obj
	rm -f boat-deck-collision-hull.obj boat-deck-collision-hull.stl
	rm -f boat-hold-floor-collision-hull.obj boat-hold-floor-collision-hull.stl
	rm -rf export/
