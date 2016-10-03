#
#
#

VHACD=~/src/hifi-master/build/tools/vhacd-util/vhacd-util
N=-n

%.obj: %.stl
	wavefront-obj-tool -m 0.4 ${N} -c $^ -o $@ -S boat_hull_mtl

all: boat.obj boat-collision-hull.obj


## visual model

boat.stl: boat.scad
	openscad -D output_visual=1 -o $@ $^

## collision hull

boat-walls.stl: boat.scad
	openscad -D output_walls=1 -o $@ $^

boat-walls-collision-hull.obj: boat-walls.obj
	$(VHACD) -g -i $^ -o $@ # --concavity 0.001 --resolution 400000 --depth 30

boat-hold-floor-collision-hull.stl: boat.scad
	openscad -D output_hold_floor_collision_hull=1 -o $@ $^

deck-parts/boat-deck-collision-hull-0.stl: boat.scad
	mkdir -p deck-parts
	openscad -D output_deck_collision_hull_0=1 -o $@ $^

deck-parts/boat-deck-collision-hull-1.stl: boat.scad
	mkdir -p deck-parts
	openscad -D output_deck_collision_hull_1=1 -o $@ $^

deck-parts/boat-deck-collision-hull-2.stl: boat.scad
	mkdir -p deck-parts
	openscad -D output_deck_collision_hull_2=1 -o $@ $^

deck-parts/boat-deck-collision-hull-3.stl: boat.scad
	mkdir -p deck-parts
	openscad -D output_deck_collision_hull_3=1 -o $@ $^

deck-parts/quarterdeck.stl: boat.scad
	mkdir -p deck-parts
	openscad -D output_deck_collision_hull_4=1 -o $@ $^

cabin-parts/cabin-front-wall-0.stl: boat.scad
	mkdir -p cabin-parts
	openscad -D output_cabin_wall_0=1 -o $@ $^

cabin-parts/cabin-front-wall-1.stl: boat.scad
	mkdir -p cabin-parts
	openscad -D output_cabin_wall_1=1 -o $@ $^

sail-parts/main-mast.stl: boat.scad
	mkdir -p sail-parts
	openscad -D output_mast=1 -o $@ $^

sail-parts/main-mast-base.stl: boat.scad
	mkdir -p sail-parts
	openscad -D output_mast_base=1 -o $@ $^

sail-parts/main-forward-mast.stl: boat.scad
	mkdir -p sail-parts
	openscad -D output_forward_mast=1 -o $@ $^

sail-parts/main-forward-mast-base.stl: boat.scad
	mkdir -p sail-parts
	openscad -D output_forward_mast_base=1 -o $@ $^


boat-collision-hull.obj: boat-walls-collision-hull.obj \
						 boat-hold-floor-collision-hull.obj \
						 deck-parts/boat-deck-collision-hull-0.obj \
						 deck-parts/boat-deck-collision-hull-1.obj \
						 deck-parts/boat-deck-collision-hull-2.obj \
						 deck-parts/boat-deck-collision-hull-3.obj \
						 cabin-parts/cabin-front-wall-0.obj \
						 cabin-parts/cabin-front-wall-1.obj \
						 deck-parts/quarterdeck.obj \
						 sail-parts/main-mast.obj \
						 sail-parts/main-forward-mast.obj
	wavefront-obj-tool $^ > $@


## miscellaneous

upload: boat.obj boat-collision-hull.obj boat.js boat.mtl
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
	rm -rf deck-parts/ cabin-parts/ sail-parts/
	rm -rf export/
