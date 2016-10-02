#
#
#

VHACD=~/src/hifi-master/build/tools/vhacd-util/vhacd-util
N= #-n

%.obj: %.stl
	wavefront-obj-tool ${N} -c $^ -o $@


all: boat.obj boat-collision-hull.obj


## visual model


# boat.obj: boat.scm
# 	./boat.scm

boat-hull.stl: boat.scad
	openscad -D output_hull=1 -o $@ $^

boat-hull.obj: boat-hull.stl
	wavefront-obj-tool -m 0.2 ${N} -c $^ -o $@ -S boat_hull_mtl


boat.obj: boat-hull.obj boat-hold-floor-collision-hull.obj \
		  deck-parts/boat-deck-collision-hull-0.obj \
		  deck-parts/boat-deck-collision-hull-1.obj \
		  deck-parts/boat-deck-collision-hull-2.obj \
		  deck-parts/boat-deck-collision-hull-3.obj \
		  deck-parts/quarterdeck.obj
	wavefront-obj-tool ${N} -c $^ -o $@ -L boat.mtl


## collision hull

boat-walls.stl: boat.scad
	openscad -D output_walls=1 -o $@ $^

boat-walls.obj: boat-walls.stl

boat-walls-collision-hull.obj: boat-walls.obj
	$(VHACD) -g -i $^ -o $@ # --concavity 0.001 --resolution 400000 --depth 30

boat-hold-floor-collision-hull.stl: boat.scad
	openscad -D output_hold_floor_collision_hull=1 -o $@ $^

boat-hold-floor-collision-hull.obj: boat-hold-floor-collision-hull.stl
	wavefront-obj-tool -m 0.2 ${N} -c $^ -o $@ -S boat_floor_mtl


##

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

##

deck-parts/boat-deck-collision-hull-0.obj: deck-parts/boat-deck-collision-hull-0.stl
	wavefront-obj-tool -m 0.2 ${N} -c $^ -o $@ -S boat_floor_mtl

deck-parts/boat-deck-collision-hull-1.obj: deck-parts/boat-deck-collision-hull-1.stl
	wavefront-obj-tool -m 0.2 ${N} -c $^ -o $@ -S boat_floor_mtl

deck-parts/boat-deck-collision-hull-2.obj: deck-parts/boat-deck-collision-hull-2.stl
	wavefront-obj-tool -m 0.2 ${N} -c $^ -o $@ -S boat_floor_mtl

deck-parts/boat-deck-collision-hull-3.obj: deck-parts/boat-deck-collision-hull-3.stl
	wavefront-obj-tool -m 0.2 ${N} -c $^ -o $@ -S boat_floor_mtl

deck-parts/quarterdeck.obj: deck-parts/quarterdeck.stl
	wavefront-obj-tool -m 0.2 ${N} -c $^ -o $@ -S boat_floor_mtl

cabin-parts/cabin-front-wall-0.obj: cabin-parts/cabin-front-wall-0.stl
	wavefront-obj-tool -m 0.2 ${N} -c $^ -o $@ -S boat_hull_mtl

cabin-parts/cabin-front-wall-1.obj: cabin-parts/cabin-front-wall-1.stl
	wavefront-obj-tool -m 0.2 ${N} -c $^ -o $@ -S boat_hull_mtl

##


boat-collision-hull.obj: boat-walls-collision-hull.obj \
						 boat-hold-floor-collision-hull.obj \
						 deck-parts/boat-deck-collision-hull-0.obj \
						 deck-parts/boat-deck-collision-hull-1.obj \
						 deck-parts/boat-deck-collision-hull-2.obj \
						 deck-parts/boat-deck-collision-hull-3.obj \
						 cabin-parts/cabin-front-wall-0.obj \
						 cabin-parts/cabin-front-wall-1.obj \
						 deck-parts/quarterdeck.obj
	wavefront-obj-tool $^ > $@


##

upload: boat.obj boat-collision-hull.obj boat.js boat.mtl
	mkdir -p export
	cp -a $^ export/
	rsync -avP --delete export/ headache.hungry.com:public_html/hifi/boat/


clean:
	rm -f *~
	rm -f boat.obj boat.stl
	rm -f boat-hull.obj boat-hull.stl
	rm -f boat-walls.obj boat-walls.stl
	rm -f boat-collision-hull.obj boat-walls-collision-hull.obj
	rm -f boat-deck-collision-hull.obj boat-deck-collision-hull.stl
	rm -f boat-hold-floor-collision-hull.obj boat-hold-floor-collision-hull.stl
	rm -rf deck-parts/ cabin-parts/
	rm -rf export/
