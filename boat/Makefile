#
#
#

VHACD=~/src/hifi-master/build/tools/vhacd-util/vhacd-util
N=-n

%.obj: %.stl
	wavefront-obj-tool -m 0.4 ${N} -c $^ -o $@ # -S boat_hull_mtl

all: boat.obj boat-collision-hull.obj

##

boat-collision-walls.stl: boat.scad
	openscad -D output_walls=1 -o $@ $^

boat-walls-collision-hull.obj: boat-collision-walls.obj
	$(VHACD) -g -i $^ -o $@ --maxvertices 14 # --concavity 0.001 --resolution 400000 --depth 30

boat-walls.stl: boat.scad
	openscad -D output_hull=1 -o $@ $^
boat-walls.obj: boat-walls.stl
	wavefront-obj-tool -m 0.4 ${N} -c $^ -o $@ -S boat_hull_mtl

boat-hold-floor-collision-hull.stl: boat.scad
	openscad -D output_hold_floor_collision_hull=1 -o $@ $^
boat-hold-floor-collision-hull.obj: boat-hold-floor-collision-hull.stl
	wavefront-obj-tool -m 0.4 ${N} -c $^ -o $@ -S boat_floor_mtl

deck-parts/boat-deck-collision-hull-0.stl: boat.scad
	mkdir -p deck-parts
	openscad -D output_deck_collision_hull_0=1 -o $@ $^
deck-parts/boat-deck-collision-hull-0.obj: deck-parts/boat-deck-collision-hull-0.stl
	wavefront-obj-tool -m 0.4 ${N} -c $^ -o $@ -S boat_floor_mtl

deck-parts/boat-deck-collision-hull-1.stl: boat.scad
	mkdir -p deck-parts
	openscad -D output_deck_collision_hull_1=1 -o $@ $^
deck-parts/boat-deck-collision-hull-1.obj: deck-parts/boat-deck-collision-hull-1.stl
	wavefront-obj-tool -m 0.4 ${N} -c $^ -o $@ -S boat_floor_mtl

deck-parts/boat-deck-collision-hull-2.stl: boat.scad
	mkdir -p deck-parts
	openscad -D output_deck_collision_hull_2=1 -o $@ $^
deck-parts/boat-deck-collision-hull-2.obj: deck-parts/boat-deck-collision-hull-2.stl
	wavefront-obj-tool -m 0.4 ${N} -c $^ -o $@ -S boat_floor_mtl

deck-parts/boat-deck-collision-hull-3.stl: boat.scad
	mkdir -p deck-parts
	openscad -D output_deck_collision_hull_3=1 -o $@ $^
deck-parts/boat-deck-collision-hull-3.obj: deck-parts/boat-deck-collision-hull-3.stl
	wavefront-obj-tool -m 0.4 ${N} -c $^ -o $@ -S boat_floor_mtl

deck-parts/quarterdeck.stl: boat.scad
	mkdir -p deck-parts
	openscad -D output_deck_collision_hull_4=1 -o $@ $^
deck-parts/quarterdeck.obj: deck-parts/quarterdeck.stl
	wavefront-obj-tool -m 0.4 ${N} -c $^ -o $@ -S boat_floor_mtl

cabin-parts/cabin-front-wall-0.stl: boat.scad
	mkdir -p cabin-parts
	openscad -D output_cabin_wall_0=1 -o $@ $^
cabin-parts/cabin-front-wall-0.obj: cabin-parts/cabin-front-wall-0.stl
	wavefront-obj-tool -m 0.4 ${N} -c $^ -o $@ -S boat_hull_mtl

cabin-parts/cabin-front-wall-1.stl: boat.scad
	mkdir -p cabin-parts
	openscad -D output_cabin_wall_1=1 -o $@ $^
cabin-parts/cabin-front-wall-1.obj: cabin-parts/cabin-front-wall-1.stl
	wavefront-obj-tool -m 0.4 ${N} -c $^ -o $@ -S boat_hull_mtl

cabin-parts/cabin-door-frame.stl: boat.scad
	mkdir -p cabin-parts
	openscad -D output_door_frame=1 -o $@ $^
cabin-parts/cabin-door-frame.obj: cabin-parts/cabin-door-frame.stl
	wavefront-obj-tool -m 0.4 ${N} -c $^ -o $@ -S boat_hull_mtl

sail-parts/main-mast.stl: boat.scad
	mkdir -p sail-parts
	openscad -D output_mast=1 -o $@ $^
sail-parts/main-mast.obj: sail-parts/main-mast.stl
	wavefront-obj-tool -m 0.4 ${N} -c $^ -o $@ -S boat_mast_mtl

sail-parts/main-mast-base.stl: boat.scad
	mkdir -p sail-parts
	openscad -D output_mast_base=1 -o $@ $^
sail-parts/main-mast-base.obj: sail-parts/main-mast-base.stl
	wavefront-obj-tool -m 0.4 ${N} -c $^ -o $@ -S boat_brass_mtl

sail-parts/forward-mast.stl: boat.scad
	mkdir -p sail-parts
	openscad -D output_forward_mast=1 -o $@ $^
sail-parts/forward-mast.obj: sail-parts/forward-mast.stl
	wavefront-obj-tool -m 0.4 ${N} -c $^ -o $@ -S boat_mast_mtl

sail-parts/forward-mast-base.stl: boat.scad
	mkdir -p sail-parts
	openscad -D output_forward_mast_base=1 -o $@ $^
sail-parts/forward-mast-base.obj: sail-parts/forward-mast-base.stl
	wavefront-obj-tool -m 0.4 ${N} -c $^ -o $@ -S boat_brass_mtl

sail-parts/main-spars.stl: boat.scad
	mkdir -p sail-parts
	openscad -D output_spars=1 -o $@ $^
sail-parts/main-spars.obj: sail-parts/main-spars.stl
	wavefront-obj-tool -m 0.4 ${N} -c $^ -o $@ -S boat_mast_mtl

sail-parts/forward-spars.stl: boat.scad
	mkdir -p sail-parts
	openscad -D output_forward_spars=1 -o $@ $^
sail-parts/forward-spars.obj: sail-parts/forward-spars.stl
	wavefront-obj-tool -m 0.4 ${N} -c $^ -o $@ -S boat_mast_mtl

##

boat-collision-hull.obj: boat-walls-collision-hull.obj \
						 boat-hold-floor-collision-hull.obj \
						 deck-parts/boat-deck-collision-hull-0.obj \
						 deck-parts/boat-deck-collision-hull-1.obj \
						 deck-parts/boat-deck-collision-hull-2.obj \
						 deck-parts/boat-deck-collision-hull-3.obj \
						 cabin-parts/cabin-front-wall-0.obj \
						 cabin-parts/cabin-front-wall-1.obj \
						 cabin-parts/cabin-door-frame.obj \
						 deck-parts/quarterdeck.obj \
						 sail-parts/main-mast.obj \
						 sail-parts/main-mast-base.obj \
						 sail-parts/forward-mast.obj \
						 sail-parts/forward-mast-base.obj
	wavefront-obj-tool $^ > $@


# boat.stl: boat.scad
# 	openscad -D output_visual=1 -o $@ $^

boat.obj: boat-walls.obj \
		  boat-hold-floor-collision-hull.obj \
		  deck-parts/boat-deck-collision-hull-0.obj \
		  deck-parts/boat-deck-collision-hull-1.obj \
		  deck-parts/boat-deck-collision-hull-2.obj \
		  deck-parts/boat-deck-collision-hull-3.obj \
		  cabin-parts/cabin-front-wall-0.obj \
		  cabin-parts/cabin-front-wall-1.obj \
		  cabin-parts/cabin-door-frame.obj \
		  deck-parts/quarterdeck.obj \
		  sail-parts/main-mast.obj \
		  sail-parts/main-mast-base.obj \
		  sail-parts/forward-mast.obj \
		  sail-parts/forward-mast-base.obj \
		  sail-parts/main-spars.obj \
		  sail-parts/forward-spars.obj
	wavefront-obj-tool -L boat.mtl $^ > $@



## miscellaneous

upload: boat.obj boat-collision-hull.obj boat.js boat.mtl wood.jpg
	mkdir -p export
	cp -a $^ export/
	rsync -avP --delete export/ headache.hungry.com:public_html/hifi/boat/


clean:
	rm -f *~
	rm -f boat.obj boat.stl
	rm -f boat-walls.obj boat-walls.stl
	rm -f boat-collision-walls.obj boat-collision-walls.stl
	rm -f boat-collision-hull.obj boat-walls-collision-hull.obj
	rm -f boat-deck-collision-hull.obj boat-deck-collision-hull.stl
	rm -f boat-hold-floor-collision-hull.obj boat-hold-floor-collision-hull.stl
	rm -rf deck-parts/ cabin-parts/ sail-parts/
	rm -rf export/
