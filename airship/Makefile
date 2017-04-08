#
#
#

VHACD=~/src/hifi/build/tools/vhacd-util/vhacd-util
N=-n

%.obj: %.stl
	wavefront-obj-tool -m 0.4 ${N} -c $^ -o $@ # -S airship_hull_mtl

all: airship.obj airship-collision-hull.obj

##

airship-collision-walls.stl: airship.scad
	openscad -D output_walls=1 -o $@ $^

airship-walls-collision-hull.obj: airship-collision-walls.obj
	$(VHACD) -g -i $^ -o $@ # --maxvertices 14 --concavity 0.001 --resolution 400000 --depth 30

airship-walls.stl: airship.scad
	openscad -D output_hull=1 -o $@ $^
airship-walls.obj: airship-walls.stl
	wavefront-obj-tool -m 0.4 ${N} -c $^ -o $@ -S airship_hull_mtl

airship-hold-floor.stl: airship.scad
	openscad -D output_hold_floor=1 -o $@ $^
airship-hold-floor.obj: airship-hold-floor.stl
	wavefront-obj-tool -m 0.4 ${N} -c $^ -o $@ -S airship_floor_mtl

airship-hold-floor-collision-hull.stl: airship.scad
	openscad -D output_hold_floor_collision_hull=1 -o $@ $^
airship-hold-floor-collision-hull.obj: airship-hold-floor-collision-hull.stl
	wavefront-obj-tool -m 0.4 ${N} -c $^ -o $@ -S airship_floor_mtl

airship-balloon.stl: airship.scad
	openscad -D output_balloon=1 -o $@ $^
airship-balloon.obj: airship-balloon.stl
	wavefront-obj-tool -m 0.4 ${N} -c $^ -o $@ -S airship_floor_mtl # XXX mtl


deck-parts/airship-deck-collision-hull-0.stl: airship.scad
	mkdir -p deck-parts
	openscad -D output_deck_collision_hull_0=1 -o $@ $^
deck-parts/airship-deck-collision-hull-0.obj: deck-parts/airship-deck-collision-hull-0.stl
	wavefront-obj-tool -m 0.4 ${N} -c $^ -o $@ -S airship_floor_mtl

deck-parts/airship-deck-collision-hull-1.stl: airship.scad
	mkdir -p deck-parts
	openscad -D output_deck_collision_hull_1=1 -o $@ $^
deck-parts/airship-deck-collision-hull-1.obj: deck-parts/airship-deck-collision-hull-1.stl
	wavefront-obj-tool -m 0.4 ${N} -c $^ -o $@ -S airship_floor_mtl

deck-parts/airship-deck-collision-hull-2.stl: airship.scad
	mkdir -p deck-parts
	openscad -D output_deck_collision_hull_2=1 -o $@ $^
deck-parts/airship-deck-collision-hull-2.obj: deck-parts/airship-deck-collision-hull-2.stl
	wavefront-obj-tool -m 0.4 ${N} -c $^ -o $@ -S airship_floor_mtl

deck-parts/airship-deck-collision-hull-3.stl: airship.scad
	mkdir -p deck-parts
	openscad -D output_deck_collision_hull_3=1 -o $@ $^
deck-parts/airship-deck-collision-hull-3.obj: deck-parts/airship-deck-collision-hull-3.stl
	wavefront-obj-tool -m 0.4 ${N} -c $^ -o $@ -S airship_floor_mtl

deck-parts/quarterdeck.stl: airship.scad
	mkdir -p deck-parts
	openscad -D output_deck_collision_hull_4=1 -o $@ $^
deck-parts/quarterdeck.obj: deck-parts/quarterdeck.stl
	wavefront-obj-tool -m 0.4 ${N} -c $^ -o $@ -S airship_floor_mtl

cabin-parts/cabin-front-wall-0.stl: airship.scad
	mkdir -p cabin-parts
	openscad -D output_cabin_wall_0=1 -o $@ $^
cabin-parts/cabin-front-wall-0.obj: cabin-parts/cabin-front-wall-0.stl
	wavefront-obj-tool -m 0.4 ${N} -c $^ -o $@ -S airship_hull_mtl

cabin-parts/cabin-front-wall-1.stl: airship.scad
	mkdir -p cabin-parts
	openscad -D output_cabin_wall_1=1 -o $@ $^
cabin-parts/cabin-front-wall-1.obj: cabin-parts/cabin-front-wall-1.stl
	wavefront-obj-tool -m 0.4 ${N} -c $^ -o $@ -S airship_hull_mtl

cabin-parts/cabin-door-frame.stl: airship.scad
	mkdir -p cabin-parts
	openscad -D output_door_frame=1 -o $@ $^
cabin-parts/cabin-door-frame.obj: cabin-parts/cabin-door-frame.stl
	wavefront-obj-tool -m 0.4 ${N} -c $^ -o $@ -S airship_hull_mtl

cabin-parts/stair0.stl: airship.scad
	mkdir -p cabin-parts
	openscad -D output_stair_0=1 -o $@ $^
cabin-parts/stair0.obj: cabin-parts/stair0.stl
	wavefront-obj-tool -m 0.4 ${N} -c $^ -o $@ -S airship_hull_mtl
cabin-parts/stair1.stl: airship.scad
	mkdir -p cabin-parts
	openscad -D output_stair_1=1 -o $@ $^
cabin-parts/stair1.obj: cabin-parts/stair1.stl
	wavefront-obj-tool -m 0.4 ${N} -c $^ -o $@ -S airship_hull_mtl


cabin-parts/stair-collision-shape0.stl: airship.scad
	mkdir -p cabin-parts
	openscad -D output_stair_collision_shape_0=1 -o $@ $^
cabin-parts/stair-collision-shape0.obj: cabin-parts/stair-collision-shape0.stl
	wavefront-obj-tool -m 0.4 ${N} -c $^ -o $@ -S airship_hull_mtl
cabin-parts/stair-collision-shape1.stl: airship.scad
	mkdir -p cabin-parts
	openscad -D output_stair_collision_shape_1=1 -o $@ $^
cabin-parts/stair-collision-shape1.obj: cabin-parts/stair-collision-shape1.stl
	wavefront-obj-tool -m 0.4 ${N} -c $^ -o $@ -S airship_hull_mtl

##

airship-collision-hull.obj: airship-walls-collision-hull.obj \
						 airship-hold-floor-collision-hull.obj \
						 deck-parts/airship-deck-collision-hull-0.obj \
						 deck-parts/airship-deck-collision-hull-1.obj \
						 deck-parts/airship-deck-collision-hull-2.obj \
						 deck-parts/airship-deck-collision-hull-3.obj \
						 cabin-parts/cabin-front-wall-0.obj \
						 cabin-parts/cabin-front-wall-1.obj \
						 cabin-parts/cabin-door-frame.obj \
						 cabin-parts/stair-collision-shape0.obj \
						 cabin-parts/stair-collision-shape1.obj \
						 deck-parts/quarterdeck.obj
	wavefront-obj-tool $^ > $@


# airship.stl: airship.scad
# 	openscad -D output_visual=1 -o $@ $^

airship.obj: airship-walls.obj \
		  airship-hold-floor.obj \
		  deck-parts/airship-deck-collision-hull-0.obj \
		  deck-parts/airship-deck-collision-hull-1.obj \
		  deck-parts/airship-deck-collision-hull-2.obj \
		  deck-parts/airship-deck-collision-hull-3.obj \
		  cabin-parts/cabin-front-wall-0.obj \
		  cabin-parts/cabin-front-wall-1.obj \
		  cabin-parts/cabin-door-frame.obj \
		  cabin-parts/stair0.obj \
	      cabin-parts/stair1.obj \
		  deck-parts/quarterdeck.obj \
		  airship-balloon.obj
	wavefront-obj-tool -L airship.mtl $^ > $@



## miscellaneous

upload: airship.obj airship-collision-hull.obj airship.js airship.mtl wood.jpg
	mkdir -p export
	cp -a $^ export/
	rsync -avP --delete export/ headache.hungry.com:public_html/hifi/airship/


clean:
	rm -f *~
	rm -f airship.obj airship.stl
	rm -f airship-walls.obj airship-walls.stl
	rm -f airship-hold-floor.obj airship-hold-floor.stl
	rm -f airship-collision-walls.obj airship-collision-walls.stl
	rm -f airship-collision-hull.obj airship-walls-collision-hull.obj
	rm -f airship-deck-collision-hull.obj airship-deck-collision-hull.stl
	rm -f airship-hold-floor-collision-hull.obj airship-hold-floor-collision-hull.stl
	rm -rf deck-parts/ cabin-parts/
	rm -rf export/
