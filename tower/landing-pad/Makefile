#
#
#

OVEN=../../../hifi/build/tools/oven/oven
BAKEDDIR=baked

OPENSCAD=openscad-nightly

WAVEFRONT_OBJ_TOOL=../../../wavefront-obj-tools/wavefront-obj-tool-gauche.scm

%.obj.gz: %.obj
	cat $< | gzip -9 - > $@

landing-pad-parts/%.stl: landing-pad.scad
	mkdir -p landing-pad-parts
	${OPENSCAD} -D nth="`basename -s .stl $@`" -o $@ $^

%.obj: %.stl
	${WAVEFRONT_OBJ_TOOL} -m 0.2 -n $^ -o $@ # -L landing-pad.mtl

COLLISION_HULL_PARTS=$(foreach nth,1 2 4,landing-pad-parts/${nth}.obj)

all: landing-pad-collision-hull.obj.gz landing-pad.obj.gz ${BAKEDDIR}/landing-pad/baked

# ---


platform.stl: landing-pad.scad
	${OPENSCAD} -D nth=4 -o $@ $^

platform.obj: platform.stl
	${WAVEFRONT_OBJ_TOOL} -n -c $^ -o $@ -S landing_pad_platform_mtl


walkway.stl: landing-pad.scad
	${OPENSCAD} -D nth=3 -o $@ $^

walkway.obj: walkway.stl
	${WAVEFRONT_OBJ_TOOL} -m 0.2 -n -c $^ -o $@ -S landing_pad_walkway_mtl


# ---

landing-pad.obj: platform.obj walkway.obj
	${WAVEFRONT_OBJ_TOOL} -n  -L landing-pad.mtl -c $^ -o $@

landing-pad-collision-hull.obj: $(COLLISION_HULL_PARTS)
	${WAVEFRONT_OBJ_TOOL} -c -o $@ $^

${BAKEDDIR}/landing-pad/baked: landing-pad.obj
	${OVEN} -i $^ -o ${BAKEDDIR} -t model

UNBAKED_FILES=landing-pad.obj.gz landing-pad-collision-hull.obj.gz landing-pad.mtl tower-ramp.png

upload: ${UNBAKED_FILES} ${BAKEDDIR}/landing-pad/baked
	mkdir -p ../tower-upload/
	rsync -avP ${BAKEDDIR}/landing-pad/baked/ ../tower-upload/
	cp ${UNBAKED_FILES} ../tower-upload/

clean:
	rm -rf *~ landing-pad-parts landing-pad-collision-hull.obj landing-pad.obj
	rm -f platform.obj platform.stl walkway.obj walkway.stl
	rm -f landing-pad.obj.tmp.obj landing-pad.stl
	rm -f landing-pad.obj.gz landing-pad-collision-hull.obj.gz
	rm -rf baked
