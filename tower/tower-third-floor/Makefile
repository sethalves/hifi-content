#
#
#

# WAVEFRONT_OBJ_TOOL=wavefront-obj-tool
WAVEFRONT_OBJ_TOOL=../../../wavefront-obj-tools/wavefront-obj-tool-gauche.scm

OVEN=../../../hifi/build/tools/oven/oven
BAKEDDIR=baked

OPENSCAD=openscad-nightly

%.obj.gz: %.obj
	cat $< | gzip -9 - > $@


tower-third-floor-parts/%.stl: tower-third-floor.scad
	mkdir -p tower-third-floor-parts
	${OPENSCAD} -D nth="`basename -s .stl $@`" -o $@ $^

%.obj: %.stl
	$(WAVEFRONT_OBJ_TOOL) -m 0.2 -n $^ -o $@ # -L tower-third-floor.mtl

COLLISION_HULL_PARTS=$(foreach nth,1 3 5 6 7 9 10,tower-third-floor-parts/${nth}.obj)

all: tower-third-floor-collision-hull.obj tower-third-floor.obj ${BAKEDDIR}/tower-third-floor/baked

# ---


platform.stl: tower-third-floor.scad
	${OPENSCAD} -D nth=0 -o $@ $^

platform.obj: platform.stl
	$(WAVEFRONT_OBJ_TOOL) -n -c $^ -o $@ -S ramp_mtl

# ---

tower-third-floor.obj: platform.obj
	$(WAVEFRONT_OBJ_TOOL) -m 0.2 -n -L tower-third-floor.mtl -S ramp_mtl -C 0.05 -c $^ -o $@

tower-third-floor-collision-hull.obj: $(COLLISION_HULL_PARTS)
	$(WAVEFRONT_OBJ_TOOL) -c -o $@ $^

${BAKEDDIR}/tower-third-floor/baked: tower-third-floor.obj
	${OVEN} -i $^ -o ${BAKEDDIR} -t model

UNBAKED_FILES=tower-third-floor.obj.gz tower-third-floor-collision-hull.obj.gz tower-third-floor.mtl tower-ramp.png

upload: ${UNBAKED_FILES} ${BAKEDDIR}/tower-third-floor/baked
	mkdir -p ../tower-upload/
	rsync -avP ${BAKEDDIR}/tower-third-floor/baked/ ../tower-upload/
	cp ${UNBAKED_FILES} ../tower-upload/

clean:
	rm -rf *~ tower-third-floor-parts tower-third-floor-collision-hull.obj tower-third-floor.obj
	rm -f platform.obj platform.stl
	rm -f tower-third-floor.obj.tmp.obj tower-third-floor.stl
	rm -rf baked
	rm -f tower-third-floor-collision-hull.obj.gz tower-third-floor.obj.gz
