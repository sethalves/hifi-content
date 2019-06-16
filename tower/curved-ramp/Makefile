#
#
#

WAVEFRONT_OBJ_TOOL=../../../wavefront-obj-tools/wavefront-obj-tool-gauche.scm

OVEN=../../../hifi/build/tools/oven/oven
BAKEDDIR=baked

OPENSCAD=openscad-nightly

curved-ramp-parts/%.stl: curved-ramp.scad
	mkdir -p curved-ramp-parts
	${OPENSCAD} -D combined=0 -D nth="`basename -s .stl $@`" -o $@ $^

%.obj: %.stl
	$(WAVEFRONT_OBJ_TOOL) -m 0.2 -n $^ -o $@

PARTS=$(foreach nth,0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23,curved-ramp-parts/${nth}.obj)

all: curved-ramp-collision-hull.obj.gz curved-ramp.obj.gz ${BAKEDDIR}/curved-ramp/baked

# ---

curved-ramp.stl: curved-ramp.scad
	${OPENSCAD} -D combined=1 -o $@ $^

curved-ramp.obj: curved-ramp.stl
	$(WAVEFRONT_OBJ_TOOL) -m 0.2 -n $^ -o $@ -L curved-ramp.mtl -S ramp_mtl

curved-ramp-collision-hull.obj: $(PARTS)
	$(WAVEFRONT_OBJ_TOOL) -c -o $@ $^

${BAKEDDIR}/curved-ramp/baked: curved-ramp.obj
	${OVEN} -i $^ -o ${BAKEDDIR} -t model

UNBAKED_FILES=curved-ramp.obj.gz curved-ramp-collision-hull.obj.gz curved-ramp.mtl tower-ramp.png

upload: ${UNBAKED_FILES} ${BAKEDDIR}/curved-ramp/baked
	mkdir -p curved-ramp
	rsync -avP ${BAKEDDIR}/curved-ramp/baked/ ../tower-upload/
	cp ${UNBAKED_FILES} ../tower-upload/


clean:
	rm -rf *~ curved-ramp-parts curved-ramp-collision-hull.obj curved-ramp.obj
	rm -f curved-ramp.obj.tmp.obj curved-ramp.stl
	rm -f curved-ramp.obj.gz curved-ramp-collision-hull.obj.gz
	rm -rf baked
