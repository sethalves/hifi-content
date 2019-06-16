#
#
#

WAVEFRONT_OBJ_TOOL=../../../wavefront-obj-tools/wavefront-obj-tool-gauche.scm

OVEN=../../../hifi/build/tools/oven/oven
BAKEDDIR=baked

OPENSCAD=openscad-nightly


second-ramp-parts/%.stl: second-ramp.scad
	mkdir -p second-ramp-parts
	${OPENSCAD} -D combined=0 -D nth="`basename -s .stl $@`" -o $@ $^

%.obj: %.stl
	${WAVEFRONT_OBJ_TOOL} -m 0.2 -n $^ -o $@

%.obj.gz: %.obj
	cat $< | gzip -9 - > $@

PARTS=$(foreach nth,0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15,second-ramp-parts/${nth}.obj)

all: second-ramp-collision-hull.obj.gz second-ramp.obj.gz ${BAKEDDIR}/second-ramp/baked

# ---

second-ramp.stl: second-ramp.scad
	${OPENSCAD} -D combined=1 -o $@ $^

second-ramp.obj: second-ramp.stl
	${WAVEFRONT_OBJ_TOOL} -m 0.2 -n $^ -o $@ -L second-ramp.mtl -S tower_mtl

second-ramp-collision-hull.obj: $(PARTS)
	${WAVEFRONT_OBJ_TOOL} -c -o $@ $^

${BAKEDDIR}/second-ramp/baked: second-ramp.obj
	${OVEN} -i $^ -o ${BAKEDDIR} -t model

UNBAKED_FILES=second-ramp.obj.gz second-ramp-collision-hull.obj.gz second-ramp.mtl tower-ramp.png

upload: ${UNBAKED_FILES} ${BAKEDDIR}/second-ramp/baked
	mkdir -p second-ramp
	rsync -avP ${BAKEDDIR}/second-ramp/baked/ ../tower-upload/
	cp ${UNBAKED_FILES} ../tower-upload/

clean:
	rm -rf *~ second-ramp-parts second-ramp-collision-hull.obj second-ramp.obj
	rm -f second-ramp.obj.tmp.obj second-ramp.stl
	rm -rf baked
	rm -f second-ramp-collision-hull.obj.gz second-ramp.obj.gz
