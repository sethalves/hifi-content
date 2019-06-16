#
#
#

WAVEFRONT_OBJ_TOOL=../../../wavefront-obj-tools/wavefront-obj-tool-gauche.scm

OVEN=../../../hifi/build/tools/oven/oven
BAKEDDIR=baked

OPENSCAD=openscad-nightly


first-balcony-parts/%.stl: first-balcony.scad
	mkdir -p first-balcony-parts
	${OPENSCAD} -D combined=0 -D nth="`basename -s .stl $@`" -o $@ $^

%.obj: %.stl
	${WAVEFRONT_OBJ_TOOL} -m 0.2 -n $^ -o $@ # -L first-balcony.mtl

%.obj.gz: %.obj
	cat $< | gzip -9 - > $@

PARTS=$(foreach nth,0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47,first-balcony-parts/${nth}.obj)

all: first-balcony-collision-hull.obj.gz first-balcony.obj.gz ${BAKEDDIR}/first-balcony/baked

# ---

first-balcony.stl: first-balcony.scad
	${OPENSCAD} -D combined=1 -o $@ $^

first-balcony.obj: first-balcony.stl
	${WAVEFRONT_OBJ_TOOL} -m 0.2 -n $^ -o $@ -L first-balcony.mtl -S balcony_mtl

first-balcony-collision-hull.obj: $(PARTS)
	${WAVEFRONT_OBJ_TOOL} -c -o $@ $^

${BAKEDDIR}/first-balcony/baked: first-balcony.obj
	${OVEN} -i $^ -o ${BAKEDDIR} -t model

UNBAKED_FILES=first-balcony.obj.gz first-balcony-collision-hull.obj.gz first-balcony.mtl tower-ramp.png

upload: ${UNBAKED_FILES} ${BAKEDDIR}/first-balcony/baked
	mkdir -p first-balcony
	rsync -avP ${BAKEDDIR}/first-balcony/baked/ ../tower-upload/
	cp ${UNBAKED_FILES} ../tower-upload/

clean:
	rm -rf *~ first-balcony-parts first-balcony-collision-hull.obj first-balcony.obj
	rm -f  first-balcony-collision-hull.obj.gz first-balcony.obj.gz
	rm -f first-balcony.obj.tmp.obj first-balcony.stl
	rm -rf baked
