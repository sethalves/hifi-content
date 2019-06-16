#
#
#

OVEN=../../../hifi/build/tools/oven/oven
BAKEDDIR=baked

OPENSCAD=openscad-nightly

WAVEFRONT_OBJ_TOOL=../../../wavefront-obj-tools/wavefront-obj-tool-gauche.scm

%.obj: %.stl
	${WAVEFRONT_OBJ_TOOL} -m 0.2 -n $^ -o $@ -L tower-third-floor-rail.mtl -S tower_third_floor_rail_mtl

%.obj.gz: %.obj
	cat $< | gzip -9 - > $@

all: tower-third-floor-rail.obj.gz ${BAKEDDIR}/tower-third-floor-rail/baked

tower-third-floor-rail.stl: tower-third-floor-rail.scad
	${OPENSCAD} -D nth=0 -o $@ $^

${BAKEDDIR}/tower-third-floor-rail/baked: tower-third-floor-rail.obj
	${OVEN} -i $^ -o ${BAKEDDIR} -t model

UNBAKED_FILES=tower-third-floor-rail.obj.gz tower-third-floor-rail.mtl

upload: ${UNBAKED_FILES} ${BAKEDDIR}/tower-third-floor-rail/baked
	mkdir -p ../tower-upload/
	rsync -avP ${BAKEDDIR}/tower-third-floor-rail/baked/ ../tower-upload/
	cp ${UNBAKED_FILES} ../tower-upload/


clean:
	rm -rf *~ tower-third-floor-rail.stl tower-third-floor-rail.obj
	rm -rf baked tower-third-floor-rail.obj.gz
