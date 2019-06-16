#
#
#

# http://headache.hungry.com/~seth/hifi/tower/third-ramp.obj
# http://headache.hungry.com/~seth/hifi/tower/third-ramp-collision-hull.obj

%.obj.gz: %.obj
	cat $< | gzip -9 - > $@

OVEN=../../../hifi/build/tools/oven/oven
BAKEDDIR=baked

OPENSCAD=openscad-nightly

third-ramp-parts/%.stl: third-ramp.scad
	mkdir -p third-ramp-parts
	${OPENSCAD} -D combined=0 -D nth="`basename -s .stl $@`" -o $@ $^

%.obj: %.stl
	wavefront-obj-tool -m 0.2 -n $^ -o $@

PARTS=$(foreach nth,0 1 2 3 4 5 6 7 8 9 10 11,third-ramp-parts/${nth}.obj)

all: third-ramp-collision-hull.obj third-ramp.obj ${BAKEDDIR}/third-ramp/baked

third-ramp.stl: third-ramp.scad
	${OPENSCAD} -D combined=1 -o $@ $^

third-ramp.obj: third-ramp.stl
	wavefront-obj-tool -m 0.2 -n $^ -o $@ -L third-ramp.mtl -S tower_mtl

third-ramp-collision-hull.obj: $(PARTS)
	wavefront-obj-tool -c -o $@ $^

${BAKEDDIR}/third-ramp/baked: third-ramp.obj
	${OVEN} -i $^ -o ${BAKEDDIR} -t model

UNBAKED_FILES=third-ramp.obj.gz third-ramp-collision-hull.obj.gz third-ramp.mtl tower-ramp.png

upload: ${UNBAKED_FILES} ${BAKEDDIR}/third-ramp/baked
	mkdir -p ../tower-upload/
	rsync -avP ${BAKEDDIR}/third-ramp/baked/ ../tower-upload/
	cp ${UNBAKED_FILES} ../tower-upload/

clean:
	rm -rf *~ third-ramp-parts third-ramp-collision-hull.obj third-ramp.obj
	rm -f third-ramp.obj.tmp.obj third-ramp.stl
	rm -f third-ramp-collision-hull.obj.gz third-ramp.obj.gz
	rm -rf baked
