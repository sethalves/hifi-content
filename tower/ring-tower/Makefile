#
#
#

DOMAIN=eschatology

WAVEFRONT_OBJ_TOOL=../../../wavefront-obj-tools/wavefront-obj-tool-gauche.scm

OVEN=../../../hifi/build/tools/oven/oven

BAKEDDIR=baked

OPENSCAD=openscad-nightly

%.obj: %.stl
	$(WAVEFRONT_OBJ_TOOL) -m 0.2 -n -c $^ -o $@

%.obj.gz: %.obj
	cat $< | gzip -9 - > $@


TARGETS=ring-tower.obj.gz ring-tower-hull.obj.gz ring-tower.mtl tower-ramp.png

ring-parts/%.stl: ring-tower.scad
	mkdir -p ring-parts
	${OPENSCAD} -D part_index="`basename -s .stl $@`" -o $@ $^


RING_PARTS=$(foreach nth,0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119,ring-parts/${nth}.obj)


all: ${TARGETS} ${BAKEDDIR}/ring-tower/baked

ring-tower.stl: ring-tower.scad
	${OPENSCAD} -o $@ $^

ring-tower.obj: ring-tower.stl
	$(WAVEFRONT_OBJ_TOOL) -m 0.2 -n -c $^ -o $@ -L ring-tower.mtl -S ring_tower_mtl

ring-tower-hull.obj: $(RING_PARTS)
	$(WAVEFRONT_OBJ_TOOL) -c -o $@ $^

${BAKEDDIR}/ring-tower/baked: ring-tower.obj
	${OVEN} -i $^ -o ${BAKEDDIR} -t model

upload: ${TARGETS} ${BAKEDDIR}/ring-tower/baked
	mkdir -p ring-tower
	rsync -avP ${BAKEDDIR}/ring-tower/baked/ ../tower-upload/
	cp ${TARGETS} ../tower-upload/

clean:
	rm -f *~ ring-tower.stl ring-tower.obj ring-tower-hull.obj ring-tower.obj.gz ring-tower-hull.obj.gz
	rm -rf ring-parts
	rm -rf baked
