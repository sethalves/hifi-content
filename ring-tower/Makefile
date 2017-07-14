#
#
#

DOMAIN=eschatology

# VHACD=~/src/hifi/build/tools/vhacd-util/vhacd-util
ATP_CLIENT=../../hifi/build/tools/atp-client/atp-client
WAVEFRONT_OBJ_TOOL=wavefront-obj-tool

%.obj: %.stl
	wavefront-obj-tool -m 0.4 -n -c $^ -o $@

%.obj.gz: %.obj
	rm -f $@
	gzip -9 $<


TARGETS=ring-tower.obj

ring-parts/%.stl: ring-tower.scad
	mkdir -p ring-parts
	openscad -D part_index="`basename -s .stl $@`" -o $@ $^


RING_PARTS=$(foreach nth,0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119,ring-parts/${nth}.obj)


all: ${TARGETS}

ring-tower.stl: ring-tower.scad
	openscad -o $@ $^

ring-tower.obj: ring-tower.stl
	wavefront-obj-tool -m 0.4 -n -c $^ -o $@ -L ring-tower.mtl -S ring_tower_mtl

ring-tower-hull.obj: $(RING_PARTS)
	wavefront-obj-tool -c -o $@ $^


clean:
	rm -f *~ ring-tower.stl ring-tower.obj ring-tower-hull.obj
	rm -rf ring-parts
