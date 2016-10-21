#
#
#

VORONOI=../../voronoi/voronoi
LIBS='(seth pbm)' '(foldling command-line)' '(seth obj-model)' '(srfi 95)' '(seth graph)' '(seth scad-model)'
WIDTH=80
HEIGHT=80



terrain-parts/terrain%.stl: terrain-parts.scad terrain.scad
	mkdir -p terrain-parts
	openscad -D xslice=`expr substr $@ 22 1` -D zslice=`expr substr $@ 23 1` -o $@ $<


terrain-parts/terrain%.obj: terrain-parts/terrain%.stl
	wavefront-obj-tool -c -n -T 1024 1024 $^ -o $@ -L terrain.mtl -S terrain_mtl



all: terrain.png terrain.obj terrain

voronoi-terrain: voronoi-terrain-chicken.scm
	csc -X r7rs $^ -o $@

libs:
	snow2 -p 'http://foldling.org/snow2/index.scm' install $(LIBS)

link-libs: very-clean
	snow2 -s \
		-p 'http://foldling.org/snow2/index.scm' \
		-p '../../snow2-packages/seth' \
		-p '../../snow2-packages/snow' \
		-p '../../seth-snow2-misc' \
		install $(LIBS)

lines: points
	cat $^ | cut -f1,3 -d' ' | $(VORONOI) 0 0 $(WIDTH) $(HEIGHT) > $@

terrain.pnm: lines points
	./voronoi-terrain-gauche.scm --pnm --input-width $(WIDTH) --input-height $(HEIGHT) lines points > $@

terrain.obj: lines points
	./voronoi-terrain-gauche.scm --obj \
			--output-x-size 1024 --output-y-size 1024 --output-z-size 1024 \
			--input-width $(WIDTH) --input-height $(HEIGHT) \
			lines points > $@

terrain.scad: lines points
	./voronoi-terrain-gauche.scm --scad \
			--output-x-size 1024 --output-y-size 1024 --output-z-size 1024 \
			--input-width $(WIDTH) --input-height $(HEIGHT) \
			lines points > $@

terrain.png: terrain.pnm
	pnmtopng $^ > $@

TERRAIN_PARTS=$(foreach xz,00 01 02 03 04 05 06 07 10 11 12 13 14 15 16 17 20 21 22 23 24 25 26 27 30 31 32 33 34 35 36 37 40 41 42 43 44 45 46 47 50 51 52 53 54 55 56 57 60 61 62 63 64 65 66 67 70 71 72 73 74 75 76 77,terrain-parts/terrain${xz}.obj)

terrain: $(TERRAIN_PARTS)
	cp terrain.mtl terrain-parts/
	cp terrain-texture.png terrain-parts/


clean:
	rm -f *~ voronoi-terrain lines terrain.pnm terrain.png terrain.obj terrain.scad
	rm -rf terrain-parts

very-clean: clean
	rm -rf seth snow srfi foldling
