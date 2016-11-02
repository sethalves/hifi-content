#
#
#

# WAVEFRONT_OBJ_TOOL=wavefront-obj-tool
WAVEFRONT_OBJ_TOOL=../../wavefront-obj-tools/wavefront-obj-tool-gauche.scm

VORONOI=../../voronoi/voronoi
LIBS='(seth pbm)' '(foldling command-line)' '(seth obj-model)' '(srfi 95)' '(seth graph)' '(seth scad-model)' '(seth octree)'
WIDTH=80
HEIGHT=80

OUTPUT_WIDTH=1024
OUTPUT_DEPTH=1024
OUTPUT_HEIGHT=1024


terrain-parts/terrain%.stl: terrain-parts.scad terrain.scad
	mkdir -p terrain-parts
	openscad -D xslice=`expr substr $@ 22 1` \
				-D zslice=`expr substr $@ 23 1` \
				-D width=$(OUTPUT_WIDTH) \
				-D depth=$(OUTPUT_DEPTH) \
				-o $@ $<


terrain-parts/terrain%.obj: terrain-parts/terrain%.stl
	$(WAVEFRONT_OBJ_TOOL) -c -n -M $(OUTPUT_WIDTH) $(OUTPUT_DEPTH) $^ -o $@ -L terrain.mtl -S terrain_mtl


json-parts/terrain%.json: terrain-parts/terrain%.obj terrain-json.m4
	mkdir -p json-parts
	export YSIZE=`$(WAVEFRONT_OBJ_TOOL) --py $<`; \
	m4 -DPART=`expr substr $@ 19 2` \
		-DYSIZE=$$YSIZE \
		-DX=`expr substr $@ 19 1` \
		-DZ=`expr substr $@ 20 1` \
		-DYVAL=`echo "scale = 5; ($$YSIZE / 2) - 82" | bc` \
		terrain-json.m4 > $@


all: terrain-lines.png terrain.scad terrain-textured.obj # terrain.obj terrain terrain.json

voronoi-terrain-chicken: voronoi-terrain-chicken.scm
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

points: heightmap.pgm
	./pnm-to-height-map-gauche.scm $^ > $@

lines: points
	cat $^ | cut -f1,3 -d' ' | $(VORONOI) 0 0 $(WIDTH) $(HEIGHT) > $@

terrain-lines.pnm: lines points
	./voronoi-terrain-gauche.scm --pnm --input-width $(WIDTH) --input-height $(HEIGHT) lines points > $@

terrain.obj: lines points
	./voronoi-terrain-gauche.scm --obj \
			--output-x-size $(OUTPUT_WIDTH) --output-y-size $(OUTPUT_HEIGHT) --output-z-size $(OUTPUT_DEPTH) \
			--input-width $(WIDTH) --input-height $(HEIGHT) \
			lines points > $@

terrain.scad: lines points
	./voronoi-terrain-gauche.scm --scad \
			--output-x-size $(OUTPUT_WIDTH) --output-y-size $(OUTPUT_HEIGHT) --output-z-size $(OUTPUT_DEPTH) \
			--input-width $(WIDTH) --input-height $(HEIGHT) \
			lines points > $@


terrain-whole.stl: terrain.scad
	openscad -o $@ $<


terrain-caves.scad: cave-arcs make-caves-gauche.scm make-caves-main.sld
	./make-caves-gauche.scm $< > $@


terrain-caves.stl: terrain-caves.scad
	openscad -o $@ $<

terrain-with-caves.stl: terrain-with-caves.scad terrain-caves.scad terrain.scad
	openscad -o $@ $<

terrain-texture.ppm: points lines
	./voronoi-terrain-gauche.scm --texture \
		--input-width $(WIDTH) --input-height $(HEIGHT) \
		--output-x-size 512 --output-z-size 512 \
		lines points > $@


terrain-texture.png: terrain-texture.ppm
	pnmtopng $^ > $@


# terrain-textured.obj: terrain-whole.stl terrain-texture.png
# 	$(WAVEFRONT_OBJ_TOOL) -c -n -M $(OUTPUT_WIDTH) $(OUTPUT_DEPTH) $< -o $@ -L terrain.mtl -S terrain_mtl


terrain-underground-textured.obj: terrain-with-caves.stl
	$(WAVEFRONT_OBJ_TOOL) -c -n $< -o $@ -L terrain.mtl -S dirt_mtl


terrain-underground-floor-textured.obj: terrain-underground-textured.obj
	$(WAVEFRONT_OBJ_TOOL) -c -n -U $< -o $@ -L terrain.mtl -S cave_floor_mtl -m 0.2


terrain-textured.obj: terrain-underground-floor-textured.obj terrain-texture.png
	$(WAVEFRONT_OBJ_TOOL) -c -n -T -M $(OUTPUT_WIDTH) $(OUTPUT_DEPTH) $< -o $@ -L terrain.mtl -S terrain_mtl


terrain-lines.png: terrain-lines.pnm
	pnmtopng $^ > $@

PARTS=00 01 02 03 04 05 06 07 10 11 12 13 14 15 16 17 20 21 22 23 24 25 26 27 30 31 32 33 34 35 36 37 40 41 42 43 44 45 46 47 50 51 52 53 54 55 56 57 60 61 62 63 64 65 66 67 70 71 72 73 74 75 76 77

TERRAIN_PARTS=$(foreach xz,$(PARTS),terrain-parts/terrain${xz}.obj)

terrain: $(TERRAIN_PARTS) terrain-texture.png
	cp terrain.mtl terrain-parts/
	cp terrain-texture.png terrain-parts/


JSON_PARTS=$(foreach xz,$(PARTS),json-parts/terrain${xz}.json)

terrain.json: $(JSON_PARTS)
	cat $^ > $@

upload: terrain-textured.obj terrain-texture.png terrain.mtl cave-floor.jpg
	mkdir -p voronoi-terrain
	cp $^ voronoi-terrain/
	rsync --delete -avP voronoi-terrain/ headache.hungry.com:public_html/hifi/voronoi-terrain/


clean:
	rm -f points lines
	rm -f *~ terrain-lines.pnm terrain-lines.png terrain.obj terrain.scad terrain.json
	rm -rf terrain-parts voronoi-terrain json-parts
	rm -f terrain-textured.obj terrain-whole.stl terrain-caves.stl terrain-whole.obj
	rm -f terrain-texture.png terrain-texture.ppm
	rm -f terrain-caves.scad terrain-with-caves.stl
	rm -f terrain-underground-textured.obj terrain-underground-floor-textured.obj

very-clean: clean
	rm -rf seth snow srfi foldling
