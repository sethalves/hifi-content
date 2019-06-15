


#
#
#

# WAVEFRONT_OBJ_TOOL=wavefront-obj-tool
WAVEFRONT_OBJ_TOOL=../../wavefront-obj-tools/wavefront-obj-tool-gauche.scm
ATP_GET=../../hifi/build/tools/atp-client/atp-client
VHACD=/home/seth/src/hifi/build/tools/vhacd-util/vhacd-util

OPENSCAD=openscad-nightly

OVEN=../../hifi/build/tools/oven/oven
BAKEDDIR=baked

VORONOI=../../voronoi/voronoi
LIBS='(seth pbm)' '(foldling command-line)' '(seth obj-model)' '(srfi 95)' '(seth graph)' '(seth scad-model)' '(seth octree)'
WIDTH=80
HEIGHT=80

OUTPUT_WIDTH=1024
OUTPUT_DEPTH=1024
OUTPUT_HEIGHT=1024

PAINT_SAVE_N=6085


all: terrain-lines.png terrain.scad terrain-textured.obj terrain-hull.obj ${BAKEDDIR}/terrain-textured/baked # terrain-sans-caves.obj

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

terrain.scad: terrain.obj
	$(WAVEFRONT_OBJ_TOOL) $^ -o $@

terrain-caves.obj:
	- $(ATP_GET) -d eschatology.hungry.com atp:/voxel-paint-save-$(PAINT_SAVE_N).obj -o $@

terrain-caves.scad: terrain-caves.obj
	$(WAVEFRONT_OBJ_TOOL) $^ -c -s 64 -o $@

terrain-with-caves.stl: terrain-with-caves.scad terrain.scad terrain-negative.scad terrain-caves.scad
	${OPENSCAD} -o $@ $<

terrain-with-caves.obj: terrain-with-caves.stl
	$(WAVEFRONT_OBJ_TOOL) $^ -o $@

###

terrain-texture.ppm: points lines
	./voronoi-terrain-gauche.scm --texture \
		--input-width $(WIDTH) --input-height $(HEIGHT) \
		--output-x-size 512 --output-z-size 512 \
		lines points > $@

terrain-texture.png: terrain-texture.ppm
	pnmflip -leftright -rotate180 $^ | pnmtopng - > $@

#	pnmtopng $^ > $@



terrain-underground-textured.obj: terrain-with-caves.obj
	$(WAVEFRONT_OBJ_TOOL) -n $< -o $@ -L terrain.mtl -S dirt_mtl

terrain-underground-floor-textured.obj: terrain-underground-textured.obj
	$(WAVEFRONT_OBJ_TOOL) -n -U $< -o $@ -L terrain.mtl -S cave_floor_mtl -m 0.2

terrain-textured.obj: terrain-underground-floor-textured.obj terrain-texture.png
	$(WAVEFRONT_OBJ_TOOL) -n -c -T -M $(OUTPUT_WIDTH) $(OUTPUT_DEPTH) $< -o $@ -L terrain.mtl -S terrain_mtl



terrain-lines.png: terrain-lines.pnm
	pnmtopng $^ > $@


terrain-hull.obj: terrain-textured.obj
	$(WAVEFRONT_OBJ_TOOL) --bullet-hull -c $^ > $@

#	$(VHACD) -f -i $^ -o $@


${BAKEDDIR}/terrain-textured/baked: terrain-textured.obj
	${OVEN} -i $^ -o ${BAKEDDIR} -t model

UNBAKED_FILES=terrain-textured.obj terrain-texture.png terrain.mtl cave-floor.jpg terrain-hull.obj

upload: ${UNBAKED_FILES} ${BAKEDDIR}/terrain-textured/baked
	mkdir -p voronoi-terrain
	rsync -avP ${BAKEDDIR}/terrain-textured/baked/ voronoi-terrain/
	cp ${UNBAKED_FILES} voronoi-terrain/
	rsync --delete -avP voronoi-terrain/ headache.hungry.com:public_html/hifi/voronoi-terrain-2/


clean:
	rm -rf lines points terrain.scad
	rm -rf voronoi-terrain
	rm -rf terrain-texture.png
	rm -rf terrain-texture.ppm
	rm -rf terrain-textured.obj
	rm -rf terrain-underground-floor-textured.obj
	rm -rf terrain-underground-textured.obj
	rm -rf terrain-with-caves.stl
	rm -f terrain-lines.png terrain-lines.pnm
	rm -f terrain-caves-scaled.obj terrain-caves.scad
	rm -f terrain-caves-reduced.obj
	rm -f terrain-sans-caves.obj
	rm -f terrain-sans-caves.stl
	rm -f terrain.obj
	rm -f terrain-reduced.obj
	rm -f terrain-hull.obj
	rm -f terrain-with-caves.obj
	rm -rf baked

very-clean: clean
	rm -rf seth snow srfi foldling
