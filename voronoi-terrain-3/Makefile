


#
#
#

# WAVEFRONT_OBJ_TOOL=wavefront-obj-tool
WAVEFRONT_OBJ_TOOL=../../wavefront-obj-tools/wavefront-obj-tool-gauche.scm
ATP_GET=/home/seth/src/hifi/build/tools/atp-client/atp-client
VHACD=/home/seth/src/hifi/build/tools/vhacd-util/vhacd-util

VORONOI=../../voronoi/voronoi
LIBS='(seth pbm)' '(foldling command-line)' '(seth obj-model)' '(srfi 95)' '(seth graph)' '(seth scad-model)' '(seth octree)'
WIDTH=80
HEIGHT=80

OUTPUT_WIDTH=3072
OUTPUT_DEPTH=3072
OUTPUT_HEIGHT=1024

PAINT_SAVE_N=6085


%.stl: %.scad
	openscad -o $@ $^

%.obj.gz: %.obj
	cat $< | gzip -9 - > $@


TERRAIN_TARGETS=terrain-lines.png terrain-hull.obj \
		combined-trees-0-0.obj.gz combined-trees-0-1.obj.gz combined-trees-0-2.obj.gz \
		combined-trees-0-3.obj.gz combined-trees-0-4.obj.gz combined-trees-0-5.obj.gz \
		combined-trees-1-0.obj.gz combined-trees-1-1.obj.gz combined-trees-1-2.obj.gz \
		combined-trees-1-3.obj.gz combined-trees-1-4.obj.gz combined-trees-1-5.obj.gz \
		combined-trees-2-0.obj.gz combined-trees-2-1.obj.gz combined-trees-2-2.obj.gz \
		combined-trees-2-3.obj.gz combined-trees-2-4.obj.gz combined-trees-2-5.obj.gz \
		combined-trees-3-0.obj.gz combined-trees-3-1.obj.gz combined-trees-3-2.obj.gz \
		combined-trees-3-3.obj.gz combined-trees-3-4.obj.gz combined-trees-3-5.obj.gz \
		combined-trees-4-0.obj.gz combined-trees-4-1.obj.gz combined-trees-4-2.obj.gz \
		combined-trees-4-3.obj.gz combined-trees-4-4.obj.gz combined-trees-4-5.obj.gz \
		combined-trees-5-0.obj.gz combined-trees-5-1.obj.gz combined-trees-5-2.obj.gz \
		combined-trees-5-3.obj.gz combined-trees-5-4.obj.gz combined-trees-5-5.obj.gz \
		rez-combined-trees.js \
		terrain-textured.obj terrain-texture.png terrain.mtl cave-floor.jpg terrain-hull.obj


all: $(TERRAIN_TARGETS)


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

# terrain-reduced.obj: terrain.obj
# 	meshlabserver -i $< -o $@ -s terrain.mlx # -om vc vn

# terrain.scad: terrain-reduced.obj
# 	$(WAVEFRONT_OBJ_TOOL) $^ -o $@

terrain.scad: terrain.obj
	$(WAVEFRONT_OBJ_TOOL) $^ -o $@

terrain-caves.obj:
	- $(ATP_GET) -d eschatology.hungry.com atp:/voxel-paint-save-$(PAINT_SAVE_N).obj -o $@

terrain-caves-water-tight.obj: terrain-caves.obj
	$(WAVEFRONT_OBJ_TOOL) $^ -C 0.01 -c -s 64 -o $@

terrain-caves-reduced.obj: terrain-caves-water-tight.obj terrain-caves.mlx
	meshlabserver -i $< -o $@ -s terrain-caves.mlx # -om vc vn

terrain-caves.scad: terrain-caves-reduced.obj
	$(WAVEFRONT_OBJ_TOOL) $^ -C 0.01 -c -o $@


###

# terrain-with-caves.stl: terrain-with-caves.scad terrain.scad terrain-negative.scad terrain-caves.scad

terrain-with-caves.stl: terrain-with-caves.scad terrain.scad terrain-negative.scad
	openscad -o $@ $<

terrain-with-caves-reduced.obj: terrain-with-caves.stl terrain-with-caves.mlx
	meshlabserver -i $< -o $@ -s terrain-with-caves.mlx # -om vc vn

###

terrain-texture.ppm: points lines
	./voronoi-terrain-gauche.scm --texture \
		--input-width $(WIDTH) --input-height $(HEIGHT) \
		--output-x-size 512 --output-z-size 512 \
		lines points > $@

terrain-texture.png: terrain-texture.ppm
	pnmtopng $^ > $@



terrain-underground-textured.obj: terrain-with-caves-reduced.obj
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

###

L_SYSTEM_TREE=../L-system-tree/L-system-tree

OPENSCAD_FN=10
COMBINE_DISTANCE=0.2

include ../L-system-tree/L-system-tree.make

TREE_TARGETS=\
	tree0.obj tree0-hull.obj \
	tree1.obj tree1-hull.obj \
	tree2.obj tree2-hull.obj


combined-trees-0-0.obj combined-trees-0-1.obj combined-trees-0-2.obj \
combined-trees-0-3.obj combined-trees-0-4.obj combined-trees-0-5.obj \
combined-trees-1-0.obj combined-trees-1-1.obj combined-trees-1-2.obj \
combined-trees-1-3.obj combined-trees-1-4.obj combined-trees-1-5.obj \
combined-trees-2-0.obj combined-trees-2-1.obj combined-trees-2-2.obj \
combined-trees-2-3.obj combined-trees-2-4.obj combined-trees-2-5.obj \
combined-trees-3-0.obj combined-trees-3-1.obj combined-trees-3-2.obj \
combined-trees-3-3.obj combined-trees-3-4.obj combined-trees-3-5.obj \
combined-trees-4-0.obj combined-trees-4-1.obj combined-trees-4-2.obj \
combined-trees-4-3.obj combined-trees-4-4.obj combined-trees-4-5.obj \
combined-trees-5-0.obj combined-trees-5-1.obj combined-trees-5-2.obj \
combined-trees-5-3.obj combined-trees-5-4.obj combined-trees-5-5.obj \
tree-locations.dat: scatter-trees-gauche.scm scatter-trees-main.sld terrain-textured.obj $(TREE_TARGETS)
	./$< > tree-locations.dat

tree-locations.png: tree-locations.dat
	gnuplot trees.gnuplot
	eog $@


###


upload: $(TERRAIN_TARGETS)
	mkdir -p voronoi-terrain-3
	cp $^ voronoi-terrain-3/
	rsync --delete -avP voronoi-terrain-3/ headache.hungry.com:public_html/hifi/voronoi-terrain-3/


clean:
	rm -f *~
	rm -rf lines points terrain.scad
	rm -rf voronoi-terrain-3
	rm -rf terrain-texture.png
	rm -rf terrain-texture.ppm
	rm -rf terrain-textured.obj
	rm -rf terrain-underground-floor-textured.obj
	rm -rf terrain-underground-textured.obj
	rm -rf terrain-with-caves.stl
	rm -f terrain-lines.png terrain-lines.pnm
	rm -f terrain-caves.obj terrain-caves-water-tight.obj terrain-caves.scad
	rm -f terrain-caves-reduced.obj
	rm -f terrain-sans-caves.obj
	rm -f terrain-sans-caves.stl
	rm -f terrain.obj
	rm -f tree-locations.png
	rm -f terrain-hull.obj terrain-reduced.obj terrain-with-caves-reduced.obj
	rm -f combined-trees-0-0.obj combined-trees-0-1.obj combined-trees-0-2.obj \
		combined-trees-0-3.obj combined-trees-0-4.obj combined-trees-0-5.obj \
		combined-trees-1-0.obj combined-trees-1-1.obj combined-trees-1-2.obj \
		combined-trees-1-3.obj combined-trees-1-4.obj combined-trees-1-5.obj \
		combined-trees-2-0.obj combined-trees-2-1.obj combined-trees-2-2.obj \
		combined-trees-2-3.obj combined-trees-2-4.obj combined-trees-2-5.obj \
		combined-trees-3-0.obj combined-trees-3-1.obj combined-trees-3-2.obj \
		combined-trees-3-3.obj combined-trees-3-4.obj combined-trees-3-5.obj \
		combined-trees-4-0.obj combined-trees-4-1.obj combined-trees-4-2.obj \
		combined-trees-4-3.obj combined-trees-4-4.obj combined-trees-4-5.obj \
		combined-trees-5-0.obj combined-trees-5-1.obj combined-trees-5-2.obj \
		combined-trees-5-3.obj combined-trees-5-4.obj combined-trees-5-5.obj \
		tree-locations.dat
	rm -f combined-trees-0-0.obj.gz combined-trees-0-1.obj.gz combined-trees-0-2.obj.gz \
		combined-trees-0-3.obj.gz combined-trees-0-4.obj.gz combined-trees-0-5.obj.gz \
		combined-trees-1-0.obj.gz combined-trees-1-1.obj.gz combined-trees-1-2.obj.gz \
		combined-trees-1-3.obj.gz combined-trees-1-4.obj.gz combined-trees-1-5.obj.gz \
		combined-trees-2-0.obj.gz combined-trees-2-1.obj.gz combined-trees-2-2.obj.gz \
		combined-trees-2-3.obj.gz combined-trees-2-4.obj.gz combined-trees-2-5.obj.gz \
		combined-trees-3-0.obj.gz combined-trees-3-1.obj.gz combined-trees-3-2.obj.gz \
		combined-trees-3-3.obj.gz combined-trees-3-4.obj.gz combined-trees-3-5.obj.gz \
		combined-trees-4-0.obj.gz combined-trees-4-1.obj.gz combined-trees-4-2.obj.gz \
		combined-trees-4-3.obj.gz combined-trees-4-4.obj.gz combined-trees-4-5.obj.gz \
		combined-trees-5-0.obj.gz combined-trees-5-1.obj.gz combined-trees-5-2.obj.gz \
		combined-trees-5-3.obj.gz combined-trees-5-4.obj.gz combined-trees-5-5.obj.gz \
		rez-combined-trees.js
	rm -f $(TREE_TARGETS)


very-clean: clean
	rm -rf seth snow srfi foldling
