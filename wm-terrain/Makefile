#
#
#

# WAVEFRONT_OBJ_TOOL=wavefront-obj-tool
WAVEFRONT_OBJ_TOOL=../../wavefront-obj-tools/wavefront-obj-tool-gauche.scm
L_SYSTEM_TREE=../L-system-tree/L-system-tree
OPENSCAD_FN=10
COMBINE_DISTANCE=0.2

SKYBOX_WIDTH=640
SKYBOX_HEIGTH=640

LIBS='(seth pbm)' '(foldling command-line)' '(seth obj-model)' '(srfi 95)' '(seth graph)' '(seth scad-model)' '(seth octree)'


%.obj.gz: %.obj
	cat $< | gzip -9 - > $@

%.pnm: %.png
	pngtopnm $^ > $@

%.png: %.pnm
	pnmtopng $^ > $@



TERRAIN_TARGETS= \
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
		terrain-textured.obj terrain-texture.png terrain.mtl rez-combined-trees.js blue-green-transparent-cube.fbx


all: $(TERRAIN_TARGETS)


terrain-textured.obj: terrain.obj
	$(WAVEFRONT_OBJ_TOOL) \
	-L terrain.mtl -S terrain_mtl \
	-R 0 0 0.7071067690849304 0.7071067690849304 \
	-s 2000 \
	--center \
	$^ \
	> $@



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

###

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

# cat $^ | grep -v 'no face' > /tmp/x # XXX
# mv /tmp/x tree-locations.dat # XXX



### skybox

skybox0.png skybox1.png skybox2.png skybox3.png skybox4.png skybox5.png: skybox.pov skybox.ini
	povray +W${SKYBOX_WIDTH} +H${SKYBOX_HEIGTH} skybox.ini

blank.pnm:
	pbmmake -black ${SKYBOX_WIDTH} ${SKYBOX_HEIGTH} > $@

skybox-row0.pnm: skybox0.pnm blank.pnm
	pnmcat -lr blank.pnm $^ blank.pnm > $@

skybox-row1.pnm: skybox1.pnm skybox2.pnm skybox3.pnm skybox4.pnm
	pnmcat -lr $^ > $@

skybox-row2.pnm: skybox5.pnm blank.pnm
	pnmcat -lr blank.pnm $^ blank.pnm > $@

skybox.pnm: skybox-row0.pnm skybox-row1.pnm skybox-row2.pnm
	pnmcat -tb $^ > $@



###



upload: $(TERRAIN_TARGETS) skybox.png
	mkdir -p wm-terrain
	cp $^ wm-terrain/
	rsync --delete -avP wm-terrain/ headache.hungry.com:public_html/hifi/wm-terrain/


clean:
	rm -f *~
	rm -rf lines points terrain.scad
	rm -rf wm-terrain
	rm -rf terrain-underground-floor-textured.obj
	rm -rf terrain-underground-textured.obj
	rm -f terrain-lines.png terrain-lines.pnm
	rm -f tree-locations.png
	rm -f terrain-hull.obj
	rm -f terrain-textured.obj
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
		combined-trees-5-3.obj.gz combined-trees-5-4.obj.gz combined-trees-5-5.obj.gz
	rm -f rez-combined-trees.js



very-clean: clean
	rm -rf seth snow srfi foldling
	rm -f $(TREE_TARGETS)
	rm -f tree0-trunk.scad tree0-leaves.scad tree0-hull.scad tree0-hull.stl tree0-leaves.obj tree0-trunk.obj
	rm -f tree1-trunk.scad tree1-leaves.scad tree1-hull.scad tree1-hull.stl tree1-leaves.obj tree1-trunk.obj
	rm -f tree2-trunk.scad tree2-leaves.scad tree2-hull.scad tree2-hull.stl tree2-leaves.obj tree2-trunk.obj
