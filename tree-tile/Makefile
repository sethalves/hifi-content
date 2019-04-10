# WAVEFRONT_OBJ_TOOL=wavefront-obj-tool
# WAVEFRONT_OBJ_TOOL=../../wavefront-obj-tools/wavefront-obj-tool-gauche.scm
WAVEFRONT_OBJ_TOOL=../../wavefront-obj-tools/wavefront-obj-tool
L_SYSTEM_TREE=../L-system-tree/L-system-tree
OPENSCAD=openscad-nightly
OPENSCAD_FN=10
COMBINE_DISTANCE=0.2

%.obj.gz: %.obj
	cat $< | gzip -9 - > $@

LIBS='(seth pbm)' '(foldling command-line)' '(seth obj-model)' '(srfi 95)' '(seth graph)' '(seth scad-model)' '(seth octree)'


all: tree-tile-0.obj


libs:
	snow2 -p 'http://foldling.org/snow2/index.scm' install $(LIBS)

link-libs: very-clean
	snow2 -s \
		-p 'http://foldling.org/snow2/index.scm' \
		-p '../../snow2-packages/seth' \
		-p '../../snow2-packages/snow' \
		-p '../../seth-snow2-misc' \
		install $(LIBS)

tree-tile-chicken: tree-tile-chicken.scm tree-tile-main.sld
	csc -X r7rs $< -o $@


###

include ../L-system-tree/L-system-tree.make

TREE_TARGETS=\
	tree0.obj tree0-hull.obj \
	tree1.obj tree1-hull.obj \
	tree2.obj tree2-hull.obj


tree-tile-0.obj: tree-tile-chicken $(TREE_TARGETS)
	./tree-tile-chicken


upload: tree-tile-0.obj.gz
	rm -rf tree-tile
	mkdir -p tree-tile
	cp $^ tree-tile/
	rsync --delete -avP tree-tile/ headache.hungry.com:public_html/hifi/tree-tile/


clean:
	rm -f *~
	rm tree-tile-chicken
	rm -f tree-tile-0.obj tree-tile-0.obj.gz
	rm -rf tree-tile

very-clean: clean
	rm -f tree*-hull.obj tree*-hull.scad tree*-hull.stl
	rm -f tree*-leaves.obj tree*-leaves.scad
	rm -f tree*.obj tree*-trunk.obj tree*-trunk.scad
	rm -rf chibi foldling seth snow srfi
