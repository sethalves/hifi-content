# WAVEFRONT_OBJ_TOOL=wavefront-obj-tool
# WAVEFRONT_OBJ_TOOL=../../wavefront-obj-tools/wavefront-obj-tool-gauche.scm
WAVEFRONT_OBJ_TOOL=../../wavefront-obj-tools/wavefront-obj-tool
L_SYSTEM_TREE=../L-system-tree/L-system-tree
OPENSCAD_FN=10
COMBINE_DISTANCE=0.2

LIBS='(seth pbm)' '(foldling command-line)' '(seth obj-model)' '(srfi 95)' '(seth graph)' '(seth scad-model)' '(seth octree)'

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


clean:
	rm -f *~

very-clean: clean
