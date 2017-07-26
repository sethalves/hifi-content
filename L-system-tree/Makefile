#
#
#

# WAVEFRONT_OBJ_TOOL=wavefront-obj-tool
WAVEFRONT_OBJ_TOOL=../../wavefront-obj-tools/wavefront-obj-tool-gauche.scm
L_SYSTEM_TREE=./L-system-tree

OPENSCAD_FN=16
COMBINE_DISTANCE=0.2


.PRECIOUS: %.obj %.stl

%.obj.gz: %.obj
	cat $< | gzip -9 - > $@


TARGETS=tree0.obj.gz tree0-hull.obj.gz \
		tree1.obj.gz tree1-hull.obj.gz \
		tree2.obj.gz tree2-hull.obj.gz \
		tree3.obj.gz tree3-hull.obj.gz


# TARGETS=tree0.obj tree0-hull.obj \
# 		tree1.obj tree1-hull.obj \
# 		tree2.obj tree2-hull.obj

all: L-system-tree $(TARGETS)

# ---

L-system-tree: L-system-tree.scm
	csc -X r7rs $^ -o $@

install: L-system-tree
	cp $^ /usr/local/bin/

uninstall:
	rm -f /usr/local/bin/L-system-tree

libs:
	snow2 -p 'http://foldling.org/snow2/index.scm' install '(seth obj-model)' '(foldling command-line)'


link-libs: very-clean
	snow2 -s \
	-p 'http://foldling.org/snow2/index.scm' \
	-p '../../snow2-packages/seth/' \
	install '(seth obj-model)' '(foldling command-line)'



include L-system-tree.make

# ---


upload: ${TARGETS}
	mkdir -p trees
	cp $^ trees/
	rsync --delete -avP trees/ headache.hungry.com:public_html/hifi/trees/


clean:
	rm -f *~ L-system-tree
	rm -f tree0.obj tree0.obj.gz
	rm -f tree0-leaves.scad tree0-leaves.stl tree0-leaves.obj.tmp.obj tree0-leaves.obj
	rm -f tree0-trunk.scad tree0-trunk.stl tree0-trunk.obj.tmp.obj tree0-trunk.obj
	rm -f tree0-hull.scad tree0-hull.stl tree0-hull.obj.tmp.obj tree0-hull.obj tree0-hull.obj.gz
	rm -f tree1.obj tree1.obj.gz
	rm -f tree1-leaves.scad tree1-leaves.stl tree1-leaves.obj.tmp.obj tree1-leaves.obj
	rm -f tree1-trunk.scad tree1-trunk.stl tree1-trunk.obj.tmp.obj tree1-trunk.obj
	rm -f tree1-hull.scad tree1-hull.stl tree1-hull.obj.tmp.obj tree1-hull.obj tree1-hull.obj.gz
	rm -f tree2.obj tree2.obj.gz
	rm -f tree2-leaves.scad tree2-leaves.stl tree2-leaves.obj.tmp.obj tree2-leaves.obj
	rm -f tree2-trunk.scad tree2-trunk.stl tree2-trunk.obj.tmp.obj tree2-trunk.obj
	rm -f tree2-hull.scad tree2-hull.stl tree2-hull.obj.tmp.obj tree2-hull.obj tree2-hull.obj.gz
	rm -f tree3.obj tree3.obj.gz
	rm -f tree3-leaves.scad tree3-leaves.stl tree3-leaves.obj.tmp.obj tree3-leaves.obj
	rm -f tree3-trunk.scad tree3-trunk.stl tree3-trunk.obj.tmp.obj tree3-trunk.obj
	rm -f tree3-hull.scad tree3-hull.stl tree3-hull.obj.tmp.obj tree3-hull.obj tree3-hull.obj.gz
	rm -rf trees


very-clean: clean
	rm -rf seth snow srfi foldling

