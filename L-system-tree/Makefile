#
#
#

# WAVEFRONT_OBJ_TOOL=wavefront-obj-tool
WAVEFRONT_OBJ_TOOL=../../wavefront-obj-tools/wavefront-obj-tool-gauche.scm

all: L-system-tree

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


%.stl: %.scad
	openscad -o $@ $^

%-trunk.obj: %-trunk.stl
	$(WAVEFRONT_OBJ_TOOL) -n -c $^ -o $@ -S trunk_mtl

%-leaves.obj: %-leaves.stl
	$(WAVEFRONT_OBJ_TOOL) -n -c $^ -o $@ -S leaves_mtl

%.obj: %.stl
	$(WAVEFRONT_OBJ_TOOL) -n -c $^ -o $@

%.obj: %-trunk.obj %-leaves.obj
	$(WAVEFRONT_OBJ_TOOL) -n -L L-system-trees.mtl -o $@ $^


# ---

TREE0_W=5
TREE0_SHAPE=iixxyiO

tree0-trunk.scad: L-system-tree Makefile
	./L-system-tree -w $(TREE0_W) -t $(TREE0_SHAPE) --skip-leaves -o $@

tree0-leaves.scad: L-system-tree Makefile
	./L-system-tree -w $(TREE0_W) -t $(TREE0_SHAPE) --skip-trunk -o $@

tree0-hull.scad: L-system-tree Makefile
	./L-system-tree -w $(TREE0_W) -t $(TREE0_SHAPE) --hull -o tree0-hull.scad

# ---

TREE1_W=3
TREE1_SHAPE=iixyyyiO

tree1-trunk.scad: L-system-tree Makefile
	./L-system-tree -w $(TREE1_W) -t $(TREE1_SHAPE) --skip-leaves -o $@

tree1-leaves.scad: L-system-tree Makefile
	./L-system-tree -w $(TREE1_W) -t $(TREE1_SHAPE) --skip-trunk -o $@

tree1-hull.scad: L-system-tree Makefile
	./L-system-tree -w $(TREE1_W) -t $(TREE1_SHAPE) --hull -o tree1-hull.scad

# ---

TREE2_W=4
TREE2_SHAPE=iixyxxiO

tree2-trunk.scad: L-system-tree Makefile
	./L-system-tree -w $(TREE2_W) -t $(TREE2_SHAPE) --skip-leaves -o $@

tree2-leaves.scad: L-system-tree Makefile
	./L-system-tree -w $(TREE2_W) -t $(TREE2_SHAPE) --skip-trunk -o $@

tree2-hull.scad: L-system-tree Makefile
	./L-system-tree -w $(TREE2_W) -t $(TREE2_SHAPE) --hull -o tree2-hull.scad

# ---


upload: L-system-trees.mtl \
		tree0.obj tree0-hull.obj \
        tree1.obj tree1-hull.obj \
        tree2.obj tree2-hull.obj
	scp $^ headache:public_html/hifi/


clean:
	rm -f *~ L-system-tree
	rm -f tree0.obj
	rm -f tree0-leaves.scad tree0-leaves.stl tree0-leaves.obj.tmp.obj tree0-leaves.obj
	rm -f tree0-trunk.scad tree0-trunk.stl tree0-trunk.obj.tmp.obj tree0-trunk.obj
	rm -f tree0-hull.scad tree0-hull.stl tree0-hull.obj.tmp.obj tree0-hull.obj
	rm -f tree1.obj
	rm -f tree1-leaves.scad tree1-leaves.stl tree1-leaves.obj.tmp.obj tree1-leaves.obj
	rm -f tree1-trunk.scad tree1-trunk.stl tree1-trunk.obj.tmp.obj tree1-trunk.obj
	rm -f tree1-hull.scad tree1-hull.stl tree1-hull.obj.tmp.obj tree1-hull.obj
	rm -f tree2.obj
	rm -f tree2-leaves.scad tree2-leaves.stl tree2-leaves.obj.tmp.obj tree2-leaves.obj
	rm -f tree2-trunk.scad tree2-trunk.stl tree2-trunk.obj.tmp.obj tree2-trunk.obj
	rm -f tree2-hull.scad tree2-hull.stl tree2-hull.obj.tmp.obj tree2-hull.obj


very-clean: clean
	rm -rf seth snow srfi foldling

