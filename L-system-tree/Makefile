#
#
#

# WAVEFRONT_OBJ_TOOL=wavefront-obj-tool
WAVEFRONT_OBJ_TOOL=../../wavefront-obj-tools/wavefront-obj-tool-gauche.scm

COMBINE_DISTANCE=0.2


.PRECIOUS: %.obj %.stl

%.obj.gz: %.obj
	cat $< | gzip -9 - > $@


TARGETS=tree0.obj.gz tree0-hull.obj.gz \
		tree1.obj.gz tree1-hull.obj.gz \
		tree2.obj.gz tree2-hull.obj.gz


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


%.stl: %.scad
	openscad -o $@ $^

%-trunk.obj: %-trunk.stl
	$(WAVEFRONT_OBJ_TOOL) -n -C $(COMBINE_DISTANCE) -c $^ -o $@ --color 0.65 0.16 0.16

%-leaves.obj: %-leaves.stl
	$(WAVEFRONT_OBJ_TOOL) -n -C $(COMBINE_DISTANCE) -c $^ -o $@ --color 0 1 0

%.obj: %.stl
	$(WAVEFRONT_OBJ_TOOL) -n -C $(COMBINE_DISTANCE) -c $^ -o $@

%.obj: %-trunk.obj %-leaves.obj
	$(WAVEFRONT_OBJ_TOOL) -n -C $(COMBINE_DISTANCE) -o $@ $^


# ---

TREE0_W=5
TREE0_SHAPE=iixxyiO

tree0-leaves.scad: L-system-tree Makefile
	./L-system-tree -w $(TREE0_W) -t $(TREE0_SHAPE) --skip-trunk -o $@

tree0-trunk.scad: L-system-tree Makefile tree0-leaves.scad
	./L-system-tree -w $(TREE0_W) -t $(TREE0_SHAPE) --skip-leaves -o $@ -d tree0-leaves.scad

tree0-hull.scad: L-system-tree Makefile
	./L-system-tree -w $(TREE0_W) -t $(TREE0_SHAPE) --hull -o tree0-hull.scad

# ---

TREE1_W=3
TREE1_SHAPE=iixyyyiO

tree1-leaves.scad: L-system-tree Makefile
	./L-system-tree -w $(TREE1_W) -t $(TREE1_SHAPE) --skip-trunk -o $@

tree1-trunk.scad: L-system-tree Makefile tree1-leaves.scad
	./L-system-tree -w $(TREE1_W) -t $(TREE1_SHAPE) --skip-leaves -o $@ -d tree1-leaves.scad

tree1-hull.scad: L-system-tree Makefile
	./L-system-tree -w $(TREE1_W) -t $(TREE1_SHAPE) --hull -o tree1-hull.scad

# ---

TREE2_W=4
TREE2_SHAPE=iixyxxiO

tree2-leaves.scad: L-system-tree Makefile
	./L-system-tree -w $(TREE2_W) -t $(TREE2_SHAPE) --skip-trunk -o $@

tree2-trunk.scad: L-system-tree Makefile tree2-leaves.scad
	./L-system-tree -w $(TREE2_W) -t $(TREE2_SHAPE) --skip-leaves -o $@ -d tree2-leaves.scad

tree2-hull.scad: L-system-tree Makefile
	./L-system-tree -w $(TREE2_W) -t $(TREE2_SHAPE) --hull -o tree2-hull.scad

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
	rm -rf trees


very-clean: clean
	rm -rf seth snow srfi foldling

