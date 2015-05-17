#
#
#

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


%.stl: %.scad
	openscad -o $@ $^

%-trunk.obj: %-trunk.stl
	- ivcon $^ $@.tmp.obj
	wavefront-obj-tool -c $@.tmp.obj -o $@ -S trunk_mtl

%-leaves.obj: %-leaves.stl
	- ivcon $^ $@.tmp.obj
	wavefront-obj-tool -c $@.tmp.obj -o $@ -S leaves_mtl

%.obj: %.stl
	- ivcon $^ $@.tmp.obj
	wavefront-obj-tool -c $@.tmp.obj -o $@

%.obj: %-trunk.obj %-leaves.obj
	wavefront-obj-tool -L L-system-trees.mtl -o $@ $^


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


upload: L-system-trees.mtl tree0.obj tree0-hull.obj
	scp $^ headache:public_html/hifi/


clean:
	rm -f *~ L-system-tree
	rm -f tree0.obj
	rm -f tree0-leaves.scad tree0-leaves.stl tree0-leaves.obj.tmp.obj tree0-leaves.obj
	rm -f tree0-trunk.scad tree0-trunk.stl tree0-trunk.obj.tmp.obj tree0-trunk.obj
	rm -f tree0-hull.scad tree0-hull.stl tree0-hull.obj.tmp.obj tree0-hull.obj

very-clean: clean
	rm -rf seth snow srfi foldling

