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


# ---

tree0-trunk.scad: L-system-tree Makefile
	./L-system-tree -w 4 -t iixxyiO --skip-leaves -o tree0-trunk.scad

tree0-trunk.stl: tree0-trunk.scad
	openscad -o $@ $^

tree0-trunk.obj: tree0-trunk.stl
	- ivcon $^ $@.tmp.obj
	wavefront-obj-tool -c $@.tmp.obj -o $@ -S trunk_mtl



tree0-leaves.scad: L-system-tree Makefile
	./L-system-tree -w 4 -t iixxyiO --skip-trunk -o tree0-leaves.scad

tree0-leaves.stl: tree0-leaves.scad
	openscad -o $@ $^

tree0-leaves.obj: tree0-leaves.stl
	- ivcon $^ $@.tmp.obj
	wavefront-obj-tool -c $@.tmp.obj -o $@ -S leaves_mtl

tree0.obj: tree0-trunk.obj tree0-leaves.obj
	wavefront-obj-tool -L L-system-trees.mtl -o $@ $^



tree0-hull.scad: L-system-tree Makefile
	./L-system-tree -w 4 -t iixxyiO --hull -o tree0-hull.scad

tree0-hull.stl: tree0-hull.scad
	openscad -o $@ $^

tree0-hull.obj: tree0-hull.stl
	- ivcon $^ $@.tmp.obj
	wavefront-obj-tool -c $@.tmp.obj -o $@

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

