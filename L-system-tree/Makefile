#
#
#

all: L-system-tree

L-system-tree: L-system-tree.scm
	csc -X r7rs $^ -o $@

install: L-system-tree
	cp $^ /usr/local/bin/

uninstall:
	rm -f /usr/local/bin/L-system-tree

libs:
	snow2 -p 'http://foldling.org/snow2/index.scm' install '(seth obj-model)' '(foldling command-line)'



# ---

# make && ./L-system-tree -t iixxyyo -o ok.scad && openscad ok.scad 


tree0.scad: L-system-tree
	./L-system-tree -t iixxyiO -o tree0.scad

tree0.stl: tree0.scad
	openscad -o $@ $^

tree0.obj: tree0.stl
	- ivcon $^ $@.tmp.obj
	wavefront-obj-tool -m 1 -c $@.tmp.obj -o $@




clean:
	rm -f *~ L-system-tree
	rm -f tree0.scad tree0.stl tree0.obj.tmp.obj tree0.obj

very-clean: clean
	rm -rf seth snow srfi foldling

