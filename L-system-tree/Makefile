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

clean:
	rm -f *~ L-system-tree

very-clean: clean
	rm -rf seth snow srfi foldling

