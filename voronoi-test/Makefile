#
#
#

VORONOI=../../voronoi/voronoi_main
LIBS='(seth pbm)' '(foldling command-line)' '(seth strings)' '(seth math-3d)'

all: lines.png

voronoi-test: voronoi-test-chicken.scm
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

lines: points
	cat $^ | $(VORONOI) > $@

lines.pnm: lines
	cat $^ | ./voronoi-test-chibi.scm > $@

lines.png: lines.pnm
	pnmtopng $^ > $@

clean:
	rm -f *~ voronoi-test lines lines.pnm lines.png

very-clean: clean
	rm -rf seth snow srfi foldling
