#
#
#

VORONOI=../../voronoi/voronoi
LIBS='(seth pbm)' '(foldling command-line)' '(seth obj-model)' '(srfi 95)' '(seth graph)'
WIDTH=200
HEIGHT=200


all: lines.png lines.obj

voronoi-terrain: voronoi-terrain-chicken.scm
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
	cat $^ | cut -f1,3 -d' ' | $(VORONOI) 0 0 $(WIDTH) $(HEIGHT) > $@

lines.pnm: lines points
	./voronoi-terrain-gauche.scm --pnm --input-width $(WIDTH) --input-height $(HEIGHT) lines points > $@

lines.obj: lines points
	./voronoi-terrain-gauche.scm --obj \
			--output-x-size 200 --output-y-size 200 --output-z-size 200 \
			--input-width $(WIDTH) --input-height $(HEIGHT) \
			lines points > $@

lines.png: lines.pnm
	pnmtopng $^ > $@

clean:
	rm -f *~ voronoi-terrain lines lines.pnm lines.png lines.obj

very-clean: clean
	rm -rf seth snow srfi foldling
