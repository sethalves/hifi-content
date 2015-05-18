#
#
#


OBJTOOL=../../wavefront-obj-tools/wavefront-obj-tool-gauche.scm


parts/%.stl: airhockey-table-hull.scad
	mkdir -p parts
	openscad -D combined=0 -D nth="`basename -s .stl $@`" -o $@ $^

%.stl: %.scad
	openscad -o $@ $^

%.obj: %.stl
	#../../wavefront-obj-tools/wavefront-obj-tool-gauche.scm -n -c $^ -o $@
	$(OBJTOOL) -n -c $^ -o $@


PARTS=$(foreach nth,1 2 3 4 5 6 7 8 9 10,parts/${nth}.obj)


# ---

all: airhockey-table-hull.obj


airhockey-table-hull.obj: $(PARTS)
	$(OBJTOOL) -c -o $@ $^



upload: airhockey-table-hull.obj
	scp $^ headache:public_html/hifi/

clean:
	rm -f *~
	rm -rf parts
	rm -f airhockey-table-hull.obj airhockey-table-hull.stl
