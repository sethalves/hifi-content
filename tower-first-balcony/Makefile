#
#
#

first-balcony-parts/%.stl: first-balcony.scad
	mkdir -p first-balcony-parts
	openscad -D combined=0 -D nth="`basename -s .stl $@`" -o $@ $^

%.obj: %.stl
	wavefront-obj-tool -m 0.2 -n $^ -o $@ # -L first-balcony.mtl

%.obj.gz: %.obj
	cat $< | gzip -9 - > $@

PARTS=$(foreach nth,0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47,first-balcony-parts/${nth}.obj)

all: first-balcony-collision-hull.obj first-balcony.obj

# ---

first-balcony.stl: first-balcony.scad
	openscad -D combined=1 -o $@ $^

first-balcony.obj: first-balcony.stl
	wavefront-obj-tool -m 0.2 -n $^ -o $@ -L first-balcony.mtl -S balcony_mtl

first-balcony-collision-hull.obj: $(PARTS)
	wavefront-obj-tool -c -o $@ $^


# ---

clean:
	rm -rf *~ first-balcony-parts first-balcony-collision-hull.obj first-balcony.obj
	rm -f  first-balcony-collision-hull.obj.gz first-balcony.obj.gz
	rm -f first-balcony.obj.tmp.obj first-balcony.stl
	rm -rf first-balcony


# upload: first-balcony.obj first-balcony-collision-hull.obj first-balcony.mtl tower-ramp.png
# 	scp $^ headache:public_html/hifi/


upload: first-balcony.obj.gz first-balcony-collision-hull.obj.gz first-balcony.mtl tower-ramp.png
	mkdir -p first-balcony
	cp $^ first-balcony/
	rsync --delete -avP first-balcony/ headache.hungry.com:public_html/hifi/first-balcony/
