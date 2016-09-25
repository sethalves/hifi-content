#
#
#


%.obj: %.stl
	wavefront-obj-tool -n $^ -o $@


all:

upload: boat.js
	scp $^ headache:public_html/hifi/boat/

clean:


all: boat-hull.obj

# boat-hull.obj: boat-hull.scm
# 	./boat-hull.scm


boat-hull.stl: boat-hull.scad
	openscad -o $@ $^

boat-hull.obj: boat-hull.stl
	wavefront-obj-tool -n -c $^ -o $@ -S boat_hull_mtl


upload: boat-hull.obj boat.js
	mkdir -p export
	cp -a $^ export/
	rsync -avP --delete export/ headache.hungry.com:public_html/hifi/boat/

clean:
	rm -f *~
	rm -f boat-hull.obj boat-hull.stl
	rm -rf export/
