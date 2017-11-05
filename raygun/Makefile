#
#
#

WAVEFRONT_OBJ_TOOL=wavefront-obj-tool
# WAVEFRONT_OBJ_TOOL=../../wavefront-obj-tools/wavefront-obj-tool-gauche.scm

%.obj: %.stl
	$(WAVEFRONT_OBJ_TOOL) -n $^ -o $@

%.obj.gz: %.obj
	cat $< | gzip -9 - > $@

all: raygun.obj.gz

# --

raygun-body.stl: raygun.scad
	openscad -D body=1 -o $@ $^

raygun-body.obj: raygun-body.stl
	$(WAVEFRONT_OBJ_TOOL) -n $^ -o $@ -S body_mtl

# --

raygun-shiney.stl: raygun.scad
	openscad -D shiney=1 -o $@ $^

raygun-shiney.obj: raygun-shiney.stl
	$(WAVEFRONT_OBJ_TOOL) -n $^ -o $@ -S shiney_mtl

# --

raygun.obj: raygun-body.obj raygun-shiney.obj
	$(WAVEFRONT_OBJ_TOOL) -n -L raygun.mtl -c $^ -o $@

upload: raygun.obj raygun.obj.gz raygun.mtl raygun.js raygun-1.js
	mkdir -p raygun
	cp $^ raygun/
	rsync --delete -avP raygun/ headache.hungry.com:public_html/hifi/raygun/


clean:
	rm -f *~ raygun raygun-body.obj raygun-body.stl raygun.obj raygun-shiney.obj raygun-shiney.stl
