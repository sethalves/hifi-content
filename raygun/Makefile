#
#
#

%.obj: %.stl
	wavefront-obj-tool -n $^ -o $@

all: raygun.obj

# --

raygun-body.stl: raygun.scad
	openscad -D body=1 -o $@ $^

raygun-body.obj: raygun-body.stl
	wavefront-obj-tool -n $^ -o $@ -S body_mtl

# --

raygun-shiney.stl: raygun.scad
	openscad -D shiney=1 -o $@ $^

raygun-shiney.obj: raygun-shiney.stl
	wavefront-obj-tool -n $^ -o $@ -S shiney_mtl

# --

raygun.obj: raygun-body.obj raygun-shiney.obj
	wavefront-obj-tool -n -L raygun.mtl -c $^ -o $@


upload: raygun.obj raygun.mtl raygun.js
	scp $^ headache:public_html/hifi/raygun/

unupload:
	scp headache:public_html/hifi/raygun/raygun.obj ./
	scp headache:public_html/hifi/raygun/raygun.mtl ./
	scp headache:public_html/hifi/raygun/raygun.js ./

clean:
	rm -f *~ raygun-body.obj raygun-body.stl raygun.obj raygun-shiney.obj raygun-shiney.stl
