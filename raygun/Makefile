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


upload:
	scp raygun.obj raygun.mtl headache:public_html/hifi/hcEdit

clean:
	rm -f *~ raygun-body.obj raygun-body.stl raygun.obj raygun-shiney.obj raygun-shiney.stl
