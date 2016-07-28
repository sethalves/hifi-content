#
#
#

all: teleportal-frame.obj


teleportal-frame.stl: teleportal-frame.scad
	openscad -o $@ $^

teleportal-frame.obj: teleportal-frame.stl
	wavefront-obj-tool -n $^ -o $@ -S teleportal_frame_mtl -L teleportal-frame.mtl

clean:
	rm -f *~
	rm -f teleportal-frame.obj teleportal-frame.stl

upload: teleportal.js teleportal-frame.obj
	scp $^ headache.hungry.com:public_html/hifi/teleportal/
